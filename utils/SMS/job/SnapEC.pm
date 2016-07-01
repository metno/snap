package SnapEC;

use Snap qw(put_statusfile);

use strict;
use warnings;

use File::Basename;
use File::Copy;

#use Time::ParseDate;
#use DateTime;
use XML::LibXSLT;
#use Template;


sub run_model {
    my ($smsdirs, $remote_hosts_and_users, $remote_ftp_dir, $run_ident, $model) = @_;

    my $mod_dir = $smsdirs->{work};
    my $PPIdir = $remote_hosts_and_users->[0]{PPIdir} or die "run_model needs PPIdir";
    my $PPIhost = $remote_hosts_and_users->[0]{PPIhost} or die "run_model needs PPIhost";
    my $PPIuser = $remote_hosts_and_users->[0]{PPIuser} or die "run_model needs PPIuser";

    my $model_error = 0;
    my $return_file;
    my $bsnap;
    my $input_res;

    # MLDPO is the original bsnap..
    if ($model eq q[MLDP0]) {
        die "can no longer run original MLDP0 model with EC-data";
    } elsif ($model eq q[TRAJ]) {

        # Do traj-stuff
        $return_file = $run_ident . q[_TRAJ2ARGOS.zip];

        $mod_dir = $smsdirs->{data}.'/work_traj';

        # Create nrpa.input to create snap.input
        my $inputfile = $run_ident . q[_TRAJ_input];
        my $nrpa;
        {
            open my $if, $inputfile
                or die "Cannot read $inputfile: $!\n";
            scalar <$if>;    # skip run-name (use run-ident instead)
            local $/ = undef;
            $nrpa = $run_ident . "\n" . <$if>;
            close $if;
        }
        open my $of, ">$mod_dir/nrpa.input"
            or die "Cannot write $mod_dir/nrpa.input: $!\n";
        print $of $nrpa;
        close $of;

        # read inputfile, fixed format
        print qq[**** Reading $inputfile\n];
        open my $ih, q[<], $inputfile
            or put_statusfile($smsdirs, $remote_hosts_and_users, $remote_ftp_dir, $run_ident, $model, 409)
            && die qq[Can't read $inputfile, $!\n];

        chdir $mod_dir;
        print qq[Running create_traj_input\n];
        $input_res = system(q[create_traj_input]);
        chdir $smsdirs->{data};

        $bsnap = qq[bsnap_traj];
    } elsif ($model eq q[SNAP]) {

        # Do nuclear accident stuffstylesheet
        $return_file = $run_ident . q[_hi_res_SNAP2ARGOS.zip];

        $mod_dir = $smsdirs->{data}.'/work_naccident';

        # Make sure directory exist before copying file:
        if (!-d $mod_dir) {
            mkdir $mod_dir || die qq[Can't mkdir to $mod_dir $!\n];
        }
        chdir $mod_dir or die qq[Can't chdir to $mod_dir, $!\n];

        # Define XML file
        my $xmlfile = $smsdirs->{data}.qq[/$run_ident] . q[_Rimsterm.xml];
        # Create qsub script
        my $qsubScript = "nrpa\_bsnap.sh";
        open(my $qsub, "> $qsubScript") or die "Cannot write '$qsubScript': $!\n";
        print $qsub <<"EOF";
#!/bin/bash
#\$ -N nrpa_bsnap
#\$ -S /bin/bash
#\$ -j n
#\$ -r y
#\$ -l h_rt=0:40:00
#\$ -l h_vmem=5G
#\$ -m bea
#\$ -pe mpi 1
#\$ -q operational.q
#\$ -sync yes
#\$ -o /home/sms/sge/OU\$JOB_NAME.\$JOB_ID
#\$ -e /home/sms/sge/ER\$JOB_NAME.\$JOB_ID

module load SnapPy/1.0.0

export OMP_NUM_THREADS=1

cd $remote_hosts_and_users->[0]{PPIdir}
snap4rimsterm --rimsterm $xmlfile --dir . --ident naccident_SNAP

EOF
        close $qsub;

        print qq[Moving input to PPI\n];
        my $scp_command = q[[scp $qsubScript $xmlfile $PPIuser\@$PPIhost:$PPIdir]];

        if (system($scp_command) != 0) {
            print STDERR "error on command: '$scp_command'";
            $input_res++;
        }

        $bsnap = qq[qsub $PPIdir/$qsubScript];
    } elsif ($model eq q[SNAP-BOMB]) {

        die "BOMB not working with EC-data";

    }

    if ($input_res != 0) {
        put_statusfile($smsdirs, $remote_hosts_and_users, $remote_ftp_dir, $run_ident, $model, $input_res);
        die("\nProblems with creating input, $input_res\n\n");
    }

    put_statusfile($smsdirs, $remote_hosts_and_users, $remote_ftp_dir, $run_ident, $model, 101);

    # Run the model
    if (!-d $mod_dir) {
        mkdir $mod_dir || die qq[Can't mkdir to $mod_dir $!\n];
    }
    chdir $mod_dir or die qq[Can't chdir to $mod_dir, $!\n];

    print qq[Run on PPI: '$bsnap'\n];
    system_ppi("$bsnap") == 0
        or do {
        $model_error = 1;
        };

    my @files;

    # when naccident is run, we need to do some postprocessing
    if ($model eq q[SNAP]) {
        # change file names to be $run_ident... :
        my @old_files = qw(
            naccident_SNAP_conc
            naccident_SNAP_dose
            naccident_SNAP_depo
            naccident_SNAP_wdep
            naccident_SNAP_prec
            );
        my $scp_command = "scp $PPIuser@$PPIhost:$PPIdir/naccident_SNAP_* .";
        print qq[Running '$scp_command'\n];
        system($scp_command);
        foreach my $file (@old_files) {
            my $new_name = $file;
            $new_name =~ s!naccident!$run_ident!;
            move($file, $new_name);
        }

        print qq[Add files $mod_dir/*_{conc,dose,depo,prec,wdep} to $return_file:\n];
        @files = glob($run_ident . qq[*_{conc,dose,depo,prec,wdep}]);
    } elsif ($model eq q[TRAJ]) {
        @files = glob(qq[Trajectory*\.DAT]);
    }

    # zip the result file, move zip file to datadir and then change to
    # datadir:
    print join "\n", @files;
    my @tmp = map {basename($_) . q[ ]} @files;

    my @command = ('zip', '-l', $return_file, @tmp);
    system("@command") == 0
        or  print STDERR "Problems when running command: @command\n";

    if (!-s $return_file) {
        $model_error = 3;
    } else {
        map {unlink $_} @files;
    }

    # Move zip file to datadir and change:
    my @output_files = ($return_file);
    rename($return_file, $smsdirs->{data}.qq[/$return_file]) or print qq[Can't rename $return_file: $!\n];
    chdir $smsdirs->{data};

    return ($model_error, @output_files);
}

# fetch the meteorology from lustre
sub fetch_meteo {

}

1;
__END__
