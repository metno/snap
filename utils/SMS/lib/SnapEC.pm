# SNAP: Servere Nuclear Accident Programme
# Copyright (C) 1992-2017   Norwegian Meteorological Institute
# 
# This file is part of SNAP. SNAP is free software: you can 
# redistribute it and/or modify it under the terms of the 
# GNU General Public License as published by the 
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
package SnapEC;

use Snap qw(put_statusfile system_ppi);

use strict;
use warnings;

use File::Basename;
use File::Copy;

sub run_model {
    my ($smsdirs, $remote_hosts_and_users, $remote_ftp_dir, $run_ident, $model) = @_;

    my $mod_dir = $smsdirs->{work};
    my $PPIdir = $remote_hosts_and_users->[0]{PPIdir} or die "run_model needs PPIdir";
    my $PPIhost = $remote_hosts_and_users->[0]{PPIhost} or die "run_model needs PPIhost";
    my $PPIuser = $remote_hosts_and_users->[0]{PPIuser} or die "run_model needs PPIuser";

    my $model_error = 0;
    my $return_file;
    my $return_files_ascii = 1; # ascii model-output, convert \n to \r\n in zip
    my $bsnap;
    my $input_res = 0;

    # MLDPO is the original bsnap..
    if ($model eq q[MLDP0]) {
        die "can no longer run original MLDP0 model with EC-data";
    } elsif ($model eq q[TRAJ]) {

        # Do traj-stuff
        $return_file = $run_ident . q[_TRAJ2ARGOS.zip];
        $return_files_ascii = 1;

        $mod_dir = $smsdirs->{data}.'/work_traj';
        # Make sure directory exist before copying file:
        if (!-d $mod_dir) {
            mkdir $mod_dir || die qq[Can't mkdir to $mod_dir $!\n];
        }

        # Trajectory input file from nrpa
        my $trajIn = $run_ident . q[_TRAJ_input];
        my $inputfile = $smsdirs->{data}. q[/]. $trajIn;

        my $qsubScript = "nrpa_bsnap_traj.sh";
        open(my $qsub, "> $qsubScript") or die "Cannot write '$qsubScript': $!\n";
        print $qsub <<"EOF";
#!/bin/bash
#\$ -N nrpa_bsnap_traj
#\$ -S /bin/bash
#\$ -V
#\$ -j n
#\$ -r y
#\$ -l h_rt=0:10:00
#\$ -l h_vmem=8G
##\$ -m bea
#\$ -pe mpi 1
#\$ -q operationalx.q
#\$ -sync yes
#\$ -o $remote_hosts_and_users->[0]{PPIdir}/OU\$JOB_NAME.\$JOB_ID
#\$ -e $remote_hosts_and_users->[0]{PPIdir}/ER\$JOB_NAME.\$JOB_ID

module load SnapPy/1.3.0

ulimit -c 0
export OMP_NUM_THREADS=1

cd $remote_hosts_and_users->[0]{PPIdir}
snap4rimsterm --trajInput $trajIn --dir . --ident $run_ident

EOF
        close $qsub;

        print qq[Moving input to PPI\n];
        my $scp_command = qq[scp $qsubScript $inputfile $PPIuser\@$PPIhost:$PPIdir];

        if (system($scp_command) != 0) {
            print STDERR "error on command: '$scp_command'";
            $input_res++;
        }

        $bsnap = qq[qsub $PPIdir/$qsubScript];

    } elsif ($model =~ m[SNAP]) {
        $return_file = $run_ident . '_'.${model}.q[2ARGOS.zip];
        # Do nuclear accident stuff
        my $worldwide = "";
        if ($model =~ m[GLOBAL]) {
            $worldwide = "--worldwide";
        }
        $return_files_ascii = 0; # grib-output

        $mod_dir = $smsdirs->{data}.'/work_naccident';

        # Make sure directory exist before copying file:
        if (!-d $mod_dir) {
            mkdir $mod_dir || die qq[Can't mkdir to $mod_dir $!\n];
        }
        chdir $mod_dir or die qq[Can't chdir to $mod_dir, $!\n];

        # Define XML file
        my $xmlfile = qq[$run_ident] . q[_Rimsterm.xml];
        my $xmlinfile = $smsdirs->{data}.qq[/$xmlfile];
        
        my $requestfile = qq[$run_ident] . q[_SNAP_request.xml];
        my $requestinfile = $smsdirs->{data}.qq[/$requestfile];
        my $argosrequest = "";
        if (-f $requestinfile) {
            $argosrequest = '--argosrequest ' . $requestfile;
        } else {
            $requestinfile = "";
        }

        
        # Create qsub script
        my $qsubScript = "nrpa\_bsnap.sh";
        open(my $qsub, "> $qsubScript") or die "Cannot write '$qsubScript': $!\n";
        print $qsub <<"EOF";
#!/bin/bash
#\$ -N nrpa_bsnap
#\$ -S /bin/bash
#\$ -V
#\$ -j n
#\$ -r y
#\$ -l h_rt=0:50:00
#\$ -l h_vmem=8G
##\$ -m bea
#\$ -pe mpi 1
#\$ -q operationalx.q
#\$ -sync yes
#\$ -o $remote_hosts_and_users->[0]{PPIdir}/OU\$JOB_NAME.\$JOB_ID
#\$ -e $remote_hosts_and_users->[0]{PPIdir}/ER\$JOB_NAME.\$JOB_ID

module load SnapPy/1.3.0

ulimit -c 0
export OMP_NUM_THREADS=1

cd $remote_hosts_and_users->[0]{PPIdir}
snap4rimsterm --rimsterm $xmlfile $argosrequest --dir . --ident naccident_SNAP $worldwide
ncatted -a title,global,o,c,"$run_ident" snap.nc

EOF
        close $qsub;

        print qq[Moving input to PPI\n];
        my $scp_command = qq[scp $qsubScript $xmlinfile $requestinfile $PPIuser\@$PPIhost:$PPIdir];

        if (system($scp_command) != 0) {
            print STDERR "error on command: '$scp_command'";
            $input_res++;
        }

        $bsnap = qq[qsub $PPIdir/$qsubScript];
    } elsif ($model eq q[SNAP-BOMB]) {

        die "BOMB not working with EC-data";

    } else {
        die "Don't know what to do with model '$model'";
    }

    if ($input_res != 0) {
        put_statusfile($smsdirs, $remote_hosts_and_users, $remote_ftp_dir, $run_ident, $model, $input_res);
        die("\nProblems while creating input, $input_res\n\n");
    }

    put_statusfile($smsdirs, $remote_hosts_and_users, $remote_ftp_dir, $run_ident, $model, 101);

    # Run the model
    if (!-d $mod_dir) {
        mkdir $mod_dir || die qq[Can't mkdir to $mod_dir $!\n];
    }
    chdir $mod_dir or die qq[Can't chdir to $mod_dir, $!\n];

    print qq[Run on PPI: '$bsnap'\n];
    system_ppi($remote_hosts_and_users, "$bsnap") == 0
        or do {
        $model_error = 1;
        };

    my @files;

    # when naccident is run, we need to do some postprocessing
    if ($model =~ m[SNAP]) {
        # change file names to be $run_ident... :
        my @old_files = qw(
            naccident_SNAP_conc
            naccident_SNAP_dose
            naccident_SNAP_depo
            naccident_SNAP_wetd
            naccident_SNAP_prec
            naccident_SNAP_tofa
            naccident_SNAP_all.nc
            );
        my $scp_command = "scp $PPIuser\@$PPIhost:$PPIdir/naccident_SNAP_* .";
        print qq[Running '$scp_command'\n];
        system($scp_command);
        foreach my $file (@old_files) {
            my $new_name = $file;
            $new_name =~ s!naccident!$run_ident!;
            move($file, $new_name);
        }

        print qq[Add files $mod_dir/*_{conc,dose,depo,prec,wetd,tofa,all.nc} to $return_file:\n];
        @files = glob($run_ident . qq[*_{conc,dose,depo,prec,wetd,tofa,all.nc}]);
    } elsif ($model eq q[TRAJ]) {
        my $scp_command = "scp $PPIuser\@$PPIhost:$PPIdir/Trajectory*\.DAT .";
        print qq[Running '$scp_command'\n];
        system($scp_command);
        @files = glob(qq[Trajectory*\.DAT]);
    }

    # zip the result file, move zip file to datadir and then change to
    # datadir:
    print join "\n", @files;
    my @tmp = map {basename($_) . q[ ]} @files;

    my @zipoptions;
    push @zipoptions, '-l', if $return_files_ascii;
    my @command = ('zip', @zipoptions, $return_file, @tmp);
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


1;
__END__

=head1 NAME

SnapEC - general functionality to run snap with EC0.1 meteorology on PPI

=head1 SYNOPSIS

  use SnapEC;

  my %smsdirs = (data => $datadir,
                 work => $workdir);

  my $remote_hosts_and_users = [ { host => "x",
                              user => "x",
                              PPIuser => 'heikok',
                              PPIhost => 'vis-m1'} ]; # PPIhost usually set by SMS: Misc/Funcs.pm GET_PPI_LOGIN_HOST
  my $PPIdir = Snap::create_ppi_dir($remote_hosts_and_users, ['dirA', 'dirB']);
  $remote_hosts_and_users->[0]{PPIdir} = $PPIdir;

  my $run_ident = 'Hartlepool-20162306-0924';

  my ($error, @files) = SnapEC::run_model(\%smsdirs, $remote_hosts_and_users, $remote_ftp_dir,
                                          $run_ident, 'SNAP');


=head1 REQUIREMENTS

=over 8

=item PPI host with SnapPy/1.3.0 module installed, and run with SGE qsub with access to the queue 'operationalx.q'

=item zip

=item ssh, scp

=item input-file: $run_ident\_Rimsterm.xml in sms-datadir

=item about 250MB / run temporary, zipped output is about 50MB

=back

=head1 DESCRIPTION

The SnapEC module will run the model remotely on the PPI.


=head2 ($error, @files) = SnapEC::run_model(\%smsdirs, $remote_hosts_and_users, $remote_ftp_dir,
                                            $ident, $model)



=over 4

=item \%smsdirs  directories used by sms, usually $smsdirs->{data}, {etc}, {work}

=item $remote_hosts_and_users

   [ { host => "$remote_host_alias",
       user => "$remote_user",
       PPIdir => 'directory/on/lustre',
       PPIuser => 'login_user',
       PPIhost => 'login_node' } ]

=item $remote_ftp_dir  a directory

=item $run_ident  the basename for the run, further input and output-files are based upon that name

=item $model the model type, e.g. MLDP0, SNAP, SNAPGLOBAL, TRAJ, SNAP-BOMB

currently, only SNAP, SNAPGLOBAL and TRAJ is implemented. MLDP0 is very old, and SNAP-BOMB never worked

=item $error Return error status, 0 on success

=item @files Files created on local disk

=back


=head1 AUTHOR

Heiko Klein, E<lt>H.Klein@met.noE<gt>

=cut
