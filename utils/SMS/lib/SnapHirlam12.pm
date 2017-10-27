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
package SnapHirlam12;

use Snap qw(put_statusfile);

use strict;
use warnings;

use File::Basename;
use File::Copy;

#use Time::ParseDate;
#use DateTime;
use XML::LibXSLT;
#use Template;


# Run the model and return filenames.  Gets as input which of the
# three models to run.
#
# All three models are run by running bsnap_model snap.input.  We start with
# creating a correct snap.input.
sub run_model {
    my ($smsdirs, $remote_hosts_and_users, $remote_ftp_dir, $run_ident, $model) = @_;

    my $mod_dir = $smsdirs->{work};

    my $model_error = 0;
    my $return_file;
    my $bsnap;
    my $input_res;

    # MLDPO is the original bsnap..
    if ($model eq q[MLDP0]) {
        return run_model_orig( $remote_hosts_and_users, $remote_ftp_dir, $run_ident );
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

        # Copy isotope file from etc:
        copy($smsdirs->{etc}.q[/isotope_list.txt], qq[$mod_dir/]);

        # Define XML- and XSLfiles:
        my $xmlfile = $smsdirs->{data}.qq[/$run_ident] . q[_Rimsterm.xml];
        my $xslfile = $smsdirs->{etc}.q[/rimsterm.xsl];
        if (! -f $xslfile) {
            die "Missing xsl-file in etc: $xslfile\n";
        }
        my $outfile = qq[$mod_dir/nrpa_input.txt];

        # Create nrpa_input.txt from XML using XSL
        print qq[Creating nrpa_input.txt from $xmlfile using $xslfile\n];
        my $parser = XML::LibXML->new();
        my $doc    = $parser->parse_file($xmlfile);

        my $root    = $doc->getDocumentElement;
        my $xslt    = XML::LibXSLT->new();
        my $source  = XML::LibXML->load_xml(location => $xmlfile);
        my $xsl_doc = XML::LibXML->load_xml(
            location => $xslfile,
            no_cata  => 1
        );

        my $stylesheet = $xslt->parse_stylesheet($xsl_doc);
        my $results    = $stylesheet->transform($source);

        $stylesheet->output_file($results, $outfile);
        print qq[Running create_naccident_input\n];
        $input_res = system(q[create_naccident_input]);

        #      if (!$input_res == 0) {
        #	print qq[res: $!\n];
        #	$input_res=0;
        #      }

        $bsnap = qq[bsnap_naccident];
    } elsif ($model eq q[SNAP-BOMB]) {

        # Do nuclear bomb stuff
        $mod_dir = $smsdirs->{data}.q[/work_bomb];

        $return_file = $run_ident . q[_SNAP-BOMB2ARGOS.zip];

        my $inputfile = $run_ident . q[_SNAP-BOMB_input];

        # read inputfile
        open my $ih, q[<], $inputfile
            or put_statusfile($smsdirs, $remote_hosts_and_users, $remote_ftp_dir, $run_ident, $model, 409)
            && die qq[Can't read $inputfile, $!\n];

        my %vars;
        extract_info(\%vars, $ih);

        $input_res = system(q[create_bomb_input]);

        $bsnap = qq[bsnap_nbomb];
    }

    if ($input_res != 0) {
        put_statusfile($smsdirs, $remote_hosts_and_users, $remote_ftp_dir, $run_ident, $model, $input_res);
        die("\nProblems with creating snap.input, $input_res\n\n");
    }

    put_statusfile($smsdirs, $remote_hosts_and_users, $remote_ftp_dir, $run_ident, $model, 101);

    # Run the model
    if (!-d $mod_dir) {
        mkdir $mod_dir || die qq[Can't mkdir to $mod_dir $!\n];
    }
    chdir $mod_dir or die qq[Can't chdir to $mod_dir, $!\n];

    print qq[Run $bsnap snap.input\n];
    system("$bsnap snap.input") == 0
        or do {
        $model_error = 1;
        };

    my @files;

    # when naccident is run, we need to do some postprocessing
    if ($model eq q[SNAP]) {
        system("create_naccident_output") == 0
            or do {
            $model_error = 2;
            };

        # change file names to be $run_ident... :
        my @old_files = qw(naccident_SNAP_conc
            naccident_SNAP_dose
            naccident_SNAP_depo);
        foreach my $file (@old_files) {
            my $new_name = $file;
            $new_name =~ s!naccident!$run_ident!;
            move($file, $new_name);
        }

        print qq[Add files $mod_dir/*_{conc,dose,depo} to $return_file:\n];
        @files = glob($run_ident . qq[*_{conc,dose,depo}]);
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

# Run the original model, using read_argos
sub run_model_orig {
    my ( $smsdirs, $remote_hosts_and_users, $remote_ftp_dir, $run_ident ) = @_;

    my $model_error = 0;

    # Prepare input
    my $arg1 = "--conffile $smsdirs->{job}/Isodata_Radius_DryDepVel.DAT";
    my $arg2 = "--staticindir $smsdirs->{job}";
    my $arg3 = "--dynamicindir $smsdirs->{data}";
    my $arg4 = "--outputfile $smsdirs->{data}/snap.input";
    my $arg5 = "--runident $run_ident";
    my $res = system( "$smsdirs->{job}/read_argos.pl $arg1 $arg2 $arg3 $arg4 $arg5" );
    if ( $res > 0 ) {
      put_statusfile( $smsdirs, $remote_hosts_and_users, $remote_ftp_dir, $run_ident,
              'MLDP0', $res );
      die( "\nProblems with $smsdirs->{job}/read_argos.pl\n\n" );
    }

    put_statusfile( $smsdirs, $remote_hosts_and_users, $remote_ftp_dir,
            $run_ident, 'MLDP0', 101 );

    # Run the model
    chdir( $smsdirs->{data} );
    system( "bsnap snap.input" ) == 0
      or do {
    $model_error = 1;
      };

    my $resfile= $run_ident . "_hi_res_MLDP02ARGOS.zip";
    my $file1=   $run_ident . "_MLDP0_depo";
    my $file2=   $run_ident . "_MLDP0_conc";
    my $file3=   $run_ident . "_MLDP0_dose";

    if ( ! -s $file1 && ! -s $file2 && ! -s $file3) { $model_error = 2 };

    my $zip = "zip";
    if ( -x "zip" ) { $zip = "zip" };

    system( "$zip -l $resfile $file1 $file2 $file3" );

    if ( ! -s $resfile ) { $model_error = 3 };

    unlink ($file1, $file2, $file3);

    my @output_files = ( $resfile );

    return ( $model_error, @output_files );

}


1;
__END__
