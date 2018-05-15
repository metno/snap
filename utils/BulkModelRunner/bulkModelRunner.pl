#! /usr/bin/perl
use strict;
use warnings;

use Time::Local qw(timegm);
use Getopt::Long qw(GetOptions);
use Pod::Usage qw(pod2usage);
use Cwd qw(cwd);

use constant SNAP_FILES => [qw(bsnap_naccident )];
#felt2ncDummyLevels2Isotopes.pl felt2nc_snap_dummy_levels.xml felt_axes.xml felt2diana.pl)];

use vars qw(%Args);
$Args{n} = 0;
$Args{arrayJob} = 0;
$Args{inputType} = 'h12';
$Args{inputDir} = '';
$Args{dailyStart} = '12';
GetOptions(\%Args,
       'debug',
       'help',
       'inputType=s',
       'inputDir=s',
       'outputDir=s',
       'startDate=s',
       'dailyStart=s',
       'endDate=s',
       'runTime=i',
       'sourceTermFile=s',
       'meteoSetupFile=s',
       'n=i',
       'arrayJob=i')
    or pod2usage(2);

pod2usage(-exitval => 0,
      -verbose => 2) if $Args{help};
pod2usage(-exitval => 2,
      -message => 'missing outputDir',
      -verbose => 0) if (! -d $Args{outputDir});
pod2usage(-exitval => 2,
      -msg => 'missing inputDir',
      -verbose => 0) if ! -d $Args{inputDir};
my @dailyStart = split ',', $Args{dailyStart};
my $sourceTerm = slurp($Args{sourceTermFile});
my $meteoSetup = slurp($Args{meteoSetupFile});

my @runs = createRuns($Args{startDate}, $Args{endDate}, \@dailyStart);

if ($Args{debug}) {
    print STDERR "# startDate\t arrayJobId\n";
    my $i = 1;
    foreach my $r (@runs) {
        my @date = gmtime($r);
        my $hour = sprintf "%02d", $date[2];
        my $day = sprintf "%02d", $date[3];
        my $month = sprintf "%02d", ($date[4] + 1);
        my $year = $date[5] + 1900;
        
        print STDERR "$year$month${day}_$hour\t$i\n";
        $i++;
    }
    exit(0);
}

if ($Args{n}  > 0) {
    my %knownChildren;
    for (my $i = 0; $i < $Args{n}; $i++) {
        makeChildRun(\@runs, \%knownChildren, $Args{runTime}, $Args{inputDir}, $Args{inputType}, $Args{outputDir}, $sourceTerm, $meteoSetup);
    }

    my $child = 1;
    while ($child > 0) {
        $child = waitpid(-1, 0);
        if (exists $knownChildren{$child}) {
            delete $knownChildren{$child};
            # start a new child after one has gone
            makeChildRun(\@runs, \%knownChildren, $Args{runTime}, $Args{inputDir}, $Args{inputType}, $Args{outputDir}, $sourceTerm, $meteoSetup);
        }
    }
} elsif ($Args{arrayJob} > 0 and $Args{arrayJob} <= @runs) {
    makeArrayRun($runs[$Args{arrayJob}-1], $Args{runTime}, $Args{inputDir}, $Args{inputType}, $Args{outputDir}, $sourceTerm, $meteoSetup);
} else {
    print STDERR "Please run either -n 4 (number of parallel processors) or\n";
    print STDERR "with -arrayJob # (pbs/sge array job number) with # between 1 and ",scalar @runs,"\n";
    exit 1;
}


sub makeChildRun {
    my ($runs, $knownChildren, $runTime, $inputDir, $inputType, $outputDir, $sourceTerm, $meteoSetup) = @_;
    my $runId = shift @$runs;
    if (defined $runId) {
    print STDERR "Starting run $runId, ".scalar @runs." remaining\n";
        my $pid = fork();
        die "fork() failed: $!" unless defined $pid;
        if ($pid == 0) {
            # child
            snapRun($runId, $runTime, $inputDir, $inputType, $outputDir, $sourceTerm, $meteoSetup);
            exit(0);
        } else {
            $knownChildren->{$pid} = 1;
        }
    }
}

sub makeArrayRun {
    my ($runId, $runTime, $inputDir, $inputType, $outputDir, $sourceTerm, $meteoSetup) = @_;
    snapRun($runId, $runTime, $inputDir, $inputType, $outputDir, $sourceTerm, $meteoSetup);
}

sub snapRun {
    my ($runId, $runTime, $inputDir, $inputType, $outputDir, $sourceTerm, $meteoSetup) = @_;
    my @date = gmtime($runId);
    my $hour = sprintf "%02d", $date[2];
    my $day = sprintf "%02d", $date[3];
    my $month = sprintf "%02d", ($date[4] + 1);
    my $year = $date[5] + 1900;
    my $newDir =  "$outputDir$year$month$day\_$hour";
    if (-f "$newDir/snap.nc") {
        print STDERR "snap.nc already exists for $year-$month-$day $hour, finish";
        return;
    }

    # create the new snap.input data
    my $snapInput = "TIME.START=   $year  $month  $day $hour\n";
    $snapInput .=   "TIME.RUN  = $runTime"."h\n";
    $snapInput .=   "STEP.HOUR.OUTPUT.FIELDS= $runTime\n";
    $snapInput .= $sourceTerm;
    $snapInput .= $meteoSetup;
    # add filenames to use for this run
    my $runEnd = $runId + 3600 * ($runTime + 6);
    my $time = $runId;
    while ($time <= $runEnd) {
        if ($inputType eq 'ec') {
            # daily files of type meteo2013090100_00.nc
            # but starting at 03-24
            my @date = gmtime($time - 3 * 3600);
            my $day = sprintf "%02d", $date[3];
            my $month = sprintf "%02d", ($date[4] + 1);
            my $year = $date[5] + 1900;
            my $fileName = $inputDir . '/meteo' . "$year$month$day" . '00_00.nc';
            if (-r $fileName) {
                $snapInput .= "FIELD.INPUT= $fileName\n";
            } else {
                die "unreadable file: $fileName";
            }
            $time += 24*3600; # advance a day
        } elsif ($inputType eq 'h12') {
            # 4 files daily of type h12sf00.dat h12snap00.dat
            # but starting at 00-24, need 3 hours startup (precip)
            my @date = gmtime($time - 3 * 3600);
            my $day = sprintf "%02d", $date[3];
            my $month = sprintf "%02d", ($date[4] + 1);
            my $year = $date[5] + 1900;
            my $dirName = $inputDir . "/$year-$month-$day/";
            foreach my $file (qw(h12sf00.dat h12snap00.dat h12sf12.dat h12snap12.dat)) {
                my $fileName = $dirName . $file;
                if (-r $fileName) {
                    $snapInput .= "FIELD.INPUT= $fileName\n";
                } else {
                    die "unreadable file: $fileName";
                }
            }
            $time += 24*3600; # advance a day
        } elsif ($inputType eq 'erai') {
            # 4 files daily of type 2001/08/fc.2001080100 fc.2001080106 fc.2001080112 fc.2001080118
            # but starting at 00-24, need 3 hours startup (precip)
            my @date = gmtime($time - 3 * 3600);
            my $day = sprintf "%02d", $date[3];
            my $month = sprintf "%02d", ($date[4] + 1);
            my $year = $date[5] + 1900;
            my $dirName = $inputDir . "/$year/$month/";
            foreach my $hh (qw(00 06 12 18)) {
                my $fileName = $dirName . "fc.$year$month$day$hh";
                if (-r $fileName) {
                    $snapInput .= "FIELD.INPUT= $fileName\n";
                } else {
                    die "unreadable file: $fileName";
                }
            }
            $time += 24*3600; # advance a day
        } else {
            die "unknown inputTye: $inputType\n";
        }
    }
    # and the rest
    $snapInput .= SNAPINPUT();

    if (!-d $newDir) {
        mkdir "$newDir" or die "Cannot create directory $newDir: $!\n";
    }
    my $orgDir = cwd();
    foreach my $file (@{ SNAP_FILES() }) {
       if (! -e "$newDir/$file") {
           unlink("$newDir/core");
	   unlink("$newDir/$file");
           symlink("$orgDir/$file", "$newDir/$file") or die "Cannot link $file to $newDir: $!\n";
       }
    }
    chdir $newDir or die "Cannot change to $newDir: $!\n";
    open my $f, '>snap.input' or die "Cannot write snap.input in $newDir: $!\n";
    print $f $snapInput;
    my $bsnap = SNAP_FILES->[0];
    my $starttime = time;
    system("./$bsnap". ' snap.input > snapOut.log 2>&1') == 0 or die "system ./$bsnap snap.input in $newDir, $runId failed: $?";
    my $runtime = time - $starttime;
    print STDERR "$bsnap successfully finished for $year$month$day\_$hour in $runtime secs\n";
#    system("perl felt2diana.pl  --tag=snap --omitDiana") == 0
#        or die "Cannot run felt2diana in $newDir";
    # cleanup
    foreach my $file (@{ SNAP_FILES() }) {
        #unlink $file;
    }
    # unlink 'snap.felt';
    chdir $orgDir or die "Cannot change to $orgDir: $!\n";
}

# return runs with runid = epoch-seconds
sub createRuns {
    my ($start, $end, $hours) = @_;
    my ($syear, $smon, $sday) = split '-', $start;
    my ($eyear, $emon, $eday) = split '-', $end;

    my $gmstart = timegm(0,0,0,$sday,$smon-1,$syear);
    my $gmend = timegm(0,0,0,$eday,$emon-1,$eyear);

    my $secsPerHour = 60*60;
    my $secsPerDay = $secsPerHour*24;
    while ($gmstart <= $gmend) {
        push @runs, map {$gmstart + $_*$secsPerHour} @$hours;
        #print STDERR "$gmstart $gmend @runs\n";
        $gmstart += $secsPerDay;
    }
    return @runs;
}

sub slurp {
    my ($file) = @_;
    local $/ = undef;
    open my $f, $file or die "cannot read $file: $!\n";
    my $retVal = <$f>;
    close $f;
    return $retVal;
}

use constant SNAPINPUT => <<'EOT'
RANDOM.WALK.ON
BOUNDARY.LAYER.FULL.MIX.OFF
DRY.DEPOSITION.NEW
WET.DEPOSITION.NEW
PRECIP(MM/H).PROBAB= 0.0,0.00, 0.5,0.31, 1.0,0.48, 1.5,0.60, 2.0,0.66
PRECIP(MM/H).PROBAB= 3.3,0.72, 8.3,0.80, 15.,0.85, 25.,0.91
REMOVE.RELATIVE.MASS.LIMIT= 0.01
*
TIME.STEP= 300.
STEP.HOUR.INPUT.MIN=  1
STEP.HOUR.INPUT.MAX=  6
*STEP.HOUR.OUTPUT.FIELDS= 240
ASYNOPTIC.OUTPUT
TOTAL.COMPONENTS.OFF
PRECIPITATION.ON
MODEL.LEVEL.FIELDS.OFF
FIELD_TIME.FORECAST
* FIELD_TIME.VALID
FIELD.OUTTYPE= netcdf
FIELD.OUTPUT= snap.nc
LOG.FILE=     snap.log
DEBUG.OFF
ENSEMBLE.PROJECT.OUTPUT.OFF
ARGOS.OUTPUT.OFF
END
**----------------------------------------------------------------------
EOT

__END__

=head1 NAME

bulkModelRunner.pl - run snap model in parallel many times

=head1 SYNOPSIS

  bulkModelRunner --inputType=ec --inputDir=/disk1/K27/Ecmet --outputDir=/tmp/results
                  --startDate=2013-08-01 --dailyStart=0,12 --endDate=2013-10-01
                  --runTime=96
                  --sourceTermFile=snapSourceTerm.txt --meteoSetupFile=snapMeteoSetup.txt
                  [ -n 8 | -arrayJob 348 ]

=head1 DESCRIPTION

Run the snap model in parallel (-n times) or in a pbs/sge array-queue (-arrayJob jobNumber)
starting at startDate and finish at endDate, with
dailyStart start-hours. inputType can be ec (IFS-emep-meteo in netcdf) or hirlam (felt).

The sourceTermFile.txt should contain all information about the source-term and
will be added as is to the snap.input file.

The modelSetupFile.txt should contain the information about the meteo-model, e.g. domain,
vertical-levels etc.

=head2 DETAILS

=over 8

=item inputType can be ec (netcdf emep/IFS), h12 (felt) or erai (felt, nora era interim 10km)

= back

=head1 AUTHOR

Heiko Klein, E<lt>Heiko.Klein@met.noE<gt>

=head1 SEE ALSO

L<perl>, L<snap>

=cut
