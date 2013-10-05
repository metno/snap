#! /usr/bin/perl
use strict;
use warnings;

use Time::Local qw(timegm);
use Getopt::Long qw(GetOptions);
use Pod::Usage qw(pod2usage);
use Cwd qw(cwd);

use constant SNAP_FILES => [qw(bsnap_naccident felt2ncDummyLevels2Isotopes.pl felt2nc_snap_dummy_levels.xml felt_axes.xml felt2diana.pl)];

use vars qw(%Args $nproc);
$Args{n} = 4;
$Args{inputType} = 'felt';
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
       'n=i')
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

foreach my $runId (@runs) {
    snapRun($runId, $Args{runTime}, $Args{inputDir}, $Args{inputType}, $Args{outputDir}, $sourceTerm, $meteoSetup);
}

sub snapRun {
    my ($runId, $runTime, $inputDir, $inputType, $outputDir, $sourceTerm, $meteoSetup) = @_;
    my @date = gmtime($runId);
    my $hour = sprintf "%02d", $date[2];
    my $day = sprintf "%02d", $date[3];
    my $month = sprintf "%02d", ($date[4] + 1);
    my $year = $date[5] + 1900;

    # create the new snap.input data
    my $snapInput = "TIME.START=   $year  $month  $day $hour\n";
    $snapInput .=   "TIME.RUN  = $runTime"."h\n";
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
        } else {
            die "unknown inputTye: $inputType\n";
        }
    }
    # and the rest
    $snapInput .= SNAPINPUT();

    my $newDir =  "$outputDir$year$month$day\_$hour";
    if (!-d $newDir) {
    	mkdir "$newDir" or die "Cannot create directory $newDir: $!\n";
    }
    foreach my $file (@{ SNAP_FILES() }) {
       if (! -e "$newDir/$file") {
           link($file, $newDir."/$file") or die "Cannot link $file to $newDir: $!\n";
       }
    }
    my $orgDir = cwd();
    chdir $newDir or die "Cannot change to $newDir: $!\n";
    open my $f, '>snap.input' or die "Cannot write snap.input in $newDir: $!\n";
    print $f $snapInput;
    my $bsnap = SNAP_FILES->[0];
    system("./$bsnap". ' snap.input > snapOut.log 2>&1') == 0 or die "system ./$bsnap snap.input in $newDir failed: $?";
    system("perl felt2diana.pl  --tag=snap --omitDiana") == 0
        or die "Cannot run felt2diana in $newDir";
    # cleanup
    foreach my $file (@{ SNAP_FILES() }) {
        #unlink $file;
    }
    unlink 'snap.felt';
    chdir $orgDir or die "Cannot change to $orgDir: $!\n";
}

# return runs with runid = epoch-seconds
sub createRuns {
    my ($start, $end, $hours) = @_;
    my ($syear, $smon, $sday) = split '-', $start;
    my ($eyear, $emon, $eday) = split '-', $start;

    my $gmstart = timegm(0,0,0,$sday,$smon-1,$syear);
    my $gmend = timegm(0,0,0,$eday,$emon-1,$eyear);

    my $secsPerHour = 60*60;
    my $secsPerDay = $secsPerHour*24;
    while ($gmstart <= $gmend) {
        push @runs, map {$gmstart + $_*$secsPerHour} @$hours;
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
GRAPHICS.OFF
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
FIELD.OUTPUT= snap.felt
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

  bulkModleRunner --inputType=ec --inputDir=/disk1/K27/Ecmet --outputDir=/tmp/results
                  --startDate=2013-08-01 --dailyStart=0,12 --endDate=2013-10-01
                  --runTime=96
                  --sourceTermFile=snapSourceTerm.txt --meteoSetupFile=snapMeteoSetup.txt
                  -n 8

=head1 DESCRIPTION

Run the snap model in parallel (-n times) starting at startDate and finish at endDate, with
dailyStart start-hours. inputType can be ec (IFS-emep-meteo in netcdf) or hirlam (felt).

The sourceTermFile.txt should contain all information about the source-term and
will be added as is to the snap.input file.

The modelSetupFile.txt should contain the information about the meteo-model, e.g. domain,
vertical-levels etc.

=head1 AUTHOR

Heiko Klein, E<lt>Heiko.Klein@met.noE<gt>

=head1 SEE ALSO

L<perl>, L<snap>

=cut
