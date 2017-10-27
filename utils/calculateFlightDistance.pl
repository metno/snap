#! /usr/bin/perl
#
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
use strict;
use warnings;
use constant PI    => 4 * atan2(1, 1);
use constant DEG2RAD => PI/180;
use PDL::NetCDF;
use PDL::Lite;
use Fcntl qw(O_RDONLY O_RDWR);
use Getopt::Long qw(GetOptions);
use Pod::Usage qw(pod2usage);

use vars qw($debug $help $input $param $lat $lon $binDist);
$binDist = 10;
GetOptions('debug' => \$debug,
       'help' => \$help,
       'input=s' => \$input,
       'parameter=s' => \$param,
       'binDist=i' => \$binDist,
       'latitude=f' => \$lat,
       'longitude=f' => \$lon)
    or pod2usage(2);

pod2usage(-exitval => 0,
      -verbose => 2) if $help;
pod2usage(-exitval => 2,
      -message => 'missing netcdf-file --input',
      -verbose => 0) if (! -f $input);
pod2usage(-exitval => 2,
      -message => 'missing parameter',
      -verbose => 0) unless defined $param;
pod2usage(-exitval => 2,
      -message => 'missing latitude',
      -verbose => 0) unless defined $lat;
pod2usage(-exitval => 2,
      -message => 'missing longitude',
      -verbose => 0) unless defined $lon;

my $nc = PDL::NetCDF->new($input, {MODE => O_RDONLY, REVERSE_DIMS => 1})
    or die "Cannot read nc-file $input: $!\n";
my $latPdl = $nc->get('latitude');
my $lonPdl = $nc->get('longitude');
my $dist = greatCircleDistance($lat, $lon, $latPdl, $lonPdl);

my $area = $nc->get('cell_area');
my $time = $nc->get('time');
my ($nx, $ny) = $area->dims;
my $lastTime = ($time->dims)[0]-1;
my $data = $area->zeroes;
foreach my $p (split ',', $param) {
    $data += $nc->get($p, [0,0,$lastTime], [$nx, $ny, 1]);
}
my $dataBq = $data->slice(":,:,(0)") * $area;
#my ($min, $max) = $dist->minmax;
#print STDERR "lastTime=$lastTime, distMinMax=$min,$max\n";


my %outputBins;
my %maxBins;
my %binAreas;
for (my $i = 0; $i < $nx; $i++) {
    for (my $j = 0; $j < $ny; $j++) {
        my $dep = $data->at($i, $j);
        my $depBq = $dataBq->at($i, $j);
        if ($dep >= 1) {
            my $d = $dist->at($i, $j);
            my $dBin = int($d/$binDist)*$binDist; # bins of 10km
            #print join " ", $dBin, $i, $j, $dep;
            #print "\n";
            $outputBins{$dBin} += $depBq;
            $maxBins{$dBin} = 0 unless exists $maxBins{$dBin};
            $maxBins{$dBin} = ($dep > $maxBins{$dBin}) ? $dep : $maxBins{$dBin};
            $binAreas{$dBin} += $area->at($i,$j);
        }
    }
}

print join ("\t", "#name", "km", "avg [Bq/m2]", "max [Bq/m2]", "[km2]"), "\n";
foreach my $dBin (sort {$a <=> $b} keys %outputBins) {
    print join ("\t", $param, $dBin, $outputBins{$dBin}/$binAreas{$dBin}, $maxBins{$dBin}, $binAreas{$dBin}/1e6), "\n";
}

# calculate the great circle distance from
# $lat, $lon to all points in $latPdl and $lonPdl
# return pdl with distances in km
sub greatCircleDistance {
    my ($lat, $lon, $latPdl, $lonPdl) = @_;

    $lat *= DEG2RAD;
    $lon *= DEG2RAD;
    $latPdl = DEG2RAD * $latPdl->copy->double;
    $lonPdl = DEG2RAD * $lonPdl->copy->double;

    return 6371 * acos(sin($lat) * sin($latPdl) + cos($lat) * cos($latPdl) * cos ($lonPdl - $lon));
}

sub acos { return atan2( sqrt(1 - $_[0] * $_[0]), $_[0] ); }

__END__

=head1 NAME

calculateFlightDistance.pl - calculate different flight distances

=head1 SYNOPSIS

  calculateFlightDistance.pl  --input=gremikhaParticles.nc --binDist=50 --parameter=accum.wet.dep_Total,accum.dry.dep_Total --latitude=68.07 --longitude=39.47


=head1 DESCRIPTION

=head2 GENERAL SYNTAX

=over 4

=item

=back

=head1 EXAMPLE

 calculateFlightDistance.pl  --input=gremikhaParticles.nc --binDist=50 --parameter=accum.wet.dep_Total,accum.dry.dep_Total --latitude=68.07 --longitude=39.47

=head1 AUTHOR

Heiko Klein, E<lt>Heiko.Klein@met.noE<gt>

=head1 SEE ALSO

L<perl>.

=cut
