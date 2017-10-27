#! /usr/bin/perl -w
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

use PDL;
use PDL::NetCDF;
use File::Copy qw(copy);
use Fcntl qw(O_RDWR);


my $input = "$ARGV[0]";
my $output = "$ARGV[1]";
if (@ARGV == 0) {
    print STDERR "usage: $0 INPUT OUTPUT\n";
    exit(1);
}

my $inNc = openFiles($input);

my @vars = map { ($_ =~ /^conc.accum_/) ? $_ : ()}  
                @{ $inNc->getvariablenames };
my $outNc = createOuputNc($input, $output, \@vars);

my $pdl;
foreach my $v (@vars) {
    if (defined $pdl) {
        $pdl += $inNc->get($v);
    } else {
        $pdl = $inNc->get($v);
    }
}
#print $pdl->info, "\n";
my $data = $pdl->reorder(2,0,1); # time is inner dimension now
# accumulate
#my $hours = $data->dim(0);
#for (my $i = 1; $i < $data->dim(0); $i++) {
#    my $previous = $i-1;
#    $data->slice("$i,:,:,:") += $data->slice("$previous,:,:,:");
#}
my $maxHours = $data->dim(0);
print STDERR $data->info, "\n", $maxHours, "\n";
my $threshold = 1;
my $arrived = $data->where($data >= $threshold); #arrived
my $flying = $data->where($data < $threshold); # still flying
$arrived .= 0;
$flying .= 1;
$data = $data->sumover; # removing time->values = flight-hours to arrival
$data += 1;

$data->where($data > $maxHours) .= $outNc->getatt('_FillValue', 'total_toa');
my @dims = qw(i j time);
$outNc->putslice('total_toa', \@dims, [$data->dims, 1], [0,0,0], [$data->dims, 1], $data);


sub createOuputNc {
    my ($input, $output, $vars) = @_;
    my @selectVariables = map {"--extract.selectVariables=$_"} $vars->[0];
    system("fimex --input.file=$input --extract.reduceDimension.name=time --extract.reduceDimension.start=0 --extract.reduceDimension.end=0 @selectVariables --output.file=$output") == 0
        or die "Cannot copy $input to $output";
    foreach my $v ($vars->[0]) {
        system("ncrename -h -v $v,total_toa $output");
        system("ncatted", "-h", "-a", "units,total_toa,o,c,hours", $output);
    }
    my $nc = PDL::NetCDF->new ($output, {REVERSE_DIMS => 1, MODE => O_RDWR});
    die "cannot readwrite $output: $!" unless $nc;
    return $nc;
}

sub openFiles {
    my ($f) = @_;
    my $nc = PDL::NetCDF->new ($f, {REVERSE_DIMS => 1});
    if (!$nc) {
        die "couldn't read $f: $!\n";
    }
    return $nc;
}

