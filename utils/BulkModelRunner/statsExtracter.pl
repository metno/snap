#! /usr/bin/perl
use strict;
use warnings;

use Getopt::Long qw(GetOptions);
use Pod::Usage qw(pod2usage);
use PDL::Lite;
use PDL::NetCDF;
use Fcntl qw(O_RDONLY O_RDWR);
use File::Copy qw(cp);

use vars qw(%Args $countryFraction);
GetOptions(\%Args,
       'dataDir=s',
       'countryFraction=s',
       'components=s',
       'debug',
       'help')
    or pod2usage(2);

pod2usage(-exitval => 0,
      -verbose => 2) if $Args{help};
pod2usage(-exitval => 2,
      -msg => 'missing components',
      -verbose => 0) unless $Args{components};
pod2usage(-exitval => 2,
      -msg => 'missing dataDir',
      -verbose => 0) if ! -d $Args{dataDir};
$countryFraction = countryFraction($Args{countryFraction});

parseDir($Args{dataDir}, $Args{components}, $countryFraction);

sub countryFraction {
    my ($countryOpt) = @_;
    my ($countryFile, $countryVar) = split ':', $countryOpt;
    pod2usage(-exitval => 2,
      -msg => 'missing countryFraction file: $countryFile',
      -verbose => 0) if ! -f $countryFile;
    my $countryNc = PDL::NetCDF->new($countryFile, {MODE => O_RDONLY, REVERSE_DIMS => 1})
        or die "unable to read countryFraction: $countryFile";
    my $pdl = $countryNc->get($countryVar);
    return $pdl;
}

sub parseDir {
    my ($dir, $comps, $fracPdl) = @_;
    print join (",", "#date", "avg.dep.[Bq/m2]", "dep[Bq]"), "\n";
    my @components = split ',', $comps;
    opendir (my $dh, $dir) or die "Cannot open dir $dir: $!";
    my @dirs = grep { /^[^.]/ && -d "$dir/$_" } readdir $dh;
    closedir $dh;

    my $probPdl;
    my $count = 0;
    my $tmpl;
    foreach my $dirtag (sort @dirs) {
        my $curdir = "$dir/$dirtag";
        next unless -d $curdir;
        if (! -f "$curdir/snap.nc") {
            print STDERR "no snap.nc in $curdir, skipping\n";
            next;
        }
        $count++;
        my $dataNc = PDL::NetCDF->new("$curdir/snap.nc", {MODE => O_RDONLY, REVERSE_DIMS => 1})
            or die "unable to read nc-file: $curdir/snap.nc";
        my $cell_area = $dataNc->get('cell_area');
        my $countryArea = $cell_area * $fracPdl/100;
        my $dep;
        foreach my $comp (@components) {
            my $d = $dataNc->get($comp)->slice(":,:,(0)");
            if (defined $dep) {
                $dep += $d;
            } else {
                $dep = $d;
            }
        }
        unless (defined $probPdl) {
            $tmpl = "$curdir/snap.nc";
            $probPdl = $dep->copy;
            $probPdl .= 0;
        }
        $probPdl += ($dep > 0);
        my $cellDep = $dep*$countryArea;
        my $totalDep = $cellDep->sum;
        my $m2Dep = $totalDep / $countryArea->sum;
        print join (",", $dirtag, $m2Dep, $totalDep), "\n";
    }
    my $outFile = "$dir/snapProb.nc";
    cp($tmpl, $outFile) or die "Cannot write $outFile: $!\n";
    my $outNc = PDL::NetCDF->new($outFile, {MODE => O_RDWR, REVERSE_DIMS => 1})
       or die "unable to read nc-file: $outFile";
    $outNc->put('accum.tdep.prob',['rlon','rlat'], $probPdl/$count);
    $outNc->putatt('longitude latitude','coordinates','accum.tdep.prob');
    $outNc->putatt('grid_mapping','projection_3','accum.tdep.prob');
    $outNc->putatt('1','units','accum.tdep.prob');
    $outNc->close();
}



__END__

=head1 NAME

statsExtracter.pl - extract statistics from a set of netcdf-files

=head1 SYNOPSIS

=head1 DESCRIPTION

=head2 GENERAL SYNTAX

=over 4

=item

=back

=head1 EXAMPLE

=head1 AUTHOR

Heiko Klein, E<lt>Heiko.Klein@met.noE<gt>

=head1 SEE ALSO

L<perl>.

=cut
