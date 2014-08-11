#! /usr/bin/perl
use strict;
use warnings;

use Getopt::Long qw(GetOptions);
use Pod::Usage qw(pod2usage);
use XML::LibXML;

use vars qw(%Args);
GetOptions(\%Args,
       'debug',
       'help',
       'levelnames=s',
       'xmlTemplate=s',
       'output=s')
    or pod2usage(2);

pod2usage(-exitval => 0,
          -verbose => 2) if $Args{help};

pod2usage(-exitval => 2,
          -msg => "missing levelnames-file") unless -f $Args{levelnames};
pod2usage(-exitval => 2,
          -msg => "missing xmlTemplate-file") unless -f $Args{xmlTemplate};


my %isotopes = readIsotopes($Args{levelnames});
my $felt2ncDoc = XML::LibXML->load_xml(location => $Args{xmlTemplate});

modifyDoc($felt2ncDoc, \%isotopes);
$felt2ncDoc->toFile($Args{output},1);

sub readIsotopes {
    my ($file) = @_;
    open (my $f, $file) or die "Cannot read levelnames file $file: $!";
    my %isotopes;
    while (defined (my $line = <$f>)) {
        if ($line =~ /^\s+(\d+)\s+"([^"]+)"/) {
            $isotopes{$1} = $2;
        } else {
            warn "Cannot parse line in $file: $line";
        }
    }
    return %isotopes;
}

sub modifyDoc {
    my ($doc, $isotopes) = @_;

    for (my $var = 500; $var < 540; ++$var) {
        my $xpath = '/cdm_felt_config/variables/parameter[@id="'.$var.',0"]';
        my @nodes = $doc->findnodes($xpath);
        if (@nodes) {
            foreach my $levelId (keys %$isotopes) {
                my $newNode = $nodes[0]->cloneNode(1);
                my ($idAttr) = $newNode->findnodes('@id');
                my $val = $idAttr->getValue() . ",$levelId";
                $idAttr->setValue($val);
                my ($nameAttr) = $newNode->findnodes('@name');
                my $name = $nameAttr->getValue() . '_' .$isotopes->{$levelId};
                $nameAttr->setValue($name);
                print STDERR "new node: $val -> $name \n" if $Args{debug};

                #$doc->insertBefore($newNode, $nodes[0]);
                $nodes[0]->parentNode()->addChild($newNode);
                print STDERR $newNode->toString(1) if $Args{debug};
            }
            $nodes[0]->unbindNode();
        }
    }

    # model levels
    foreach my $var (540, 570) {
        my $xpath = '/cdm_felt_config/variables/parameter[@id="'.$var.'"]';
        my @nodes = $doc->findnodes($xpath);
        if (@nodes) {
            foreach my $isoId (keys %$isotopes) {
                my $newNode = $nodes[0]->cloneNode(1);
                my ($idAttr) = $newNode->findnodes('@id');
                my $val = $idAttr->getValue() + $isoId;
                $idAttr->setValue($val);
                my ($nameAttr) = $newNode->findnodes('@name');
                my $name = $nameAttr->getValue() . '_' .$isotopes->{$isoId};
                $nameAttr->setValue($name);
                print STDERR "new node: $val -> $name \n" if $Args{debug};

                #$doc->insertBefore($newNode, $nodes[0]);
                $nodes[0]->parentNode()->addChild($newNode);
                print STDERR $newNode->toString(1) if $Args{debug};
            }
            $nodes[0]->unbindNode();
        }
    }


}


__END__

=head1 NAME

felt2ncDummyLevels2Isotopes.pl - TBD

=head1 SYNOPSIS

felt2ncDummyLevels2Isotopes.pl --levelnames=snap.felt_level_names --xmlTemplate=felt2nc_snap_dummy_levels.xml --output=felt2nc_snap.xml

=head1 DESCRIPTION

Add the level-names from to the felt2nc-input to prepare a input file for fimex/diana
with real isotopes instead of dummy levels.

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
