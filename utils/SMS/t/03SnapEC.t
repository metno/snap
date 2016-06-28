#! /usr/bin/perl -w

use Test::More tests => 1;
use strict;
use warnings;
use FindBin qw( $Bin );
use lib "$Bin/../job/";



BEGIN {
    use_ok('SnapEC');
}


$ENV{PATH} = join(':', ("$Bin/../binprecise/", $ENV{PATH}));
$Snap::DEBUG = 1;
my %smsdirs = (data => "$Bin/data",
               work => "$Bin/work",
               etc  => "$Bin/../etc/");

my $remote_hosts_and_users = [ { host => "x",
	                          user => "x" } ];

my (undef,undef,undef,$mday, $mon, $year) = gmtime(time);
$mon++;$year += 1900;
my $date = sprintf("%04d-%02d-%02d", $year, $mon, $mday);
{
    # fix date of test-input
    my $file = "$Bin/data/Hartlepool-20162306-0924_Rimsterm.xml";
    open my $fh, "$file.org" or die "Cannot read $file.org: $!\n";
    local $/ = undef;
    my $content = <$fh>;
    $content =~ s/2016-06-23/$date/g;
    close $fh;
    open my $nfh, ">$file" or die "Cannot write $file: $!\n";
    print $nfh $content;
    close $nfh;
}

SnapEC::run_model(\%smsdirs, $remote_hosts_and_users, '', 
			'Hartlepool-20162306-0924', 'SNAP');
