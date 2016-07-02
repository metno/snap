#! /usr/bin/perl -w

use Test::More tests => 4;
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
                              user => "x",
                              PPIuser => 'heikok',
                              PPIhost => 'vis-m2',
                               } ];
my $PPIdir = Snap::create_ppi_dir($remote_hosts_and_users, ['/lustre/storeB/project/fou/kl/snap/nrpa_runs/test'.$$]);
$remote_hosts_and_users->[0]{PPIdir} = $PPIdir;

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


SKIP: {
    skip "cannot create PPI directory", 2 unless $PPIdir;

    my ($error, @files) = SnapEC::run_model(\%smsdirs, $remote_hosts_and_users, '',
            'Hartlepool-20162306-0924', 'SNAP');
    ok($error == 0, "running model: SNAP");

    ok(-f "$Bin/data/Hartlepool-20162306-0924_hi_res_SNAP2ARGOS.zip", "SNAP2ARGOS file created");
    unlink "$Bin/data/Hartlepool-20162306-0924_hi_res_SNAP2ARGOS.zip";
    # cleanup
    #ok(Snap::system_ppi($remote_hosts_and_users, "rm -r $PPIdir") == 0, "cleanup");
};

eval {
    delete $remote_hosts_and_users->[0]{PPIuser};
    my ($error, @files) = SnapEC::run_model(\%smsdirs, $remote_hosts_and_users, '',
            'Hartlepool-20162306-0924', 'SNAP');
};
ok($@, "run_model dies on missing PPI variables");

