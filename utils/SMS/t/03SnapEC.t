#! /usr/bin/perl -w

use Test::More tests => 10;
use strict;
use warnings;
use FindBin qw( $Bin );
use lib "$Bin/../lib/";



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
                              PPIhost => 'vis-m1',
                               } ];
my $PPIdir = Snap::create_ppi_dir($remote_hosts_and_users, ['/lustre/storeB/project/fou/kl/snap/nrpa_runs/test'.$$]);
$remote_hosts_and_users->[0]{PPIdir} = $PPIdir;

my $time = time;
my (undef,undef,undef,$mday, $mon, $year) = gmtime($time);
$mon++;$year += 1900;
my $date = sprintf("%04d-%02d-%02d", $year, $mon, $mday);
my (undef,undef,undef,$ymday, $ymon, $yyear) = gmtime($time - 24*60*60);
$ymon++;$yyear += 1900;
my $yesterday = sprintf("%04d-%02d-%02d", $yyear, $ymon, $ymday);
foreach my $file ("$Bin/data/Hartlepool-20162306-0924_Rimsterm.xml", "$Bin/data/TestBackModeling_Rimsterm.xml", "$Bin/data/TestBackModeling_SNAP_request.xml") {
    # fix date of test-input
    open my $fh, "$file.org" or die "Cannot read $file.org: $!\n";
    local $/ = undef;
    my $content = <$fh>;
    $content =~ s/2017-08-02/$yesterday/g;
    $content =~ s/2017-08-03/$date/g;
    close $fh;
    open my $nfh, ">$file" or die "Cannot write $file: $!\n";
    print $nfh $content;
    close $nfh;
}
{
    my $date2 = sprintf("%04d%02d%02d", $year, $mon, $mday);
    # fix date of test-input
    foreach my  $file ("$Bin/data/Brokdorf_test_2_TRAJ_input", "$Bin/data/backward2_TRAJ_input") {
        open my $fh, "$file.org" or die "Cannot read $file.org: $!\n";
        local $/ = undef;
        my $content = <$fh>;
        $content =~ s/20130411/$date2/g;
        close $fh;
        open my $nfh, ">$file" or die "Cannot write $file: $!\n";
        print $nfh $content;
        close $nfh;
    }
}


SKIP: {
    skip "cannot create PPI directory", 6 unless $PPIdir;
    my ($error, @files);

    # dispersion
    ($error, @files) = SnapEC::run_model(\%smsdirs, $remote_hosts_and_users, '',
            'Hartlepool-20162306-0924', 'SNAP');
    ok($error == 0, "running model: SNAP");

    ok(-f "$Bin/data/Hartlepool-20162306-0924_hi_res_SNAP2ARGOS.zip", "SNAP2ARGOS file created");
    unlink "$Bin/data/Hartlepool-20162306-0924_hi_res_SNAP2ARGOS.zip";

    # backward dispersion
    ($error, @files) = SnapEC::run_model(\%smsdirs, $remote_hosts_and_users, '',
            'TestBackModeling', 'SNAP');
    ok($error == 0, "running backward model: SNAP");

    ok(-f "$Bin/data/TestBackModeling_hi_res_SNAP2ARGOS.zip", "SNAP2ARGOS file created");
    unlink "$Bin/data/TestBackModeling_hi_res_SNAP2ARGOS.zip";
    

    # forward trajectory
    my $ident = 'Brokdorf_test_2';
    ($error, @files) = SnapEC::run_model(\%smsdirs, $remote_hosts_and_users, '',
            $ident, 'TRAJ');
    ok($error == 0, "running model: TRAJ");

    ok(-f "$Bin/data/$ident\_TRAJ2ARGOS.zip", "TRAJ2ARGOS file created");
    unlink "$Bin/data/$ident\_TRAJ2ARGOS.zip";


    # backward trajectory
    $ident = 'backward2';
    ($error, @files) = SnapEC::run_model(\%smsdirs, $remote_hosts_and_users, '',
            $ident, 'TRAJ');
    ok($error == 0, "running model: TRAJ");

    ok(-f "$Bin/data/$ident\_TRAJ2ARGOS.zip", "TRAJ2ARGOS file created");
    unlink "$Bin/data/$ident\_TRAJ2ARGOS.zip";


    # cleanup
    #ok(Snap::system_ppi($remote_hosts_and_users, "rm -r $PPIdir") == 0, "cleanup");
};

eval {
    delete $remote_hosts_and_users->[0]{PPIuser};
    my ($error, @files) = SnapEC::run_model(\%smsdirs, $remote_hosts_and_users, '',
            'Hartlepool-20162306-0924', 'SNAP');
};
ok($@, "run_model dies on missing PPI variables");

