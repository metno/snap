#! /usr/bin/perl -w

use Test::More tests => 8;
use strict;
use warnings;

use FindBin qw( $Bin );
use lib "$Bin/../lib/";

BEGIN {
    use_ok('Snap', 'put_statusfile', 'system_ppi', 'create_ppi_dir');
}

$Snap::DEBUG = 1;
my %smsdirs = (data => 't/data',
               work => 't/work',
               etc  => 'etc/');

my $remote_hosts_and_users = [ { host => "x",
                              user => "x",
                              PPIuser => 'heikok',
                              PPIhost => 'vis-m1'} ]; # PPIhost usually set by SMS: Misc/Funcs.pm GET_PPI_LOGIN_HOST
my $testdir = "/tmp/testdir";

put_statusfile( \%smsdirs, $remote_hosts_and_users, '',
            'blub', 'SNAP', 409 );

ok(-f 'sftp.input');
unlink 'sftp.input';

{
    my $file = 'blub_SNAP_status';
    open my $fh, $file
        or die "Cannot read $file: $!\n";
    local $/ = undef;
    my $content = <$fh>;
    ok( $content =~ /409/, $file." contains 409");
    close $fh;
    unlink $file;
}


ok(system_ppi($remote_hosts_and_users, "ls -l") == 0, "system ppi: ls -l");
ok(system_ppi($remote_hosts_and_users, "ls -l /xxx") != 0, "system ppi fail: ls -l /xxx");

system_ppi($remote_hosts_and_users, "rmdir $testdir");
ok(create_ppi_dir($remote_hosts_and_users, [qw(/xxx/nodir), $testdir]) eq $testdir, "create $testdir");

$remote_hosts_and_users->[0]{PPIdir} = $testdir;
ok(system_ppi($remote_hosts_and_users, "ls -l") == 0, "ls in existing $testdir");
system_ppi($remote_hosts_and_users, "rmdir $testdir");
ok(system_ppi($remote_hosts_and_users, "ls -l") != 0, "ls in non existing $testdir");
