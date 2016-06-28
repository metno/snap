#! /usr/bin/perl -w

use Test::More tests => 3;
use strict;
use warnings;

use FindBin qw( $Bin );
use lib "$Bin/../job/";

BEGIN {
    use_ok('Snap', 'put_statusfile');
}

$Snap::DEBUG = 1;
my %smsdirs = (data => 't/data',
               work => 't/work',
               etc  => 'etc/');

my $remote_hosts_and_users = [ { host => "x",
	                          user => "x" } ];

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
