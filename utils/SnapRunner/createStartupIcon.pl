#! /usr/bin/perl
use strict;
use warnings;

use FindBin;
use Cwd qw(abs_path);
umask 0000; # make sure all files are writable by all

my $path = abs_path($FindBin::Bin);
my $desktop = `$path/printDesktop.sh`;

my $file = $desktop."/SNAPrunner.desktop";
print STDERR "create starter in $file\n";
open my $df, ">$file" or die "Cannot create $file: $!\n";
print $df <<"EOT";
[Desktop Entry]
Version=0.1
Name=SNAPrunner
Comment=Startup script to run snap
Exec=$path/snapRunner.sh
Icon=$path/radioMapIcon.png
Terminal=false
Type=Application
Categories=Utility;Application;
EOT

close($df);
chmod 0755, $file;


my $startScript = $path . "/snapRunner.sh";
print STDERR "create startScript in $startScript\n";
open my $sf, ">$startScript" or die "Cannot create $startScript: $!\n";
print $sf <<"EOT";
#! /bin/sh

cd $path
gnome-terminal -e 'perl snapRunner.pl' --title 'SNAP runner log'&
sleep 2;
xdg-open http://localhost:8081/snaprunner/&
EOT
close $sf;
chmod 0777, $startScript;
