#! /usr/bin/perl

use File::Copy;

while (defined (my $ARGV = shift @ARGV)) {
    copy($ARGV, $ARGV.".bak") or die "Cannot copy $ARGV: $!\n";
    open(IN, $ARGV.".bak") or die "Cannot read $ARGV.bak: $!\n";
    open(OUT, ">$ARGV") or die "Cannot write $ARGV: $!\n";
    while (defined (my $line = <IN>)) {
        $line =~ s/iavail\(\s*1\s*,\s*([^)]+)\)/iavail($1)%aYear/ig;
        $line =~ s/iavail\(\s*2\s*,\s*([^)]+)\)/iavail($1)%aMonth/ig;
        $line =~ s/iavail\(\s*3\s*,\s*([^)]+)\)/iavail($1)%aDay/ig;
        $line =~ s/iavail\(\s*4\s*,\s*([^)]+)\)/iavail($1)%aHour/ig;
        $line =~ s/iavail\(\s*5\s*,\s*([^)]+)\)/iavail($1)%fcHour/ig;
        $line =~ s/iavail\(\s*6\s*,\s*([^)]+)\)/iavail($1)%fileNo/ig;
        $line =~ s/iavail\(\s*7\s*,\s*([^)]+)\)/iavail($1)%fileType/ig;
        $line =~ s/iavail\(\s*8\s*,\s*([^)]+)\)/iavail($1)%oHour/ig;
        $line =~ s/iavail\(\s*9\s*,\s*([^)]+)\)/iavail($1)%nAvail/ig;
        $line =~ s/iavail\(\s*10\s*,\s*([^)]+)\)/iavail($1)%pAvail/ig;
        print OUT $line;

#        if ($line =~ /iavail/) {
#            print $line;
#        }
    }
}
