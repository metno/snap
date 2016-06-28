#!/usr/bin/perl -w

##########################################################################
# CHANGE LOG:
#    2002-12-12::"Anstein Foss" <ansteinf>: Testing!
#    2003-02-03::"Kjersti Røkenes" <kjerstir>: Use new version of
#			read_argos.pl (with Getopt::Long) 
#    2003-02-06::"Anstein Foss" <ansteinf>: correcting...
#    2003-06-10::"Anstein Foss" <ansteinf>: changes...
#    2003-06-11::"Anstein Foss" <ansteinf>: changes...
#    2003-06-20::"Anstein Foss" <ansteinf>: extended the Argos grid 
#    2003-08-22::"Anstein Foss" <ansteinf>: Argos/MLDP0 version
#    2003-10-21::"Anstein Foss" <ansteinf>: snap.time file
#    2003-12-18::"Anstein Foss" <ansteinf>: MAX.PARTICLES.PER.RELEASE=1000
#    2008-10-20::<arildb>: ported to debian sarge
##########################################################################

use strict;
use Getopt::Long;

my $options = {};
GetOptions ( $options,
	     "conffile=s",
	     "staticindir=s",
	     "dynamicindir=s",
	     "outputfile=s",
	     "runident=s" )
  or die ("$0 Parsing options failed\n");

my $conffile = $options->{conffile};
my $static_inputfiles_dir = $options->{staticindir};
my $dynamic_inputfiles_dir = $options->{dynamicindir};
my $outputfile = $options->{outputfile};
my $run_ident = $options->{runident};

my $inputfile1= "$dynamic_inputfiles_dir/$run_ident" . "_MLDP0_iso";
my $inputfile2= "$dynamic_inputfiles_dir/$run_ident" . "_MLDP0_src";
my $inputfile3= "$dynamic_inputfiles_dir/$run_ident" . "_MLDP0_input";
my $inputfile4= "$static_inputfiles_dir/snap.input_FIXED_PART_1";
my $inputfile5= "$static_inputfiles_dir/snap.input_FIXED_PART_2";

#-----------------------------------------------------------

print ("\n");

open (DAT,$conffile) or ( $!=409, die "OPEN ERROR: $conffile");

print ("READ $conffile\n");

my ( @name, @halftime, @halftimeunit, @component, %nameindex );

while (<DAT>) {
    chomp;
    my @parts= split(/\s+/);
    if ($#parts == 6) {
	print ("$parts[0]\t$parts[1]\t$parts[2]\t$parts[3]\t$parts[4]\t$parts[5]\t$parts[6]\n");
	push(@name,     $parts[0]);
	#push(@radius,   $parts[1]);
	#push(@drydepvel,$parts[2]);
	#push(@decayrate,$parts[3]);
	push(@halftime, $parts[4]);
	push(@halftimeunit,$parts[5]);
	push(@component,$parts[6]);

	$nameindex{$parts[0]} = $#name;
    }
}

close(DAT);

#-----------------------------------------------------------

print ("\n");

open (ISO,$inputfile1) or ( $!=402, die "OPEN ERROR: $inputfile1");

print ("READ $inputfile1\n");

my ( @compname, @compident );

while (<ISO>) {
    chomp;
    s/\s//g;
    my @parts= split(/,/);
    print ("\t @parts\n");
    if ($#parts == 1) {
	push(@compname, $parts[0]);
	push(@compident,$parts[1]);
    }
}

close(ISO);

#-----------------------------------------------------------

my ( %compbqname, %compbq);

for (my $i=0; $i<=$#compname; $i++) {
    $compbqname{$compident[$i]} = $compname[$i];
    $#{ $compbq{$compident[$i]} } = -1;
}

print ("\n");

open (SRC,$inputfile2) or ( $!=403, die "OPEN ERROR: $inputfile2");

print ("READ $inputfile2\n");

my $line= 0;
my ( @tstart, @duration, @height, @kw );

while (<SRC>) {
    chomp;
    s/\s//g;
    my @parts= split(/,/);
    print ("\t @parts\n");
    $line++;
    if ($line == 1) {
	push(@tstart,  $parts[0]);
	push(@duration,$parts[1]);
	foreach my $comp (@compident) {
	    push ( @{ $compbq{$comp} }, "0.0");
	}
    } elsif ($line == 2) {
	push(@height,$parts[0]);
	push(@kw,    $parts[1]);
    } elsif ($parts[0]<0) {
	$line = 0;
    } else {
	${ $compbq{$parts[0]} }[$#{ $compbq{$parts[0]} }]= $parts[1];
}
}

close(SRC);

push(@height,$height[$#height]);
push(@kw,    $kw[$#kw]);

print ("\n");
print ("===========================================\n");
print ("tstart:   @tstart\n");
print ("duration: @duration\n");
print ("height:   @height\n");
print ("kw:       @kw\n");
foreach my $comp (@compident) {
    print ("COMP $comp $compbqname{$comp} : @{ $compbq{$comp} }\n");
    my $n= $nameindex{ $compbqname{$comp} };
    print ("\tnameindex=$n\n");
    print ("\t\tname=$name[$n]  ht=$halftime[$n]$halftimeunit[$n] c=$component[$n]\n");
}
print ("===========================================\n");

#-----------------------------------------------------------

print ("\n");

open (INP,$inputfile3) or ( $!=401, die "OPEN ERROR: $inputfile3");

print ("READ $inputfile3\n");

my $n= 0;
my ( $latitude, $longitude, $year, $month, $day, $hour, $minute, $outputstephour );

while (<INP>) {
    chomp;
    my @parts= split(/\s+/);
    print ("\t @parts\n");
    $n++;
    if ($n==1) {
      $latitude= $parts[0];
    } elsif ($n==2) {
      $longitude= $parts[0];
    } elsif ($n==3) {
      $year=   substr($parts[0],0,4);
      $month=  substr($parts[0],4,2);
      $day=    substr($parts[0],6,2);
      $hour=   substr($parts[0],8,2);
      $minute= substr($parts[0],10,2);
    } elsif ($n==4) {
      $outputstephour= $parts[0];
    }
  }

close(INP);

print ("\n");
print ("===========================================\n");
print ("  latitude= $latitude\n");
print ("  longitude=$longitude\n");
print ("  year,month,day,hour,minute: $year $month $day $hour $minute\n");
print ("  outputstephour=$outputstephour\n");
print ("===========================================\n");

#--------------------------------------------------------------------

open (TFILE,">snap.time") or ( $!=409, die "OPEN ERROR: snap.time");
print TFILE ("$year $month $day $hour $minute\n");
close(TFILE);

#--------------------------------------------------------------------

print ("\n");

my ( @releaseradius, @releaselower );

for (my $i=0; $i<=$#height; $i++) {
    push(@releaseradius, "0.0");
    push(@releaselower,  "0.0");
}

open (RES,">$outputfile") or ( $!=409, die "OPEN ERROR: $outputfile");

print ("WRITE $outputfile\n");

open (FIX1,$inputfile4) or ( $!=409, die "OPEN ERROR: $inputfile4");
while (<FIX1>) {
    print RES ("$_");
}
close(FIX1);

print RES ("*\n");
print RES ("POSITIONS.DECIMAL\n");

print RES ("*\n");
print RES ("TIME.START= $year $month $day $hour $minute\n");
print RES ("TIME.RUN= 12d\n");
print RES ("* TIME.RELEASE= xxx\n");
print RES ("SET_RELEASE.POS= P=$latitude,$longitude\n");

print RES ("*\n");
print RES ("RANDOM.WALK.ON\n");
print RES ("BOUNDARY.LAYER.FULL.MIX.ON\n");

print RES ("*\n");
print RES ("TIME.RELEASE.PROFILE.STEPS\n");
print RES ("RELEASE.SECOND= @tstart\n");
print RES ("RELEASE.RADIUS.M= @releaseradius\n");
print RES ("RELEASE.UPPER.M= @height\n");
print RES ("RELEASE.LOWER.M= @releaselower\n");

my( $aname );

foreach my $comp (@compident) {
### print RES ("RELEASE.BQ/SEC.COMP= @{ $compbq{$comp} } '$compbqname{$comp}' \n");
    $n= $nameindex{ $compbqname{$comp} };
    $aname= $name[$n] . "(" . $component[$n] . ")";
    print RES ("RELEASE.BQ/SEC.COMP= @{ $compbq{$comp} } '$aname' \n");
}

print RES ("*\n");
print RES ("MAX.PARTICLES.PER.RELEASE= 1000\n");

foreach my $comp (@compident) {
    $n= $nameindex{ $compbqname{$comp} };
    print RES ("*\n");
    ### print RES ("COMPONENT= $name[$n]\n");
    ### print RES ("* $name[$n] is $component[$n]\n");
    $aname= $name[$n] . "(" . $component[$n] . ")";
    print RES ("COMPONENT= $aname\n");
    if ($component[$n] eq "noblegas") {
      print RES ("DRY.DEP.OFF\n");
      print RES ("WET.DEP.OFF\n");
    } elsif ($component[$n] eq "gas") {
      print RES ("DRY.DEP.ON\n");
      print RES ("WET.DEP.ON\n");
      print RES ("DRY.DEP.HEIGHT= 44.\n");
      print RES ("DRY.DEP.RATIO=  0.164\n");
      print RES ("WET.DEP.RATIO=  0.07\n");
    } else {			# aerosol
      print RES ("DRY.DEP.ON\n");
      print RES ("WET.DEP.ON\n");
      print RES ("DRY.DEP.HEIGHT= 44.\n");
      print RES ("DRY.DEP.RATIO=  0.04\n");
      print RES ("WET.DEP.RATIO=  0.2\n");
    }
    print RES ("RADIOACTIVE.DECAY.ON\n");
    if ($halftimeunit[$n] eq "[m]") {
      print RES ("HALF.LIFETIME.MINUTES= $halftime[$n]\n");
    } elsif ($halftimeunit[$n] eq "[h]") {
      print RES ("HALF.LIFETIME.HOURS= $halftime[$n]\n");
    } elsif ($halftimeunit[$n] eq "[d]") {
      print RES ("HALF.LIFETIME.DAYS= $halftime[$n]\n");
    } else {
      print RES ("HALF.LIFETIME.YEARS= $halftime[$n]\n");
    }
    print RES ("GRAVITY.OFF\n");
    print RES ("FIELD.IDENTIFICATION= $comp\n");
  }

my $file1= $run_ident . "_MLDP0_depo";
my $file2= $run_ident . "_MLDP0_conc";
my $file3= $run_ident . "_MLDP0_dose";

print RES ("*\n");
print RES ("* ARGOS.OUTPUT.OFF\n");
print RES ("ARGOS.OUTPUT.ON\n");
print RES ("ARGOS.OUTPUT.DEPOSITION.FILE=    $file1\n");
print RES ("ARGOS.OUTPUT.CONCENTRATION.FILE= $file2\n");
print RES ("ARGOS.OUTPUT.TOTALDOSE.FILE=     $file3\n");
print RES ("ARGOS.OUTPUT.TIMESTEP.HOUR= $outputstephour\n");
print RES ("*\n");

open (FIX2,$inputfile5) or ( $!=409, die "OPEN ERROR: $inputfile5");
while (<FIX2>) {
  print RES ("$_");
}
close(FIX2);

close(RES);

print ("done.............................\n");
