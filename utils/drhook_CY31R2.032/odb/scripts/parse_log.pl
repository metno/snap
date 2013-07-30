#!/usr/bin/perl
# Usage: parse_log.pl [-f] [-u] [log_file]
# -f : "Flat" view of times (default is hierarchical)
# -u : UnSorted by times (default is sorted)
# [log_file] : Makefile output (default is "log.make")

use strict;
use Getopt::Std;

my $usage="
Usage: parse_log.pl [-f] [-u] [log_file]
   -f : \"Flat\" view of times (default is hierarchical)
   -u : UnSorted by times (default is sorted)
   [log_file] : Makefile output (default is \"log.make\")\n";


my $LogFile="log.make";
my $ResultsStyle="path";
my $Sorted="yes";

my ($line,$type,$time,$major,$minor,%StartTime,%Times,%Count,
    $Name,$FullName,$count,$mcount,$majorlen,$minorlen,$Type,
    $ARCH,%MajorNames,%SubTimes,%SubCount,$SubName,$Path,$FullNameSl,
    $Last,%PathTimes,%PathCount,%PathDisplayed,@SortedList,%DepthCount,
    $Depth,$RealTime,$UserTime,$SysTime);

my (%option);
getopts("fu",\%option) or die $usage;

if (defined($option{f})) {$ResultsStyle="flat"}
if (defined($option{u})) {$Sorted="no"}

if (defined $ARGV[0]) {$LogFile=$ARGV[0]}

if (-r $LogFile) {
  open(LOGFILE,"< $LogFile");
  while(defined($line=<LOGFILE>)) {
    if (! $ARCH) {
      ($ARCH)=($line=~/ARCH\s*=\s*(\w+)/);
    }
    
    if ($line =~ /^real\s+/) {
      ($RealTime)=($line=~/^real\s+(\S+)/);
    }elsif ($line =~ /^user\s+/) {
      ($UserTime)=($line=~/^user\s+(\S+)/);
    }elsif ($line =~ /^sys\s+/) {
      ($SysTime)=($line=~/^sys\s+(\S+)/);
    }
    
    ($type,$major,$minor,$time)=
      ($line=~/^Timestamp\s+(Start|End)\s*:\s*(.*?)\s*:\s*(.*?)\s*:\s*(.*)\s*$/);
    $major=~s/\s/_/g;
    $minor=~s/\s/_/g;
    
    if ($minor) {
      $FullName="$major:$minor";
      $FullNameSl="$major/$minor";
    } else {
      $FullName="$major";
      $FullNameSl="$major";
    }
    
    if ($type eq "Start") {
      $StartTime{$major}=$time;
      $StartTime{$FullName}=$time;
      $Path.="/".$FullNameSl;
    } elsif ($type eq "End") {
      $MajorNames{$major}=1;
      if (length($major)>$majorlen) {$majorlen=length($major)}
      if (length($minor)>$minorlen) {$minorlen=length($minor)}
      $Times{$major}+=($time-$StartTime{$FullName});
      $Count{$major}++;
      #print "$major : $Times{$major} / $Count{$major}\n";
      if ($FullName ne $major) {
        $Times{$FullName}+=($time-$StartTime{$FullName});
        $Count{$FullName}++;
      }
      $PathTimes{$Path}+=($time-$StartTime{$FullName});
      $PathCount{$Path}++;
      #print " $Path : $PathTimes{$Path} : $PathCount{$Path}\n";
      if ($Path !~ /\/$FullNameSl$/) {
        print "Path error : $Path : $FullNameSl\n";
      } else {
        $Path =~ s/(\/$FullNameSl)$//;
      }
      #print "$major($minor) : $Times{$FullName} / $Count{$FullName}\n";
    }      
  }
  close(LOGFILE);
} else {
  die "Logfile \"$LogFile\" not found.\n";
}

if ($majorlen < 8 ) {$majorlen=8}
if ($minorlen < 8 ) {$minorlen=8}

print "Results for $ARCH\n\n";
if ($RealTime) {print "Real time : $RealTime\n"}
if ($UserTime) {print "User time : $UserTime\n"}
if ($SysTime) {print "Sys  time : $SysTime\n\n\n"}

if ($ResultsStyle eq "flat") {
  if ($Sorted eq "no") {
    for $Type ("Short","Full") {
      $count=0;
      print "Flat View : Unsorted Results ($Type)\n\n";
      if ($Type eq "Short") {
        printf "| %-4s | %-".$majorlen."s | %5s | %8s |\n",
               "Idx","Name","Cnt","Time";  
      } else {
        printf "| %-9s | %-".$majorlen."s |  %-".$minorlen."s | %5s | %8s |\n",
               "Idx","Name","Sub Div","Cnt","Time";
      }
      for $Name (sort (keys(%Times))) {
        if ($Name =~ /:/) {
          ($major,$minor)=($Name=~/(.*?):(.*)/);
          $mcount++;
        } else {
          $major=$Name;
          $minor="-";
          $count++;
          $mcount=0;
        }

        if (($Type eq "Short") && ($minor eq "-")) { 
          printf "| %4i | %-".$majorlen."s | %5i | %8.2f |\n",
                 $count, $major,$Count{$Name},$Times{$Name};    
        } elsif ($Type eq "Full") {
          printf "| %4i.%-4i | %-".$majorlen."s |  %-".$minorlen."s | %5i | %8.2f |\n",
                 $count, $mcount, $major,$minor,$Count{$Name},$Times{$Name};
        }
      }
      print "\n\n";
    }
  }

  if ($Sorted eq "yes") {
    for $Type ("Short","Full") {
      $count=0;
      print "Flat View : Sorted Results ($Type)\n\n";
      if ($Type eq "Short") {
        printf "| %-4s | %-".$majorlen."s | %5s | %8s |\n",
               "Idx","Name","Cnt","Time";  
      } else {
        printf "| %-9s | %-".$majorlen."s |  %-".$minorlen."s | %5s | %8s |\n",
               "Idx","Name","Sub Div","Cnt","Time";
      }
      for $Name (sort { $Times{$b} <=> $Times{$a} } (keys(%MajorNames))) {
        $count++;
        if ($Type eq "Short") {
          printf "| %4i | %-".$majorlen."s | %5i | %8.2f |\n",
                 $count, $Name,$Count{$Name},$Times{$Name};
        }elsif ($Type eq "Full") {
          %SubTimes=();
          %SubCount=();
          for $SubName (keys(%Times)) {
            if ($SubName =~ /^$Name/) {
              $SubTimes{$SubName}=$Times{$SubName};
              $SubCount{$SubName}=$Count{$SubName};
            }
          }
          $mcount=0;
          for $SubName (sort { $SubTimes{$b} <=> $SubTimes{$a} } (keys(%SubTimes))) {
            $mcount++;
            if ($SubName =~ /:/) {
              ($major,$minor)=($SubName=~/(.*?):(.*)/);
            } else {
              $major=$SubName;
              $minor="-";
            }
            printf "| %4i.%-4i | %-".$majorlen."s |  %-".$minorlen."s | %5i | %8.2f |\n",
                   $count, $mcount, $major,$minor,$SubCount{$SubName},$SubTimes{$SubName};
          }
        }        
      }
      print "\n\n";
    }
  } 

} else {

  if ($Sorted eq "no") {
    $count=0;
    print "Hierarchical View : Sorted by Section Time\n\n";
    printf "| %-4s | %-5s | %-8s | %-60s |\n",
            "Idx","Cnt","Time","Name";
    for $Path (sort { $PathTimes{$b} <=> $PathTimes{$a}} (keys(%PathTimes))) {
      $count++;
      printf "| %4i | %5i | %8.2f | %-60s |\n",
             $count,$PathCount{$Path},$PathTimes{$Path},$Path;
    }
    print "\n\n";
  }

  if ($Sorted eq "yes") {
    print "Hierarchical View : Fully Sorted Results\n\n";
    $count=0;
    printf "| %-8s | %-5s | %-8s | %-60s |\n",
            "Depth.Idx","Cnt","Time","Name";
    for $Path (sort { $PathTimes{$b} <=> $PathTimes{$a}} (keys(%PathTimes))) {
      push(@SortedList,$Path);
    }
    $Depth=1;
    ShowPath(""," ");
    print "\n\n";
  }
}

sub ShowPath {
  my $PathRoot=shift;
  my $Indent=shift;
  my (%Paths,$Path,$ShortPath,$First);
  $First="+--";
  for $Path (@SortedList) {
    if ((! $PathDisplayed{$Path}) && ($Path =~ /^$PathRoot\//)) {
      #print "$PathRoot : $Path\n";
      #if ($Path =~ /^$PathRoot\//) {print "OK\n"}      
      ($ShortPath=$Path)=~s/^$PathRoot\///;
      #if ($ShortPath eq $Path) {print "***$PathRoot : $Path\n"} else {print "---$PathRoot : $Path\n"}
       $DepthCount{$Depth}++;
       printf "| %4i.%-4i | %5i | %8.2f | %-60s |\n",
              $Depth,$DepthCount{$Depth},$PathCount{$Path},$PathTimes{$Path},$Indent.$First.$ShortPath;
      $PathDisplayed{$Path}=1;
      #$First="  ";
      $Depth++;
      $DepthCount{$Depth}=0;
      ShowPath($Path,$Indent."!   ");
      $Depth--;
    }
  }
}
