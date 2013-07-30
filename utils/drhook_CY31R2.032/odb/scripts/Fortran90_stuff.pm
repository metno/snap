package Fortran90_stuff;
use strict;
use warnings;
require Exporter;
use Data::Dumper;

$Data::Dumper::Indent = 1;

our($nest_par);
our @ISA      = qw(Exporter);
our @EXPORT   = qw(study setup_parse pre_tidy remove_macro expcont 
                   process_include_files tidy tidy_decl getvars 
		   find_unused_vars remove_unused_vars doctor_viol
		   fix_doctor_viol various cont_lines f90_indent
		   writefile readfile create_interface_block
		   add_interface_blocks change_var_names insert_hook
		   remake_arg_decl remove_some_comments parse_prog_unit
		   get_calls_inc
		   $study_called $name $nest_par);


#==========================================================================
sub study{
# Study statements and put attributes into array $statements
# Attributes assigned:
# $href->{content}       - What statement it is
# $href->{decl}       - true if declaration,
#                       5 means statement function
#                       4 means USE statement,
#                       2 means type decleration
#                       3 means FORMAT statement
#                       1 means the rest
# $href->{in_contain} - true while in internal procedure(s)
# $href->{exec}       - true if executable statement
#                     - 2 means first executable statement in program unit
#                     - 3 means last executable statement in program unit
#                     - 23 means first and last executable statement in program unit
# $href->{prog_unit}  - program unit number (numbered from 0)
# $href->{number}     - statement number (numbered from 0)
 
# Further attributes will be assigned later (action attributes)

  my($statements,$prog_info) = @_;

  our ($name,$nest_par);
  my ($unit_name,@args,$prog_unit,$href,@punit,$current_punit);
  my ($content,$decl,$exec);
  my($type_def)=0;
  my($unit_count)=-1;
  @punit=();
  $current_punit='';
  my $number=-1;
  my $in_contain=0;
  my $in_interface=0;
  my $contain_host='';
  my $current_unit_name='';
  our($study_called);
#  if(! $study_called) {
#    $$prog_info{has_interface_block}=0;
#  }
# Initial "parsing" loop

  foreach $href (@$statements) {
    $href->{in_contain}=$in_contain;
    $href->{contain_host}=$contain_host if($in_contain);
    $number++;
    $_=$href->{statement};
    $content='unknown';
    my $content2='';
    $decl=0;
    $exec=0;

    if($type_def) {
 #     $href->{content}='typedef';
    }
    
# Comment
CRACK:    {
      if(/^\s*!/ || /^\s*$/) {
	$content='comment';
	last CRACK;
      }
      $_=uc($_)  unless(/^\#/);
      s/^[ \t]*//;
      s/\!.*\n/\n/g;                        # Remove trailing comments in all lines
#      print "AA $_";
 
# Program name statement 
      if($content eq 'unknown' and ! $in_interface) {
	$prog_unit=&parse_prog_unit(\$unit_name,\@args);
	if($prog_unit) {
          $current_unit_name=$unit_name;
	  $content=uc($prog_unit);
	  push(@punit,$prog_unit);
	  $current_punit=$prog_unit;
	  $unit_count++;
	  if(! $study_called) {
	    $$prog_info{'unit_name'}[$unit_count]=uc($unit_name);
	    $$prog_info{'unit_name'}[$unit_count]=uc($unit_name);
#	  $$prog_info{'tokens'}[$unit_count]=[];
	    if($prog_unit eq 'module') {
	      $$prog_info{'is_module'}=1;
	      $$prog_info{'module_name'}=$unit_name;
	    }
	  }
	  last CRACK;
	}
      }
      if($content eq 'unknown') {
	$decl=0;
	$exec=1;
# Executable constructs
	&study_exec(\$content,$prog_info,\$study_called);
	if($content eq 'IF') {
	  s/^IF\s*$nest_par\s*//;
	  &study_exec(\$content2,$prog_info,\$study_called);
	}
      }
      

      if($content eq 'unknown') {
# Specification statemnts
	$exec=0;
	$decl=1;
	if(/^USE\b/) {
	  $content='USE';
	  $decl=4;
	}
	elsif(/^INTEGER\b/) {
	  $content='INTEGER';
	  $decl=2;
	}
	elsif(/^REAL\b/) {
	  $content='REAL';
	  $decl=2;
	}
	elsif(/^LOGICAL\b/) {
	  $content='LOGICAL';
	  $decl=2;
	}
	elsif(/^CHARACTER\b/) {
	  $content='CHARACTER';
	  $decl=2;
	}
	elsif(/^DOUBLE PRECISION\b/) {
	  $content='DOUBLE PRECISION';
	  $decl=2;
	}
	elsif(/^COMPLEX\b/) {
	  $content='COMPLEX';
	  $decl=2;
	}
	elsif(/^TYPE *\(/) {
	  $content='type_decl';
	  $decl=2;
	}
	elsif(/^ALLOCATABLE\b/) {
	  $content='ALLOCATABLE';
	}
	elsif(/^COMMON\b/) {
	  $content='COMMON';
	}
	elsif(/^DATA\b/) {
	  $content='DATA';
	}
	elsif(/^DIMENSION\b/) {
	  $content='DIMENSION';
	}
	elsif(/^EQUIVALENCE\b/) {
	  $content='EQUIVALENCE';
	}
	elsif(/^EXTERNAL\b/) {
	  $content='EXTERNAL';
	}
	elsif(/^\d+\s+FORMAT\b/) {
	  $content='FORMAT';
	  $decl=3;
	}
	elsif(/^IMPLICIT\b\s+NONE\b/) {
	  $content='IMPLICIT NONE';
	}
	elsif(/^IMPLICIT\b/) {
	  $content='IMPLICIT';
	}
	elsif(/^INTENT\b/) {
	  $content='INTENT';
	}
	elsif(/^INTRINSIC\b/) {
	  $content='INTRINSIC';
	}
	elsif(/^NAMELIST\b/) {
	  $content='NAMELIST';
	}
	elsif(/^OPTIONAL\b/) {
	  $content='OPTIONAL';
	}
	elsif(/^PARAMETER\b/) {
	  $content='PARAMETER';
	}
	elsif(/^POINTER\b/) {
	  $content='POINTER';
	}
	elsif(/^PUBLIC\b/) {
	  $content='PUBLIC';
	}
	elsif(/^PRIVATE\b/) {
	  $content='PRIVATE';
	}
	elsif(/^SAVE\b/) {
	  $content='SAVE';
	}
	elsif(/^TARGET\b/) {
	  $content='TARGET';
	}
	elsif(/^SEQUENCE\b/) {
	  $content='SEQUENCE';
	}
	elsif(/^INTERFACE\b/) {
	  $content='INTERFACE';
	  if(! $study_called) {
	    $$prog_info{has_interface_block}=1;
	    $in_interface=1;
	  }
	}
	elsif(/^END ?INTERFACE\b/) {
	  $content='END INTERFACE';
	    $in_interface=0;
	}
	elsif(/^TYPE *[^\( ]/i) {
	  $content='type_def';
	  $type_def=1;
	}
	elsif(/^END TYPE\b/){
	  $content='type_def';
	  $type_def=0;
	}
        elsif( $in_interface ) {
          if(/^MODULE PROCEDURE\b/) {
            $content='MODULE PROCEDURE';
          }
        }
      }
# Other constructs
      if($content eq 'unknown') {
	$decl=0;
	$exec=0;
	
	if(/^CONTAINS\b/) {
	  $content='CONTAINS';
	  $in_contain=1;
          $contain_host=uc($current_unit_name);
	  if(! $study_called) {
	    $$prog_info{has_contain}=1;
	    $$prog_info{containing}=1;
	  }
	}
	elsif(/^\#include\b/) {
	  $content='include';
	  if(! $study_called) {
	    $$prog_info{has_include}=1;
	  }
	}
	elsif(/^\#/) {
	  $content='cpp';
	}
	elsif(/^\@/) {
	  $content='compiler_directive';
	}
	
	else{
	  if(/^END\b/) {
	    $prog_unit=pop(@punit);
	    $content='END '.uc($prog_unit);
	    if($in_contain) {
	      unless(@punit) {
		$unit_count=0;
		$href->{in_contain}=0;
		$in_contain=0;
	      }
	    }
	  }
	} 
      }
    }
    
    if($in_interface and $content ne 'INTERFACE') {
      $content='in_interface';
      $exec=0;
      $decl=1;
    }

#    print "BB $unit_count $content $_";
    if($content  eq 'unknown') {
      print STDERR "Failed to crack statement starting at line $href->{first_line}",
      " - syntax error?? \n";
      print STDERR " $_ ";
#      print STDERR "study_called $study_called in_interface $in_interface \n";
#      print STDERR Dumper($statements);
      die "Failed in study";
    }
#    unless($content eq 'comment') {
#      my @tmpvar=/\b$name\b/g;
#      my $i=0;
#      foreach my $tmp (@tmpvar){ 
#	$href->{'tokens'}[$i]=$tmp;
#	$i++;
#	if(! $study_called and $unit_count > -1) {
#	  $$prog_info{'token_hash'}[$unit_count]{$tmp}++;
#	}
#      }
#    }
               
    $href->{content}=$content;
    $href->{content2}=$content2 if($content2);
    $href->{decl}=$decl;
    $href->{exec}=$exec;
#    $href->{type_decl}=$type_decl;
    $href->{prog_unit}=$unit_count;
    $href->{number}=$number;
    unless($content eq 'comment') {
      $href->{multi_line} = 1 if(tr/\n// > 1);
    }
  }


# Find first executable statement in each program unit
# Also repair statement functions wrongly assigned as executable
  my $prev_unit_count=-2;
  my $stat_func_suspicion=0;
  my @lastexec=();

  foreach $href (@$statements) {
    $exec=$href->{exec};
    $unit_count=$href->{prog_unit};
    if($exec) {
      if($unit_count > $prev_unit_count) {
	$content=$href->{content};
	if($content eq 'array_assign') {
	  $stat_func_suspicion=1;
	  $_=$href->{statement};
	  if(/^\s*$name\s*\(\s*:/){
	    $stat_func_suspicion=0;
#	    print " A $_";
	   } 
	  elsif(/^\s*$name\s*\(\s*$name\s*:/){
	    $stat_func_suspicion=0;
#	    print " B $_";
	  } 
	  elsif(/^\s*$name\s*\(\s*\d+/){
	    $stat_func_suspicion=0;
#	    print " C $_";
	  }
	  else {
	    $href->{exec}=0;
	    $href->{decl}=5;
	    $href->{content}='statmf';
#	    print " D $_";
	    next;
	  }
	}
	$href->{exec}=2;
	$prev_unit_count=$unit_count;
	$content=$href->{content};
      }
      $lastexec[$unit_count]=$href->{number}  unless ($unit_count < 0);  
# No prog_unit assigned, include file?
    }
  }

# Assign last executable statement
  if(@lastexec) {
    foreach my $last (@lastexec) {
      if(defined ($last)) {
	if($$statements[$last]->{exec} == 1) {
	  $$statements[$last]->{exec}=3;
	}
	else{
	  $$statements[$last]->{exec}=23;
	}      
      }
    }
  }
# Consistency checks
  my $fail=0;
  my $prev_exec=0;
  $prev_unit_count=-1;
  foreach $href (@$statements) {
    $content=$href->{content};
    next if($content eq 'comment');
    $unit_count=$href->{prog_unit};
    $exec=$href->{exec};
    $decl=$href->{decl};
    if($unit_count == $prev_unit_count) {
      if($decl and $prev_exec) {
	unless ($content eq 'FORMAT' | $content eq 'DATA' ) {
	  print STDERR "$href->{first_line} $href->{statement}";
	  print STDERR 'Decleration after executable statement';
	  $fail=1;
	}
      }
    }
    $prev_unit_count=$unit_count;
    $prev_exec=$exec;
  }
  if($fail) {
    foreach $href (@$statements) {
      $content=$href->{content};
      $unit_count=$href->{prog_unit};
      $exec=$href->{exec};
      $decl=$href->{decl};
      print "$content $unit_count $exec $decl $href->{statement}";
    }
    die 'FAIL';
  }
  $study_called=1;
}

#==========================================================================
sub study_exec{
  my($content,$prog_info,$study_called) = @_;
  our ($name,$nest_par);
  if(/^(\w+\s*:\s*)*IF\s*$nest_par\s*THEN/) {
    $$content='IF_construct';
  }
  elsif(/^ELSE ?IF *\(/) {
    $$content='ELSEIF';
  }
  elsif(/^ELSE\b\s*($name)*/) {
    $$content='ELSE';
  }
  elsif(/^END ?IF\b\s*($name)*/) {
    $$content='ENDIF';
  }
  elsif(/^($name\s*:\s*)*DO\b/) {
    $$content='DO';
  }
  elsif(/^END ?DO\b/) {
    $$content='ENDDO';
  }
  elsif(/^ALLOCATE\b/) {
    $$content='ALLOCATE';
  }
  elsif(/^ASSIGN\b/) {
    $$content='ASIGN';
  }
  elsif(/^BACKSPACE\b/) {
    $$content='BACKSPACE';
  }
  elsif(/^CALL\b/) {
    $$content='CALL';
    if(!$$study_called) {
      $$prog_info{no_calls}++;
    }
  }
  elsif(/^CLOSE\b/) {
    $$content='CLOSE';
  }
  elsif(/^(\d+)*\s*CONTINUE\b/) {
    $$content='CONTINUE';
  }
  elsif(/^CYCLE\b/) {
    $$content='CYCLE';
  }
  elsif(/^DEALLOCATE\b/) {
    $$content='DEALLOCATE';
  }
  elsif(/^ENDFILE\b/) {
    $$content='ENDFILE';
  }
  elsif(/^EXIT\b/) {
    $$content='EXIT';
  }
  elsif(/^GO ?TO\b/) {
    $$content='GOTO';
  }
  elsif(/^IF\s*\(/) {
    $$content='IF';
  }
  elsif(/^INQUIRE\b/) {
    $$content='INQUIRE';
  }
  elsif(/^NULLIFY\b/) {
    $$content='NULLIFY';
  }
  elsif(/^OPEN\b/) {
    $$content='OPEN';
  }
  elsif(/^PAUSE\b/) {
    $$content='PAUSE';
  }
  elsif(/^PRINT\b/) {
    $$content='PRINT';
  }
  elsif(/^READ\b/) {
    $$content='READ';
  }
  elsif(/^READ\b/) {
    $$content='READ';
  }
  elsif(/^RETURN\b/) {
    $$content='RETURN';
  }
  elsif(/^REWIND\b/) {
    $$content='REWIND';
  }
  elsif(/^STOP\b/) {
    $$content='STOP';
  }
  elsif(/^WHERE\s*$nest_par\s*$name.*=/) {
    $$content='WHERE';
  }
  elsif(/^WRITE\s*\(/) {
    $$content='WRITE';
  }
  elsif(/^($name\s*:\s*)*SELECT\s?CASE\b/) {
    $$content='SELECT CASE';
  }
  elsif(/^CASE\b/) {
    $$content='CASE';
  }
  elsif(/^END ?SELECT\b/) {
    $$content='END SELECT';
  }
  elsif(/^WHERE *\(/) {
    $$content='WHERE_construct';
  }
  elsif(/^ELSE ?WHERE\b/) {
    $$content='ELSEWHERE';
  }
  elsif(/^END ?WHERE\b/) {
    $$content='ENDWHERE';
  }
  elsif(/^$name\s*=/o) {                                 #ZVAR = ....
    $$content='scal_assign';
  }
  elsif(/^$name\s*$nest_par\s*=/o) {                     #ZARR(JL) = ....
    $$content='array_assign';
  }
  elsif(/^$name\s*%\s*$name\s*=/o) {                     #ZMYTYPE%ICOMP = .....
    $$content='scal_assign';
  }
  elsif(/^$name\s*$nest_par\s*%\s*$name\s*=/o) {         #ZMYTYPE(JK)%ICOMP = .....
    $$content='array_assign';
  }
  elsif(/^$name\s*%\s*$name\s*$nest_par\s*=/o) {         #ZMYTYPE%ICOMP(JL) = .....
    $$content='array_assign';
  }
  elsif(/^($name\s*($nest_par)*\s*%\s*$name\s*($nest_par)* *)+=/o) { #ZMYTYPE(JK)%ICOMP(JL) = ...
    $$content='array_assign';
  }
  elsif(/^$name\s*($nest_par)($nest_par)\s*=/o) {        #CLNAME(JK)(1:5) = ......
    $$content='array_assign';
  }
}
#===================================================================================
sub pre_tidy {

# Initial tidying to make the rest work

  my($lines)=@_;
  foreach (@$lines) {

# Substitute tab with four blanks
    s/\t/    /g;
    s/^ *INTEGER /INTEGER_M /i;
    s/^ *REAL /REAL_B /i;
  }
}
#==========================================================================
sub remove_macro {

# Remove INTEGER_M, _ONE_ etc. macros and replace by expanded statement

  my($lines)=@_;

  my($im)=1; # Until I start checking include files
  my($ia)=0;
  my($ib)=0;
  my($rb)=1; # Until I start checking include files
  my($is)=0;
  my($rh)=0;
  my($rm)=0;
  my(@pars,$string);
  for (@$lines) {
    next if(/^ *$/ | /^ *!/);
# The following two substitutions should be restored at end of processing
    s/(\'[^!]*)!+(.*\')/$1\£$2/;   # Protect against mischief 
    s/(["][^!]*)!+(.*["])/$1\£$2/;      # Protect against mischief
    $im=$im+/JPIM\b/i unless($im);
    $rb=$rb+/JPRB\b/i unless($rb);
    $rm=$rm+/JPRM\b/i unless($rm);
    $im=$im+s/\bINTEGER_M\b/INTEGER(KIND=JPIM)/o;
    $ia=$ia+s/\bINTEGER_A\b/INTEGER(KIND=JPIA)/o;
    $ib=$ib+s/\bINTEGER_B\b/INTEGER(KIND=JPIB)/o;
    $is=$is+s/\bINTEGER_S\b/INTEGER(KIND=JPIS)/o;
    $rb=$rb+s/\bREAL_B\b/REAL(KIND=JPRB)/o;
    $rh=$rh+s/\bREAL_H\b/REAL(KIND=JPRH)/o;
    $rm=$rm+s/\bREAL_M\b/REAL(KIND=JPRM)/o;
    $rb=$rb+s/\b_ZERO_\b/0.0_JPRB/og;
    $rb=$rb+s/\b_ONE_\b/1.0_JPRB/og;
    $rb=$rb+s/\b_TWO_\b/2.0_JPRB/og;
    $rb=$rb+s/\b_HALF_\b/0.5_JPRB/og;
  }
  @pars=();
  push(@pars,"JPIM") if $im;
  push(@pars,"JPRB") if $rb;
  push(@pars,"JPRM") if $rm;
  push(@pars,"JPIA") if $ia;
  push(@pars,"JPIB") if $ib;
  push(@pars,"JPIS") if $is;
  ($string=join('     ,',@pars))=~s/ *$//;
  for (@$lines) {
    next unless (/^\#/);
    if(@pars) {
      s/^#include +"tsmbkind.h"/USE PARKIND1  ,ONLY : $string/ ;
    }
    else {
      s/^#include +"tsmbkind.h"//;
    }
#    if($rh) {
      s/^#include +"hugekind.h"/USE PARKIND2  ,ONLY : JPRH/ ;
#    }
#    else {
#      s/^#include +"hugekind.h"// ;
#    }
  }
}

#==========================================================================
sub readfile  {
# Read file 
  my($fname)=@_;
  my(@lines);
  if(!open(INFIL,$fname)) {
    print STDERR "Can't open $fname for reading\n";
    die("Can't open $fname for reading\n");
  }
  @lines=<INFIL>;
  close INFIL;
  (@lines);
}

#==========================================================================
sub writefile  {
# Write file
  my($fname,$lines)=@_;
  if(!open(OUTFIL,">".$fname)) {
    print STDERR "Can't open $fname for writing\n";
    exit;
  }
  print OUTFIL @$lines;
  close OUTFIL;
}

#==========================================================================
sub expcont {
#
# Expand continuation lines into statements for free-format Fortran while
# maintaining line-breaking and all comments
# Put statements onto array of references to anonymous hashes as key 'statement'
# Also put into the hash the linenumber of first line of statement as key 'first_line'
  my($lines,$statements) = @_;
  my($statm,$prev,$rec,$line,$first_line);
  $prev=0;
  my $line_number=0;
  foreach $line (@$lines) {
    $_=$line;
    $line_number++;
    if(/^ *&/) {
      s/^( *)&(.*)$/$1$2/s;
##      print " 1 $_";
    }
    if( !/^ *!.*$/ && /^.+&(?: *!.*)* *$/) {
      s/(.+)&(.+)/$1$2/s;
      $statm.=$_;
      $first_line=$line_number unless($prev);
      $prev=1;
##      print " 2 $_";
      next;
    }
    elsif($prev && /^ *!/) {
      $statm.=$_;
##      print " 3 $_";
      next;
    }
    elsif($prev && /^ *$/) {
##      print " 4 $_";
      next;
    }
    else {
##      print " 5 $_";
      $first_line=$line_number unless($prev);
      $statm.=$_;
      {
	my $rec={};
	$rec->{'statement'}=$statm;
	$rec->{'first_line'}=$first_line;
	push(@$statements,$rec);
      }
      $statm = "";
      $prev=0;
    }
  }
}
#==========================================================================

sub cont_lines {
#
# Put back continuation character in correct place and execute delayed actions
#
  my($statements,$lines,$line_hash) = @_;
  my(@temp,$i,$iup,$href);


# Put back continuation characters and split statements into lines as they were
  @$lines=();
  @$line_hash=();
  foreach $href (@$statements) {
    $_=$href->{statement};
    if (/\n.*\n/){                      # This is a multi-line statement
      @temp=split /\n/;                 # Split statement into lines (removes EOL)
      $iup=scalar(@temp);               # Number of lines in statement
      for ($i=0;$i < $iup;$i++) {       # Loop through lines
	$_=$temp[$i];
	if($i == 0 ){                   # First line
	  if(/^([^!]+)(!.*)$/) {        # Line has trailing comment
	    s/^([^!]+)(!.*)$/$1&$2\n/;  # Put back & at end of line before comment
	  }
	  else {                        # No trailing comment
	    s/^([^!]+)$/$1&\n/;         # Put back & and EOL at end of line
	  }	    
	}
	elsif ($i == ($iup-1)) {        # Last line
	  s/^( *)(.*)$/$1& $2 \n/;      # Put back & at beginning of line
	}
	else {                          # Other lines
	  if (/^ *!/) {                 # Line is comment line
	    $_=$_."\n";                 # Restore EOL for comments
	  }
	  else {
	    if(/^( *)([^!]*)(!.*)$/) {  # Line has trailing comment
	      s/^( *)([^!]*)(!.*)*$/$1& $2&$3\n/;  # & at beginning and end of line
	    }
	    else {                      # No trailing comment
	      s/^( *)([^!]*)$/$1& $2&\n/; # & at beggining and end of line
	    }	 
	  }  
	}
	if($i == 0        && exists $href->{pre_insert}) {
	  my @templines=split('\n',$href->{pre_insert});
	  foreach my $tline (@templines) {
	    my $rec={};
	    $rec->{'content'}='unknown';
	    $rec->{'line'}=$tline."\n";
	    push(@$lines,$rec->{'line'});
	    push(@$line_hash,$rec);
	  }
	}
	unless(exists $href->{remove}) {
	  my $rec={};
	  $rec->{'line'}=$_;
	  if($i == 0) {
	    $rec->{'content'}=$href->{content};
	  }
	  else {
	    $rec->{'content'}='cont_line';
	  }
	  push(@$lines,$rec->{'line'});
	  push(@$line_hash,$rec);
	}
	if($i == ($iup-1) && exists $href->{post_insert}) {
	  my @templines=split('\n',$href->{post_insert});
	  foreach my $tline (@templines) {
	    my $rec={};
	    $rec->{'content'}='unknown';
	    $rec->{'line'}=$tline."\n";
	    push(@$lines,$rec->{'line'});
	    push(@$line_hash,$rec);
	  }
	}
      }
    }
    else {  # Not multiline statement
      if(exists $href->{pre_insert}) {
	my @templines=split('\n',$href->{pre_insert});
	foreach my $tline (@templines) {
	  my $rec={};
	  $rec->{'content'}='unknown';
	  $rec->{'line'}=$tline."\n";
	  push(@$lines,$rec->{'line'});
	  push(@$line_hash,$rec);
	}
      }
      unless(exists $href->{remove}) {
	my $rec={};
	$rec->{'line'}=$_;
	$rec->{'content'}=$href->{content};
	push(@$lines,$rec->{'line'});
	push(@$line_hash,$rec);
#	print $rec;
      }
      if(exists $href->{post_insert}) {
	my @templines=split('\n',$href->{post_insert});
	foreach my $tline (@templines) {
	  my $rec={};
	  $rec->{'content'}='unknown';
	  $rec->{'line'}=$tline."\n";
	  push(@$lines,$rec->{'line'});
	  push(@$line_hash,$rec);
	}
      }
    }
  }
}
#==========================================================================
sub getvars {
# Return list of locally declared variables with type and scope information
# 
  my($statements,$prog_info,$vars,$use_vars) = @_;
  my ($test,$type,@vars1,$func,$prog_unit,$dum,$tmp_name,@pu_args);
  my ($preserve,$rank,$href);
  our($nest_par,$name);

  %$vars=();
  $func="";
  $prog_unit=0;
  %$use_vars=();
  foreach $href (@$statements) {
    next if($href->{content} eq 'comment');           # Skip comments
    next if($href->{exec});                        # Don't look in executable statements
    next if($$prog_info{is_module} and ! $href->{in_contain}); # Unless inside CONTAIN skip module 
    $prog_unit=$href->{prog_unit};
    if($href->{content} eq 'FUNCTION') {
      $_=$href->{statement};
      my $dum=&parse_prog_unit(\$func,\@pu_args);          # Get name of FUNCTION
#      print "GETVARS FUNCTION $func \n";
      $func=uc($func);
    }
    if($href->{decl} == 2 or $href->{content} eq 'EXTERNAL'){  # Real parse starts
      $_=$href->{statement};
      $_=uc($_);                                   # Upcase to avoid /.../i
      s/^ *//;                                     # remove leading blanks
      if($href->{decl} == 2) {
	$type=lc(substr($href->{content},0,1));
      }
      else {
	$type='e';
      }
      s/\!.*\n/\n/g;                               # Remove trailing comments in all lines
      $preserve=$_;
      s/(.+)::(.+)/$2/s;                           #REAL(KIND=JPRB) :: zsig(:) -> zsig(:),
      s/^EXTERNAL (.+)$/$1/;
      s/\s+//g;                                    # Remove all white-space
      if($href->{content} eq 'CHARACTER') {
	s/($name)\*\d+/$1/g;
	s/($name)\*$nest_par/$1/g;
	s/($name)$nest_par\*\w+/$1/g;
      }
      s#=\(/.+/\)##;      # ZVAL(1:2)=(/1.0,2.0/) -> ZVAL(1:2)
#?      s/=[^,\n]+//g;
      s/$nest_par//g;     # ISEC3(SIZE(NSEC3)),ISEC4(SIZE(NSEC4)) -> ISEC3,ISEC4
      s/=\w+//g;          # ZVAL=1.0 -> ZVAL
      s@/.*/@@;           # What?
      @vars1=split(',',$_);
      for(@vars1) {
	next unless /^$name$/;          # A bit of security
	if($preserve =~ /\b$_\b *\(/ | $preserve =~ /DIMENSION/) {
	  $rank=1;        # Variable is array
	}
	else {
	  $rank=0;        # Variable is scalar
	}
	if($_ eq $func) {
	  $$vars{$_}{type_spec}="f";
	} 
	else {
	  if($href->{content} eq 'FUNCTION') {
	    $$vars{$_}{type_spec}='f';
	  }
	  else {
	    $$vars{$_}{type_spec}=$type;
	  }
	}
	$$vars{$_}{scope}=$prog_unit;
	$$vars{$_}{rank}=$rank;
	$$vars{$_}{usage}='local';
      }
    }
# Perhaps the variable is really a statement function?
    if($href->{decl} == 5) {
      $_=$href->{statement};
      s/\s+//g;                                    # Remove all white-space
      /^($name)\((.+)\)=/i;
      my $tvar=uc($1);
      my @stmf_args=split(',',$2);
      if (exists($$vars{$tvar})) {
	$$vars{$tvar}{type_spec}='s';
#	print "STATMF OK $tvar \n ";
      }
      for (@stmf_args) {
	if (exists($$vars{$_})) {
	  $$vars{$_}{type_spec}='s';
#	  print "STATMF ARG OK $_ \n ";
	}
      }
    }
  }
# Perhaps instead the variable is a declaration of an external function?
  my @extract=();                  # Extract part of statements for efficiency
  foreach $href (@$statements) {
    if($href->{exec}) {                 # Function call must be in executable stat.
      next if($href->{content} eq 'CALL'); # A call can't contain an undeclared function
      push(@extract,$href->{statement});
    }
  }
  
  foreach my $var (keys (%$vars)) {
    next if($$vars{$var}{rank} > 0);   # Can't be a function if rank > 0
    next if($$vars{$var}{type_spec} eq 's' | $$vars{$var}{type_spec} eq 'f');
    my $dec_unit=$$vars{$var}{scope};
    my $regex1=qr/\b$var\b\s*\(/i;      # As var's rank=0 this could be function call
    for(@extract) {
      if(/${regex1}/) {
	s/\!.*\n/\n/g;                       # Remove trailing comments in all lines
	s/\s+//g;                            # Remove all white-space
	if(/${regex1}/) {
	  if($$vars{$var}{type_spec} eq 'c') {   # Avoid CLVAR(1:3) etc.
	    next if(/${regex1}\s*(\d+|$name)*\s*:\s*(\d+|$name)*\s*\)/);
	  }
#	  print "TYPE changed to function $var $_ \n";
	  $$vars{$var}{type_spec}='f';
	  last;
	}
      }
    }
  }
# ---------------------------------------------------------------------
# Assign  "usage" in Doctor sense to variable (default usage is 'local')
# 
  foreach $href (@$statements) {
# Is the varaible a dummy argument
    if($href->{content} eq 'FUNCTION' or $href->{content} eq 'SUBROUTINE') {
      $_=$href->{statement};
      @pu_args=();
      my $dum=&parse_prog_unit(\$func,\@pu_args);   # Get arguments
      for(@pu_args) {
	if( exists $$vars{$_} ) {
	  if($$vars{$_}{scope} == $href->{prog_unit}) {
	    $$vars{$_}{usage}='arg';
	  }
	}
	else {
	  print STDERR "Argument $_ has not got a corresponding declaration statement\n";
         print "Argument $_ has not got a corresponding declaration statement\n";
         print "Bailing out at this point\n";
         die "Bailing out";
	}
      }
    }
# Does the variable appear in a NAMELIST
# We want to distinguish this for more lenient Doctor check
    if($href->{content} eq 'NAMELIST') {
      $_=$href->{statement};
      s/\!.*\n/\n/g;     # Remove trailing comments in all lines
      s/\s+//g;          # Remove all white-space
      m:NAMELIST/\w+/(.+):;
      my @namvars=split(',',uc($1));
      for (@namvars) {
	if( exists $$vars{$_} ) {
	  if($$vars{$_}{scope} == $href->{prog_unit}) {
	    $$vars{$_}{usage}='namvar';
	  }
	}
      }
    }
    if(exists $href->{inc_statm}) { # We also have to look in include files
      my $incs=$href->{inc_statm};
      foreach my $hrefi (@$incs) {
	if($hrefi->{content} eq 'NAMELIST') {
	  $_=$hrefi->{statement};
	  s/\!.*\n/\n/g;     # Remove trailing comments in all lines
	  s/\s+//g;          # Remove all white-space
	m:NAMELIST/\w+/(.+):;
	  my @namvars=split(',',uc($1));
	  for (@namvars) {
	    if( exists $$vars{$_} ) {
	      if($$vars{$_}{scope} == $href->{prog_unit}) {
		$$vars{$_}{usage}='namvar';
	      }
	    }
	  }
	}
      }
    }
  }
# -----------------------------------------------------------------------------
# Find use variables
  my %use_count=();
  foreach $href (@$statements) {
    if($href->{content} eq 'USE') {
      $prog_unit=$href->{prog_unit};
      $_=$href->{statement};
      s/\!.*\n/\n/g;                               # Remove trailing comments in all lines
      s/\s+//g;                                    # Remove all white-space
      $_=uc($_);                                   # Upcase to avoid /.../i
      if(/^USE($name),ONLY:(.+)$/){
	my $modname=$1;
	if( exists $use_count{$modname}) {
	  if($prog_unit == $use_count{$modname}) {
	    print "-> $href->{statement}";
           print "USE $modname appears more than once in program unit $prog_unit \n\n";

	  }
	}
	$use_count{$modname} = $prog_unit;
	my @usevars = split /,/ ,$2;
	my %usevars=();
	foreach my $usevar (@usevars) {
	  $usevars{$usevar}++;
	  $$use_vars{$usevar}{module}=$modname;
	  $$use_vars{$usevar}{scope}=$prog_unit;
	  $$use_vars{$usevar}{count}++;
	}
	foreach my $usevar (keys (%usevars)) {
	  if($usevars{$usevar} >1) {
	    print "DUPLICATE USE ONLY VARIABLE ",
	    "$modname $usevar $prog_unit \n";
	    $_=$href->{statement};
	    s/\b$usevar\b//i;
	    s/,\s*,/,/;
	    s/,\s*\n$/\n/;
	    s/\n *\n/\n/;
	    s/^(.+:\s*),/$1/;
	    $href->{statement}=$_;
	  }
	}
      }
      else {
#	print "WARNING:USE without ONLY \n";
      }
    }
  }
}
#==========================================================================
sub find_unused_vars {
# Find declared variables not used
  my($statements,$vars,$unused_vars,$use_vars,$unused_use_vars) = @_;
  my ($var,@tokens,$href);
  @tokens=();
# Find all tokens in file
  foreach $href (@$statements) {
    next if($href->{content} eq 'comment');
    if(exists $href->{inc_statm}) {  # Look also in include files
      my $incs=$href->{inc_statm};
      foreach my $hrefi (@$incs) {
	die "FUV $href->{content} $href->{statement}" unless exists $hrefi->{statement};
	$_=$hrefi->{statement};
	if(/\b[a-zA-Z]\w*\b/) {
	  push(@tokens,/\b[a-zA-Z]\w*\b/g);
	}
      }
    }
    else {
      $_=$href->{statement};
      push(@tokens,/\b[a-zA-Z]\w*\b/g);
    }
  }
  @tokens= map {uc} @tokens; # Upcase array of tokens, the variables are upper-case

# Find out how many times the variable appears in array tokens
  foreach $var (keys (%$vars)) {
    $$vars{$var}{uses}=0;
  }
  foreach $var (keys (%$use_vars)) {
    $$use_vars{$var}{uses}=0;
  }
  for (@tokens) {
    if(exists($$vars{$_})){
      $$vars{$_}{uses}++; 
    }
    if(exists($$use_vars{$_})){
      $$use_vars{$_}{uses}++; 
    }
  }
# If it appears only one time (which must be in a declaration) it is unused
  @$unused_vars=();
  foreach $var (keys (%$vars)) {
    push(@$unused_vars,$var) if($$vars{$var}{uses} < 2);
  }
  @$unused_use_vars=();
  foreach $var (keys (%$use_vars)) {
    push(@$unused_use_vars,$var) if($$use_vars{$var}{uses} < 2);
  }
}
#==========================================================================
sub remove_unused_vars {
# Does what it says on the tin
  my($statements,$unused_vars,$unused_use_vars) = @_;
  my ($var,$href);
  our $nest_par;
  for (@$unused_vars) {
    $var=$_;
    foreach $href (@$statements) {
      $_=$href->{statement};
      next unless(($href->{decl}) | ($href->{content} eq 'comment'));
      if($href->{content} eq 'comment') {
	next unless(/^ *!\$OMP/);
      }
      if(/\b$var\b/i) {
#	print $_;
	
	if(/\b$var\b *\(/i) {
#	  print "ZYZ $var $_";
	  s/\b$var\b *$nest_par *(=\s*\(\/.*\/\))*//si;
#	  print "ZZZ $var $_";
	}
	s/\b$var\b\s*=\s*\d+(\.\d*)*//i;
	s/\b$var\b *(\* *\d+)*//i if($href->{content} eq 'CHARACTER') ;
	s/\b$var\b//i; 
#	print $_;
        s/^.+:: *\n$//;
        s/^.+:: *\!.*\n$//;
#	print $_;
        s/,\s*,/,/;
#	print $_;
	s/, *\n$/\n/;
#	print $_;
        s/(::\s*),(.+)$/$1$2/s;
        s/\n *\n/\n/;
        s/\n *!.*\n/\n/;
	s/, *\n$/\n/;
# Remove "empty" lines
        s/^.+::\s*$//;
        s/^.+::\s*=.*$//;
        s/^.+::\s*!.*$//;
#	print $_;
        s/^CHARACTER *\*\d+ *\n$//i if($href->{content} eq 'CHARACTER') ;
	$href->{statement}=$_;
      }
    }
  }
  for (@$unused_use_vars) {
    $var=$_;
    foreach $href (@$statements) {
      next unless($href->{decl} == 4);
      $_=$href->{statement};
      next if(/PARKIND/); #I am sure this could be done betterh

      if(/\b$var\b/i) {
	s/\b$var\b//i;
        s/,\s*,/,/;
        s/,\s*\n$/\n/;
        s/\n *\n/\n/;
	s/^(.+:\s*),/$1/;
	s/^.+:\s*$//;
	$href->{statement}=$_;
      }
    }
  }
}
#==========================================================================
sub tidy_decl {
# Tidy up declarions
  my($statements) = @_;
  my($href,$content);

  foreach $href (@$statements) {
    next unless($href->{decl} == 2);
    $_=$href->{statement};
    $content=$href->{content};
    
    if($content eq 'CHARACTER') {
      s/CHARACTER *\* *(\w+)/CHARACTER \(LEN = $1\)/i; 
      s/CHARACTER *\* *\(\*\)/CHARACTER \(LEN = \*\)/i;
      s/CHARACTER *\* *\( *(\w+) *\)/CHARACTER \(LEN = $1)/i;
    }
    if($content eq 'INTEGER') {
      if(/^ *INTEGER[^\(]/i) {
	s/INTEGER\b/INTEGER(KIND=JPIM)/;
      }
    }
    unless (/::/) {
      s/^( *LOGICAL )/$1:: /i;
      s/^( *INTEGER\(KIND=JPI\w\) )/$1:: /;
      s/^( *REAL\(KIND=JPR\w\) )/$1:: /;
      if(/^ *CHARACTER/i) {
	if( s/^( *CHARACTER *\( *LEN *= *\w+ *\))/$1 :: /i) {
	  $href->{statement}=$_;
	  next;
	}
	if(s/^( *CHARACTER *\( *LEN *= *\* *\))/$1 :: /i) {
	  $href->{statement}=$_;
	  next;
	}
	s/^( *CHARACTER )/$1:: /i;
      }
    }
    $href->{statement}=$_;
  }
}
#==========================================================================

sub doctor_viol {
# Find Doctor violations

  my($vars,$fix_doc) = @_;
  my ($var,$type,$zz,$prog_unit,$usage);
  %$fix_doc=();

  foreach $var (keys (%$vars)) {
    $type=$$vars{$var}{type_spec};
    $prog_unit=$$vars{$var}{scope};
    $usage=$$vars{$var}{usage};
#    print "DOC $var $type $prog_unit $usage \n";
    if($zz=&doc_char($type,$usage,$var)) {
#      print "DOCTOR VIOL - ",$var," $type $zz $prog_unit\n";
      $$fix_doc{$var}=$zz.'_'.$var.','.$prog_unit
    }
  }  
}
#==========================================================================

sub fix_doctor_viol {
# Fix Doctor violations
  my($statements,$fix_doc) = @_;
  my($doc_viol,$repl,$prog_unit,$cur_prog_unit,@allowed,$href,$content);
  my($tmp_name,@pu_args);

  @allowed=('NRGRI'); # Hack

  VIOL:foreach $doc_viol (keys (%$fix_doc)) {
    # Let's allow some violations
    for (@allowed){ 
      next VIOL if($doc_viol eq $_);
    }

    ($repl,$prog_unit)=split(',',$$fix_doc{$doc_viol});

    print "FIX $repl $prog_unit \n";
    foreach $href (@$statements) {
      $content=$href->{content};
      $_=$href->{statement};
      if($href->{content} eq 'comment') {
	next unless(/^ *!\$OMP/);
      }
      $cur_prog_unit=$href->{prog_unit};
      if($prog_unit == $cur_prog_unit) {  # Could be fine in other program units
	if(/\b$doc_viol\b/i) {
	  s/%$doc_viol\b/_X_$doc_viol/ig; # Protect type-components
	  s/\b$doc_viol\b/$repl/ig;
	  s/_X_$doc_viol\b/%$doc_viol/ig; # Restore type-components
	}
      }
      $href->{statement}=$_;
    }
  }
  
}
#==========================================================================
sub various{
# 
  my($statements,$prog_info,$vars) = @_;
  my($punit,@args,$tmp_name,$cont,$statm);
  my($href,$exec);
  our $nest_par;
#------------------------------------------------------------------
# Remove unneccesary RETURN statement
  foreach $href (@$statements) {
    $cont=$href->{content};
    if($cont eq 'RETURN') {
      if($href->{exec} == 3) {   # $href->{exec} == 3 means last executable statement
	$href->{remove} = 1;     # Post remove line for later
      }
    }
  }


# Make sure all CALL MPL_... has a CDSTRING argument
  foreach $href (@$statements) {
    $cont=$href->{content};
    if($href->{content} eq 'CALL' ) {
      $_=$href->{statement};
      if(/^\s*CALL\s+MPL_/i) {
	next if(/^\s*CALL\s+MPL_ABORT/i);
	next if(/^\s*CALL\s+MPL_WRITE/i);
	next if(/^\s*CALL\s+MPL_READ/i);
	next if(/^\s*CALL\s+MPL_OPEN/i);
	next if(/^\s*CALL\s+MPL_CLOSE/i);
	next if(/^\s*CALL\s+MPL_INIT/i);
 	next if(/^\s*CALL\s+MPL_END/i);
	next if(/^\s*CALL\s+MPL_GROUPS_CREATE/i);
	next if(/^\s*CALL\s+MPL_BUFFER_METHOD/i);
	next if(/^\s*CALL\s+MPL_IOINIT/i);
	next if(/^\s*CALL\s+MPL_CART_COORD/i);
	next if(/^\s*CALL\s+MPL_BARRIER/i);
	next if(/^\s*CALL\s+MPL_GETARG/i);
	next if(/^\s*CALL\s+MPL_IARGC/i);
#	print "CDSTRING=$$prog_info{'unit_name'}[$href->{prog_unit}]: \n";
	unless(/CDSTRING\s*=/i) {
	  s/\)(\s)$/,CDSTRING=\'$$prog_info{'unit_name'}[$href->{prog_unit}]:\'\)$1/;
	  $href->{statement}=$_;
	}
      }
    }
  }
	


#------------------------------------------------------------------
# Add Standard Modification Line

  my $start=0;
  foreach $href (@$statements) {
    $cont=$href->{content};
    if($cont eq 'comment') {
      $_=$href->{statement};
      if($start) {                        # Found header - look for end of mod lines
	if(/^ *$/ || /^! *------------------------/) {
	  $href->{pre_insert} = "!        M.Hamrud      01-Oct-2003 CY28 Cleaning\n";
	  last;
	}
	next;
      }
      $start=1 if(/^! +Modifications/i) ;  # This how the header should look
      next;
    }
    last if($href->{exec});                # We have failed - bail out
  }

# Change subroutine and call multi-line statements so that the comma
# beetwen variables comes at the end of the line
  my @lines=();
  foreach $href (@$statements) {
    if(exists $href->{multi_line}) {
      $cont=$href->{content};
      if($cont eq 'SUBROUTINE' | $cont eq 'CALL' ) {
	$statm=$href->{statement};
	@lines=split "\n", $statm;
	@lines = reverse @lines;
	my $append_comma=0;
	for (@lines) {
#	  print "A $append_comma $_ \n";
	  next if(/^ *!/);
	  if($append_comma) {
	    if(/\S *!.*$/) {
	      s/(\S)( *!.*)$/$1,$2/;
	    }
	    else {
	      s/(\S) *$/$1,/;
	    }
	  }
	  $append_comma=s/^ *,//;
#	  print "B $append_comma $_ \n";
	}
	@lines = reverse @lines;
	$statm=join  "\n",@lines;
	$statm=$statm."\n";
	$href->{statement}=$statm;
      }
    }
  }
  our $name;
  foreach $href (@$statements) {
    if($href->{content} eq 'USE') {
      $_=$href->{statement};
      unless(/^\s*USE\s+$name\s*,\s*ONLY\s*:/i){
#	print $_;
#	print "WARNING:USE without ONLY \n";
      }
    }
  }    
}
#==========================================================================
sub insert_hook{
# 
  my($statements,$prog_info,$vars) = @_;
  my($punit,@args,$tmp_name,$cont,$statm);
  my($href,$exec);
  our $nest_par;
#------------------------------------------------------------------
# Add HOOK function
  my $unit_name='';
  my $last_use=0;
  my $parkind1_inserted=0;
  my $hook_status=0;
  my $in_contain=0;
  my $prev_prog=0;
  my ($decl,$remember);
  foreach $href (@$statements) {
    $cont=$href->{content};
    next if($cont eq 'comment');

    $decl=$href->{decl};
    $exec=$href->{exec};
    $in_contain=$href->{in_contain};
    if(! $in_contain and $href->{prog_unit} > $prev_prog) {
      $parkind1_inserted=0;
      $hook_status=0;
      $prev_prog=$href->{prog_unit};
#      print "resetting hook status \n";
    }

    if($cont eq 'FUNCTION' or $cont eq 'SUBROUTINE' or 
       $cont eq 'PROGRAM'){ # Need name of routine
      $_=$href->{statement};
      &parse_prog_unit(\$unit_name,\@args);
      $unit_name=uc($unit_name);
# If in module pre-pend module name
      $unit_name=$$prog_info{module_name}.':'.$unit_name if($$prog_info{is_module}); 
      $remember=0;
    }

    if($hook_status == 0) {   # $hook_status == 0 means we have not done anything yet
      if($cont eq 'USE') {    # Add USE YOMHOOK as second use statement
	$parkind1_inserted=1 if ($parkind1_inserted == 0 && $href->{statement} =~ m/\bparkind1\b/i);
	$href->{post_insert}="USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK\n";
	$hook_status=1;
      }
      elsif($cont eq 'IMPLICIT NONE') { # No previous USE, add USE YOMHOOK before IMPLICIT NONE
	$href->{pre_insert} ="USE PARKIND1  ,ONLY : JPRB\n".
	  "USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK\n";
	$parkind1_inserted=1;
	$hook_status=1;
      } 
    }

    if ($parkind1_inserted == 0 && $hook_status == 1 && $cont eq 'IMPLICIT NONE') {
      # By the time we have reached IMPLICIT NONE 'USE PARKIND1' was not detected => insert
      $href->{pre_insert}="USE PARKIND1  ,ONLY : JPRB\n";
      $parkind1_inserted=1;
    }

    $remember=$href->{number} if($decl == 2); 

#   Use statement added ($hook_status == 1), now insert HOOK switch on statement
#   before first executable statement in program unit ($exec == 2)
    if($hook_status == 1 && ($exec == 2 or $exec == 23)) {
      if($remember) {
	$$statements[$remember]->{post_insert}="REAL(KIND=JPRB) :: ZHOOK_HANDLE\n";
	$href->{pre_insert}="IF (LHOOK) CALL DR_HOOK(\'${unit_name}\',0,ZHOOK_HANDLE)\n";
      }
      else {
	$href->{pre_insert}="REAL(KIND=JPRB) :: ZHOOK_HANDLE\n".
	    "IF (LHOOK) CALL DR_HOOK(\'${unit_name}\',0,ZHOOK_HANDLE)\n";
      }   
      if($cont eq 'IF') {
	if($href->{content2} eq 'RETURN') {
	  $_=$href->{statement};
	  s/(\s*IF\s*$nest_par).*\n/$1/i;
	  s/\)$/ .AND. LHOOK\)/;
	  $href->{pre_insert}=$href->{pre_insert}."$_ CALL DR_HOOK(\'${unit_name}\',1,ZHOOK_HANDLE)\n";
	}
      }
      $hook_status=2;
    }
#   Hook switched on($hook_status == 2), switch off after last executable statement
#   ($exec == 3)
    if($hook_status == 2) {
      if($exec == 3 or $exec == 23) {
	$href->{post_insert}="IF (LHOOK) CALL DR_HOOK(\'${unit_name}\',1,ZHOOK_HANDLE)\n";
	$hook_status=3;
      }
      elsif($cont eq 'RETURN') {
	$href->{pre_insert}="IF (LHOOK) CALL DR_HOOK(\'${unit_name}\',1,ZHOOK_HANDLE)\n";
      }
      elsif($cont eq 'IF') {
	if($href->{content2} eq 'RETURN') {
	  $_=$href->{statement};
	  s/(\s*IF\s*$nest_par).*\n/$1/i;
	  s/\)$/ .AND. LHOOK\)/;
	  $href->{pre_insert}="$_ CALL DR_HOOK(\'${unit_name}\',1,ZHOOK_HANDLE)\n";
	}
      }	
    }
    $hook_status=1 if($in_contain && $hook_status==3); # Reset hook status in CONTAIN region
  }
  die "Adding HOOK function failed " if($hook_status == 2);
}
#==========================================================================

sub doc_char{
# Returns suggested prefix in case of DOCTOR violation (otherwise null string)
  my($type,$usage,$var) = @_;
  my $prefix="";
# INTEGER variables
  if( $type eq "i") {
    if($usage eq "arg") {
      $prefix="K" unless($var=~/^K/i);
    }
    elsif($usage eq "local") {
      $prefix="I" unless($var=~/^[IJ]/i);
    }
    elsif($usage eq "module") {
      $prefix="N" unless($var=~/^[MN]/i);
    }
    elsif($usage eq "namvar") {
      $prefix="I" unless($var=~/^[MNIJ]/i);
    }
    else { 
      die "Unknown usage";
    }
  }
# REAL variables
  elsif( $type eq "r") {
    if($usage eq "arg") {
      $prefix="P" unless($var=~/^P/i);
    }
    elsif($usage eq "local") {
      $prefix="Z" unless($var=~/^Z|^PP/i);
    }
    elsif($usage eq "module") {
      $prefix="R" if ($var=~/^[ZPIJKLMNCY]/i);
    }
    elsif($usage eq "namvar") {
      $prefix="Z" if ($var=~/^[PIJKLMNCY]/i);
    }
    else { 
      die "Unknown usage";
    }
  }
#LOGICAL variables
  elsif( $type eq "l") {
    if($usage eq "arg") {
      $prefix="LD" unless($var=~/^LD/i);
    }
    elsif($usage eq "local") {
      $prefix="LL" unless($var=~/^LL/i);
    }
    elsif($usage eq "module") {
      $prefix="L" unless($var=~/^L[^LD]/i);
    }
    elsif($usage eq "namvar") {
      $prefix="LL" unless($var=~/^L/i);
    }
    else { 
      die "Unknown usage";
    }
  }
#CHARACTER variables
  elsif( $type eq "c") {
    if($usage eq "arg") {
      $prefix="CD" unless($var=~/^CD/i);
    }
    elsif($usage eq "local") {
      $prefix="CL" unless($var=~/^CL/i);
    }
    elsif($usage eq "module") {
      $prefix="C" unless($var=~/^C[^LD]/i);
    }
    elsif($usage eq "namvar") {
      $prefix="CL" unless($var=~/^C/i);
    }
    else { 
      die "Unknown usage";
    }
  }
# USER DEFINED TYPES
  elsif( $type eq 't') {
    if($usage eq "arg") {
      $prefix="YD" unless($var=~/^YD/i);
    }
    elsif($usage eq "local") {
      $prefix="YL" unless($var=~/^YL/i);
    }
    elsif($usage eq "module") {
      $prefix="Y" unless($var=~/^Y[^LD]/i);
    }
    elsif($usage eq "namvar") {
      $prefix="YL" unless($var=~/^Y/i);
    }
    else { 
      die "Unknown usage";
    }
  }
# FUNCTION/EXTERNAL declarations
  elsif( $type eq 'f' || $type eq 'e' || $type eq 's') {
# Everything is OK
  }
  else {
    die "Unknown type $type"
  }
  ($prefix);
}
#==========================================================================
     
sub parse_prog_unit {
# Find out program type,program name and arguments for program statement
  my($unit_name,$args)=@_;
  my($type)='';
  $$unit_name='';
  @$args=();
  our($name,$type_spec);
  if(/^ *MODULE +($name) *$/io) {
    $type='module';
    $$unit_name=$1;
  }
  elsif(/^ *PROGRAM +($name) *$/io) {
    $type='program';
    $$unit_name=$1;
  }
  elsif(/^ *(?:RECURSIVE)? *(SUBROUTINE) +($name)\b/io |
	/^ *(?:$type_spec)?\s*(FUNCTION) +($name)\b/io) {
    $type=lc($1);
    $$unit_name=$2;
#    print "FOUND PPU  $type $$unit_name \n ";
    if(/^[^\(]+\([^\)]+\)/) {
      my $tstatm=$_;
      $tstatm=~ s/\!.*\n/\n/g;
      $tstatm=~s/\s//g;
      $tstatm=~s/.+\((.+)\)/$1/;
      @$args=split(',',uc($tstatm));
    }
  }
  return $type;
}

#==========================================================================

sub setup_parse {
# Set up some "global" variables that helps with parsing statements
  our $nest_par = qr{\((?:(?>[^()]+)|(??{$nest_par}))*\)}; #Camel p214
  our $name='[a-zA-Z]\w*';
  our $digit_string='\d+';
  our $type_name=$name;
  our $specification_expr='(?:'.$name.'|'.$digit_string.')'; # Simplification
  our $type_param_value='(?:\*|'.$specification_expr.')';
  our $char_selector='LEN *= *'.$type_param_value; # Simplification
  our $kind_selector='\( *KIND *= *'.$name.' *\)';    # Simplification
  our $type_spec='INTEGER *(?:'.$kind_selector.')?|REAL *(?:'.$kind_selector.
    ')?|DOUBLE PRECISION|COMPLEX *(?:'.$kind_selector.')?|CHARACTER *'.
    $char_selector.'|LOGICAL *(?:'.$kind_selector.')?|TYPE\s*\(\s*'.$type_name.
    '\s*\)';
}

#==========================================================================

sub f90_indent {
# Indent free-format F90 program to our standards
  my($line_hash,$lines)=@_;
  my($delta)='  '; 
  my($cur_indent)='';
  @$lines=();
  foreach my $href (@$line_hash) {
    $_=$href->{line};
    if($href->{content} eq 'comment') {
      push(@$lines,$_);
      next;
    }
    s/^ *//; # Remove current indentation
    my($post_chg)=0;
    my($pre_chg)=0;
    my($cont_line)='';
    exit if (! exists $href->{content});
    if($href->{content} eq 'DO') {
      $post_chg=1 unless /^DO\s+\d/;
    }
    elsif($href->{content} eq 'ENDDO') {
      $pre_chg=1;
    }
    elsif($href->{content} eq 'IF_construct') {
      $post_chg=1;
    }
    elsif($href->{content} eq 'ELSEIF') {
      $post_chg=1;
      $pre_chg=1;
    }
    elsif($href->{content} eq 'ELSE') {
      $post_chg=1;
      $pre_chg=1;
    }
    elsif($href->{content} eq 'ENDIF') {
      $pre_chg=1;
    }
    elsif($href->{content} eq 'ENDIF') {
      $pre_chg=1;
    }
    elsif($href->{content} eq 'WHERE_construct') {
      $post_chg=1;
    }
    elsif($href->{content} eq 'ELSEWHERE') {
      $post_chg=1;
      $pre_chg=1;
    }
    elsif($href->{content} eq 'ENDWHERE') {
      $pre_chg=1;
    }
    elsif($href->{content} eq 'ENDIF') {
      $pre_chg=1;
    }
    elsif($href->{content} eq 'SELECT CASE') {
      $post_chg=1;
    }
    elsif($href->{content} eq 'CASE') {
      $post_chg=1;
      $pre_chg=1;
    }
    elsif($href->{content} eq 'END SELECT') {
      $pre_chg=1;
    }
    $cont_line=' ' if($href->{content} eq 'cont_line');
    if( $pre_chg ) {
      unless($cur_indent=~s/^$delta//o) {
	print $_;
	die  "f90_indent: something wrong,indent negative\n";;
      }
    }
#    print "$cur_indent$cont_line$_";
    
    $_=$cur_indent.$cont_line.$_;
    push(@$lines,$_);
    $cur_indent.=$delta if( $post_chg );
  }

  if(! ($cur_indent eq '')) {
    die "f90_indent: something wrong, indent=XX${cur_indent}XX\n";
  }
}

#==========================================================================

sub tidy {
# Straigthforward tidiyng of statements
  my($statements) = @_;
  my($href,$content);
  foreach $href (@$statements) {
    $_=$href->{statement};
    $content=$href->{content};
# Substitute tab with four blanks
    s/\t/    /g;
    if($content eq 'comment') {
# Substitute empty comment line with empty line
      s/^[!] *\n$/\n/;
      $href->{statement}=$_;
      next;
    }
    if($href->{exec}) {
      if($content eq 'ENDDO') {
	s/\bEND DO\b/ENDDO/i;
	$href->{statement}=$_;
	next;
      }
      if($content eq 'ENDIF') {
	s/\bEND IF\b/ENDIF/i;
	$href->{statement}=$_;
	next;
      }
      if($content eq 'ENDWHERE') {
	s/\bEND WHERE\b/ENDWHERE/i;
	$href->{statement}=$_;
	next;
      }

      s/\bELSE IF\b/ELSEIF/i  if($content eq 'ELSEIF');

      if(/\./) {
	s/ *\.EQ\. */ == /gi;
	s/ *\.NE\. */ \/= /gi;
	s/ *\.LT\. */ < /gi;
	s/ *\.LE\. */ <= /gi;
	s/ *\.GT\. */ > /gi;
	s/ *\.GE\. */ >= /gi;
      }

#
      s/\bA?MAX[01]\b/MAX/gi;
      s/\bA?MIN[01]\b/MIN/gi;
      s/\bAMOD\b/MOD/gi;
      s/\bALOG\b/LOG/gi;
      s/\bALOG10\b/LOG10/gi;
#      s/\bI(SIGN *\()/$1/gi; # Goes wrong in larcinad etc.
      s/\bFLOAT\b/REAL/g;
      s/\bfloat\b/real/g;
    }
    
    $href->{statement}=$_;
  }
}

#==========================================================================

sub process_include_files {
# Read include files and put reference to the anonomys array
# holding the array of "statement" hashes in $href->{inc_statm}
  my($statements,$prog_info,$inc_statements) = @_;
  my ($content,$fname,$href);
  return unless ($$prog_info{has_include});
  my @lines=();
  foreach $href (@$statements) {
    $content=$href->{content};
    if($content eq 'include'){
      $_=$href->{statement};
      /["](\S+)["]/;
      $fname=$1;
      &get_inc_lines($fname,\@lines);
# Macro-removal
      &remove_macro(\@lines);
# Expand lines into statements and put refernce to this
# array of hashes into $href->{inc_statm}
      my @inc_statms=();
      my $dum={};
      &expcont(\@lines,\@inc_statms);
      $href->{inc_statm}=[@inc_statms];
      my $incs=$href->{inc_statm};
# Study the read in file and add more attributes
      &study($incs);
#      print Dumper($incs,$dum);
      
    }
  }
}
#==========================================================================
sub get_inc_lines{
# Recurcivly get lines from include files, flatten into array of lines
  my ($fname,$lines) = @_;
  my ($VPATH,@vpath,@tmp_lines);

  $VPATH=$ENV{VPATH} or die "VPATH not defined ";
  @vpath=split(":",$VPATH);
# Look for include file in VPATH
  foreach my $path (@vpath) {
    my $ffname=$path.'/'.$fname;
    if( -f $ffname) {
# Read lines from include file
      @tmp_lines = &readfile($ffname);
#      print "$ffname \n";
      for (@tmp_lines) {
	if(/^\#include\b/){
	  /["](\S+)["]/;
	  my $fname2=$1;
	  &get_inc_lines($fname2,$lines);
	}
	else {
	  push(@$lines,$_);
	}
      }
      last;
    }
  }
  die "Include file $fname not found in VPATH=$VPATH " unless(@$lines);
}

#==========================================================================

sub create_interface_block {
# Create a "minimal" interface block for subroutines
  my($statements,$interface_block) = @_;
  my($href,$content,@pu_args,%pu_args,$func,%tokens);
  our($name,$nest_par);
  @$interface_block=();
  @pu_args=();
# Gather information needed to create interface block for routine
  foreach $href (@$statements) {
    last if($href->{exec});
    if($href->{content} eq 'SUBROUTINE') {   # Get arguments of subroutine
      $_=$href->{statement};
      my $dum=&parse_prog_unit(\$func,\@pu_args);
      for(@pu_args) {
	$_=uc($_);
	$pu_args{$_}++;
      }
      next;
    }
    if($href->{decl} == 2) {                 # Get all tokens from lines where argument present
      $_=uc($href->{statement});
      my @line_tokens=/\b$name\b/g;
      for (@line_tokens) {
	if($pu_args{$_}) {
	  for (@line_tokens) {
	    $tokens{$_}++;
	  }
	  last;
	}
      }
    }
  }

# Create interface block
  foreach $href (@$statements) {
    my %myhref=%$href;       # We have to make a copy rather that another reference
    my $myhref=\%myhref;     # since we have to modify statements for this purpose only
    $content=$myhref->{content};
    next if($content eq 'comment');
    next if($myhref->{exec});
    next if($myhref->{in_contain});
    delete $myhref->{pre_insert} if(exists $myhref->{pre_insert}); #Delete existing pre- and post -inserts
    delete $myhref->{post_insert} if(exists $myhref->{post_insert});
    if($myhref->{content} eq 'SUBROUTINE') { # Put subroutine statement into interface block
      $myhref->{pre_insert} = "INTERFACE\n";
      push(@$interface_block,$myhref);
    }
    if($myhref->{decl} == 4) { # Include USE statement in interface block if needed
      $_=$myhref->{statement};
      tr/ \n//d;
      $_=uc($_);
      if(/^USE$name,ONLY:(.+)$/) {
	$_=$1;
	my @line_tokens=/\b$name\b/g;
	for (@line_tokens) {
	  if($tokens{$_}) {
	    push(@$interface_block,$myhref);
	    last;
	  }
	}
      }
      else {
	push(@$interface_block,$myhref);  # Always include USE without ONLY for safety
      }  
    }
    if($myhref->{decl} == 1 or $myhref->{decl} == 2) {
      $_=uc($myhref->{statement});
      if($myhref->{content} eq 'INTEGER' and /\bPARAMETER\b/) { # Integer parameters may be used for dimensioning
	my @line_tokens=/\b$name\b/g;
	for (@line_tokens) {
	  if($tokens{$_}) {
	    push(@$interface_block,$myhref);
	    last;
	  }
	}
      }
      else{    #Include only lines where an argument is present
	s/$nest_par//g;
	my @line_tokens=/\b$name\b/g;
	for (@line_tokens) {
	  if($pu_args{$_}) {
	    push(@$interface_block,$myhref);
	    last;
	  }
	}
      }
    }
    if($content eq 'END SUBROUTINE') { # Add END statement to interface block
      $myhref->{post_insert} = "END INTERFACE\n";
      push(@$interface_block,$myhref);
      last;
    }
  }
  foreach $href (@$interface_block) {
    $_=$href->{statement};
    s/\!.*\n/\n/g;      # Remove trailing comments
    s/ +/ /g;           # Only one space
    s/\n *\n/\n/g;      # Remove empty lines
    s/\n *\n/\n/g;      # Remove empty lines again
    s/ +\n/\n/g;        # No trailing spaces
    $href->{statement}=$_;
  }
}

#==========================================================================
sub change_var_names{
  my($statements) = @_;
  foreach my $href (@$statements) {
    $_=$href->{statement};
    s/\bVAZX\b/YVAZX/ig;
    s/\bPVAZX\b/YDVAZX/ig;
    s/\bVAZG\b/YVAZG/ig;
    s/\bPVAZG\b/YDVAZG/ig;
    s/\bSCALP_DV\b/YSCALP/ig;
    s/\bRSCALP_DV\b/YRSCALP/ig;
    s/\bSCALPSQRT_DV\b/YSCALPSQRT/ig;
    s/\bRSCALPSQRT_DV\b/YRSCALPSQRT/ig;
    s/\bPYBAR\b/YDYBAR/ig;
    s/\bPSBAR\b/YDSBAR/ig;
    s/\bVCGLPC\b/YVCGLPC/ig;
    s/\bVCGLEV\b/YVCGLEV/ig;
    s/\bSKFROT\b/YSKFROT/ig;
    s/\bSKFMAT\b/YSKFMAT/ig;
    s/\bSTATE_VECTOR_4D\b/YSTATE_VECTOR_4D/ig;
    s/\bVAZX0\b/YVAZX0/ig;
    s/\bVAZG0\b/YVAZG0/ig;
    s/\bRSPFORCE\b/YSPFORCE/ig;
    $href->{statement}=$_;
  }
}
# =========================================================================
sub remake_arg_decl{
  my($statements,$prog_info) = @_;
  my($href,$content,@pu_args,$func,%tokens);
  my($left,$right,%arghash,$dim);
  our($nest_par,$name);

  my $dims='';
# Crack existing dummy declarations, build hash arghash
  foreach $href (@$statements) {
    last if($href->{prog_unit} >0);
    if($href->{content} eq 'SUBROUTINE') {   # Get arguments of subroutine
      $_=$href->{statement};
      my $dum=&parse_prog_unit(\$func,\@pu_args);
#      print Dumper(\@pu_args);
      for(@pu_args) {
	$_=uc($_);
	$arghash{$_}{other}='';
	$arghash{$_}{dimuse}=0;
	$arghash{$_}{intent}='';
	$arghash{$_}{used}=0;
	$arghash{$_}{set}=0;
	$arghash{$_}{reallyset}=0;
	$arghash{$_}{type}='';
	$arghash{$_}{comment}='';
	$arghash{$_}{inif}=0;
      }
      next;
    }
    if($href->{decl} == 2) {
      $_=$href->{statement};
      my $comment='';
      $comment=$1 if(/.*(\!.*)$/);
      s/\!.*\n/\n/g;                        # Remove trailing comments in all lines
      $_=uc($_);
      s/\s//g;
      if(/^(.+)::(.+)$/){
	$left=$1;
	$right=$2;
	$_=$right;
	s/$nest_par//g;
	s/($name)\*\w+/$1/g;
#	print "XX  $_ \n";
	foreach my $arg (@pu_args) {
	  if(/\b$arg\b/) {
#	    print "ARG $arg $left $_ \n";
	    $arghash{$arg}{linedec}=$href->{number};
	    $arghash{$arg}{comment}=$comment;
	    my @locdec =split ',',$left;
	    my $i=0;
	    foreach my $locdec (@locdec) {
	      if($i == 0) {
		$arghash{$arg}{type}=$locdec;
	      }
	      elsif($locdec=~/\bINTENT/) {
		$arghash{$arg}{intent}=','.$locdec;
	      }
	      else {
		$arghash{$arg}{other}=$arghash{$arg}{other}.','.$locdec;
	      }
	      $i++;
	    }
	    if($right=~/\b$arg\b(\*\w+)/) {
	      $dim=$1;
	    }
	    elsif($right=~/\b$arg\b($nest_par\*$nest_par)/) {
	      $dim=$1;
	    }
	    elsif($right=~/\b$arg\b($nest_par\*\w+)/) {
	      $dim=$1;
	    }
	    elsif($right=~/\b$arg\b(\*$nest_par)/) {
	      $dim=$1;
	    }
	    elsif($right=~/\b$arg\b($nest_par)/) {
	      $dim=$1;
	    }
	    else {
	      $dim='';
	    }
	    $arghash{$arg}{dim}=$dim;
	    $dims=$dims.$dim
	      }
	}
	foreach my $arg (@pu_args) {  # Is arg. used for dimensioning other args?
	  if($dims=~/\b$arg\b/i) {
	    $arghash{$arg}{dimuse}=1;
	  }
	}  
      }
    }
  }
  my $insert_line=0;
  foreach $href (@$statements) {
    last if($href->{prog_unit} >0);
    if($href->{decl} == 2 or $href->{content} eq 'PARAMETER') {                 
      $_=uc($href->{statement});
      next unless /\bPARAMETER\b/;
      my @tmpvar=/\b$name\b/g;
      foreach my $token (@tmpvar) {
	if($dims=~/\b$token\b/) {
	  $insert_line=$href->{number};
	}
      }
    }
  }
      
# Gather info to decide INTENT status
  my $inif=0;
  my @inif_stack=();
  my $cur_inif=0;
  foreach $href (@$statements) {
    last if($href->{prog_unit} >0);
    if($href->{exec}) {
      if($href->{content} eq 'ENDIF') {
	$inif--;
	$cur_inif=pop @inif_stack;
	next;
      }
      elsif($href->{content} eq 'ELSEIF' or $href->{content} eq 'ELSE') {
	$cur_inif=pop @inif_stack;
	$cur_inif=$href->{number};
	push @inif_stack,$cur_inif;
      }
      my ($left,$right);
      $_=$href->{statement};
      s/\!.*\n/\n/g;                        # Remove trailing comments in all lines
      my %setnow=();
      foreach my $arg (@pu_args) {
	$setnow{$arg}=0;
	$setnow{$arg}=1 if($arghash{$arg}{reallyset});
	unless ($setnow{$arg}) {
	  foreach my $xx (@inif_stack) {
	    $setnow{$arg}=1 if($xx == $arghash{$arg}{inif});
	  }
	}
      }
      
      if($href->{content} eq 'scal_assign' or $href->{content} eq 'array_assign') {
	s/\s//g;
	($left,$right)=/^(.+)=(.+)$/;
	$_=$right;
	foreach my $arg (@pu_args) {
	  if(/\b$arg\b/i) {
	    $arghash{$arg}{used}=1 unless $setnow{$arg};
	  }
	}
	$_=$left;
	if(/($nest_par)/) {
	  $_=$1;
	  foreach my $arg (@pu_args) {
	    if(/\b$arg\b/i) {
	      $arghash{$arg}{used}=1 unless $setnow{$arg};
	    }
	  }
	}
	$_=$left;
	foreach my $arg (@pu_args) {
	  if(/^$arg\b/i) {
	    $arghash{$arg}{set}=1;
	    $arghash{$arg}{inif}=$cur_inif;
	    $arghash{$arg}{reallyset}=1 unless($inif);
	  }
	}
      }
      elsif($href->{content} eq 'IF' ) {
	if($href->{content2} eq 'scal_assign' or $href->{content2} eq 'array_assign' or 
	   $href->{content2} eq 'CALL') {
	  s/\n//g;
	  ($left,$right)=/^\s*(IF\b\s*$nest_par)(.+)/i;
	  $_=$left;
	  foreach my $arg (@pu_args) {
	    if(/\b$arg\b/i) {
	      $arghash{$arg}{used}=1 unless $setnow{$arg};
	    }
	  }
	  $_=$right;
	  if($href->{content2} eq 'CALL') {
	    my $statement=$right;
	    my $inifx=1;
	    &propag_arg(\$statement,\%arghash,\$inifx,\%setnow);
	  }
	  else {
	    s/\s//g;
	    ($left,$right)=/^(.+)=(.+)$/;
	    $_=$right;
	    foreach my $arg (@pu_args) {
	      if(/\b$arg\b/i) {
		$arghash{$arg}{used}=1 unless $setnow{$arg};
	      }
	    }
	    $_=$left;
	    if(/($nest_par)/) {
	      $_=$1;
	      foreach my $arg (@pu_args) {
		if(/\b$arg\b/i) {
		  $arghash{$arg}{used}=1 unless $setnow{$arg};
		}
	      }
	    }
	    $_=$left;
	    foreach my $arg (@pu_args) {
	      if(/^$arg\b/i) {
		$arghash{$arg}{inif}=$cur_inif;
		$arghash{$arg}{set}=1;
	      }
	    }
	  }
	}
	else {
	  foreach my $arg (@pu_args) {
	    if(/\b$arg\b/i) {
	      $arghash{$arg}{used}=1 unless $setnow{$arg};
	    }
	  }
	}
      }
      elsif($href->{content} eq 'WHERE' ) {
	s/\s//g;
	($left,$right)=/^(WHERE$nest_par)(.+)/i;
	$_=$left;
	foreach my $arg (@pu_args) {
	  if(/\b$arg\b/i) {
	    $arghash{$arg}{used}=1 unless $setnow{$arg};
	  }
	}
	$_=$right;
	($left,$right)=/^(.+)=(.+)$/;
	$_=$right;
	foreach my $arg (@pu_args) {
	  if(/\b$arg\b/i) {
	    $arghash{$arg}{used}=1 unless $setnow{$arg};
	  }
	}
	$_=$left;
	foreach my $arg (@pu_args) {
	  if(/^$arg\b/i) {
	    $arghash{$arg}{inif}=$cur_inif;
	    $arghash{$arg}{set}=1;
	  }
	}
      }
      elsif($href->{content} eq 'CALL') {
	my $statement=$_;
	&propag_arg(\$statement,\%arghash,\$inif);
      }
      else{
	foreach my $arg (@pu_args) {
	  if(/\b$arg\b/i) {
	    $arghash{$arg}{used}=1 unless $setnow{$arg};
	  }
	}
      }
      if($href->{content} eq 'IF_construct') {
	$inif++;
	$cur_inif=$href->{number};
	push @inif_stack,$cur_inif;
      }
    }	  
  }

# Create INTENT statemant based on gathered info
  foreach my $arg (@pu_args) {
    if($arghash{$arg}{linedec}) {
      if($arghash{$arg}{nointent}) {
	unless($arghash{$arg}{intent}) {
	  $arghash{$arg}{intent}=' ';
	  $arghash{$arg}{comment}='! UNDETERMINED INTENT';
	}
      }
      else{
	my $intent='';
	$intent='IN' if($arghash{$arg}{used} or $arghash{$arg}{dimuse});
	$intent=$intent.'OUT' if($arghash{$arg}{set});
	if($intent) {
	  if($arghash{$arg}{intent} and $intent eq 'OUT') {
	    $intent='INOUT' if $arghash{$arg}{intent}=~/INOUT/i;
	  }
	  $arghash{$arg}{intent}=',INTENT('.$intent.')';
	}
	else {
	  $arghash{$arg}{intent}=' ';
	  $arghash{$arg}{comment}='! Argument NOT used';
	}
      }
    }
  }

# Remove existing argument declarations
  foreach my $arg (@pu_args) {
    if($arghash{$arg}{linedec}) {
      $_=$$statements[$arghash{$arg}{linedec}]->{statement};
      #   print "BEFORE $arg $_";
      if(/.*::\s*\b$arg\b\s*(\!.*\n)*$/i) {
	$_='';
      }
      elsif(/.*::\s*\b$arg\b\s*$nest_par\s*(\!.*\n)*$/i) {
	$_='';
      }
      elsif(/.*::\s*\b$arg\b\s*\*\s*\w+\s*(\!.*\n)*$/i) {
	$_='';
      }
      elsif(/.*::\s*\b$arg\b\s*\*\s*$nest_par\s*(\!.*\n)*$/i) {
	$_='';
      }
      elsif(/.*::\s*\b$arg\b\s*$nest_par\s*\*\s*\w+\s*(\!.*\n)*$/i) {
	$_='';
      }
      elsif(/.*::\s*\b$arg\b\s*$nest_par\s*\*\s*$nest_par\s*(\!.*\n)*$/i) {
	$_='';
      }
      else{
	/^(.*::)(.*)$/s;
	my $left=$1;
	$_=$2;
	s/\b$arg\b\s*$nest_par//i;
	s/\b$arg\b\s*\*\s*\w+//i;
	s/\b$arg\b\s*\*\s*$nest_par//i;
	s/\b$arg\b//i;
	s/,\s*,/,/;
	s/,(\s*)$/$1/;
	s/\n\s*\n/\n/g;
	$_=$left.$_;
	s/::\s*,/::/;
      }
 #   print "AFTER $arg $_\n";
      $$statements[$arghash{$arg}{linedec}]->{statement}=$_; 
    }
  }

 # Write out

  my $newdecl='';
  my $linedec;
  foreach my $arg (@pu_args) {
    if($arghash{$arg}{linedec}) {
      if($arghash{$arg}{other} and ! $arghash{$arg}{dim}) {
	$arghash{$arg}{other}=~s/\s//g;
	if($arghash{$arg}{other}=~/^,DIMENSION($nest_par)$/i) {
	  $arghash{$arg}{other}='';
	  $arghash{$arg}{dim}=$1;
	}
      }
      if($arghash{$arg}{dimuse}) { # Put declerations of args first
	$linedec=sprintf "%-18s%s%-14s%s%s%s%s %s",
	$arghash{$arg}{type},$arghash{$arg}{other},$arghash{$arg}{intent},
	    ' :: ',$arg,$arghash{$arg}{dim},$arghash{$arg}{comment},"\n";
	$newdecl=$newdecl.$linedec;
      }
    }
  }
  foreach my $arg (@pu_args) {
    if($arghash{$arg}{linedec}) {
      unless($arghash{$arg}{dimuse}) {
	$linedec=sprintf "%-18s%s%-14s%s%s%s %s%s",
	$arghash{$arg}{type},$arghash{$arg}{other},$arghash{$arg}{intent},
	    ' :: ',$arg,$arghash{$arg}{dim},$arghash{$arg}{comment},"\n";
	$newdecl=$newdecl.$linedec;
      }
    }
  }
#  print "INSERT_LINE $insert_line \n";
  if($insert_line) {
    $$statements[$insert_line]->{post_insert}=$newdecl;
  }
  else{
    foreach $href (@$statements) {
      if($href->{decl} == 2) {                 
	$href->{pre_insert}=$newdecl;
	last;
      }
    }
  }

#  print $newdecl;
#  print Dumper(\%arghash);
}

sub propag_arg{
  my ($statement,$arghash,$inif,$setnow) = @_;
  our ($name,$nest_par);
  my (%argpos);
  $_=$$statement;
  s/^\s*CALL\s+($name)//i;
  my $called=lc($1);
  s/\s//g;
  s/^\((.*)\)$/$1/s;
  my @inpars=/$nest_par/g;
  s/$nest_par//g;
  s/($name)%$name/$1/g;
  $_=uc($_);
#  print "PROPAG $called $_ ££ @inpars \n";
  my @call_args=split ',' , $_;
  my $i=0;
  my $interesting=0;
  %argpos=();
  foreach my $call (@call_args) {
    
#    print "CALL $called $call \n" ;
    if($call=~/(.+)=(.+)/) {
      $call=$2; #This just by-passes the problem
    }
    if(exists $$arghash{$call}) {
      if(exists $argpos{$call}) {
	push @{$argpos{$call}},$i;
      }
      else {
	my @i=($i);
	$argpos{$call}=[@i];
      }
      $interesting=1;
    }
    $i++;
  }
  if($interesting) {
    my $fname='/tmp/intblocks/'.$called.'.intfb.h';
    if( -f $fname ) {
      my @dumargs=();
      my $unit_name;
      print "FILE $fname FOUND \n";
      my @lines = &readfile($fname);
      my @loc_statements=(); 
      &expcont(\@lines,\@loc_statements);
      foreach my $href (@loc_statements) {
	$_=$href->{statement};
	if(/^\s*SUBROUTINE/i) {
	  my $dum=&parse_prog_unit(\$unit_name,\@dumargs);
	  next;
	}
	if(/::/) {
	  s/\s//g;
	  foreach my $arg (keys (%argpos)) {
	    my $set_before=$$setnow{$arg};
	    foreach my $i (@{$argpos{$arg}}){
	      if(/::$dumargs[$i]/) {
		if(/INTENT\(IN\)/i) {
		  $$arghash{$arg}{used}=1 unless $set_before;
		}
		elsif(/INTENT\(OUT\)/i) {
		  $$arghash{$arg}{set}=1;
		  $$setnow{$arg}=1 unless($$inif);
		}
		elsif(/INTENT\(INOUT\)/i) {
		  $$arghash{$arg}{set}=1;
		  $$arghash{$arg}{used}=1 unless $set_before;;
		  $$arghash{$arg}{reallyset}=1 unless($$inif);
		}
		elsif(/\! UNDETERMINED INTENT/) {
		  $$arghash{$arg}{nointent}=1;
		}
	      }
	    }
	  }
	}
      }
    }
    else {
      foreach my $arg (keys (%argpos)) {
	$$arghash{$arg}{nointent}=1;
      }
    }
  }
  for (@inpars) {
    foreach my $arg (keys (%$arghash)) {
      if(exists $$arghash{$arg}) {
	if(/\b$arg\b/i) {
	  $$arghash{$arg}{used}=1 unless $$setnow{$arg};
	}
      }
    }
  }
}
  
sub add_interface_blocks {
# Add interface block for called routines
  use File::Find;
  my($statements,$prog_info) = @_;
  my($href,$call);
  our($name,$nest_par);
  our(@call_names,@call_names_found,%call_names);

  return unless ($$prog_info{no_calls}); # Skip if there are no calls
  @call_names=();
  %call_names=();

  my $last_decl=0;
  my $in_intfblk=0;
  my %already_in=();
  ST:foreach $href (@$statements) {
    last if($href->{prog_unit} > 0);  # Only consider first program unit (no contains)
    if($href->{content} eq 'INTERFACE') {
      $in_intfblk=1;
      next;
    }
    if($href->{content} eq 'END INTERFACE') {
      $in_intfblk=0;
      next;
    }
    if($in_intfblk) {
      $_=$href->{statement};
      s/\#include\s*\"(\w+)\.h\"\s*$/$1/;
      $_=lc($_);
      $already_in{$_}++;
      next;
    }
    
# Find last declaration
    if($href->{decl}) {
      next if($href->{content} eq 'FORMAT');
      next if($href->{content} eq 'DATA');
      $last_decl = $href->{number} ;
    }
# Find calls
    next unless($href->{exec});
    if($href->{content} eq 'CALL' or 
       (exists  $href->{content2} and$ href->{content2} eq 'CALL') ) {
      $_=$href->{statement};
      /\s*\bCALL\b\s*($name)/i;
      my $call=lc($1);
      next if($already_in{$call}); # Exclude already existing interface block
      next if($call eq 'packmsg'); # A couple of special exceptions
      next if($call eq 'unpkmsg');
      $call_names{$call}++;
    }
  }
  

# Check that routine exists in IFS
  @call_names_found=();
  find(\&calls_wanted,'/tmp/27/ifs/');
#  find(\&calls_wanted,'/home/mats/work/cy28/ifs/');
#  find(\&calls_wanted,'/tmp/27/trans/');
  @call_names_found=sort(@call_names_found);
#  print "P2 @call_names_found \n";
  @call_names=@call_names_found;

# Contruct include block
  my $block='';
  for (@call_names) {
    $block=$block.'#include "'.$_.'.intfb.h"'."\n";
  }
#  print $block;

  my $clean=0;
  if(@call_names) {
    if($$prog_info{has_interface_block}) {
      foreach $href (@$statements) {
# Add interface block to routine that already has INTERFACE statement
	if($href->{content} eq 'END INTERFACE'){
	  if($href->{post_insert}) {
	    $href->{post_insert}=$href->{post_insert}."\n".$block;
	  }
	  else {
	    $href->{post_insert}="\n".$block;
	  }	    
	  last;
	}
      }
    }
# Add interface block to routine that does not have previous INTERFACE statement
    else {
      $href=@$statements[$last_decl];
      if($href->{post_insert}) {
	$href->{post_insert}=$href->{post_insert}."\n".$block;
      }
      else {
	$href->{post_insert}="\n".$block;
      }	    
    }
# Remove from EXTERNAL statement where interface block has been added
    foreach $href (@$statements) {
      if($href->{content} eq 'EXTERNAL') {
	$_=$href->{statement};
	foreach my $ext (@call_names) {
	  s/\b$ext\b//i;
	}
	s/,\s*,/,/g;
	s/^(\s*EXTERNAL\s*),/$1/i;
	s/^(\s*EXTERNAL.*),\s*$/$1/i;
	s/^\s*EXTERNAL\s*,*\s*$//i;
	$href->{statement}=$_;	
      }
    }
  }
}
#======================================================================================
sub calls_wanted {
  # Used by Find as called from add_interface_blocks
  our(%call_names,@call_names_found);
  return unless (/^(\w+)\.F90$/);
  my $call=$1;
  if($call_names{$call}) {
    push(@call_names_found,$call);
  }    
}
sub remove_some_comments{
  my($statements) = @_;
  my $prev_empty=0;
  foreach my $href (@$statements) {
    if($href->{content} eq 'comment'){
      $_=$href->{statement};
      if(/^\s*$/) {
	if($prev_empty) {
	  s/\s*//;
	  $href->{statement}=$_;
	}
	else {
	  $prev_empty=1;
	} 
	next;
      }
      $prev_empty=0;
      s/^\s*![\s\*]*\bLOCAL\s+(INTEGER|REAL|LOGICAL|CHARACTER)\s+(SCALARS|ARRAYS).*\n$//i;
      s/^\s*![\s\*]*\bDUMMY\s+(INTEGER|REAL|LOGICAL|CHARACTER)\s+(SCALARS|ARRAYS).*\n$//i;
      s/^\s*![\s\*]*\bLOCAL\s+(INTEGER|REAL|LOGICAL|CHARACTER).*\n$//i;
      s/^\s*![\s\*]*\bDUMMY\b\s*$//i;
      s/^\s*![\s\*]*\bLOCAL\b\s*$//i;
      s/^\s*![\s\*]*\bLOCAL\b:\s*$//i;
      s/^\s*![\s\*]*\bLOCAL ARRAYS\b[\s\*]*$//i;
      s/^\s*![\s\*]*\bLOCAL SCALARS\b\s*$//i;
      s/^\s*![\s\*]*\s*\d\.\d+\s*\bLOCAL ARRAYS\b\s*$//i;
      s/^\s*![\s\*]*\s*=== LOCAL ARRAYS ===\s*$//i;
      $href->{statement}=$_;
    }
    else {
      $prev_empty=0;
    }
  }
}      
sub get_calls_inc {
  my($statements,$calls,$intfb) = @_;
  foreach my $href (@$statements) {
    if($href->{content} eq 'CALL') {
      $_=$href->{statement};
      /^\s*CALL\s+([A-Z]\w*)/i;
      $$calls{lc($1)}++;
    }
    elsif($href->{content} eq 'IF') {
      if($href->{content2} eq 'CALL') {
	$_=$href->{statement};
	/\bCALL\s+([A-Z]\w*)/i;
	$$calls{lc($1)}++;
      }
    }
    elsif($href->{content} eq 'include') {
      $_=$href->{statement};
      $$intfb{$1}=1 if(/["](\S+)\.intfb\.h["]/);
      $$intfb{$1}=2 if(/["](\S+)\.h["]/); # For old-style interface blocks
    }
  }
}
      
