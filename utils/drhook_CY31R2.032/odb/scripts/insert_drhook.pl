#---/usr/local/apps/perl/bin/perl

use strict;
use warnings;
#use lib "/ccvobs/satrad/rttov";
use Fortran90_stuff;
use Data::Dumper;
use File::Basename;

$Data::Dumper::Indent = 1;

#use Devel::DProf; #dprofpp
#use Devel::SmallProf; #dprofpp

#my $PROJECT=$ENV{PROJECT} or die "PROJECT not defined ";

{
  our $study_called=0;
  my (@files);
  (@files) = @ARGV;
  &setup_parse();
  for (@files) {
    my (%vars,@unused_vars,%args,$var,%doctor_viols,$action);
    my (@inc_statements,@interface_block);
    my (%use_vars,@unused_use_vars,%prog_info,@line_hash);
    
# Read in lines from file
    my $fname = $_;
    my $dirname=&dirname($fname);
    $dirname=~s!.*/(.+)!$1!;
    next unless ($fname=~/\.F90$/ or $fname=~/\.h$/);

    $action='hook';
    # print "ACTION $action \n";
    my $modfile= "_$fname";
    print "insert_hook: $fname => $modfile\n";
      
    my @lines = &readfile($fname);

#   if($action eq 'all' or $action eq 'hook' ) {
# Some initial tidiyng to make the rest work smoothly
#     &pre_tidy(\@lines);
#   }
# Expand the macros INTEGER_M etc into Fortran
    &remove_macro(\@lines);

# Expand lines into statements
#   "@statements" is an array of hashes ultimately containing the statements themself and
#   different attributes of the statement,  expcont puts in the statements
    if($action eq 'all' or $action eq 'hook' ) {
      my @statements=(); 
      &expcont(\@lines,\@statements);

      if($fname=~/\.F90$/) {
# "Study" the statements and set attributes in @statements
	$study_called=0;
	&study(\@statements,\%prog_info);
#      foreach my $key (keys (%prog_info)) {
#	print "$key = $prog_info{$key} \n";
#      }
#      my @keys = keys %{$prog_info{token_hash}[0]} ;
#      print "@keys\n ";


#       print Dumper(\%prog_info);
#       print Dumper(\@statements);
#      foreach my $href (@statements){
#	print "$href->{decl} $href->{exec} $href->{in_contain} $href->{prog_unit} ";
#	print "$href->{cont} $href->{statement}";
#      }
	&process_include_files(\@statements,\%prog_info,\@inc_statements);
##    foreach my $href (@statements){
##      if(exists $href->{inc_statm}) {
##	print "$href->{statement}";
##	my $xx = ref($href->{inc_statm});
##       print "$href->{inc_statm} XX=$xx  ZZ\n";
##	my $incs=$href->{inc_statm};
##	foreach my $hrefi (@$incs) {
##	  print "$hrefi->{statement}";
##	}
##      }
##     }
 
# Simple tidying (simple replacemnts .EQ. -> ==  etc.)
#   &tidy(\@statements);

# Tidy up declerations
#	&tidy_decl(\@statements);

# Rename some variables
      }
      if($action eq 'all' and $fname=~/\.F90$/) {
      
#	&change_var_names(\@statements);

# Get all locally declared variables and all USE, ONLY variables
	%vars=();
	%use_vars=();
	&getvars(\@statements,\%prog_info,\%vars,\%use_vars);
##      foreach $var (keys (%vars)) {
##	my $attr;
##	print "VAR $var ";
##	foreach $attr (keys %{$vars{$var}}) {
##	  print " $attr=$vars{$var}{$attr}";
##	}
##	print "\n ";
##      }
##    foreach $var (keys (%use_vars)) {
##      my $attr;
##      print "USE_VAR $var ";
##      foreach $attr (keys %{$use_vars{$var}}) {
##	print " $attr=$use_vars{$var}{$attr}";
##      }
##      print "\n ";
##    }

# Find variables declared but not used
	&find_unused_vars(\@statements,\%vars,\@unused_vars,
			  \%use_vars,\@unused_use_vars);
#	print "unused_vars @unused_vars \n";
#	print "unused_use_vars @unused_use_vars \n";
# Remove the unused variables found
	&remove_unused_vars(\@statements,\@unused_vars,\@unused_use_vars);

# Find (local) Doctor violations
#	&doctor_viol(\%vars,\%doctor_viols);
#	foreach $var (keys (%doctor_viols)) {
#	  print "doctor_viol ",$var," $doctor_viols{$var} \n";
#      }
# Fix Doctor violations found
#	&fix_doctor_viol(\@statements,\%doctor_viols);
    
# Final set of more complex modifications to the code
#	unless($prog_info{is_module} and ! $prog_info{has_contain}){
#	  &various(\@statements,\%prog_info,\%vars); 
#	}
      }
      if($fname=~/\.F90$/ and ($action eq 'all' or $action eq 'hook' and  $fname!~/codb_distribute\.F90/)){

 	unless($prog_info{is_module} and ! $prog_info{has_contain}){
 	  &various(\@statements,\%prog_info,\%vars); 
 	  &insert_hook(\@statements,\%prog_info,\%vars); 
 	}
      }
      if($action eq 'all' and $fname=~/\.F90$/) {
	&remake_arg_decl(\@statements,\%prog_info); 
	&cont_lines(\@statements,\@lines,\@line_hash);
#	&f90_indent(\@line_hash,\@lines);
	@statements=(); 
	&expcont(\@lines,\@statements);
	$study_called=0;
	&study(\@statements,\%prog_info);
	

	unless($prog_info{is_module}) {
	  &create_interface_block(\@statements,\@interface_block);
	  &cont_lines(\@interface_block,\@lines,\@line_hash);
	   my $int_block_fname=$fname;
	  $int_block_fname=~s/\.F90/.intfb.h/;
	  $int_block_fname=~s#.*/(.+)$#$1#;
	  $int_block_fname='/tmp/intblocks/'.$int_block_fname;
	  print "$int_block_fname \n";
	  &writefile($int_block_fname,\@lines);
#	for (@lines) {
#	  print "IB $_";
#	}
#    &add_interface_blocks(\@statements,\%prog_info);
	}
      }
#     if($action eq 'all' and $fname=~/\.F90$/) {
#       &remove_some_comments(\@statements);
#     }
      if($fname=~/\.F90$/ and ($action eq 'all' or $action eq 'hook' )) {
    
# Get lines back from the statements
  	&cont_lines(\@statements,\@lines,\@line_hash);
#    foreach my $href (@line_hash){
#      print "$href->{cont} $href->{line}";
#    }
# Properly indent lines
#      &f90_indent(\@line_hash,\@lines);

      }
    }

# Restore protected !
    for (@lines) {
      s/£/!/g;
    }
# And write them out
    &writefile($modfile,\@lines);

  } # This file done
}


