#! /usr/bin/perl
use strict;
use warnings;
use FindBin qw($Bin);

use Getopt::Long qw(GetOptions);
use Pod::Usage qw(pod2usage);
use XML::LibXML;

our $EtcDir = "$Bin/../etc/";
use vars qw(%Args);
$Args{levelnames} = "snap.felt_level_names";
$Args{felt_input} = "snap.felt";
$Args{xml_template} = "$EtcDir/felt2nc_snap_dummy_levels.xml";
GetOptions(\%Args,
       'debug',
       'help',
       'felt_input=s',
       'levelnames=s',
       'xml_template=s',
       'tag=s',
       'omitDiana')
    or pod2usage(2);

pod2usage(-exitval => 0,
          -verbose => 2) if $Args{help};

pod2usage(-exitval => 2,
          -msg => "missing levelnames-file") unless -f $Args{levelnames};
pod2usage(-exitval => 2,
          -msg => "missing xmlTemplate-file") unless -f $Args{xml_template};
pod2usage(-exitval => 2,
          -msg => "missing tag") unless $Args{tag};


my $command =  "perl $Bin/felt2ncDummyLevels2Isotopes.pl  --levelnames=$Args{levelnames} --xmlTemplate=$Args{xml_template} --output=felt2nc_$Args{tag}.xml";
print $command, "\n";
unless (-f "felt_axes.xml") {
    symlink("$EtcDir/felt_axes.xml", "felt_axes.xml")
        or die "Cannot link $EtcDir/felt_axes.xml to $PWD\n";
}
system($command) == 0 or die "system $command failed: $?";
$command = "fimex --input.file=$Args{felt_input} --input.config=felt2nc_$Args{tag}.xml --output.file=$Args{tag}.nc --output.type=nc4 --output.config=$EtcDir/cdmWriterConfig.xml";
print $command, "\n";
system($command) == 0 or die "system $command failed: $?";

my %isotopes = readIsotopes($Args{levelnames});

createDianaSetup($Args{tag}, \%isotopes);
if (!$Args{omitDiana}) {
    $command = "diana.bin -s diana.setup_$Args{tag}";
    system($command) == 0 or die "system $command failed: $?";
}

sub readIsotopes {
	my ($file) = @_;
	open (my $f, $file) or die "Cannot read levelnames file $file: $!";
	my %isotopes;
	while (defined (my $line = <$f>)) {
		if ($line =~ /^\s+(\d+)\s+"([^"]+)"/) {
			$isotopes{$1} = $2 unless $2 eq 'Total';
		} else {
			warn "Cannot parse line in $file: $line";
		}
	}
	return %isotopes;
}

sub createDianaSetup {
    my ($tag, $isotopes) = @_;
    open my $f, ">diana.setup_${tag}" or die "Cannot write diana.setup_${tag}: $!\n";
    print $f <<'EOT';
#----------------------------------------------------------------------------------
# Setupfile for Diana / snap
#----------------------------------------------------------------------------------

# =============================================

%include /etc/diana/${PVERSION}/diana.setup-COMMON

# =============================================

# CHANGES

<BASIC>
language=en #change language
</BASIC>



<COLOURS>

#redefine
#darkRed=      100,0,0 #even darker

#more colours
green5=     153,204,51
green6=     171,191,8

</COLOURS>


<PALETTES>
# Add palettes
no_green_ncl = 0:128:161,0:161:191,0:191:224,0:224:255,0:255:255,51:255:255,102:255:255,153:255:255,204:255:255,255:255:255,252:252:0,252:224:0,252:191:0,252:161:0,252:128:0,252:97:0,252:64:0,252:33:0,191:0:0
no_green_nclp = 0:255:255,51:255:255,102:255:255,153:255:255,204:255:255,255:255:255,252:252:0,252:224:0,252:191:0,252:161:0,252:128:0,252:97:0,252:64:0,252:33:0,191:0:0
no_green_nclm = 0:224:255,0:191:224,0:161:191,0:128:161
wave_pal=42:255:255,85:255:255,127:255:255,170:255:255,255:255:150,255:255:84,255:240:0,255:191:0,255:168:0,255:138:0,255:112:0,255:77:0,255:0:0,255:0:100,255:0:150,255:0:200,255:0:250
grey_pal=245:245:245,240:240:240,235:235:235,230:230:230,225:225:225,220:220:220,215:215:215,210:210:210,205:205:205,200:200:200,195:195:195,190:190:190,185:185:185,180:180:180,175:175:175,170:170:170,165:165:165,160:160:160,155:155:155,150:150:150
grey_pal_inv=150:150:150,155:155:155,160:160:160,165:165:165,170:170:170,175:175:175,180:180:180,185:185:185,190:190:190,195:195:195,200:200:200,205:205:205,210:210:210,215:215:215,220:220:220,225:225:225,230:230:230,235:235:235,240:240:240,245:245:245
allwhite= 255:255:255,255:255:255
pure_white=255:255:255,255:255:255,255:255:255,255:255:255,255:255:255,255:255:255,255:255:255,255:255:255,255:255:255

</PALETTES>




<LINETYPES>
# Add linetypes
dash1=    1111000011111111
</LINETYPES>




<QUICKMENUS>
#Add quick menus

</QUICKMENUS>


<FIELD_COMPUTE>
EOT
    # add the sum for accumulation of aerosols
    foreach my $type (qw(accum.dry.dep_ accum.wet.dep_ conc.average_ conc.instant_ conc.accum_)) {
        print $f $type,"Total=sum(",
            join(',',map {$type.$isotopes->{$_}} keys %$isotopes),
            ")\n";
    }
    $isotopes->{sum} = "Total";

    foreach my $id (keys %$isotopes) {
        my $iso = $isotopes->{$id};
        print $f "${iso}_concentration=add(conc.average_${iso},0)\n";
        print $f "${iso}_concentration_instant=add(conc.instant_${iso},0)\n";
        print $f "${iso}_concentration_accum=add(conc.accum_${iso},0)\n";
        print $f "${iso}_tdep=add(accum.dry.dep_${iso},accum.wet.dep_${iso})\n";
    }


print $f <<'EOT';

</FIELD_COMPUTE>


<FIELD_PLOT>

#  Clear the definitions from diana.setup-COMMON
CLEAR

#change plot options of existing field

# For netCDF files these use the variable names, not the standard_names.
# Thus, this section must be tailored for various datasets, i.e.,
# a separate setup file for each dataset.
# For example: different setup files for TOPAZ, MIPOM, ROMS, etc.


# SNAP and eEMEP

field=PRECIP.SNAP
      line.values=.2,.5,1.,2.,4.,6.,10.,15.,20.,25.
      colour=red linetype=solid linewidth=1
      plot=CONTOUR(precipitation_amount)
end.field

EOT


# the isotopes
    foreach my $id (sort keys %$isotopes) {
        my $iso = $isotopes->{$id};
        print $f <<"EOT";
field=${iso}_total_deposisjon
      colour=off log.line.values=100.,300. palettecolours=who_uvi table=1 units=Bq/m2
      plot=CONTOUR(${iso}_tdep)
end.field
field=${iso}_dry_deposisjon
      colour=off log.line.values=100.,300. palettecolours=who_uvi table=1 units=Bq/m2
      plot=CONTOUR(accum.dry.dep_${iso})
end.field
field=${iso}_wet_deposisjon
      colour=off log.line.values=100.,300. palettecolours=who_uvi table=1 units=Bq/m2
      plot=CONTOUR(accum.wet.dep_${iso})
end.field
field=${iso}_tidsintegrert_konsentrasjon_bakke
      colour=off log.line.values=100.,300. palettecolours=who_uvi table=1 units=Bq*hr/m3
      plot=CONTOUR(${iso}_concentration_accum)
end.field
field=${iso}_instantan_konsentrasjon_bakke
      colour=off log.line.values=100.,300. palettecolours=who_uvi table=1 units=Bq/m3
      plot=CONTOUR(${iso}_concentration_instant)
end.field

EOT
    }

print $f <<"EOT";
</FIELD_PLOT>

<FIELD_FILES>

filegroup=SNAP
m=SNAP.${tag} t=fimex format=netcdf f=./${tag}.nc


</FIELD_FILES>
EOT


    print $f <<'EOT';

<MAP_AREA>

#add new map area or change existing area
name=EMEP_PROJ proj4string="+proj=stere +lat_0=90 +lon_0=-32 +ellps=WGS84 +towgs84=0,0,0 +no_defs" rectangle=1.23949e+07:1.3471e+07:6.59011e+06:6.97476e+06
</MAP_AREA>


<MAP_TYPE>

#map=elver_grenser    file=/metno/local/maps/land5.dat    # emep(grenser,elver)

</MAP_TYPE>


<OBJECTS>

</OBJECTS>


<VERTICAL_CROSSECTION_FILES>

</VERTICAL_CROSSECTION_FILES>


<VERTICAL_CROSSECTION_PARAMETERS>

</VERTICAL_CROSSECTION_PARAMETERS>


<VERTICAL_CROSSECTION_COMPUTATIONS>

</VERTICAL_CROSSECTION_COMPUTATIONS>


<VERTICAL_CROSSECTION_PLOTS>

</VERTICAL_CROSSECTION_PLOTS>

<SPECTRUM_FILES>

# SAR-derived wave spectrum files
m=ASAR-IMS,     f=/home/bruceh/Project/Gov/NRS/SAR_wavespec/asar-ims_wamspec.dat

</SPECTRUM_FILES>

EOT
    close $f;
}

__END__

=head1 NAME

felt2ncDummyLevels2Isotopes.pl - TBD

=head1 SYNOPSIS

felt2diana.pl --tag=2013_03_12_dounreay [--levelnames=snap.felt_level_names --xml_template=felt2nc_snap_dummy_levels.xml --felt_input=felt2nc_snap.xml]

=head1 DESCRIPTION

Convert the felt-output from snap to a netcdf. Create a diana.setup-file
to read that input, and start diana with it.

Uses felt2ncDumyLevels2Isotopes.pl, fimex and diana.bin.

=head2 GENERAL SYNTAX

=over 4

=item

=back

=head1 EXAMPLE

=head1 AUTHOR

Heiko Klein, E<lt>Heiko.Klein@met.noE<gt>

=head1 SEE ALSO

L<perl>.

=cut
