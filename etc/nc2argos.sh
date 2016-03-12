#! /bin/bash

set -x

infile=/lustre/storeB/users/heikok/2016_Chernobyl/Runs/snapChernobyl2.nc
outdir=/tmp/`basename $infile .nc`_SNAP
mkdir -p $outdir
nobelgases="Xe133"
deps="Cs137 Cs134 Sr90 I131"

projection=--interpolate.projString="+proj=latlong +R=6371000 +no_defs" --interpolate.xAxisUnit=degree --interpolate.yAxisUnit=degree --interpolate.xAxisValues=2,2.1,...,34 --interpolate.yAxisValues=47,47.1,...,74

#concentrations
extract=''
for x in $deps $nobelgases; do
  extract="$extract --extract.selectVariables=${x}_concentration"
done
fimex --input.file="$infile" $projection $extract --output.file=${outdir}/`basename $infile .nc`_SNAP_conc --output.type=grib --output.config=cdmGribWriterIsotopesNc.xml

#accumulated concentrations
extract=''
for x in $deps $nobelgases; do
  extract="$extract --extract.selectVariables=${x}_acc_concentration"
done
fimex --input.file="$infile" $projection $extract --output.file=${outdir}/`basename $infile .nc`_SNAP_dose --output.type=grib --output.config=cdmGribWriterIsotopesNc.xml

#wet depositions
extract=''
for x in $deps; do
  extract="$extract --extract.selectVariables=${x}_acc_wet_deposition"
done
fimex --input.file="$infile" $projection $extract --output.file=${outdir}/`basename $infile .nc`_SNAP_wdep --output.type=grib --output.config=cdmGribWriterIsotopesNc.xml

#total depositions
extract=''
extract2=''
ncap2com=''
for x in $deps; do
  extract="$extract --extract.selectVariables=${x}_acc_wet_deposition --extract.selectVariables=${x}_acc_dry_deposition"
  extract2="$extract2 --extract.selectVariables=${x}_acc_total_deposition"
  ncap2com="$ncap2com ${x}_acc_total_deposition=${x}_acc_wet_deposition+${x}_acc_dry_deposition;"
done
fimex --input.file="$infile" $projection $extract --output.file=${outdir}/temp.nc
ncap2 -s "$ncap2com" ${outdir}/temp.nc ${outdir}/temp2.nc
fimex --input.file=${outdir}/temp2.nc $extract2 --output.file=${outdir}/`basename $infile .nc`_SNAP_tdep --output.type=grib --output.config=cdmGribWriterIsotopesNc.xml

# precipitation
fimex --input.file="$infile" $projection --extract.selectVariables=precipitation_amount_acc --process.deaccumulateVariable=precipitation_amount_acc --output.file=${outdir}/`basename $infile .nc`_SNAP_prec --output.type=grib --output.config=cdmGribWriterIsotopesNc.xml

