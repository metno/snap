#! /bin/bash
module load fimex-bin

INPUT=/lustre/storeB/project/fou/kl/cerad/Meteorology/AROME-CHERNOBYL/surfex/LargestFraction.nc

TMPOUTPUT=LargestFraction_MEPS_tmp.nc
# TEMPLATE=/lustre/storeB/immutable/archive/projects/metproduction/MEPS/2025/02/13/meps_det_2_5km_20250213T06Z.nc

fimex --input.file $INPUT --output.file $TMPOUTPUT --output.type nc4 \
  --interpolate.projString "+proj=lcc +lat_0=63.3 +lon_0=15 +lat_1=63.3 +lat_2=63.3 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs" \
  --interpolate.xAxisValues -1060084,-1057584,...,1309916 \
  --interpolate.yAxisValues -1332518,-1330018,...,1337482 \
  --interpolate.xAxisUnit m \
  --interpolate.yAxisUnit m \
  --interpolate.method nearestneighbor

OUTPUT=largestLandFraction_MEPS_byte.nc
# create a very small version suitable to be put under version control
fimex --input.file=$TMPOUTPUT  --ncml.config=shortify.ncml --output.file=$OUTPUT --output.type=nc4

rm $TMPOUTPUT
