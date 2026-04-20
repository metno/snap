#!/bin/bash

set -e
shopt -s nullglob

# Get the list of current meteo files and select the first one as the template
_meteo_files=(/lustre/storeB/project/metproduction/products/ecmwf/nc/ec_atmo_0_1deg_????????T??????Z_3h.nc)
TEMPLATE="${_meteo_files[0]}"
echo "Using template file: $TEMPLATE"

TMPOUTPUT=LargestFraction_ECglobal_tmp.nc
OUTPUT=largestLandFraction_EC0p1Global.nc


echo "Aggregating land classes for EC global domain..."
python aggregate_landclasses.py --template-path=$TEMPLATE --output-path $TMPOUTPUT --overwrite --global-input
echo "Converting classes to SNAP..."
python convert_largest_fraction_esa_to_snap.py --input-path $TMPOUTPUT --output-path $OUTPUT --overwrite

rm $TMPOUTPUT
