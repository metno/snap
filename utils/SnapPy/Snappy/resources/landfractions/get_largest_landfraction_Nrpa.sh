#!/bin/bash

set -e
TMPOUTPUT=LargestFraction_nrpa.nc
OUTPUT=largestLandFraction_NrpaEC0p1.nc

echo "Aggregating land classes for Nrpa domain..."
python aggregate_landclasses.py --output-path $TMPOUTPUT --overwrite
echo "Converting classes to SNAP..."
python convert_largest_fraction_esa_to_snap.py --input-path $TMPOUTPUT --output-path $OUTPUT --overwrite

rm $TMPOUTPUT
