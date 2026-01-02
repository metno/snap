#!/usr/bin/env python
# coding: utf-8

import numpy as np
import xarray as xr
import pandas as pd
import pathlib

import sys

def add_attributes(largest_fraction):
    # Add attributes
    largest_fraction.attrs['Conventions'] = 'CF-1.4'
    largest_fraction["Main_Nature_Cover"].attrs['units'] = "1"
    largest_fraction["Main_Nature_Cover"].attrs['long_name'] = "Nature cover with the largest fraction"
    comment = ("11: Sea, 12: Inland water, 13: Tundra/desert, 14: Ice and ice sheets, 15: Urban, 16: Crops, 17: Grass, "
               "18: Wetlands, 19: Evergreen needleleaf, 20: Deciduous broadleaf, 21: Mixed forest, 22: Shrubs and interrupted woodlands")
    largest_fraction["Main_Nature_Cover"].attrs['comment'] = comment
    largest_fraction["Main_Nature_Cover"].attrs['coordinates'] = "lon lat"
    largest_fraction["Main_Nature_Cover"].attrs['_FillValue'] = np.uint8(0)

    return largest_fraction


def convert_dtype(largest_fraction):
    # Convert to uint8 for extra space saving
    largest_fraction['Main_Nature_Cover'] = largest_fraction['Main_Nature_Cover'].astype(np.uint8)
    largest_fraction['lat'] = largest_fraction['lat'].astype(np.float32)
    largest_fraction['lon'] = largest_fraction['lon'].astype(np.float32)
    return largest_fraction


def calculate_largest_fraction(fractions, class_name='snap_class'):
    largest_fraction = fractions.argmax(dim=class_name)

    # Convert argmax indexes to class values
    largest_fraction['Main_Nature_Cover'] = fractions[class_name][largest_fraction['Main_Nature_Cover']]
    largest_fraction = largest_fraction.drop_vars('time').drop_vars(class_name)
    return largest_fraction

def convert_esa_to_snap(input_path, output_path, lookup_table='esa_to_snap.csv'):
    # load aggregated ifs data
    fractions_esa = xr.load_dataarray(input_path)
    fractions_esa.name = 'lccs'
    fractions_esa = fractions_esa.to_dataset()

    # Load class lookup table
    lookup_table = pd.read_csv(lookup_table, index_col=0)
    # Map from value to value
    lookup_table = lookup_table.set_index("esa_value")['snap_value'].to_dict()


    # Convert fractional classes to snap
    fractions_esa = fractions_esa.assign_coords(
        snap_class=("lccs_class", [lookup_table[lc] for lc in fractions_esa.lccs_class.values])
    )
    fractions_snap = fractions_esa.groupby("snap_class").sum("lccs_class")

    fractions_snap = fractions_snap.rename({"lccs": "Main_Nature_Cover"})

    # Calculate largest fraction
    largest_fraction_snap = calculate_largest_fraction(fractions_snap)

    largest_fraction_snap = add_attributes(largest_fraction_snap)
    largest_fraction_snap = convert_dtype(largest_fraction_snap)

    # fractions_snap.to_netcdf('LandFractionEC.nc')
    print(f"Saving data converted from {args.input_path} to {args.output_path}")
    largest_fraction_snap.to_netcdf(output_path)
    print("Done")


def get_args():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--input_path",
                        default="LandCoverFractions_EsaCCI_ecemep.nc",
                        type=pathlib.Path)
    parser.add_argument("--lookup_table", default="esa_to_snap.csv",
                        type=pathlib.Path)
    parser.add_argument("--output_path", default="LargestFractionEC.nc",
                        type=pathlib.Path)
    parser.add_argument("--overwrite", action='store_true',
                        help="Overwrite existing output file.")
    return parser.parse_args()


if __name__ == "__main__":
    args = get_args()
    if args.output_path.exists() and not args.overwrite:
        print(f"Error, output file {args.output_path} exists. Use --overwrite to overwrite")
        sys.exit(1)

    convert_esa_to_snap(args.input_path, args.output_path, lookup_table=args.lookup_table)
