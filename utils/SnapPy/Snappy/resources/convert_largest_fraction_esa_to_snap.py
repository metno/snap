#!/usr/bin/env python
# coding: utf-8

import numpy as np
import xarray as xr
import pandas as pd
import pathlib

import sys


def add_attributes(largest_fraction):
    # Add attributes
    largest_fraction.attrs["Conventions"] = "CF-1.4"
    largest_fraction["Main_Nature_Cover"].attrs["units"] = "1"
    largest_fraction["Main_Nature_Cover"].attrs["long_name"] = (
        "Nature cover with the largest fraction"
    )
    largest_fraction["Main_Nature_Cover"].attrs["comment"] = (
        "11: Sea, 12: Inland water, 13: Tundra/desert, 14: Ice and ice sheets, 15: Urban, 16: Crops, 17: Grass, "
        "18: Wetlands, 19: Evergreen needleleaf, 20: Deciduous broadleaf, 21: Mixed forest, 22: Shrubs and interrupted woodlands"
    )
    largest_fraction["Main_Nature_Cover"].attrs["coordinates"] = "lon lat"
    largest_fraction["Main_Nature_Cover"].attrs["_FillValue"] = np.uint8(0)

    largest_fraction["Main_Category"].attrs["units"] = "1"
    largest_fraction["Main_Category"].attrs["long_name"] = (
        "Main category with the largest fraction"
    )
    largest_fraction["Main_Category"].attrs["comment"] = (
        "1: Nature, 2: Inland water, 3: Sea, 4: Urban"
    )
    largest_fraction["Main_Category"].attrs["coordinates"] = "lon lat"
    largest_fraction["Main_Category"].attrs["_FillValue"] = np.uint8(0)

    return largest_fraction


def convert_dtype(largest_fraction):
    # Convert to uint8 for extra space saving
    largest_fraction["Main_Nature_Cover"] = largest_fraction[
        "Main_Nature_Cover"
    ].astype(np.uint8)
    largest_fraction["Main_Category"] = largest_fraction["Main_Category"].astype(
        np.uint8
    )
    largest_fraction["lat"] = largest_fraction["lat"].astype(np.float32)
    largest_fraction["lon"] = largest_fraction["lon"].astype(np.float32)
    return largest_fraction


def calculate_largest_fraction(fractions):
    """Calculate largest class of the largest main category.

    Example: A grid cell with 35% grass, 25% forest and 40% water has largest
    main category Nature (60%), and as such the largest class is set to grass.
    """
    largest_category = (
        fractions["Main_Nature_Cover"].groupby("main_category").sum("snap_class")
    )
    # calculate largest class per category
    categories = []
    largest_classes = []

    for category, item in fractions.groupby("main_category"):
        cat_largest_index = item["Main_Nature_Cover"].argmax(dim="snap_class")
        cat_largest_class = item["snap_class"][cat_largest_index]

        categories.append(category)
        largest_classes.append(
            cat_largest_class.drop_vars("snap_class").drop_vars("main_category")
        )
    largest_classes = xr.concat(largest_classes, dim="main_category")
    largest_classes.coords["main_category"] = categories

    # get largest class of largest category
    largest_fraction = largest_classes[largest_category.argmax(dim="main_category")]
    largest_fraction.name = "Main_Nature_Cover"
    largest_fraction = (
        largest_fraction.to_dataset()
        .reset_coords(["main_category"])
        .rename({"main_category": "Main_Category"})
    )
    return largest_fraction


def convert_esa_to_snap(input_path, output_path, lookup_table="esa_to_snap.csv"):
    # load aggregated ifs data
    fractions_esa = xr.load_dataarray(input_path).drop_vars("time")
    fractions_esa.name = "lccs"
    fractions_esa = fractions_esa.to_dataset()

    # Load class lookup table
    lookup_table = pd.read_csv(lookup_table, index_col=0)
    # Map from value to value
    lookup_table = lookup_table.set_index("esa_value")["snap_value"].to_dict()

    # Convert fractional classes to snap
    fractions_esa = fractions_esa.assign_coords(
        snap_class=(
            "lccs_class",
            [lookup_table[lc] for lc in fractions_esa.lccs_class.values],
        )
    )
    fractions_snap = fractions_esa.groupby("snap_class").sum("lccs_class")

    fractions_snap = fractions_snap.rename({"lccs": "Main_Nature_Cover"})

    snap_class_to_main_category = {
        11: 3,
        12: 2,
        13: 1,
        14: 1,
        15: 4,
        16: 1,
        17: 1,
        18: 1,
        19: 1,
        20: 1,
        21: 1,
        22: 1,
    }

    fractions_snap = fractions_snap.assign_coords(
        main_category=(
            "snap_class",
            [
                snap_class_to_main_category[lc]
                for lc in fractions_snap.snap_class.values
            ],
        )
    )

    largest_fraction_snap = calculate_largest_fraction(fractions_snap)

    largest_fraction_snap = add_attributes(largest_fraction_snap)
    largest_fraction_snap = convert_dtype(largest_fraction_snap)

    print(f"Saving data converted from {input_path} to {output_path}")
    largest_fraction_snap.to_netcdf(output_path)
    print("Done")


def get_args():
    import argparse

    parser = argparse.ArgumentParser(
        description="This script processes land cover data to determine the largest fractional land cover type for a given grid cell. The script reads an input NetCDF file containing land cover fractions, applies a mapping from ESA CCI (European Space Agency Climate Change Initiative) land cover classes to SNAP classes using a lookup table, and calculates the largest fractional land cover type along with its corresponding main category. The transformed data is saved to an output NetCDF file."
    )
    parser.add_argument(
        "--input_path",
        default="./LandCoverFractions_EsaCCI_ecemep.nc",
        type=pathlib.Path,
        help="Path to the input NetCDF file containing land cover fractions (default: ./LandCoverFractions_EsaCCI_ecemep.nc).",
    )
    parser.add_argument(
        "--lookup_table",
        default="./esa_to_snap.csv",
        type=pathlib.Path,
        help="Path to the CSV lookup table that maps ESA land cover classes to SNAP classes (default: ./esa_to_snap.csv).",
    )
    parser.add_argument(
        "--output_path",
        default="./largestLandFraction_NrpaEC0p1.nc",
        type=pathlib.Path,
        help="Path to the output NetCDF file where the processed data will be saved (default: ./largestLandFraction_NrpaEC0p1.nc).",
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Overwrite the existing output file if it already exists.",
    )
    return parser.parse_args()


def main():
    args = get_args()
    if args.output_path.exists() and not args.overwrite:
        print(
            f"Error, output file {args.output_path} exists. Use --overwrite to overwrite"
        )
        sys.exit(1)

    convert_esa_to_snap(
        args.input_path, args.output_path, lookup_table=args.lookup_table
    )


if __name__ == "__main__":
    main()
