#!/usr/bin/env python
# coding: utf-8

import pathlib
import pandas as pd
import numpy as np
import xarray as xr
from dask.diagnostics import ProgressBar
import sys


def _assert_coordinate_resolution_equals(ds, target_res, name="target"):
    _msg = f"resolution of {name} latitudes not equal to expected resolution of {target_res}"
    assert np.allclose(target_res, np.diff(np.sort(ds.lat.values))), _msg
    _msg = f"resolution of {name} longitudes not equal to expected resolution of {target_res}"
    assert np.allclose(target_res, np.diff(np.sort(ds.lon.values))), _msg


def open_datasets(input_path, template_path, input_res, output_res):
    ds = xr.open_dataset(input_path, chunks={"time": 1})
    ds = ds.isel(time=0)

    ds_template = xr.open_dataset(template_path)

    # Check validity of input and output grids
    _assert_coordinate_resolution_equals(ds, input_res, "input")
    _assert_coordinate_resolution_equals(ds_template, output_res, "output")

    return subset_dataset(ds, ds_template, output_res)


def subset_dataset(ds, ds_template, output_res):
    "Subset dataset ds using template dataset lat lon grid."

    # Template coordinates are assumed to be at grid cell centers, so we add
    # half a grid cell in all directions to include the entire grid cells
    min_lat = ds_template.lat.min() - output_res / 2
    max_lat = ds_template.lat.max() + output_res / 2
    min_lon = ds_template.lon.min() - output_res / 2
    max_lon = ds_template.lon.max() + output_res / 2

    # Get mapping from land class value to land class index
    return ds.sel(lat=(ds.lat >= min_lat) & (ds.lat < max_lat),
                  lon=(ds.lon >= min_lon) & (ds.lon < max_lon))


def _count_values(array, lccs_classes):
    """
    Count all integer values of an array.
    """
    n = np.max(lccs_classes) + 1
    counts = np.bincount(array.ravel(), minlength=n)
    return np.array([counts[v] for v in lccs_classes])


def aggregate_land_classes(ds, agg_factor, var_name="lccs_class"):
    """Aggregates dataset by reducing grid size by a factor agg_factor in each
    dimension"""
    da = ds[var_name]

    lccs_classes = da.flag_values
    # Coarsen grid, keeping coarse and fine dimensions
    coarse = da.coarsen(lon=agg_factor, lat=agg_factor, boundary="exact")
    regions = coarse.construct(lon=("lon", "x_fine"), lat=("lat", "y_fine"))

    # Aggregate regions over fine dimension, leaving coarse dimension
    counts_da = xr.apply_ufunc(
        _count_values,
        regions,
        input_core_dims=[["x_fine", "y_fine"]],
        output_core_dims=[[var_name]],
        dask_gufunc_kwargs=dict(output_sizes={var_name: len(lccs_classes)}),
        vectorize=True,
        dask="parallelized",
        output_dtypes=[float],
        kwargs=dict(lccs_classes=lccs_classes)
    )
    fractions_da = counts_da / agg_factor**2

    # Assign grid coords as center of grid lat/lon
    center_lat = regions.lat.mean(dim="y_fine")
    center_lon = regions.lon.mean(dim="x_fine")
    fractions_da = fractions_da.assign_coords({"lat": center_lat,
                                               "lon": center_lon,
                                               var_name: lccs_classes})
    fractions_da.attrs['Conventions'] = 'CF-1.4'
    fractions_da[var_name].attrs = ds[var_name].attrs

    fractions_da["lat"].attrs['standard_name'] = "latitude"
    fractions_da["lat"].attrs['long_name'] = "latitude"
    fractions_da["lat"].attrs['units'] = "degree_north"

    fractions_da["lon"].attrs['standard_name'] = "longitude"
    fractions_da["lon"].attrs['long_name'] = "longitude"
    fractions_da["lon"].attrs['units'] = "degree_east"

    fractions_da = fractions_da.reindex(lat=fractions_da.lat[::-1])

    print("Calculating grid cell fractions")
    with ProgressBar():
        return fractions_da.compute()


def get_args():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--input_path",
                        default="/lustre/storeB/project/fou/kl/cerad/Meteorology/Landuse/C3S-LC-L4-LCCS-Map-300m-P1Y-2022-v2.1.1.nc",
                        type=pathlib.Path)
    parser.add_argument("--output_path",
                        default="LandCoverFractions_EsaCCI_ecemep.nc",
                        type=pathlib.Path)
    parser.add_argument("--template_path",
                        default="/lustre/storeB/project/fou/kl/cerad/Meteorology/Landuse/meteo_template/meteo20251214_03.nc",
                        type=pathlib.Path,
                        help="Dataset with the lat lon grid used for the output")
    parser.add_argument("--input_res", type=float, default = 1 / 360,
                        help="Input resolution in degrees. Default is 1/360 degrees = 10 arcseconds.")
    parser.add_argument("--output_res", type=float, default = 0.1,
                        help="Output resolution in degrees. Default is 0.1 degrees.")
    parser.add_argument("--overwrite", action='store_true',
                        help="Overwrite existing output file.")
    return parser.parse_args()


def main():
    args = get_args()
    if args.output_path.exists() and not args.overwrite:
        print(f"Error, output file {args.output_path} exists. Use --overwrite to overwrite")
        sys.exit(1)

    agg_factor = args.output_res / args.input_res
    if not np.isclose(agg_factor % 2, 0):
        raise ValueError("Ratio of input and output resolutions must be divisible by 2, got {agg_factor=}")
    agg_factor = int(np.round(agg_factor))

    sub = open_datasets(input_path=args.input_path,
                        template_path=args.template_path,
                        input_res=args.input_res,
                        output_res=args.output_res)
    fractions_da = aggregate_land_classes(ds=sub, agg_factor=agg_factor, var_name="lccs_class")

    print(f"Saving data aggregated from {args.input_path} to {args.output_path}")
    fractions_da.to_netcdf(args.output_path)
    print("Done")


if __name__ == "__main__":
    main()
