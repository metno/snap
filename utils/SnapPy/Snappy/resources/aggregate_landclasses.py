#!/usr/bin/env python
# coding: utf-8

import pathlib
import numpy as np
import xarray as xr
from dask.diagnostics import ProgressBar
import sys


def check_coordinate_resolution_equals(
    ds,
    target_res,
    coord,
    name="target",
):
    """Check resolution of a given coordinate in dataset against an expected value"""
    _msg = "Warning! Mismatch in resolution of coord '{coord}' in ds '{name}'. Expected {target_res}, got deviations up to {max_dev}"

    diffs = np.diff(np.sort(ds.get(coord).values))

    if not np.allclose(target_res, diffs, rtol=1e-4):
        max_deviation = np.max(np.abs(diffs - target_res))
        msg = _msg.format(
            name=name, coord=coord, target_res=target_res, max_dev=max_deviation
        )
        print(msg)


def check_output_coord(da, da_template, coord):
    """Check that the coordinates of two datasets are equal (up to a given tolerance)"""
    _msg = "Warning! Resolution of coord '{coord}' between output and template does not match. Got deviations up to {max_dev} at i={index}/{size}"

    diffs = np.diff(da[coord].values)
    diffs_template = np.diff(np.sort(da_template[coord].values))

    if not np.allclose(diffs, diffs_template, rtol=1e-4):
        max_deviation = np.max(np.abs(diffs - diffs_template))
        index = np.argmax(np.abs(diffs - diffs_template))
        msg = _msg.format(
            coord=coord, max_dev=max_deviation, index=index, size=da[coord].size
        )
        print(msg)


def rename_coords(ds):
    names = {}
    for v in ds.coords:
        standard_name = ds[v].attrs.get("standard_name", "")
        if standard_name == "latitude":
            names[v] = "lat"
        elif standard_name == "longitude":
            names[v] = "lon"
    return ds.rename(names)


def open_datasets(input_path, template_path, input_res, output_res):
    ds = xr.open_dataset(input_path, chunks={"time": 1})
    ds = ds.isel(time=0)

    ds_template = xr.open_dataset(template_path)
    ds_template = rename_coords(ds_template)

    # Check validity of input and output grids
    check_coordinate_resolution_equals(ds, input_res, "lat", "input")
    check_coordinate_resolution_equals(ds, input_res, "lon", "input")
    check_coordinate_resolution_equals(ds_template, output_res, "lat", "template")
    check_coordinate_resolution_equals(ds_template, output_res, "lon", "template")

    return ds, ds_template


def subset_dataset(ds, ds_template, output_res):
    "Subset dataset ds using template dataset lat lon grid."

    # Template coordinates are assumed to be at grid cell centers, so we add
    # half a grid cell in all directions to include the entire grid cells
    min_lat = ds_template.lat.min() - output_res / 2
    max_lat = ds_template.lat.max() + output_res / 2
    min_lon = ds_template.lon.min() - output_res / 2
    max_lon = ds_template.lon.max() + output_res / 2

    # Get mapping from land class value to land class index
    return ds.sel(
        lat=(ds.lat >= min_lat) & (ds.lat < max_lat),
        lon=(ds.lon >= min_lon) & (ds.lon < max_lon),
    )


def roll_and_pad_coordinates(da, input_res, output_res):
    """Roll longitude and pad latitude to ensure region centers are correct at
    the boundaries, i.e. that the boundary grid points have grid centers at
    longitudes [-180 + resolution, 180] and latitudes [-90, 90]."""
    agg_factor = calc_agg_factor(input_res, output_res)
    agg_half = agg_factor // 2
    new_da = da.roll({"lon": agg_factor // 2}, roll_coords=True)

    lon = new_da["lon"].values
    lon[: agg_factor // 2] -= 360

    new_da = new_da.assign_coords({"lon": lon})

    new_da = new_da.pad({"lat": (agg_half, agg_half)}, mode="symmetric")

    lat = new_da.lat.values
    lat[:agg_half] = da.lat.values[0] + input_res * np.arange(1, agg_half + 1)[::-1]
    lat[-agg_half:] = da.lat.values[-1] - input_res * np.arange(1, agg_half + 1)
    new_da = new_da.assign_coords({"lat": lat})

    # Check that values are as expected
    lon = new_da.lon.values
    lon_regions = lon.reshape(lon.size // agg_factor, -1)
    lon_means = lon_regions.mean(axis=1)
    lon_means_expected = np.arange(-180, 180, output_res)
    assert np.allclose(lon_means, lon_means_expected, rtol=1e-4)

    lat = new_da.lat.values
    lat_regions = lat.reshape(lat.size // agg_factor, -1)
    lat_means = lat_regions.mean(axis=1)
    lat_means_expected = np.arange(90, -90 - output_res, -output_res)

    assert np.allclose(lat_means, lat_means_expected, rtol=1e-4)
    return new_da


def _count_values(array, possible_values):
    """
    Count all unique values of an array for a predefined set of possible values. Equal to np.unique(arr, return_counts=True) when all values are present.
    """
    n = np.max(possible_values) + 1
    # Faster than np.unique when n is not too large
    counts = np.bincount(array.ravel(), minlength=n)
    return np.array([counts[v] for v in possible_values])


def aggregate_land_classes(
    da: xr.DataArray, agg_factor: int, var_name: str = "lccs_class"
):
    """Aggregates land class values by dividing lat-lon grid into regions of size agg_factor² and counting values in each region."""

    land_class_values = da.flag_values
    # Coarsen grid, subdividing into regions of shape (agg_factor, agg_factor) with a fine sub-dimension
    coarse = da.coarsen(lon=agg_factor, lat=agg_factor, boundary="exact")
    regions = coarse.construct(lon=("lon", "x_fine"), lat=("lat", "y_fine"))

    # Counts land_class values in each regions by reducing over the fine dimensions. Counts are given along a new output dimension named by var_name.
    counts_da = xr.apply_ufunc(
        _count_values,
        regions,
        input_core_dims=[["x_fine", "y_fine"]],
        output_core_dims=[[var_name]],
        dask_gufunc_kwargs=dict(output_sizes={var_name: len(land_class_values)}),
        vectorize=True,
        dask="parallelized",
        output_dtypes=[float],
        kwargs=dict(possible_values=land_class_values),
    )
    fractions_da = counts_da / agg_factor**2

    # Assign grid coords as center of grid lat/lon
    center_lat = regions.lat.mean(dim="y_fine")
    center_lon = regions.lon.mean(dim="x_fine")
    fractions_da = fractions_da.assign_coords(
        {"lat": center_lat, "lon": center_lon, var_name: land_class_values}
    )

    fractions_da[var_name].attrs = da.attrs

    fractions_da["lat"].attrs["standard_name"] = "latitude"
    fractions_da["lat"].attrs["long_name"] = "latitude"
    fractions_da["lat"].attrs["units"] = "degree_north"

    fractions_da["lon"].attrs["standard_name"] = "longitude"
    fractions_da["lon"].attrs["long_name"] = "longitude"
    fractions_da["lon"].attrs["units"] = "degree_east"

    fractions_da = fractions_da.reindex(lat=np.sort(fractions_da.lat))

    print("Calculating grid cell fractions")
    return fractions_da

    # res.pathglob = "ec_atmo_0_1deg_????????T??????Z_3h.nc"


def get_args():
    import argparse

    parser = argparse.ArgumentParser(
        description="Aggregates high-resolution land class data onto a lower-resolution grid by calculating land class fractions. The input latitude-longitude grid is divided into regions centered around each point of the output grid, and in each region we count the occurance of each land class and calculate fractional land classes. The input grid must be commensurate with the output grid, and is defined by a template file."
    )
    parser.add_argument(
        "--input_path",
        default="/lustre/storeB/project/fou/kl/cerad/Meteorology/Landuse/C3S-LC-L4-LCCS-Map-300m-P1Y-2022-v2.1.1.nc",
        help="Input netcdf file. Lat lon grid must conform to CF conventions.",
        type=pathlib.Path,
    )
    parser.add_argument(
        "--land_class_variable",
        default="lccs_class",
        help="Variable of land class data in the input netcdf file. Must follow CF convention for flag values.",
        type=str,
    )
    parser.add_argument(
        "--output_path",
        default="LandCoverFractions_EsaCCI_ecemep.nc",
        help="Output netcdf file.",
        type=pathlib.Path,
    )
    parser.add_argument(
        "--template_path",
        default="/lustre/storeB/project/fou/kl/cerad/Meteorology/Landuse/meteo_template/meteo20251214_03.nc",
        type=pathlib.Path,
        help="Dataset defining the lat lon grid used for the output. Note that lat lon grid must be commensurate with the input grid.",
    )
    parser.add_argument(
        "--input_res",
        type=float,
        default=1 / 360,
        help="Input resolution in degrees. Must equal input grid resolution. Default 1/360 degrees = 10 arcseconds. ",
    )
    parser.add_argument(
        "--output_res",
        type=float,
        default=0.1,
        help="Output resolution in degrees. Must equal template grid resolution. Default 0.1 degrees.",
    )
    parser.add_argument(
        "--overwrite", action="store_true", help="Overwrite existing output file."
    )
    parser.add_argument(
        "--global_input",
        action="store_true",
        help="Assume global input with periodic longitude and output latitude bounds at +- 90°.",
    )
    return parser.parse_args()


def calc_agg_factor(input_res, output_res):
    """Calculate agg factor with check"""
    agg_factor = output_res / input_res
    if not np.isclose(agg_factor % 2, 0):
        raise ValueError(
            "Ratio of input and output resolutions must be divisible by 2, got {agg_factor=}"
        )
    return int(np.round(agg_factor))


def main():
    args = get_args()
    agg_factor = calc_agg_factor(args.input_res, args.output_res)

    if args.output_path.exists() and not args.overwrite:
        print(
            f"Error, output file {args.output_path} exists. Use --overwrite to overwrite"
        )
        sys.exit(1)

    ds, ds_template = open_datasets(
        input_path=args.input_path,
        template_path=args.template_path,
        input_res=args.input_res,
        output_res=args.output_res,
    )
    var_name = args.land_class_variable
    da = ds[var_name]
    if args.global_input:
        da = roll_and_pad_coordinates(da, args.input_res, args.output_res)
    else:
        da = subset_dataset(da, ds_template, args.output_res)

    fractions_da = aggregate_land_classes(
        da=da, agg_factor=agg_factor, var_name=var_name
    )

    # check that output grid is correct
    check_output_coord(fractions_da, ds_template, coord="lat")
    check_output_coord(fractions_da, ds_template, coord="lon")

    print(f"Saving data aggregated from {args.input_path} to {args.output_path}")
    with ProgressBar():
        fractions_da.to_netcdf(args.output_path)
    print("Done")


if __name__ == "__main__":
    main()
