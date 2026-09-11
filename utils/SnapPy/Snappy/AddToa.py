#! /usr/bin/env python3
import logging

import netCDF4
import numpy as np
import pyproj

from Snappy.EEMEP.SixHourMax import SixHourMax


def add_toa_to_nc(nc: netCDF4.Dataset, overwrite=False):
    """
    add time of arrival and total depositions
    for ASH_ snap-files, creates also 6h-max and total-column
    to the nc-filehandle (netCDF4.Dataset)
    filehandle must be rewriteable snap-output file
    """
    try:
        # try adding lat/lon if missing
        _add_latlon_to_nc(nc)
    except Exception as e:
        logging.error(f"Failed to add lat/lon to nc file: {e}")

    if "time_of_arrival" in nc.variables:
        if not overwrite:
            return
    times = netCDF4.num2date(nc.variables["time"][:], units=nc.variables["time"].units)
    timeMax = abs(int((times[-1] - times[0]).total_seconds() / (60 * 60)))
    timeDelta = abs(int((times[1] - times[0]).total_seconds() / (60 * 60)))
    data = 0
    exampleVar = None
    for v, var in nc.variables.items():
        if v.endswith("_acc_concentration") or v.endswith("_acc_wet_deposition"):
            exampleVar = var
            data += var[:]
    if not isinstance(data, np.ndarray):
        raise Exception(
            f"no variable with *_acc_concentration found in file: {nc.filepath()}"
        )
    th = 0.0001  # low threshold
    # arrived: data >= th
    # flying: data < th
    data = np.where(data >= th, 0.0, timeDelta)
    # print(data.dtype) must be float!
    toa = np.sum(data, axis=0)
    toa[toa > timeMax] = -999
    # snap output start at first timeDelta, not 0, so 0 means 0 - 3h
    # make sure that timestamp means 'within this time', e.g. 0 -> 3
    # use slightly less, so that diana is satisfied [0,3[
    toa[toa != -999] += float(timeDelta) - 0.01

    if "time_of_arrival" in nc.variables:
        # for debugging or in special cases only
        toaVar = nc["time_of_arrival"]
        logging.warning("overwriting existing variable time_of_arrival")
    else:
        toaVar = nc.createVariable(
            "time_of_arrival",
            "f",
            exampleVar.dimensions,
            zlib=True,
            fill_value=-999,
        )
    toaVar.units = "hours"
    for attr in ("grid_mapping", "coordinates"):
        if attr in exampleVar.ncattrs():
            toaVar.setncattr(attr, exampleVar.getncattr(attr))
    toaVar[0, :] = toa

    # add also total depositions
    # run over list(keys()) since variables-dict is exanded in loop
    for v in list(nc.variables.keys()):
        var = nc[v]
        if v.endswith("_acc_dry_deposition"):
            total = var[:]
            comp = v.replace("_acc_dry_deposition", "")
            wdepName = f"{comp}_acc_wet_deposition"
            if wdepName in nc.variables:
                total += nc.variables[wdepName][:]
            totalVarName = f"{comp}_acc_total_deposition"
            if totalVarName not in nc.variables:
                totalVar = nc.createVariable(
                    totalVarName, "f", nc.variables[v].dimensions, zlib=True
                )
                totalVar.units = nc.variables[v].units
                for attr in ("grid_mapping", "coordinates", "standard_name"):
                    if attr in nc.variables[v].ncattrs():
                        totalVar.setncattr(attr, nc.variables[v].getncattr(attr))
                totalVar[:] = total
    nc.sync()

    # postprocess snap ash-files
    if SixHourMax.detect_ash_model(nc) == "snap":
        SixHourMax(nc)


def _add_latlon_to_nc(nc: netCDF4.Dataset):
    """add 2d latitude/longitude variables if the netcdf-file contains a CF
    grid-mapping and coordinates, but no auxiliary variables exist.

    :param nc: netcdf-dataset object
    """
    for vname in list(nc.variables.keys()):
        var = nc[vname]
        if "grid_mapping" in var.ncattrs() and "coordinates" in var.ncattrs():
            coords = var.getncattr("coordinates").split()
            lat_name = None
            lon_name = None
            for coord in coords:
                if coord.startswith("lat"):
                    lat_name = coord
                elif coord.startswith("lon"):
                    lon_name = coord
            if lat_name not in nc.variables and lon_name not in nc.variables:
                x_axis = var.dimensions[-1]
                y_axis = var.dimensions[-2]
                x_vals = nc.variables[x_axis][:]
                y_vals = nc.variables[y_axis][:]

                proj = pyproj.CRS.from_cf(nc[var.getncattr("grid_mapping")].__dict__)
                transformer = pyproj.Transformer.from_crs(
                    proj, pyproj.CRS.from_epsg(4326), always_xy=True
                )

                x_vals2d, y_vals2d = np.meshgrid(x_vals, y_vals)
                # Transform the coordinates to the projection defined by the grid_mapping
                lon_vals, lat_vals = transformer.transform(x_vals2d, y_vals2d)
                lat = nc.createVariable(lat_name, "f", (y_axis, x_axis), zlib=True)
                lon = nc.createVariable(lon_name, "f", (y_axis, x_axis), zlib=True)
                lat.units = "degrees_north"
                lon.units = "degrees_east"
                lat[:, :] = lat_vals
                lon[:, :] = lon_vals
                nc.sync()


def main():
    import argparse

    parser = argparse.ArgumentParser(
        description="""Add time_of_arrival, total_deposition to the nc-file.
                       For ASH, it adds also MAX6h_ASH in flightlevels
                       and COLUMN_ASH_kmax.
                       It will also add latitude and longitude variables if they are needed but missing.
                       """
    )
    parser.add_argument("snapNc", help="snap.nc file to be changed")
    parser.add_argument(
        "--overwrite",
        help="force rewriting time_of_arrival, even if it exists",
        action="store_true",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        help="Be verbose",
        action="store_const",
        dest="loglevel",
        const=logging.INFO,
        default=logging.WARNING,
    )
    args = parser.parse_args()
    logging.basicConfig(level=args.loglevel)
    with netCDF4.Dataset(args.snapNc, "a") as nc:
        add_toa_to_nc(nc, args.overwrite)


if __name__ == "__main__":
    main()
