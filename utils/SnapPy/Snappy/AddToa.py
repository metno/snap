#! /usr/bin/env python3
#
# SNAP: Servere Nuclear Accident Programme
# Copyright (C) 1992-2017   Norwegian Meteorological Institute
#
# This file is part of SNAP. SNAP is free software: you can
# redistribute it and/or modify it under the terms of the
# GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

import logging
import netCDF4
import numpy

from Snappy.EEMEP.SixHourMax import SixHourMax

def add_toa_to_nc(nc: netCDF4.Dataset, overwrite=False):
    """
    add time of arrival and total depositions
    for ASH_ snap-files, creates also 6h-max and total-column
    to the nc-filehandle (netCDF4.Dataset)
    filehandle must be rewriteable snap-output file
    """
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
    if not isinstance(data, numpy.ndarray):
        raise Exception(
            f"no variable with *_acc_concentration found in file: {nc.filepath()}"
        )
    th = 0.0001 # low threshold
    # arrived: data >= th
    # flying: data < th
    data = numpy.where(data >= th, 0., timeDelta)
    # print(data.dtype) must be float!
    toa = numpy.sum(data, axis=0)
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
    for v, var in nc.variables.items():
        if v.endswith("_acc_dry_deposition"):
            total = var[:]
            comp = v.replace("_acc_dry_deposition","")
            wdepName = f"{comp}_acc_dry_deposition"
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
    if SixHourMax.detect_ash_model(nc) == 'snap':
        SixHourMax(nc)



if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="""Add time_of_arrival, total_deposition to the nc-file.
                       For ASH, it adds also MAX6h_ASH in flightlevels
                       and COLUMN_ASH_kmax."""
    )
    parser.add_argument("snapNc", help="snap.nc file to be changed")
    parser.add_argument("--overwrite",
                        help="force rewriting time_of_arrival, even if it exists",
                        action="store_true")
    parser.add_argument(
        '-v', '--verbose',
        help="Be verbose",
        action="store_const", dest="loglevel",
        const=logging.INFO,
        default=logging.WARNING
    )
    args = parser.parse_args()
    logging.basicConfig(level=args.loglevel)
    with netCDF4.Dataset(args.snapNc, "a") as nc:
        add_toa_to_nc(nc, args.overwrite)
