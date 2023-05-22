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

import netCDF4
import numpy


def add_toa_to_nc(nc: netCDF4.Dataset):
    """
    add time of arrival and total depositions
    to the nc-filehandle (netCDF4.Dataset)
    filehandle must be rewriteable snap-output file
    """
    if "time_of_arrival" in nc.variables:
        return
    times = netCDF4.num2date(nc.variables["time"][:], units=nc.variables["time"].units)
    timeMax = abs(int((times[-1] - times[0]).total_seconds() / (60 * 60)))
    timeDelta = abs(int((times[1] - times[0]).total_seconds() / (60 * 60)))
    data = 0
    exampleVar = ""
    for v in list(nc.variables.keys()):
        if v[-18:] == "_acc_concentration" or v[-19:] == "_acc_wet_deposition":
            exampleVar = v
            data += nc.variables[v][:]
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
        print("overwriting existing variable time_of_arrival")
    else:
        toaVar = nc.createVariable(
            "time_of_arrival",
            "f",
            nc.variables[exampleVar].dimensions,
            zlib=True,
            fill_value=-999,
        )
    toaVar.units = "hours"
    for attr in ("grid_mapping", "coordinates"):
        if attr in nc.variables[exampleVar].ncattrs():
            toaVar.setncattr(attr, nc.variables[exampleVar].getncattr(attr))
    toaVar[0, :] = toa

    # add also total depositions
    for v in list(nc.variables.keys()):
        if v[-19:] == "_acc_wet_deposition":
            comp = v[:-19]
            #            print(comp)
            total = (
                nc.variables[v][:]
                + nc.variables["{}_acc_dry_deposition".format(comp)][:]
            )
            totalVarName = "{}_acc_total_deposition".format(comp)
            if totalVarName not in nc.variables:
                totalVar = nc.createVariable(
                    totalVarName, "f", nc.variables[v].dimensions, zlib=True
                )
                totalVar.units = nc.variables[v].units
                for attr in ("grid_mapping", "coordinates", "standard_name"):
                    if attr in nc.variables[v].ncattrs():
                        totalVar.setncattr(attr, nc.variables[v].getncattr(attr))
                totalVar[:] = total


def main():
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("snapNc", help="snap.nc file to be changed")
    args = parser.parse_args()

    with netCDF4.Dataset(args.snapNc, "a") as nc:
        add_toa_to_nc(nc)


if __name__ == "__main__":
    main()
