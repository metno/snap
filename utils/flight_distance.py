#! /usr/bin/env python3

# SNAP: Servere Nuclear Accident Programme
# Copyright (C) 1992-2021   Norwegian Meteorological Institute
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

import argparse
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import math
import netCDF4
import numpy as np
import pathlib
import sys

def distance(clon, clat, lons, lats):
    '''calculate the distance of cells given by lons,lats from a clon,clat
       using the haversine formula
    '''
    from math import sin, cos, sqrt, atan2, radians
    R = 6371.

    rclat = radians(clat)
    rclon = radians(clon)
    rlats = np.radians(lats)
    rlons = np.radians(lons)

    dlon = rlons - rclon
    dlat = rlats - rclat

    a = np.sin(dlat / 2)**2 + cos(rclat) * np.cos(rlats) * np.sin(dlon / 2)**2
    c = 2 * np.arctan2(np.sqrt(a), np.sqrt(1 - a))

    return R * c


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Plot the shortest downwind distance for concentration/deposition from  SNAP output files"
    )
    parser.add_argument("--input", type=pathlib.Path, required=True)
    parser.add_argument("--output", type=pathlib.Path, help="output-file", required=True)
    parser.add_argument("--parameter", type=str, help="comma-separated list of parameters (accumulated)", default="Cs137_acc_concentration")
    parser.add_argument("--timestep", type=int, help="timestep to analyze", default="-1")
    parser.add_argument("--lat", type=float, help="source-latitude", required=True)
    parser.add_argument("--lon", type=float, help="source-longitude", required=True)
    args = parser.parse_args()

    lon = args.lon
    lat = args.lat
    tstep = args.timestep
    ifile = args.input
    ofile = args.output
    params = args.parameter.split(',')

    with netCDF4.Dataset(ifile, 'r') as nc:
        dist = distance(lon, lat, nc['longitude'][:], nc['latitude'][:])

        #print(np.min(dist), np.max(dist))
        data = 0
        xvals = []
        yvals = []
        for p in params:
            data += nc[p][tstep,:]
        maxlog = math.ceil(math.log10(np.max(data)))
        print(np.max(data), maxlog)
        thresholds = np.logspace(maxlog-9,9,num=400, dtype='float')
        times = netCDF4.num2date(nc['time'][:], nc['time'].units)
        td = (times[tstep] - times[0]) + (times[1] - times[0]) # snap omits writing first timestep, so add one
        hours = td.days*24 + td.seconds//3600
        data /= hours # average
        for i in range(len(thresholds)):
            mask = data > thresholds[i]
            if (np.sum(mask) > 0):
                xvals.append(np.max(dist[mask]))
                yvals.append(thresholds[i])

    fig = plt.figure()
    ax = plt.axes()
    ax.plot(xvals, yvals, color='b', label=", ".join(params))
    ax.legend()
    ax.plot([1,300], [1000, 1000], color='y' )
    ax.plot([1,300], [10000, 10000], color='r' )
    ax.set_yscale('log')
    ax.set_xscale('log')
    ax.set_xlabel('Downwind distance [km]')
    ax.set_ylabel(f'Bq/m³ ({hours}h avg.)')
    ax.grid('b', which='both')
    formatter = matplotlib.ticker.ScalarFormatter()
    formatter.set_powerlimits((-3,10))
    ax.xaxis.set_major_formatter(formatter)
    # ax.set_xlim([1,200])
    fig.savefig(ofile)
