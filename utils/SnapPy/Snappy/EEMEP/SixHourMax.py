# SNAP: Servere Nuclear Accident Programme
# Copyright (C) 1992-2018   Norwegian Meteorological Institute
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
"""
Created on Feb 12, 2018

@author: heikok
"""
import math
import numpy as np
from netCDF4 import Dataset
import datetime
from collections import deque


def flightlevel_in_pressure(flightlevel):
    """convert flightlevel (in 100 ft) to pressure in hPa using the international height formula"""
    h = flightlevel * 100 * 0.3048  # feet to meter
    p = 1013.25 * math.pow(1 - (0.0065 * h) / 288.15, 5.255)  # international formula
    return p


class SixHourMax:
    """Calculate the 6h max VAAC output from 3D eemep-hourly mean output

    i.e. calculate the 6hour mean of the last six hours (running) (average also the surface pressure)
         retrieve the max value within the fat flight layers (FL == atmospheric pressure altitude above 1013.25hPa) (0-200, 200-350, 350-550)
    """

    FL = (200, 350, 550)

    def __init__(self, nc):
        """Initialize with Dataset nc"""
        pFL = [flightlevel_in_pressure(x) for x in SixHourMax.FL]

        # ap and b at layer interface, from troposphere to surface
        v200 = nc.createVariable(
            "MAX6h_ASH_fl000-200", "f4", ("time", "lat", "lon"), zlib=True
        )
        v350 = nc.createVariable(
            "MAX6h_ASH_fl200-350", "f4", ("time", "lat", "lon"), zlib=True
        )
        v550 = nc.createVariable(
            "MAX6h_ASH_fl350-550", "f4", ("time", "lat", "lon"), zlib=True
        )

        v200.units = nc["D3_ug_ASH"].units
        v350.units = nc["D3_ug_ASH"].units
        v550.units = nc["D3_ug_ASH"].units

        hyai = nc["hyai"][:]  # in hPa
        hybi = nc["hybi"][:]

        time = nc["time"][:]
        lev = nc["lev"][:]
        ps = deque(maxlen=6)
        ash = deque(maxlen=6)
        for t in range(len(time)):
            ash.append(nc["D3_ug_ASH"][t, :, :, :])
            ps.append(nc["PS"][t, :, :])

            pa = sum(ps) / len(ps)
            asha = sum(ash) / len(ash)

            max200 = np.zeros(pa.shape, dtype="float")
            max350 = np.zeros(pa.shape, dtype="float")
            max550 = np.zeros(pa.shape, dtype="float")

            for l in range(len(lev)):
                # 6h mean of ash in level l
                ashal = np.squeeze(asha[l, :, :])
                pal = pa * hybi[l] + hyai[l]
                tmp = np.where((pFL[0] < pal), ashal, 0)
                max200 = np.maximum(tmp, max200)

                tmp = np.where((pFL[0] >= pal) & (pFL[1] < pal), ashal, 0)
                max350 = np.maximum(tmp, max350)

                tmp = np.where((pFL[1] >= pal) & (pFL[2] < pal), ashal, 0)
                max550 = np.maximum(tmp, max550)

                # print(np.info(max200))
            # print(datetime.datetime.now())
            v200[t, :] = max200 * 10.0
            v350[t, :] = max350 * 10.0
            v550[t, :] = max550 * 10.0
            nc.sync()


if __name__ == "__main__":
    print("FL180=", flightlevel_in_pressure(180), "should result in ~ 500hPa")
    #    import cProfile
    with Dataset("/disk1/Fimex/eemep_hour.nc", "a") as nc:
        SixHourMax(nc)
#    cProfile.run("SixHourMax('/lustre/storeB/project/fou/kl/eva/eemep/runs/Jan_Mayen_ondemand/eemep_hour.nc')")
