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
'''
Created on Feb 12, 2018

@author: heikok
'''
import logging
import math
import re
import netCDF4
import numpy as np
from netCDF4 import Dataset
from collections import deque

def flightlevel_in_pressure(flightlevel):
    '''convert flightlevel (in 100 ft) to pressure in hPa using the international height formula'''
    h = flightlevel * 100 * 0.3048 # feet to meter
    p = 1013.25 * math.pow(1-(0.0065*h)/288.15,5.255) # international formula
    return p

logger = logging.getLogger('SixHourMax')

class SixHourMax:
    """Calculate the 6h max VAAC output from 3D eemep-hourly mean output

i.e. calculate the 6hour mean of the last six hours (running) (average also the surface pressure)
     retrieve the max value within the fat flight layers (FL == atmospheric pressure altitude above 1013.25hPa) (0-200, 200-350, 350-550)

     The peak to mean factor, which was 10 for Eyjafjella is now set to 1 following
      (https://www.mdpi.com/2073-4433/11/4/352 Becket et al. 2020 (VAAC developments))
    """
    # performance option
    USE_2D = False
    FL = (200, 350, 550)
    PEAK_TO_MEAN_CALIBRATION = 1.
    SNAP_ASH_CONC_PATTERN = re.compile(r"ASH(_\d+)?_concentration_ml")
    SNAP_ASH_COL_PATTERN = re.compile(r"ASH(_\d+)?_column_concentration")
    SNAP_SO2_CONC_PATTERN = re.compile(r"SO2_concentration_ml")
    SNAP_SO2_COL_NAME = "SO2_column_concentration"

    @classmethod
    def detect_ash_model(cls, nc: netCDF4.Dataset) -> str:
        """check what type of ash-model the netcdf-dataset is

        :param nc: netCDF4 dataset to check
        :return: 'emep' or 'snap' or '' (undetected)
        """
        if 'D3_ug_ASH' in nc.variables:
            return 'emep'
        else:
            for vname, _ in nc.variables.items():
                if re.match(cls.SNAP_ASH_CONC_PATTERN, vname):
                    return 'snap'
        return ''

    def _detect_model(self):
        model = self.detect_ash_model(self.nc)
        if model == '':
            raise Exception('cannot detect model of nc-file')
        else:
            self.model = model
        return

    def _get_3d_dims(self):
        if self.model == 'emep':
            return [self.nc['D3_ug_ASH'].dimensions[i] for i in (0,2,3)]
        elif self.model == 'snap':
            for vname, v in self.nc.variables.items():
                if re.match(self.SNAP_ASH_CONC_PATTERN, vname):
                    return [v.dimensions[i] for i in (0,2,3)]
        raise Exception('cannot detect dimension names in nc-file')

    def _get_ash_attrib(self, attrib: str):
        if self.model == 'emep':
            if attrib in self.nc['D3_ug_ASH'].ncattrs():
                return self.nc['D3_ug_ASH'].getncattr(attrib)
        elif self.model == 'snap':
            for vname, v in self.nc.variables.items():
                if re.match(self.SNAP_ASH_CONC_PATTERN, vname):
                    if attrib in v.ncattrs():
                        val = v.getncattr(attrib)
                        if attrib == "units":
                            val = val.replace('Bq', 'g')
                        return val
        return None

    def _get_ap_levels(self):
        # in hPa
        if self.model == 'emep':
            return np.ma.filled(self.nc['hyam'][:], np.nan)
        elif self.model == 'snap':
            return np.ma.filled(self.nc['ap'][:], np.nan)
        raise Exception(f'unknown model: {self.model}')

    def _get_b_levels(self):
        if self.model == 'emep':
            return np.ma.filled(self.nc['hybm'][:], np.nan)
        elif self.model == 'snap':
            return np.ma.filled(self.nc['b'][:], np.nan)
        raise Exception(f'unknown model: {self.model}')

    def _get_surface_pressure(self, t: int):
        if self.model == 'emep':
            return np.ma.filled(self.nc['PS'][t,:,:], np.nan)
        elif self.model == 'snap':
            return np.ma.filled(self.nc['surface_air_pressure'][t,:,:], np.nan)
        raise Exception(f'unknown model: {self.model}')


    def _get_ash_data(self, t: int) -> np.ndarray:
        if self.model == 'emep':
            return np.ma.filled(self.nc['D3_ug_ASH'][t,:,:,:], np.nan)
        elif self.model == 'snap':
            data = 0
            for vname, v in self.nc.variables.items():
                if re.match(self.SNAP_ASH_CONC_PATTERN, vname):
                    data += np.ma.filled(v[t,:,:,:], np.nan)
            return data
        raise Exception(f'unknown model: {self.model}')

    def _snap_add_total_ash_column(self) -> None:
        if self.model != 'snap':
            return
        exampleVar = None
        data = 0
        for vName, var in self.nc.variables.items():
            if re.match(self.SNAP_ASH_COL_PATTERN, vName):
                var.units = var.units.replace('Bq', 'g')
                exampleVar = var
                data += var[:]
        if exampleVar:
            logger.info(f"adding COLUMN_ASH_kmax to snap-model")
            columnName = 'COLUMN_ASH_kmax' # name in emep-model
            if not columnName in self.nc.variables:
                self.nc.createVariable(columnName,'f4',exampleVar.dimensions, zlib=True)
            v = self.nc[columnName]
            for attr in ("units", "grid_mapping", "coordinates"):
                if attr in exampleVar.ncattrs():
                    v.setncattr(attr, exampleVar.getncattr(attr))
            v[:] = data
            self.nc.sync()
        return

    def _snap_rename_total_so2_column(self) -> None:
        if self.model != 'snap':
            return
        if self.SNAP_SO2_COL_NAME in self.nc.variables:
            self.nc.renameVariable(self.SNAP_SO2_COL_NAME, "COLUMN_SO2_kmax")
            self.nc["COLUMN_SO2_kmax"].units = self.nc["COLUMN_SO2_kmax"].units.replace('Bq', 'g')
            self.nc.sync()
        return


    def __init__(self, nc):
        '''Initialize with Dataset nc'''
        logger.info("Adding 6hour max to nc-file")
        self.nc = nc
        self._detect_model()

        # ap and b at layer interface, from troposphere to surface
        if not 'MAX6h_ASH_fl000-200' in nc.variables:
            v200 = nc.createVariable('MAX6h_ASH_fl000-200','f4',self._get_3d_dims(), zlib=True)
            v350 = nc.createVariable('MAX6h_ASH_fl200-350','f4',self._get_3d_dims(), zlib=True)
            v550 = nc.createVariable('MAX6h_ASH_fl350-550','f4',self._get_3d_dims(), zlib=True)
        else:
            v200 = nc['MAX6h_ASH_fl000-200']
            v350 = nc['MAX6h_ASH_fl200-350']
            v550 = nc['MAX6h_ASH_fl350-550']

        for attr in ("units", "grid_mapping", "coordinates"):
            val = self._get_ash_attrib(attr)
            if val:
                v200.setncattr(attr, val)
                v350.setncattr(attr, val)
                v550.setncattr(attr, val)

        hy_ap = self._get_ap_levels() # in hPa
        hy_b = self._get_b_levels()

        time = nc['time'][:]
        ps = deque(maxlen=6)
        ash = deque(maxlen=6)
        pFL = [flightlevel_in_pressure(x) for x in SixHourMax.FL]
        for t in range(len(time)):
            ash.append(self._get_ash_data(t))
            ps.append(self._get_surface_pressure(t))

            pa = sum(ps) / len(ps)
            asha = sum(ash) / len(ash)

            max200 = np.zeros(pa.shape, dtype='float')
            max350 = np.zeros(pa.shape, dtype='float')
            max550 = np.zeros(pa.shape, dtype='float')

            if self.USE_2D:
                for l in range(len(hy_ap)):
                    # 6h mean of ash in level l
                    ashal = np.squeeze(asha[l,:,:])
                    pal = pa * hy_b[l] + hy_ap[l]
                    tmp = np.where((pFL[0] < pal), ashal, 0)
                    max200 = np.maximum(tmp, max200)

                    tmp =  np.where((pFL[0] >= pal) & (pFL[1] < pal), ashal, 0)
                    max350 = np.maximum(tmp, max350)

                    tmp =  np.where((pFL[1] >= pal) & (pFL[2] < pal), ashal, 0)
                    max550 = np.maximum(tmp, max550)
            else:
                # 3d solution, 1.25x faster, 1.36x more memory (1.9G for NRPA-domain)
                pal = pa[np.newaxis,:,:]*hy_b[:,np.newaxis,np.newaxis] + hy_ap[:,np.newaxis,np.newaxis]
                max200 = np.max(asha, axis=0, where=(pFL[0] < pal), initial=0)
                max350 = np.max(asha, axis=0, where=(pFL[0] >= pal) & (pFL[1] < pal), initial=0)
                max550 = np.max(asha, axis=0, where=(pFL[1] >= pal) & (pFL[2] < pal), initial=0)


            # use factor for obs/model differences
            v200[t,:] = max200 * self.PEAK_TO_MEAN_CALIBRATION
            v350[t,:] = max350 * self.PEAK_TO_MEAN_CALIBRATION
            v550[t,:] = max550 * self.PEAK_TO_MEAN_CALIBRATION
            nc.sync()

        self._snap_add_total_ash_column()
        self._snap_rename_total_so2_column()
        return

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    print ('FL180=',flightlevel_in_pressure(180),'should result in ~ 500hPa')
#    import cProfile

    with Dataset('/home/heikok/emep_hour.nc', 'a') as nc:
    #with Dataset('/lustre/storeB/project/fou/kl/snap/run_ecens/20230918T00/snap_GRIMSVOTN_1.nc', 'a') as nc:
    # with Dataset('/lustre/storeB/project/fou/kl/eva/eemep/runs/etna/eemep_hour_20230918T143002.nc', 'a') as nc:
        SixHourMax(nc)
#    cProfile.run("SixHourMax('/lustre/storeB/project/fou/kl/eva/eemep/runs/Jan_Mayen_ondemand/eemep_hour.nc')")
