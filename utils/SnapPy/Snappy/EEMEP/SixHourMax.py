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
import math
import numpy as np
from netCDF4 import Dataset
import datetime
from collections import deque

def flightlevel_in_pressure(flightlevel):
    '''convert flightlevel (in 100 ft) to pressure in hPa using the international height formula'''
    h = flightlevel * 100 * 0.3048 # feet to meter
    p = 1013.25 * math.pow(1-(0.0065*h)/288.15,5.255) # international formula
    return p

class SixHourMax:
    '''Calculate the 6h max VAAC output from 3D eemep-hourly mean output

i.e. calculate the 6hour mean of the last six hours (running) (average also the surface pressure)
     retrieve the max value within the fat flight layers (FL == atmospheric pressure altitude above 1013.25hPa) (0-200, 200-350, 350-550)

     Multiply by 10, to match observations from Eyjafjella. (https://www.mdpi.com/2073-4433/11/4/352 Becket et al. 2020 (VAAC developments))
    '''
    # performance option
    USE_2D = False

    FL = (200, 350, 550)

    def _detect_model(self):
        if 'D3_ug_ASH' in self.nc.variables:
            self.model = 'emep'
            return
        else:
            for vname, v in self.nc.variables.items():
                if vname.startswith('ASH_') and vname.endswith('_concentration_ml'):
                    self.model = 'snap'
                    return
        raise Exception('cannot detect model of nc-file')

    def _get_3d_dims(self):
        if self.model == 'emep':
            return [self.nc['D3_ug_ASH'].dimensions[i] for i in (0,2,3)]
        elif self.model == 'snap':
            for vname, v in self.nc.variables.items():
                if vname.startswith('ASH_') and vname.endswith('_concentration_ml'):
                    return [v.dimensions[i] for i in (0,2,3)]
        raise Exception('cannot detect dimension names in nc-file')

    def _get_ash_units(self):
        if self.model == 'emep':
            return self.nc['D3_ug_ASH'].units
        elif self.model == 'snap':
            for vname, v in self.nc.variables.items():
                if vname.startswith('ASH_') and vname.endswith('_concentration_ml'):
                    return v.units.replace('Bq', 'g')
        raise Exception('cannot detect units in nc-file')

    def _get_ap_levels(self):
        # in hPa
        if self.model == 'emep':
            return np.ma.filled(nc['hyam'][:], np.nan)
        elif self.model == 'snap':
            return np.ma.filled(nc['ap'][:], np.nan)
        raise Exception(f'unknown model: {self.model}')

    def _get_b_levels(self):
        if self.model == 'emep':
            return np.ma.filled(nc['hybm'][:], np.nan)
        elif self.model == 'snap':
            return np.ma.filled(nc['b'][:], np.nan)
        raise Exception(f'unknown model: {self.model}')

    def _get_surface_pressure(self, t: int):
        if self.model == 'emep':
            return np.ma.filled(nc['PS'][t,:,:], np.nan)
        elif self.model == 'snap':
            return np.ma.filled(nc['surface_air_pressure'][t,:,:], np.nan)
        raise Exception(f'unknown model: {self.model}')


    def _get_ash_data(self, t: int):
        if self.model == 'emep':
            return np.ma.filled(nc['D3_ug_ASH'][t,:,:,:], np.nan)
        elif self.model == 'snap':
            data = 0
            for vname, v in self.nc.variables.items():
                if vname.startswith('ASH_') and vname.endswith('_concentration_ml'):
                    data += np.ma.filled(v[t,:,:,:], np.nan)
            return data
        raise Exception(f'unknown model: {self.model}')


    def __init__(self, nc):
        '''Initialize with Dataset nc'''
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

        v200.units = self._get_ash_units()
        v350.units = v200.units
        v550.units = v200.units

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


            # add factor 10 for obs/model differences
            v200[t,:] = max200 * 10.
            v350[t,:] = max350 * 10.
            v550[t,:] = max550 * 10.
            nc.sync()


if __name__ == '__main__':
    print ('FL180=',flightlevel_in_pressure(180),'should result in ~ 500hPa')
#    import cProfile
    # with Dataset('/disk1/Fimex/eemep_hour.nc', 'a') as nc:
    with Dataset('/lustre/storeB/project/fou/kl/snap/run_ecens/20230918T00/snap_GRIMSVOTN_1.nc', 'a') as nc:
    # with Dataset('/lustre/storeB/project/fou/kl/eva/eemep/runs/etna/eemep_hour_20230918T143002.nc', 'a') as nc:
        SixHourMax(nc)
#    cProfile.run("SixHourMax('/lustre/storeB/project/fou/kl/eva/eemep/runs/Jan_Mayen_ondemand/eemep_hour.nc')")
