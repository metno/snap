import datetime
import os
import sys
import shutil
from pathlib import Path

import math
import netCDF4
import numpy as np

from Snappy.EEMEP.SixHourMax import SixHourMax
from Snappy.Isotopes import Isotopes
from Snappy.AddToa import add_toa_to_nc


class StderrLogger():
    def _write_log(self, msg):
        print(msg, file=sys.stderr)

def _get_isotope_setup(isotope, type):
    '''
        retrieve an object describing the isotope of the type=WDEP,DDEP,CONC giving
        need_decay # True: decay needed between accumulation steps
        acc_only   # True: only accumulated file in output
        decayrate  # decay-rate factor
        eemep_name # name in eemep
        snap_name  # name in snap without accumulation
        snap_acc_name  # name in snap with accumulation
        units # units without accumulation
        units_acc # units with accumulation
    '''
    retval = {'need_decay': False,
                'acc_only': True,
                'decayrate': 0,
                'eemep_name': None,
                'snap_name': None,
                'units_acc': None,
                'units': None}
    if type == 'CONC':
        retval['eemep_name'] = f'SURF_uBq_NPP_{isotope}'
        retval['snap_acc_name'] = f'{isotope}_acc_concentration'
        retval['acc_only'] = True
        retval['decay_needed'] = False
        retval['units_acc'] = 'uBq*hr/m3'
    elif type == 'WDEP':
        retval['eemep_name'] = f'WDEP_NPP_{isotope}'
        retval['snap_acc_name'] = f'{isotope}_acc_wet_deposition'
        retval['snap_name'] = f'{isotope}_wet_deposition'
        retval['acc_only'] = False
        retval['decay_needed'] = True
        retval['decayrate'] = 1 # TBD
        retval['units'] = 'mBq/m2'
        retval['units_acc'] = 'mBq/m2' # no *hr, since this is what is on ground
    elif type == 'DDEP':
        retval['eemep_name'] = f'DDEP_NPP_{isotope}_m2Grid'
        retval['snap_acc_name'] = f'{isotope}_acc_dry_deposition'
        retval['snap_name'] = f'{isotope}_dry_deposition'
        retval['acc_only'] = False
        retval['decay_needed'] = True
        retval['decayrate'] = 1 # TBD
        retval['units'] = 'mBq/m2'
        retval['units_acc'] = 'mBq/m2'
    else:
        raise Exception(f'wrong type: {type}')
    return retval


class PostProcess():
    ''' Run the postprocessing, usage:
pp = PostProcess('.', datetime.now())
pp.convert_files('eemep_hourInst.nc', 'eemep_hour.nc')
'''

    def __init__(self, path, timestamp, logger=None):
        '''Initialize the Postprocess, but run nothing.

        logger is a class with a _write_log(msg) method.
        '''
        self.timestamp = timestamp
        self.path = path

        if logger:
            self.logger = logger
        else:
            self.logger = StderrLogger()

    def convert_files(self, instantFilename, averageFilename):
        '''Run the postprocessing on the instant- and averageFilename files'''
        # rename files, make them available to further processes
        timestamp = self.timestamp.strftime("%Y%m%dT%H%M%S")
        simulationstart = self.timestamp.strftime("%Y-%m-%d_%H:%M:%S")

        self.logger._write_log("postprocessing {}".format(instantFilename))
        with netCDF4.Dataset(os.path.join(self.path, instantFilename), 'a') as nc:
            nc.setncattr('SIMULATION_START_DATE', simulationstart)

        self.logger._write_log("postprocessing (adding 6h_vmax) {}".format(averageFilename))
        with netCDF4.Dataset(os.path.join(self.path, averageFilename), 'a') as nc:
            nc.setncattr('SIMULATION_START_DATE', simulationstart)
            nc['time'][:] += (0.5 / 24.) # add half an hour as 'days since'
            SixHourMax(nc)

        newInstFile = os.path.join(self.path, 'eemep_hourInst_{}.nc'.format(timestamp))
        newAvgFile = os.path.join(self.path, 'eemep_hour_{}.nc'.format(timestamp))
        self.logger._write_log("making files available as {} and {}".format(newInstFile, newAvgFile))
        os.rename(instantFilename, newInstFile)
        os.rename(averageFilename, newAvgFile)




    def accumulate_and_toa_nuc_files(self, instantFilename, averageFilename) -> None:
        self.logger._write_log(f"Accumulating nuclear outputs found in {self.path}")
        # rename files, make them available to further processes
        timestamp = self.timestamp.strftime("%Y%m%dT%H%M%S")
        simulationstart = self.timestamp.strftime("%Y-%m-%d_%H:%M:%S")

        dir = Path(self.path)

        new_filename = dir / 'eemep_nuc_{}.nc'.format(timestamp)
        nc_path = dir / instantFilename
        shutil.copy(nc_path, new_filename)

        isotopes = set()
        with netCDF4.Dataset(new_filename, 'a') as nc:
            nc.setncattr('SIMULATION_START_DATE', simulationstart)
            for varname, var in nc.variables.items():
                if varname.startswith('SURF_uBq_NPP_'):
                    isotopes.add(var.name[13:])
            # rename variables in copied instantFile
            for isotope in isotopes:
                varname = f'SURF_uBq_NPP_{isotope}'
                newvar = f'{isotope}_concentration'
                if varname in nc.variables:
                    self.logger._write_log(f"renaming {varname} to {newvar}")
                    # nc.renameVariabe(varname, newvar) # bug in renameVariable -> just create a copy for now
                    var = nc[varname]
                    nvar = nc.createVariable(newvar, var.datatype, dimensions=var.dimensions,
                                            zlib=True, complevel=1)
                    nvar.setncatts(var.__dict__)
                    for t in range(0, nc['time'].shape[0]):
                        nvar[t,:] = var[t,:]
                
                    
            # copy data from averageFile
            with netCDF4.Dataset(dir / averageFilename, 'r') as ncAvg:
                for isotope in isotopes:
                    for type in ('CONC', 'WDEP', 'DDEP'):
                        isosetup = _get_isotope_setup(isotope, type)
                        self.logger._write_log(f"fix output for {isotope}, {type}, {isosetup}")
                        varname = isosetup['eemep_name']
                        newaccvar = isosetup['snap_acc_name']
                        newvar = isosetup['snap_name']
                        if varname in ncAvg.variables:
                            var = ncAvg[varname]
                            naccvar = nc.createVariable(newaccvar, var.datatype, dimensions=var.dimensions,
                                                    zlib=True, complevel=1)
                            # copy variable attributes all at once via dictionary
                            naccvar.setncatts(var.__dict__)
                            naccvar.units = isosetup['units_acc']
                            if not isosetup['acc_only']:
                                nvar = nc.createVariable(newvar, var.datatype, dimensions=var.dimensions,
                                                        zlib=True, complevel=1)
                                # copy variable attributes all at once via dictionary
                                nvar.setncatts(var.__dict__)
                                nvar.units = isosetup['units']

                            times = ncAvg['time'][:]
                            dates = netCDF4.num2date(times, nc['time'].units)
                            data = var[0,:]
                            naccvar[0,:] = data
                            if not isosetup['acc_only']:
                                nvar[0,:] = data
                            for t in range(1, nc['time'].shape[0]):
                                data = var[t,:]
                                if not isosetup['acc_only']:
                                    nvar[t,:] = data
                                if isosetup['decay_needed']:
                                    decayrate = Isotopes().byName(isotope)['decay']
                                    secs = (dates[t] - dates[t-1]).total_seconds()
                                    decayfactor = math.exp(-1*decayrate*secs)
                                else:
                                    decayfactor = 1
                                naccvar[t,:] = naccvar[t-1,:]*decayfactor + data
                            # sync after each variable
                            nc.sync()
            # file is now very like snap-output, so possible to add toa
            self.logger._write_log('adding toa and totals to output')
            add_toa_to_nc(nc)

if __name__ == '__main__':
    pp = PostProcess('.', datetime.datetime.now())
    #pp.convert_files('eemep_hourInst.nc', 'eemep_hour.nc')
    pp.accumulate_and_toa_nuc_files('eemep_hourInst.nc', 'eemep_hour.nc')
