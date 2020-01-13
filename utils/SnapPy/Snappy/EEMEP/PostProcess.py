import datetime
import os
import sys

from netCDF4 import Dataset
from Snappy.EEMEP.SixHourMax import SixHourMax

class StderrLogger():
    def _write_log(self, msg):
        print(msg, file=sys.stderr)

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
        with Dataset(os.path.join(self.path, instantFilename), 'a') as nc:
            nc.setncattr('SIMULATION_START_DATE', simulationstart)
        
        self.logger._write_log("postprocessing (adding 6h_vmax) {}".format(averageFilename))
        with Dataset(os.path.join(self.path, averageFilename), 'a') as nc:
            nc.setncattr('SIMULATION_START_DATE', simulationstart)
            nc['time'][:] += (0.5 / 24.) # add halv an hour as 'days since'
            SixHourMax(nc)

        newInstFile = os.path.join(self.path, 'eemep_hourInst_{}.nc'.format(timestamp)) 
        newAvgFile = os.path.join(self.path, 'eemep_hour_{}.nc'.format(timestamp)) 
        self.logger._write_log("making files available as {} and {}".format(newInstFile, newAvgFile))
        os.rename(instantFilename, newInstFile)
        os.rename(averageFilename, newAvgFile)

if __name__ == '__main__':
    pp = PostProcess('.', datetime.datetime.now())
    pp.convert_files('eemep_hourInst.nc', 'eemep_hour.nc')
