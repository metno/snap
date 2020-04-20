import math
import netCDF4
import numpy
import unittest

class SnapTestCase(unittest.TestCase):
    '''extension of a unittest TestCase with methods useful for SNAP outputs'''

    @staticmethod
    def get_nc_filename(snap_input):
        '''get the filename of the output netcdf file from a snap.input file'''
        with open(snap_input, 'rt') as fh:
            for line in fh:
                if line.startswith('FIELD.OUTPUT='):
                    outfile = line[13:].strip()
                    return outfile
        assert(False)

    def compare_fields(self, ex, new, msg):
        '''compare two numpy fields to be almost equal'''
        self.assertSequenceEqual(ex.shape, new.shape)
        # allow 1% numerical difference
        rel = 4
        exS = numpy.sum(ex)
        neS = numpy.sum(new)
        if exS < 1: rel = 20
        elif exS < 10: rel = 10
        self.assertTrue(math.isclose(exS, neS, rel_tol=rel/100),
                        msg="{}: {} !~ {} within {}%".format(msg,exS,neS,rel))


    def compare_nc(self, expected, outfile, varNames=[]):
        '''compare if the outfile is almost equal the expected file for the varNames variables'''
        with netCDF4.Dataset(expected, 'r') as exNc:
            time = exNc['time'][:]
            for x in varNames:
                self.assertIn(x, exNc.variables)
            with netCDF4.Dataset(outfile, 'r') as nc:
                for x in varNames:
                    self.assertIn(x, nc.variables)
                    for t in range(time.shape[0]):
                        self.compare_fields(exNc[x][t,:], nc[x][t,:], "{} at time({})".format(x,t))

