#! /usr/bin/env python3
import os
import subprocess
from snapunittest import SnapTestCase
import unittest


class SnapEcEMEPForwardTestCase(SnapTestCase):
    input = 'snap.input_ecemep_fimex'
    snap = '../bsnap_naccident'
    datadir = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'data')

    snapExpected = 'https://thredds.met.no/thredds/dodsC/metusers/heikok/snap_testdata/snap_ecemep_expected.nc'

    def setUp(self):
        pass

    def tearDown(self):
        pass

    @unittest.skipIf(os.getenv('FIMEXLIB') is None,
                     "fimex not supported in this build")
    def test_runfimex(self):
        env = os.environ.copy()
        env["OMP_NUM_THREADS"] = "1"
        proc = subprocess.run([os.path.join(self.datadir, self.snap), self.input], cwd=self.datadir, env=env)
        self.assertEqual(proc.returncode, 0)

        outfile = self.get_nc_filename(os.path.join(self.datadir, self.input))
        self.compare_nc(self.snapExpected,
                        os.path.join(self.datadir, outfile),
                        ['instant_height_boundary_layer',
                         'precipitation_amount_acc',
                         'Cs137_concentration',
                         'Cs137_concentration_bl',
                         'Cs137_acc_concentration',
                         'Cs137_acc_dry_deposition',
                         'Cs137_acc_wet_deposition'])

    @unittest.skip("test not implemented properly yet")
    def test_runnclib(self):
        #TBD create input-file with FILE.TYPE=netcdf instead of fimex
        env = os.environ.copy()
        env["OMP_NUM_THREADS"] = "1"
        proc = subprocess.run([os.path.join(self.datadir, self.snap), self.input], cwd=self.datadir, env=env)
        self.assertEqual(proc.returncode, 0)

        outfile = self.get_nc_filename(os.path.join(self.datadir, self.input))
        self.compare_nc(self.snapExpected,
                        os.path.join(self.datadir, outfile),
                        ['instant_height_boundary_layer',
                         'precipitation_amount_acc',
                         'Cs137_concentration',
                         'Cs137_concentration_bl',
                         'Cs137_acc_concentration',
                         'Cs137_acc_dry_deposition',
                         'Cs137_acc_wet_deposition'])


class SnapMEPSForwardTestCase(SnapTestCase):
    input = 'snap.input_meps_fimex'
    snap = '../bsnap_naccident'
    datadir = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'data')

    snapExpected = 'https://thredds.met.no/thredds/dodsC/metusers/heikok/snap_testdata/snap_meps_expected.nc'

    def setUp(self):
        pass

    def tearDown(self):
        pass

    @unittest.skipIf(os.getenv('FIMEXLIB') is None,
                     "fimex not supported in this build")
    def test_runfimex(self):
        env = os.environ.copy()
        env["OMP_NUM_THREADS"] = "1"
        proc = subprocess.run([os.path.join(self.datadir, self.snap), self.input], cwd=self.datadir, env=env)
        self.assertEqual(proc.returncode, 0)

        outfile = self.get_nc_filename(os.path.join(self.datadir, self.input))
        self.compare_nc(self.snapExpected,
                        os.path.join(self.datadir, outfile),
                        ['instant_height_boundary_layer',
                         'precipitation_amount_acc',
                         'Cs137_concentration',
                         'Cs137_concentration_bl',
                         'Cs137_acc_concentration',
                         'Cs137_dry_deposition',
                         'Cs137_wet_deposition'])

    @unittest.skip("test not implemented properly yet")
    def test_runnclib(self):
        #TBD create input-file with FILE.TYPE=netcdf instead of fimex
        env = os.environ.copy()
        env["OMP_NUM_THREADS"] = "1"
        proc = subprocess.run([os.path.join(self.datadir, self.snap), self.input], cwd=self.datadir, env=env)
        self.assertEqual(proc.returncode, 0)

        outfile = self.get_nc_filename(os.path.join(self.datadir, self.input))
        self.compare_nc(self.snapExpected,
                        os.path.join(self.datadir, outfile),
                        ['instant_height_boundary_layer',
                         'precipitation_amount_acc',
                         'Cs137_concentration',
                         'Cs137_concentration_bl',
                         'Cs137_acc_concentration',
                         'Cs137_dry_deposition',
                         'Cs137_wet_deposition'])


if __name__ == '__main__':
    unittest.main()
