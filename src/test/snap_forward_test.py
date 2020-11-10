#! /usr/bin/env python3
import os
import subprocess
from snapunittest import SnapTestCase
import unittest
import pathlib
import tempfile
import shutil


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


class ReleaseTests(unittest.TestCase):
    datadir = pathlib.Path(os.path.dirname(os.path.realpath(__file__))).joinpath('data')
    snap = datadir.joinpath('../bsnap_naccident')
    input = datadir.joinpath('snap.input_releasetests')

    def test_start_half_hour(self):
        d = tempfile.mkdtemp()
        tmp = pathlib.Path(d)
        print(tmp)

        with self.input.open("r") as f:
            snapinput = f.read()

        snapinput = snapinput.replace("RELEASE.HOUR= XX", "RELEASE.HOUR= 0.5, 1.0")

        with tmp.joinpath("snap.input").open("w") as f:
            f.write(snapinput)

        subprocess.check_call([self.snap.resolve().as_posix(), "snap.input"], cwd=tmp)

        dt = 60
        releases = 0
        istep = -1
        logfile = tmp.joinpath("snap.log")
        with logfile.open("r") as logfile:
            for line in logfile:
                if line.startswith(" istep,nplume"):
                    tline = line.strip()
                    tline = [e for e in tline.split() if len(e) > 0]
                    istep = int(tline[1])
                if not line.startswith(" comp,totalbq,numtotal:"):
                    continue
                line = line.strip()
                elems = [e for e in line.split(" ") if len(e) > 0]
                numtotal = int(elems[3])
                print(istep)
                if istep < 30:
                    self.assertEqual(numtotal, 0)
                elif istep < 60:
                    self.assertEqual(numtotal, (istep - 30) * 100)
                else:
                    self.assertEqual(numtotal, 30 * 100)

        shutil.rmtree(tmp)

    def test_end_non_integer_hour(self):
        d = tempfile.mkdtemp()
        tmp = pathlib.Path(d)
        print(tmp)

        with self.input.open("r") as f:
            snapinput = f.read()

        snapinput = snapinput.replace("RELEASE.HOUR= XX", "RELEASE.HOUR= 0.0, 1.45")

        with tmp.joinpath("snap.input").open("w") as f:
            f.write(snapinput)

        subprocess.check_call([self.snap.resolve().as_posix(), "snap.input"], cwd=tmp)

        dt = 60
        releases = 0
        logfile = tmp.joinpath("snap.log")
        with logfile.open("r") as logfile:
            for line in logfile:
                if not line.startswith(" comp,totalbq,numtotal:"):
                    continue
                releases += 1
                line = line.strip()
                elems = [e for e in line.split(" ") if len(e) > 0]
                numtotal = int(elems[3])
                self.assertEqual(numtotal, releases * 100)
                bqtotal = float(elems[2])
                self.assertAlmostEqual(bqtotal, releases * 60, places=3)

                print(elems)
                assert numtotal <= 8700  # 100 * 1.45 * (3600 / dt)

        shutil.rmtree(tmp)

    def test_gap_non_integer_hour(self):
        d = tempfile.mkdtemp()
        tmp = pathlib.Path(d)
        print(tmp)

        with self.input.open("r") as f:
            snapinput = f.read()

        snapinput = snapinput.replace("RELEASE.HOUR= XX", "RELEASE.HOUR= 0.0, 0.5, 1.25, 3.0")
        snapinput = snapinput.replace("RELEASE.RADIUS.M= 1.0, 1.0", "RELEASE.RADIUS.M= 1.0, 1.0, 1.0, 1.0")
        snapinput = snapinput.replace("RELEASE.LOWER.M= 1.0, 1.0", "RELEASE.LOWER.M= 1.0, 1.0, 1.0, 1.0")
        snapinput = snapinput.replace("RELEASE.UPPER.M= 1.0, 1.0", "RELEASE.UPPER.M= 1.0, 1.0, 1.0, 1.0")
        snapinput = snapinput.replace("RELEASE.BQ/SEC.COMP= 1.0, 1.0", "RELEASE.BQ/SEC.COMP= 1.0, 0.0, 1.0, 0.0")

        with tmp.joinpath("snap.input").open("w") as f:
            f.write(snapinput)

        subprocess.check_call([self.snap.resolve().as_posix(), "snap.input"], cwd=tmp)

        dt = 60
        releases = 0
        istep = -1
        logfile = tmp.joinpath("snap.log")
        with logfile.open("r") as logfile:
            for line in logfile:
                if line.startswith(" istep,nplume"):
                    tline = line.strip()
                    tline = [e for e in tline.split() if len(e) > 0]
                    istep = int(tline[1])
                if not line.startswith(" comp,totalbq,numtotal:"):
                    continue
                line = line.strip()
                elems = [e for e in line.split(" ") if len(e) > 0]
                numtotal = int(elems[3])
                if istep < 30:
                    self.assertEqual(numtotal, istep * 100)
                elif istep < 75:
                    self.assertEqual(numtotal, 30 * 100)
                elif istep < 180:
                    self.assertEqual(numtotal, 30 * 100 + (istep - 75) * 100)
                else:
                    # 13500 => 2.25 hours * 60 releases/hour * 100 parts/release
                    self.assertEqual(numtotal, 13500)

        shutil.rmtree(tmp)


if __name__ == '__main__':
    unittest.main()
