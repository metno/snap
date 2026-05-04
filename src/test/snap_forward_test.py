#! /usr/bin/env python3
import os
import pathlib
import re
import shutil
import subprocess
import tempfile
import unittest

import netCDF4
import numpy as np
from snapunittest import SnapTestCase


def run_snap(snap, input_file, cwd):
    env = os.environ.copy()
    env["OMP_NUM_THREADS"] = "1"
    try:
        out = subprocess.run(
            [snap, input_file],
            cwd=cwd,
            env=env,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
        )
    except Exception as e:
        print(e.stdout)
        raise e
    print(out.stdout)


def _resolve_relative_paths(datadir: pathlib.Path, snapinput: str) -> str:
    """resolve known relative paths in snapinput to absolute paths based on datadir"""

    def replacement(match: re.Match) -> str:
        prefix = match.group(1)
        filename = match.group(2)
        resolved_path = (datadir / filename).resolve()
        return f"{prefix}{resolved_path}"

    for field in [
        "FIMEX.CONFIG",
        "FIELD.INPUT",
        "DRY.DEPOSITION.LARGEST_LANDFRACTION_FILE",
    ]:
        # replace path to datafields ommitting = surrounded by optional spaces

        snapinput = re.sub(
            rf"({field}\s*=\s*)(\S+)",
            replacement,
            snapinput,
        )
    return snapinput


class SnapEcEMEPForwardTestCase(SnapTestCase):
    input: str = "snap.input_ecemep_fimex"
    snapExpected: str = "snap_testdata/snap_ecemep_expected5.nc"
    check_output: bool = True

    datadir: pathlib.Path = (pathlib.Path(__file__).parent / "data").resolve()
    snap: pathlib.Path = (datadir / "../bsnap_naccident").resolve()
    variables: list[str] = [
        "instant_height_boundary_layer",
        "precipitation_amount_acc",
        # "Cs137_concentration", # too variable between RNGs
        "Cs137_concentration_bl",
        "Cs137_acc_concentration",
        "Cs137_acc_dry_deposition",
        "Cs137_acc_wet_deposition",
    ]

    @unittest.skipIf(os.getenv("FIMEXLIB") is None, "fimex not supported in this build")
    def test_runfimex(self):
        run_snap(str(self.snap.resolve()), self.input, str(self.datadir.resolve()))

        if self.check_output:
            outfile = self.get_nc_filename(os.path.join(self.datadir, self.input))
            self.compare_nc(
                self.snapExpected,
                os.path.join(self.datadir, outfile),
                self.variables,
            )

    @unittest.skip("test not implemented properly yet")
    def test_runnclib(self):
        # TBD create input-file with FILE.TYPE=netcdf instead of fimex
        pass


class SnapEcEMEPEmersonForwardTestCase(SnapEcEMEPForwardTestCase):
    input: str = "snap.input_ecemep_emerson_fimex"
    snapExpected: str = "snap_testdata/snap_ecemep_emerson_expected_20260304.nc"

    def test_runfimex_async(self):
        d = tempfile.mkdtemp()
        tmp = pathlib.Path(d)

        with (self.datadir / self.input).open("r") as f:
            snapinput = f.read()
        snapinput += "\nASYNC_IO.ON\n"

        snapinput = _resolve_relative_paths(self.datadir, snapinput)

        with (tmp / "snap.input").open("w") as f:
            f.write(snapinput)

        run_snap(str(self.snap), "snap.input", cwd=str(tmp.resolve()))

        outfile = self.get_nc_filename(str(tmp / "snap.input"))
        self.compare_nc(
            self.snapExpected,
            (tmp / outfile).resolve().as_posix(),
            self.variables,
        )


class SnapEcGlobalForwardTestCase(SnapEcEMEPForwardTestCase):
    input: str = "snap.input_ecglobal_emerson_fimex"
    snapExpected: str = "snap_testdata/snap_meps_interpolated_expected_20251125.nc"
    check_output: bool = False


class SnapMEPSForwardTestCase(SnapEcEMEPForwardTestCase):
    input: str = "snap.input_meps_fimex"
    snapExpected: str = "snap_testdata/snap_meps_interpolated_expected_20251125.nc"
    variables: list[str] = [
        "instant_height_boundary_layer",
        "precipitation_amount_acc",
        "Cs137_concentration",
        "Cs137_concentration_bl",
        "Cs137_acc_concentration",
        "Cs137_dry_deposition",
        "Cs137_wet_deposition",
    ]


class ReleaseTests(unittest.TestCase):
    datadir = pathlib.Path(os.path.dirname(os.path.realpath(__file__))).joinpath("data")
    snap = datadir.joinpath("../bsnap_naccident")
    testdata = datadir.joinpath("../snap_testdata")
    input = datadir.joinpath("snap.input_releasetests")

    def test_start_half_hour(self):
        d = tempfile.mkdtemp()
        tmp = pathlib.Path(d)

        with self.input.open("r") as f:
            snapinput = f.read()

        snapinput = snapinput.replace(
            "../snap_testdata", self.testdata.resolve().as_posix()
        )
        snapinput = snapinput.replace("RELEASE.HOUR= XX", "RELEASE.HOUR= 0.5, 1.0")

        with tmp.joinpath("snap.input").open("w") as f:
            f.write(snapinput)

        run_snap(
            self.snap.resolve().as_posix(),
            "snap.input",
            cwd=tmp.resolve().as_posix(),
        )

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
                    self.assertEqual(numtotal, 0)
                elif istep <= 60:
                    self.assertEqual(numtotal, (istep - 30 + 1) * 100)
                else:
                    self.assertEqual(numtotal, 30 * 100)

        shutil.rmtree(tmp.as_posix())

    def test_end_non_integer_hour(self):
        d = tempfile.mkdtemp()
        tmp = pathlib.Path(d)

        with self.input.open("r") as f:
            snapinput = f.read()

        snapinput = snapinput.replace(
            "../snap_testdata", self.testdata.resolve().as_posix()
        )
        snapinput = snapinput.replace("RELEASE.HOUR= XX", "RELEASE.HOUR= 0.0, 1.45")

        with tmp.joinpath("snap.input").open("w") as f:
            f.write(snapinput)

        run_snap(
            self.snap.resolve().as_posix(), "snap.input", cwd=tmp.resolve().as_posix()
        )

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

                self.assertTrue(numtotal <= 8700)  # 100 * 1.45 * (3600 / dt)

        shutil.rmtree(tmp.as_posix())

    def test_gap_non_integer_hour(self):
        d = tempfile.mkdtemp()
        tmp = pathlib.Path(d)

        with self.input.open("r") as f:
            snapinput = f.read()

        snapinput = snapinput.replace(
            "../snap_testdata", self.testdata.resolve().as_posix()
        )
        snapinput = snapinput.replace(
            "RELEASE.HOUR= XX", "RELEASE.HOUR= 0.0, 0.5, 1.25, 3.0"
        )
        snapinput = snapinput.replace(
            "RELEASE.RADIUS.M= 1.0, 1.0", "RELEASE.RADIUS.M= 1.0, 1.0, 1.0, 1.0"
        )
        snapinput = snapinput.replace(
            "RELEASE.LOWER.M= 1.0, 1.0", "RELEASE.LOWER.M= 1.0, 1.0, 1.0, 1.0"
        )
        snapinput = snapinput.replace(
            "RELEASE.UPPER.M= 1.0, 1.0", "RELEASE.UPPER.M= 1.0, 1.0, 1.0, 1.0"
        )
        snapinput = snapinput.replace(
            "RELEASE.BQ/SEC.COMP= 1.0, 1.0", "RELEASE.BQ/SEC.COMP= 1.0, 0.0, 1.0, 0.0"
        )

        with tmp.joinpath("snap.input").open("w") as f:
            f.write(snapinput)

        run_snap(
            self.snap.resolve().as_posix(), "snap.input", cwd=tmp.resolve().as_posix()
        )

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
                    self.assertEqual(numtotal, (istep + 1) * 100)
                elif istep < 75:
                    self.assertEqual(numtotal, 30 * 100)
                elif istep <= 180:
                    self.assertEqual(numtotal, 30 * 100 + (istep - 75 + 1) * 100)
                else:
                    # 13500 => 2.25 hours * 60 releases/hour * 100 parts/release
                    self.assertEqual(numtotal, 13500)

        shutil.rmtree(tmp.as_posix())


class ReleaseDepositionTests(unittest.TestCase):
    datadir = pathlib.Path(os.path.dirname(os.path.realpath(__file__))).joinpath("data")
    snap = datadir.joinpath("../bsnap_naccident")
    testdata = datadir.joinpath("../snap_testdata")

    @unittest.skipIf(os.getenv("FIMEXLIB") is None, "fimex not supported in this build")
    def test_deposition_ecemerson(self):
        d = tempfile.mkdtemp()
        tmp = pathlib.Path(d)

        input_snap = self.datadir.joinpath("snap.input_ecemep_fimex_pos")
        input_ncml = self.datadir.joinpath("ecemep_fixunit.ncml")

        shutil.copy(input_ncml, tmp.joinpath("ecemep_fixunit.ncml"))

        for factor in [1, 3, 5, 10]:
            with input_snap.open("r") as f:
                snapinput = f.read()
            snapinput = snapinput.replace(
                "../snap_testdata", self.testdata.resolve().as_posix()
            )
            snapinput = snapinput + "\n" + f"FIELD.OUTPUT_RESOLUTION_FACTOR={factor}"
            with tmp.joinpath("snap.input").open("w") as f:
                f.write(snapinput)

            run_snap(
                self.snap.resolve().as_posix(),
                "snap.input",
                cwd=tmp.resolve().as_posix(),
            )

            outfile = tmp / "snap.nc"
            with netCDF4.Dataset(str(outfile), "r") as nc:
                self.assertIn("Cs137_acc_dry_deposition", nc.variables)
                ddep = nc.variables["Cs137_acc_dry_deposition"][-1, :, :]
                self.assertTrue((ddep >= 0).all())
                # deposition at 1, 1 (starting from 0)
                pos = factor * 1 + factor // 2
                self.assertGreater(ddep[pos, pos], 0)
                if factor % 2 == 0:
                    # for even factors, the center of the cell is between two grid points, so deposition is split between them
                    self.assertAlmostEqual(
                        np.sum(ddep[pos - 1 : pos + 1, pos - 1 : pos + 1]),
                        np.sum(ddep),
                        places=1,
                    )
                else:
                    self.assertAlmostEqual(ddep[pos, pos], np.sum(ddep), places=1)

        shutil.rmtree(tmp.as_posix())
        pass

    @unittest.skipIf(os.getenv("FIMEXLIB") is None, "fimex not supported in this build")
    def test_deposition_meps(self):
        d = tempfile.mkdtemp()
        tmp = pathlib.Path(d)

        input_snap = self.datadir.joinpath("snap.input_meps_fimex_pos")

        for factor in [5, 10]:
            with input_snap.open("r") as f:
                snapinput = f.read()
            snapinput = snapinput.replace(
                "../snap_testdata", self.testdata.resolve().as_posix()
            )
            snapinput = snapinput + "\n" + f"FIELD.OUTPUT_RESOLUTION_FACTOR={factor}"
            with tmp.joinpath("snap.input").open("w") as f:
                f.write(snapinput)

            run_snap(
                self.snap.resolve().as_posix(),
                "snap.input",
                cwd=tmp.resolve().as_posix(),
            )

            outfile = tmp / "snap.nc"
            with netCDF4.Dataset(str(outfile), "r") as nc:
                self.assertIn("Cs137_acc_dry_deposition", nc.variables)
                ddep = nc.variables["Cs137_acc_dry_deposition"][-1, :, :]
                self.assertTrue((ddep >= 0).all())
                # deposition at 1, 1 (starting from 0)
                pos = factor * 1 + factor // 2
                self.assertGreater(ddep[pos, pos], 0)
                if factor % 2 == 0:
                    # for even factors, the center of the cell is between two grid points, so deposition is split between them
                    self.assertAlmostEqual(
                        np.sum(ddep[pos - 1 : pos + 1, pos - 1 : pos + 1]),
                        np.sum(ddep),
                        places=1,
                    )
                else:
                    self.assertAlmostEqual(ddep[pos, pos], np.sum(ddep), places=1)

        shutil.rmtree(tmp.as_posix())
        pass


if __name__ == "__main__":
    unittest.main(verbosity=2)
