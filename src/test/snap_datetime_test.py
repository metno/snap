"""Integration tests for timestep-aligned second-resolution start times."""

import datetime
import os
import pathlib
import re
import subprocess
import tempfile
import unittest

import netCDF4
import numpy as np
from snap_forward_test import _resolve_relative_paths


class SnapDatetimeTestCase(unittest.TestCase):
    datadir = pathlib.Path(__file__).resolve().parent / "data"
    snap = datadir.parent / "bsnap_naccident"

    def run_case(
        self,
        start: str,
        *,
        backward: bool = False,
        asynchronous: bool = False,
        synoptic: bool = False,
        timestep: float = 900,
        hours: int = 2,
        output_interval: str = "1",
        valid: bool = True,
    ):
        text = (self.datadir / "snap.input_ecemep_fimex").read_text()
        text = _resolve_relative_paths(self.datadir, text)
        replacements = {
            "TIME.START": start,
            "TIME.RUN": f"{-hours if backward else hours}h",
            "TIME.STEP": str(timestep),
            "STEP.HOUR.OUTPUT.FIELDS": output_interval,
            "MAX.PARTICLES.PER.RELEASE": "10",
            "FIELD.OUTPUT": "snap.nc",
            "LOG.FILE": "snap.log",
        }
        for key, value in replacements.items():
            text, count = re.subn(
                rf"(?im)^{re.escape(key)}\s*=.*$", f"{key}={value}", text
            )
            self.assertEqual(count, 1, key)
        text += "\nLOG.SOURCETERM.FILE=source.csv\n"
        if asynchronous:
            text += "ASYNC_IO.ON\n"
        if synoptic:
            text += "SYNOPTIC.OUTPUT\n"

        with tempfile.TemporaryDirectory() as directory:
            tmp = pathlib.Path(directory)
            (tmp / "snap.input").write_text(text)
            env = dict(os.environ, OMP_NUM_THREADS="2")
            result = subprocess.run(
                [str(self.snap), "snap.input"],
                cwd=tmp,
                env=env,
                capture_output=True,
                text=True,
                timeout=120,
            )
            output = result.stdout + result.stderr
            if not valid:
                self.assertNotEqual(result.returncode, 0, output)
                return output
            self.assertEqual(result.returncode, 0, output)
            with netCDF4.Dataset(tmp / "snap.nc") as dataset:
                time = dataset["time"]
                dates = netCDF4.num2date(time[:], time.units)
                dates = [date.strftime("%Y-%m-%dT%H:%M:%S") for date in dates]
                offsets = np.asarray(time[:])
                units = time.units
                precipitation_units = dataset["lwe_precipitation_rate"].units
            log = (tmp / "snap.log").read_text()
            header = (tmp / "source.csv").read_text().splitlines()[0]
            return dates, offsets, units, log, header, precipitation_units

    def test_legacy_and_optional_components(self):
        for start in (
            "2020 04 16 00",
            "2020,04,16,00,0",
            "2020,04,16,00,0,0",
            "2020,04,16,00,30,15\nTIME.START=2020 04 16 00",
        ):
            with self.subTest(start=start):
                dates, offsets, units, log, header, _ = self.run_case(start)
                self.assertEqual(dates, ["2020-04-16T01:00:00", "2020-04-16T02:00:00"])
                np.testing.assert_array_equal(offsets, [3600, 7200])
                self.assertIn("00:00:00", units)
                self.assertIn("2020-04-16T00:00:00", log)
                self.assertIn("2020-04-16T00:00:00Z", header)

    def test_aligned_start_modes(self):
        for backward in (False, True):
            for asynchronous in (False, True):
                for synoptic in (False, True):
                    with self.subTest(
                        backward=backward,
                        asynchronous=asynchronous,
                        synoptic=synoptic,
                    ):
                        hour = 4 if backward else 0
                        hours = 2 if backward else 4
                        dates, offsets, units, log, header, _ = self.run_case(
                            f"2020,04,16,{hour},30",
                            backward=backward,
                            asynchronous=asynchronous,
                            synoptic=synoptic,
                            hours=hours,
                        )
                        origin = datetime.datetime(2020, 4, 16, hour, 30)
                        expected_offsets_seconds = [
                            3600 * (value - 0.5) if synoptic else 3600 * value
                            for value in range(1, hours + 1)
                        ]
                        if backward:
                            expected_offsets_seconds = [
                                -value for value in expected_offsets_seconds
                            ]
                        expected_dates = [
                            (origin + datetime.timedelta(seconds=value)).isoformat()
                            for value in expected_offsets_seconds
                        ]
                        self.assertEqual(dates, expected_dates)
                        np.testing.assert_array_equal(offsets, expected_offsets_seconds)
                        self.assertIn(f"{hour:02d}:30:00", units)
                        self.assertIn(origin.isoformat(), log)
                        self.assertIn(origin.isoformat() + "Z", header)

    def test_seconds_and_fractional_effective_timestep(self):
        for start, timestep, origin in (
            ("2020,04,16,00,30,15", 15, "2020-04-16T00:30:15"),
            ("2020,04,16,00,30", 3600 / 14, "2020-04-16T00:30:00"),
        ):
            with self.subTest(start=start, timestep=timestep):
                dates, offsets, units, log, header, _ = self.run_case(
                    start, timestep=timestep
                )
                initial = datetime.datetime.fromisoformat(origin)
                self.assertEqual(
                    dates,
                    [
                        (initial + datetime.timedelta(seconds=value)).isoformat()
                        for value in (3600, 7200)
                    ],
                )
                np.testing.assert_array_equal(offsets, [3600, 7200])
                self.assertIn(origin[11:], units)
                self.assertIn(origin, log)
                self.assertIn(origin + "Z", header)

    def test_fractional_output_interval_aligns_to_nearest_timestep(self):
        for interval, step_count in (("0.3", 1), ("0.4", 2)):
            with self.subTest(interval=interval):
                dates, offsets, _, _, _, precipitation_units = self.run_case(
                    "2020,04,16,00",
                    timestep=900,
                    hours=2,
                    output_interval=interval,
                )
                output_seconds = [
                    3600 * step * 0.25 for step in range(step_count, 8 + 1, step_count)
                ]
                expected_dates = [
                    (
                        datetime.datetime(2020, 4, 16)
                        + datetime.timedelta(seconds=value)
                    ).isoformat()
                    for value in output_seconds
                ]
                self.assertEqual(dates, expected_dates)
                np.testing.assert_allclose(offsets, output_seconds)
                self.assertEqual(
                    precipitation_units,
                    f"mm/({step_count * 0.25:g}hr)",
                )

    def test_fractional_synoptic_output_remains_on_meteorology_hours(self):
        dates, offsets, _, _, _, precipitation_units = self.run_case(
            "2020,04,16,00,30",
            timestep=900,
            hours=2,
            output_interval="0.5",
            synoptic=True,
        )
        expected_offsets_secs = [3600 * x for x in [0.5, 1.5]]
        expected_dates = [
            (
                datetime.datetime(2020, 4, 16, 0, 30)
                + datetime.timedelta(seconds=value)
            ).isoformat()
            for value in expected_offsets_secs
        ]
        self.assertEqual(dates, expected_dates)
        np.testing.assert_allclose(offsets, expected_offsets_secs)
        self.assertEqual(precipitation_units, "mm/(0.5hr)")

    def test_output_interval_must_align_to_at_least_one_timestep(self):
        output = self.run_case(
            "2020,04,16,00",
            timestep=900,
            output_interval="0.01",
            valid=False,
        )
        self.assertIn("must align to at least one model timestep", output)

    def test_rejected_start_times(self):
        for start in (
            "2020,04,16,00,30,15",
            "2020,04,16,00,61",
            "2020,04,16,00,30,60",
            "2020,04,16",
            "2020,04,16,00,bad",
            "2020,04,16,00,0,0,7",
            "2020,04,16,00,,15",
            "2020,04,16,00,",
        ):
            with self.subTest(start=start):
                output = self.run_case(start, valid=False)
                if start == "2020,04,16,00,30,15":
                    self.assertIn("TIME.START must align", output)
                else:
                    self.assertIn("SNAP ERROR EXIT", output)


if __name__ == "__main__":
    unittest.main()
