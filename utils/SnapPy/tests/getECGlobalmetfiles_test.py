# import sys
import os
import pathlib
from datetime import date, datetime, time, timedelta

import pytest
from Snappy.Resources import Resources, MetModel
from Snappy.ResourcesCommon import LustreDir

""""
Created on Jan 09, 2025

@author: geche8548

Test data date range: 30-12-2025 to 05-01-2026
Except for test 7, "Today" refers to final day in date range (final date with 0 offset forecast)
For test 7, "Today" refers to the day this test is run (Actually today)
Tests included:

Realtime:
1. Forecast 48 hours
2. Run from yesterday 48 hours
3. Backwards run -72 hours
4. missing 00 file start today
5. Missing todays data start today
6. start at 00 today

Specific period:
7. Hindcast 48 hours
8. Long run 96 hours over date periodicities
9. Missing specific files:
    a) missing 00 file
    b) missing whole day
    c) missing two days of data
    d) missing end days
    e) missing all data
    f) missing 00, 18 files, 00 start
10. Starting at 00:00
    a) Starting at 00:00, full data
    b) Missing 00 and 18 files
"""

today = datetime.combine(date.today(), time(5, 0, 0))
yesterday = today - timedelta(days=1)


def setup_environment(path: pathlib.Path):
    # set env variables to point to non-existing directories, so that we are sure that the test data is used
    for dir_type in LustreDir:
        os.environ[dir_type.name] = str(path)


@pytest.fixture
def tmp_path_with_meteo_files(tmp_path: pathlib.Path) -> pathlib.Path:
    """Create temporary directory with meteo-files, extending pytests build-in `tmp_path`.

    :param tmp_path: pytest build-in fixture tmp_pat
    :return: tmp_path, filled with meteo-files
    """
    d = tmp_path
    d.mkdir(parents=True,exist_ok=True)

    for day in range(1, 6):
        for utc in ["00", "06", "12", "18"]:
            p = d / f"ec_atmo_0_1deg_202601{day:02d}T{utc}0000Z_3h.nc"
            p.touch()

    for day in range(30, 32):
        for utc in ["00", "06", "12", "18"]:
            p = d / f"ec_atmo_0_1deg_202512{day:02d}T{utc}0000Z_3h.nc"
            p.touch()
    setup_environment(tmp_path)
    return tmp_path


@pytest.fixture
def tmp_path_with_realtime_meteo_files(tmp_path: pathlib.Path) -> pathlib.Path:
    """Create temporary directory with meteo-files, extending pytests build-in `tmp_path`.

    :param tmp_path: pytest build-in fixture tmp_pat
    :return: tmp_path, filled with meteo-files
    """
    d = tmp_path
    d.mkdir(parents=True,exist_ok=True)

    for n_days in range(5):
        dat = today + timedelta(days=-n_days)
        for utc in ["00", "06", "12", "18"]:
            p = d /f"ec_atmo_0_1deg_{dat.year:04d}{dat.month:02d}{dat.day:02d}T{utc}0000Z_3h.nc"
            p.touch()
    setup_environment(tmp_path)
    return tmp_path


class TestClass:
    def test_forecast48(self, tmp_path_with_realtime_meteo_files):
        # Test 1: Future forecast 48 hours

        # str-handling required in Resources
        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
            MetModel.EC0p1Global: [tmpdir]
        }
        start = today
        duration = 48

        expected = [
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T180000Z_3h.nc",
        ]

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global",start, duration) == expected
        )

    def test_start_yesterday(self, tmp_path_with_realtime_meteo_files):
        # Test 2: Starting yesterday. Tests both cases (past and future) in code.

        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
            MetModel.EC0p1Global: [tmpdir]
        }
        start = yesterday
        duration = 48

        expected = [
            f"{tmpdir}/ec_atmo_0_1deg_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T180000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T180000Z_3h.nc",
        ]

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global", start, duration) == expected
        )

    def test_backwards_forecast72(self, tmp_path_with_realtime_meteo_files):
        # Part 3: Backwards forecast across future and past data.

        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
            MetModel.EC0p1Global: [tmpdir]
        }
        start = today + timedelta(days=2)  # starting day after tomorrow
        duration = -72

        expected = [
            f"{tmpdir}/ec_atmo_0_1deg_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T180000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T180000Z_3h.nc",
        ]

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global", start, duration) == expected
        )

    def test_missing_00_start_today(self, tmp_path_with_realtime_meteo_files):
        # Part 4: starting today with missing 00 files

        for file in (tmp_path_with_realtime_meteo_files).glob("ec_atmo_0_1deg_*T000000Z_3h.nc"):
                        file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
            MetModel.EC0p1Global: [tmpdir]
        }

        start = today
        duration = 48

        expected = [
            f"{tmpdir}/ec_atmo_0_1deg_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T180000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T180000Z_3h.nc",
        ]

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global", start, duration) == expected
        )

    def test_missing_today(self, tmp_path_with_realtime_meteo_files):
        # Test 5: missing all data from today

        for file in (tmp_path_with_realtime_meteo_files).glob(f"ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T*.nc"):
            file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
            MetModel.EC0p1Global: [tmpdir]
        }
        start = today  # starting on missing day
        duration = 48

        expected = [
            f"{tmpdir}/ec_atmo_0_1deg_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T180000Z_3h.nc",
        ]

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global", start, duration) == expected
        )

    def test_today_00_start(self, tmp_path_with_realtime_meteo_files):
        # Test 6: Starting at 00:00
        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
            MetModel.EC0p1Global: [tmpdir],
        }
        start = today - timedelta(hours=5)
        duration = 12

        expected = [
            f"{tmpdir}/ec_atmo_0_1deg_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T180000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_{today.year}{today.month:02d}{today.day:02d}T120000Z_3h.nc",
        ]

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global", start, duration) == expected
        )

    # ################# Specific period of data                        #################
    # ################# Test data date range: 30-12-2025 to 05-01-2026 #################

    def test_hindcast48(self, tmp_path_with_meteo_files):
        # Test 7: Hindcast 48 hours
        tmpdir = str(tmp_path_with_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
        }

        start = datetime.fromisoformat("2026-01-02T13:00:00")
        duration = 48

        expected = [
            f"{tmpdir}/ec_atmo_0_1deg_20260102T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260102T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260102T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260102T180000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T180000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260104T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260104T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260104T120000Z_3h.nc",
        ]

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global", start, duration) == expected
        )

    def test_forecast96(self, tmp_path_with_meteo_files):
        # Test 8: Long forecast 96 hours
        tmpdir = str(tmp_path_with_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
        }

        start = datetime.fromisoformat("2025-12-30T13:00:00")
        duration = 96

        expected = [
            f"{tmpdir}/ec_atmo_0_1deg_20251230T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20251230T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20251230T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20251230T180000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20251231T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20251231T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20251231T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20251231T180000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260101T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260101T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260101T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260101T180000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260102T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260102T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260102T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260102T180000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T120000Z_3h.nc",
        ]

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global", start, duration) == expected
        )

    ####    Test 9    ####
    def test_missing_00_files(self, tmp_path_with_meteo_files):
        # Part a: missing 00 files
        for file in (tmp_path_with_meteo_files).glob("ec_atmo_0_1deg_*T000000Z_3h.nc"):
                file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)

        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
        }

        start = datetime.fromisoformat("2026-01-04T13:00:00")
        duration = 48

        expected = [
            f"{tmpdir}/ec_atmo_0_1deg_20260103T180000Z_3h.nc", 
            f"{tmpdir}/ec_atmo_0_1deg_20260104T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260104T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260104T180000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260105T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260105T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260105T180000Z_3h.nc",
        ]

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global", start, duration) == expected
        )

    def test_missing_whole_day(self, tmp_path_with_meteo_files):
        # Part b: missing whole day starting from day before missing day
        for file in (tmp_path_with_meteo_files).glob(f"ec_atmo_0_1deg_20260102T*.nc"):
            file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)

        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
        }

        start = datetime.fromisoformat("2026-01-01T13:00:00")
        duration = 48


        expected = [
            f"{tmpdir}/ec_atmo_0_1deg_20260101T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260101T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260101T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260101T180000Z_3h.nc", 
            f"{tmpdir}/ec_atmo_0_1deg_20260103T000000Z_3h.nc",                    
            f"{tmpdir}/ec_atmo_0_1deg_20260103T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T120000Z_3h.nc",
        ]

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global", start, duration) == expected
        )

    def test_missing_two_days(self, tmp_path_with_meteo_files):
        # Part c: missing data for two days
        for file in (tmp_path_with_meteo_files).glob(f"ec_atmo_0_1deg_20260102T*.nc"):
            file.unlink(missing_ok=True)
        for file in (tmp_path_with_meteo_files).glob(f"ec_atmo_0_1deg_20260101T*.nc"):
            file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)

        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
        }

        start = datetime.fromisoformat("2026-01-02T13:00:00")  # starting on missing day
        duration = 48

        expected = [
            f"{tmpdir}/ec_atmo_0_1deg_20251231T180000Z_3h.nc", 
            f"{tmpdir}/ec_atmo_0_1deg_20260103T000000Z_3h.nc",                    
            f"{tmpdir}/ec_atmo_0_1deg_20260103T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T180000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260104T000000Z_3h.nc",                    
            f"{tmpdir}/ec_atmo_0_1deg_20260104T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260104T120000Z_3h.nc",
        ]

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global", start, duration) == expected
        )

    def test_missing_end_days(self, tmp_path_with_meteo_files):
        # Part d: missing all data from last days in run
        for file in (tmp_path_with_meteo_files).glob(f"ec_atmo_0_1deg_20260105T*.nc"):
            file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
        }

        start = datetime.fromisoformat("2026-01-05T13:00:00")  # starting on missing day
        duration = 48

        expected = [
            f"{tmpdir}/ec_atmo_0_1deg_20260104T180000Z_3h.nc",
        ]

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global", start, duration) == expected
        )

    def test_missing_all(self, tmp_path_with_meteo_files):
        # Part e: missing all data
        for file in (tmp_path_with_meteo_files).glob(f"*"):
            file.unlink(missing_ok=True)

        tmpdir = str(tmp_path_with_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
        }

        start = datetime.fromisoformat("2026-01-03T13:00:00")
        duration = 48

        expected = []

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global", start, duration) == expected
        )

    
    #### Test 10 ####
    def test_00_start(self, tmp_path_with_meteo_files):
        # Part a: Starting at 00:00
        tmpdir = str(tmp_path_with_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
        }
        start = datetime.fromisoformat("2026-01-03T00:00:00")
        duration = 12

        expected = [
            f"{tmpdir}/ec_atmo_0_1deg_20260102T180000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T000000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T120000Z_3h.nc",
        ]

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global", start, duration) == expected
        )
    
    def test_missing_00_and_18(self, tmp_path_with_meteo_files):
        # Part b: Starting at 00:00, missing 00 and 18 files
        for file in (tmp_path_with_meteo_files).glob("ec_atmo_0_1deg_*T000000Z_3h.nc"):
            file.unlink(missing_ok=True)
        for file in (tmp_path_with_meteo_files).glob("ec_atmo_0_1deg_*T180000Z_3h.nc"):
            file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.EC0p1Global: [tmpdir],
        }
        start = datetime.fromisoformat("2026-01-03T00:00:00")
        duration = 12

        expected = [
            f"{tmpdir}/ec_atmo_0_1deg_20260102T120000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T060000Z_3h.nc",
            f"{tmpdir}/ec_atmo_0_1deg_20260103T120000Z_3h.nc",
        ]

        assert (
            res.getRequiredMeteorologyFiles("ec_0p1_global", start, duration) == expected
        )
