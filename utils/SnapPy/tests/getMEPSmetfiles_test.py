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
    d = tmp_path / "2026" / "01"
    d.mkdir(parents=True,exist_ok=True)

    for day in range(1, 6):
        dir = d / f"0{day}"
        dir.mkdir(exist_ok=True)
        for utc in ["00", "06", "12", "18"]:
            p = dir / f"meps_det_2_5km_2026010{day}T{utc}Z.nc"
            p.touch()
    
    d = tmp_path / "2025" / "12"
    d.mkdir(parents=True,exist_ok=True)

    for day in range(30, 32):
        dir = d / f"{day}"
        dir.mkdir(exist_ok=True)
        for utc in ["00", "06", "12", "18"]:
            p = dir / f"meps_det_2_5km_202512{day}T{utc}Z.nc"
            p.touch()
    setup_environment(tmp_path)
    return tmp_path


@pytest.fixture
def tmp_path_with_realtime_meteo_files(tmp_path: pathlib.Path) -> pathlib.Path:
    """Create temporary directory with meteo-files, extending pytests build-in `tmp_path`.

    :param tmp_path: pytest build-in fixture tmp_pat
    :return: tmp_path, filled with meteo-files
    """

    for n_days in range(5):
        dat = today + timedelta(days=-n_days)
        d = tmp_path / f"{dat.year}" / f"{dat.month:02d}" / f"{dat.day:02d}"
        d.mkdir(parents=True)
        for utc in ["00", "06", "12", "18"]:
            p = d / f"meps_det_2_5km_{dat.year}{dat.month:02d}{dat.day:02d}T{utc}Z.nc"
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
            MetModel.Meps2p5: [tmpdir],
        }
        start = today
        duration = 48

        expected = [
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T00Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T06Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T12Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T18Z.nc",
        ]

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )

    def test_start_yesterday(self, tmp_path_with_realtime_meteo_files):
        # Test 2: Starting yesterday. Tests both cases (past and future) in code.

        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.Meps2p5: [tmpdir],
        }
        start = yesterday
        duration = 48

        expected = [
            f"{tmpdir}/{yesterday.year}/{yesterday.month:02d}/{yesterday.day:02d}/meps_det_2_5km_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T00Z.nc",
            f"{tmpdir}/{yesterday.year}/{yesterday.month:02d}/{yesterday.day:02d}/meps_det_2_5km_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T06Z.nc",
            f"{tmpdir}/{yesterday.year}/{yesterday.month:02d}/{yesterday.day:02d}/meps_det_2_5km_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T12Z.nc",
            f"{tmpdir}/{yesterday.year}/{yesterday.month:02d}/{yesterday.day:02d}/meps_det_2_5km_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T18Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T00Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T06Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T12Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T18Z.nc",
        ]

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )

    def test_backwards_forecast72(self, tmp_path_with_realtime_meteo_files):
        # Part 3: Backwards forecast across future and past data.

        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.Meps2p5: [tmpdir],
        }
        start = today + timedelta(days=2)  # starting day after tomorrow
        duration = -72

        expected = [
            f"{tmpdir}/{yesterday.year}/{yesterday.month:02d}/{yesterday.day:02d}/meps_det_2_5km_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T00Z.nc",
            f"{tmpdir}/{yesterday.year}/{yesterday.month:02d}/{yesterday.day:02d}/meps_det_2_5km_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T06Z.nc",
            f"{tmpdir}/{yesterday.year}/{yesterday.month:02d}/{yesterday.day:02d}/meps_det_2_5km_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T12Z.nc",
            f"{tmpdir}/{yesterday.year}/{yesterday.month:02d}/{yesterday.day:02d}/meps_det_2_5km_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T18Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T00Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T06Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T12Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T18Z.nc",
        ]

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )

    def test_missing_00_start_today(self, tmp_path_with_realtime_meteo_files):
        # Part 4: starting today with missing 00 files
        for year in (tmp_path_with_realtime_meteo_files).glob("*"):
            for month in (year).glob("*"):
                for day in (month).glob("*"):
                    for file in (day).glob("meps_det_2_5km_*T00Z.nc"):
                        file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.Meps2p5: [tmpdir],
        }

        start = today
        duration = 48

        expected = [
            f"{tmpdir}/{yesterday.year}/{yesterday.month:02d}/{yesterday.day:02d}/meps_det_2_5km_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T18Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T06Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T12Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T18Z.nc",
        ]

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )

    def test_missing_today(self, tmp_path_with_realtime_meteo_files):
        # Test 5: missing all data from today

        for file in (
            tmp_path_with_realtime_meteo_files
            / f"{today.year}/{today.month:02d}/{today.day:02d}"
        ).glob("meps_det_2_5km_*T*Z.nc"):
            file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.Meps2p5: [tmpdir],
        }
        start = today  # starting on missing day
        duration = 48

        expected = [
            f"{tmpdir}/{yesterday.year}/{yesterday.month:02d}/{yesterday.day:02d}/meps_det_2_5km_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T18Z.nc",
        ]

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )

    def test_today_00_start(self, tmp_path_with_realtime_meteo_files):
        # Test 6: Starting at 00:00
        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.Meps2p5: [tmpdir],
        }
        start = today - timedelta(hours=5)
        duration = 12

        expected = [
            f"{tmpdir}/{yesterday.year}/{yesterday.month:02d}/{yesterday.day:02d}/meps_det_2_5km_{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}T18Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T00Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T06Z.nc",
            f"{tmpdir}/{today.year}/{today.month:02d}/{today.day:02d}/meps_det_2_5km_{today.year}{today.month:02d}{today.day:02d}T12Z.nc",
        ]

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )

    # ################# Specific period of data                        #################
    # ################# Test data date range: 30-12-2025 to 05-01-2026 #################

    def test_hindcast48(self, tmp_path_with_meteo_files):
        # Test 7: Hindcast 48 hours
        tmpdir = str(tmp_path_with_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.Meps2p5: [tmpdir],
        }

        start = datetime.fromisoformat("2026-01-02T13:00:00")
        duration = 48

        expected = [
            f"{tmpdir}/2026/01/02/meps_det_2_5km_20260102T00Z.nc",
            f"{tmpdir}/2026/01/02/meps_det_2_5km_20260102T06Z.nc",
            f"{tmpdir}/2026/01/02/meps_det_2_5km_20260102T12Z.nc",
            f"{tmpdir}/2026/01/02/meps_det_2_5km_20260102T18Z.nc",
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T00Z.nc",
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T06Z.nc",
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T12Z.nc",
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T18Z.nc",
            f"{tmpdir}/2026/01/04/meps_det_2_5km_20260104T00Z.nc",
            f"{tmpdir}/2026/01/04/meps_det_2_5km_20260104T06Z.nc",
            f"{tmpdir}/2026/01/04/meps_det_2_5km_20260104T12Z.nc",
        ]

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )

    def test_forecast96(self, tmp_path_with_meteo_files):
        # Test 8: Long forecast 96 hours
        tmpdir = str(tmp_path_with_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.Meps2p5: [tmpdir],
        }

        start = datetime.fromisoformat("2025-12-30T13:00:00")
        duration = 96

        expected = [
            f"{tmpdir}/2025/12/30/meps_det_2_5km_20251230T00Z.nc",
            f"{tmpdir}/2025/12/30/meps_det_2_5km_20251230T06Z.nc",
            f"{tmpdir}/2025/12/30/meps_det_2_5km_20251230T12Z.nc",
            f"{tmpdir}/2025/12/30/meps_det_2_5km_20251230T18Z.nc",
            f"{tmpdir}/2025/12/31/meps_det_2_5km_20251231T00Z.nc",
            f"{tmpdir}/2025/12/31/meps_det_2_5km_20251231T06Z.nc",
            f"{tmpdir}/2025/12/31/meps_det_2_5km_20251231T12Z.nc",
            f"{tmpdir}/2025/12/31/meps_det_2_5km_20251231T18Z.nc",
            f"{tmpdir}/2026/01/01/meps_det_2_5km_20260101T00Z.nc",
            f"{tmpdir}/2026/01/01/meps_det_2_5km_20260101T06Z.nc",
            f"{tmpdir}/2026/01/01/meps_det_2_5km_20260101T12Z.nc",
            f"{tmpdir}/2026/01/01/meps_det_2_5km_20260101T18Z.nc",
            f"{tmpdir}/2026/01/02/meps_det_2_5km_20260102T00Z.nc",
            f"{tmpdir}/2026/01/02/meps_det_2_5km_20260102T06Z.nc",
            f"{tmpdir}/2026/01/02/meps_det_2_5km_20260102T12Z.nc",
            f"{tmpdir}/2026/01/02/meps_det_2_5km_20260102T18Z.nc",                     
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T00Z.nc",
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T06Z.nc",
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T12Z.nc",
        ]

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )

    ####    Test 9    ####
    def test_missing_00_files(self, tmp_path_with_meteo_files):
        # Part a: missing 00 files
        for year in (tmp_path_with_meteo_files).glob("*"):
            for month in (year).glob("*"):
                for day in (month).glob("*"):
                    for file in (day).glob("meps_det_2_5km_*T00Z.nc"):
                        file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)

        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.Meps2p5: [tmpdir],
        }

        start = datetime.fromisoformat("2026-01-04T13:00:00")
        duration = 48

        expected = [
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T18Z.nc",
            f"{tmpdir}/2026/01/04/meps_det_2_5km_20260104T06Z.nc",
            f"{tmpdir}/2026/01/04/meps_det_2_5km_20260104T12Z.nc",
            f"{tmpdir}/2026/01/04/meps_det_2_5km_20260104T18Z.nc",                     
            f"{tmpdir}/2026/01/05/meps_det_2_5km_20260105T06Z.nc",
            f"{tmpdir}/2026/01/05/meps_det_2_5km_20260105T12Z.nc",
            f"{tmpdir}/2026/01/05/meps_det_2_5km_20260105T18Z.nc",
        ]

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )

    def test_missing_whole_day(self, tmp_path_with_meteo_files):
        # Part b: missing whole day starting from day before missing day
        for file in (
            tmp_path_with_meteo_files
            / "2026/01/02"
        ).glob("meps_det_2_5km_*T*Z.nc"):
            file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)

        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.Meps2p5: [tmpdir],
        }

        start = datetime.fromisoformat("2026-01-01T13:00:00")
        duration = 48


        expected = [
            f"{tmpdir}/2026/01/01/meps_det_2_5km_20260101T00Z.nc",
            f"{tmpdir}/2026/01/01/meps_det_2_5km_20260101T06Z.nc",
            f"{tmpdir}/2026/01/01/meps_det_2_5km_20260101T12Z.nc",
            f"{tmpdir}/2026/01/01/meps_det_2_5km_20260101T18Z.nc", 
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T00Z.nc",                    
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T06Z.nc",
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T12Z.nc",
        ]

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )

    def test_missing_two_days(self, tmp_path_with_meteo_files):
        # Part c: missing data for two days
        for file in (
                    tmp_path_with_meteo_files
                    / "2026/01/02"
                ).glob("meps_det_2_5km_*T*Z.nc"):
            file.unlink(missing_ok=True)
        for file in (
                    tmp_path_with_meteo_files
                    / "2026/01/01"
                ).glob("meps_det_2_5km_*T*Z.nc"):
            file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)

        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.Meps2p5: [tmpdir],
        }

        start = datetime.fromisoformat("2026-01-02T13:00:00")  # starting on missing day
        duration = 48

        expected = [
            f"{tmpdir}/2025/12/31/meps_det_2_5km_20251231T18Z.nc", 
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T00Z.nc",                    
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T06Z.nc",
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T12Z.nc",
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T18Z.nc",
            f"{tmpdir}/2026/01/04/meps_det_2_5km_20260104T00Z.nc",                    
            f"{tmpdir}/2026/01/04/meps_det_2_5km_20260104T06Z.nc",
            f"{tmpdir}/2026/01/04/meps_det_2_5km_20260104T12Z.nc",
        ]

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )

    def test_missing_end_days(self, tmp_path_with_meteo_files):
        # Part d: missing all data from last days in run
        for file in (
                    tmp_path_with_meteo_files
                    / "2026/01/05"
                ).glob("meps_det_2_5km_*T*Z.nc"):
            file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.Meps2p5: [tmpdir],
        }

        start = datetime.fromisoformat("2026-01-05T13:00:00")  # starting on missing day
        duration = 48

        expected = [
            f"{tmpdir}/2026/01/04/meps_det_2_5km_20260104T18Z.nc",
        ]

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )

    def test_missing_all(self, tmp_path_with_meteo_files):
        # Part e: missing all data
        for year in (tmp_path_with_meteo_files).glob("*"):
            for month in (year).glob("*"):
                for day in (month).glob("*"):
                    for file in (day).glob("meps_det_2_5km_*T*Z.nc"):
                        file.unlink(missing_ok=True)

        tmpdir = str(tmp_path_with_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.Meps2p5: [tmpdir],
        }

        start = datetime.fromisoformat("2026-01-03T13:00:00")
        duration = 48

        expected = []

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )

    
    #### Test 10 ####
    def test_00_start(self, tmp_path_with_meteo_files):
        # Part a: Starting at 00:00
        tmpdir = str(tmp_path_with_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.Meps2p5: [tmpdir],
        }
        start = datetime.fromisoformat("2026-01-03T00:00:00")
        duration = 12

        expected = [
            f"{tmpdir}/2026/01/02/meps_det_2_5km_20260102T18Z.nc",
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T00Z.nc",
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T06Z.nc",
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T12Z.nc",
        ]

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )
    
    def test_missing_00_and_18(self, tmp_path_with_meteo_files):
        # Part b: Starting at 00:00, missing 00 and 18 files
        for year in (tmp_path_with_meteo_files).glob("*"):
            for month in (year).glob("*"):
                for day in (month).glob("*"):
                    for file in (day).glob("meps_det_2_5km_*T00Z.nc"):
                        file.unlink(missing_ok=True)
                    for file in (day).glob("meps_det_2_5km_*T18Z.nc"):
                        file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)
        res = Resources()
        res._MET_INPUTDIRS = {
            MetModel.Meps2p5: [tmpdir],
        }
        start = datetime.fromisoformat("2026-01-03T00:00:00")
        duration = 12

        expected = [
            f"{tmpdir}/2026/01/02/meps_det_2_5km_20260102T12Z.nc",
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T06Z.nc",
            f"{tmpdir}/2026/01/03/meps_det_2_5km_20260103T12Z.nc",
        ]

        assert (
            res.getMEPS25MeteorologyFiles(start, duration, prod_check=False) == expected
        )
