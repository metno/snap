# import sys
import pathlib
from datetime import date, datetime, time, timedelta

import pytest
from Snappy.Resources import Resources

""""
Created on Jan 09, 2025

@author: geche8548

Test data date range: 30-12-2025 to 05-01-2026
Except for test 7, "Today" refers to final day in date range (final date with 0 offset forecast)
For test 7, "Today" refers to the day this test is run (Actually today)
Tests included:

1. Only future forecast 48 hours
2. Only past forecast 48 hours
3. Long forecast 96 hours
4. Long forecast -96 hours
5. Missing specific files:
    a) missing 00 file
    b) missing whole day (not today)
    c) missing todays data
    d) missing two days of data
    e) missing all data
6. Starting at 00:00
    a) Starting at 00:00, full data
    b) Missing 18 file
7. Using data from the actual "today"
    a) 48 hour run from yesterday
    b) Missing todays data. 48 hour run from today


Realtime:
1. Forecast 48 hours
2. Run from yesterday 48 hours
3. Backwards run -72 hours
4. missing 00 file start today
5. Missing todays data start today
6. 
    a) start at 00 today
    b) missing 18 data start 00 today

   
Specific period:
7. Hindcast 48 hours
8. Long run 96 hours over date periodicities
9. Missing specific files:
    a) missing 00 file
    b) missing whole day
    c) missing two days of data
    d) missing end days
    e) missing all data
10. Starting at 00:00
    a) Starting at 00:00, full data
    b) Missing 18 file
    c) Missing day

"""


res = Resources()

today = datetime.combine(date.today(), time(5, 0, 0))
yesterday = today - timedelta(days=1)


@pytest.fixture
def tmp_path_with_meteo_files(tmp_path: pathlib.Path) -> pathlib.Path:
    """Create temporary directory with meteo-files, extending pytests build-in `tmp_path`.

    :param tmp_path: pytest build-in fixture tmp_pat
    :return: tmp_path, filled with meteo-files
    """
    for utc in ["00", "06", "12", "18"]:
        d = tmp_path / f"NRPA_EUROPE_0_1_{utc}"
        d.mkdir()
        for offset in ["00", "01", "02", "03"]:
            for day in range(1, 6):
                p = d / f"meteo2026010{day}_{offset}.nc"
                p.touch()
            for day in range(30, 32):
                p = d / f"meteo202512{day}_{offset}.nc"
                p.touch()
    return tmp_path


@pytest.fixture
def tmp_path_with_realtime_meteo_files(tmp_path: pathlib.Path) -> pathlib.Path:
    """Create temporary directory with meteo-files, extending pytests build-in `tmp_path`.

    :param tmp_path: pytest build-in fixture tmp_pat
    :return: tmp_path, filled with meteo-files
    """

    for utc in ["00", "06", "12", "18"]:
        d = tmp_path / f"NRPA_EUROPE_0_1_{utc}"
        d.mkdir()
        for offset in ["00", "01", "02", "03"]:
            for n_days in range(5):
                dat = today + timedelta(days=-n_days)
                p = d / f"meteo{dat.year}{dat.month:02d}{dat.day:02d}_{offset}.nc"
                p.touch()
    return tmp_path


class TestClass:
    def test_forecast48(self, tmp_path_with_realtime_meteo_files):
        # Test 1: Future forecast 48 hours

        # str-handling required in Resources
        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res._ECINPUTDIRS = [tmpdir]
        start = today
        duration = 48

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo{today.year}{today.month:02d}{today.day:02d}_02.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{today.year}{today.month:02d}{today.day:02d}_02.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{today.year}{today.month:02d}{today.day:02d}_02.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{today.year}{today.month:02d}{today.day:02d}_02.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    def test_start_yesterday(self, tmp_path_with_realtime_meteo_files):
        # Test 2: Starting yesterday. Tests both cases (past and future) in code.

        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res._ECINPUTDIRS = [tmpdir]
        start = yesterday
        duration = 48

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    def test_backwards_forecast72(self, tmp_path_with_realtime_meteo_files):
        # Part 3: Backwards forecast across future and past data.

        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res._ECINPUTDIRS = [tmpdir]
        start = today + timedelta(days=2)  # starting day after tomorrow
        duration = -72

        yesterday = today - timedelta(days=1)

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo{today.year}{today.month:02d}{today.day:02d}_02.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{today.year}{today.month:02d}{today.day:02d}_02.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{today.year}{today.month:02d}{today.day:02d}_02.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{today.year}{today.month:02d}{today.day:02d}_02.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    def test_missing_00_start_today(self, tmp_path_with_realtime_meteo_files):
        # Part 4: starting today with missing 00 files
        for file in (tmp_path_with_realtime_meteo_files / "NRPA_EUROPE_0_1_00/").glob(
            "*"
        ):
            file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_realtime_meteo_files)

        res._ECINPUTDIRS = [tmpdir]

        start = today
        duration = 48

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_02.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{today.year}{today.month:02d}{today.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_03.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{today.year}{today.month:02d}{today.day:02d}_02.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{today.year}{today.month:02d}{today.day:02d}_02.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{today.year}{today.month:02d}{today.day:02d}_02.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    def test_missing_today(self, tmp_path_with_realtime_meteo_files):
        # Test 5: missing all data from today
        for utc in ["00", "06", "12", "18"]:
            for file in (
                tmp_path_with_realtime_meteo_files / f"NRPA_EUROPE_0_1_{utc}/"
            ).glob(f"meteo{today.year}{today.month:02d}{today.day:02d}_*"):
                file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res._ECINPUTDIRS = [tmpdir]
        start = today  # starting on missing day
        duration = 48

        yesterday = today - timedelta(days=1)

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_02.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_03.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_01.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    def test_today_00_start(self, tmp_path_with_realtime_meteo_files):
        # Test 6a: Starting at 00:00
        tmpdir = str(tmp_path_with_realtime_meteo_files)
        res._ECINPUTDIRS = [tmpdir]
        start = today - timedelta(hours=5)
        duration = 12

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    def test_today_00_start_missing_18(self, tmp_path_with_realtime_meteo_files):
        # Test 6b: Starting at 00:00, missing 18:00 files
        for file in (tmp_path_with_realtime_meteo_files / "NRPA_EUROPE_0_1_18/").glob(
            "*"
        ):
            file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_realtime_meteo_files)

        res._ECINPUTDIRS = [tmpdir]

        start = today - timedelta(hours=5)
        duration = 12

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{yesterday.year}{yesterday.month:02d}{yesterday.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo{today.year}{today.month:02d}{today.day:02d}_00.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    ################# Specific period of data                        #################
    ################# Test data date range: 30-12-2025 to 05-01-2026 #################

    def test_hindcast48(self, tmp_path_with_meteo_files):
        # Test 7: Hindcast 48 hours
        tmpdir = str(tmp_path_with_meteo_files)
        res._ECINPUTDIRS = [tmpdir]

        start = datetime.fromisoformat("2026-01-02T13:00:00")
        duration = 48

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260104_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260104_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260104_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260104_00.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    def test_forecast96(self, tmp_path_with_meteo_files):
        # Test 8: Long forecast 96 hours
        tmpdir = str(tmp_path_with_meteo_files)
        res._ECINPUTDIRS = [tmpdir]

        start = datetime.fromisoformat("2025-12-30T13:00:00")
        duration = 96

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20251230_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20251230_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20251230_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20251230_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20251231_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20251231_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20251231_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20251231_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260101_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260101_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260101_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260101_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    ####    Test 9    ####
    def test_missing_00_files(self, tmp_path_with_meteo_files):
        # Part a: missing 00 files
        for file in (tmp_path_with_meteo_files / "NRPA_EUROPE_0_1_00/").glob("*"):
            file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)

        res._ECINPUTDIRS = [tmpdir]

        start = datetime.fromisoformat("2026-01-04T13:00:00")
        duration = 48

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260103_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260104_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260104_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260104_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260104_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260105_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260105_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260105_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260105_01.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    def test_missing_whole_day(self, tmp_path_with_meteo_files):
        # Part b: missing whole day starting from day before missing day
        for utc in ["00", "06", "12", "18"]:
            for file in (tmp_path_with_meteo_files / f"NRPA_EUROPE_0_1_{utc}/").glob(
                "meteo20260102_*"
            ):
                file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)

        res._ECINPUTDIRS = [tmpdir]

        start = datetime.fromisoformat("2026-01-01T13:00:00")
        duration = 48

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260101_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260101_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260101_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260101_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260101_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    def test_missing_two_days(self, tmp_path_with_meteo_files):
        # Part c: missing data for two days
        for utc in ["00", "06", "12", "18"]:
            for file in (tmp_path_with_meteo_files / f"NRPA_EUROPE_0_1_{utc}/").glob(
                "meteo20260105_*"
            ):
                file.unlink(missing_ok=True)
            for file in (tmp_path_with_meteo_files / f"NRPA_EUROPE_0_1_{utc}/").glob(
                "meteo20260104_*"
            ):
                file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)

        res._ECINPUTDIRS = [tmpdir]

        start = datetime.fromisoformat("2026-01-03T13:00:00")  # starting on missing day
        duration = 48

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260103_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260103_02.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    def test_missing_end_days(self, tmp_path_with_meteo_files):
        # Part d: missing all data from last days in run
        for utc in ["00", "06", "12", "18"]:
            for file in (tmp_path_with_meteo_files / f"NRPA_EUROPE_0_1_{utc}/").glob(
                "meteo20260105_*"
            ):
                file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)

        res._ECINPUTDIRS = [tmpdir]

        start = datetime.fromisoformat("2026-01-05T13:00:00")  # starting on missing day
        duration = 48

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260104_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260104_02.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260104_03.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    def test_missing_all(self, tmp_path_with_meteo_files):
        # Part e: missing all data
        for utc in ["00", "06", "12", "18"]:
            for file in (tmp_path_with_meteo_files / f"NRPA_EUROPE_0_1_{utc}/").glob(
                "*"
            ):
                file.unlink(missing_ok=True)

        tmpdir = str(tmp_path_with_meteo_files)
        res._ECINPUTDIRS = [tmpdir]

        start = datetime.fromisoformat("2026-01-03T13:00:00")
        duration = 48

        expected = []

        assert res.getECMeteorologyFiles(start, duration) == expected

    #### Test 10 ####
    def test_00_start(self, tmp_path_with_meteo_files):
        # Part a: Starting at 00:00
        tmpdir = str(tmp_path_with_meteo_files)
        res._ECINPUTDIRS = [tmpdir]
        start = datetime.fromisoformat("2026-01-03T00:00:00")
        duration = 12

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    def test_00_start_missing18(self, tmp_path_with_meteo_files):
        # Part b: Starting at 00:00, missing 18:00 files
        for file in (tmp_path_with_meteo_files / "NRPA_EUROPE_0_1_18/").glob("*"):
            file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)

        res._ECINPUTDIRS = [tmpdir]

        start = datetime.fromisoformat("2026-01-03T00:00:00")
        duration = 12

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260102_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected

    def test_missing_day_00_start(self, tmp_path_with_meteo_files):
        # Part c: missing all data from today
        for utc in ["00", "06", "12", "18"]:
            for file in (tmp_path_with_meteo_files / f"NRPA_EUROPE_0_1_{utc}/").glob(
                "meteo20260102_*"
            ):
                file.unlink(missing_ok=True)
        tmpdir = str(tmp_path_with_meteo_files)

        res._ECINPUTDIRS = [tmpdir]
        start = datetime.fromisoformat("2026-01-03T00:00:00")
        duration = 12

        expected = [
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260101_01.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
            f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
        ]

        assert res.getECMeteorologyFiles(start, duration) == expected
