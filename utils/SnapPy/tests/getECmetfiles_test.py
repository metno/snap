import sys
from datetime import datetime
import subprocess
import tempfile
import os
import stat
import pytest

sys.path.append("../")
from Snappy import Resources as R

""""
Created on Jan 09, 2025

@author: geche8548

Test data date range: 30-12-2025 to 05-01-2025
Throughout script "Today" refers to final day in date range (final date with 0 offset forecast)
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
"""


Res = R.Resources()


@pytest.fixture
def make_files(tmp_path):
    for utc in ["00", "06", "12", "18"]:
        d = tmp_path/f"NRPA_EUROPE_0_1_{utc}"
        d.mkdir()
        for offset in ["00", "01", "02", "03"]:
            for day in range(1,6):
                p = d/f"meteo2026010{day}_{offset}.nc"
                p.write_text("")
            for day in range(30,32):
                p = d/f"meteo202512{day}_{offset}.nc"
                p.write_text("")
    return tmp_path

class TestClass:
                         

    def test_Forecast48(self,make_files):
        # Test 1: Future forecast 48 hours
        tmpdir = str(make_files)
        Res._ECINPUTDIRS = [tmpdir]
        start = datetime.fromisoformat("2026-01-05T13:00:00")
        duration = 48

        expected = [
        f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260105_00.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260105_00.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260105_00.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260105_00.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260105_01.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260105_02.nc",
        ]
    
        assert Res.getECMeteorologyFiles(start, duration) == expected

    def test_Hindcast48(self,make_files):
        # Test 2: Hindcast 48 hours
        tmpdir = str(make_files)
        Res._ECINPUTDIRS = [tmpdir]

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

        assert Res.getECMeteorologyFiles(start, duration) == expected

    def test_Forecast96(self,make_files):
        # Test 3 Long forecast 96 hours
        tmpdir = str(make_files)
        Res._ECINPUTDIRS = [tmpdir]

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

        assert Res.getECMeteorologyFiles(start, duration) == expected

    def test_BackwardsForecast96(self,make_files):
        # Test 4 Long forecast -96 hours
        tmpdir = str(make_files)
        Res._ECINPUTDIRS = [tmpdir]
        
        start = datetime.fromisoformat("2026-01-02T13:00:00")
        duration = -96

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
    ]

        assert Res.getECMeteorologyFiles(start, duration) == expected
    ####    Test 5    ####
    def test_Missing00Files(self,make_files):
        # Part a: missing 00 files
        for file in (make_files/"NRPA_EUROPE_0_1_00/").glob("*"):
            file.unlink(missing_ok=True)
        tmpdir = str(make_files)

    
        Res._ECINPUTDIRS = [tmpdir]

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

        assert Res.getECMeteorologyFiles(start, duration) == expected

    def test_MissingWholeDay1(self,make_files):
        # Part b: missing whole day starting from day before missing day
        for utc in ["00", "06", "12", "18"]:
            for file in (make_files/f"NRPA_EUROPE_0_1_{utc}/").glob("meteo20260102_*"):
                file.unlink(missing_ok=True)
        tmpdir = str(make_files)

        Res._ECINPUTDIRS = [tmpdir]

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

        assert Res.getECMeteorologyFiles(start, duration) == expected

    def test_MissingWholeDay2(self,make_files):
        # Part b: missing whole day starting on missing day
        for utc in ["00", "06", "12", "18"]:
            for file in (make_files/f"NRPA_EUROPE_0_1_{utc}/").glob("meteo20260102_*"):
                file.unlink(missing_ok=True)
        tmpdir = str(make_files)

        Res._ECINPUTDIRS = [tmpdir]

        start = datetime.fromisoformat("2026-01-02T13:00:00")
        duration = 48

        expected = [
        f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260101_01.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_00/meteo20260104_00.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_06/meteo20260104_00.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_12/meteo20260104_00.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260104_00.nc",
    ]
        
        assert Res.getECMeteorologyFiles(start, duration) == expected

    def test_MissingToday(self,make_files):
        # Part c: missing all data from today
        for utc in ["00", "06", "12", "18"]:
            for file in (make_files/f"NRPA_EUROPE_0_1_{utc}/").glob("meteo20260105_*"):
                file.unlink(missing_ok=True)
        tmpdir = str(make_files)

        Res._ECINPUTDIRS = [tmpdir]

        start = datetime.fromisoformat("2026-01-05T13:00:00")  # starting on missing day
        duration = 48

        expected = [
        f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260104_01.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260104_02.nc",
        f"{tmpdir}/NRPA_EUROPE_0_1_18/meteo20260104_03.nc",
    ]

        assert Res.getECMeteorologyFiles(start, duration) == expected
    

    def test_MissingTwoDays(self,make_files):
        # Part d: missing data for two days
        for utc in ["00", "06", "12", "18"]:
            for file in (make_files/f"NRPA_EUROPE_0_1_{utc}/").glob("meteo20260105_*"):
                file.unlink(missing_ok=True)
            for file in (make_files/f"NRPA_EUROPE_0_1_{utc}/").glob("meteo20260104_*"):
                file.unlink(missing_ok=True)            
        tmpdir = str(make_files)

        Res._ECINPUTDIRS = [tmpdir]
        
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

        assert Res.getECMeteorologyFiles(start, duration) == expected

    def test_MissingAll(self,make_files):
        # Part e: missing all data
        for utc in ["00", "06", "12", "18"]:
            for file in (make_files/f"NRPA_EUROPE_0_1_{utc}/").glob("*"):
                file.unlink(missing_ok=True)
        
        tmpdir = str(make_files)
        Res._ECINPUTDIRS = [tmpdir]

        start = datetime.fromisoformat("2026-01-03T13:00:00")  # starting on missing day
        duration = 48

        expected =[]

        assert Res.getECMeteorologyFiles(start, duration) == expected
