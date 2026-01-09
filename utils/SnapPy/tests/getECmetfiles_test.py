import sys
from datetime import datetime
import subprocess
import tempfile
import os
import stat

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

# Create Tmp directory

Dir = tempfile.TemporaryDirectory()

# Code for creating files

with open(Dir.name + "/make-files-2.sh", "w") as rsh:
    rsh.write("""\
#! /bin/bash -x
for utc in 00 06 12 18; do
mkdir ./NRPA_EUROPE_0_1_${utc}
    for day in {1..5}; do
        for offset in 00 01 02 03; do
            echo "" > ./NRPA_EUROPE_0_1_${utc}/meteo2026010${day}_${offset}.nc
        done
    done
done
for utc in 00 06 12 18; do
    for day in {30..31}; do
        for offset in 00 01 02 03; do
            echo "" > ./NRPA_EUROPE_0_1_${utc}/meteo202512${day}_${offset}.nc
        done
    done
done
              
              """)


os.chmod(Dir.name + "/make-files-2.sh", stat.S_IRWXU)


####    TESTS 1-4    #####

# Create files

subprocess.run("mkdir ./Tests1-4", shell=True, cwd=Dir.name)

subprocess.run(["bash ../make-files-2.sh"], shell=True, cwd=f"{Dir.name}/Tests1-4")

subprocess.run("cp -r ./Tests1-4/ ./Test5a", shell=True, cwd=Dir.name)
subprocess.run("rm -r ./Test5a/NRPA_EUROPE_0_1_00/", shell=True, cwd=Dir.name)

subprocess.run("cp -r ./Tests1-4/ ./Test5b", shell=True, cwd=Dir.name)

for utc in ["00", "06", "12", "18"]:
    subprocess.run(
        f"rm  ./Test5b/NRPA_EUROPE_0_1_{utc}/meteo20260102_*.nc",
        shell=True,
        cwd=Dir.name,
    )

subprocess.run("cp -r ./Tests1-4/ ./Test5c", shell=True, cwd=Dir.name)

for utc in ["00", "06", "12", "18"]:
    subprocess.run(
        f"rm  ./Test5c/NRPA_EUROPE_0_1_{utc}/meteo20260105_*.nc",
        shell=True,
        cwd=Dir.name,
    )


subprocess.run("cp -r ./Test5c/ ./Test5d", shell=True, cwd=Dir.name)

for utc in ["00", "06", "12", "18"]:
    subprocess.run(
        f"rm  ./Test5d/NRPA_EUROPE_0_1_{utc}/meteo20260104_*.nc",
        shell=True,
        cwd=Dir.name,
    )

subprocess.run("cp -r ./Test5d/ ./Test5e", shell=True, cwd=Dir.name)

for utc in ["00", "06", "12", "18"]:
    subprocess.run(f"rm  ./Test5e/NRPA_EUROPE_0_1_{utc}/*", shell=True, cwd=Dir.name)


class TestClass:
    def test_Forecast48(self):
        # Test 1: Future forecast 48 hours
        Res._ECINPUTDIRS = [f"{Dir.name}/Tests1-4"]
        start = datetime.fromisoformat("2026-01-05T13:00:00")
        duration = 48

        expected = [
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_00/meteo20260105_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_06/meteo20260105_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_12/meteo20260105_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20260105_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20260105_01.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20260105_02.nc",
        ]
    
        assert Res.getECMeteorologyFiles(start, duration) == expected

    def test_Hindcast48(self):
        # Test 2: Hindcast 48 hours
        Res._ECINPUTDIRS = [f"{Dir.name}/Tests1-4"]

        start = datetime.fromisoformat("2026-01-02T13:00:00")
        duration = 48

        expected = [
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_00/meteo20260102_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_06/meteo20260102_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_12/meteo20260102_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20260102_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_00/meteo20260104_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_06/meteo20260104_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_12/meteo20260104_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20260104_00.nc",
    ]

        assert Res.getECMeteorologyFiles(start, duration) == expected

    def test_Forecast96(self):
        # Test 3 Long forecast 96 hours
        Res._ECINPUTDIRS = [f"{Dir.name}/Tests1-4"]

        start = datetime.fromisoformat("2025-12-30T13:00:00")
        duration = 96

        expected = [
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_00/meteo20251230_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_06/meteo20251230_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_12/meteo20251230_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20251230_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_00/meteo20251231_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_06/meteo20251231_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_12/meteo20251231_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20251231_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_00/meteo20260101_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_06/meteo20260101_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_12/meteo20260101_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20260101_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_00/meteo20260102_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_06/meteo20260102_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_12/meteo20260102_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20260102_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
    ]

        assert Res.getECMeteorologyFiles(start, duration) == expected

    def test_BackwardsForecast96(self):
        # Test 4 Long forecast -96 hours
        Res._ECINPUTDIRS = [f"{Dir.name}/Tests1-4"]
        
        start = datetime.fromisoformat("2026-01-02T13:00:00")
        duration = -96

        expected = [
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_00/meteo20251230_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_06/meteo20251230_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_12/meteo20251230_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20251230_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_00/meteo20251231_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_06/meteo20251231_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_12/meteo20251231_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20251231_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_00/meteo20260101_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_06/meteo20260101_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_12/meteo20260101_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20260101_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_00/meteo20260102_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_06/meteo20260102_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_12/meteo20260102_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20260102_00.nc",
    ]

        assert Res.getECMeteorologyFiles(start, duration) == expected
    ####    Test 5    ####
    def test_Missing00Files(self):
        # Part a: missing 00 files
    
        Res._ECINPUTDIRS = [f"{Dir.name}/Test5a"]

        start = datetime.fromisoformat("2026-01-04T13:00:00")
        duration = 48

        expected = [
        f"{Dir.name}/Test5a/NRPA_EUROPE_0_1_18/meteo20260103_01.nc",
        f"{Dir.name}/Test5a/NRPA_EUROPE_0_1_06/meteo20260104_00.nc",
        f"{Dir.name}/Test5a/NRPA_EUROPE_0_1_12/meteo20260104_00.nc",
        f"{Dir.name}/Test5a/NRPA_EUROPE_0_1_18/meteo20260104_00.nc",
        f"{Dir.name}/Test5a/NRPA_EUROPE_0_1_18/meteo20260104_01.nc",
        f"{Dir.name}/Test5a/NRPA_EUROPE_0_1_06/meteo20260105_00.nc",
        f"{Dir.name}/Test5a/NRPA_EUROPE_0_1_12/meteo20260105_00.nc",
        f"{Dir.name}/Test5a/NRPA_EUROPE_0_1_18/meteo20260105_00.nc",
        f"{Dir.name}/Test5a/NRPA_EUROPE_0_1_18/meteo20260105_01.nc",
    ]

        assert Res.getECMeteorologyFiles(start, duration) == expected

    def test_MissingWholeDay1(self):
        # Part b: missing whole day starting from day before missing day
        Res._ECINPUTDIRS = [f"{Dir.name}/Test5b"]

        start = datetime.fromisoformat("2026-01-01T13:00:00") 
        duration = 48


        expected = [
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_00/meteo20260101_00.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_06/meteo20260101_00.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_12/meteo20260101_00.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_18/meteo20260101_00.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_18/meteo20260101_01.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
    ]

        assert Res.getECMeteorologyFiles(start, duration) == expected

    def test_MissingWholeDay2(self):
        # Part b: missing whole day starting on missing day
        Res._ECINPUTDIRS = [f"{Dir.name}/Test5b"]

        start = datetime.fromisoformat("2026-01-02T13:00:00")
        duration = 48

        expected = [
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_18/meteo20260101_01.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_00/meteo20260104_00.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_06/meteo20260104_00.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_12/meteo20260104_00.nc",
        f"{Dir.name}/Test5b/NRPA_EUROPE_0_1_18/meteo20260104_00.nc",
    ]
        
        assert Res.getECMeteorologyFiles(start, duration) == expected

    def test_MissingToday(self):
        # Part c: missing all data from today
        Res._ECINPUTDIRS = [f"{Dir.name}/Test5c"]

        start = datetime.fromisoformat("2026-01-05T13:00:00")  # starting on missing day
        duration = 48

        expected = [
        f"{Dir.name}/Test5c/NRPA_EUROPE_0_1_18/meteo20260104_01.nc",
        f"{Dir.name}/Test5c/NRPA_EUROPE_0_1_18/meteo20260104_02.nc",
        f"{Dir.name}/Test5c/NRPA_EUROPE_0_1_18/meteo20260104_03.nc",
    ]

        assert Res.getECMeteorologyFiles(start, duration) == expected
    

    def test_MissingTwoDays(self):
        # Part d: missing data for two days
        Res._ECINPUTDIRS = [f"{Dir.name}/Test5d"]
        
        start = datetime.fromisoformat("2026-01-03T13:00:00")  # starting on missing day
        duration = 48

        expected = [
        f"{Dir.name}/Test5d/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
        f"{Dir.name}/Test5d/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
        f"{Dir.name}/Test5d/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
        f"{Dir.name}/Test5d/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
        f"{Dir.name}/Test5d/NRPA_EUROPE_0_1_18/meteo20260103_01.nc",
        f"{Dir.name}/Test5d/NRPA_EUROPE_0_1_18/meteo20260103_02.nc",
    ]

        assert Res.getECMeteorologyFiles(start, duration) == expected

    def test_MissingAll(self):
        # Part e: missing all data
        Res._ECINPUTDIRS = [f"{Dir.name}/Test5e"]

        start = datetime.fromisoformat("2026-01-03T13:00:00")  # starting on missing day
        duration = 48

        expected =[]

        assert Res.getECMeteorologyFiles(start, duration) == expected
