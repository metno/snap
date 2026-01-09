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

#!/usr/bin/env python

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


Res._ECINPUTDIRS = [f"{Dir.name}/Tests1-4"]


# Test 1: Future forecast 48 hours


start = datetime.fromisoformat("2026-01-05T13:00:00")
duration = 48


ans1 = Res.getECMeteorologyFiles(start, duration)


def func1(ans):
    if ans == [
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_00/meteo20260105_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_06/meteo20260105_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_12/meteo20260105_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20260105_00.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20260105_01.nc",
        f"{Dir.name}/Tests1-4/NRPA_EUROPE_0_1_18/meteo20260105_02.nc",
    ]:
        return True
    else:
        return False


# Test 2: Hindcast 48 hours

start = datetime.fromisoformat("2026-01-02T13:00:00")
duration = 48

ans2 = Res.getECMeteorologyFiles(start, duration)


def func2(ans):
    if ans == [
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
    ]:
        return True
    else:
        return False


# Test 3 Long forecast 96 hours

start = datetime.fromisoformat("2025-12-30T13:00:00")
duration = 96

ans3 = Res.getECMeteorologyFiles(start, duration)


def func3(ans):
    if ans == [
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
    ]:
        return True
    else:
        return False


# Test 4 Long forecast -96 hours

start = datetime.fromisoformat("2026-01-02T13:00:00")
duration = -96

ans4 = Res.getECMeteorologyFiles(start, duration)


def func4(ans):
    if ans == [
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
    ]:
        return True
    else:
        return False


####    Test 5    ####
### Missing specific files files

subprocess.run("cp -r ./Tests1-4 ./Test5", shell=True, cwd=Dir.name)
Res._ECINPUTDIRS = [f"{Dir.name}/Test5"]

# Part a: missing 00 files

subprocess.run("rm -r ./Test5/NRPA_EUROPE_0_1_00/", shell=True, cwd=Dir.name)

start = datetime.fromisoformat("2026-01-04T13:00:00")
duration = 48

ans5a = Res.getECMeteorologyFiles(start, duration)


def func5a(ans):
    if ans == [
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260103_01.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_06/meteo20260104_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_12/meteo20260104_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260104_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260104_01.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_06/meteo20260105_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_12/meteo20260105_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260105_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260105_01.nc",
    ]:
        return True
    else:
        return False


# Part b: missing whole day

subprocess.run("cp -r ./Tests1-4/* ./Test5", shell=True, cwd=Dir.name)

for utc in ["00", "06", "12", "18"]:
    subprocess.run(
        f"rm  ./Test5/NRPA_EUROPE_0_1_{utc}/meteo20260102_*.nc",
        shell=True,
        cwd=Dir.name,
    )

start = datetime.fromisoformat(
    "2026-01-01T13:00:00"
)  # starting from day before missing day
duration = 48

ans5b1 = Res.getECMeteorologyFiles(start, duration)


def func5b1(ans):
    if ans == [
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_00/meteo20260101_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_06/meteo20260101_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_12/meteo20260101_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260101_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260101_01.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
    ]:
        return True
    else:
        return False


start = datetime.fromisoformat("2026-01-02T13:00:00")  # starting on missing day
duration = 48

ans5b2 = Res.getECMeteorologyFiles(start, duration)


def func5b2(ans):
    if ans == [
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260101_01.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_00/meteo20260104_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_06/meteo20260104_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_12/meteo20260104_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260104_00.nc",
    ]:
        return True
    else:
        return False


# Part c: missing all data from today

subprocess.run("cp -r ./Tests1-4/* ./Test5", shell=True, cwd=Dir.name)

for utc in ["00", "06", "12", "18"]:
    subprocess.run(
        f"rm  ./Test5/NRPA_EUROPE_0_1_{utc}/meteo20260105_*.nc",
        shell=True,
        cwd=Dir.name,
    )

start = datetime.fromisoformat("2026-01-05T13:00:00")  # starting on missing day
duration = 48


ans5c = Res.getECMeteorologyFiles(start, duration)


def func5c(ans):
    if ans == [
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260104_01.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260104_02.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260104_03.nc",
    ]:
        return True
    else:
        return False


# Part d: missing data for two days

for utc in ["00", "06", "12", "18"]:
    subprocess.run(
        f"rm  ./Test5/NRPA_EUROPE_0_1_{utc}/meteo20260104_*.nc",
        shell=True,
        cwd=Dir.name,
    )

start = datetime.fromisoformat("2026-01-03T13:00:00")  # starting on missing day
duration = 48


ans5d = Res.getECMeteorologyFiles(start, duration)


def func5d(ans):
    if ans == [
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_00/meteo20260103_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_06/meteo20260103_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_12/meteo20260103_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260103_00.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260103_01.nc",
        f"{Dir.name}/Test5/NRPA_EUROPE_0_1_18/meteo20260103_02.nc",
    ]:
        return True
    else:
        return False


# Part e: missing all data

for utc in ["00", "06", "12", "18"]:
    subprocess.run(f"rm  ./Test5/NRPA_EUROPE_0_1_{utc}/*", shell=True, cwd=Dir.name)

start = datetime.fromisoformat("2026-01-03T13:00:00")  # starting on missing day
duration = 48


ans5e = Res.getECMeteorologyFiles(start, duration)


def func5e(ans):
    if ans == []:
        return True
    else:
        return False


class TestClass:
    def test_1(self):
        assert func1(ans1)

    def test_2(self):
        assert func2(ans2)

    def test_3(self):
        assert func3(ans3)

    def test_4(self):
        assert func4(ans4)

    def test_5a(self):
        assert func5a(ans5a)

    def test_5b1(self):
        assert func5b1(ans5b1)

    def test_5b2(self):
        assert func5b2(ans5b2)

    def test_5c(self):
        assert func5c(ans5c)

    def test_5d(self):
        assert func5d(ans5d)

    def test_5e(self):
        assert func5e(ans5e)
