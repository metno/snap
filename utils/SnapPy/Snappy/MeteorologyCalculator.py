# SNAP: Servere Nuclear Accident Programme
# Copyright (C) 1992-2017   Norwegian Meteorological Institute
#
# This file is part of SNAP. SNAP is free software: you can
# redistribute it and/or modify it under the terms of the
# GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
"""
Created on Oct 24, 2016

@author: heikok
"""

from datetime import datetime, timedelta
import abc
from glob import iglob
import math
import os
import subprocess
import time

from Snappy.Resources import Resources


class GlobalMeteoResource:
    """containter for information needed by MeteorologyCalculator containing:

    indirs: inputdirectories
    outputdir: output directory
    output_filename_pattern: pattern of output file
    pathglob: glob for files in inputdirectories, e.g. ec_atmo_0_1deg_????????T??????Z_3h.nc
    pathptime: strptime for files, e.g. ec_atmo_0_1deg_%Y%m%dT%H%M%SZ_3h.nc
    path_grace_period_sec: seconds to wait before a file is used (i.e. no longer written)
    domainHeight: total y-Axis length of domain
    domainWidth: total x-Axis lenght of domain
    domainDeltaX: x-resolution of domain
    domainDeltaY: y-resolution of domain
    """

    indirs = []
    outputdir = ""
    output_filename_pattern = ""
    pathglob = ""
    pathptime = ""
    path_grace_period_sec = 60 * 15
    domainHeight = 10
    domainWidth = 10
    domainDeltaX = 0.1
    domainDeltaY = 0.1
    timeoffset = (
        0  # required offset between reference-time and first useful startup-time
    )


class MeteoDataNotAvailableException(Exception):
    def __init__(self, value):
        """exception having some kind of documention in args[0]"""
        self.parameter = value

    def __str__(self):
        return repr(self.parameter)


class MeteorologyCalculator(abc.ABC):
    """Base-class to pre-calculate/extract meteorology"""

    @staticmethod
    def findAllGlobalData(res: GlobalMeteoResource):
        """Static method to find all global dataset.

        Args:
            dtime: datetime object with a start-time, which should be included in the dataset

        Returns:
            A list of tuples with [(forecast-time, file)]
        """
        timesFiles = []  # tuples with datetime, file
        for inDir in res.indirs:
            for iFile in iglob(os.path.join(inDir, res.pathglob)):
                statinfo = os.stat(iFile)
                if statinfo.st_mtime < (
                    time.time() - res.path_grace_period_sec
                ):  # file hasn't been changed in x sec
                    dateFile = datetime.strptime(os.path.basename(iFile), res.pathptime)
                    timesFiles.append((dateFile, iFile))
        return timesFiles

    @staticmethod
    def findGlobalData(res: GlobalMeteoResource, dtime: datetime):
        """Method to find the global dataset with the latest forecast time which includes dtime.

        Args:
            res: resources from getGlobalMeteoResources
            dtime: datetime object with a start-time, which will be included in the dataset

        Returns:
            A tuple with referencetime and filename

        Raises:
            MeteoDataNotAvailableException: no data for the dtime can be found
        """
        timesFiles = MeteorologyCalculator.findAllGlobalData(res)
        lastTimeFile = (None, None)
        for timeFile in sorted(timesFiles, key=lambda t: t[0]):
            if timeFile[0] <= dtime:
                lastTimeFile = timeFile
            else:
                break
        if lastTimeFile[0] is None:
            raise MeteoDataNotAvailableException(
                "no input data in {dirs} for {time}: ".format(
                    dirs=res.indirs, time=dtime
                )
            )
        return lastTimeFile

    @abc.abstractstaticmethod
    def getGlobalMeteoResources():
        """retrieve the GlobalMeteoResources from internal resources"""
        pass

    def getLat0(latCenter, domainHeight):
        # get a domain starting every 10th degree
        lat0 = math.floor((latCenter - (domainHeight / 2.0)) / 10.0) * 10
        if lat0 < -80:
            lat0 = -89
        if lat0 + domainHeight > 89:
            lat0 = 89 - domainHeight
        return lat0

    def getLon0(lonCenter, domainWidth):
        # get a domain starting every 10th degree
        lon0 = math.floor((lonCenter - (domainWidth / 2.0)) / 10.0) * 10
        return lon0

    def __init__(
        self, res: GlobalMeteoResource, dtime: datetime, domainCenterX, domainCenterY
    ):
        """Calculate the ec-meteorology unless it exists

        Args:
            res: GlobalMeteoResource object
            dtime: datetime object containing the earliest time expected
            domainCenterX: number where the domain should start (longitude), will be rounded
            domainCenterY: number where the domain should start (latitude), will be rounded

        Raises:
            MeteoDataNotAvailableException: no data for the dtime can be found
        """
        self.proc = None  # storage for background-process
        self.res = res

        lastDateFile = self.findGlobalData(res, dtime - timedelta(hours=res.timeoffset))
        self.date = lastDateFile[0]
        self.globalfile = lastDateFile[1]
        utc = lastDateFile[0].hour
        # domain every 10th degree
        lat0 = math.floor((domainCenterY - (res.domainHeight / 2.0)) / 10.0) * 10
        if lat0 < -80:
            lat0 = -89
        if lat0 + res.domainHeight > 89:
            lat0 = 89 - res.domainHeight
        # snap can only cross date-line when both start and position are negative or positive
        lon0 = math.floor((domainCenterX - (res.domainWidth / 2.0)) / 10.0) * 10
        self.lat0 = int(lat0)
        self.lon0 = int(lon0)
        self.outputdir = os.path.join(
            res.outputdir,
            "NRPA_LON{x}_LAT{y}_{utc:02d}".format(x=self.lon0, y=self.lat0, utc=utc),
        )

        # try to avoid conflicting processes (not 100% save)
        i = 1
        while os.path.isfile(os.path.join(self.outputdir, "running")):
            self.outputdir = os.path.join(
                res.outputdir, "NRPA_TEMP_{utc:02d}_{i}".format(utc=utc, i=i)
            )
            i += 1

        if not os.path.exists(self.outputdir):
            os.makedirs(self.outputdir)

        self.add_expected_files(lastDateFile[0])

    @abc.abstractmethod
    def add_expected_files(self, date):
        """add the expected files to files (must exist) and optFiles (don't need to exist).
        This method must be overwritten."""
        assert False
        pass

    def get_meteorology_files(self):
        """return the meteorology files"""
        if self.must_calc():
            raise MeteoDataNotAvailableException(
                "unable to create meteo-data for {year}-{month}-{day} in {dir}".format(
                    year=self.date.year,
                    month=self.date.month,
                    day=self.date.day,
                    dir=self.outputdir,
                )
            )
        files = self.files
        for f in self.optFiles:
            if os.path.isfile(f):
                files.append(f)
        return files

    def get_grid_startX_Y(self):
        """return a tuple with x0 and y0"""
        return (self.lon0, self.lat0)

    def must_calc(self):
        """check if calculation is required or has been done earlier"""
        recalc = False
        for f in self.files:
            if not os.path.isfile(f):
                recalc = True
        return recalc

    @abc.abstractmethod
    def calc(self, proc=None):
        """abstract baseclass to run the calculation of meteo-data if required. Should check if self.must_calc() at the beginning.

        Args:
           proc -- A QProcess, which will be used to run a longer process in the background.
                  STDERR/STDOUT and signal-handler should be set. If proc is None, the
                  subprocess will be run in the current-process. If proc is set, the caller
                  needs to wait for the proc to finish before calling other methods of this object"""
        #        if (not self.must_calc()):
        #            return
        pass


if __name__ == "__main__":
    pass
