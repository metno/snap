import abc
import os
import time
from datetime import datetime, timedelta
from glob import iglob


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
    dir_template = "NRPA_LON{lon0}_LAT{lat0}_{utc:02d}"

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

    @staticmethod
    @abc.abstractmethod
    def getGlobalMeteoResources():
        """retrieve the GlobalMeteoResources from internal resources"""
        pass

    @staticmethod
    def getLat0(latCenter, domainHeight) -> int:
        # get a domain starting every 10th degree
        lat0 = round((latCenter - (domainHeight / 2.0)) / 10.0) * 10
        if lat0 < -80:
            lat0 = -89
        if lat0 + domainHeight > 89:
            lat0 = 89 - domainHeight
        return int(lat0)

    @staticmethod
    def getLon0(lonCenter, domainWidth) -> int:
        # get a domain starting every 10th degree
        lon0 = round((lonCenter - (domainWidth / 2.0)) / 10.0) * 10
        return int(lon0)

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
        self.lat0 = self.getLat0(domainCenterY, res.domainHeight)
        # snap can only cross date-line when both start and position are negative or positive
        self.lon0 = self.getLon0(domainCenterX, res.domainWidth)
        self._set_outputdir(res, self.dir_template, self.lon0, self.lat0, utc)
        self.add_expected_files(lastDateFile[0])

    def _set_outputdir(self, res, template, lon0, lat0, utc):
        self.outputdir = os.path.join(
            res.outputdir, template.format(lon0=lon0, lat0=lat0, utc=utc)
        )
        self._check_create_outputdir(res, utc)

    def _check_create_outputdir(self, res, utc):
        """Create the output-directory and, when already in use, create a temporary one.

        In use is defined by the existence of a 'running' file (no locking since
        not well enough supported across nodes on lustre)
        """
        # try to avoid conflicting processes (not 100% save)
        i = 1
        while os.path.isfile(os.path.join(self.outputdir, "running")):
            self.outputdir = os.path.join(res.outputdir, f"NRPA_TEMP_{utc}_{i}")
            i += 1

        if not os.path.exists(self.outputdir):
            os.makedirs(self.outputdir)

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
                  needs to wait for the proc to finish before calling other methods of this object
        """
        #        if (not self.must_calc()):
        #            return
        pass


if __name__ == "__main__":
    pass
