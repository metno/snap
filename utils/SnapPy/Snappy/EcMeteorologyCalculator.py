'''
Created on Oct 24, 2016

@author: heikok
'''

from datetime import datetime, timedelta
from glob import iglob
import math
import os
import subprocess
import time

from Snappy.Resources import Resources


class ECDataNotAvailableException(Exception):
    def __init__(self, value):
        '''exception having some kind of documention in args[0]'''
        self.parameter = value
    def __str__(self):
        return repr(self.parameter)


class EcMeteorologyCalculator():
    '''Calculate ec-meteorology'''
    @staticmethod
    def findECGlobalData(dtime: datetime):
        '''Static method to find the closest global ec dataset earlier than dtime.

        Args:
            dtime: datetime object with a start-time, which should be included in the dataset


        Returns:
            A tuple with referencetime and filename

        Raises:
            ECDataNotAvailableException: no data for the dtime can be found
        '''
        #should eventually use productstatus.met.no?

        timesFiles = [] # tuples with datetime, file
        for inDir in Resources.ECGLOBALINPUTDIRS:
            for iFile in iglob(os.path.join(inDir, "ec_atmo_0_1deg_????????T??????Z_3h.nc")):
                statinfo = os.stat(iFile)
                if statinfo.st_mtime < (time.time() - 15*60): # file hasn't been changed in 15 minutes
                    date_string = iFile[-22:-7]
                    dateFile = datetime.strptime(date_string, "%Y%m%dT%H%M%S")
                    timesFiles.append((dateFile, iFile))
        lastTimeFile = (None, None)
        for timeFile in sorted(timesFiles, key=lambda t: t[0]):
            if (timeFile[0] <= dtime):
                lastTimeFile = timeFile
            else:
                break
        if (lastTimeFile[0] is None):
            raise ECDataNotAvailableException("no input data in {dirs} for {time}: ".format(dirs=Resources.ECGLOBALINPUTDIRS, time=dtime))
        return lastTimeFile


    def __init__(self, res: Resources, dtime: datetime, domainCenterX, domainCenterY):
        '''Calculate the ec-meteorology unless it exists

        Args:
            res: resources object
            dtime: datetime object containing the earliest time expected
            domainCenterX: number where the domain should start (longitude), will be rounded
            domainCenterY: number where the domain should start (latitude), will be rounded
            proc: A QProcess, which will be used to run a longer process in the background.
                  STDERR/STDOUT and signal-handler should be set. If proc is None, the
                  subprocess will be run in the current-process. If proc is set, the caller
                  needs to wait for the proc to finish before calling other methods of this object
        Raises:
            ECDataNotAvailableException: no data for the dtime can be found
        '''
        self.proc = None # storage for background-process
        self.res = res

        lastDateFile = EcMeteorologyCalculator.findECGlobalData(dtime-timedelta(hours=3)) # no useful data in EC the first 3 hours
        self.date = lastDateFile[0]
        self.globalfile = lastDateFile[1]
        utc = lastDateFile[0].hour
        # domain every 10th degree
        lat0 = math.floor((domainCenterY-(res.ecDomainHeight/2.))/10.)*10
        if (lat0 < -80): lat0 = -89
        if (lat0+res.ecDomainHeight > 89): lat0 = 89 - res.ecDomainHeight
        # snap cannot cross date-line
        lon0 = math.floor((domainCenterX-(res.ecDomainWidth/2.))/10.)*10
        if (lon0 < -180): lon0 = -180
        if (lon0+res.ecDomainWidth > 180): lon0 = 180 - res.ecDomainWidth
        self.lat0 = int(lat0)
        self.lon0 = int(lon0)
        self.outputdir = os.path.join(res.OUTPUTDIR, "NRPA_LON{x}_LAT{y}_{utc:02d}".format(x=self.lon0, y=self.lat0, utc=utc))

        # try to avoid conflicting processes (not 100% save)
        i = 1
        while (os.path.isfile(os.path.join(self.outputdir, "running"))):
            self.outputdir =  os.path.join(res.OUTPUTDIR, "NRPA_TEMP_{utc:02d}_{i}".format(utc=utc, i=i))
            i+=1

        if (not os.path.exists(self.outputdir)):
            os.makedirs(self.outputdir)

        self.files = []
        self.optFiles = []
        for i in (0,1,2):
            self.files.append(os.path.join(self.outputdir,
                                           res.EC_FILENAME_PATTERN.format(year=lastDateFile[0].year,
                                                                          month=lastDateFile[0].month,
                                                                          day=lastDateFile[0].day,
                                                                          dayoffset=i)))
        for i in (3,4,5):
            self.optFiles.append(os.path.join(self.outputdir,
                                           res.EC_FILENAME_PATTERN.format(year=lastDateFile[0].year,
                                                                          month=lastDateFile[0].month,
                                                                          day=lastDateFile[0].day,
                                                                          dayoffset=i)))
        return

    def get_meteorology_files(self):
        '''return the meteorology files'''
        if self.must_calc():
            raise ECDataNotAvailableException("unable to create ec-data for {year}-{month}-{day} in {dir}".
                                              format(year=self.date.year,
                                                     month=self.date.month,
                                                     day=self.date.day,
                                                     dir=self.outputdir))
        files = self.files
        for f in self.optFiles:
            if (os.path.isfile(f)):
                files.append(f)
        return files

    def get_grid_startX_Y(self):
        '''return a tuple with x0 and y0'''
        return (self.lon0, self.lat0)

    def must_calc(self):
        '''check if calculation is required or has been done earlier'''
        recalc = False
        for f in self.files:
            if (not os.path.isfile(f)):
                recalc = True
        return recalc

    def calc(self, proc):
        '''run the calculation of ec-data if required'''
        if (not self.must_calc()):
            return
#        if 'MODULESHOME' not in os.environ:
#            raise ECDataNotAvailableException("unable to load module")

        precommand = '''#! /bin/bash
. /usr/share/modules/init/bash
module load ecdis4cwf/1.2.0
export OMP_NUM_THREADS=1
export DATE='{year:04d}{month:02d}{day:02d}'
export UTC='{utc}'

cd {outdir}
touch {outdir}/running
WORKDIR={outdir}/work
mkdir $WORKDIR
ECDIS_PARALLEL=0 NREC_DAY_MIN=2 NDAYS_MAX=3 DOMAIN=VARIABLE LON_DEF={lon0}.,{dlon},{nx} LAT_DEF={lat0}.,{dlat},{ny} ECDIS={globalfile} OUTDIR={outdir} ECDIS_TMPDIR=$WORKDIR $ECDIS_MODULE_PATH/ecdis4cwf.sh
rm {outdir}/running
'''
        command = precommand.format(year=self.date.year,
                                    month=self.date.month,
                                    day=self.date.day,
                                    utc=self.date.hour,
                                    outdir=self.outputdir,
                                    lon0=self.lon0,
                                    lat0=self.lat0,
                                    dlon=self.res.ecDomainRes,
                                    dlat=self.res.ecDomainRes,
                                    nx=round(self.res.ecDomainWidth/self.res.ecDomainRes)+1,
                                    ny=round(self.res.ecDomainHeight/self.res.ecDomainRes)+1,
                                    globalfile=self.globalfile,
                                    )
        scriptFile = os.path.join(self.outputdir, "command.sh")
        with open(scriptFile, 'w') as script:
            script.write(command)

        if proc is None:
            subprocess.run(['/bin/bash', scriptFile])
        else:
            self.proc = proc # make sure proc lives long enough
            proc.start('/bin/bash', [scriptFile])

        return

if __name__ == "__main__":
    print(EcMeteorologyCalculator.findECGlobalData(datetime.strptime("2016-10-24T00", "%Y-%m-%dT%H")))
    try:
        EcMeteorologyCalculator.findECGlobalData(datetime.strptime("2010-10-24T00", "%Y-%m-%dT%H"))
    except ECDataNotAvailableException as e:
        print(e.args[0])
#    print(EcMeteorologyCalculator(Resources(), datetime.strptime("2016-10-24T00", "%Y-%m-%dT%H"), 63, 42, None))
    ecmet = EcMeteorologyCalculator(Resources(), datetime.strptime("2016-10-27T00", "%Y-%m-%dT%H"), -159, 20) # hawaii
    print("recalc: ", ecmet.must_calc())
