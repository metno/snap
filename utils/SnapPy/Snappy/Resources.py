'''
Created on Apr 13, 2016

@author: heikok
'''

import re
import os
from time import gmtime, strftime
from datetime import datetime, time, date, timedelta

class Resources():
    '''
    Read the resources and combine them
    '''
    #OUTPUTDIR = "/disk1/tmp"
    OUTPUTDIR = "/lustre/storeB/project/fou/kl/snap/runs"
    ECINPUTDIRS = ["/lustre/storeA/project/metproduction/products/ecmwf/cwf_input/", "/lustre/storeB/project/metproduction/products/ecmwf/cwf_input/"]
    #ECINPUTDIRS = ["/lustre/storeB/users/heikok/Meteorology/ecdis2cwf/"]
    EC_FILE_PATTERN = "NRPA_EUROPE_0_1_{UTC:02d}/meteo{year:04d}{month:02d}{day:02d}_{dayoffset:02d}.nc"

    def __init__(self):
        '''
        initialize
        '''
        startScreenFH = open(os.path.join(os.path.dirname(__file__),"resources/startScreen.html"),
                             mode='r', encoding="UTF-8")
        self.startScreen = startScreenFH.read()
        startScreenFH.close()
        plantBB = {'west': -60,'east': 70,'north': 90,'south': 30}
        npps = self.readNPPs(plantBB)
        nppStrings = []
        for tag, site in npps.items():
            nppStrings.append('<option value="{tag}">{site}</options>\n'.format(tag=tag, site=site['site']))
        self.startScreen = re.sub(r'%NPP_OPTIONS%',"".join(nppStrings),self.startScreen)
        self.startScreen = re.sub(r'%CURRENTTIME%',strftime("%Y-%m-%d %H:00", gmtime()),self.startScreen)

        ecmodelruns=""
        for run in self.getECRuns():
            ecmodelruns += "<option value=\"{run}\">{run}</option>\n".format(run=run)
        self.startScreen = re.sub(r'%ECMODELRUN%',ecmodelruns,self.startScreen)

    def getStartScreen(self):
        return self.startScreen

    def getIconPath(self):
        return os.path.join(os.path.dirname(__file__),"resources/radioMapIcon.png")

    def readNPPs(self, bb={'west': -180., 'east': 180., 'north': 90., 'south': -90.}):
        nppsFile = open(os.path.join(os.path.dirname(__file__),"resources/npps.csv"),
                        mode='r', encoding="UTF-8")
        # skip header
        nppsFile.readline()
        # read rest
        npps = {}
        for line in nppsFile:
            # [site, country, long, lat, status)
            site = line.split('|')
            if len(site) < 4:
                print("NPP not properly defined: ", line, file=sys.stderr)
            tag = site[0]
            tag = tag.replace(' ', '_')
            if ((float(site[2]) >= bb['west']) and (float(site[2]) <= bb['east']) and
                (float(site[3]) >= bb['south']) and (float(site[3]) <= bb['north'])):
                npps[tag] = {'site': site[0], 'CC': site[1], 'lon': float(site[2]), 'lat': float(site[3]), 'status': site[4]}
        nppsFile.close()
        return npps

    def getSnapInputTemplate(self, metmodel=None):
        """Read a snap input file without source-term parameters and eventually without meteorology files.

        Keyword arguments:
        metmodel -- h12, hirlam12 including files (default), or nrpa_ec_0p1 without met-files
        """
        if (metmodel is None) or (metmodel == 'h12'):
            filename = os.path.join(os.path.dirname(__file__),"resources/snap.input.tmpl")
        else:
            filename = os.path.join(os.path.dirname(__file__),"resources/snap.input_{}.tmpl".format(metmodel))
        f = open(filename)
        return f.read()

    def getSendmailScript(self):
        filename = os.path.join(os.path.dirname(__file__),"resources/sendmail.sh")
        return filename

    def getBSnapInputFile(self):
        filename = os.path.join(os.path.dirname(__file__),"resources/snap.in")
        return filename

    def getSnapOutputDir(self):
        return self.OUTPUTDIR

    def _findFileInPathes(self, file, pathes):
        for path in pathes:
            filename = os.path.join(path, file)
            if os.path.isfile(filename):
                return filename
        return None

    def getECRuns(self):
        """Find ec-runs with at least 2 days of forecast"""
        relevant = []
        today = datetime.combine(date.today(), time(0,0,0))
        start = today - timedelta(hours=72)
        tomorrow = today + timedelta(days=1)
        while (start < tomorrow):
            for utc in [0, 6, 12, 18]:
                file = self.EC_FILE_PATTERN.format(dayoffset=2, UTC=utc, year=start.year, month=start.month, day=start.day)
                filename = self._findFileInPathes(file, self.ECINPUTDIRS)
                if filename is not None:
                    relevant.append("{year:04d}-{month:02d}-{day:02d}_{UTC:02d}".format(UTC=utc, year=start.year, month=start.month, day=start.day))
            start += timedelta(days=1)
        return relevant

    def getECMeteorologyFiles(self, dtime: datetime, run_hours: int, fixed_run="best"):
        """Get available meteorology files for the last few days around dtime and run_hours

        Keyword arguments:
        dtime -- start time of model run
        run_hours -- run length in hours, possibly negative
        fixed_run -- string of form YYYY-MM-DD_HH giving a specific model-run
        """
        relevant_dates = []

        if (fixed_run == "best"):
            if run_hours < 0:
                start = dtime + timedelta(hours=run_hours)
            else:
                start = dtime

            start -= timedelta(hours=72) # go 72 hours (forecast-length) back

            today = datetime.combine(date.today(), time(0,0,0))
            tomorrow = today + timedelta(days=1)
            days = []
            while (start < tomorrow):
                days.append(start)
                start += timedelta(days=1)
            # loop needs to have latest model runs/hindcast runs last
            for offset in [3,2,1,0]:
                for day in days:
                    for utc in [0, 6, 12, 18]:
                        file = self.EC_FILE_PATTERN.format(dayoffset=offset, UTC=utc, year=day.year, month=day.month, day=day.day)
                        filename = self._findFileInPathes(file, self.ECINPUTDIRS)
                        if filename is not None: relevant_dates.append(filename)
        else:
            match = re.match(r'(\d{4})-(\d{2})-(\d{2})_(\d{2})', fixed_run)
            if (match):
                (year,month,day,utc) = tuple(map(int, list(match.group(1,2,3,4))))
                for offset in [0,1,2,3]:
                    file = self.EC_FILE_PATTERN.format(dayoffset=offset, UTC=utc, year=year, month=month, day=day)
                    filename = self._findFileInPathes(file, self.ECINPUTDIRS)
                    if filename is not None: relevant_dates.append(filename)

        return relevant_dates



if __name__ == "__main__":
    print(Resources().getStartScreen())
    print(Resources().getECMeteorologyFiles(datetime.combine(date.today(),time(0)), 48))
    print(Resources().getECMeteorologyFiles(datetime.combine(date.today(),time(0)), -72))
    runs = Resources().getECRuns()
    print(runs)
    print(Resources().getECMeteorologyFiles(datetime.combine(date.today(),time(0)), 48, runs[1]))
