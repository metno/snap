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
    ECINPUTDIRS = ["/lustre/storeA/project/metproduction/products/ecmwf/cwf_input/", "/lustre/storeB/project/metproduction/products/ecmwf/cwf_input/"];
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

    def getECMeteorologyFiles(self, dtime: datetime, run_hours: int):
        relevant_dates = []
        today = datetime.combine(date.today(), time(0,0,0))
        yesterday = today - timedelta(days=1)

        tomorrow = today + timedelta(days=1)
        # set start 3 hours before earliest date
        if run_hours < 0:
            start = dtime + timedelta(hours=(run_hours-3))
        else:
            start = dtime + timedelta(hours=-3)

        if start >= tomorrow:
            start -= timedelta(hours=72) # go 72 hours (forecast-length) back

        while (start < yesterday):
            for utc in [0, 6, 12, 18]:
                file = self.EC_FILE_PATTERN.format(dayoffset=0, UTC=utc, year=start.year, month=start.month, day=start.day)
                filename = self._findFileInPathes(file, self.ECINPUTDIRS)
                if filename is not None: relevant_dates.append(filename)
            start += timedelta(days=1)

        # today
        while (start < tomorrow):
            for offset in [0,1,2,3]:
                for utc in [0, 6, 12, 18]:
                    file = self.EC_FILE_PATTERN.format(dayoffset=offset, UTC=utc, year=start.year, month=start.month, day=start.day)
                    filename = self._findFileInPathes(file, self.ECINPUTDIRS)
                    if filename is not None: relevant_dates.append(filename)
            start += timedelta(days=1)
        return relevant_dates



if __name__ == "__main__":
    print(Resources().getStartScreen())
    print(Resources().getECMeteorologyFiles(datetime.combine(date.today(),time(0)), 72))
    print(Resources().getECMeteorologyFiles(datetime.combine(date.today(),time(0)), -72))
