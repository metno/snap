'''
Created on Aug 04, 2016

@author: heikok
'''

import re
import os
import sys
import math
from collections import OrderedDict
from time import gmtime, strftime
from datetime import datetime, time, date, timedelta

class Resources():
    '''
    Read the resources and combine them
    '''
    ECINPUTDIRS = ["/lustre/storeA/project/metproduction/products/ecmwf/cwf_input/", "/lustre/storeB/project/metproduction/products/ecmwf/cwf_input/"]
    #ECINPUTDIRS = ["/lustre/storeB/users/heikok/Meteorology/ecdis2cwf/"]
    EC_FILE_PATTERN = "NRPA_EUROPE_0_1_{UTC:02d}/meteo{year:04d}{month:02d}{day:02d}_{dayoffset:02d}.nc"
    OUTPUTDIR = "/tmp/test"

    def __init__(self):
        '''
        initialize
        '''
        with open(os.path.join(os.path.dirname(__file__),"resources/startScreenVolc.html"),
                             mode='r', encoding="UTF-8") as fh:
            self.startScreen = fh.read()
        volcBB = {'west': -60,'east': 70,'north': 90,'south': 30}
        self.volcs = self.readVolcanoes(volcBB)
        volcStrings = []
        for tag, site in sorted(self.volcs.items(), key=lambda t: t[1]['NAME'].upper()):
            volcStrings.append('<option value="{tag}">{site} ({lat:.2f},{lon:.2f})</option>\n'.format(tag=tag, site=site['NAME'], lat=site['LATITUDE'], lon=site['LONGITUDE']))
        self.startScreen = re.sub(r'%VOLCANO_OPTIONS%',"".join(volcStrings),self.startScreen)

        volcTypes = []
        for tag, vtype in sorted(self.readVolcanoTypes().items(), key=lambda t:t[0].upper()):
            volcTypes.append('<option value="{tag}">{vtype}: {description}</option>\n'.format(tag=tag, vtype=vtype['TYPE'], description=vtype['DESCRIPTION']))
        self.startScreen = re.sub(r'%VOLCANOTYPE_OPTIONS%',"".join(volcTypes),self.startScreen)

        self.startScreen = re.sub(r'%CURRENTTIME%',strftime("%Y-%m-%d %H:00", gmtime()),self.startScreen)



        ecmodelruns=""
        for run in self.getECRuns():
            ecmodelruns += "<option value=\"{run}\">{run}</option>\n".format(run=run)
        self.startScreen = re.sub(r'%ECMODELRUN%',ecmodelruns,self.startScreen)

    def getOutputDir(self):
        return self.OUTPUTDIR

    def getStartScreen(self):
        '''return the html-code of the start-screen'''
        return self.startScreen


    def readVolcanoes(self, bb={'west': -180., 'east': 180., 'north': 90., 'south': -90.}):
        volcanoes = {}

        fields = 'NUMBER,RN,SN,VN,NAME,LOCATION,STATUS,LATITUDE,NS,VF,LONGITUDE,EW,ELEV,TYPE,TIMEFRAME,ERUPTIONTYPE'.split(',')
        with open(os.path.join(os.path.dirname(__file__),"resources/Mastin_et_al_2009a_table3.csv"),
                        mode='r', encoding="UTF-8") as mh:
            for line in mh:
                line = line.rstrip()
                #NUMBER,RN,SN,VN,NAME,LOCATION,STATUS,LATITUDE,NS,VF,LONGITUDE,EW,ELEV,TYPE,TIMEFRAME,ERUPTION TYPE
                if line == '':
                    continue
                if line[0] == '#':
                    continue
                site = line.split(',')
                if len(site) != len(fields):
                    print("volcano not properly defined: ", line, file=sys.stderr)
                    continue
                volcano = dict(zip(fields, site))
                tag = volcano['NUMBER']
                tag = tag.replace(' ', '_')
                volcano['LATITUDE'] = float(volcano['LATITUDE'])
                if (volcano['NS'] == 'S'):
                    volcano['LATITUDE'] *= -1
                volcano['LONGITUDE'] = float(volcano['LONGITUDE'])
                if (volcano['EW'] == 'W'):
                    volcano['LONGITUDE'] *= -1
                try:
                    volcano['ELEV'] = float(volcano['ELEV'])
                except:
                    volcano['ELEV'] = 0.0
                if (volcano['NAME'] == 'Unnamed'):
                    volcano['NAME'] = "_"

                if ((volcano['LONGITUDE'] >= bb['west']) and (volcano['LONGITUDE'] <= bb['east']) and
                    (volcano['LATITUDE'] >= bb['south']) and (volcano['LATITUDE'] <= bb['north'])):
                    volcanoes[tag] = volcano

        return volcanoes

    def readVolcanoTypes(self):
        vtypes={}
        fields = 'TYPE,x,BASE,H,D,dM/dt,m63,START,END,DESCRIPTION'.split(',')
        with open(os.path.join(os.path.dirname(__file__),"resources/Mastin_et_al_2009b_table3.csv"),
                        mode='r', encoding="UTF-8") as mh:
            for line in mh:
                line = line.rstrip()
                if line == '':
                    continue
                if line[0] == '#':
                    continue
                typedef = line.split(',')
                if len(typedef) != len(fields):
                    print("volcanotype not properly defined: ", line, file=sys.stderr)
                    continue
                vtype = dict(zip(fields, typedef))
                vtype['m63'] = float(vtype['m63'])
                vtype['D'] = float(vtype['D'])
                vtypes[vtype['TYPE']] = vtype
        return vtypes

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

    def _findFileInPathes(self, file, pathes):
        for path in pathes:
            filename = os.path.join(path, file)
            if os.path.isfile(filename):
                return filename
        return None

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
    res = Resources()
    print(res.getStartScreen())
    print(res.getECMeteorologyFiles(datetime.combine(date.today(),time(0)), 48))
    print(res.getECMeteorologyFiles(datetime.combine(date.today(),time(0)), -72))
    runs = res.getECRuns()
    print(runs)
    print(res.getECMeteorologyFiles(datetime.combine(date.today(),time(0)), 48, runs[1]))
    print(res.volcs)
