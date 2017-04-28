'''
Created on Aug 04, 2016

@author: heikok
'''

from datetime import datetime, time, date, timedelta
import math
import os
import re
import sys
from time import gmtime, strftime


class Resources():
    '''
    Read the resources and combine them
    '''
    HPC = {"vilje": {'RUNDIR': '/prod/forecast/run/eemep/single_run/'},
           "frost": {'RUNDIR': '/home/metno_op/run/eemep/single_run/'},
           "alvin": {'RUNDIR': '/home/metno_op/run/eemep/single_run/'}}
    ECINPUTDIRS = ["/lustre/storeA/project/metproduction/products/ecmwf/cwf_input/", "/lustre/storeB/project/metproduction/products/ecmwf/cwf_input/"]
    ECVLEVELS = "Vertical_levels48.txt"
    #ECINPUTDIRS = ["/lustre/storeB/users/heikok/Meteorology/ecdis2cwf/"]
    EC_FILE_PATTERN = "NRPA_EUROPE_0_1_{UTC:02d}/meteo{year:04d}{month:02d}{day:02d}_{dayoffset:02d}.nc"
    OUTPUTDIR = "/lustre/storeB/project/fou/kl/eva/eemep/runs/"
    #OUTPUTDIR = "/tmp/test"

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

    def get_job_script(self, hpcName):
        '''get the contents of the job-script for a certain hpc-machine'''
        with open(os.path.join(os.path.dirname(__file__),"resources/job_script_{}.job".format(hpcName))) as fh:
            job = fh.read()
        return job


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

    def getVerticalLevelDefinition(self):
        """Get the definition for the vertical levels"""
        with open(os.path.join(os.path.dirname(__file__),"resources",self.ECVLEVELS),
                        mode='r') as fh:
            vlevels = fh.read()
        return vlevels

    def getECMeteorologyFiles(self, dtime: datetime, run_hours: int, fixed_run="best"):
        """Get available meteorology files starting with date of dtime, only full days

        Keyword arguments:
        dtime -- start time of model run
        run_hours -- run length in hours, possibly negative
        fixed_run -- string of form YYYY-MM-DD_HH giving a specific model-run

        Returns: list with files for day 0 to day+run_hours, TODO: currently only UTC=00 run supported
        """
        dates = []

        # this will only find the 00 run, since eemep needs to start at 00
        if (fixed_run == "best"):
            # need to have latest model runs from dtime up to run_hours in the future
            # find the meteo of the next days days
            for d in range(0,1+math.ceil(float(run_hours)/24.)):
                startday = dtime + timedelta(days=d)
                relevant_dates = []
                for offset in range(0,3):
                    # lowest offset is best, but earlier forecasts will do
                    curday = startday + timedelta(days=-1*offset)
                    for utc in [0]: # only utc 0
                        file = self.EC_FILE_PATTERN.format(dayoffset=offset, UTC=utc, year=curday.year, month=curday.month, day=curday.day)
                        filename = self._findFileInPathes(file, self.ECINPUTDIRS)
                        if filename is not None: relevant_dates.append(filename)
                if (len(relevant_dates) == 0):
                    return dates # longest continuous series
                else:
                    dates.append(relevant_dates[0])
        else:
            startday = datetime.strptime(fixed_run, "%Y-%m-%d_%H")
            assert isinstance(startday, datetime), "getECMeteorology: fixed_run must be 'best' or YYYY-MM-DD_HH"
            for offset in range(0,math.ceil(run_hours/24.)):
                file = self.EC_FILE_PATTERN.format(dayoffset=offset, UTC=startday.hour, year=startday.year, month=startday.month, day=startday.day)
                filename = self._findFileInPathes(file, self.ECINPUTDIRS)
                if filename is None:
                    return dates # longest continuous series
                else:
                    dates.append(filename)

        return dates



if __name__ == "__main__":
    res = Resources()
    print(res.getStartScreen())
    print(res.getECMeteorologyFiles(datetime.combine(date.today(),time(0)), 48))
    print(res.getECMeteorologyFiles(datetime.combine(date.today()-timedelta(days=1),time(0)), 48))
    runs = res.getECRuns()
    print(runs)
    print(res.getECMeteorologyFiles(datetime.combine(date.today(),time(0)), 48, runs[1]))
    print(res.volcs)
