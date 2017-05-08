'''
Created on Apr 13, 2016

@author: heikok
'''

from collections import OrderedDict
from datetime import datetime, time, date, timedelta
import math
import os
import re
import sys
import time as mtime
from time import gmtime, strftime


class Resources():
    '''
    Read the resources and combine them
    '''
    #OUTPUTDIR = "/disk1/tmp"
    OUTPUTDIR = "/lustre/storeB/project/fou/kl/snap/runs"
    ECINPUTDIRS = ["/lustre/storeA/project/metproduction/products/ecmwf/cwf_input/", "/lustre/storeB/project/metproduction/products/ecmwf/cwf_input/"]
    #ECINPUTDIRS = ["/lustre/storeB/users/heikok/Meteorology/ecdis2cwf/"]
    EC_FILENAME_PATTERN = "meteo{year:04d}{month:02d}{day:02d}_{dayoffset:02d}.nc"
    EC_FILE_PATTERN = os.path.join("NRPA_EUROPE_0_1_{UTC:02d}", EC_FILENAME_PATTERN)
    ECGLOBALINPUTDIRS = ["/lustre/storeA/project/metproduction/products/ecmwf/nc/", "/lustre/storeB/project/metproduction/products/ecmwf/nc/"]
    EC_GLOBAL_PATTERN = "ec_atmo_0_1deg_{year:04d}{month:02d}{day:02d}T{dayoffset:02d}0000Z_3h.nc"

    MEPSINPUTDIRS = ["/lustre/storeA/project/metproduction/products/meps/", "/lustre/storeB/project/metproduction/products/meps/"]
    MEPS25_FILENAME_PATTERN = "meps_full_2_5km_{year:04d}{month:02d}{day:02d}T{UTC:02d}Z.nc"

    def __init__(self):
        '''
        initialize
        '''
        self.ecDomainWidth = 125.
        self.ecDomainHeight = 60.
        self.ecDomainRes = 0.1
        self.ecDefaultDomainStartX = -50.
        self.ecDefaultDomainStartY = 25.

        startScreenFH = open(os.path.join(os.path.dirname(__file__),"resources/startScreen.html"),
                             mode='r', encoding="UTF-8")
        self.startScreen = startScreenFH.read()
        startScreenFH.close()
        plantBB = {'west': -60,'east': 70,'north': 85,'south': 30}
        npps = self.readNPPs(plantBB)
        nppStrings = []
        for tag, site in npps.items():
            nppStrings.append('<option value="{tag}">{site}</option>\n'.format(tag=tag, site=site['site']))
        self.startScreen = re.sub(r'%NPP_OPTIONS%',"".join(nppStrings),self.startScreen)
        self.startScreen = re.sub(r'%CURRENTTIME%',strftime("%Y-%m-%d %H:00", gmtime()),self.startScreen)

        ecmodelruns=""
        for run in self.getECRuns():
            ecmodelruns += "<option value=\"{run}\">{run}</option>\n".format(run=run)
        self.startScreen = re.sub(r'%ECMODELRUN%',ecmodelruns,self.startScreen)

    def getDefaultMetDefinitions(self, metmodel):
        '''get the default meteo-definitions as dict to be used as *dict for getSnapInputMetDefinitions'''
        if metmodel == 'h12' or metmodel == 'hirlam12':
            return {}
        elif (metmodel == 'nrpa_ec_0p1') or (metmodel == 'nrpa_ec_0p1_global'):
            return {"nx": 1+round(self.ecDomainWidth/self.ecDomainRes),
                    "ny": 1+round(self.ecDomainHeight/self.ecDomainRes),
                    "startX": self.ecDefaultDomainStartX,
                    "startY": self.ecDefaultDomainStartY,
                    "dx": self.ecDomainRes,
                    "dy": self.ecDomainRes,
                    }
        elif (metmodel == 'meps_2_5km'):
            return {}

        raise(NotImplementedError("metmodel='{}' not implememented".format(metmodel)))

    def getStartScreen(self):
        return self.startScreen

    def getIconPath(self):
        return os.path.join(os.path.dirname(__file__),"resources/radioMapIcon.png")

    def getIsotopes(self):
        """ return a dictionary of isotope-ids mapping to a dictionary with isotope,type and decay"""
        isotopes = dict()
        with open(os.path.join(os.path.dirname(__file__),"resources/isotope_list.txt"),
                        mode='r', encoding="UTF-8") as isoFH:
            for line in isoFH:
                if not line.strip() is  "":
                    isoId = int(line[0:4])
                    el = line[4:7].strip()
                    iso = line[8:13].strip()
                    isoType= int(line[13:14])
                    decay=float(line[14:])
                    isotopes[isoId] = {'isotope': "{0}{1}".format(el,iso), 'type': isoType, 'decay': decay}
        return isotopes

    def isotopes2snapinput(self, isotopeIds):
        '''Read a list of isotopeIds and return a text-block to be used for a snap.input file, like
COMPONENT= Cs137
RADIOACTIVE.DECAY.ON
HALF.LIFETIME.YEARS= 30
DRY.DEP.ON
WET.DEP.ON
RADIUS.MICROMETER=0.55
DENSITY.G/CM3=2.3
GRAVITY.FIXED.M/S=0.0002
FIELD.IDENTIFICATION=01
'''
        snapinputs = ["***List of components"]
        isotopes = self.getIsotopes()
        fieldId = 0
        for isoId in isotopeIds:
            fieldId += 1
            iso = isotopes[isoId]
            snapinput = "COMPONENT= {}\n".format(iso['isotope'])
            snapinput += "RADIOACTIVE.DECAY.ON\n"
            snapinput += "HALF.LIFETIME.DAYS= {:14.8E}\n".format(math.log(2.)/(iso['decay']*60.*60.*24.))
            if (iso['type'] == 0):
                # Noble gas
                snapinput += """DRY.DEP.OFF
WET.DEP.OFF
GRAVITY.OFF
"""
            elif (iso['type'] == 1):
                # Gas
                snapinput += """DRY.DEP.ON
WET.DEP.ON
RADIUS.MICROMETER=0.05
DENSITY.G/CM3=0.001
GRAVITY.FIXED.M/S=0.00001
"""
            elif (iso['type'] == 2):
                # Aerosol
                snapinput += """DRY.DEP.ON
WET.DEP.ON
RADIUS.MICROMETER=0.55
DENSITY.G/CM3=2.3
GRAVITY.FIXED.M/S=0.0002
"""
            else:
                raise Exception("Error, unknown type '{1}' for isotope '{2}'".format(iso['type'],iso['isotope']))
            snapinput += "FIELD.IDENTIFICATION= {:02d}\n".format(fieldId)
            snapinputs.append(snapinput)

        return "\n".join(snapinputs)

    def getGribWriterConfig(self, isotopes):
        '''Return a dictionary with a xml: xmlconfiguration string and a exracts: output-type and variable-names'''
        allIsos = self.getIsotopes()
        extracts = {'tofa': ['time_of_arrival'],
                    'prec': ['precipitation_amount_acc'],
                    'wetd': [],
                    'depo': [],
                    'dose': [],
                    'conc': []
                    }

        with open(os.path.join(os.path.dirname(__file__),"resources/isotopes_template.xml"),) as isoTemplate:
            isoTemp = isoTemplate.read()

        isoStr = ""
        for isoId in isotopes:
            isoName = allIsos[isoId]['isotope']
            isoStr += isoTemp.format(ISOTOPE=isoName, ID=isoId)
            extracts['conc'].append("{}_concentration".format(isoName))
            extracts['dose'].append("{}_acc_concentration".format(isoName))
            if allIsos[isoId]['type'] > 0:
                extracts['depo'].append("{}_acc_total_deposition".format(isoName))
                extracts['wetd'].append("{}_acc_wet_deposition".format(isoName))

        with open(os.path.join(os.path.dirname(__file__),"resources/cdmGribWriterIsotopesTemplate.xml"),) as xmlTemplate:
            xmlTemp = xmlTemplate.read()

        xmlOut = xmlTemp.format(ISOTOPES=isoStr, GRIB_TEMPLATE_PATH=os.path.join(os.path.dirname(__file__),"resources/template_conc_Am-241.ID_328.grib"))

        return {'extracts': extracts, 'xml': xmlOut, 'ncml': os.path.join(os.path.dirname(__file__),"resources/removeSnapReferenceTime.ncml")}


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
        return OrderedDict(sorted(npps.items(), key=lambda t: t[0].lower()))

    def _getSnapInputTemplate(self, metmodel=None):
        """Read a snap input file without source-term parameters, isotopes (isotopes2snapinput) and eventually without meteorology files.

        Keyword arguments:
        metmodel -- h12, hirlam12 including files (default), or nrpa_ec_0p1 without met-definitions (see getSnapInputMetDefinitions)
        """
        if (metmodel is None) or (metmodel == 'h12'):
            filename = os.path.join(os.path.dirname(__file__),"resources/snap.input.tmpl")
        elif (metmodel == 'nrpa_ec_0p1') or (metmodel == 'nrpa_ec_0p1_global'):
            filename = os.path.join(os.path.dirname(__file__),"resources/snap.input_nrpa_ec_0p1.tmpl")
        elif (metmodel == 'meps_2_5km'):
            filename = os.path.join(os.path.dirname(__file__),"resources/snap.input_meps_2_5km.tmpl")
        else:
            raise(NotImplementedError("metmodel='{}' not implememented".format(metmodel)))

        f = open(filename)
        return f.read()

    def getSnapInputMetDefinitions(self, metmodel, files, nx=0, ny=0, startX=0, startY=0, dx=0, dy=0):
        '''Get the definitions for the metmodel, including met-files and domain (unless default).
        This should be written to the snap.input file, in addition to the source-term. files may be empty for e.g. h12
        '''
        lines = []
        if (metmodel is None) or (metmodel == 'h12'):
            # no setup needed, decoded in snap-template
            pass
        elif (metmodel == 'nrpa_ec_0p1') or (metmodel == 'nrpa_ec_0p1_global'):
            # GRID.GPARAM = 2, -50., 25,.1,.1, 0., 0.
            # GRID.SIZE = 1251,601
            if (nx == 0):
                nx = 1+round(self.ecDomainWidth/self.ecDomainRes)
                ny = 1+round(self.ecDomainHeight/self.ecDomainRes)
                startX = self.ecDefaultDomainStartX
                startY = self.ecDefaultDomainStartY
                dx=self.ecDomainRes
                dy=self.ecDomainRes
            lines.append("GRID.SIZE = {nx},{ny}".format(nx=nx, ny=ny))
            lines.append("GRID.GPARAM = {gtype}, {startX}, {startY}, {dx}, {dy}, 0., 0.".
                         format(gtype=2,
                                startX=startX,
                                startY=startY,
                                dx=dx,
                                dy=dy))
        elif (metmodel == 'meps_2_5km'):
            # no setup needed, decoded in snap-template
            pass
        else:
            raise(NotImplementedError("metmodel='{}' not implememented".format(metmodel)))

        for f in files:
            lines.append("FIELD.INPUT={}".format(f))

        lines.append("")
        lines.append(self._getSnapInputTemplate(metmodel))
        return "\n".join(lines)

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

    def getMEPS25MeteorologyFiles(self, dtime: datetime, run_hours: int, fixed_run="best"):
        """Get available meteorology files for the last few days around dtime and run_hours.

        Keyword arguments:
        dtime -- start time of model run
        run_hours -- run length in hours, possibly negative
        fixed_run -- string of form YYYY-MM-DD_HH giving a specific model-run
        latitude -- float of latitude position
        longitude -- float of longitude position
        """
        relevant_dates = []


        # only best currently implemented
        if (fixed_run == "best"):
            if run_hours < 0:
                start = dtime + timedelta(hours=run_hours)
            else:
                start = dtime

            start -= timedelta(hours=66) # go 66 hours (forecast-length) back

            today = datetime.combine(date.today(), time(0,0,0))
            tomorrow = today + timedelta(days=1)
            days = []
            while (start < tomorrow):
                days.append(start)
                start += timedelta(days=1)
            # loop needs to have latest model runs/hindcast runs last
            for day in days:
                for utc in [0, 6, 12, 18]:
                    file = self.MEPS25_FILENAME_PATTERN.format(UTC=utc, year=day.year, month=day.month, day=day.day)
                    filename = self._findFileInPathes(file, self.MEPSINPUTDIRS)
                    if filename is not None:
                        fmtime = os.stat(filename).st_mtime
                        if ((mtime.time() - fmtime) > (60*10)): # file older than 10min -> no longer under production
                            relevant_dates.append(filename)

        return relevant_dates


    def getECMeteorologyFiles(self, dtime: datetime, run_hours: int, fixed_run="best"):
        """Get available meteorology files for the last few days around dtime and run_hours.

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
    print(Resources().isotopes2snapinput([169, 158, 148]))
    print(Resources().getMEPS25MeteorologyFiles(datetime.combine(date.today(),time(0)), -48))
    print(Resources().getMEPS25MeteorologyFiles(datetime.combine(date.today(),time(0)), -72))
