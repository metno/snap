# SNAP: Servere Nuclear Accident Programme
# Copyright (C) 1992-2020   Norwegian Meteorological Institute
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
Created on Apr 13, 2016

@author: heikok
"""

from collections import OrderedDict
from datetime import datetime, time, date, timedelta
import enum
import math
import os
import re
import sys
import time as mtime
from time import gmtime, strftime

from Snappy.ResourcesCommon import ResourcesCommon
from Snappy import read_dosecoefficients_icrp


@enum.unique
class MetModel(enum.Enum):
    Icon0p25Global = "icon_0p25_global"
    Meps2p5 = "meps_2_5km"
    NrpaEC0p1 = "nrpa_ec_0p1"
    NrpaEC0p1Global = "nrpa_ec_0p1_global"
    GfsGribFilter = "gfs_grib_filter_fimex"

    def __eq__(self, other):
        return self.value == str(other)

    def __hash__(self):
        return self.value.__hash__()


class Resources(ResourcesCommon):
    """
    Read the resources and combine them
    """

    # OUTPUTDIR = "/disk1/tmp"
    _OUTPUTDIR = "{LUSTREDIR}/project/fou/kl/snap/runs"
    _ECINPUTDIRS = ["{LUSTREDIR}/project/metproduction/products/cwf-input/"]
    # ECINPUTDIRS = ["/lustre/storeB/users/heikok/Meteorology/ecdis2cwf/"]
    EC_FILENAME_PATTERN = "meteo{year:04d}{month:02d}{day:02d}_{dayoffset:02d}.nc"
    EC_FILE_PATTERN = os.path.join("NRPA_EUROPE_0_1_{UTC:02d}", EC_FILENAME_PATTERN)
    EC_GLOBAL_PATTERN = (
        "ec_atmo_0_1deg_{year:04d}{month:02d}{day:02d}T{dayoffset:02d}0000Z_3h.nc"
    )

    _MET_GLOBAL_INPUTDIRS = {
        MetModel.NrpaEC0p1Global: [
            "{LUSTREDIR}/project/metproduction/products/ecmwf/nc/"
        ],
        MetModel.Icon0p25Global: ["{LUSTREDIR}/project/metproduction/products/icon/"],
    }

    _MET_INPUTDIRS = {
        MetModel.Meps2p5: [
            "{LUSTREDIR}/immutable/archive/projects/metproduction/MEPS/"
        ],
        MetModel.GfsGribFilter: ["/disk1/tmp/"],
    }
    MET_FILENAME_PATTERN = {
        MetModel.Meps2p5: "{year:04d}/{month:02d}/{day:02d}/meps_det_2_5km_{year:04d}{month:02d}{day:02d}T{UTC:02d}Z.nc",
        MetModel.Icon0p25Global: "icon_{year:04d}{month:02d}{day:02d}T{UTC:02d}Z.nc",
        MetModel.GfsGribFilter: "gfs_0p25deg_{year:04d}{month:02d}{day:02d}T{UTC:02d}Z.nc",
    }

    def __init__(self):
        """
        initialize
        """
        self.directory = os.path.join(os.path.dirname(__file__), "resources")
        self.ecDomainWidth = 125.0
        self.ecDomainHeight = 60.0
        self.ecDomainRes = 0.1
        self.ecDefaultDomainStartX = -50.0
        self.ecDefaultDomainStartY = 25.0

        startScreenFH = open(
            os.path.join(self.directory, "startScreen.html"), mode="r", encoding="UTF-8"
        )
        self.startScreen = startScreenFH.read()
        startScreenFH.close()
        plantBB = {"west": -60, "east": 70, "north": 85, "south": 30}
        npps = self.readNPPs(plantBB)
        npps.update(self.readRadnett())
        nppStrings = []
        for tag, site in npps.items():
            nppStrings.append(
                '<option value="{tag}">{site}</option>\n'.format(
                    tag=tag, site=site["site"]
                )
            )
        self.startScreen = re.sub(
            r"%NPP_OPTIONS%", "".join(nppStrings), self.startScreen
        )
        self.startScreen = re.sub(
            r"%CURRENTTIME%", strftime("%Y-%m-%d %H:00", gmtime()), self.startScreen
        )

        ecmodelruns = ""
        for run in self.getECRuns():
            ecmodelruns += '<option value="{run}">{run}</option>\n'.format(run=run)
        self.startScreen = re.sub(r"%ECMODELRUN%", ecmodelruns, self.startScreen)

    def getDefaultMetDefinitions(self, metmodel):
        """get the default meteo-definitions as dict to be used as *dict for getSnapInputMetDefinitions"""
        if (metmodel == MetModel.NrpaEC0p1) or (metmodel == MetModel.NrpaEC0p1Global):
            return {
                "nx": 1 + round(self.ecDomainWidth / self.ecDomainRes),
                "ny": 1 + round(self.ecDomainHeight / self.ecDomainRes),
                "startX": self.ecDefaultDomainStartX,
                "startY": self.ecDefaultDomainStartY,
                "dx": self.ecDomainRes,
                "dy": self.ecDomainRes,
            }
        elif metmodel == MetModel.Meps2p5:
            return {}
        elif metmodel == MetModel.Icon0p25Global:
            return {}
        elif metmodel == MetModel.GfsGribFilter:
            return {}

        raise (NotImplementedError("metmodel='{}' not implememented".format(metmodel)))

    def getStartScreen(self):
        return self.startScreen

    def getStartScreenInverse(self):
        with open(
            os.path.join(self.directory, "startScreenInverse.html"),
            mode="r",
            encoding="UTF-8",
        ) as sfh:
            startScreenInverse = sfh.read()
        return startScreenInverse

    def getIconPath(self):
        return os.path.join(self.directory, "radioMapIcon.png")

    def getIsotopes(self):
        """ return a dictionary of isotope-ids mapping to a dictionary with isotope,type and decay"""
        isotopes = dict()
        with open(
            os.path.join(self.directory, "isotope_list.txt"), mode="r", encoding="UTF-8"
        ) as isoFH:
            for line in isoFH:
                if line.strip() != "":
                    isoId = int(line[0:4])
                    el = line[4:7].strip()
                    iso = line[8:13].strip()
                    isoType = int(line[13:14])
                    decay = float(line[14:])
                    isotopes[isoId] = {
                        "isotope": "{0}{1}".format(el, iso),
                        "type": isoType,
                        "decay": decay,
                    }
        return isotopes

    def isotopes2snapinput(self, isotopeIds, add_DPUI=False):
        """Read a list of isotopeIds and return a text-block to be used for a snap.input file, like
COMPONENT= Cs137
RADIOACTIVE.DECAY.ON
HALF.LIFETIME.YEARS= 30
DRY.DEP.ON
WET.DEP.ON
RADIUS.MICROMETER=0.55
DENSITY.G/CM3=2.3
GRAVITY.FIXED.M/S=0.0002
FIELD.IDENTIFICATION=01
"""
        if add_DPUI:
            dosecoeff = self.getDoseCoefficients()
        else:
            dosecoeff = None
        snapinputs = ["***List of components"]
        isotopes = self.getIsotopes()
        fieldId = 0
        for isoId in isotopeIds:
            fieldId += 1
            DPUI = None
            iso = isotopes[isoId]
            snapinput = "COMPONENT= {}\n".format(iso["isotope"])
            snapinput += "RADIOACTIVE.DECAY.ON\n"
            snapinput += "HALF.LIFETIME.DAYS= {:14.8E}\n".format(
                math.log(2.0) / (iso["decay"] * 60.0 * 60.0 * 24.0)
            )
            if iso["type"] == 0:
                # Noble gas
                snapinput += """DRY.DEP.OFF
WET.DEP.OFF
GRAVITY.OFF
"""
                if dosecoeff is not None:
                    DPUI = dosecoeff.DPUI(iso["isotope"], "noble")
            elif iso["type"] == 1:
                # Gas
                snapinput += """DRY.DEP.ON
WET.DEP.ON
RADIUS.MICROMETER=0.05
DENSITY.G/CM3=0.001
GRAVITY.FIXED.M/S=0.00001
"""
                if dosecoeff is not None:
                    DPUI = dosecoeff.DPUI(iso["isotope"], "gas")
            elif iso["type"] == 2:
                # Aerosol
                snapinput += """DRY.DEP.ON
WET.DEP.ON
RADIUS.MICROMETER=0.55
DENSITY.G/CM3=2.3
GRAVITY.FIXED.M/S=0.0002
"""
                if dosecoeff is not None:
                    DPUI = dosecoeff.DPUI(iso["isotope"], "particulate")
            else:
                raise Exception(
                    "Error, unknown type '{1}' for isotope '{2}'".format(
                        iso["type"], iso["isotope"]
                    )
                )
            print("Isotope: ", iso["isotope"])
            print("DPUI: ", DPUI)
            if DPUI is not None and DPUI >= 0.0:
                snapinput += f"DPUI.Sv_per_Bq_M3 = {DPUI}\n"
            snapinput += "FIELD.IDENTIFICATION= {:02d}\n".format(fieldId)
            snapinputs.append(snapinput)

        return "\n".join(snapinputs)

    def getGribWriterConfig(self, isotopes):
        """Return a dictionary with a xml: xml-configuration string and a exracts: output-type and variable-names"""
        allIsos = self.getIsotopes()
        extracts = {
            "tofa": ["time_of_arrival"],
            "prec": ["lwe_precipitation_rate"],
            "wetd": [],
            "depo": [],
            "dose": [],
            "conc": [],
        }

        with open(os.path.join(self.directory, "isotopes_template.xml")) as isoTemplate:
            isoTemp = isoTemplate.read()

        isoStr = ""
        for isoId in isotopes:
            isoName = allIsos[isoId]["isotope"]
            isoStr += isoTemp.format(ISOTOPE=isoName, ID=isoId)
            extracts["conc"].append("{}_concentration".format(isoName))
            extracts["dose"].append("{}_acc_concentration".format(isoName))
            if allIsos[isoId]["type"] > 0:
                extracts["depo"].append("{}_acc_total_deposition".format(isoName))
                extracts["wetd"].append("{}_acc_wet_deposition".format(isoName))

        with open(
            os.path.join(self.directory, "cdmGribWriterIsotopesTemplate.xml")
        ) as xmlTemplate:
            xmlTemp = xmlTemplate.read()

        xmlOut = xmlTemp.format(
            ISOTOPES=isoStr,
            GRIB_TEMPLATE_PATH=os.path.join(
                self.directory, "template_conc_Am-241.ID_328.grib"
            ),
        )

        return {
            "extracts": extracts,
            "xml": xmlOut,
            "ncml": os.path.join(self.directory, "removeSnapReferenceTime.ncml"),
        }

    def readNPPs(
        self, bb={"west": -180.0, "east": 180.0, "north": 90.0, "south": -90.0}
    ):
        nppsFile = open(
            os.path.join(self.directory, "npps.csv"), mode="r", encoding="UTF-8"
        )
        # skip header
        nppsFile.readline()
        # read rest
        npps = {}
        for line in nppsFile:
            # [site, country, long, lat, status)
            site = line.split("|")
            if len(site) < 4:
                print("NPP not properly defined: ", line, file=sys.stderr)
            tag = site[0]
            tag = tag.replace(" ", "_")
            if (
                (float(site[2]) >= bb["west"])
                and (float(site[2]) <= bb["east"])
                and (float(site[3]) >= bb["south"])
                and (float(site[3]) <= bb["north"])
            ):
                npps[tag] = {
                    "site": site[0],
                    "CC": site[1],
                    "lon": float(site[2]),
                    "lat": float(site[3]),
                    "status": site[4],
                }
        nppsFile.close()
        return OrderedDict(sorted(npps.items(), key=lambda t: t[0].lower()))

    def readRadnett(self,):
        stations = OrderedDict()
        with open(
            os.path.join(os.path.dirname(__file__), "resources/radnett.csv"),
            mode="r",
            encoding="UTF-8",
        ) as f:
            degree_minute_regex = re.compile("([0-9]+)°\s([0-9]+)’\s[NØ]")
            for line in f:
                if line.startswith("#"):
                    continue
                station, position = (x.strip() for x in line.split("|"))

                latitude, longitude = (x.strip() for x in position.split(","))
                m = degree_minute_regex.match(latitude)
                latitude = int(m[1]) + int(m[2]) / 60
                m = degree_minute_regex.match(longitude)
                longitude = int(m[1]) + int(m[2]) / 60

                tag = "RADNETT:" + station.replace(" ", "_")
                tag = tag.encode("ascii", "ignore").decode("ascii")

                stations[tag] = {
                    "site": f"RADNETT: {station}",
                    "lon": longitude,
                    "lat": latitude,
                }

        return stations

    def _getSnapInputTemplate(self, metmodel=None):
        """Read a snap input file without source-term parameters, isotopes (isotopes2snapinput) and eventually without meteorology files.

        Keyword arguments:
        metmodel
        """
        if (metmodel == MetModel.NrpaEC0p1) or (metmodel == MetModel.NrpaEC0p1Global):
            filename = os.path.join(self.directory, "snap.input_nrpa_ec_0p1.tmpl")
        elif metmodel == MetModel.Meps2p5:
            filename = os.path.join(self.directory, "snap.input_meps_2_5km.tmpl")
        elif metmodel == MetModel.Icon0p25Global:
            filename = os.path.join(self.directory, "snap.input_icon_0p25.tmpl")
        elif metmodel == MetModel.GfsGribFilter:
            filename = os.path.join(self.directory, "snap.input_gfs_grib_filter.tmpl")
        else:
            raise (
                NotImplementedError("metmodel='{}' not implememented".format(metmodel))
            )

        f = open(filename)
        return f.read()

    def getSnapInputMetDefinitions(
        self, metmodel, files, nx=0, ny=0, startX=0, startY=0, dx=0, dy=0
    ):
        """Get the definitions for the metmodel, including met-files and domain (unless default).
        This should be written to the snap.input file, in addition to the source-term. files may be empty
        """
        lines = []
        if metmodel == MetModel.NrpaEC0p1:
            # no setup needed, autdetection in snap
            pass
        elif metmodel == MetModel.NrpaEC0p1Global:
            # no setup needed, autdetection in snap
            pass
        elif metmodel == MetModel.Meps2p5:
            # no setup needed, autdetection in snap
            pass
        elif metmodel == MetModel.Icon0p25Global:
            # no setup needed, autdetection in snap
            pass
        elif metmodel == MetModel.GfsGribFilter:
            # no setup needed, autdetection in snap
            pass
        else:
            raise (
                NotImplementedError("metmodel='{}' not implememented".format(metmodel))
            )

        for f in files:
            lines.append("FIELD.INPUT={}".format(f))

        lines.append("")
        lines.append(self._getSnapInputTemplate(metmodel))
        return "\n".join(lines)

    def getSendmailScript(self):
        filename = os.path.join(self.directory, "sendmail.sh")
        return filename

    def getBSnapInputFile(self):
        filename = os.path.join(self.directory, "snap.in")
        return filename

    def getSnapOutputDir(self):
        return self._OUTPUTDIR.format(LUSTREDIR=self.getLustreDir())

    def _findFileInPathes(self, file, pathes):
        for path in pathes:
            filename = os.path.join(path, file)
            if os.path.isfile(filename):
                return filename
        return None

    def _lustreTemplateDirs(self, dirs):
        return [x.format(LUSTREDIR=self.getLustreDir()) for x in dirs]

    def getMetGlobalInputDirs(self, metmodel):
        return self._lustreTemplateDirs(self._MET_GLOBAL_INPUTDIRS[metmodel])

    def getMetInputDirs(self, metmodel):
        return self._lustreTemplateDirs(self._MET_INPUTDIRS[metmodel])

    def getECInputDirs(self):
        return self._lustreTemplateDirs(self._ECINPUTDIRS)

    def getECRuns(self):
        """Find ec-runs with at least 2 days of forecast"""
        relevant = []
        today = datetime.combine(date.today(), time(0, 0, 0))
        start = today - timedelta(hours=72)
        tomorrow = today + timedelta(days=1)
        while start < tomorrow:
            for utc in [0, 6, 12, 18]:
                file = self.EC_FILE_PATTERN.format(
                    dayoffset=2,
                    UTC=utc,
                    year=start.year,
                    month=start.month,
                    day=start.day,
                )
                filename = self._findFileInPathes(file, self.getECInputDirs())
                if filename is not None:
                    relevant.append(
                        "{year:04d}-{month:02d}-{day:02d}_{UTC:02d}".format(
                            UTC=utc, year=start.year, month=start.month, day=start.day
                        )
                    )
            start += timedelta(days=1)
        return relevant

    def getMEPS25MeteorologyFiles(
        self, dtime: datetime, run_hours: int, fixed_run="best"
    ):
        return self.getMeteorologyFiles(MetModel.Meps2p5, dtime, run_hours, fixed_run)

    def getMeteorologyFiles(
        self, metmodel, dtime: datetime, run_hours: int, fixed_run="best"
    ):
        """Get available meteorology files for the last few days around dtime and run_hours.

        Keyword arguments:
        dtime -- start time of model run
        run_hours -- run length in hours, possibly negative
        fixed_run -- string of form YYYY-MM-DD_HH giving a specific model-run
        latitude -- float of latitude position
        longitude -- float of longitude position
        """
        relevant_dates = []

        if metmodel not in self.MET_FILENAME_PATTERN:
            raise NotImplementedError(
                "metmodel='{}' not implememented for meteorology".format(metmodel)
            )
        if metmodel not in self._MET_INPUTDIRS:
            raise NotImplementedError(
                "metmodel='{}' not implememented for meteorology".format(metmodel)
            )

        # only best currently implemented
        if fixed_run == "best":
            if run_hours < 0:
                start = dtime + timedelta(hours=run_hours)
            else:
                start = dtime

            start -= timedelta(hours=66)  # go 66 hours (forecast-length) back
            last = start + timedelta(
                days=24
            )  # offer max 21days (24days - 66hours) in archive
            today = datetime.combine(date.today(), time(0, 0, 0))
            tomorrow = today + timedelta(days=1)
            if tomorrow < last:
                last = tomorrow
            days = []
            while start < last:
                days.append(start)
                start += timedelta(days=1)
            # loop needs to have latest model runs/hindcast runs last
            for day in days:
                for utc in [0, 6, 12, 18]:
                    file = self.MET_FILENAME_PATTERN[metmodel].format(
                        UTC=utc, year=day.year, month=day.month, day=day.day
                    )
                    filename = self._findFileInPathes(
                        file, self.getMetInputDirs(metmodel)
                    )
                    if filename is not None:
                        fmtime = os.stat(filename).st_mtime
                        if (mtime.time() - fmtime) > (
                            60 * 10
                        ):  # file older than 10min -> no longer under production
                            relevant_dates.append(filename)

        return relevant_dates

    def getECMeteorologyFiles(
        self, dtime: datetime, run_hours: int, fixed_run="best", pattern=""
    ):
        """Get available meteorology files for the last few days around dtime and run_hours.

        Keyword arguments:
        dtime -- start time of model run
        run_hours -- run length in hours, possibly negative
        fixed_run -- string of form YYYY-MM-DD_HH giving a specific model-run
        """
        relevant_dates = []
        if not pattern:
            pattern = Resources.EC_FILE_PATTERN

        if fixed_run == "best":
            if run_hours < 0:
                start = dtime + timedelta(hours=run_hours)
            else:
                start = dtime

            start -= timedelta(hours=72)  # go 72 hours (forecast-length) back

            today = datetime.combine(date.today(), time(0, 0, 0))
            tomorrow = today + timedelta(days=1)
            days = []
            if (tomorrow - start).days > 1000:
                raise Exception(
                    "too long timespan: " + str(start) + " to " + str(tomorrow)
                )
            while start < tomorrow:
                days.append(start)
                start += timedelta(days=1)
            # loop needs to have latest model runs/hindcast runs last
            for offset in [3, 2, 1, 0]:
                for day in days:
                    for utc in [0, 6, 12, 18]:
                        file = pattern.format(
                            dayoffset=offset,
                            UTC=utc,
                            year=day.year,
                            month=day.month,
                            day=day.day,
                        )
                        filename = self._findFileInPathes(file, self.getECInputDirs())
                        if filename is not None:
                            relevant_dates.append(filename)
        else:
            match = re.match(r"(\d{4})-(\d{2})-(\d{2})_(\d{2})", fixed_run)
            if match:
                (year, month, day, utc) = tuple(map(int, list(match.group(1, 2, 3, 4))))
                for offset in [0, 1, 2, 3]:
                    file = self.EC_FILE_PATTERN.format(
                        dayoffset=offset, UTC=utc, year=year, month=month, day=day
                    )
                    filename = self._findFileInPathes(file, self.getECInputDirs())
                    if filename is not None:
                        relevant_dates.append(filename)

        return relevant_dates

    def getDoseCoefficients(self):
        try:
            dosecoeffs = read_dosecoefficients_icrp.DoseCoefficientsICRP(
                os.path.join(self.directory, "1-s2.0-S0146645313000110-mmc1.zip")
            )
        except Exception as e:
            dosecoeffs = None
        return dosecoeffs


if __name__ == "__main__":
    print(Resources().getStartScreen())
    print(
        Resources().getECMeteorologyFiles(datetime.combine(date.today(), time(0)), 48)
    )
    print(
        Resources().getECMeteorologyFiles(datetime.combine(date.today(), time(0)), -72)
    )
    runs = Resources().getECRuns()
    print(runs)
    print(
        Resources().getECMeteorologyFiles(
            datetime.combine(date.today(), time(0)), 48, runs[1]
        )
    )
    print(Resources().isotopes2snapinput([169, 158, 148]))
    print(
        Resources().getMEPS25MeteorologyFiles(
            datetime.combine(date.today(), time(0)), -48
        )
    )
    print(
        Resources().getMEPS25MeteorologyFiles(
            datetime.combine(date.today(), time(0)), -72
        )
    )
    print(Resources().get_dosecoefficients())
