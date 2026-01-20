import enum
import logging
import math
import os
import re
import subprocess
import sys
import time as mtime
from collections import OrderedDict
from datetime import date, datetime, time, timedelta
from time import gmtime, strftime

from Snappy import read_dosecoefficients_icrp
from Snappy.ResourcesCommon import ResourcesCommon

logger = logging.getLogger(__name__)


@enum.unique
class MetModel(enum.Enum):
    Icon0p25Global = "icon_0p25_global"
    Meps2p5 = "meps_2_5km"
    NrpaEC0p1 = "nrpa_ec_0p1"
    NrpaEC0p1Global = "nrpa_ec_0p1_global"
    EC0p1Global = "ec_0p1_global"
    EC0p1Europe = "ec_0p1_europe"
    GfsGribFilter = "gfs_grib_filter_fimex"
    Era5Nancy = "era5_nancy"

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
    _OUTPUTDIR_AUTOMATED = "{LUSTREDIR}/project/fou/kl/snap/automated_runs"
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
        MetModel.EC0p1Global: ["{LUSTREDIR}/project/metproduction/products/ecmwf/nc/"],
        MetModel.Icon0p25Global: ["{LUSTREDIR}/project/metproduction/products/icon/"],
    }

    _MET_INPUTDIRS = {
        MetModel.Meps2p5: [
            "{LUSTREDIR}/immutable/archive/projects/metproduction/MEPS/"
        ],
        MetModel.EC0p1Europe: ["{LUSTREDIR}/project/fou/kl/snap/meteo/ec_europe/"],
        MetModel.GfsGribFilter: ["{LUSTREDIR}/project/fou/kl/snap/meteo/gfs_europe/"],
        MetModel.Era5Nancy: [
            "{LUSTREDIR}/project/fou/kl/cerad//Meteorology/EC/Era5/Nancy/"
        ],
    }
    MET_FILENAME_PATTERN = {
        MetModel.Meps2p5: "{year:04d}/{month:02d}/{day:02d}/meps_det_2_5km_{year:04d}{month:02d}{day:02d}T{UTC:02d}Z.nc",
        MetModel.Icon0p25Global: "icon_{year:04d}{month:02d}{day:02d}T{UTC:02d}Z.nc",
        MetModel.GfsGribFilter: "gfs_0p25deg_{year:04d}{month:02d}{day:02d}T{UTC:02d}Z.nc",
        MetModel.EC0p1Europe: "ec_atmo_0_1deg_{year:04d}{month:02d}{day:02d}T{UTC:02d}0000Z_3h.nc",
        MetModel.Era5Nancy: "nancy_{year:04d}-{month:02d}-{day:02d}_{UTC:02d}.nc",
    }

    def __init__(self):
        """
        initialize the resources
        """
        self.directory = os.path.join(os.path.dirname(__file__), "resources")
        self.ecDomainWidth = 125.0
        self.ecDomainHeight = 60.0
        self.ecDomainRes = 0.1
        self.ecDefaultDomainStartX = -50.0
        self.ecDefaultDomainStartY = 25.0
        self.ecMaxFileOffset = 3

        startScreenFH = open(
            os.path.join(self.directory, "startScreen.html"), mode="r", encoding="UTF-8"
        )
        self.startScreen = startScreenFH.read()
        startScreenFH.close()
        plantBB = {"west": -60, "east": 70, "north": 85, "south": 26}
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
        elif metmodel == MetModel.EC0p1Global:
            return {}
        elif metmodel == MetModel.EC0p1Europe:
            return {}
        elif metmodel == MetModel.Meps2p5:
            return {}
        elif metmodel == MetModel.Icon0p25Global:
            return {}
        elif metmodel == MetModel.GfsGribFilter:
            return {}
        elif metmodel == MetModel.Era5Nancy:
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
        """return a dictionary of isotope-ids mapping to a dictionary with isotope,type and decay"""
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

    def isotopes2isoIds(self, isotopes: list[str | int]) -> list[int]:
        """
        translate a list of isotopes, i.e. ['Cs-137', ...] or ['Cs137', ...] or ['17', ...]
        to argos isotope id's
        """
        retval = []
        allIsos = self.getIsotopes()
        for iso in isotopes:
            isoId = -1
            try:
                iId = int(iso)
                if iId in allIsos:
                    isoId = iId
            except Exception:
                pass
            if isoId == -1:
                iso = iso.replace("-", "")
                for iId, isoDict in allIsos.items():
                    if iso == isoDict["isotope"]:
                        isoId = iId
                        break
            if isoId == -1:
                raise Exception(f"no isotope-id for isotope {iso}")
            retval.append(isoId)
        return retval

    def isotopes2snapinput(self, isotopeIds, add_DPUI=True):
        """Read a list of isotopeIds and return a text-block to be used for a snap.input file, like
        COMPONENT= Cs137
        RADIOACTIVE.DECAY.ON
        HALF.LIFETIME.YEARS= 30
        DRY.DEP.ON
        WET.DEP.ON
        RADIUS.MICROMETER=0.55
        DENSITY.G/CM3=2.3
        GRAVITY.FIXED.M/S=0.0002"""
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
                    f"Error, unknown type '{iso['type']}' for isotope '{iso['isotope']}'"
                )
            if DPUI is not None and DPUI >= 0.0:
                snapinput += f"DPUI.Sv_per_Bq_M3 = {DPUI}\n"
            snapinputs.append(snapinput)

        return "\n".join(snapinputs)

    def _getGribWriterConfig(self, isoIds, setFillValue=True):
        """Return a dictionary with a xml: xml-configuration string and a exracts: output-type and variable-names.
        Use in conjunction with convert_to_grib.

        Definitions from: https://wiki.pdc-argos.com/argoswiki/images/f/fc/ARGOS-MLDP.pdf
        """
        allIsos = self.getIsotopes()
        extracts = {
            "tofa": ["time_of_arrival"],
            "prec": ["lwe_precipitation_rate"],
            "wetd": [],
            "depo": [],
            "dose": [],
            "conc": [],
            "coco": [],
        }

        with open(os.path.join(self.directory, "isotopes_template.xml")) as isoTemplate:
            isoTemp = isoTemplate.read()

        isoStr = ""
        for isoId in isoIds:
            isoName = allIsos[isoId]["isotope"]
            isoStr += isoTemp.format(ISOTOPE=isoName, ID=isoId)
            extracts["conc"].append(f"{isoName}_concentration")
            extracts["coco"].append(f"{isoName}_column_concentration")
            extracts["dose"].append(f"{isoName}_acc_concentration")
            if allIsos[isoId]["type"] > 0:
                extracts["depo"].append(f"{isoName}_acc_total_deposition")
                extracts["wetd"].append(f"{isoName}_acc_wet_deposition")

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

        # change the ncml-config
        with open(
            os.path.join(self.directory, "fillValue_template.ncml")
        ) as fillValueFH:
            fillValueTemp = fillValueFH.read()

        varFills = []
        if setFillValue:
            for t in ("conc", "coco", "dose", "depo", "wetd"):
                for var in extracts[t]:
                    varFills.append(fillValueTemp.format(varname=var))

        with open(
            os.path.join(self.directory, "removeSnapReferenceTime.ncml")
        ) as ncmlFH:
            ncmlOut = ncmlFH.read()
        ncmlOut = ncmlOut.format(variables="\n".join(varFills))

        return {
            "extracts": extracts,
            "xml": xmlOut,
            "ncml": ncmlOut,
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

    def readRadnett(
        self,
    ):
        stations = OrderedDict()
        with open(
            os.path.join(os.path.dirname(__file__), "resources/radnett.csv"),
            mode="r",
            encoding="UTF-8",
        ) as f:
            degree_minute_regex = re.compile(r"([0-9]+)°\s([0-9]+)’\s[NØ]")
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
        elif metmodel == MetModel.EC0p1Global or metmodel == MetModel.EC0p1Europe:
            filename = os.path.join(self.directory, "snap.input_ec_0p1.tmpl")
        elif metmodel == MetModel.Meps2p5:
            filename = os.path.join(self.directory, "snap.input_meps_2_5km.tmpl")
        elif metmodel == MetModel.Icon0p25Global:
            filename = os.path.join(self.directory, "snap.input_icon_0p25.tmpl")
        elif metmodel == MetModel.GfsGribFilter:
            filename = os.path.join(self.directory, "snap.input_gfs_grib_filter.tmpl")
        elif metmodel == MetModel.Era5Nancy:
            filename = os.path.join(self.directory, "snap.input_era5_nancy.tmpl")
        else:
            raise (
                NotImplementedError("metmodel='{}' not implememented".format(metmodel))
            )

        f = open(filename)
        return f.read()

    def getSnapInputMetDefinitions(
        self,
        metmodel,
        files,
        nx=0,
        ny=0,
        startX=0,
        startY=0,
        dx=0,
        dy=0,
        interpolation="",
    ):
        """Get the definitions for the metmodel, including met-files and domain (unless default).
        This should be written to the snap.input file, in addition to the source-term. files may be empty
        """
        largest_landfraction_file = (
            f"ERROR: no largest land cover defined for metmodel {metmodel}"
        )
        lines = []
        if metmodel == MetModel.NrpaEC0p1:
            largest_landfraction_file = os.path.join(
                self.directory, "largestLandFraction_NrpaEC0p1.nc"
            )
        elif metmodel == MetModel.NrpaEC0p1Global:
            # no setup needed, autdetection in snap
            pass
        elif metmodel == MetModel.EC0p1Global:
            # no setup needed, autdetection in snap
            pass
        elif metmodel == MetModel.EC0p1Europe:
            # no setup needed, autdetection in snap
            pass
        elif metmodel == MetModel.Meps2p5:
            largest_landfraction_file = os.path.join(
                self.directory, "largestLandFraction_MEPS_byte.nc"
            )
        elif metmodel == MetModel.Icon0p25Global:
            # no setup needed, autdetection in snap
            pass
        elif metmodel == MetModel.GfsGribFilter:
            # no setup needed, autdetection in snap
            pass
        elif metmodel == MetModel.Era5Nancy:
            pass
        else:
            raise (
                NotImplementedError("metmodel='{}' not implememented".format(metmodel))
            )

        for f in files:
            lines.append(f"FIELD.INPUT= {f}")

        lines.append("")
        lines.append(
            self._getSnapInputTemplate(metmodel).format(
                interpolation=interpolation,
                LUSTREDIR=self.getLustreDir(),
                largest_landfraction_file=largest_landfraction_file,
            )
        )
        return "\n".join(lines)

    def getSendmailScript(self):
        filename = os.path.join(self.directory, "sendmail.sh")
        return filename

    def getBSnapInputFile(self):
        filename = os.path.join(self.directory, "snap.in")
        return filename

    def getSnapOutputDir(self):
        """Output directory of SNAP-runs for manual runs through the front-end.
        This directory is available for all SnapPy users.

        :return: path as directory, adapted for LUSTREDIR
        """
        return self._OUTPUTDIR.format(LUSTREDIR=self.getLustreDir())

    def getSnapOutputDirAutomated(self):
        """Output directory of SNAP-runs for automated runs, e.g. run through cron.
        This directory is available for group snap-op.

        :return: path as directory, adapted for LUSTREDIR
        """
        return self._OUTPUTDIR_AUTOMATED.format(LUSTREDIR=self.getLustreDir())

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
    ) -> list[str]:
        """Get available meteorology files for the last few days around dtime and run_hours.

        :param dtime: start time of model run
        :param run_hours: run length in hours, possibly negative
        :param fixed_run: string of form YYYY-MM-DD_HH giving a specific model-run, defaults to "best"
        :param pattern: pattern to match files, defaults to ""
        :raises Exception: if the timespan is too long
        :return: list of relevant meteorology files
        """
        relevant_dates = []
        if not pattern:
            pattern = Resources.EC_FILE_PATTERN

        if fixed_run == "best":
            if run_hours < 0:  # start and finish flipped to collect time range
                start = dtime + timedelta(hours=run_hours)
                finish = dtime  # start < finish always
            else:
                start = dtime  # start < finish always
                finish = start + timedelta(hours=run_hours)

            if (
                start.hour < 3
            ):  # accounts for special case where start before 3am. Then previous days files are needed.
                start -= timedelta(hours=3)

            today = datetime.combine(date.today(), time(0, 0, 0))
            tomorrow = today + timedelta(days=1)

            logger.debug((f"start {start}"))
            logger.debug((f"finish {finish}"))

            # Case 1: Collect future files first

            if finish >= tomorrow:
                future_days = (finish - today).days
                # Loop through forecasts on latest model run. Collects all utcs in case of missing data
                for offset in range(1, future_days + 1):
                    for utc in [0, 6, 12, 18]:
                        file = pattern.format(
                            dayoffset=offset,
                            UTC=utc,
                            year=today.year,
                            month=today.month,
                            day=today.day,
                        )
                        filename = self._findFileInPathes(file, self.getECInputDirs())
                        if filename is not None:
                            relevant_dates.append(filename)
                        elif utc == 0:  # Accounts for no complete dataset for today
                            logger.debug(f"else: File {file} doesnt exist")
                            utc_list = [18, 12, 6, 0]
                            cases = [
                                (u, d)
                                for d in range(1, self.ecMaxFileOffset + 1)
                                for u in utc_list
                            ]
                            # Index loops through day offsets (max of ecMaxFileOffset) and utc list (last index of len(utc_list))
                            for new_utc, dayoffset in cases:
                                logger.debug("Still going...")
                                file = pattern.format(
                                    dayoffset=dayoffset + offset,
                                    UTC=new_utc,
                                    year=(today - timedelta(days=dayoffset)).year,
                                    month=(today - timedelta(days=dayoffset)).month,
                                    day=(today - timedelta(days=dayoffset)).day,
                                )
                                filename = self._findFileInPathes(
                                    file, self.getECInputDirs()
                                )
                                logger.debug(f"Trying {file}")
                                if filename is not None:
                                    logger.debug(f"Took {file} instead")
                                    relevant_dates.append(filename)
                                    logger.debug("break")
                                    break
                            if filename is None:
                                logger.debug("No alternative file exists")

                        else:
                            logger.debug(f"File {file} doesnt exist")

            # Case 2: Collect hindcasts

            if start <= tomorrow:
                n_days = (today - start).days

                if abs(n_days) > 1000:  # accounts for stored data period
                    raise Exception(
                        "too long timespan: " + str(start) + " to " + str(tomorrow)
                    )

                elif n_days < 0:  # accounts for future-only forecasts
                    days = [
                        today,
                    ]

                else:
                    days = []
                    tmp = start
                    while tmp < min(finish + timedelta(days=1), tomorrow):
                        days.append(tmp)
                        tmp += timedelta(days=1)

                for day in days:  # Loops through past dates needed
                    for utc in [0, 6, 12, 18]:
                        file = pattern.format(
                            dayoffset=0,
                            UTC=utc,
                            year=day.year,
                            month=day.month,
                            day=day.day,
                        )
                        filename = self._findFileInPathes(file, self.getECInputDirs())
                        if filename is not None:
                            relevant_dates.append(filename)
                        elif utc == 0:
                            logger.debug(f"File {file} doesnt exist")
                            utc_list = [18, 12, 6, 0]

                            cases = [
                                (u, d)
                                for d in range(1, self.ecMaxFileOffset + 1)
                                for u in utc_list
                            ]
                            for new_utc, dayoffset in cases:
                                file = pattern.format(
                                    dayoffset=dayoffset,
                                    UTC=new_utc,
                                    year=(day - timedelta(days=dayoffset)).year,
                                    month=(day - timedelta(days=dayoffset)).month,
                                    day=(day - timedelta(days=dayoffset)).day,
                                )
                                filename = self._findFileInPathes(
                                    file, self.getECInputDirs()
                                )

                                if filename is not None:
                                    logger.debug(f"Took {file} instead")
                                    relevant_dates.append(filename)
                                    break
                            if filename is None:
                                logger.debug("No alternative file exists")

                        else:
                            logger.debug(f"File {file} doesnt exist")

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
        except Exception:
            dosecoeffs = None
        return dosecoeffs


# setting bitmapCompress as default to False
# fimex drops all fields which are completely missing, which argos doesn't like
# waiting for fimex-fix
def snapNc_convert_to_grib(snapNc, basedir, ident, isotopes, bitmapCompress=False):
    """simple function to implement conversion to grib snap.nc using fimex
    and resources-setup

    Parameters
    ----------
    isotopes : list
        list of isoIds [1,2,3,4...] or isotope-names like ['Cs-137', 'Cs-134',...]
    """
    res = Resources()
    isoIds = res.isotopes2isoIds(isotopes)
    config = res._getGribWriterConfig(isoIds, setFillValue=bitmapCompress)
    xmlFile = "cdmGribWriterConfig.xml"
    basexmlFile = os.path.join(basedir, xmlFile)
    ncmlFile = "config.ncml"
    baseNcmlFile = os.path.join(basedir, ncmlFile)
    with open(baseNcmlFile, "w") as nh:
        nh.write(config["ncml"])

    errlog = open(os.path.join(basedir, "fimex.errlog"), "w")
    outlog = open(os.path.join(basedir, "fimex.outlog"), "w")
    tempfile = "tmp.grib"
    basetempfile = os.path.join(basedir, tempfile)
    # fimex works in basedir, so it does not need the basefiles
    for appendix, params in config["extracts"].items():
        if appendix == "tofa":
            omitEmptyFields = True
        else:
            omitEmptyFields = False
        with open(basexmlFile, "w") as xh:
            xh.write(config["xml"].format(OMIT_EMPTY_FIELDS=omitEmptyFields))
        outFile = os.path.join(basedir, f"{ident}_{appendix}")
        with open(outFile, "wb") as gh:
            for param in params:
                if os.path.exists(basetempfile):
                    os.remove(basetempfile)
                procOptions = [
                    "fimex",
                    f"--input.file={snapNc}",
                    f"--input.config={ncmlFile}",
                    # avoid problem with lat/lon variables
                    # in fimex grib-writer< 0.64
                    # '--extract.removeVariable=longitude',
                    # '--extract.removeVariable=latitude',
                    f"--output.file={tempfile}",
                    "--output.type=grib",
                    f"--output.config={xmlFile}",
                ]
                procOptions.append(f"--extract.selectVariables={param}")
                print(" ".join(procOptions))
                proc = subprocess.Popen(
                    procOptions, cwd=basedir, stderr=errlog, stdout=outlog
                )
                if proc.wait() != 0:
                    errlog.write(
                        "'{fimex}' in {dir} failed".format(
                            fimex=" ".join(procOptions), dir=basedir
                        )
                    )
                else:
                    # append tmp-file to final grib-file
                    with open(basetempfile, "rb") as th:
                        while True:
                            data = th.read(16 * 1024 * 1024)  # read max 16M blocks
                            if data:
                                gh.write(data)
                            else:
                                break
                if os.path.exists(basetempfile):
                    os.remove(basetempfile)

    errlog.close()
    outlog.close()


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
    print(Resources().getDoseCoefficients())
    isotopes = ["Cs-137", "Cs134"]
    isoIds = Resources().isotopes2isoIds(isotopes)
    print(f"f{isotopes} have ids:  {isoIds}")
    assert len(isotopes) == len(isoIds)
    # isotopes2isoIds idempotent
    assert len(isoIds) == len(Resources().isotopes2isoIds(isoIds))
