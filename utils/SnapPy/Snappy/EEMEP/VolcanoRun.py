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
Created on Nov 24, 2016

@author: heikok
"""
import datetime
import re
import unittest

import xml.etree.ElementTree as ET


class Eruption:
    def __init__(
        self,
        top: int,
        bottom: int,
        rate: float,
        m63: float,
        start: datetime.datetime,
        end: datetime.datetime,
    ):
        """Container class for eruption parameters

        :param top: top of the eruption-cloud above MSL in m
        :param rate: emission rate in kg/s
        :param m63: fraction of particles below 63µm
        :param start: start of eruption
        :param end: end of this eruption
        """
        self._top = top
        self._bottom = bottom
        self._rate = rate
        self._m63 = m63
        self._start = start
        self._end = end
        return

    @property
    def top(self) -> int:
        """Top of plume above ground in m

        :return: top
        """
        return self._top

    @property
    def bottom(self) -> int:
        """Bottom of plume above ground in m

        :return: bottom
        """
        return self._bottom

    @property
    def rate(self) -> float:
        """Emission rate of plume in kg/s

        :return: rate
        """
        return self._rate

    @property
    def m63(self) -> float:
        """Fraction of ash with size < 63µm (fine-ash fraction)

        :return: m63 fraction
        """
        return self._m63

    @property
    def start(self) -> datetime.datetime:
        """Start-time of the eruption

        :return: start-time
        """
        return self._start

    @property
    def end(self) -> datetime.datetime:
        """End-time of the eruption

        :return: end-time
        """
        return self._end


class VolcanoXML:
    def __init__(self, xmlFile):
        """A parser for volcano.xml files

        :param xmlFile: filename to parse
        :raises several: mainly parsing errors
        """

    def __init__(self, xmlFile):
        """
        Open a volcano by reading a volcano.xml file. Might raise several Exceptions.
        """
        self.root = None
        _defs = {}
        self.xmlFile = xmlFile
        # raise eventual xml-parsing errors already in init
        self._defs = {}
        try:
            self._parseXML()
        except Exception as ex:
            raise Exception(f"Error while parsing {xmlFile}") from ex

    def _parseXML(self):
        """parse volcano.xml file to self._defs"""
        tree = ET.parse(self.xmlFile)
        root = tree.getroot()
        assert (
            root.tag == "volcanic_eruption_run"
        ), "not a volcanic_eruption_run file: {}".format(self.xmlFile)
        self._defs["outputDir"] = root.attrib["output_directory"]
        self._defs["runTimeHours"] = float(root.attrib["run_time_hours"])

        # lat/lon
        volc = root.find("volcano")
        self._defs["lat"] = float(volc.attrib["lat"])
        self._defs["lon"] = float(volc.attrib["lon"])
        self._defs["altitude"] = int(volc.attrib["altitude"])

        # volcano-name
        name = volc.attrib["name"]
        name = re.sub(r"[\W,]", "", name)
        self._defs["name"] = name

        # meteo-dates
        if (weather := root.find("model_setup/weather_forecast")) is not None:
            self._defs["eemep"] = {}
            self._defs["eemep"]["reference_date"] = weather.attrib["reference_date"]
            self._defs["eemep"]["model_start_time"] = datetime.datetime.strptime(
                weather.attrib["model_start_time"], "%Y-%m-%dT%H:%M:%SZ"
            )
            # round down to full hours
            self._defs["eemep"]["model_start_time"] = self._defs["eemep"][
                "model_start_time"
            ].replace(minute=0, second=0, microsecond=0)
            # round down to latest 3 hour multiple (meteo every 3 hours)
            hour = self._defs["eemep"]["model_start_time"].hour
            if hour % 3 != 0:
                offset
                self._defs["eemep"]["model_start_time"] = self._defs["eemep"][
                    "model_start_time"
                ].replace(hour=hour - offset)
                # increase run-time accordingly
                self._defs["runTimeHours"] += offset


            # restart-file
            model_run = root.find("model_setup[@use_restart_file]")
            self._defs["eemep"]["use_restart_file"] = False
            if model_run.attrib["use_restart_file"] == "restart":
                self._defs["eemep"]["use_restart_file"] = True
                if self._defs["eemep"]["model_start_time"].hour != 0:
                    # model has to start at midnight for restart runs
                    offset = self._defs["eemep"]["model_start_time"].hour
                    self._defs["eemep"]["model_start_time"] -= datetime.timedelta(
                        hours=offset
                    )
                    # increase run-time accordingly
                    self._defs["runTimeHours"] += offset
        if (snap := root.find("snap_model_setup/weather_forecast")) is not None:
            self._defs["snap"] = {}
            self._defs["snap"]["reference_date"] = snap.attrib["reference_date"]
            self._defs["snap"]["model"] = snap.attrib["model"]

        # eruptions
        self._defs["eruptions"] = self._parseEruptions(root)

    def _parseEruptions(self, root):
        erups = []
        for erup in root.findall("eruptions/eruption"):
            top = float(erup.attrib["top"])
            bottom = float(erup.attrib["bottom"])
            rate = int(erup.attrib["rate"])
            m63 = float(erup.attrib["m63"])
            start = datetime.datetime.strptime(
                erup.attrib["start"], "%Y-%m-%dT%H:%M:%SZ"
            )
            end = datetime.datetime.strptime(erup.attrib["end"], "%Y-%m-%dT%H:%M:%SZ")
            erups.append(Eruption(top, bottom, rate, m63, start, end))
        return erups

    @property
    def eemep_model_setup(self) -> None | tuple[str, datetime.datetime, bool]:
        """get the model setup parameters for eemep, i.e. the reference-date as string
        (allways best currently), the model start-time, which might be different from the
        eruption start, and a boolean flag if restart-files should be used

        :return: tuple of reference-date, start-date, use-restart-file
        """
        if "eemep" in self._defs:
            return (
                self._defs["eemep"]["reference_date"],
                self._defs["eemep"]["model_start_time"],
                self._defs["eemep"]["use_restart_file"],
            )
        return None

    @property
    def snap_model_setup(self) -> None | tuple[str, str]:
        """get the model setup parameters for snap-ash runs, i.e. the reference date ("best) and meteorological model

        :return: tuple of reference-date and met-model, None if snap not configured
        """
        if "snap" in self._defs:
            return (self._defs["snap"]["reference_date"], self._defs["snap"]["model"])

    @property
    def outputDir(self) -> str:
        """Output directory"""
        return self._defs["outputDir"]

    @property
    def runTimeHours(self) -> float:
        """Runtime of the volcano-run in hours

        :return: hours
        """
        return self._defs["runTimeHours"]

    @property
    def latlon(self) -> tuple[float, float]:
        """the location of the volcano

        :returns: (latitude, longitude)
        """
        return (self._defs["lat"], self._defs["lon"])

    @property
    def latitude(self) -> float:
        """the latitude location of the volcano

        :returns: latitude
        """
        return self.latlon[0]

    @property
    def longitude(self) -> float:
        """the longitude location of the volcano

        :returns: longitude
        """
        return self.latlon[1]

    @property
    def name(self) -> str:
        """the name of the volcano

        :return: volcano-name
        """
        return self._defs["name"]

    @property
    def altitude(self) -> str:
        """altitude of the volcano

        :return: altitude
        """
        return self._defs["altitude"]

    @property
    def eruptions(self) -> list[Eruption]:
        """A list of eruptions from the same volcano

        :return: eruptions
        """
        return self._defs["eruptions"]


class VolcanoRun(VolcanoXML):
    """
    volcano-run definition handling for eemep, e.g. reading from a xml-file and translating
    to columnsource-emission/location files
    """

    def get_meteo_dates(self) -> tuple[str, datetime.datetime]:
        """meteorological dates

        :return: (reference_date, model_start_time) of the meteorology,
        where reference_date is a string (e.g. "best") and model_start_time a datetime object
        """
        if self.eemep_model_setup is None:
            raise Exception(
                f"{self.xmlFile} not configured for eemep, no model_setup found"
            )
        return self.eemep_model_setup[0:2]

    def get_columnsource_location(self):
        """get a string used within a eemep columnsource_location.csv file, e.g.
        #NUMBER,NAME,LOCATION,LATITUDE,NS,LONGITUDE,EW,ELEV,TYPE,ERUPTION TYPE
        V1702A02B,Bardarbunga,Iceland-NE,64.63,N,17.53,W,2009,Stratovolcano,M0
        """
        lat, lon = self.latlon
        northsouth = "N"
        if lat < 0:
            northsouth = "S"
            lat = lat * -1

        eastwest = "E"
        if lon < 0:
            eastwest = "W"
            lon = lon * -1

        desc = (
            "#NUMBER,NAME,LOCATION,LATITUDE,NS,LONGITUDE,EW,ELEV,TYPE,ERUPTION TYPE\n"
        )
        definition = f"ASH,{self.name},XXX,{lat},{northsouth},{lon},{eastwest},{self.altitude},xxx,M0\n"

        return desc + definition

    def get_columnsource_emission(self):
        """get a string used within a eemep columnsource_emission.csv file, e.g.

        #TYPE/NPP,VARIABLE,BASE[km],H[km above vent],D[h],dM/dt[kBq/s],m63[-],START[code/date],END[code/date],DESCRIPTION
        M0,,VENT,  10.000,  96,  1987149.0,  0.05,2016-10-11 05:00:00,SE+D, no description
        """
        desc = "#TYPE/NPP,VARIABLE,BASE[km],H[km above vent],D[h],dM/dt[kBq/s],m63[-],START[code/date],END[code/date],DESCRIPTION\n"
        definition = "M0,,VENT,  {height},  {duration},  {rate},  {m63},{startdate:%Y-%m-%d %H:%M:%S},{enddate:%Y-%m-%d %H:%M:%S}, no description\n"
        # add constant 2h emissions of 0.1Tg/h SO2 from the beginning of the run for all emissions (M0)
        definition += "M0,SO2,VENT, 2.0,  2.0,  27777,  1.,{startdate:%Y-%m-%d %H:%M:%S},SE+D, 0.1 Tg/h - Holuraun: ~ 0.1-0.5Tg/day: 1 DU = 2.85*10^-5 kg/m2\n"
        out = [desc]
        for erup in self.eruptions:
            defs = {}
            defs["height"] = erup.top / 1000
            defs["rate"] = erup.rate
            defs["m63"] = erup.m63
            start = erup.start
            end = erup.end
            defs["duration"] = (end - start).total_seconds() / (60.0 * 60.0)
            defs["startdate"] = start
            defs["enddate"] = end
            out.append(definition.format(**defs))

        assert len(out) > 1, "no eruptions found"

        return "".join(out)

    def run_as_restart(self):
        if self.eemep_model_setup is None:
            raise Exception(
                f"{self.xmlFile} not configured for eemep, no model_setup found"
            )
        return self.eemep_model_setup[2]


class TestVolcanoRun(unittest.TestCase):
    def setUp(self):
        import os

        unittest.TestCase.setUp(self)
        self.volcFile = os.path.join(os.path.dirname(__file__), "test", "volcano.xml")

    def test_init(self):
        VolcanoRun(self.volcFile)

    def test_meteo(self):
        volc = VolcanoRun(self.volcFile)
        (refString, start) = volc.get_meteo_dates()
        self.assertEqual(refString, "best", "meteo reference date")
        self.assertIsInstance(start, datetime.datetime, "meteo start date is datetime")

    def test_location(self):
        volc = VolcanoRun(self.volcFile)
        expected = """#NUMBER,NAME,LOCATION,LATITUDE,NS,LONGITUDE,EW,ELEV,TYPE,ERUPTION TYPE
ASH,Askja,XXX,65.03,N,16.75,W,1516,xxx,M0
"""
        self.assertEqual(
            volc.get_columnsource_location(), expected, "columnsource_location"
        )

    def test_emission(self):
        volc = VolcanoRun(self.volcFile)
        expected = """#TYPE/NPP,VARIABLE,BASE[km],H[km above vent],D[h],dM/dt[kBq/s],m63[-],START[code/date],END[code/date],DESCRIPTION
M0,,VENT,  7.0,  1.0,  100000,  0.05,2016-11-03 08:00:00,2016-11-03 09:00:00, no description
M0,SO2,VENT, 2.0,  2.0,  27777,  1.,2016-11-03 08:00:00,SE+D, 0.1 Tg/h - Holuraun: ~ 0.1-0.5Tg/day: 1 DU = 2.85*10^-5 kg/m2
"""
        self.assertEqual(
            volc.get_columnsource_emission(), expected, "columnsource_emission"
        )

    def test_run_restart(self):
        volc = VolcanoRun(self.volcFile)
        self.assertTrue(volc.run_as_restart(), "restart run")


if __name__ == "__main__":
    unittest.main()
