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
'''
Created on Nov 24, 2016

@author: heikok
'''
import datetime
import re
import unittest

import xml.etree.ElementTree as ET


class NppRun():
    '''
    npp-run definition handling, e.g. reading from a xml-file
    '''
    root = None
    xmlFile = None
    outputDir = None
    runTimeHours = 0.

    def __init__(self, xmlFile):
        '''
        Open a npp by reading a npp.xml file. Might raise several Exceptions.
        '''
        self.xmlFile = xmlFile
        tree = ET.parse(xmlFile)
        self.root = tree.getroot()
        assert self.root.tag == 'npp_emission_run', \
            "not a npp_emission_run file: {}".format(xmlFile)
        self.outputDir = self.root.attrib['output_directory']
        self.runTimeHours = float(self.root.attrib['run_time_hours'])
        # raise eventual xml-parsing errors already in init
        self.get_columnsource_emission()
        self.get_columnsource_location()
        self.get_meteo_dates()
        self.run_as_restart()

    def get_meteo_dates(self):
        '''Returns (reference_date, model_start_time) of the meteorology,
        where reference_date is a string (best) and model_start_time a datetime object'''

        weather = self.root.find("model_setup/weather_forecast")
        return (weather.attrib["reference_date"], \
                datetime.datetime.strptime(weather.attrib["model_start_time"],"%Y-%m-%dT%H:%M:%SZ"))


    def get_columnsource_location(self):
        '''get a string used within a eemep columnsource_location.csv file, e.g.
        #NUMBER,NAME,LOCATION,LATITUDE,NS,LONGITUDE,EW,ELEV,TYPE,ERUPTION TYPE
        V1702A02B,Bardarbunga,Iceland-NE,64.63,N,17.53,W,2009,Stratovolcano,M0
        '''
        volc = self.root.find("npp")
        defs = {}
        defs["lat"] = float(volc.attrib['lat'])
        defs["north"] = 'N'
        if (defs["lat"] < 0):
            defs["north"] = 'S'
            defs["lat"] = defs["lat"] * -1
        defs["lon"] = float(volc.attrib['lon'])
        defs["east"] = 'E'
        if (defs["lon"] < 0):
            defs["east"] = 'W'
            defs["lon"] = defs["lon"] * -1
        name = volc.attrib['name']
        defs["name"] = re.sub(r'[\W,]', '', name)
        defs["altitude"] = int(volc.attrib['altitude'])

        desc = "#NUMBER,NAME,LOCATION,LATITUDE,NS,LONGITUDE,EW,ELEV,TYPE,ERUPTION TYPE\n"
        definition = "NUC,{name},XXX,{lat},{north},{lon},{east},{altitude},xxx,NUC\n".format(**defs)

        self.latitude = defs['lat']
        self.longitude = defs ['lon']
        
        return desc + definition


    def get_columnsource_emission(self):
        '''get a string used within a eemep columnsource_emission.csv file, e.g.

        #TYPE/NPP,VARIABLE,BASE[km],H[km above vent],D[h],dM/dt[kBq/s],m63[-],START[code/date],END[code/date],DESCRIPTION
        NUC,,VENT,  10.000,  96,  1987149.0,  0.05,2016-10-11 05:00:00,SE+D, no description
        '''
        desc = "#TYPE/NPP,VARIABLE,BASE[km],H[km above vent],D[h],dM/dt[kBq/s],m63[-],START[code/date],END[code/date],DESCRIPTION\n"
        definition = "NUC,NPP_{component},VENT,  {height},  {duration},  {rate},  1.0,{startdate},{enddate}, no description\n"

        out = [desc]
        for emis in self.root.findall('emissions/emission'):
            defs = {}
            defs["height"] = float(emis.attrib['top'])/1000.
            defs["rate"] = int(emis.attrib['rate'])
            start = datetime.datetime.strptime(emis.attrib['start'], '%Y-%m-%dT%H:%M:%SZ')
            end = datetime.datetime.strptime(emis.attrib['end'], '%Y-%m-%dT%H:%M:%SZ')

            defs["component"] = emis.attrib["component"]
            defs["duration"] = (end-start).total_seconds() / (60.*60.)
            defs["startdate"] = start.strftime('%Y-%m-%d %H:%M:%S')
            defs["enddate"] = end.strftime('%Y-%m-%d %H:%M:%S')
            out.append(definition.format(**defs))

        assert len(out) > 1, "no emission found"

        return ''.join(out)

    def run_as_restart(self):
        model_run = self.root.find('model_setup[@use_restart_file]')
        if (model_run.attrib["use_restart_file"] == "restart"):
            return True
        return False




