'''
Created on Sep 2, 2016

@author: heikok
'''
import datetime
import os
import sys
import unittest

from Snappy.EEMEP.Resources import Resources
import xml.etree.ElementTree as ET


class ModelRunner():

    def __init__(self, path):
        self.res = Resources()
        volcano_path = os.path.join(path, "volcano.xml")
        if not os.path.exists(volcano_path):
            raise Exception("no such file or directory: {}".format(volcano_path))
        self.defs = ET.parse(volcano_path).getroot()
        assert self.defs.tag == "volcanic_eruption_run", 'not a volcanic_eruption_run: {}'.format(volcano_path)

        self.path = self.defs.attrib["output_directory"]
        os.makedirs(name=self.path, exist_ok=True)
        self.volcano_to_column_source()
        self.create_meteo_files()

    def volcano_to_column_source(self):
        '''write columnsource_location.csv and columnsource_emissions.csv from volcano.xml'''
        locinfo = '''#NUMBER,NAME,LOCATION,LATITUDE,NS,LONGITUDE,EW,ELEV,TYPE,ERUPTION TYPE
VOLCANOXX,{name},unknown,{latitude},{NS},{longitude},{WE},{altitude},none,v0
'''
        volc = self.defs.find("volcano")
        latitude = float(volc.attrib['lat'])
        northSouth = 'S' if (latitude < 0) else 'N'
        latitude = abs(latitude)
        longitude = float(volc.attrib['lon'])
        eastWest = 'W' if (longitude < 0) else 'E'
        longitude = abs(longitude)
        with open(os.path.join(self.path, "columnsource_location.csv"), 'wt') as lh:
            lh.write(locinfo.format(name=volc.attrib['name'],
                                    latitude=latitude,
                                    longitude=longitude,
                                    NS=northSouth,
                                    WE=eastWest,
                                    altitude=int(volc.attrib['altitude'])))

        with open(os.path.join(self.path, "columnsource_emission.csv"), 'wt') as eh:
            eh.write("#TYPE/VOLCANO,VARIABLE,BASE[km],H[km above VENT],D[h],dM/dt[kg/s],m63[-],START[code/date],END[code/date],DESCRIPTION\n")
            eruption = "v0,,VENT,  {top},  {duration}, {rate}, {m63},{start},{end},none\n"
            for erup in self.defs.findall("eruptions/eruption"):
                bottom = int(erup.attrib['bottom'])
                if (bottom != 0):
                    print("ignoring bottom {}, setting to 0".format(bottom), file=sys.stderr)
                start = datetime.datetime.strptime(erup.attrib['start'], "%Y-%m-%dT%H:%M:%SZ")
                end = datetime.datetime.strptime(erup.attrib['end'], "%Y-%m-%dT%H:%M:%SZ")
                duration = round((end - start).total_seconds() / 3600)
                eh.write(eruption.format(top=erup.attrib['top'],
                                         start=start.isoformat(),
                                         end=end.isoformat(),
                                         duration=duration,
                                         rate=erup.attrib['rate'],
                                         m63=erup.attrib['m63']))
    def create_meteo_files(self):
        '''Create meteorology files in the output-directory of volcano.xml.
        This involves linking and copying of needed meteorology. and eventually
        addition of a few timesteps at the beginning of the run

        Returns: list of meteorology files
        '''
        weather = self.defs.find("model_setup/weather_forecast")
        model_start_time = datetime.datetime.strptime(weather.attrib["model_start_time"],"%Y-%m-%dT%H:%M:%SZ")
        # TODO: this will only the todays 00 run, this is not flexible enough yet
        files = self.res.getECMeteorologyFiles(model_start_time, 96, weather.attrib["reference_date"])
        for file in files:
            outfile = os.path.join(self.path, os.path.basename(file))
            if not os.path.exists(outfile):
                os.symlink(file, outfile)



class ModelRunnerTest(unittest.TestCase):

    def setUp(self):
        unittest.TestCase.setUp(self)
        self.dir = os.path.join(os.path.dirname(__file__),"test")
        self.files = ('columnsource_location.csv', 'columnsource_emission.csv')
        for file in self.files:
            if (os.path.exists(file)):
                os.unlink(file)

    def testModelRunner(self):
        mr = ModelRunner(self.dir)
        self.assertTrue(len(self.files) == 2)
        #self.assertTrue(os.path.exists(self.files[0]))
        for x in self.files:
            self.assertTrue(os.path.exists(os.path.join(mr.path, x)), "file created: {}".format(x))



if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
