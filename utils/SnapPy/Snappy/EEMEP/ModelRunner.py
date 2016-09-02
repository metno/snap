'''
Created on Sep 2, 2016

@author: heikok
'''
import unittest
import xml.etree.ElementTree as ET
import datetime
import os
import sys


class ModelRunner():

    def __init__(self, path):
        self.path = path
        volcanoPath = os.path.join(self.path, "volcano.xml")
        if not os.path.exists(volcanoPath):
            raise Exception("no such file or directory: {}".format(volcanoPath))
        self.defs = ET.parse(volcanoPath).getroot()
        assert self.defs.tag == "volcanic_eruption_run", 'not a volcanic_eruption_run: {}'.format(volcanoPath)

        self.volcanoToColumnSource()

    def volcanoToColumnSource(self):
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


class ModelRunnerTest(unittest.TestCase):

    def setUp(self):
        unittest.TestCase.setUp(self)
        self.dir = os.path.join(os.path.dirname(__file__),"test")
        self.files = tuple(map(lambda x: os.path.join(self.dir, x), ('columnsource_location.csv', 'columnsource_emission.csv')))
        for f in self.files:
            if (os.path.exists(f)):
                os.unlink(f)

    def testModelRunner(self):
        ModelRunner(self.dir)
        self.assertTrue(len(self.files) == 2)
        #self.assertTrue(os.path.exists(self.files[0]))
        tuple(map(lambda x: self.assertTrue(os.path.exists(x), "file created: {}".format(x)), self.files))
        pass


if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
