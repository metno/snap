'''
Created on Sep 2, 2016

@author: heikok
'''
import datetime
import os
import re
import sys
import unittest

from Snappy.EEMEP.Resources import Resources
from Snappy.EEMEP.VolcanoRun import VolcanoRun


class ModelRunner():

    def __init__(self, path):
        self.res = Resources()
        volcano_path = os.path.join(path, "volcano.xml")
        if not os.path.exists(volcano_path):
            raise Exception("no such file or directory: {}".format(volcano_path))
        self.volcano = VolcanoRun(volcano_path)


        self.path = self.volcano.outputDir
        os.makedirs(name=self.path, exist_ok=True)
        self.volcano_to_column_source()
        self.create_meteo_files()

    def volcano_to_column_source(self):
        '''write columnsource_location.csv and columnsource_emissions.csv from volcano.xml'''
        with open(os.path.join(self.path, "columnsource_location.csv"), 'wt') as lh:
            lh.write(self.volcano.get_columnsource_location())
        with open(os.path.join(self.path, "columnsource_emission.csv"), 'wt') as eh:
            eh.write(self.volcano.get_columnsource_emission())

    def create_meteo_files(self, overwrite_model_start_time=None):
        '''Create meteorology files in the output-directory of volcano.xml.
        This involves linking and copying of needed meteorology. and eventually
        addition of a few timesteps at the beginning of the run

        Args:
            overwrite_model_start_time: don't used the volcano meteo, but the provided, used only for testing

        Returns: list of meteorology files
        '''
        (ref_date, model_start_time) = self.volcano.get_meteo_dates()

        # TODO: this will only the todays 00 run, this is not flexible enough yet
        files = self.res.getECMeteorologyFiles(model_start_time, 96, ref_date)
        for file in files:
            outfile = os.path.join(self.path, os.path.basename(file))
            if not os.path.exists(outfile):
                os.symlink(file, outfile)



class TestModelRunner(unittest.TestCase):

    def setUp(self):
        unittest.TestCase.setUp(self)
        self.indir = os.path.join(os.path.dirname(__file__),"test")
        volcanoFile = os.path.join(self.indir, "volcano.xml")
        volc = VolcanoRun(volcanoFile)
        self.dir = volc.outputDir
        os.makedirs(name=self.dir, exist_ok=True)
        yesterday = datetime.datetime.now() - datetime.timedelta(days=1)
        with open(os.path.join(self.dir, "volcano.xml"), "wt") as oh:
            with open(volcanoFile, "rt") as ih:
                for line in ih:
                    oh.write(re.sub('2016-11-03',yesterday.strftime('%Y-%m-%d'), line))
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
