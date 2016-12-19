'''
Created on Sep 2, 2016

@author: heikok
'''
import datetime
import os
import re
import sys
from time import sleep
import unittest

from METNO.HPC import HPC, StatusFile, QJobStatus
from Snappy.EEMEP.Resources import Resources
from Snappy.EEMEP.VolcanoRun import VolcanoRun


class ModelRunner():


    def __init__(self, path, hpcMachine):
        self.upload_files = set()
        self.timestamp = datetime.datetime.now()
        self.jobscript = "eemep_script.job"
        self.statusfile = "status"
        self.res = Resources()
        self.hpc = HPC.by_name(hpcMachine)
        self.hpcMachine = hpcMachine
        self.inpath = path

        self.rundir = self.res.HPC[self.hpcMachine]["RUNDIR"]
        volcano_path = os.path.join(path, "volcano.xml")
        if not os.path.exists(volcano_path):
            raise Exception("no such file or directory: {}".format(volcano_path))
        self.volcano = VolcanoRun(volcano_path)
        self.runtag = "eemep_{}".format(os.path.basename(self.volcano.outputDir))
        self.hpc_outdir = os.path.join(self.rundir, self.runtag)

        self.path = self.volcano.outputDir
        os.makedirs(name=self.path, exist_ok=True)
        self._volcano_to_column_source()
        self._get_meteo_files()
        self._get_restart_file()
        self._create_job_script()

    def _write_log(self, msg):
        with open(os.path.join(self.inpath, 'volcano.log'), 'a') as fh:
            print("{}: {}".format(datetime.datetime.now().strftime("%Y%m%dT%H%M%SZ"), msg), file=fh)

    def _volcano_to_column_source(self):
        '''write columnsource_location.csv and columnsource_emissions.csv from volcano.xml'''
        location = os.path.join(self.path, "columnsource_location.csv")
        with open(location, 'wt') as lh:
            lh.write(self.volcano.get_columnsource_location())
        self.upload_files.add(location)

        emission = os.path.join(self.path, "columnsource_emission.csv")
        with open(emission, 'wt') as eh:
            eh.write(self.volcano.get_columnsource_emission())
        self.upload_files.add(emission)

    def _get_meteo_files(self):
        '''Create meteorology files in the output-directory of volcano.xml.
        This involves linking and copying of needed meteorology. and eventually
        addition of a few timesteps at the beginning of the run

        Args:
            overwrite_model_start_time: don't used the volcano meteo, but the provided, used only for testing

        Returns: list of meteorology files
        '''
        (ref_date, model_start_time) = self.volcano.get_meteo_dates()

        # TODO: this will only the todays 00 run, this is not flexible enough yet
        files = self.res.getECMeteorologyFiles(model_start_time, 72, ref_date)
        for i, file in enumerate(files):
            file_date = model_start_time + datetime.timedelta(days=i)
            outfile = os.path.join(self.path, "meteo{date}.nc".format(date=file_date.strftime("%Y%m%d")))
            if not os.path.exists(outfile):
                os.symlink(file, outfile)
            self.upload_files.add(outfile)
        # vlevel-definition
        vlevels = self.res.getVerticalLevelDefinition()
        vfile = os.path.join(self.path, "Vertical_levels.txt")
        with open(vfile, 'w') as vh:
            vh.write(vlevels)
            self.upload_files.add(vfile)

    def _get_restart_file(self):
        if (self.volcano.run_as_restart()):
            model_start_time = self.volcano.get_meteo_dates()[1]
            restart_file = os.path.join(self.path, "EMEP_IN_{date}.nc".format(date=model_start_time.strftime("%Y%m%d")))
            if (os.path.exists(restart_file)):
                self.upload_files.add(restart_file)

    def _create_job_script(self):
        job = self.res.get_job_script(self.hpcMachine)
        defs = {"rundir": self.rundir,
                "runtag": self.runtag} # year, month, day, hour, runhour
        defs["runhour"] = "{}".format(int(self.volcano.runTimeHours))
        start_time = self.volcano.get_meteo_dates()[1]
        defs["year"] = start_time.year
        defs["month"] = "{:02d}".format(start_time.month)
        defs["day"] = "{:02d}".format(start_time.day)
        defs["hour"] = "{:02d}".format(start_time.hour)

        filename = os.path.join(self.path, self.jobscript)
        with open(filename, 'wt') as jh:
            jh.write(job.format(**defs))
        self.upload_files.add(filename)

    def do_upload_files(self):
        self._write_log("uploading to {}:{}".format(self.hpcMachine, self.hpc_outdir))
        self.hpc.syscall("mkdir", ["-p", self.hpc_outdir])
        for f in self.upload_files:
            self._write_log("uploading '{}'".format(f))
            self.hpc.put_files([f], self.hpc_outdir, 600)

    def run_and_wait(self):
        '''Start the model and wait for it to finish

        Returns QJobStatus code
        '''
        self._write_log("removing old status on {}: {}".format(self.hpcMachine, self.hpc_outdir))
        self.hpc.syscall("rm", ["-f", os.path.join(self.hpc_outdir, self.statusfile)])
        self._write_log("starting run on hpc {}: {}".format(self.hpcMachine, self.hpc_outdir))
        remote_jobscript = os.path.join(self.hpc_outdir, self.jobscript)
        qjob = self.hpc.submit_job(remote_jobscript, [])
        qjob.status_file = StatusFile(os.path.join(self.hpc_outdir, self.statusfile), "finished")

        # wait for 60 minutes to finish, check every minute
        sleep_time = 60 # seconds
        count = 60 # * sleep_time
        status = self.hpc.get_status(qjob)
        while not (status == QJobStatus.finished or status == QJobStatus.failed):
            sleep(sleep_time)
            count -= 1
            if count == 0:
                break
            status = self.hpc.get_status(qjob)
            self._write_log("jobstatus on hpc {} jobid={}: {}".format(self.hpcMachine, qjob.jobid, status))

        return status

    def download_results(self):
        '''download the result-files, and rename them as appropriate'''
        start_time = self.volcano.get_meteo_dates()[1]
        tomorrow = (start_time + datetime.timedelta(days=1)).strftime("%Y%m%d")
        timestamp = self.timestamp.strftime("%Y%m%dT%H%M%S")

        file = 'eemep_hourInst.nc'
        self._write_log("downloading {}:{} to {}".format(file, self.hpcMachine, self.path))
        self.hpc.get_files([os.path.join(self.hpc_outdir, file)], self.path, 1200)
        os.rename(os.path.join(self.path, file),
                  os.path.join(self.path, 'eemep_hourInst_{}.nc'.format(timestamp)))

        file = 'eemep_hour.nc'
        self._write_log("downloading {}:{} to {}".format(file, self.hpcMachine, self.path))
        self.hpc.get_files([os.path.join(self.hpc_outdir, file)], self.path, 1200)
        os.rename(os.path.join(self.path, file),
                  os.path.join(self.path, 'eemep_hour_{}.nc'.format(timestamp)))


        file = 'EMEP_OUT_{}.nc'.format(tomorrow)
        self._write_log("downloading {}:{} to {}".format(file, self.hpcMachine, self.path))
        try :
             self.hpc.get_files([os.path.join(self.hpc_outdir, file)], self.path, 1200)
        except Exception as ex:
            # not dangerous if it fail, but remove file
            self._write_log("couldn't download '{}', ignoring: {}".format(file, ex.args))
            filename = os.path.join(self.hpc_outdir, file)
            if os.path.exists(filename): os.unlink(filename)
        else:
            os.rename(os.path.join(self.path, file),
                      os.path.join(self.path, 'EMEP_IN_{}.nc'.format(tomorrow)))

    def work(self):
        '''do the complete work, e.g. upload, run, wait and download'''
        self.do_upload_files()
        self.run_and_wait()
        self.download_results()

class TestModelRunner(unittest.TestCase):
    hpcMachine = "frost"
    doRun = False

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
        self.files = ('columnsource_location.csv', 'columnsource_emission.csv', 'eemep_script.job')
        for file in self.files:
            if (os.path.exists(file)):
                os.unlink(file)
        for f in os.scandir(self.dir):
            if f.is_symlink():
                os.unlink(f.path)


    def testModelRunner(self):
        mr = ModelRunner(self.dir, TestModelRunner.hpcMachine)
        self.assertTrue(len(self.files) == 3)
        #self.assertTrue(os.path.exists(self.files[0]))
        for x in self.files:
            self.assertTrue(os.path.exists(os.path.join(mr.path, x)), "file created: {}".format(x))
        meteo_count = 0
        for x in os.scandir(self.dir):
            if x.is_symlink() and re.search(r'meteo\d{8}.nc', x.path):
                meteo_count += 1
        self.assertEqual(meteo_count, 4, "meteo files created")


    @unittest.skipIf(doRun, "testet in upload and run")
    def test_upload_files(self):
        mr = ModelRunner(self.dir, TestModelRunner.hpcMachine)
        mr.do_upload_files()

    @unittest.skipIf(doRun == False, "no run")
    def test_upload_files_and_run(self):
        mr = ModelRunner(self.dir, TestModelRunner.hpcMachine)
        mr.do_upload_files()
        status = mr.run_and_wait()
        print(status)

    @unittest.skipIf(doRun == False, "no run")
    def test_download(self):
        mr = ModelRunner(self.dir, TestModelRunner.hpcMachine)
        mr.download_results()


if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
