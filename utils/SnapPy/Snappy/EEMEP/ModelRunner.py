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
from Snappy.EcMeteorologyCalculator import EcMeteorologyCalculator

"""
Created on Sep 2, 2016

@author: heikok
"""
from METNO.HPC import HPC, StatusFile, QJobStatus
import datetime
from netCDF4 import Dataset, num2date
import os
import glob
import re
import subprocess
import sys
from time import sleep, gmtime
import unittest
import logging
import tempfile

from Snappy.EEMEP.Resources import Resources
from Snappy.EEMEP.PostProcess import PostProcess
from Snappy.EEMEP.VolcanoRun import VolcanoRun
from Snappy.EEMEP.NppRun import NppRun
import Snappy.Resources


class AbortFile:
    """Abort control with a filename. Abort as soon as the file disappears.

    The file must be read and writable, or abort is not supported."""

    def __init__(self, filename):
        self.filename = None
        if filename:
            try:
                if os.path.exists(filename):
                    print(
                        "abortfile '{}' exists, removing".format(filename),
                        file=sys.stderr,
                    )
                    os.remove(filename)
                with open(filename, "wt") as fh:
                    fh.write("delete this file to abort processing")
                self.filename = filename
            except OSError:
                print(
                    "cannot write filename: '{}', modelrunner abort disabled".format(
                        filename
                    ),
                    sys.stderr,
                )

    def abortRequested(self):
        """return TRUE if abort requested, FALSE if we should continue"""
        if self.filename:
            if os.path.exists(self.filename):
                return False
            else:
                return True
        return False

    def __del__(self):
        if self.filename and os.path.exists(self.filename):
            try:
                os.remove(self.filename)
            except:
                pass


class ModelRunner:

    VOLCANO_FILENAME = "volcano.xml"
    NPP_FILENAME = "npp.xml"
    ABORT_FILENAME = "deleteToRequestAbort"
    OUTPUT_INSTANT_FILENAME = "eemep_hourInst.nc"
    OUTPUT_AVERAGE_FILENAME = "eemep_hour.nc"

    logger = None

    @staticmethod
    def getLogger(path=None):
        if path is None:
            if ModelRunner.logger is None:
                raise Exception(
                    "getLogger() called without being initialized with path"
                )
            return ModelRunner.logger

        ModelRunner.logger = logging.getLogger("ModelRunner")
        ModelRunner.logger.setLevel(logging.DEBUG)
        fmt = logging.Formatter("%(asctime)s: %(message)s", datefmt="%Y%m%dT%H%M%SZ")
        fmt.converter = gmtime  # Make sure we are using UTC time
        # logging to file
        fh = logging.FileHandler(os.path.join(path, "volcano.log"))
        fh.setLevel(logging.DEBUG)
        fh.setFormatter(fmt)
        # errors on stderr, too (e.g. for cron)
        sh = logging.StreamHandler(stream=sys.stderr)
        sh.setLevel(logging.WARNING)
        sh.setFormatter(fmt)
        # add the handlers
        ModelRunner.logger.addHandler(fh)
        ModelRunner.logger.addHandler(sh)
        return ModelRunner.logger

    def __init__(self, path, hpcMachine, npp=False):
        """
        for correct working logs, make sure to have ModelRunner.getLogger(path=...) called before initialization
        """
        self.npp = npp
        self.upload_files = set()
        self.timestamp = datetime.datetime.now()
        self.jobscript = "eemep_script.job"
        self.statusfile = "status"
        self.res = Resources()
        self.hpc = HPC.by_name(hpcMachine)
        self.hpcMachine = hpcMachine
        self.inpath = path

        # Set up logging
        self.logger = self.getLogger()

        self.rundir = self.res.getHPCRunDir(self.hpcMachine)
        if self.npp:
            volcano_path = os.path.join(path, ModelRunner.NPP_FILENAME)
        else:
            volcano_path = os.path.join(path, ModelRunner.VOLCANO_FILENAME)

        if not os.path.exists(volcano_path):
            raise Exception("no such file or directory: {}".format(volcano_path))

        if self.npp:
            self.volcano = NppRun(volcano_path)
        else:
            self.volcano = VolcanoRun(volcano_path)
        self.runtag = "eemep_{}".format(os.path.basename(self.volcano.outputDir))
        self.hpc_outdir = os.path.join(self.rundir, self.runtag)

        self.path = self.volcano.outputDir
        os.makedirs(name=self.path, exist_ok=True)
        self.abortRequest = AbortFile(
            os.path.join(self.inpath, ModelRunner.ABORT_FILENAME)
        )
        self._volcano_to_column_source()
        self._get_meteo_files()
        self._get_restart_file()
        self._create_job_script()

    def _volcano_to_column_source(self):
        """write columnsource_location.csv and columnsource_emissions.csv from volcano.xml"""
        location = os.path.join(self.path, "columnsource_location.csv")
        with open(location, "wt") as lh:
            lh.write(self.volcano.get_columnsource_location())
        self.upload_files.add(location)

        emission = os.path.join(self.path, "columnsource_emission.csv")
        with open(emission, "wt") as eh:
            eh.write(self.volcano.get_columnsource_emission())
        self.upload_files.add(emission)

    def _generate_meteo_file(self, outfile, date_files):
        """Generate a meteo file, eventually by concatenting several input-files to get a file with 8 timesteps

        Args:
           outfile: output filename
           date_files: list of pairs, each pair consisting of a input-file and the number of time-steps (3hourly) containted in this timestep
        """
        if os.path.islink(outfile):
            os.unlink(outfile)
        if os.path.isfile(outfile):
            if os.access(outfile, os.W_OK):
                os.unlink(outfile)
            elif os.access(outfile, os.R_OK):
                # file only readable, forced to that file
                return
        if date_files[0][1] == 8:
            # no file-concatenation needed, just create a link
            if not os.path.lexists(outfile):
                os.symlink(date_files[0][0], outfile)
        else:
            # find the number of steps needed for which file (latest date first)
            timesteps_in_file = 0
            use_steps = []
            for (file, tsteps) in date_files:
                newsteps = tsteps - timesteps_in_file
                if newsteps <= 0:
                    continue
                use_steps.append((file, newsteps, timesteps_in_file))
                timesteps_in_file = timesteps_in_file + newsteps
                assert timesteps_in_file <= 8
                if timesteps_in_file == 8:
                    break
            # create a list of all files needed (first date first) and
            # find the timesteps to select from the joined files. (from first date to last date)
            steps = []
            files = []
            pos = 0
            for (file, use_steps, skip_steps) in reversed(use_steps):
                for i in range(0, use_steps):
                    steps.append(pos)
                    pos = pos + 1
                pos = pos + skip_steps
                files.append('<netcdf location="{file}" />'.format(file=file))

            # run fimex on files/steps
            joinfiles = """<?xml version="1.0" encoding="UTF-8"?>
<netcdf xmlns="http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
<aggregation type="joinExisting">
    {files}
</aggregation>
</netcdf>
"""
            ncml_file = os.path.join(self.path, "joinMeteo.ncml")
            with open(ncml_file, "wt") as fh:
                fh.write(joinfiles.format(files="\n".join(files)))
            subprocess.call(
                args=[
                    "fimex",
                    "--input.file",
                    ncml_file,
                    "--output.file",
                    outfile,
                    "--output.type=nc4",
                    "--extract.pickDimension.name=time",
                    "--extract.pickDimension.list={}".format(
                        ",".join(str(x) for x in steps)
                    ),
                ]
            )

    def _get_meteo_files(self):
        """Create meteorology files in the output-directory of volcano.xml.
        This involves linking and copying of needed meteorology. and eventually
        addition of a few timesteps at the beginning of the run

        Returns: list of meteorology files
        """
        (ref_date, model_start_time) = self.volcano.get_meteo_dates()

        sres = Snappy.Resources.Resources()
        border = 7
        if (
            self.volcano.latitude > (sres.ecDefaultDomainStartY + border)
            and self.volcano.latitude
            < (sres.ecDefaultDomainStartY + sres.ecDomainHeight - border)
            and self.volcano.longitude > (sres.ecDefaultDomainStartX + border)
            and self.volcano.longitude
            < (sres.ecDefaultDomainStartX + sres.ecDomainWidth - border)
        ):
            files = self.res.getECMeteorologyFiles(model_start_time, 72, ref_date)
        else:
            self.logger.debug("Calculating Meteorology, takes about 15min")
            # make sure to use the 00UTC meteorology, eemep needs start-time at midnight
            start_time = model_start_time.replace(hour=3)
            ecMetCalc = EcMeteorologyCalculator(
                EcMeteorologyCalculator.getGlobalMeteoResources(),
                dtime=start_time,
                domainCenterX=self.volcano.longitude,
                domainCenterY=self.volcano.latitude,
            )
            ecMetCalc.calc()
            files = [[(x, 8)] for x in ecMetCalc.get_meteorology_files()]
            self.logger.debug("Meteorology calculated")

        for i, date_files in enumerate(files):
            file_date = model_start_time + datetime.timedelta(days=i)
            outfile = os.path.join(
                self.path, "meteo{date}.nc".format(date=file_date.strftime("%Y%m%d"))
            )
            self._generate_meteo_file(outfile, date_files)
            self.upload_files.add(outfile)
        # vlevel-definition
        vlevels = self.res.getVerticalLevelDefinition()
        vfile = os.path.join(self.path, "Vertical_levels.txt")
        with open(vfile, "w") as vh:
            vh.write(vlevels)
        self.upload_files.add(vfile)

    def _get_restart_file(self):
        if self.volcano.run_as_restart():
            model_start_time = self.volcano.get_meteo_dates()[1]
            restart_file = os.path.join(
                self.path,
                "EMEP_IN_{date}.nc".format(date=model_start_time.strftime("%Y%m%d")),
            )
            if os.path.exists(restart_file):
                self.upload_files.add(restart_file)

    def _create_job_script(self):
        npp_extension = "_npp" if self.npp else ""
        job = self.res.get_job_script(self.hpcMachine + npp_extension)
        defs = {
            "rundir": self.rundir,
            "runtag": self.runtag,
        }  # year, month, day, hour, runhour
        defs["runhour"] = "{}".format(int(self.volcano.runTimeHours))
        start_time = self.volcano.get_meteo_dates()[1]
        defs["year"] = start_time.year
        defs["month"] = "{:02d}".format(start_time.month)
        defs["day"] = "{:02d}".format(start_time.day)
        defs["hour"] = "{:02d}".format(start_time.hour)

        self.logger.debug(
            "Creating job script with the following definitions: {:s}".format(str(defs))
        )

        filename = os.path.join(self.path, self.jobscript)

        with open(filename, "wt") as jh:
            jh.write(job.format(**defs))
        self.upload_files.add(filename)

    def _write_log(self, msg):
        """Used by PostProcess to log"""
        self.logger.debug(msg)

    def clean_old_files(self):
        """Delete files fromprevious runs"""
        self.logger.debug(
            "cleaning files in {}:{}".format(self.hpcMachine, self.hpc_outdir)
        )

        hpc_files_to_delete = (
            list(self.upload_files)
            + [self.statusfile]
            + [ModelRunner.OUTPUT_AVERAGE_FILENAME]
            + [ModelRunner.OUTPUT_INSTANT_FILENAME]
        )

        hpc_files_to_delete = [os.path.basename(f) for f in hpc_files_to_delete]
        hpc_files_to_delete = [
            os.path.join(self.hpc_outdir, f) for f in hpc_files_to_delete
        ]
        self.hpc.syscall("rm", ["-f"] + hpc_files_to_delete)

    def do_upload_files(self):
        self.logger.debug("uploading to {}:{}".format(self.hpcMachine, self.hpc_outdir))
        self.hpc.syscall("mkdir", ["-p", self.hpc_outdir])
        for f in self.upload_files:
            self.logger.debug("uploading '{}'".format(f))
            self.hpc.put_files([f], self.hpc_outdir, 600)

    def get_run_file_ages(self):
        """Return age of files on HPC"""
        try:
            # Get current date and date of files on HPC
            hpc_date, cerr, retval = self.hpc.syscall("date", ["+%s"], timeout=30)
            if retval != 0:
                self.logger.error("Tried to get date, got cerr {:s}".format(cerr))
                return None
            files, cerr, retval = self.hpc.syscall("find", [self.hpc_outdir])
            if retval != 0:
                self.logger.error("Tried to call find, got cerr {:s}".format(cerr))
                return None
            stat_out, cerr, retval = self.hpc.syscall(
                "stat", ["-c", "%Y %n"] + files.splitlines()
            )
            if retval != 0:
                self.logger.error("Tried to call stat, got cerr {:s}".format(cerr))
                return None
        except Exception as ex:
            self.logger.debug(
                "Could not stat files on HPC machine: {ex}".format(ex=ex.args)
            )
            return None

        # Process dates to compute age of all files
        hpc_date = datetime.datetime.fromtimestamp(int(hpc_date))
        self.logger.debug("HPC date: '{}'".format(str(hpc_date)))
        file_age = {}
        for line in stat_out.splitlines():
            date, filename = line.split(" ")
            file_age[filename] = hpc_date - datetime.datetime.fromtimestamp(int(date))
            self.logger.debug(
                "Age of '{:s}' is {:s}".format(filename, str(file_age[filename]))
            )

        return file_age

    def run_and_wait(self):
        """Start the model and wait for it to finish

        Returns QJobStatus code
        """
        self.logger.debug(
            "starting run on hpc {}: {}".format(self.hpcMachine, self.hpc_outdir)
        )

        remote_jobscript = os.path.join(self.hpc_outdir, self.jobscript)
        qjob = self.hpc.submit_job(remote_jobscript, [])
        qjob.status_file = StatusFile(
            os.path.join(self.hpc_outdir, self.statusfile), "finished"
        )

        # wait for 60 minutes to finish, check every minute
        sleep_time = 60  # seconds
        count = 60  # * sleep_time
        status = self.hpc.get_status(qjob)
        while not (status == QJobStatus.finished or status == QJobStatus.failed):
            sleep(sleep_time)
            count -= 1
            if count == 0 or self.abortRequest.abortRequested():
                self.hpc.delete_job(qjob)
                break
            status = self.hpc.get_status(qjob)
            self.logger.debug(
                "jobstatus on hpc {} jobid={}: {}".format(
                    self.hpcMachine, qjob.jobid, status
                )
            )

        return status

    def download_results(self):
        """download the result-files, and rename them as appropriate"""
        start_time = self.volcano.get_meteo_dates()[1]
        tomorrow = (start_time + datetime.timedelta(days=1)).strftime("%Y%m%d")

        # Get age of files on HPC
        file_age = self.get_run_file_ages()
        if file_age is None:
            self.logger.error(
                f"Could not get run-file ages on {self.hpcMachine} - something is wrong!"
            )
            return

        # Download output files
        for filename in [
            ModelRunner.OUTPUT_AVERAGE_FILENAME,
            ModelRunner.OUTPUT_INSTANT_FILENAME,
        ]:
            filename_local = os.path.join(self.path, filename)
            filename = os.path.join(self.hpc_outdir, filename)
            if filename in file_age:
                age = file_age[filename] / datetime.timedelta(minutes=1)
            else:
                age = 999
            if age > 120:
                self.logger.error(
                    "File {} too old on {}".format(filename, self.hpcMachine)
                )
                return
            self.logger.debug(
                "downloading {}:{} to {}".format(filename, self.hpcMachine, self.path)
            )
            self.hpc.get_files([filename], self.path, 1200)

            # Check sanity of output results
            try:
                with Dataset(filename_local) as nc_file:
                    time_var = nc_file["time"]
                    times = num2date(time_var[:], units=time_var.units).astype(
                        "datetime64[ns]"
                    )
            except Exception as e:
                self.logger.error(
                    "Unable to open NetCDF file {:s}: {:s}".format(
                        filename_local, str(e)
                    )
                )
                return
            self.logger.debug(
                "File {:s} contains the following timesteps: {:s}..{:s}".format(
                    filename_local, str(times[0]), str(times[-1])
                )
            )
            if len(times) < self.volcano.runTimeHours:
                self.logger.warning(
                    "WARNING: File {:s} appears not to have the correct timesteps!".format(
                        filename_local
                    )
                )

        # Download initial conditions for continued run
        file = "EMEP_OUT_{}.nc".format(tomorrow)
        age = file_age.pop(os.path.join(self.hpc_outdir, file), None)
        if age is None:
            self.logger.error(
                "File {} does not exist on {}".format(file, self.hpcMachine)
            )
            return
        if age / datetime.timedelta(minutes=1) > 120:
            self.logger.error("File {} too old on {}".format(file, self.hpcMachine))
            return
        self.logger.debug(
            "downloading {}:{} to {}".format(file, self.hpcMachine, self.path)
        )
        try:
            self.hpc.get_files([os.path.join(self.hpc_outdir, file)], self.path, 1200)
        except Exception as ex:
            # not dangerous if it fail, but remove file
            self.logger.debug(
                "couldn't download '{}', ignoring: {}".format(file, ex.args)
            )
            filename = os.path.join(self.path, file)
            if os.path.lexists(filename):
                os.unlink(filename)
        else:
            os.rename(
                os.path.join(self.path, file),
                os.path.join(self.path, "EMEP_IN_{}.nc".format(tomorrow)),
            )

        # Postprocess
        pp = PostProcess(self.path, self.timestamp, logger=self)
        if self.npp:
            pp.accumulate_and_toa_nuc_files(
                os.path.join(self.path, ModelRunner.OUTPUT_INSTANT_FILENAME),
                os.path.join(self.path, ModelRunner.OUTPUT_AVERAGE_FILENAME),
            )
        else:
            pp.convert_files(
                os.path.join(self.path, ModelRunner.OUTPUT_INSTANT_FILENAME),
                os.path.join(self.path, ModelRunner.OUTPUT_AVERAGE_FILENAME),
            )

        # cleanup softlinks in output-dir
        findArgs = [self.hpc_outdir, "-type", "l", "-delete"]
        try:
            self.hpc.syscall("find", findArgs, timeout=30)
        except Exception as ex:
            self.logger.warning(
                "cannot excecute command 'find {args}': {ex}".format(
                    args=" ".join(findArgs), ex=ex.args
                )
            )

    def work(self):
        """do the complete work, e.g. upload, run, wait and download"""
        self.clean_old_files()
        self.do_upload_files()
        status = self.run_and_wait()
        if status == QJobStatus.failed:
            self.logger.error("HPC-job failed: Not downloading any results.")
        elif status == QJobStatus.queued:
            self.logger.error(
                "HPC-resource not available on {}, giving up.".format(self.hpcMachine)
            )
        elif status == QJobStatus.running:
            self.logger.error(
                "HPC-job on {} not finished in time, downloading partial".format(
                    self.hpcMachine
                )
            )
        else:
            self.download_results()


class TestModelRunner(unittest.TestCase):
    hpcMachine = "ppi_centos7_direct"
    doRun = True

    def setUp(self):
        self.logger = logging.getLogger("TestModelRunner")
        unittest.TestCase.setUp(self)

        self.indir = os.path.join(os.path.dirname(__file__), "test")
        self.logger.debug("Input dir: {:s}".format(self.indir))

        volcanoFile = os.path.join(self.indir, "volcano.xml")
        self.logger.debug("Input volcano file: {:s}".format(volcanoFile))
        volc = VolcanoRun(volcanoFile)

        self.dir = tempfile.TemporaryDirectory(prefix="volcano_download_")
        self.logger.debug("Download directory: {:s}".format(self.dir.name))

        yesterday = datetime.datetime.now() - datetime.timedelta(days=1)
        with open(os.path.join(self.dir.name, "volcano.xml"), "wt") as oh:
            with open(volcanoFile, "rt") as ih:
                for line in ih:
                    line = re.sub("2016-11-03", yesterday.strftime("%Y-%m-%d"), line)
                    oh.write(line)
        self.files = (
            "columnsource_location.csv",
            "columnsource_emission.csv",
            "eemep_script.job",
        )
        self.output_files = ("eemep_hourInst.nc", "eemep_hour.nc")

    def testModelRunner(self):
        # Create model runner
        mr = ModelRunner(self.dir.name, TestModelRunner.hpcMachine)
        self.logger.debug(
            "Modelrunner setup complete, local outdir is {:s}".format(mr.hpc_outdir)
        )
        self.logger.debug(
            "Modelrunner setup complete, HPC outdir is {:s}".format(mr.hpc_outdir)
        )

        # Test uploading of files
        mr.do_upload_files()
        self.logger.debug("Files uploaded")

        file_ages = mr.get_run_file_ages()
        self.assertTrue(file_ages is not None, "Could not get file ages!")

        # Check that we find meteo files
        meteo_count = 0
        for x in file_ages.keys():
            self.logger.debug("Found file {:s}".format(x))
            if re.search(r"meteo\d{8}.nc", x):
                self.logger.debug("Found meteo file {:s}".format(x))
                meteo_count += 1
        # FIXME: This changes. 09:38 it gives 3 files. +36 hours related, i.e., should give 4 when after 12, and 3 between 00 and 12?
        self.assertTrue(meteo_count >= 3, msg="Meteo files not created!")

        # Check that we find the expected config files
        for filename in self.files:
            filename = os.path.join(mr.hpc_outdir, filename)
            self.assertTrue(
                filename in file_ages.keys(), "Could not find {:s}".format(filename)
            )
            self.logger.debug(
                "Input file '{:s}' is {:s} old".format(
                    filename, str(file_ages[filename])
                )
            )
            self.assertTrue(file_ages[filename] / datetime.timedelta(minutes=1) < 15)

        if self.doRun == False:
            self.logger.debug("Skipping remainder of test - not doRun is false")
        else:
            # Test running.
            status = mr.run_and_wait()
            self.assertTrue(
                status == QJobStatus.finished,
                "Run and wait returned unexpected status {:s}".format(str(status)),
            )
            file_ages = mr.get_run_file_ages()
            self.assertTrue(len(file_ages.keys()) > len(self.files))
            for filename in [
                ModelRunner.OUTPUT_INSTANT_FILENAME,
                ModelRunner.OUTPUT_AVERAGE_FILENAME,
            ]:
                filename = os.path.join(mr.hpc_outdir, filename)
                self.assertTrue(filename in file_ages.keys(), " ")
                self.logger.debug(
                    "Output file '{:s}' is {:s} minutes old".format(
                        filename, str(file_ages[filename])
                    )
                )
                self.assertTrue(
                    file_ages[filename] / datetime.timedelta(minutes=1) < 15
                )

            # Test downloading / postprocessing
            mr.download_results()
            for pattern in ["eemep_hourInst_*.nc", "eemep_hour_*.nc", "EMEP_IN_*.nc"]:
                self.logger.debug("Checking for pattern '{:s}'".format(pattern))
                files = glob.glob(os.path.join(mr.path, pattern))
                timestamp_ok = False
                for f in files:
                    age = datetime.datetime.now() - datetime.datetime.fromtimestamp(
                        os.path.getmtime(f)
                    )
                    self.logger.debug("Found {:s} with age {:s}".format(f, str(age)))
                    if age / datetime.timedelta(minutes=1) < 120:
                        self.logger.debug("Age OK, skipping remaining files")
                        timestamp_ok = True
                self.assertTrue(
                    timestamp_ok,
                    msg="Could not find file matching {:s}".format(pattern),
                )

            # Test cleanup
            mr.clean_old_files()
            file_ages = mr.get_run_file_ages()
            self.assertTrue(file_ages is not None, msg="")
            self.assertTrue(
                len(file_ages.keys()) >= len(self.files),
                msg="Too few files in output directory!",
            )
            for filename in [
                ModelRunner.OUTPUT_INSTANT_FILENAME,
                ModelRunner.OUTPUT_AVERAGE_FILENAME,
                mr.statusfile,
            ]:
                filename = os.path.join(mr.hpc_outdir, filename)
                self.assertFalse(filename in file_ages.keys(), " ")

    @unittest.skipIf(doRun == False, "Do run is false")
    def testWork(self):
        # Create model runner and test
        mr = ModelRunner(self.dir.name, TestModelRunner.hpcMachine)
        mr.work()

        for pattern in ["eemep_hourInst_*.nc", "eemep_hour_*.nc", "EMEP_IN_*.nc"]:
            self.logger.debug("Checking for pattern '{:s}'".format(pattern))
            files = glob.glob(os.path.join(mr.path, pattern))
            timestamp_ok = False
            for f in files:
                age = datetime.datetime.now() - datetime.datetime.fromtimestamp(
                    os.path.getmtime(f)
                )
                self.logger.debug("Found {:s} with age {:s}".format(f, str(age)))
                if age / datetime.timedelta(minutes=1) < 120:
                    self.logger.debug("Age OK, skipping remaining files")
                    timestamp_ok = True
            self.assertTrue(
                timestamp_ok, msg="Could not find file matching {:s}".format(pattern)
            )


if __name__ == "__main__":
    logging.basicConfig(
        format="%(asctime)s: %(message)s", datefmt="%Y%m%dT%H%M%SZ", stream=sys.stderr
    )
    logging.root.setLevel(logging.NOTSET)
    # logging.getLogger("TestModelRunner").setLevel(logging.DEBUG)
    # logging.getLogger("ModelRunner").

    # Do not sort tests

    self.logger.warning("This test takes 1-2 hours to complete")

    unittest.TestLoader.sortTestMethodsUsing = None
    unittest.main(verbosity=2, failfast=True)
