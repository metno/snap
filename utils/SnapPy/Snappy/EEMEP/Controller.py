import datetime
import getpass
import json
import os
import pwd
import re
import subprocess
import sys
import time
import traceback

from PyQt5 import QtWidgets
from PyQt5.QtCore import (
    QThread,
    pyqtSignal,
)

import Snappy.Utils
from Snappy.BrowserWidget import BrowserWidget
from Snappy.EEMEP.ModelRunner import ModelRunner
from Snappy.EEMEP.Resources import Resources


def getFileOwner(filename):
    pwuid = pwd.getpwuid(os.stat(filename).st_uid)
    return pwuid.pw_name, pwuid.pw_gecos


def debug(*objs):
    print("DEBUG: ", *objs, file=sys.stderr)


class _UpdateThread(QThread):
    update_log_signal = pyqtSignal()

    def __init__(self, controller):
        super().__init__()
        self.controller = controller

    def __del__(self):
        try:
            if self.isRunning():
                self.wait()
        except RuntimeError:
            pass  # C++ object already deleted

    def run(self):
        debug("run-status:" + self.controller.eemepRunning)
        try:
            while self.controller.eemepRunning == "running":
                debug("running")
                self.update_log_signal.emit()
                self.sleep(3)
        except Exception:
            traceback.print_exc()


class SnapVolcanoWorker(QThread):
    finished = pyqtSignal()

    def __init__(self, outputdir, volcanofile):
        super().__init__()
        self.outputdir = outputdir
        self.volcanofile = volcanofile

    def run(self):
        """Run the snapVolcano as subprocess command."""

        dtnow = datetime.datetime.now()
        with open(os.path.join(self.outputdir, "snapVolcano.log"), "w") as fh:
            subprocess.Popen(
                [
                    "snapVolcano",
                    self.volcanofile,
                    "--snapnc",
                    f"snapash_{dtnow:%Y%m%dT%H%M%S}.nc",
                ],
                stdout=fh,
                stderr=fh,
                cwd=self.outputdir,
                start_new_session=True,  # Detach from parent process
            )
        self.finished.emit()


class Controller:
    """
    Controller for EEMEP Widget. Starts the browserwidget as self.main and connects it to the form handler
    """

    def __init__(self):
        """
        Initialize Widget and handlers
        """
        self.res = Resources()
        self.main = BrowserWidget()
        self.main.set_html(self.res.getStartScreen())
        self.main.set_form_handler(self._create_form_handler())
        self.main.show()
        self.eemepRunning = "inactive"
        self.lastOutputDir = ""
        self.lastQDict = {}
        self.volcano_file = ""
        self.volcano_logfile = ""
        self.lastLog = []
        self.logfile_size = 0

    def write_log(self, txt: str, max_lines=30, clear_log=False):
        if clear_log:
            self.lastLog = [txt]
        else:
            self.lastLog += txt.splitlines()
        debug(txt)

        # Write at most 30 lines to screen
        if len(self.lastLog) > max_lines:
            self.lastLog = self.lastLog[-max_lines:]

        self.main.evaluate_javaScript(
            "updateEemepLog({0});".format(json.dumps("\n".join(self.lastLog)))
        )

    def update_log_query(self, qDict):
        # MainBrowserWindow._default_form_handler(qDict)
        # self.write_log("updating...")

        if os.path.isfile(self.volcano_logfile):
            current_size = os.path.getsize(self.volcano_logfile)

            # Log overwritten - new file (this should not happen)
            if current_size < self.logfile_size:
                self.write_log(
                    "WARNING: Logfile overwritten - someone else is running this volcano also"
                )
                self.logfile_size = 0

            # If new content in logfile
            if current_size > self.logfile_size:
                with open(self.volcano_logfile) as lf:
                    lf.seek(self.logfile_size)
                    for line in lf:
                        self.write_log(line)
                self.logfile_size = current_size
        else:
            if os.path.isfile(self.volcano_file):
                self.write_log(
                    "Queue busy {:%Y-%m-%d %H:%M:%S}".format(datetime.datetime.now())
                )
                if self.res.getModelRunnerLogs():
                    self.write_log(self.res.getModelRunnerLogs())
            else:
                # Check if volcano logfile exists after waiting slightly
                # (to avoid race conditions in which volcano.xml is deleted before logfile is created)
                time.sleep(1.0)
                if os.path.isfile(self.volcano_logfile):
                    self.write_log(
                        "ERROR: Neither '{:s}' \nnor '{:s}' Exists!\nSomeone may have deleted the run.".format(
                            self.volcano_file, self.volcano_logfile
                        )
                    )
                    self.eemepRunning = "inactive"

    def cancel_first_in_queue(self, qDict):
        """Mark all currently active model-runs for abort"""
        for dirpath, dirs, files in os.walk(self.res.getOutputDir()):
            for file in files:
                if file == ModelRunner.ABORT_FILENAME:
                    try:
                        self.write_log("trying to abort {}".format(dirpath))
                        abortLogFile = (
                            datetime.datetime.now()
                            .strftime("{fname}_%Y%m%d-%H%M%S")
                            .format(fname=ModelRunner.ABORT_FILENAME)
                        )
                        with open(os.path.join(dirpath, abortLogFile), "wt") as lh:
                            lh.write("aborted by {}".format(getpass.getuser()))
                        os.remove(os.path.join(dirpath, file))
                    except Exception:
                        traceback.print_exc()
                        self.write_log("aborting {} failed!".format(dirpath))
        pass

    def cancel_submitted(self, qDict):
        """Cancel the last submitted volcano-file"""
        if os.path.isfile(self.volcano_file):
            owner, gecos = getFileOwner(self.volcano_file)
            user = getpass.getuser()
            debug(
                "Deleting {:s} owned by {:s} ({:s}) with user {:s}".format(
                    self.volcano_file, owner, gecos, user
                )
            )
            if owner != user:
                self.write_log(
                    "WARNING: {:s}\nwas started by {:s} ({:s}). Please notify this user that you canceled the run!".format(
                        self.volcano_file, owner, gecos
                    )
                )
            try:
                os.remove(os.path.join(self.volcano_file))
                self.write_log("{} deleted".format(self.volcano_file))
                self.eemepRunning = "inactive"
            except Exception as e:
                self.write_log(
                    "ERROR: could not cancel the currently submitted volcano!\n Error was {:s}".format(
                        e.msg
                    )
                )
                pass
        else:
            self.write_log(
                "Volcano file ('{:s}') does not exist".format(self.volcano_file)
            )

    def update_log(self):
        self.update_log_query({})

    def _create_form_handler(self):
        def handler(queryDict):
            """a form-handler with closure for self"""
            options = {
                "Run": self.run_eemep_query,
                "Update": self.update_log_query,
                "Cancel+active": self.cancel_first_in_queue,
                "Cancel+submitted": self.cancel_submitted,
            }
            # mapping from QList<QPair> to simple dictionary
            qDict = dict()
            for key, value in queryDict:
                qDict[key] = value
            # calling the correct handler depending on the module
            try:
                options[qDict["action"]](qDict)
            except TypeError as ex:
                self.write_log("type-error: {}".format(ex))
            except ValueError as ex:
                self.write_log("value-error: {}".format(ex))
            except:
                self.write_log(
                    "Unexpected error on {0}: {1}".format(
                        qDict["action"], sys.exc_info()[0]
                    )
                )
                raise

        return handler

    def run_eemep_query(self, qDict):
        # make sure all files are rw for everybody (for later deletion)
        os.umask(0)
        self.write_log("Running EEMEP query", clear_log=True)
        debug("run_eemep_query")
        for key, value in qDict.items():
            print(f"{key} => {value}")
        errors = ""
        match = re.search(
            r"(\d{4})-(\d{2})-(\d{2})[\+\s]+(\d{1,2})(?::(\d{2}))?", qDict["startTime"]
        )
        if match:
            if match.group(5):  # including minutes
                startTime = f"{match.group(1, 2, 3, 4, 5)}"
                startDT = datetime.datetime(
                    *tuple(map(int, list(match.group(1, 2, 3, 4, 5))))
                )
            else:
                startTime = f"{match.group(1, 2, 3, 4)}"
                startDT = datetime.datetime(
                    *tuple(map(int, list(match.group(1, 2, 3, 4))))
                )
            modelStartDT = datetime.datetime(
                startDT.year, startDT.month, startDT.day, 0, 0, 0
            )
        else:
            errors += f"Cannot interpret startTime: {qDict['startTime']}\n"

        try:
            runTime = int(qDict["runTime"])
        except Exception:
            errors += f"Cannot interpret runTime: {qDict['runTime']}\n"

        restart = "false"
        if "restart_file" in qDict and qDict["restart_file"].lower() == "true":
            restart = "restart"

        if qDict["volcanotype"] == "default":
            type = "M0"
        else:
            type = qDict["volcanotype"]
        volcanoes = self.res.readVolcanoes()
        if qDict["volcano"] and volcanoes[qDict["volcano"]]:
            volcano = re.sub(r"[^\w.-_]", "_", volcanoes[qDict["volcano"]]["NAME"])
            latf = volcanoes[qDict["volcano"]]["LATITUDE"]
            lonf = volcanoes[qDict["volcano"]]["LONGITUDE"]
            altf = volcanoes[qDict["volcano"]]["ELEV"]
            if qDict["volcanotype"] == "default":
                type = volcanoes[qDict["volcano"]]["ERUPTIONTYPE"]
        else:
            lat = qDict["latitude"]
            lon = qDict["longitude"]
            alt = qDict["altitude"]
            try:
                latf = Snappy.Utils.parseLat(lat)
                lonf = Snappy.Utils.parseLon(lon)
                altf = float(alt)
            except Exception:
                latf = 0.0
                lonf = 0.0
                altf = 0.0
                errors += (
                    f"Cannot interpret latitude/longitude/altitude: {lat}/{lon}/{alt}\n"
                )
            volcano = f"{latf}N_{lonf}E"

        debug(f"volcano: {volcano} {latf:.2f} {lonf:.2f} {altf} {type}")

        try:
            volctype = self.res.readVolcanoType(type)
        except Exception as ex:
            errors += str(ex) + "\n"
            errors += "Please select Height and Type (Advanced) manually.\n"

        # fine-ash fraction (particles < 63µm)
        try:
            m63 = qDict["m63"]
            if m63.lower() == "mastin":
                m63f = volctype["m63"]
            else:
                m63f = float(m63)
                if not (0 < m63f <= 1):
                    raise Exception(f"fine-ash fraction out of range 0-1: {m63f}")
        except Exception as ex:
            errors += str(ex) + "\n"

        self.write_log(
            f"working with {volcano:s} (lat={latf:.2f}N lon={lonf:.2f}E) starting at {startTime:s}"
        )

        # Get cloud height if supplied and calculate eruption rate
        if qDict["cloudheight"]:
            try:
                cheight = float(qDict["cloudheight"])
            except Exception:
                errors += f"cannot interpret cloudheight (m): {qDict['cloudheight']}\n"

            if cheight % 1 != 0:
                self.write_log(
                    "WARNING: Ash cloud height supplied with fraction. Please check that you supplied meters, not km!"
                )

            if qDict["cloudheight_datum"] == "mean_sea_level":
                # Interpret cloud height as above sea level
                # - remove volcano vent altitude to get plume height
                self.write_log(
                    f"Ash cloud height measured from mean sea level: {cheight / 1000.0:.2f} km"
                )
                cheight = cheight - altf

            elif qDict["cloudheight_datum"] == "vent":
                # Interpret cloud height as above vent
                pass

            else:
                errors += f"cannot interpret cloud height datum: {qDict['cloudheight_datum']:s}"

            # rate in kg/s from Mastin et al. 2009, formular (1) and a volume (DRE) (m3) to
            # mass (kg) density of 2500kg/m3
            rate = 2500.0 * ((0.5 * max(0, cheight) / 1000.0) ** (1.0 / 0.241))
        else:
            cheight = float(volctype["H"]) * 1000  # km -> m
            rate = float(volctype["dM/dt"])

        # Check negative ash cloud height
        if cheight <= 0:
            errors += (
                f"Negative cloud height {cheight / 1000.0:.2f}! Please check ash cloud."
            )
        self.write_log(
            f"Ash cloud height measured from volcano: {cheight / 1000.0:.2f} km, rate: {rate:.0f} kg/s, volcano height: {altf / 1000.0:.2f} km."
        )

        # Abort if errors
        if len(errors) > 0:
            debug('updateLog("{0}");'.format(json.dumps("ERRORS:\n" + errors)))
            self.write_log(f"ERRORS:\n{errors}")
            return

        # eEMEP runs up-to 23 km, so remove all ash above 23 km,
        # See Varsling av vulkanaske i norsk luftrom - driftsfase,
        # February 2020 for details
        eemep_cheight_max = 23000.0 - altf
        if cheight > eemep_cheight_max:
            rate_fraction = eemep_cheight_max / cheight
            self.write_log(
                "Cropping ash cloud to {:.2f} km from {:.2f} km using factor {:.3f}".format(
                    eemep_cheight_max / 1000.0, cheight / 1000.0, rate_fraction
                )
            )
            rate = rate * rate_fraction
            cheight = eemep_cheight_max

        eruptions = []
        eruption = '<eruption start="{start}Z" end="{end}Z" bottom="{bottom:.0f}" top="{top:.0f}" rate="{rate:.0f}" m63="{m63:.2f}"/>\n'
        eruptions.append(
            eruption.format(
                start=startDT.isoformat(),
                end=(startDT + datetime.timedelta(hours=runTime)).isoformat(),
                bottom=0,
                top=cheight,
                rate=rate,
                m63=m63f,
            )
        )

        self.lastOutputDir = os.path.join(
            self.res.getOutputDir(), f"{volcano}_ondemand"
        )
        self.volcano_file = os.path.join(
            self.lastOutputDir, ModelRunner.VOLCANO_FILENAME
        )
        self.lastQDict = qDict
        ecModelRun = qDict["ecmodelrun"]
        if not ecModelRun == "best":
            ecModelRun += "Z"
        snapMetModel = qDict["snap_metmodel"]
        snapModelRun = qDict["snap_modelrun"]
        if snapMetModel == "":
            snapsetup = ""
        else:
            snapsetup = f"""
  <!-- optional snap setup -->
  <snap_model_setup>
    <weather_forecast reference_date="{snapModelRun}" model="{snapMetModel}" />
  </snap_model_setup>
            """

        self.lastSourceTerm = f"""<?xml version="1.0" encoding="UTF-8"?>
<volcanic_eruption_run run_time_hours="{runTime}" output_directory="{self.lastOutputDir}">
<model_setup use_restart_file="{restart}">
   <!-- reference_date might also be best_estimate, e.g. mix latest forecasts -->
   <weather_forecast reference_date="{ecModelRun}" model_start_time="{modelStartDT.isoformat()}Z"/>
</model_setup>
{snapsetup}
<volcano name="{volcano}" lat="{latf:.3f}" lon="{lonf:.3f}" altitude="{altf:.0f}" />
<eruptions>
<!-- bottom and top of ash-cloud in m above ground -->
<!-- rate in kg/s -->
{"".join(eruptions)}
</eruptions>

</volcanic_eruption_run>"""
        debug(f"output directory: {self.lastOutputDir}")
        os.makedirs(self.lastOutputDir, exist_ok=True)

        self.volcano_logfile = os.path.join(self.lastOutputDir, "volcano.log")
        if os.path.exists(self.volcano_logfile):
            logdate = datetime.datetime.fromtimestamp(
                os.path.getmtime(self.volcano_logfile)
            )
            os.rename(
                self.volcano_logfile,
                f"{self.volcano_logfile}_{logdate.strftime('%Y%m%dT%H%M%S')}",
            )
        try:
            # Mode x - open for exclusive creation, failing if the file already exists
            with open(self.volcano_file, "x") as fh:
                fh.write(self.lastSourceTerm)
        except FileExistsError:
            owner = "unknown"
            if os.path.exists(self.volcano_file):
                owner, gecos = getFileOwner(self.volcano_file)
            errmsg = f"ERROR: Run ({self.volcano_file:s}) already exists!\nCreated by user {owner:s} ({gecos:s}).\nPlease try again later."
            debug(f'updateLog("{json.dumps(errmsg)}");')
            self.write_log(errmsg)
            return

        self.eemepRunning = "running"

        self.model_update = _UpdateThread(self)
        self.model_update.update_log_signal.connect(self.update_log)
        self.model_update.start(QThread.LowPriority)

        if snapsetup != "":
            self.write_log(f"starting a background snapash run in {self.lastOutputDir}")
            self.snap_thread = SnapVolcanoWorker(self.lastOutputDir, self.volcano_file)
            self.snap_thread.start()


if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    ctr = Controller()
    sys.exit(app.exec_())
