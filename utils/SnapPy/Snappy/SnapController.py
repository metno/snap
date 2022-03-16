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
import datetime
import json
import os
import re
import sys
from time import gmtime, strftime
import traceback

from PyQt5 import QtWidgets, QtGui
from PyQt5.QtCore import (
    QProcess,
    QProcessEnvironment,
    QThread,
    QIODevice,
    QThreadPool,
    pyqtSignal,
)
from Snappy.BrowserWidget import BrowserWidget
from Snappy.EcMeteorologyCalculator import EcMeteorologyCalculator
from Snappy.ICONMeteorologyCalculator import ICONMeteorologyCalculator
from Snappy.MeteorologyCalculator import MeteoDataNotAvailableException
from Snappy.MailImages import sendPngsFromDir
from Snappy.Resources import Resources, MetModel
import Snappy.Utils


def debug(*objs):
    print("DEBUG: ", *objs, file=sys.stderr)


class SnapUpdateThread(QThread):
    update_log_signal = pyqtSignal()

    def __init__(self, snapController):
        QThread.__init__(self)
        self.snap_controller = snapController

    def __del__(self):
        self.wait()

    def run(self):
        debug("run-status:" + self.snap_controller.snapRunning)
        try:
            while self.snap_controller.snapRunning == "running":
                debug("running")
                debug(
                    tail(
                        os.path.join(
                            self.snap_controller.lastOutputDir, "snap.log.out"
                        ),
                        10,
                    )
                )
                self.update_log_signal.emit()
                self.sleep(3)
        except Exception:
            traceback.print_exc()


class SnapRun:
    @staticmethod
    def getQProcess(snap_controller):
        """initialize a QProcess with the correct environment and output-files"""
        proc = QProcess()
        proc.setWorkingDirectory(snap_controller.lastOutputDir)
        proc.setStandardOutputFile(
            os.path.join(snap_controller.lastOutputDir, "snap.log.out"),
            QIODevice.Append,
        )
        proc.setStandardErrorFile(
            os.path.join(snap_controller.lastOutputDir, "snap.log.out"),
            QIODevice.Append,
        )
        env = QProcessEnvironment.systemEnvironment()
        env.insert("OMP_NUM_THREADS", "1")
        proc.setProcessEnvironment(env)
        return proc

    def __init__(self, snap_controller):
        self.snap_controller = snap_controller
        self.proc = SnapRun.getQProcess(snap_controller)

    def start(self):
        debug("outputdir: " + self.snap_controller.lastOutputDir)
        #         self.proc.start('/home/heikok/sleepLong.sh', ['snap.input'])
        self.proc.start("/usr/bin/bsnap_naccident", ["snap.input"])
        if self.proc.waitForStarted(3000):
            self.snap_controller.snapRunning = "running"
            debug("started: " + self.snap_controller.snapRunning)
        else:
            self.snap_controller.write_log(
                "starting /usr/bin/bsnap_naccident snap.input failed"
            )


class SnapController:
    def __init__(self):
        self.res = Resources()
        self.main = BrowserWidget()
        self.main.set_html(self.res.getStartScreen())
        self.main.set_form_handler(self._create_snap_form_handler())
        self.main.show()
        self.snapRunning = "inactive"
        self.lastOutputDir = ""
        self.lastQDict = {}

    def write_log(self, txt: str):
        debug(txt)
        self.main.evaluate_javaScript("updateSnapLog({0});".format(json.dumps(txt)))

    def _snap_finished(self):
        debug("finished")
        self.snapRunning = "finished"
        self.results_add_toa()
        self.plot_results()
        with open(os.path.join(self.lastOutputDir, "snap.log.out"), "a") as logFile:
            logFile.write(
                "All work finished. Please open diana to see results.\nResults in {}\n".format(
                    self.lastOutputDir
                )
            )
        self.update_log()

    def _met_calculate_and_run(self):
        if self.metcalc.must_calc():
            proc = SnapRun.getQProcess(self)
            proc.finished.connect(self._met_finished_run_snap)
            self.metcalc.calc(proc)
            if proc.waitForStarted(3000):
                self.snapRunning = "running"
            # start background logging thread
            self.snap_update = SnapUpdateThread(self)
            self.snap_update.update_log_signal.connect(self.update_log)
            self.snap_update.start(QThread.LowPriority)
        else:
            self._met_finished_run_snap()

    def _met_finished_run_snap(self):
        debug("ec_finished")
        self.snapRunning = "finished"  # quit update-thread
        if self.metcalc.must_calc():
            with open(os.path.join(self.lastOutputDir, "snap.log.out"), "a") as logFile:
                logFile.write(
                    "Meteorology not generated.\nErrors in {}\n".format(
                        self.lastOutputDir
                    )
                )
            self.update_log()
            return

        metdefs = self.res.getDefaultMetDefinitions(self.lastQDict["metmodel"])
        (metdefs["startX"], metdefs["startY"]) = self.metcalc.get_grid_startX_Y()
        with open(os.path.join(self.lastOutputDir, "snap.input"), "a") as fh:
            fh.write(
                self.res.getSnapInputMetDefinitions(
                    self.lastQDict["metmodel"],
                    self.metcalc.get_meteorology_files(),
                    **metdefs
                )
            )
        self._snap_model_run()

    def results_add_toa(self):
        proc = SnapRun.getQProcess(self)
        proc.start("snapAddToa", ["snap.nc"])
        proc.waitForFinished(-1)

    def plot_results(self):
        match = re.search(r"(\d*\.\d+)", self.lastQDict["dianaversion"])
        if match:
            diVersion = "-{}".format(match.group(1))
        else:
            diVersion = ""

        if "region" in self.lastQDict and self.lastQDict["region"]:
            prod_dir = os.path.join(self.lastOutputDir, "prod")
            os.mkdir(prod_dir)
            with open(os.path.join(prod_dir, "diana.setup"), "wt") as fh:
                di_setup = """
%include /etc/diana/setup/diana.setup-COMMON
<FIELD_FILES>
filegroup=SNAP
m=SNAP.current t=fimex format=netcdf f={}
</FIELD_FILES>
"""
                fh.write(di_setup.format(os.path.join(self.lastOutputDir, "snap.nc")))

            hours = ",".join(
                [
                    "{x} | {y}".format(x=x, y=x + 3)
                    for x in range(0, abs(int(self.lastQDict["runTime"])), 3)
                ]
            )
            component = "Cs137"
            if "isBomb" in self.lastQDict:
                component = "Aerosol"
            dianaIn = os.path.join(prod_dir, "diana.in")
            with open(self.res.getBSnapInputFile(), "rt") as fh:
                dianaInTmpl = fh.read()
            with open(dianaIn, "wt") as fh:
                fh.write(
                    dianaInTmpl.format(
                        hours=hours,
                        component=component,
                        areaname=self.lastQDict["region"],
                    )
                )

            # plots
            proc = QProcess()
            proc.setWorkingDirectory(os.path.join(prod_dir))
            proc.setStandardOutputFile(
                os.path.join(self.lastOutputDir, "snap.log.out"), QIODevice.Append
            )
            proc.setStandardErrorFile(
                os.path.join(self.lastOutputDir, "snap.log.out"), QIODevice.Append
            )
            proc.start(
                "bdiana{}".format(diVersion), ["-i", dianaIn, "-s", "diana.setup"]
            )
            proc.waitForFinished(-1)
            with open(os.path.join(self.lastOutputDir, "snap.log.out"), "a") as lfh:
                lfh.write("plotting finished\n")

            sendPngsFromDir(
                "SNAP calculation: {}".format(self.lastTag),
                "Finished in {wdir}. See attached file(s).\n SourceTerm: \n{sourceTerm}".format(
                    wdir=self.lastOutputDir, sourceTerm=self.lastSourceTerm
                ),
                prod_dir,
            )

    def _defaultDomainCheck(self, lonf, latf):
        if (
            latf <= self.res.ecDefaultDomainStartY
            or latf >= (self.res.ecDefaultDomainStartY + self.res.ecDomainHeight)
            or lonf <= self.res.ecDefaultDomainStartX
            or lonf >= self.res.ecDefaultDomainStartX + self.res.ecDomainWidth
        ):
            self.write_log(
                "(lat,lon) = ({lat},{lon}) outside domain.\nTry global EC meteorology under advanced.".format(
                    lat=latf, lon=lonf
                )
            )
            return False
        return True

    def _meps25DomainCheck(self, lonf, latf):
        if latf <= 53.0 or latf >= 72.0 or lonf <= 0.0 or lonf >= 36.0:
            self.write_log(
                "(lat,lon) = ({lat},{lon}) outside domain for meps.\n".format(
                    lat=latf, lon=lonf
                )
            )
            return False
        return True

    def get_bomb_yield(self, yld):
        if yld == 1:
            lower = 500.0
            upper = 1500.0
            radius = 600.0
            activity = 2.0e19
        elif yld == 3:
            lower = 1400.0
            upper = 3100.0
            radius = 1000.0
            activity = 6.0e19
        elif yld == 10:
            lower = 2250.0
            upper = 4750.0
            radius = 1400.0
            activity = 2.0e20
        elif yld == 30:
            lower = 4100.0
            upper = 8400.0
            radius = 2300.0
            activity = 6.0e20
        elif yld == 100:
            lower = 5950.0
            upper = 12050.0
            radius = 3200.0
            activity = 2.0e21
        elif yld == 300:
            lower = 8000.0
            upper = 18500.0
            radius = 5800.0
            activity = 6.0e21
        elif yld == 1000:
            lower = 10000.0
            upper = 25000.0
            radius = 8500.0
            activity = 2.0e22
        elif yld == 3000:
            lower = 12000.0
            upper = 32000.0
            radius = 11100.0
            activity = 6.0e22
        else:
            lower = 0.0
            upper = 0.0
            radius = 0.0
            activity = 0
        return (lower, upper, radius, activity)

    def get_bomb_release(self, qDict):
        source_tmpl = """
MAX.PARTICLES.PER.RELEASE= 1000000

* PARTICLE CLASSES
COMPONENT= Aerosol_2.2mym
DRY.DEP.ON
WET.DEP.ON
RADIOACTIVE.DECAY.OFF
RADIUS.MICROMETER= 2.2
*DENSITY.G/CM3=2.95
GRAVITY.FIXED.CM/S= 0.2
FIELD.IDENTIFICATION=01
*
COMPONENT= Aerosol_4.4mym
DRY.DEP.ON
WET.DEP.ON
RADIOACTIVE.DECAY.OFF
RADIUS.MICROMETER= 4.4
GRAVITY.FIXED.CM/S= 0.7
FIELD.IDENTIFICATION=2
*
COMPONENT= Aerosol_8.6mym
DRY.DEP.ON
WET.DEP.ON
RADIOACTIVE.DECAY.OFF
RADIUS.MICROMETER= 8.6
GRAVITY.FIXED.CM/S= 2.5
FIELD.IDENTIFICATION=3
*
COMPONENT= Aerosol_14.6mym
DRY.DEP.ON
WET.DEP.ON
RADIOACTIVE.DECAY.OFF
RADIUS.MICROMETER= 14.6
GRAVITY.FIXED.CM/S= 6.9
FIELD.IDENTIFICATION=4
*
COMPONENT= Aerosol_22.8mym
DRY.DEP.ON
WET.DEP.ON
RADIOACTIVE.DECAY.OFF
RADIUS.MICROMETER= 22.8
GRAVITY.FIXED.CM/S= 15.9
FIELD.IDENTIFICATION=5
*
COMPONENT= Aerosol_36.1mym
DRY.DEP.ON
WET.DEP.ON
RADIOACTIVE.DECAY.OFF
RADIUS.MICROMETER= 36.1
GRAVITY.FIXED.CM/S= 35.6
FIELD.IDENTIFICATION=6
*
COMPONENT= Aerosol_56.5mym
DRY.DEP.ON
WET.DEP.ON
RADIOACTIVE.DECAY.OFF
RADIUS.MICROMETER= 56.5
GRAVITY.FIXED.CM/S= 71.2
FIELD.IDENTIFICATION=7
*
COMPONENT= Aerosol_92.3mym
DRY.DEP.ON
WET.DEP.ON
RADIOACTIVE.DECAY.OFF
RADIUS.MICROMETER= 92.3
GRAVITY.FIXED.CM/S= 137.0
FIELD.IDENTIFICATION=8
*
COMPONENT= Aerosol_173.2mym
DRY.DEP.ON
WET.DEP.ON
RADIOACTIVE.DECAY.OFF
RADIUS.MICROMETER=173.2
GRAVITY.FIXED.CM/S= 277.3
FIELD.IDENTIFICATION=9
*
COMPONENT= Aerosol_250.0mym
DRY.DEP.ON
WET.DEP.ON
RADIOACTIVE.DECAY.OFF
RADIUS.MICROMETER= 250.0
GRAVITY.FIXED.CM/S= 10000.0
FIELD.IDENTIFICATION=10

** Explosive yield = {yld}kt
TIME.RELEASE.PROFILE.BOMB
RELEASE.SECOND=    0
RELEASE.RADIUS.M={radius}
RELEASE.LOWER.M= {lowerHeight}
RELEASE.UPPER.M= {upperHeight}

"""
        errors = ""
        try:
            yld = int(qDict["yield"])
            (lowerHeight, upperHeight, radius, activity) = self.get_bomb_yield(yld)
            if activity == 0.0:
                raise Exception()
        except Exception:
            errors += "unknown yield\n"

        source_term = source_tmpl.format(
            radius=radius, lowerHeight=lowerHeight, upperHeight=upperHeight, yld=yld
        )

        release = []
        tags = (
            "2.2",
            "4.4",
            "8.6",
            "14.6",
            "22.8",
            "36.1",
            "56.5",
            "92.3",
            "173.2",
            "250.0",
        )
        for tag in tags:
            release.append(
                "RELEASE.BQ/STEP.COMP= {amount:.3E} 'Aerosol_{tag}mym'\n".format(
                    tag=tag, amount=activity / len(tags)
                )
            )

        source_term += "".join(release)

        return (source_term, errors)

    def get_isotope_release(self, qDict):
        errors = ""
        for tag in ("releaseTime", "radius", "lowerHeight", "upperHeight"):
            if not re.search("\d+", qDict[tag]):
                errors += "Cannot interprete {}: {}".format(tag, qDict[tag])

        source_tmpl = """
MAX.PARTICLES.PER.RELEASE= 2000
TIME.RELEASE.PROFILE.STEPS
RELEASE.HOUR= 0, {releaseTime}
RELEASE.RADIUS.M= {radius}, {radius}
RELEASE.LOWER.M= {lowerHeight}, {lowerHeight}
RELEASE.UPPER.M= {upperHeight}, {upperHeight}
"""
        source_term = source_tmpl.format(
            releaseTime=qDict["releaseTime"],
            radius=qDict["radius"],
            lowerHeight=qDict["lowerHeight"],
            upperHeight=qDict["upperHeight"],
        )

        isotopes = {"relI131": "I131", "relXE133": "Xe133", "relCS137": "Cs137"}
        for rel, iso in isotopes.items():
            emis = 0.0
            try:
                emis = float(qDict[rel])
            except Exception:
                pass
            if emis > 0.0:
                source_term += "RELEASE.BQ/SEC.COMP= {rel}, {rel}, '{iso}'\n".format(
                    rel=qDict[rel], iso=iso
                )

        # add Cs137, I131 and Xe133
        source_term += self.res.isotopes2snapinput([169, 158, 148])

        return (source_term, errors)

    def run_snap_query(self, qDict):
        # make sure all files are rw for everybody (for later deletion)
        os.umask(0)
        debug("run_snap_query")
        for key, value in qDict.items():
            print(str.format("{0} => {1}", key, value))
        errors = ""
        match = re.search(
            r"(\d{4})-(\d{2})-(\d{2})[\+\s]+(\d{1,2})", qDict["startTime"]
        )
        if match:
            startTime = "{0} {1} {2} {3}".format(*match.group(1, 2, 3, 4))
            startDT = datetime.datetime(*tuple(map(int, list(match.group(1, 2, 3, 4)))))
        else:
            errors += "Cannot interprete startTime: {0}\n".format(qDict["startTime"])

        if not re.search("-?\d+(.\d+)?", qDict["runTime"]):
            errors += "Cannot interprete runTime: {}\n".format(qDict["runTime"])

        lat = qDict["latitude"]
        lon = qDict["longitude"]
        tag = "latlon"
        nPPs = self.res.readNPPs()
        nPPs.update(self.res.readRadnett())
        if qDict["npp"] and nPPs[qDict["npp"]]:
            tag = qDict["npp"]
            npp = nPPs[qDict["npp"]]["site"]
            lat = nPPs[qDict["npp"]]["lat"]
            lon = nPPs[qDict["npp"]]["lon"]
            debug("NPP: {0} {1} {2}".format(npp, lat, lon))
        self.lastTag = "{0} {1}".format(tag, startTime)

        try:
            latf = Snappy.Utils.parseLat(lat)
            lonf = Snappy.Utils.parseLon(lon)
        except ValueError as ve:
            latf = 0.0
            lonf = 0.0
            errors += "Cannot interprete latitude/longitude: {lat}/{lon}: {ex}\n".format(
                lat=lat, lon=lon, ex=ve
            )

        if len(errors) > 0:
            debug('updateSnapLog("{0}");'.format(json.dumps("ERRORS:\n\n" + errors)))
            self.write_log("ERRORS:\n\n{0}".format(errors))
            return
        self.write_log(
            "working with lat/lon=({0}/{1}) starting at {2}".format(
                latf, lonf, startTime
            )
        )

        curtime = gmtime()
        self.lastOutputDir = os.path.join(
            self.res.getSnapOutputDir(),
            "{0}_{1}".format(tag, strftime("%Y-%m-%dT%H%M%S", curtime)),
        )
        self.lastQDict = qDict
        sourceTerm = """
TITLE={tag}
SIMULATION.START.DATE={simStart}
SET_RELEASE.POS= P=   {lat},   {lon}
TIME.START= {startTime}
TIME.RUN = {runTime}h
STEP.HOUR.OUTPUT.FIELDS= 3
"""
        self.lastSourceTerm = sourceTerm.format(
            tag=self.lastTag,
            simStart=strftime("%Y-%m-%d_%H:%M:%S", curtime),
            lat=latf,
            lon=lonf,
            startTime=startTime,
            runTime=qDict["runTime"],
        )

        if "isBomb" in qDict:
            (term, errors) = self.get_bomb_release(qDict)
        else:
            (term, errors) = self.get_isotope_release(qDict)
        if len(errors) > 0:
            debug('updateSnapLog("{0}");'.format(json.dumps("ERRORS:\n\n" + errors)))
            self.write_log("ERRORS:\n\n{0}".format(errors))
            return
        self.lastSourceTerm += term

        debug("output directory: {}".format(self.lastOutputDir))
        os.mkdir(self.lastOutputDir)

        with open(os.path.join(self.lastOutputDir, "snap.input"), "w") as fh:
            fh.write(self.lastSourceTerm)
        if qDict["metmodel"] == "nrpa_ec_0p1":
            files = self.res.getECMeteorologyFiles(
                startDT, int(qDict["runTime"]), qDict["ecmodelrun"]
            )
            if len(files) == 0:
                self.write_log(
                    "no EC met-files found for {}, runtime {}".format(
                        startDT, qDict["runTime"]
                    )
                )
                return
            if not self._defaultDomainCheck(lonf, latf):
                return
            with open(os.path.join(self.lastOutputDir, "snap.input"), "a") as fh:
                fh.write(self.res.getSnapInputMetDefinitions(qDict["metmodel"], files))
            self._snap_model_run()
        elif qDict["metmodel"] == MetModel.NrpaEC0p1Global:
            try:
                self.write_log("extracting meteorology from EC for domain")
                self.metcalc = EcMeteorologyCalculator(
                    EcMeteorologyCalculator.getGlobalMeteoResources(),
                    startDT,
                    lonf,
                    latf,
                )
                self._met_calculate_and_run()
            except MeteoDataNotAvailableException as e:
                self.write_log("problems creating EC-met: {}".format(e.args[0]))
        elif qDict["metmodel"] == MetModel.Icon0p25Global:
            try:
                self.write_log("extracting meteorology from ICON for domain")
                self.metcalc = ICONMeteorologyCalculator(
                    ICONMeteorologyCalculator.getGlobalMeteoResources(),
                    startDT,
                    lonf,
                    latf,
                )
                self._met_calculate_and_run()
            except MeteoDataNotAvailableException as e:
                self.write_log("problems creating ICON-met: {}".format(e.args[0]))
        elif qDict["metmodel"] == "meps_2_5km":
            files = self.res.getMEPS25MeteorologyFiles(
                startDT, int(qDict["runTime"]), "best"
            )
            if len(files) == 0:
                self.write_log(
                    "no MEPS2_5 met-files found for {}, runtime {}".format(
                        startDT, qDict["runTime"]
                    )
                )
                return
            if not self._meps25DomainCheck(lonf, latf):
                return
            with open(os.path.join(self.lastOutputDir, "snap.input"), "a") as fh:
                fh.write(self.res.getSnapInputMetDefinitions(qDict["metmodel"], files))
            self._snap_model_run()
        else:
            # hirlam
            if not self._defaultDomainCheck(lonf, latf):
                return
            with open(os.path.join(self.lastOutputDir, "snap.input"), "a") as fh:
                fh.write(self.res.getSnapInputMetDefinitions(qDict["metmodel"], []))
            self._snap_model_run()

    def _snap_model_run(self):
        self.snap_run = SnapRun(self)
        self.snap_run.proc.finished.connect(self._snap_finished)
        self.snap_run.start()

        self.snap_update = SnapUpdateThread(self)
        self.snap_update.update_log_signal.connect(self.update_log)
        self.snap_update.start(QThread.LowPriority)

    def update_log_query(self, qDict):
        # MainBrowserWindow._default_form_handler(qDict)
        self.write_log("updating...")
        if os.path.isfile(os.path.join(self.lastOutputDir, "snap.log.out")):
            lfh = open(os.path.join(self.lastOutputDir, "snap.log.out"))
            debug(tail(os.path.join(self.lastOutputDir, "snap.log.out"), 30))
            self.write_log(tail(os.path.join(self.lastOutputDir, "snap.log.out"), 30))
            lfh.close()

    def update_log(self):
        self.update_log_query({})

    def _create_snap_form_handler(self):
        def handler(queryDict):
            """a form-handler with closure for self"""
            options = {"Run": self.run_snap_query, "Update": self.update_log_query}
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
            except Exception:
                self.write_log(
                    "Unexpected error on {0}: {1}".format(
                        qDict["action"], sys.exc_info()[0]
                    )
                )
                raise

        return handler


def tail(f, n):
    fh = open(f)
    lines = []
    while 1:
        line = fh.readline()
        if line:
            lines.append(line)
            if len(lines) > n:
                lines = lines[len(lines) - n :]
        else:
            break
    return "".join(lines)


if __name__ == "__main__":
    debug("threads: {}".format(QThreadPool.globalInstance().maxThreadCount()))
    app = QtWidgets.QApplication(sys.argv)
    ctr = SnapController()
    sys.exit(app.exec_())
