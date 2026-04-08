import datetime
import json
import logging
import math
import os
import re
import sys
from time import gmtime, strftime

from PyQt5 import QtWidgets
from PyQt5.QtCore import (
    QThread,
    QThreadPool,
)

from Snappy.BrowserWidget import BrowserWidget
from Snappy.Resources import Resources
from Snappy.SnapController import SnapRun, SnapUpdateThread
from Snappy.Utils import parseLat, parseLon

logger = logging.getLogger(__name__)


class SnapRunInverse(SnapRun):
    def start(self, snapscript):
        logger.info("outputdir: " + self.snap_controller.lastOutputDir)
        #         self.proc.start('/home/heikok/sleepLong.sh', ['snap.input'])
        self.proc.start(snapscript)
        if self.proc.waitForStarted(3000):
            self.snap_controller.snapRunning = "running"
            logger.info(
                "started  " + snapscript + " " + self.snap_controller.snapRunning
            )
        else:
            self.snap_controller.write_log(f"starting {snapscript} failed")


class Measurement:
    def __init__(self, id, name, lon, lat, start, end):
        self.id = id
        self.name = name
        self.lon = lon
        self.lat = lat
        self.start = start
        self.end = end


class SnapControllerInverse:
    def __init__(self):
        self.res = Resources()
        self.lastOutputDir = os.path.join(
            self.res.getSnapOutputDir(),
            f"backtrack_{strftime('%Y-%m-%dT%H%M%S', gmtime())}",
        )
        self.main = BrowserWidget()
        self.main.set_html(
            self.res.getStartScreenInverse().replace("OUTPUTDIR", self.lastOutputDir)
        )
        self.main.set_form_handler(self._create_snap_form_handler())
        self.main.show()
        self.snapRunning = "inactive"
        self.lastQDict = {}

    def write_log(self, txt: str):
        logger.info(txt)
        self.main.evaluate_javaScript(f"updateSnapLog({json.dumps(txt)});")

    def _snap_finished(self):
        logger.info("finished")
        self.snapRunning = "finished"
        # self.plot_results()
        with open(os.path.join(self.lastOutputDir, "snap.log.out"), "a") as logFile:
            logFile.write(
                f"All work finished. Please open 'vgl-launch diana -s {self.lastOutputDir}/diana.setup' to see results.\n"
            )
        self.update_log()

    def _defaultDomainCheck(self, lonf, latf):
        if (
            latf <= self.res.ecDefaultDomainStartY
            or latf >= (self.res.ecDefaultDomainStartY + self.res.ecDomainHeight)
            or lonf <= self.res.ecDefaultDomainStartX
            or lonf >= self.res.ecDefaultDomainStartX + self.res.ecDomainWidth
        ):
            self.write_log(
                f"(lat,lon) = ({latf},{lonf}) outside domain.\nTry global EC meteorology under advanced."
            )
            return False
        return True

    def run_snap_query(self, qDict):
        # make sure all files are rw for everybody (for later deletion)
        os.umask(0)
        logger.info("run_snap_query inverse")
        for key, value in qDict.items():
            print(f"{key} => {value}")
        runTime = -96
        if "runTime" in qDict:
            runTime = int(qDict["runTime"])
        if "outputDir" in qDict:
            self.lastOutputDir = qDict["outputDir"]

        errors = ""
        self.measurements = []
        # measurementX, latX, lonX, startX, encX
        for i in range(1, 100):
            if f"measurement{i}" not in qDict:
                continue
            name = qDict[f"measurement{i}"]
            for tag in (f"lat{i}", f"lon{i}", f"start{i}", f"end{i}"):
                if tag not in qDict:
                    errors += f"No tag {tag} for {name}\n"
            if len(errors) > 0:
                continue
            match = re.search(
                r"(\d{4})-(\d{2})-(\d{2})[\+\s]+(\d{1,2})", qDict[f"start{i}"]
            )
            if match:
                startDT = datetime.datetime(
                    *tuple(map(int, list(match.group(1, 2, 3, 4))))
                )
            else:
                errors += f"Cannot interprete startTime: {qDict[f'start{i}']}\n"
            match = re.search(
                r"(\d{4})-(\d{2})-(\d{2})[\+\s]+(\d{1,2}):(\d{1,2})",
                qDict[f"end{i}"],
            )
            if match:
                endDT = datetime.datetime(
                    *tuple(map(int, list(match.group(1, 2, 3, 4))))
                )
                if int(match.group(5)) > 0:
                    endDT = endDT + datetime.timedelta(hours=1)
            else:
                errors += f"Cannot interprete endTime: {qDict[f'end{i}']}\n"
            if startDT >= endDT:
                errors += f"Start must be before end for {name}\n"
            lat = qDict[f"lat{i}"]
            lon = qDict[f"lon{i}"]
            try:
                latf = parseLat(lat)
                lonf = parseLon(lon)
            except ValueError as ve:
                latf = 0.0
                lonf = 0.0
                errors += f"Cannot interprete latitude/longitude: {lat}/{lon}: {ve}\n"
            if len(errors) == 0:
                self.measurements.append(
                    Measurement(i, name, lonf, latf, startDT, endDT)
                )

        logger.info(f"output directory: {self.lastOutputDir}")
        if not os.path.isdir(self.lastOutputDir):
            try:
                os.mkdir(self.lastOutputDir)
            except Exception:
                errors += f"cannot create directory: {self.lastOutputDir}"
        else:
            errors += f"cowardly refusing to write into existing directory: {self.lastOutputDir}"

        if len(errors) > 0:
            logger.info(f'updateSnapLog("{json.dumps("ERRORS:\n\n" + errors)}");')
            self.write_log(f"ERRORS:\n\n{errors}")
            return

        curtime = gmtime()
        self.lastQDict = qDict
        self.write_log(
            f"working with {len(self.measurements)} measurements in {self.lastOutputDir}"
        )

        # write snap.input files
        for mes in self.measurements:
            print(f"{mes.id} {mes.name}")
            releaseDT = mes.end - mes.start
            releaseH = releaseDT.days * 24 + math.ceil(releaseDT.seconds / 3600)
            self.lastSourceTerm = f"""
SIMULATION.START.DATE={strftime("%Y-%m-%d_%H:%M:%S", curtime)}
SET_RELEASE.POS= P=   {mes.lat},   {mes.lon}
TIME.START= {mes.end:%Y %m %d %H}
TIME.RUN = {runTime}h
MAX.PARTICLES.PER.RELEASE= 4000
TIME.RELEASE.PROFILE.STEPS
STEP.HOUR.OUTPUT.FIELDS= 1
RELEASE.HOUR= 0, {releaseH}
RELEASE.RADIUS.M= 500, 500
RELEASE.LOWER.M= 0, 0
RELEASE.UPPER.M= 250, 250
RELEASE.BQ/SEC.COMP= 1e12, 1e12, 'Cs137'
"""

            with open(
                os.path.join(self.lastOutputDir, f"snap.input{mes.id}"), "w"
            ) as fh:
                fh.write(self.lastSourceTerm)
                # add Cs137 definition
                fh.write(self.res.isotopes2snapinput([169]))

            metmodel = "nrpa_ec_0p1"
            if metmodel == "nrpa_ec_0p1":
                if "metpattern" in qDict:
                    files = self.res.getECMeteorologyFiles(
                        mes.end,
                        runTime,
                        pattern=qDict["metpattern"],
                    )
                    if len(files) == 0:
                        self.write_log(
                            f"no EC met-files found for {mes.end}, runtime {runTime} with pattern {qDict['metpattern']}"
                        )
                        return
                else:
                    files = self.res.getECMeteorologyFiles(mes.end, runTime)
                    if len(files) == 0:
                        self.write_log(
                            f"no EC met-files found for {mes.end}, runtime {runTime}"
                        )
                        return
                if not self._defaultDomainCheck(lonf, latf):
                    return
                snapIn = self.res.getSnapInputMetDefinitions(metmodel, files)
                snapIn = snapIn.replace(
                    "snap.", f"snap{mes.id}."
                )  # replace snap.nc and snap.log to snap1.nc snap1.log
                with open(
                    os.path.join(self.lastOutputDir, f"snap.input{mes.id}"), "a"
                ) as fh:
                    fh.write(snapIn)

        snapscript = os.path.join(self.lastOutputDir, "snap.sh")
        with open(snapscript, "a") as fh:
            fh.write("#! /bin/bash\n")
            fh.write("export OMP_NUM_THREADS=2\n")
            fh.write("export OMP_PLACES=cores\n")
            fh.write(f"cd {self.lastOutputDir}\n")
            ids = " ".join([str(x.id) for x in self.measurements])
            fh.write(r"parallel -i -j 4 bsnap_naccident snap.input{} -- " + ids + "\n")
            joinIds = " ".join([f"-i snap{x.id}.nc" for x in self.measurements])
            fh.write(f"snapCombineInverse -I Cs137 -o snapCombined.nc {joinIds}\n")

        # create diana.setup
        with open(os.path.join(self.lastOutputDir, "diana.setup"), "w") as fh:
            fh.write(
                f"""
%include /etc/diana/setup/diana.setup-COMMON
<FIELD_PLOT>
field=source_probability
  colour=off minvalue=0.1 line.interval=1. palettecolours=gray_rev
  plot=FILL_CELL(Cs137_probability)
end.field

field=acc_source_probability
  colour=off minvalue=0.1 line.interval=3. palettecolours=who_uvi
  plot=FILL_CELL(Cs137_acc_probability)
end.field

field=Cs137_avg_in_boundary_layer
  colour=off minvalue=0.1 line.values=0,1 palettecolours=gray_rev
  plot=FILL_CELL(Cs137_concentration_bl)
end.field

</FIELD_PLOT>

<FIELD_FILES>
filegroup=snapBacktrack
m=combined t=fimex format=netcdf f={self.lastOutputDir}/snapCombined.nc
            """
            )
            for m in self.measurements:
                fh.write(
                    f"m={m.name} t=fimex format=netcdf f={self.lastOutputDir}/snap{m.id}.nc\n"
                )
            fh.write("</FIELD_FILES>\n")
        os.chmod(snapscript, 0o755)
        self._snap_model_run(snapscript)

    def _snap_model_run(self, snapscript):
        self.snap_run = SnapRunInverse(self)
        self.snap_run.proc.finished.connect(self._snap_finished)
        self.snap_run.start(snapscript)

        self.snap_update = SnapUpdateThread(self)
        self.snap_update.update_log_signal.connect(self.update_log)
        self.snap_update.start(QThread.LowPriority)

    def update_log_query(self, qDict):
        # MainBrowserWindow._default_form_handler(qDict)
        self.write_log("updating...")
        if os.path.isfile(os.path.join(self.lastOutputDir, "snap.log.out")):
            lfh = open(os.path.join(self.lastOutputDir, "snap.log.out"))
            logger.info(tail(os.path.join(self.lastOutputDir, "snap.log.out"), 30))
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
                self.write_log(f"type-error: {ex}")
            except ValueError as ex:
                self.write_log(f"value-error: {ex}")
            except:
                self.write_log(
                    f"Unexpected error on {qDict['action']}: {sys.exc_info()[0]}"
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
    logger.info(f"threads: {QThreadPool.globalInstance().maxThreadCount()}")
    app = QtWidgets.QApplication(sys.argv)
    ctr = SnapControllerInverse()
    sys.exit(app.exec_())
