import datetime
import json
import os
import re
import sys
from time import gmtime, strftime
import traceback

from PyQt5 import QtWidgets, QtGui
from PyQt5.QtCore import QProcess, QProcessEnvironment, QThread, QIODevice, QThreadPool, pyqtSignal, pyqtSlot
from Snappy.BrowserWidget import BrowserWidget
from Snappy.EcMeteorologyCalculator import EcMeteorologyCalculator, ECDataNotAvailableException
from Snappy.MailImages import sendPngsFromDir
from Snappy.Resources import Resources


def debug(*objs):
    print("DEBUG: ", *objs, file=sys.stderr)

class _SnapUpdateThread(QThread):
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
                debug(tail(os.path.join(self.snap_controller.lastOutputDir,"snap.log.stdout"),10))
                self.update_log_signal.emit()
                self.sleep(3)
        except:
            traceback.print_exc()


class _SnapRun():

    @staticmethod
    def getQProcess(snap_controller):
        '''initialize a QProcess with the correct environment and output-files'''
        proc = QProcess()
        proc.setWorkingDirectory(snap_controller.lastOutputDir)
        proc.setStandardOutputFile(os.path.join(snap_controller.lastOutputDir,"snap.log.stdout"), QIODevice.Append)
        proc.setStandardErrorFile(os.path.join(snap_controller.lastOutputDir,"snap.log.stderr"), QIODevice.Append)
        env = QProcessEnvironment.systemEnvironment()
        env.insert("OMP_NUM_THREADS", "1")
        proc.setProcessEnvironment(env)
        return proc

    def __init__(self, snap_controller):
        self.snap_controller = snap_controller
        self.proc = _SnapRun.getQProcess(snap_controller)


    def start(self):
        debug("outputdir: "+self.snap_controller.lastOutputDir)
#         self.proc.start('/home/heikok/sleepLong.sh', ['snap.input'])
        self.proc.start('/usr/bin/bsnap_naccident', ['snap.input'])
        if (self.proc.waitForStarted(3000)) :
            self.snap_controller.snapRunning = "running"
            debug("started: "+ self.snap_controller.snapRunning)
        else:
            self.snap_controller.write_log("starting /usr/bin/bsnap_naccident snap.input failed")

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

    def write_log(self, txt:str):
        debug(txt)
        self.main.evaluate_javaScript('updateSnapLog({0});'.format(json.dumps(txt)))

    @pyqtSlot()
    def _snap_finished(self):
        debug("finished")
        self.snapRunning = "finished"
        self.results_add_toa()
        self.plot_results()
        with open(os.path.join(self.lastOutputDir,"snap.log.stdout"), "a") as logFile:
            logFile.write("All work finished. Please open diana to see results.\nResults in {}\n".format(self.lastOutputDir))
        self.update_log()

    @pyqtSlot()
    def _ec_finished_run_snap(self):
        debug("ec_finished")
        self.snapRunning = "finished" # quit update-thread
        if (self.ecmet.must_calc()):
            with open(os.path.join(self.lastOutputDir,"snap.log.stdout"), "a") as logFile:
                logFile.write("Meteorology not generated.\nErrors in {}\n".format(self.lastOutputDir))
            self.update_log()
            return

        metdefs = self.res.getDefaultMetDefinitions(self.lastQDict['metmodel'])
        (metdefs["startX"], metdefs["startY"]) = self.ecmet.get_grid_startX_Y()
        with open(os.path.join(self.lastOutputDir, "snap.input"),'a') as fh:
            fh.write(self.res.getSnapInputMetDefinitions(self.lastQDict['metmodel'],
                                self.ecmet.get_meteorology_files(), **metdefs))
        self._snap_model_run()

    def results_add_toa(self):
        proc = _SnapRun.getQProcess(self)
        proc.start("snapAddToa", ['snap.nc'])
        proc.waitForFinished(-1)


    def plot_results(self):
        match = re.search(r'(\d*\.\d+)', self.lastQDict['dianaversion'])
        if match:
            diVersion = "-{}".format(match.group(1))
        else :
            diVersion = ""

        if 'region' in self.lastQDict:
            prod_dir = os.path.join(self.lastOutputDir, "prod")
            os.mkdir(prod_dir)
            fh = open(os.path.join(prod_dir, "diana.setup"),'w')
            di_setup = """
%include /etc/diana/setup/diana.setup-COMMON
<FIELD_FILES>
filegroup=SNAP
m=SNAP.current t=fimex format=netcdf f={}
</FIELD_FILES>
"""
            fh.write(di_setup.format(os.path.join(self.lastOutputDir, "snap.nc")))
            fh.close()


            # plots
            proc = QProcess()
            proc.setWorkingDirectory(os.path.join(prod_dir))
            proc.setStandardOutputFile(os.path.join(self.lastOutputDir,"snap.log.stdout"), QIODevice.Append)
            proc.setStandardErrorFile(os.path.join(self.lastOutputDir,"snap.log.stderr"), QIODevice.Append)
            proc.start("bdiana{}".format(diVersion), ['-i', self.res.getBSnapInputFile(), '-s', 'diana.setup', 'p={}'.format(self.lastQDict['region'])])
            proc.waitForFinished(-1)
            with open(os.path.join(self.lastOutputDir,"snap.log.stdout"), 'a') as lfh:
                lfh.write("plotting finished\n")

            sendPngsFromDir("SNAP calculation: {}".format(self.lastTag),
                            "Finished in {wdir}. See attached file(s).\n SourceTerm: \n{sourceTerm}".format(wdir=self.lastOutputDir, sourceTerm=self.lastSourceTerm),
                            prod_dir)


    def _defaultDomainCheck(self, lonf, latf):
        if (latf <= self.res.ecDefaultDomainStartY or
            latf >= (self.res.ecDefaultDomainStartY + self.res.ecDomainHeight) or
            lonf <= self.res.ecDefaultDomainStartX or
            lonf >= self.res.ecDefaultDomainStartX + self.res.ecDomainWidth):
            self.write_log("(lat,lon) = ({lat},{lon}) outside domain.\nTry global EC meteorology under advanced.".format(lat=latf, lon=lonf))
            return False
        return True

    def _meps25DomainCheck(self, lonf, latf):
        if (latf <= 53. or
            latf >= 72. or
            lonf <= 0. or
            lonf >= 36.):
            self.write_log("(lat,lon) = ({lat},{lon}) outside domain for meps.\n".format(lat=latf, lon=lonf))
            return False
        return True



    def run_snap_query(self, qDict):
        # make sure all files are rw for everybody (for later deletion)
        os.umask(0)
        debug("run_snap_query")
        for key, value in qDict.items():
            print(str.format("{0} => {1}", key, value))
        errors = ""
        match = re.search(r'(\d{4})-(\d{2})-(\d{2})[\+\s]+(\d{1,2})', qDict['startTime'])
        if match:
            startTime = "{0} {1} {2} {3}".format(*match.group(1,2,3,4))
            startDT = datetime.datetime(*tuple(map(int, list(match.group(1,2,3,4)))))
        else:
            errors += "Cannot interprete startTime: {0}\n".format(qDict['startTime'])

        if not re.search('-?\d+(.\d+)?', qDict['runTime']):
            errors += "Cannot interprete runTime: {}\n".format(qDict['runTime'])

        if not re.search('\d+', qDict['releaseTime']):
            errors += "Cannot interprete releaseTime: {}".format(qDict['releaseTime'])

        lat = qDict['latitude']
        lon = qDict['longitude']
        tag = "latlon"
        nPPs = self.res.readNPPs()
        if (qDict['npp'] and nPPs[qDict['npp']]):
            tag = qDict['npp']
            npp = nPPs[qDict['npp']]['site']
            lat = nPPs[qDict['npp']]['lat']
            lon = nPPs[qDict['npp']]['lon']
            debug("NPP: {0} {1} {2}".format(npp, lat, lon))
        self.lastTag = "{0} {1}".format(tag, startTime)

        try:
            latf = float(lat)
            lonf = float(lon)
        except:
            latf = 0.
            lonf = 0.
            errors += "Cannot interprete latitude/longitude: {0}/{1}\n".format(lat,lon);

        if (abs(latf) > 90):
            errors += "latitude {0} outside bounds\n".format(latf)
        if (abs(lonf) > 180):
            errors += "longitude {0} outside bounds\n".format(lonf)

        if (len(errors) > 0):
            debug('updateSnapLog("{0}");'.format(json.dumps("ERRORS:\n\n"+errors)))
            self.write_log("ERRORS:\n\n{0}".format(errors))
            return
        self.write_log("working with lat/lon=({0}/{1}) starting at {2}".format(latf, lonf, startTime))

        curtime = gmtime()
        self.lastOutputDir = os.path.join(self.res.getSnapOutputDir(), "{0}_{1}".format(tag, strftime("%Y-%m-%dT%H%M%S", curtime)))
        self.lastQDict = qDict
        sourceTerm = """
SIMULATION.START.DATE={simStart}
SET_RELEASE.POS= P=   {lat},   {lon}
TIME.START= {startTime}
TIME.RUN = {runTime}h
TIME.RELEASE.PROFILE.STEPS
STEP.HOUR.OUTPUT.FIELDS= 3
RELEASE.HOUR= 0, {releaseTime}
RELEASE.RADIUS.M= {radius}, {radius}
RELEASE.LOWER.M= {lowerHeight}, {lowerHeight}
RELEASE.UPPER.M= {upperHeight}, {upperHeight}
"""
        self.lastSourceTerm = sourceTerm.format(simStart=strftime("%Y-%m-%d_%H:%M:%S",curtime),
                                                lat=latf, lon=lonf, startTime=startTime,
                                                runTime=qDict['runTime'],
                                                releaseTime=qDict['releaseTime'],
                                                radius=qDict['radius'],
                                                lowerHeight=qDict['lowerHeight'], upperHeight=qDict['upperHeight'])
        isotopes = {'relI131': 'I131',
                    'relXE133': 'Xe133',
                    'relCS137': 'Cs137'}
        for rel, iso in isotopes.items():
            emis = 0.
            try:
                emis = float(qDict[rel])
            except:
                pass
            if (emis > 0.):
                self.lastSourceTerm += "RELEASE.BQ/SEC.COMP= {rel}, {rel}, '{iso}'\n".format(rel=qDict[rel], iso=iso)

        debug("output directory: {}".format(self.lastOutputDir))
        os.mkdir(self.lastOutputDir)

        with open(os.path.join(self.lastOutputDir, "snap.input"),'w') as fh:
            fh.write(self.lastSourceTerm)
            # add Cs137, I131 and Xe133
            fh.write(self.res.isotopes2snapinput([169, 158, 148]))
        if (qDict['metmodel'] == 'nrpa_ec_0p1'):
            files = self.res.getECMeteorologyFiles(startDT, int(qDict['runTime']), qDict['ecmodelrun'])
            if (len(files) == 0):
                self.write_log("no EC met-files found for {}, runtime {}".format(startDT, qDict['runTime']))
                return
            if (not self._defaultDomainCheck(lonf,latf)):
                return
            with open(os.path.join(self.lastOutputDir, "snap.input"),'a') as fh:
                fh.write(self.res.getSnapInputMetDefinitions(qDict['metmodel'], files))
            self._snap_model_run()
        elif qDict['metmodel'] == 'nrpa_ec_0p1_global':
            try:
                self.ecmet = EcMeteorologyCalculator(self.res, startDT, lonf, latf)
                if self.ecmet.must_calc():
                    proc = _SnapRun.getQProcess(self)
                    proc.finished.connect(self._ec_finished_run_snap)
                    self.ecmet.calc(proc)
                    if (proc.waitForStarted(3000)) :
                        self.snapRunning = "running"
                    # start background logging thread
                    self.snap_update = _SnapUpdateThread(self)
                    self.snap_update.update_log_signal.connect(self.update_log)
                    self.snap_update.start(QThread.LowPriority)
                else:
                    self._ec_finished_run_snap()
            except ECDataNotAvailableException as e:
                self.write_log("problems creating EC-met: {}".format(e.args[0]))
        elif qDict['metmodel'] == 'meps_2_5km':
            files = self.res.getMEPS25MeteorologyFiles(startDT, int(qDict['runTime']), "best")
            if (len(files) == 0):
                self.write_log("no MEPS2_5 met-files found for {}, runtime {}".format(startDT, qDict['runTime']))
                return
            if (not self._meps25DomainCheck(lonf,latf)):
                return
            with open(os.path.join(self.lastOutputDir, "snap.input"),'a') as fh:
                fh.write(self.res.getSnapInputMetDefinitions(qDict['metmodel'], files))
            self._snap_model_run()
        else:
            # hirlam
            if (not self._defaultDomainCheck(lonf,latf)):
                return
            with open(os.path.join(self.lastOutputDir, "snap.input"),'a') as fh:
                fh.write(self.res.getSnapInputMetDefinitions(qDict['metmodel'], []))
            self._snap_model_run()

    def _snap_model_run(self):
        self.snap_run = _SnapRun(self)
        self.snap_run.proc.finished.connect(self._snap_finished)
        self.snap_run.start()

        self.snap_update = _SnapUpdateThread(self)
        self.snap_update.update_log_signal.connect(self.update_log)
        self.snap_update.start(QThread.LowPriority)


    def update_log_query(self, qDict):
        #MainBrowserWindow._default_form_handler(qDict)
        self.write_log("updating...")
        if os.path.isfile(os.path.join(self.lastOutputDir,"snap.log.stdout")) :
            lfh = open(os.path.join(self.lastOutputDir,"snap.log.stdout"))
            debug(tail(os.path.join(self.lastOutputDir,"snap.log.stdout"),30))
            self.write_log(tail(os.path.join(self.lastOutputDir,"snap.log.stdout"), 30))
            lfh.close()

    @pyqtSlot()
    def update_log(self):
        self.update_log_query({})

    def _create_snap_form_handler(self):
        def handler(queryDict):
            """a form-handler with closure for self"""
            options = { 'Run' : self.run_snap_query,
                        'Update' : self.update_log_query
            }
            # mapping from QList<QPair> to simple dictionary
            qDict = dict()
            for key, value in queryDict:
                qDict[key] = value
            # calling the correct handler depending on the module
            try:
                options[qDict['action']](qDict)
            except TypeError as ex:
                self.write_log("type-error: {}".format(ex))
            except ValueError as ex:
                self.write_log("value-error: {}".format(ex))
            except:
                self.write_log("Unexpected error on {0}: {1}".format(qDict['action'],sys.exc_info()[0]))
                raise
        return handler



def tail(f, n):
    fh = open(f)
    lines = []
    while 1:
        line = fh.readline()
        if (line):
            lines.append(line)
            if len(lines) > n:
                lines = lines[len(lines)-n:]
        else:
            break
    return "".join(lines)

if __name__ == "__main__":
    debug("threads: {}".format(QThreadPool.globalInstance().maxThreadCount()))
    app = QtWidgets.QApplication(sys.argv)
    ctr = SnapController()
    sys.exit(app.exec_())

