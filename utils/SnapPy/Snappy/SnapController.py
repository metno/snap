import sys
import re
import json
import os
import traceback

from time import gmtime, strftime
import datetime
from Snappy.MainBrowserWindow import MainBrowserWindow
from Snappy.MailImages import sendPngsFromDir
from Snappy.Resources import Resources
from PyQt5 import QtWidgets, QtGui
from PyQt5.QtCore import QProcess, QProcessEnvironment, QThread, QIODevice, QThreadPool, pyqtSignal, pyqtSlot

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


    def __init__(self, snapController):
        self.snap_controller = snapController
        self.proc = QProcess()


    def start(self):
        debug("outputdir: "+self.snap_controller.lastOutputDir)
        self.proc.setWorkingDirectory(self.snap_controller.lastOutputDir)
        self.proc.setStandardOutputFile(os.path.join(self.snap_controller.lastOutputDir,"snap.log.stdout"))
        self.proc.setStandardErrorFile(os.path.join(self.snap_controller.lastOutputDir,"snap.log.stderr"))
        env = QProcessEnvironment.systemEnvironment()
        env.insert("OMP_NUM_THREADS", "1")
        self.proc.setProcessEnvironment(env)
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
        self.main = MainBrowserWindow()
        self.main.setWindowTitle('SNAPpy')
        self.main.setWindowIcon(QtGui.QIcon(self.res.getIconPath()))
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
        self.plot_results()
        with open(os.path.join(self.lastOutputDir,"snap.log.stdout"), "a") as logFile:
            logFile.write("All work finished. Please open diana to see results.\nResults in {}\n".format(self.lastOutputDir))
        self.update_log()

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

            proc = QProcess()
            proc.setWorkingDirectory(os.path.join(prod_dir))
            proc.setStandardOutputFile(os.path.join(self.lastOutputDir,"snap.log.stdout"), QIODevice.Append)
            proc.setStandardErrorFile(os.path.join(self.lastOutputDir,"snap.log.stderr"), QIODevice.Append)
            proc.start("bdiana{}".format(diVersion), ['-i', self.res.getBSnapInputFile(), '-s', 'diana.setup', 'p={}'.format(self.lastQDict['region'])])
            proc.waitForFinished(-1)
            with open(os.path.join(self.lastOutputDir,"snap.log.stdout"), 'a') as lfh:
                lfh.write("plotting finished\n")

            sendPngsFromDir("SNAP calculation: {}".format(self.lastTag),
                            "Finished in {dir}. See attached file(s).\n SourceTerm: \n{sourceTerm}".format(dir=self.lastOutputDir, sourceTerm=self.lastSourceTerm),
                            prod_dir)



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

        self.lastOutputDir = os.path.join(self.res.getSnapOutputDir(), "{0}_{1}".format(tag, strftime("%Y-%m-%dT%H%M%S", gmtime())))
        self.lastQDict = qDict
        sourceTerm = """
SET_RELEASE.POS= P=   {lat},   {lon}
TIME.START= {startTime}
TIME.RUN = {runTime}h
RELEASE.HOUR= 0, {releaseTime}
RELEASE.RADIUS.M= {radius}, {radius}
RELEASE.LOWER.M= {lowerHeight}, {lowerHeight}
RELEASE.UPPER.M= {upperHeight}, {upperHeight}
RELEASE.BQ/SEC.COMP= {relI131}, {relI131}, 'I131'
RELEASE.BQ/SEC.COMP= {relXE133}, {relXE133}, 'Xe133'
RELEASE.BQ/SEC.COMP= {relCS137}, {relCS137}, 'Cs137'
"""
        self.lastSourceTerm = sourceTerm.format(lat=latf, lon=lonf, startTime=startTime,
                                                runTime=qDict['runTime'],
                                                releaseTime=qDict['releaseTime'],
                                                radius=qDict['radius'],
                                                lowerHeight=qDict['lowerHeight'], upperHeight=qDict['upperHeight'],
                                                relI131=qDict['relI131'],
                                                relXE133=qDict['relXE133'],
                                                relCS137=qDict['relCS137'])
        debug("output directory: {}".format(self.lastOutputDir))
        os.mkdir(self.lastOutputDir)

        fh = open(os.path.join(self.lastOutputDir, "snap.input"),'w')
        fh.write(self.lastSourceTerm)
        if (qDict['metmodel'] == 'nrpa_ec_0p1'):
            files = self.res.getECMeteorologyFiles(startDT, int(qDict['runTime']), qDict['ecmodelrun'])
            if (len(files) == 0):
                self.write_log("no EC met-files found for {}, runtime {}".format(startDT, qDict['runTime']))
                return
            for f in files:
                fh.write("FIELD.INPUT={}\n".format(f))
            fh.write(self.res.getSnapInputTemplate('nrpa_ec_0p1'))
        else:
            fh.write(self.res.getSnapInputTemplate())
        fh.close()

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

