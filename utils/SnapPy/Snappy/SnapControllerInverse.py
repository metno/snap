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
import math
import os
import re
import sys
from time import gmtime, strftime

from PyQt5 import QtWidgets, QtGui
from PyQt5.QtCore import QProcess, QProcessEnvironment, QThread, QIODevice, QThreadPool, pyqtSignal
from Snappy.BrowserWidget import BrowserWidget
from Snappy.Resources import Resources
from Snappy.SnapController import SnapRun, SnapUpdateThread
import Snappy.Utils

def debug(*objs):
    print("DEBUG: ", *objs, file=sys.stderr)

class SnapRunInverse(SnapRun):

    def start(self, snapscript):
        debug("outputdir: "+self.snap_controller.lastOutputDir)
#         self.proc.start('/home/heikok/sleepLong.sh', ['snap.input'])
        self.proc.start(snapscript)
        if (self.proc.waitForStarted(3000)) :
            self.snap_controller.snapRunning = "running"
            debug("started  " + snapscript + " "+ self.snap_controller.snapRunning)
        else:
            self.snap_controller.write_log("starting {} failed".format(snapscript))

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
        self.lastOutputDir = os.path.join(self.res.getSnapOutputDir(), "{0}_{1}".format("backtrack", strftime("%Y-%m-%dT%H%M%S", gmtime())))
        self.main = BrowserWidget()
        self.main.set_html(self.res.getStartScreenInverse().replace("OUTPUTDIR", self.lastOutputDir))
        self.main.set_form_handler(self._create_snap_form_handler())
        self.main.show()
        self.snapRunning = "inactive"
        self.lastQDict = {}

    def write_log(self, txt:str):
        debug(txt)
        self.main.evaluate_javaScript('updateSnapLog({0});'.format(json.dumps(txt)))

    def _snap_finished(self):
        debug("finished")
        self.snapRunning = "finished"
        #self.plot_results()
        with open(os.path.join(self.lastOutputDir,"snap.log.out"), "a") as logFile:
            logFile.write("All work finished. Please open 'vgl-launch diana -s {dir}/diana.setup' to see results.\n".format(dir=self.lastOutputDir))
        self.update_log()

    def _defaultDomainCheck(self, lonf, latf):
        if (latf <= self.res.ecDefaultDomainStartY or
            latf >= (self.res.ecDefaultDomainStartY + self.res.ecDomainHeight) or
            lonf <= self.res.ecDefaultDomainStartX or
            lonf >= self.res.ecDefaultDomainStartX + self.res.ecDomainWidth):
            self.write_log("(lat,lon) = ({lat},{lon}) outside domain.\nTry global EC meteorology under advanced.".format(lat=latf, lon=lonf))
            return False
        return True

    def run_snap_query(self, qDict):
        # make sure all files are rw for everybody (for later deletion)
        os.umask(0)
        debug("run_snap_query inverse")
        for key, value in qDict.items():
            print(str.format("{0} => {1}", key, value))
        runTime = -96
        if "runTime" in qDict:
            runTime = int(qDict["runTime"])
        if "outputDir" in qDict:
            self.lastOutputDir = qDict["outputDir"]

        errors = ""
        self.measurements = []
        # measurementX, latX, lonX, startX, encX
        for i in range(1,100):
            if 'measurement{}'.format(i) not in qDict:
                continue
            name = qDict['measurement{}'.format(i)]
            for tag in ('lat{}', 'lon{}', 'start{}', 'end{}'):
                if tag.format(i) not in qDict:
                    errors += "No tag " + tag.format(i) + " for "  + name + "\n"
            if len(errors) > 0:
                continue
            match = re.search(r'(\d{4})-(\d{2})-(\d{2})[\+\s]+(\d{1,2})', qDict['start{}'.format(i)])
            if match:
                startDT = datetime.datetime(*tuple(map(int, list(match.group(1,2,3,4)))))
            else:
                errors += "Cannot interprete startTime: {0}\n".format(qDict['start{}'.format(i)])
            match = re.search(r'(\d{4})-(\d{2})-(\d{2})[\+\s]+(\d{1,2}):(\d{1,2})', qDict['end{}'.format(i)])
            if match:
                endDT = datetime.datetime(*tuple(map(int, list(match.group(1,2,3,4)))))
                if int(match.group(5)) > 0:
                    endDT = endDT + datetime.timedelta(hours=1)
            else:
                errors += "Cannot interprete endTime: {0}\n".format(qDict['end{}'.format(i)])
            if startDT >= endDT:
                errors += "Start must be before end for {}\n".format(name)
            lat = qDict['lat{}'.format(i)]
            lon = qDict['lon{}'.format(i)]
            try:
                latf = Snappy.Utils.parseLat(lat)
                lonf = Snappy.Utils.parseLon(lon)
            except ValueError as ve:
                latf = 0.
                lonf = 0.
                errors += "Cannot interprete latitude/longitude: {lat}/{lon}: {ex}\n".format(lat=lat,lon=lon,ex=ve);
            if len(errors) == 0:
                self.measurements.append(Measurement(i,name,lonf, latf, startDT, endDT))


        debug("output directory: {}".format(self.lastOutputDir))
        if not os.path.isdir(self.lastOutputDir):
            try:
                os.mkdir(self.lastOutputDir)
            except:
                errors += "cannot create directory: {}".format(self.lastOutputDir)
        else:
            errors += "cowardly refusing to write into existing directory: {}".format(self.lastOutputDir)

        if (len(errors) > 0):
            debug('updateSnapLog("{0}");'.format(json.dumps("ERRORS:\n\n"+errors)))
            self.write_log("ERRORS:\n\n{0}".format(errors))
            return

        curtime = gmtime()
        self.lastQDict = qDict
        self.write_log("working with {number} measurements in {dir}".format(number=len(self.measurements), dir=self.lastOutputDir))
        

        # write snap.input files
        for mes in self.measurements:
            print("{id} {name}".format(id=mes.id,name=mes.name)) 
            releaseDT = mes.end - mes.start
            releaseH = releaseDT.days * 24 + math.ceil(releaseDT.seconds/3600)
            sourceTerm = """
SIMULATION.START.DATE={simStart}
SET_RELEASE.POS= P=   {lat},   {lon}
TIME.START= {startTime}
TIME.RUN = {runTime}h
MAX.PARTICLES.PER.RELEASE= 4000
TIME.RELEASE.PROFILE.STEPS
STEP.HOUR.OUTPUT.FIELDS= 1
RELEASE.HOUR= 0, {releaseTime}
RELEASE.RADIUS.M= {radius}, {radius}
RELEASE.LOWER.M= {lowerHeight}, {lowerHeight}
RELEASE.UPPER.M= {upperHeight}, {upperHeight}
RELEASE.BQ/SEC.COMP= 1e12, 1e12, 'Cs137'
"""
            self.lastSourceTerm = sourceTerm.format(simStart=strftime("%Y-%m-%d_%H:%M:%S",curtime),
                                                lat=mes.lat, lon=mes.lon, startTime=mes.end.strftime("%Y %m %d %H"),
                                                runTime=runTime,
                                                releaseTime=releaseH,
                                                radius=500,
                                                lowerHeight=0, upperHeight=250)


            with open(os.path.join(self.lastOutputDir, "snap.input{}".format(mes.id)),'w') as fh:
                fh.write(self.lastSourceTerm)
                # add Cs137 definition
                fh.write(self.res.isotopes2snapinput([169]))

            metmodel = 'nrpa_ec_0p1'
            if (metmodel == 'nrpa_ec_0p1'):
                if ('metpattern' in qDict):
                    files = self.res.getECMeteorologyFiles(startDT, runTime, pattern=qDict['metpattern'])
                    if (len(files) == 0):
                        self.write_log("no EC met-files found for {}, runtime {} with pattern {}".format(startDT, runTime, qDict['metpattern']))
                        return
                else:
                    files = self.res.getECMeteorologyFiles(startDT, runTime)
                    if (len(files) == 0):
                        self.write_log("no EC met-files found for {}, runtime {}".format(startDT, runTime))
                        return
                if (not self._defaultDomainCheck(lonf,latf)):
                    return                
                snapIn = self.res.getSnapInputMetDefinitions(metmodel, files)
                snapIn = snapIn.replace("snap.", "snap{}.".format(mes.id)) # replace snap.nc and snap.log to snap1.nc snap1.log
                with open(os.path.join(self.lastOutputDir, "snap.input{}".format(mes.id)),'a') as fh:
                    fh.write(snapIn)
        
        snapscript = os.path.join(self.lastOutputDir, "snap.sh")
        with open(snapscript,'a') as fh:
            fh.write("#! /bin/bash\n")
            fh.write("cd {}\n".format(self.lastOutputDir))
            ids = " ".join([str(x.id) for x in self.measurements])
            fh.write(r'parallel -i -j 4 bsnap_naccident snap.input{} -- ' +ids + "\n")
            joinIds = " ".join(["-i snap{}.nc".format(x.id) for x in self.measurements]) 
            fh.write("snapCombineInverse -I Cs137 -o snapCombined.nc {}\n".format(joinIds))
        
        # create diana.setup
        with open(os.path.join(self.lastOutputDir, "diana.setup"), 'w') as fh:
            fh.write('''
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
m=combined t=fimex format=netcdf f={dir}/snapCombined.nc
            '''.format(dir=self.lastOutputDir))
            for m in self.measurements:
                fh.write("m={name} t=fimex format=netcdf f={dir}/snap{id}.nc\n".format(name=m.name, dir=self.lastOutputDir, id=m.id))
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
        #MainBrowserWindow._default_form_handler(qDict)
        self.write_log("updating...")
        if os.path.isfile(os.path.join(self.lastOutputDir,"snap.log.out")) :
            lfh = open(os.path.join(self.lastOutputDir,"snap.log.out"))
            debug(tail(os.path.join(self.lastOutputDir,"snap.log.out"),30))
            self.write_log(tail(os.path.join(self.lastOutputDir,"snap.log.out"), 30))
            lfh.close()

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
    ctr = SnapControllerInverse()
    sys.exit(app.exec_())

