import sys
import re
import json
import os
from time import gmtime, strftime
from Snappy.MainBrowserWindow import MainBrowserWindow
from Snappy.Resources import Resources
from PyQt5 import QtWidgets, QtGui

def debug(*objs):
    print("DEBUG: ", *objs, file=sys.stderr)

class SnapController:
    def __init__(self):
        self.res = Resources()
        self.main = MainBrowserWindow()
        self.main.setWindowTitle('SNAPpy')
        self.main.setWindowIcon(QtGui.QIcon(self.res.getIconPath()))
        self.main.set_html(self.res.getStartScreen())
        self.main.set_form_handler(self._create_snap_form_handler())
        self.main.show()

    def write_log(self, str:str):
        debug(str)
        self.main.evaluate_javaScript('updateSnapLog({0});'.format(json.dumps(str)))

    def run_snap_query(self, qDict):
        debug("run_snap_query")
        for key, value in qDict.items():
            print(str.format("{0} => {1}", key, value))
        errors = ""
        match = re.search(r'(\d{4})-(\d{2})-(\d{2})[\+\s]+(\d{1,2})', qDict['startTime'])
        if match:
            startTime = "{0} {1} {2} {3}".format(*match.group(1,2,3,4))
            startYY = "{:02d}".format(int(match.group(1)))
            startmm = "{:02d}".format(int(match.group(2)))
            startDD = "{:02d}".format(int(match.group(3)))
            startHH = "{:02d}".format(int(match.group(4)))
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
        debug("output directory: {}".format(self.lastOutputDir))
        os.mkdir(self.lastOutputDir)
        fh = open(os.path.join(self.lastOutputDir, "snap.input"),'w')
        print("SET_RELEASE.POS= P=   {lat},   {lon}\n".format(lat=latf, lon=lonf), file=fh)
        print("RELEASE.HOUR= 0, {releaseTime}\n".format(releaseTime=qDict['releaseTime']), file=fh)
        print("TIME.START= {startTime}\n".format(startTime=startTime), file=fh)
        print("TIME.RUN = {runTime}h\n".format(runTime=qDict['runTime']), file=fh)

        snap_input = self.res.getSnapInputTemplate()
        print(snap_input, file=fh)
#
#     # fix diana-setup
#     {
#         open (my $diTmpl, "$FindBin::Bin/diana.setup.tmpl")
#             or die "Cannot read $FindBin::Bin/diana.setup.tmpl: $!\n";
#         local $/ = undef;
#         my $d = <$diTmpl>;
#         $d =~ s/%PWD%/$FindBin::Bin/g;
#         open ($oh, ">diana.setup")
#             or die "Cannot write diana.setup: $!\n";
#         print $oh $d;
#         close $oh;
#         close $diTmpl;
#     }
#     my $command = "./bsnap $inputFile";
#     print STDERR $command, "\n";
#     system($command);
#     $command = "fimex --input.file=snap.felt --input.config=felt2nc_snap.xml --output.file=snap.nc --output.type=nc4";
#     print STDERR $command, "\n";
#     system($command);
#     unlink("snap.felt");
#     chmod(0666, "snap.felt_level_names");
#
#     my $region = $params->{"region"};
#     my $startdiana = $params->{"startdiana"};
#     my $diVersion = $params->{"dianaversion"} || "";
#     print STDERR "diVersion: '$diVersion'\n";
#     if ($diVersion =~ /(\d*\.\d+)/) {
#         $diVersion = "-$1";
#     } else {
#         $diVersion = "";
#     }
#
#     if ($startdiana) {
#         system("diana.bin$diVersion -s diana.setup&");
#     }
#     if ($region) {
#         system("bdiana$diVersion -i snap.in -s diana.setup p='$region'");
#         system("rm -rf prod");
#         mkdir("prod") or print STDERR "Cannot create prod directory: $!\n";
#         system("mv snap_* prod");
#         system("./sendmail.sh prod/*.png");
#     }
#
#
#     return "<html><head><title>SNAP-Runner</title></head><body><h1>SNAP-Runner</h1>SNAP run successfull for: <p>Time: $params->{startTime} Length $params->{runTime}h<p>Place $npp<br>Lat: $lat<br>Lon: $lon<br><p>Release Scenario:<br>$releaseScenario<p> Start diana with <pre>diana.bin$diVersion -s $FindBin::Bin/diana.setup</pre><a href=\"default\">Start new run.</a></body></html>";
# }


    def update_log_query(self, qDict):
        MainBrowserWindow._default_form_handler(queryDict)
        self.write_log("updating...")


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




if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    ctr = SnapController()
    sys.exit(app.exec_())

