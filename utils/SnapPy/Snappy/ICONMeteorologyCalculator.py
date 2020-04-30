# SNAP: Servere Nuclear Accident Programme
# Copyright (C) 1992-2020   Norwegian Meteorological Institute
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
'''
Created on Oct 24, 2016

@author: heikok
'''

from datetime import datetime, timedelta
from glob import iglob
import math
import os
import subprocess
import time

from Snappy.Resources import Resources, MetModel
import Snappy.MeteorologyCalculator

class ICONMeteorologyCalculator(Snappy.MeteorologyCalculator.MeteorologyCalculator):
    '''Calculate dwd icon-meteorology'''

    @staticmethod
    def getGlobalMeteoResources():
        '''retrieve the GlobalMeteoResources from internal resources'''
        gres = Resources()
        res = Snappy.MeteorologyCalculator.GlobalMeteoResource()
        res.indirs = gres.MET_GLOBAL_INPUTDIRS[MetModel.Icon0p25Global]
        # icon_20200427T00Z.nc
        res.pathglob = "icon_????????T??Z.nc"
        res.pathptime = "icon_%Y%m%dT%HZ.nc"
        res.outputdir = gres.getSnapOutputDir()
        res.output_filename_pattern = gres.MET_FILENAME_PATTERN[MetModel.Icon0p25Global] # keeping filename for extracted data
        res.domainHeight = gres.ecDomainHeight # reuse domainHeight/Width
        res.domainWidth = gres.ecDomainWidth
        res.domainDeltaX = 0.25
        res.domainDeltaY = 0.25
        return res

#    def __init__(self, res: Snappy.MeteorologyCalculator.GlobalMeteoResource, dtime: datetime, domainCenterX, domainCenterY):
#        super(res, dtime, domainCenterX, domainCenterY)

    def add_expected_files(self, date):
        self.files = []
        self.optFiles = []
        # only one file expected
        self.files.append(os.path.join(self.outputdir,
                                       self.res.output_filename_pattern.format(year=date.year,
                                                                               month=date.month,
                                                                               day=date.day,
                                                                               UTC=date.hour)))
        return


    def calc(self, proc=None):
        '''run the calculation of ec-data if required.
        
        Args:
           proc -- A QProcess, which will be used to run a longer process in the background.
                  STDERR/STDOUT and signal-handler should be set. If proc is None, the
                  subprocess will be run in the current-process. If proc is set, the caller
                  needs to wait for the proc to finish before calling other methods of this object
'''
        if (not self.must_calc()):
            return

        precommand = '''#! /bin/bash
. /usr/share/modules/init/bash
module load ecdis4cwf/1.2.0
export OMP_NUM_THREADS=1
export DATE='{year:04d}{month:02d}{day:02d}'
export UTC='{utc}'

umask 000
cd {outdir}
touch {outdir}/running
WORKDIR={outdir}/work
mkdir $WORKDIR
ECDIS_PARALLEL=0 NREC_DAY_MIN=2 NDAYS_MAX=3 DOMAIN=VARIABLE LON_DEF={lon0}.,{dlon},{nx} LAT_DEF={lat0}.,{dlat},{ny} ECDIS={globalfile} OUTDIR={outdir} ECDIS_TMPDIR=$WORKDIR $ECDIS_MODULE_PATH/ecdis4cwf.sh
rm {outdir}/running
'''
        command = precommand.format(year=self.date.year,
                                    month=self.date.month,
                                    day=self.date.day,
                                    utc=self.date.hour,
                                    outdir=self.outputdir,
                                    lon0=self.lon0,
                                    lat0=self.lat0,
                                    dlon=self.res.domainDeltaX,
                                    dlat=self.res.domainDeltaY,
                                    nx=round(self.res.domainWidth/self.res.domainDeltaX)+1,
                                    ny=round(self.res.domainHeight/self.res.domainDeltaY)+1,
                                    globalfile=self.globalfile,
                                    )
        scriptFile = os.path.join(self.outputdir, "command.sh")
        with open(scriptFile, 'w') as script:
            script.write(command)

        if proc is None:
            subprocess.call(['/bin/bash', scriptFile])
        else:
            self.proc = proc # make sure proc lives long enough
            proc.start('/bin/bash', [scriptFile])

        return

if __name__ == "__main__":
    print(ICONMeteorologyCalculator.findGlobalData(ICONMeteorologyCalculator.getGlobalMeteoResources(), datetime.strptime("2020-04-27T00", "%Y-%m-%dT%H")))
    try:
        ICONMeteorologyCalculator.findGlobalData(ICONMeteorologyCalculator.getGlobalMeteoResources(), datetime.strptime("2010-10-24T00", "%Y-%m-%dT%H"))
    except Exception as e:
        print(e.args[0])
#    print(EcMeteorologyCalculator(Resources(), datetime.strptime("2016-10-24T00", "%Y-%m-%dT%H"), 63, 42, None))
    met = ICONMeteorologyCalculator(ICONMeteorologyCalculator.getGlobalMeteoResources(), datetime.strptime("2020-04-29T00", "%Y-%m-%dT%H"), -159, 20) # hawaii
    print("recalc: ", met.must_calc())
