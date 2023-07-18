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
"""
Created on Oct 24, 2016

@author: heikok
"""

from datetime import datetime, timedelta
from glob import iglob
import math
import os
import subprocess
import time

from Snappy.Resources import Resources, MetModel
import Snappy.MeteorologyCalculator


class ECDataNotAvailableException(Exception):
    def __init__(self, value):
        """exception having some kind of documention in args[0]"""
        self.parameter = value

    def __str__(self):
        return repr(self.parameter)


class EcMeteorologyCalculator(Snappy.MeteorologyCalculator.MeteorologyCalculator):
    """Calculate ec-meteorology"""

    @staticmethod
    def findECGlobalData(dtime: datetime):
        """Static method to find the closest global ec dataset earlier than dtime.

        Args:
            dtime: datetime object with a start-time, which should be included in the dataset


        Returns:
            A tuple with referencetime and filename

        Raises:
            ECDataNotAvailableException: no data for the dtime can be found
        """
        return Snappy.MeteorologyCalculator.MeteorologyCalculator.findGlobalData(
            EcMeteorologyCalculator.getGlobalMeteoResources(), dtime
        )

    @staticmethod
    def getGlobalMeteoResources():
        """retrieve the GlobalMeteoResources from internal resources"""
        ecres = Resources()
        res = Snappy.MeteorologyCalculator.GlobalMeteoResource()
        res.indirs = ecres.getMetGlobalInputDirs(MetModel.NrpaEC0p1Global)
        res.pathglob = "ec_atmo_0_1deg_????????T??????Z_3h.nc"
        res.pathptime = "ec_atmo_0_1deg_%Y%m%dT%H%M%SZ_3h.nc"
        res.path_grace_period_sec = (
            2 * 60
        )  # 2min grace to ensure lustre cross-dir mv finishes
        res.outputdir = ecres.getSnapOutputDir()
        res.output_filename_pattern = ecres.EC_FILENAME_PATTERN
        res.domainHeight = ecres.ecDomainHeight
        res.domainWidth = ecres.ecDomainWidth
        res.domainDeltaX = ecres.ecDomainRes
        res.domainDeltaY = ecres.ecDomainRes
        res.timeoffset = (
            3  # required offset between reference-time and first useful startup-time
        )
        return res

    #    def __init__(self, res: Snappy.MeteorologyCalculator.GlobalMeteoResource, dtime: datetime, domainCenterX, domainCenterY):
    #        super(res, dtime, domainCenterX, domainCenterY)

    def add_expected_files(self, date):
        self.files = []
        self.optFiles = []
        for i in (0, 1, 2):
            self.files.append(
                os.path.join(
                    self.outputdir,
                    self.res.output_filename_pattern.format(
                        year=date.year, month=date.month, day=date.day, dayoffset=i
                    ),
                )
            )
        for i in (3, 4, 5):
            self.optFiles.append(
                os.path.join(
                    self.outputdir,
                    self.res.output_filename_pattern.format(
                        year=date.year, month=date.month, day=date.day, dayoffset=i
                    ),
                )
            )
        return

    def calc(self, proc=None):
        """run the calculation of ec-data if required.

        Args:
           proc -- A QProcess, which will be used to run a longer process in the background.
                  STDERR/STDOUT and signal-handler should be set. If proc is None, the
                  subprocess will be run in the current-process. If proc is set, the caller
                  needs to wait for the proc to finish before calling other methods of this object"""
        if not self.must_calc():
            return
        #        if 'MODULESHOME' not in os.environ:
        #            raise ECDataNotAvailableException("unable to load module")

        precommand = """#! /bin/bash
source /etc/profile.d/modules.sh
release=$(lsb_release --codename --short)
if [[ "$release" == "Ootpa" ]]; then
    module use /modules/MET/rhel8/user-modules/ /modules/MET/rhel8/user-modules/fou-modules
    module load ecdis4cwf/1.6.1
elsif [[ "$release" == "bionic" ]]; then
    module load ecdis4cwf/1.2.6
else
    echo "ERROR: unknown OS release $release, don't know how to load ecdis4cwf"
    exit 1
fi

export OMP_NUM_THREADS=1
export DATE='{year:04d}{month:02d}{day:02d}'
export UTC='{utc}'

umask 000
cd {outdir}
touch {outdir}/running
WORKDIR={outdir}/work
mkdir -p -- $WORKDIR
echo "Preprocessing 3days EC meteorology, please wait ca. 15min"
ECDIS_PARALLEL=0 NREC_DAY_MIN=2 NDAYS_MAX=3 DOMAIN=VARIABLE LON_DEF={lon0}.,{dlon},{nx} LAT_DEF={lat0}.,{dlat},{ny} ECDIS={globalfile} OUTDIR={outdir} ECDIS_TMPDIR=$WORKDIR $ECDIS_MODULE_PATH/ecdis4cwf.sh
rm {outdir}/running
"""
        command = precommand.format(
            year=self.date.year,
            month=self.date.month,
            day=self.date.day,
            utc=self.date.hour,
            outdir=self.outputdir,
            lon0=self.lon0,
            lat0=self.lat0,
            dlon=self.res.domainDeltaX,
            dlat=self.res.domainDeltaY,
            nx=round(self.res.domainWidth / self.res.domainDeltaX) + 1,
            ny=round(self.res.domainHeight / self.res.domainDeltaY) + 1,
            globalfile=self.globalfile,
        )
        scriptFile = os.path.join(self.outputdir, "command.sh")
        with open(scriptFile, "w") as script:
            script.write(command)

        if proc is None:
            subprocess.call(["/bin/bash", scriptFile])
        else:
            self.proc = proc  # make sure proc lives long enough
            proc.start("/bin/bash", [scriptFile])

        return


if __name__ == "__main__":
    print(
        EcMeteorologyCalculator.findECGlobalData(
            datetime.strptime("2020-04-29T00", "%Y-%m-%dT%H")
        )
    )
    try:
        EcMeteorologyCalculator.findECGlobalData(
            datetime.strptime("2010-10-24T00", "%Y-%m-%dT%H")
        )
    except Exception as e:
        print(e.args[0])
    #    print(EcMeteorologyCalculator(Resources(), datetime.strptime("2016-10-24T00", "%Y-%m-%dT%H"), 63, 42, None))
    ecmet = EcMeteorologyCalculator(
        EcMeteorologyCalculator.getGlobalMeteoResources(),
        datetime.strptime("2020-04-29T00", "%Y-%m-%dT%H"),
        -159,
        20,
    )  # hawaii
    print("recalc: ", ecmet.must_calc())
