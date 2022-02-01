# SNAP: Severe Nuclear Accident Programme
# Copyright (C) 1992-2018   Norwegian Meteorological Institute
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
Created on Mar 8, 2018

@author: Heiko Klein
"""

import os
import re


class SnapJobEC:
    """tasks to work with the model SNAP, SNAPGLOBAL and TRAJ with EC-data"""

    def __init__(self, task, hpc):
        """ construct a snap-job with a task (see snapRemoteRunner)  and a hpc"""
        self.task = task
        self.hpc = hpc

    def get_return_filename(self):
        """return the filename expected by argos when return a file"""
        if self.task.model == "TRAJ":
            return self.task.id + "_TRAJ2ARGOS.zip"
        elif re.match(r"^SNAP.*", self.task.model):
            return self.task.id + "_" + self.task.model + "2ARGOS.zip"
        else:
            raise Exception("unknown model:" + self.task.model)

    def get_input_files(self):
        """return a list of input-files for each model"""
        if self.task.model == "TRAJ":
            return [self.task.id + "_TRAJ_input"]
        elif re.match(r"^SNAP.*", self.task.model):
            return [self.task.id + "_Rimsterm.xml", self.task.id + "_SNAP_request.xml"]
        else:
            raise Exception("unknown model:" + self.task.model)

    def job_script(self):
        """return a sge job-script for the different models """
        if self.task.model == "SNAP":
            metmodel = "nrpa_ec_0p1"
        elif self.task.model == "SNAPGLOBAL":
            metmodel = "nrpa_ec_0p1_global"
        elif self.task.model == "SNAPNORDIC":
            metmodel = "meps_2_5km"
        elif self.task.model == "SNAPICONGLOBAL":
            metmodel = "icon_0p25_global"
        else:
            raise Exception("unknown model:" + self.task.model)

        (xmlfile, requestfile) = self.get_input_files()
        argosrequest = ""
        if os.path.exists(os.path.join(self.task.rundir, requestfile)):
            argosrequest = "--argosrequest " + requestfile

        # Create qsub script
        script = """#!/bin/bash
#$ -N dsa_bsnap
#$ -S /bin/bash
#$ -V
#$ -j n
#$ -r y
#$ -l h_rt=2:00:00
#$ -l h_vmem=8G
#$ -M beredskap-fou-kl@met.no
#$ -m a
#$ -P dsa
#$ -pe shmem-1 1
#$ -q operational-bionic.q
#$ -sync no
#$ -o {rundir}/$JOB_NAME.$JOB_ID.logout
#$ -e {rundir}/$JOB_NAME.$JOB_ID.logerr

chmod g+rw {rundir}/$JOB_NAME.$JOB_ID.logout
chmod g+rw {rundir}/$JOB_NAME.$JOB_ID.logerr

function send_status()
{{
    scp {scpoptions} {statusfile} {scpdestination}
}}

function send_msg()
{{
    code=$1
    msg="$2"
    TS=`date +%Y%m%d%H%M`
    echo $code":"$TS":"$msg > {statusfile} && send_status
}}


module load SnapPy/2.0.7
#module load fimex/1.5.0

ulimit -c 0
export OMP_NUM_THREADS=1

cd {rundir}
send_msg 101 "Starting run for {model} (timeout: 2h)"
snap4rimsterm --rimsterm {xmlfile} {argosrequest} --dir . --ident {ident}_SNAP --metmodel {metmodel}
if [ $? -ne 0 ]; then
    send_msg 409 "{model} output data does not exist, snap4rimsterm failed"
    exit 1;
fi
ncatted -a title,global,o,c,"{ident}" snap.nc
if [ $? -ne 0 ]; then
    send_msg 410 "{model} internal error, ncatted failed"
    exit 1;
fi


# create and deliver the file
zip {zipreturnfile} {ident}_SNAP_conc {ident}_SNAP_dose {ident}_SNAP_depo {ident}_SNAP_prec {ident}_SNAP_wetd {ident}_SNAP_tofa {ident}_SNAP_all.nc {ident}_SNAP_IncidentFile.zip
if [ $? -ne 0 ]; then
    send_msg 410 "{model} internal error, zip failed"
    exit 1;
fi
scp {scpoptions} {zipreturnfile} {scpdestination}
if [ $? -ne 0 ]; then
    send_msg 410 "{model} internal error copying data to destination"
    exit 1;
fi
send_msg 202 "Finished extracting {model} data for ARGOS"
exit 0;
""".format(
            rundir=self.task.rundir,
            ident=self.task.id,
            xmlfile=xmlfile,
            argosrequest=argosrequest,
            metmodel=metmodel,
            zipreturnfile=self.get_return_filename(),
            model=self.task.model,
            statusfile=self.task.status_filename(),
            scpoptions=self.task.scpoptions,
            scpdestination=self.task.scpdestination,
        )

        return script
