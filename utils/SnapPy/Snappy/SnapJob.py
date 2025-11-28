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
from Snappy.Resources import MetModel

class UnknownModelException(Exception):
    """Exception when wrong model/prefix-name is send to SnapJob"""

    pass


class SnapJob:
    """tasks to work with the model SNAP, SNAPGLOBAL and TRAJ with EC-data"""

    def __init__(self, task, hpc):
        """construct a snap-job with a task (see snapRemoteRunner)  and a hpc"""
        self.task = task
        self.hpc = hpc

    def get_return_filename(self):
        """return the filename expected by argos when return a file"""
        if self.task.model == "TRAJ":
            return self.task.id + "_TRAJ2ARGOS.zip"
        elif re.match(r"^SNAP.*", self.task.model):
            return self.task.id + "_" + self.task.model + "2ARGOS.zip"
        else:
            raise UnknownModelException("unknown model:" + self.task.model)

    def get_input_files(self):
        """return a list of input-files for each model"""
        if self.task.model == "TRAJ":
            return [self.task.id + "_TRAJ_input"]
        elif re.match(r"^SNAP.*", self.task.model):
            return [self.task.id + "_Rimsterm.xml", self.task.id + "_SNAP_request.xml"]
        else:
            raise UnknownModelException("unknown model:" + self.task.model)

    def job_script(self):
        """return a sge job-script for the different models
        allow for SNAP, SNAPGLOBAL, SNAPNORDIC, SNAPICONGLOBAL
        and       SNAPBOMB, SNAPBOMBERA5, SNAPBOMBGLOBAL, SNAPBOMBNORDIC, SNAPBOMBICONGLOBAL
        and       SNAPOPBOMB, SNAPOPBOMBERA5, SNAPOPBOMBGLOBAL, SNAPOPBOMBNORDIC, SNAPOPBOMBICONGLOBAL
        """
        argos_operational = ""
        if self.task.model.startswith("SNAPBOMB"):
            task_model = self.task.model[8:]
        elif self.task.model.startswith("SNAPOPBOMB"):
            task_model = self.task.model[10:]
            argos_operational = "--argos_operational"
        elif self.task.model.startswith("SNAP"):
            task_model = self.task.model[4:]
        else:
            raise UnknownModelException("unknown model:" + self.task.model)
        if task_model == "":
            metmodel = MetModel.NrpaEC0p1
        elif task_model == "GLOBAL":
            metmodel = MetModel.EC0p1Global
        elif task_model == "NORDIC":
            metmodel = MetModel.Meps2p5
        elif task_model == "ICONGLOBAL":
            metmodel = MetModel.Icon0p25Global
        elif task_model == "ERA5":
            metmodel = MetModel.Era5Nancy
        else:
            raise UnknownModelException("unknown model:" + self.task.model)

        (xmlfile, requestfile) = self.get_input_files()
        argosrequest = ""
        if os.path.exists(os.path.join(self.task.rundir, requestfile)):
            argosrequest = "--argosrequest " + requestfile

        module_to_load = os.getenv("SNAP_MODULE", default="SnapPy/2.4.4")
        mem_options = "h_rss=8G,mem_free=8G,h_data=8G"
        queue = self.task.queue

        # Create qsub script
        script = """#!/bin/bash
#$ -N dsa_bsnap
#$ -S /bin/bash
#$ -v STORE
#$ -j n
#$ -r y
#$ -l h_rt=2:00:00
#$ -l {mem_options}
#$ -M beredskap-fou-kl@met.no
#$ -m a
#$ -P dsa
#$ -pe shmem-1 1
#$ -q {queue}
#$ -sync no
#$ -o {rundir}/$JOB_NAME.$JOB_ID.logout
#$ -e {rundir}/$JOB_NAME.$JOB_ID.logerr

chmod g+rw {rundir}/$JOB_NAME.$JOB_ID.logout
chmod g+rw {rundir}/$JOB_NAME.$JOB_ID.logerr

function send_status()
{{
    rsync --archive --rsh 'ssh {sshoptions}' {statusfile} {scpdestination}
}}

function send_msg()
{{
    code=$1
    msg="$2"
    TS=`date +%Y%m%d%H%M`
    echo $code":"$TS":"$msg > {statusfile} && send_status
}}


module load {module_to_load}

ulimit -c 0
# run might have many particles, use two hyper-threads
export OMP_NUM_THREADS=2
export OMP_THREAD_LIMIT=2

cd {rundir}
send_msg 102 "Starting run for {model} (timeout: 2h)"
snap4rimsterm --rimsterm {xmlfile} {argosrequest} {argos_operational} --dir . --ident {ident}_SNAP --metmodel {metmodel} --bitmapCompress
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
zip {zipreturnfile} {ident}_SNAP_conc {ident}_SNAP_coco {ident}_SNAP_dose {ident}_SNAP_depo {ident}_SNAP_prec {ident}_SNAP_wetd {ident}_SNAP_tofa {ident}_SNAP_all.nc {ident}_SNAP_IncidentFile.zip
if [ $? -ne 0 ]; then
    send_msg 410 "{model} internal error, zip failed"
    exit 1;
fi
rsync --archive --rsh 'ssh {sshoptions}' {zipreturnfile} {scpdestination}
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
            argos_operational=argos_operational,
            metmodel=metmodel.value,
            zipreturnfile=self.get_return_filename(),
            model=self.task.model,
            statusfile=self.task.status_filename(),
            sshoptions=self.task.sshoptions,
            scpdestination=self.task.scpdestination,
            module_to_load=module_to_load,
            queue=queue,
            mem_options=mem_options,
        )

        return script
