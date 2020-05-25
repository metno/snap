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
'''
Created on Mar 8, 2018

@author: Heiko Klein
'''

import os
import re


class SnapJobEC():
    '''tasks to work with the model SNAP, SNAPGLOBAL and TRAJ with EC-data'''


    def __init__(self, task, hpc):
        ''' construct a snap-job with a task (see snapRemoteRunner)  and a hpc'''
        self.task = task
        self.hpc = hpc
        
    def get_return_filename(self):
        '''return the filename expected by argos when return a file'''
        if self.task.model == 'TRAJ':
            return self.task.id + '_TRAJ2ARGOS.zip'
        elif re.match(r'^SNAP.*', self.task.model):
            return self.task.id + '_' + self.task.model + '2ARGOS.zip'
        else:
            raise Exception('unknown model:' + self.task.model)

    def get_input_files(self):
        '''return a list of input-files for each model'''
        if self.task.model == 'TRAJ':
            return [self.task.id + '_TRAJ_input']
        elif re.match(r'^SNAP.*', self.task.model):
            return [self.task.id +'_Rimsterm.xml', self.task.id + '_SNAP_request.xml']
        else:
            raise Exception('unknown model:' + self.task.model)
                    
    def job_script(self):
        ''' return a sge job-script for the different models '''
        if self.task.model == 'TRAJ':
            script = '''#!/bin/bash
#$ -N nrpa_bsnap_traj
#$ -S /bin/bash
#$ -V
#$ -j n
#$ -r y
#$ -l h_rt=0:10:00
#$ -l h_vmem=8G
#$ -M heikok@met.no,andreb@met.no
#$ -m a
#$ -P dsa
#$ -pe mpi 1
#$ -q operationalx.q
#$ -sync no
#$ -o {rundir}/$JOB_NAME.$JOB_ID.logout
#$ -e {rundir}/$JOB_NAME.$JOB_ID.logerr

chmod g+rw {rundir}/$JOB_NAME.$JOB_ID.logout
chmod g+rw {rundir}/$JOB_NAME.$JOB_ID.logerr

module load SnapPy/1.3.0

ulimit -c 0
export OMP_NUM_THREADS=1

cd {rundir}
snap4rimsterm --trajInput {traj_in} --dir . --ident {ident}

# create and deliver the file
zip -l {zipreturnfile} Trajectory*.DAT
scp {scpoptions} {zipreturnfile} {scpdestination}
TS=`date +%Y%m%d%H%M`
echo "202:"$TS":Finished extracting {model} data for ARGOS" >> {statusfile}
scp {scpoptions} {statusfile} {scpdestination}

'''.format(rundir=self.task.rundir,
           ident=self.task.id,
           traj_in=self.get_input_files()[0],
           zipreturnfile=self.get_return_filename(),
           model=self.task.model,
           statusfile=self.task.status_filename(),
           scpoptions=self.task.scpoptions,
           scpdestination=self.task.scpdestination
           )
        
        elif re.match(r'^SNAP.*', self.task.model):
            worldwide = ""
            if re.search(r'GLOBAL', self.task.model):
                worldwide = "--worldwide"

            (xmlfile, requestfile) = self.get_input_files()
            argosrequest = ""
            if os.path.exists(os.path.join(self.task.rundir, requestfile)):
                argosrequest = '--argosrequest ' + requestfile
        
            # Create qsub script
            script = '''#!/bin/bash
#$ -N nrpa_bsnap
#$ -S /bin/bash
#$ -V
#$ -j n
#$ -r y
#$ -l h_rt=2:00:00
#$ -l h_vmem=8G
#$ -M heikok@met.no,andreb@met.no
#$ -m a
#$ -P dsa
#$ -pe mpi 1
#$ -q operationalx.q
#$ -sync no
#$ -o {rundir}/$JOB_NAME.$JOB_ID.logout
#$ -e {rundir}/$JOB_NAME.$JOB_ID.logerr

chmod g+rw {rundir}/$JOB_NAME.$JOB_ID.logout
chmod g+rw {rundir}/$JOB_NAME.$JOB_ID.logerr

module load SnapPy/1.3.0

ulimit -c 0
export OMP_NUM_THREADS=1

cd {rundir}
snap4rimsterm --rimsterm {xmlfile} {argosrequest} --dir . --ident {ident}_SNAP {worldwide}
ncatted -a title,global,o,c,"{ident}" snap.nc


# create and deliver the file
zip {zipreturnfile} {ident}_SNAP_conc {ident}_SNAP_dose {ident}_SNAP_depo {ident}_SNAP_prec {ident}_SNAP_wetd {ident}_SNAP_tofa {ident}_SNAP_all.nc
scp {scpoptions} {zipreturnfile} {scpdestination}
TS=`date +%Y%m%d%H%M`
echo "202:"$TS":Finished extracting {model} data for ARGOS" >> {statusfile}
scp {scpoptions} {statusfile} {scpdestination}

'''.format(rundir=self.task.rundir,
           ident=self.task.id,
           xmlfile=xmlfile,
           argosrequest=argosrequest,
           worldwide=worldwide,
           zipreturnfile=self.get_return_filename(),
           model=self.task.model,
           statusfile=self.task.status_filename(),
           scpoptions=self.task.scpoptions,
           scpdestination=self.task.scpdestination
           )
        
        else:
            raise Exception('unknown model:' + self.task.model)

        return script
