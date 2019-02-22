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
'''
Created on Nov 7, 2016

@author: heikok
'''
import os
from subprocess import TimeoutExpired
from time import sleep
import unittest

from METNO.HPC import HPC, StatusFile, QJobStatus
from METNO.SLURMQueue import SLURMQueue
from METNO.SSHConnection import SSHConnection


class Stratus(HPC):
    '''
    Implementation of a HPC machine for stratus.nsc.liu.se
    '''


    def __init__(self):
        '''
        Constructor
        '''
        connection = SSHConnection(username="metno_op", machine="stratus.nsc.liu.se", port=22)
        queue = SLURMQueue()
        super().__init__(connection, queue)


class TestStratus(unittest.TestCase):
    '''tests for stratus, only working when having an existing forecast account'''
    def setUp(self):
        unittest.TestCase.setUp(self)
        self.hpc = HPC.by_name("stratus")
        self.rdir = "/home/metno_op/work/emep/metno_hpc_test"
        self.strangeFiles = ["file with spaces", "file with wildcards*"]
        self.testFiles = ["script.job", "status"] + self.strangeFiles

    def tearDown(self):
        unittest.TestCase.tearDown(self)
        for f in self.testFiles:
            if (os.path.exists(f)):
                os.unlink(f)

    def test_connect(self):
        (out, error, retval) = self.hpc.syscall("echo", ["5"])
        self.assertEqual(retval, 0, "command succeeded")
        self.assertEqual(int(out), 5, "command output correct")


    def test_timeout(self):
        with self.assertRaises(TimeoutExpired):
            self.hpc.syscall("sleep", ["5"], timeout=1)

    def test_full(self):
        status_file = os.path.join(self.rdir, "status")
        self.hpc.syscall("rm", ["-r", self.rdir])
        self.hpc.syscall("mkdir", ["-p", self.rdir])
        
        for f in self.strangeFiles:
            with open(f, 'w') as fh:
                fh.write(f)
            self.hpc.put_files([f], self.rdir)
            self.hpc.syscall("ls", [f])
        
        with open(self.testFiles[0], "w") as fh:
            fh.write('''#! /bin/bash

#SBATCH -A met
#SBATCH --nodes=1 --ntasks-per-node=1 --time=01:00:00
#SBATCH -D {rdir}/
#SBATCH --mail-type=FAIL --mail-user=heiko.klein@met.no
#SBATCH -n1

sleep 8
echo "finished" > {status}
            '''.format(status=status_file, rdir=self.rdir))
        self.hpc.put_files([self.testFiles[0]], self.rdir)
        qjob = self.hpc.submit_job(os.path.join(self.rdir, self.testFiles[0]), [])
        self.assertIsNotNone(qjob, "job submitted")
        qjob.status_file = StatusFile(status_file, "finished")
        count = 100
        status = self.hpc.get_status(qjob)
        print(qjob.jobid)
        while not (status == QJobStatus.finished or status == QJobStatus.failed):
            sleep(5)
            count -= 1
            if count == 0:
                break
            status = self.hpc.get_status(qjob)

        self.assertEqual(status, QJobStatus.finished, "remote job successfully running")

        self.hpc.get_files([status_file])
        self.assertTrue(os.path.exists("status"))
        with open("status", "r") as sh:
            content = sh.read()
            self.assertEqual(content, "finished\n")

        self.hpc.syscall("rm", ["-r", self.rdir])

if __name__ == "__main__":
    unittest.main()
