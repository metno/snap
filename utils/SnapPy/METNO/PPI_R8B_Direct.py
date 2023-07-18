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
Created on Apr 28, 2017

@author: heikok
"""
import os
from subprocess import TimeoutExpired
from time import sleep
import unittest

from METNO.HPC import HPC, StatusFile, QJobStatus
from METNO.SGEQueue import SGEQueue
from METNO.DirectConnection import DirectConnection


class PPI_R8B_Direct(HPC):
    """
    Implementation of a HPC machine for PPI-r8
    """

    def __init__(self):
        """
        Constructor
        """
        connection = DirectConnection()
        queue = SGEQueue()
        super().__init__(connection, queue)


class TestPPI_R8B_Direct(unittest.TestCase):
    """tests for alvin, only working when having an existing forecast account on alvin"""

    def setUp(self):
        unittest.TestCase.setUp(self)
        self.hpc = HPC.by_name("ppi_r8b_direct")
        self.rdir = "/lustre/storeB/users/heikok/tmp/metno_hpc_test"
        self.testFiles = ["script.job", "status"]

    def tearDown(self):
        unittest.TestCase.tearDown(self)
        for f in self.testFiles:
            if os.path.exists(f):
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
        with open(self.testFiles[0], "w") as fh:
            fh.write(
                """#! /bin/bash

sleep 8
echo "finished" > {status}
            """.format(
                    status=status_file,
                )
            )
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
