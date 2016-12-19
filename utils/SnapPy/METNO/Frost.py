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


class Frost(HPC):
    '''
    Implementation of a HPC machine for frost.nsc.liu.se
    '''


    def __init__(self):
        '''
        Constructor
        '''
        connection = SSHConnection(username="metno_op", machine="frost.nsc.liu.se", port=22)
        queue = SLURMQueue()
        super().__init__(connection, queue)


class TestFrost(unittest.TestCase):
    '''tests for frost, only working when having an existing forecast account on frost'''
    def setUp(self):
        unittest.TestCase.setUp(self)
        self.hpc = HPC.by_name("frost")
        self.rdir = "/home/metno_op/work/emep/metno_hpc_test"
        self.testFiles = ["script.job", "status"]

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
        self.hpc.syscall("mkdir", [self.rdir])
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
