'''
Created on Nov 7, 2016

@author: heikok
'''
import os
from subprocess import TimeoutExpired
from time import sleep
import unittest

from METNO.HPC import HPC, StatusFile, QJobStatus
from METNO.PBSQueue import PBSQueue
from METNO.SSHConnection import SSHConnection


class Vilje(HPC):
    '''
    Implementation of a HPC machine for vilje.notur.ntnu.no
    '''


    def __init__(self):
        '''
        Constructor
        '''
        connection = SSHConnection(username="forecast", machine="vilje.hpc.ntnu.no", port=22)
        queue = PBSQueue()
        super().__init__(connection, queue)


class TestVilje(unittest.TestCase):
    '''tests for vilje, only working when having an existing forecast account on vilje'''
    def setUp(self):
        unittest.TestCase.setUp(self)
        self.vilje = HPC.by_name("vilje")
        self.rdir = "/work/forecast/metno_hpc_test"
        self.testFiles = ["script.job", "status"]

    def tearDown(self):
        unittest.TestCase.tearDown(self)
        for f in self.testFiles:
            if (os.path.exists(f)):
                os.unlink(f)

    def test_connect(self):
        (out, error, retval) = self.vilje.syscall("echo", ["5"])
        self.assertEqual(retval, 0, "command succeeded")
        self.assertEqual(int(out), 5, "command output correct")


    def test_timeout(self):
        with self.assertRaises(TimeoutExpired):
            self.vilje.syscall("sleep", ["5"], timeout=1)

    def test_full(self):
        status_file = os.path.join(self.rdir, "status")
        self.vilje.syscall("rm", ["-r", self.rdir])
        self.vilje.syscall("mkdir", [self.rdir])
        with open(self.testFiles[0], "w") as fh:
            fh.write('''
#! /bin/bash
#PBS -l select=1:ncpus=1:mpiprocs=1:mem=1GB
#PBS -lwalltime=00:00:10
#PBS -W umask=0022
#PBS -A mipa01kl

echo "finished" > {}
            '''.format(status_file))
        self.vilje.put_files([self.testFiles[0]], self.rdir)
        qjob = self.vilje.submit_job(os.path.join(self.rdir, self.testFiles[0]), [])
        self.assertIsNotNone(qjob, "job submitted")
        qjob.status_file = StatusFile(status_file)
        count = 100
        status = self.vilje.get_status(qjob)
        while not (status == QJobStatus.finished or status == QJobStatus.failed):
            sleep(5)
            count -= 1
            if count == 0:
                break
            status = self.vilje.get_status(qjob)

        self.assertEqual(status, QJobStatus.finished, "remote job successfully running")

        self.vilje.get_files([status_file])
        self.assertTrue(os.path.exists("status"))
        with open("status", "r") as sh:
            content = sh.read()
            self.assertEqual(content, "finished\n")

        self.vilje.syscall("rm", ["-r", self.rdir])

if __name__ == "__main__":
    unittest.main()
