'''
Created on Aug 08, 2017

@author: heikok
'''
import os
from subprocess import TimeoutExpired
from time import sleep
import unittest

from METNO.HPC import HPC, StatusFile, QJobStatus
from METNO.SGEQueue import SGEQueue
from METNO.SSHConnection import SSHConnection


class PPI(HPC):
    '''
    Implementation of a HPC machine for alvin.nsc.liu.se
    '''


    def __init__(self):
        '''
        Constructor
        '''
        connection = SSHConnection(machine="xvis-m3b.met.no", port=22)
        queue = SGEQueue()
        super().__init__(connection, queue)


class TestPPI(unittest.TestCase):
    '''tests for ppi, only working when having an existing account on ppi'''
    def setUp(self):
        unittest.TestCase.setUp(self)
        self.hpc = HPC.by_name("ppi")
        self.rdir = "/lustre/storeB/users/heikok/tmp/metno_hpc_test"
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
        self.hpc.syscall("mkdir", ["-p", self.rdir])
        with open(self.testFiles[0], "w") as fh:
            fh.write('''#! /bin/bash

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
