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
Created on Nov 17, 2016

@author: heikok
'''
import re
import sys
import unittest

from METNO.HPC import typed_property, Queue, QJob, QJobStatus


class PBSQJob(QJob):
    jobid = typed_property("jobid", str)

    def __init__(self, jobid):
        super().__init__()
        self.jobid = jobid


class PBSQueue(Queue):

    def __init__(self):
        super().__init__()

    def submit_command(self, jobscript, args):
        '''return the submit command, e.g. qsub or sbatch

        Keyword arguments:
            jobscript -- the jobscript to submit
            args -- arguments to the jobscript

        Returns: the complete command as tuple (program, args), e.g. (qsub, [jobscript arg1 arg2 arg3]
        '''
        jobargs = [jobscript]
        jobargs.extend(args)
        return ("qsub", jobargs)

    def parse_submit(self, command_output, command_error, returncode):
        '''parse the output from the job-submission and return a QJob object'''
        if (returncode == 0):
            jobid = command_output.strip()
            assert(jobid != '')
            return PBSQJob(jobid)
        else:
            print("qsub failed with code {}: {}".format(returncode, command_error),
                  file=sys.stderr)
        return None


    def status_command(self, qJob):
        '''return the status command for the QJob'''
        assert(isinstance(qJob, PBSQJob))
        return ("qstat", ["{}".format(qJob.jobid)])

    def delete_command(self, qJob):
        '''return the delete command for the QJob'''
        assert(isinstance(qJob, PBSQJob))
        return ("qdel", ["{}".format(qJob.jobid)])


    def _parse_int(self, string):
        m = re.search(r'(\d+)', string)
        if m:
            return int(m.group(1))
        return 0

    def _pure_parse_status(self, qJob, status_output):
        for s in status_output.splitlines():
            fields = s.split()
            if len(fields) >= 5 and fields[0] == qJob.jobid:
                if fields[4] == "F":
                    return QJobStatus.finished
                if fields[4] == "E":
                    return QJobStatus.finished
                elif fields[4] == "R":
                    return QJobStatus.running
                elif fields[4] == "B":
                    return QJobStatus.running
                else:
                    return QJobStatus.queued
        return QJobStatus.unknown


    def parse_status(self, qJob, status_output, status_err, returncode):
        '''return the QJobStatus the QJob, except for testing for the status-file'''

        assert(isinstance(qJob, PBSQJob))
        if returncode == 153:
            # unknown jobid = no longer existing
            return QJobStatus.finished
        elif returncode == 35:
            # job has finished
            return QJobStatus.finished
        elif returncode == 0:
            return self._pure_parse_status(qJob, status_output)
        else:
            return QJobStatus.unknown



class TestPBSQueue(unittest.TestCase):

    def setUp(self):
        super().setUp()
        self.queue = PBSQueue()
        self.jobid = "7483866.service2"


    def test_parse_status(self):
        status_output = '''
Job id            Name             User              Time Use S Queue
----------------  ---------------- ----------------  -------- - -----
7483866.service2  podV01.2         catharfl          00:00:07 R workq
'''
        qJob = PBSQJob(self.jobid)
        self.assertEqual(self.queue.parse_status(qJob, status_output, "", 35),
                         QJobStatus.finished, "parsing returncode")
        self.assertEqual(self.queue.parse_status(qJob, status_output, "", 0),
                         QJobStatus.running, "parsing output")

    def test_parse_submit(self):
        command_output = '''7483866.service2
'''
        self.assertEqual(self.queue.parse_submit(command_output, "", 0).jobid,
                         self.jobid, "parsing qsub command")


if __name__ == "__main__":
    unittest.main()
