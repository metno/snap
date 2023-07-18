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
Created on Nov 17, 2016

@author: heikok
"""
import re
import sys
import unittest

from METNO.HPC import typed_property, Queue, QJob, QJobStatus


class SGEQJob(QJob):
    jobid = typed_property("jobid", str)

    def __init__(self, jobid):
        super().__init__()
        self.jobid = jobid


class SGEQueue(Queue):
    def __init__(self):
        super().__init__()

    def submit_command(self, jobscript, args):
        """return the submit command, e.g. qsub or sbatch

        Keyword arguments:
            jobscript -- the jobscript to submit
            args -- arguments to the jobscript

        Returns: the complete command as tuple (program, args), e.g. (qsub, [jobscript arg1 arg2 arg3]
        """
        jobargs = [jobscript]
        jobargs.extend(args)
        return ("qsub", jobargs)

    def parse_submit(self, command_output, command_error, returncode):
        """parse the output from the job-submission and return a QJob object"""
        if returncode == 0:
            jobid = self._parse_int(command_output.strip())
            assert jobid != 0
            return SGEQJob("{:d}".format(jobid))
        else:
            print(
                "qsub failed with code {}: {}".format(returncode, command_error),
                file=sys.stderr,
            )
        return None

    def status_command(self, qJob):
        """return the status command for the QJob"""
        assert isinstance(qJob, SGEQJob)
        return ("qstat", [])

    def delete_command(self, qJob):
        """return the delete command for the QJob"""
        assert isinstance(qJob, SGEQJob)
        return ("qdel", ["{}".format(qJob.jobid)])

    def _parse_int(self, string):
        m = re.search(r"Your job (\d+)", string)
        if m:
            return int(m.group(1))
        return 0

    def _pure_parse_status(self, qJob, status_output):
        for s in status_output.splitlines():
            fields = s.split()
            if len(fields) >= 5 and fields[0] == qJob.jobid:
                if re.match(r"(q|qw|hqw|hRwq)", fields[4]):
                    return QJobStatus.queued
                elif re.match(r"(r|t|Rr|Rt)", fields[4]):
                    return QJobStatus.running
                else:
                    return QJobStatus.finished
        return QJobStatus.finished

    def parse_status(self, qJob, status_output, status_err, returncode):
        """return the QJobStatus the QJob, except for testing for the status-file"""

        assert isinstance(qJob, SGEQJob)
        if returncode == 0:
            return self._pure_parse_status(qJob, status_output)
        else:
            return QJobStatus.unknown


class TestSGEQueue(unittest.TestCase):
    def setUp(self):
        super().setUp()
        self.queue = SGEQueue()
        self.jobid = "8133836"

    def test_parse_status(self):
        status_output = """
job-ID  prior   name       user         state submit/start at     queue                          slots ja-task-ID
-----------------------------------------------------------------------------------------------------------------
8133835 15.50032 SAsh010430 heikok       r     08/01/2017 07:15:04 ded-parallelx.q@c6220ii-bvz1zz     1
8133836 15.50032 test.sh    heikok       r     08/01/2017 07:15:04 ded-parallelx.q@c6220ii-bvz1zz     1
8133837 15.50032 SAsh010345 heikok       r     08/01/2017 07:15:04 ded-parallelx.q@c6220ii-bvz1zz     1
8089930 15.50000 osisaf_log steinare     Eqw   07/29/2017 18:30:02                                    1
"""
        qJob = SGEQJob("8089930")
        self.assertEqual(
            self.queue.parse_status(qJob, status_output, "", 0),
            QJobStatus.finished,
            "parsing returncode",
        )
        qJob = SGEQJob(self.jobid)
        self.assertEqual(
            self.queue.parse_status(qJob, status_output, "", 0),
            QJobStatus.running,
            "parsing output",
        )

    def test_parse_submit(self):
        command_output = """Your job 8133836 ("test.sh") has been submitted
"""
        self.assertEqual(
            self.queue.parse_submit(command_output, "", 0).jobid,
            self.jobid,
            "parsing qsub command",
        )


if __name__ == "__main__":
    unittest.main()
