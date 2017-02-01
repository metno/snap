'''
Created on Nov 17, 2016

@author: heikok
'''
import re
import sys
import unittest

from METNO.HPC import typed_property, Queue, QJob, QJobStatus


class SLURMQJob(QJob):
    jobid = typed_property("jobid", str)

    def __init__(self, jobid):
        super().__init__()
        self.jobid = jobid


class SLURMQueue(Queue):

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
        return ("sbatch", jobargs)

    def parse_submit(self, command_output, command_error, returncode):
        '''parse the output from the job-submission and return a QJob object'''
        if (returncode == 0):
            fields = command_output.split()
            jobid = fields[3]
            return SLURMQJob(jobid)
        else:
            print("sbatch failed with code {}: {}".format(returncode, command_error),
                  file=sys.stderr)
        return None


    def status_command(self, qJob):
        '''return the status command for the QJob'''
        assert(isinstance(qJob, SLURMQJob))
        return ("squeue", ['-j', "{}".format(qJob.jobid)])

    def delete_command(self, qJob):
        '''return the delete command for the QJob'''
        assert(isinstance(qJob, SLURMQJob))
        return ("scancel", ["{}".format(qJob.jobid)])


    def _parse_int(self, string):
        m = re.search(r'(\d+)', string)
        if m:
            return int(m.group(1))
        return 0

    def _pure_parse_status(self, qJob, status_output):
        lines = 0
        for s in status_output.splitlines():
            lines += 1
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
        if lines >= 1:
            return QJobStatus.finished
        return QJobStatus.unknown


    def parse_status(self, qJob, status_output, status_err, returncode):
        '''return the QJobStatus the QJob, except for testing for the status-file'''

        assert(isinstance(qJob, SLURMQJob))
        if returncode == 0:
            return self._pure_parse_status(qJob, status_output)
        else:
            return QJobStatus.unknown



class TestSLURMQueue(unittest.TestCase):

    def setUp(self):
        super().setUp()
        self.queue = SLURMQueue()
        self.jobid = "2839455"


    def test_parse_status(self):
        status_output = '''
             JOBID PARTITION     NAME     USER ST       TIME  NODES NODELIST(REASON)
           2839455     frost interact   cooper  R    1:17:46      1 n362
'''
        qJob = SLURMQJob(self.jobid)
        self.assertEqual(self.queue.parse_status(qJob, status_output, "", 0),
                         QJobStatus.running, "parsing output")

    def test_parse_submit(self):
        command_output = '''Submitted batch job 2839455
'''
        self.assertEqual(self.queue.parse_submit(command_output, "", 0).jobid,
                         self.jobid, "parsing squeue command")


if __name__ == "__main__":
    unittest.main()
