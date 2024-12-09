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
Created on Nov 7, 2016

@author: heikok
"""
from abc import ABCMeta, abstractmethod
from enum import Enum, unique
from subprocess import CalledProcessError, TimeoutExpired, SubprocessError
import sys


def typed_property(name, expected_type):
    """Helper function to create properties with types"""
    storage_name = "_" + name

    @property
    def prop(self):
        return getattr(self, storage_name)

    @prop.setter
    def prop(self, value):
        if (value is not None) and (not isinstance(value, expected_type)):
            raise TypeError(
                "{} must be a '{}', but is '{}'".format(
                    name, expected_type.__name__, value.__class__.__name__
                )
            )
        setattr(self, storage_name, value)

    return prop


class Connection(metaclass=ABCMeta):
    """Baseclass for connections to another machine."""

    def __init__(self):
        return

    @abstractmethod
    def put_files(self, files, remote_path, timeout=None):
        """Put files to a remote machine.

        Raises: TimeoutExpired, SubprocessError on failure
        """
        pass

    @abstractmethod
    def get_files(self, files, local_path=None, timeout=None):
        """Get files from a remote machine.

        Raises: TimeoutExpired, SubprocessError on failure
        """
        pass

    @abstractmethod
    def syscall(self, program, args, timeout=None):
        """Call a program remote, eventually setting a timeout.

        Returns: (stdout, stderr, retval) tuple, with retval being the returncode of the command.

        Raises: TimeoutExpired, SubprocessError
        """
        pass


class Queue(metaclass=ABCMeta):
    def __init__(self):
        return

    @abstractmethod
    def submit_command(self, jobscript, args):
        """return the submit command, e.g. qsub or sbatch

        Keyword arguments:
            jobscript -- the jobscript to submit
            args -- arguments to the jobscript

        Returns: the complete command as tuple (program, args), e.g. (qsub, [jobscript arg1 arg2 arg3]
        """
        pass

    @abstractmethod
    def parse_submit(self, command_output, command_error, returncode):
        """parse the output from the job-submission and return a QJob object

        Returns: QJob on success or None on failure
        """
        pass

    @abstractmethod
    def status_command(self, qJob):
        """return the status command for the QJob"""
        pass

    @abstractmethod
    def delete_command(self, qJob):
        """return the deletion command for the QJob"""
        pass

    @abstractmethod
    def parse_status(self, qJob, status_output, status_err, returncode):
        """return the QJobStatus the QJob, except for testing for the status-file"""
        pass


class HPC:
    """
    A HPC is a abstract base-class to send and retrieve data to a HPC machine
    or to submit and parse jobs on the HPC machines

    Typical usage::

        hpc = HPC.by_name("vilje")
        # vilje default is forecast, set your own username if desired,
        # or create your own HPC subclass
        hpc.connection.username = "mifahik"
        hpc.put_files([file1, file2], "/work/mydir")
        qjob = hpc.submit_job("/my/work/script.job", [])
        if qjob is not None:
            count = 10 # wait 10*60s for job to finish
            status = hpc.get_status(qjob)
            while not (status == QJobStatus.finished or status == QJobStatus.failed):
                sleep(60) # wait 60s
                status = hpc.get_status(qjob)
                count -= 1
                if (count == 0):
                    break

            if status == QJobStatus.finished or status == QJobStatus.failed:
                hpc.get_files([file1, file2], "/lustre/storeB")
            else:
                # clean up and give up
                hpc.delete_job(qJob)


    """

    connection = typed_property("connection", Connection)
    queue = typed_property("queue", Queue)

    def __init__(self, connection, queue):
        self.connection = connection
        self.queue = queue
        return

    def put_files(self, files, hpcpath, timeout=None):
        """send files to the HPC-machine

        Keyword arguments:
            files -- list of files
            hpcpath -- filename on hpc (relative or absolute)

        Raises: SubprocessError, e.g. TimeoutExpired if it took to long

        Returns: True if all files have been submitted
        """
        return self.connection.put_files(files, hpcpath, timeout)

    def get_files(self, files, localpath=None, timeout=None):
        """retrieve files from the HPC-machine

        Keyword arguments:
            files -- list of files to retrieve from hpc (relative path to home, or absolute path)
            localpath -- local directory to write the files to, might be


        Returns: True if all files have been retrieved

        Raises: SubprocessError, e.g. TimeoutExpired if it took to long
        """
        return self.connection.get_files(files, localpath, timeout)

    def submit_job(self, jobfile, args):
        """submit a job to the HPC machines queue

        Keyword arguments:
            jobfile -- hpc-filename to send to the HPC-queue
            args -- job-arguments

        Returns: a QJob object, None on failure
        """
        (command, args) = self.queue.submit_command(jobfile, args)
        (stdout, stderr, retval) = self.syscall(command, args, timeout=None)
        return self.queue.parse_submit(stdout, stderr, retval)

    def delete_job(self, qJob):
        """delete a submitted job"""
        (command, args) = self.queue.delete_command(qJob)
        self.syscall(command, args, timeout=None)

    def get_status(self, qjob, timeout=10):
        """Get the status of the job.

        Returns: a QJobStatus
            running -- the job is still in the queue and indicated running
            queued -- the job is in the queue and not indicated running
            finished -- the status_file is accepted or the job is no longer in the queue
            failed -- the job is not in the queue and the status_file is not accepted
            unknown -- not able to read the status currently (usually timeouts)
        """
        (command, args) = self.queue.status_command(qjob)
        try:
            (stdout, stderr, ret_val) = self.syscall(command, args, timeout=timeout)
            qstatus = self.queue.parse_status(qjob, stdout, stderr, ret_val)
        except SubprocessError as spe:
            print(spe, file=sys.stderr)
            return QJobStatus.unknown

        if qstatus == QJobStatus.finished and qjob.status_file is not None:
            try:
                (status_content, serr, retval) = self.syscall(
                    program="cat",
                    args=[qjob.status_file.status_filename],
                    timeout=timeout,
                )
                if retval == 0:
                    if qjob.status_file.finished:
                        if status_content.find(qjob.status_file.finished) != -1:
                            qstatus = QJobStatus.finished
                        else:
                            qstatus = QJobStatus.failed
                    else:
                        qstatus = QJobStatus.finished
                elif retval == 2:  # no such file or directory
                    qstatus = QJobStatus.failed
                else:
                    qstatus = QJobStatus.unknown
            except SubprocessError as spe:
                print(spe, file=sys.stderr)
                qstatus = QJobStatus.unknown
        return qstatus

    def syscall(self, program, args, timeout=None):
        """Call a program with arguments on the machine.

        Returns: the output of the program as tuple (stdout, stderr, ret_val), with ret_val being the return-code

        Raises: SubprocessError, e.g. TimeoutException if it took to long
        """
        return self.connection.syscall(program, args, timeout)

    @staticmethod
    def by_name(name):
        """
        Initialize a HPC by names, e.g. stratus, ppi_r8ucxB_direct
        """
        if name == "stratus":
            from . import Stratus

            return Stratus.Stratus()
        elif name == "ppi_r8b":
            from . import PPI_R8B

            return PPI_R8B.PPI_R8B()
        elif name == "ppi_r8b_direct":
            from . import PPI_R8_Direct

            return PPI_R8_Direct.PPI_R8_Direct()
        elif name == "ppi_r8ucxB_direct":
            from . import PPI_R8_Direct

            return PPI_R8_Direct.PPI_R8_Direct()
        elif name == "ppi_r8ucxA_direct":
            from . import PPI_R8_Direct

            return PPI_R8_Direct.PPI_R8_Direct()
        elif name == "ppi_r8ibA_direct":
            from . import PPI_R8_Direct

            return PPI_R8_Direct.PPI_R8_Direct()
        else:
            raise NotImplementedError("no HPC named '{}'".format(name))
        return


class StatusFile:
    """Object to encapsulate the filename and status-strings in a status-file"""

    status_filename = typed_property("status_filename", str)
    finished = typed_property("finished", str)

    def __init__(self, status_filename, finished=""):
        """Constructor

        Keyword arguments:
            status_filename -- filename to a statusfile
            finished -- optional text the status_file needs to contain if really finished
        """
        self.status_filename = status_filename
        self.finished = finished
        return


@unique
class QJobStatus(Enum):
    """The job status. A status <= 0 means to continue to wait."""

    unknown = -2
    queued = -1
    running = 0
    finished = 1
    failed = 2


class QJob(metaclass=ABCMeta):
    """Abstract baseclass of jobs submitted to a queue. Besides checking the queue, the job may
    also be controlled by a status_file which needs to be set manually with the status_file property"""

    status_file = typed_property("status_file", StatusFile)

    def __init__(self):
        """Constructor"""
        self.status_file = None
        return
