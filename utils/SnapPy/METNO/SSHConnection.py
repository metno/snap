'''
Created on Nov 15, 2016

@author: heikok
'''

import os
import unittest
import subprocess

from METNO.HPC import typed_property, Connection


class SSHConnection(Connection):
    '''connection via ssh

    besides the main options username, machine and port, the user can set special attributes

    '''

    username = typed_property("username", str)
    '''name of the user on the remote machine, None possible'''
    machine = typed_property("machine", str)
    '''name or IP-address of the remote machine'''
    remote_charset = typed_property("remote_charset", str)
    '''charset of stdout of the remote machine, usually utf-8'''
    port = typed_property("port", int)
    '''port to connect on the remote machine, None possible'''
    ssh_command = typed_property("ssh_command", str)
    '''command to use for ssh-connections, usually just 'ssh' for the ssh command in the PATH'''
    scp_command = typed_property("scp_command", str)
    '''command to use for scp-connections, usually just 'scp' for the scp command in the PATH'''
    ssh_options = typed_property("ssh_options", list)
    '''additional options to add to ssh'''
    scp_options = typed_property("scp_options", list)
    '''additional options to add to scp'''


    def __init__(self, username=None, machine="localhost", port=None):
        super().__init__()
        self.username = username
        self.machine = machine
        self.remote_charset = "utf-8"
        self.port = port
        self.ssh_command = "ssh"
        self.scp_command = "scp"
        self.scp_options = ["-o", "ConnectTimeout=20", "-o", "Batchmode=yes",
                            "-o", "StrictHostKeyChecking=no",
                            "-q", "-p"]
        self.ssh_options = ["-o", "ConnectTimeout=20", "-o", "Batchmode=yes",
                            "-o", "StrictHostKeyChecking=no"]
        return

    def _build_scp_args(self):
        args = [self.scp_command]
        args.extend(self.scp_options)
        if self.port is not None:
            args.extend(["-P", "{}".format(self.port)])
        return args

    def _build_ssh_args(self):
        args = [self.ssh_command]
        args.extend(self.ssh_options)
        if self.port is not None:
            args.extend(["-p", "{}".format(self.port)])
        if self.username is not None:
            args.extend(["-l", self.username])
        args.append(self.machine)
        return args


    def put_files(self, files, remote_path, timeout=None):
        args = self._build_scp_args()
        args.extend(files)
        user = ""
        if self.username is not None:
            user = self.username + '@'
        args.append("{user}{machine}:{path}".format(user=user,
                                                    machine=self.machine,
                                                    path=remote_path))

        proc = subprocess.run(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=timeout)
        proc.check_returncode()
        return True

    def get_files(self, files, local_path=None, timeout=None):
        args = self._build_scp_args()
        user = ""
        if self.username is not None:
            user = self.username + '@'
        for file in files:
            args.append("{user}{machine}:{path}".format(user=user,
                                                        machine=self.machine,
                                                        path=file))
        if local_path is None:
            local_path = "."
        args.append(local_path)

        proc = subprocess.run(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=timeout)
        proc.check_returncode()
        return True

    def syscall(self, program, args, timeout=None):
        ssh_args = self._build_ssh_args()
        args.insert(0, program)
        # print(args)
        remote_command = " ".join(args)
        ssh_args.append(remote_command)

        proc = subprocess.run(ssh_args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=timeout)
        return (proc.stdout.decode(self.remote_charset),
                proc.stderr.decode(self.remote_charset),
                proc.returncode)


class TestSSHConnection(unittest.TestCase):
    '''Test for SSHConnection'''
    def setUp(self):
        unittest.TestCase.setUp(self)
        self.dir1 = os.path.join(os.path.dirname(__file__), "testdir1")
        if not os.path.exists(self.dir1):
            os.mkdir(self.dir1)
        self.dir2 = os.path.join(os.path.dirname(__file__), "testdir2")
        if not os.path.exists(self.dir2):
            os.mkdir(self.dir2)

        self.files = ['file1', 'file2']
        for file in self.files:
            infile = os.path.join(self.dir1, file)
            if not os.path.exists(infile):
                with open(infile, 'w') as ifh:
                    ifh.write("file: {name}".format(name=infile))
            outfile = os.path.join(self.dir2, file)
            if os.path.exists(outfile):
                os.unlink(outfile)
        self.conn = SSHConnection(machine="localhost")



    def tearDown(self):
        unittest.TestCase.tearDown(self)
        for path in (self.dir1, self.dir2):
            for file in self.files:
                pfile = os.path.join(path, file)
                if os.path.exists(pfile):
                    os.unlink(pfile)
            if os.path.exists(path):
                os.rmdir(path)

    def test_syscall(self):
        (out, err, retval) = self.conn.syscall("ls", [self.dir1], 60)
        self.assertTrue(retval == 0)
        self.assertTrue(err == "")
        self.assertTrue(out.find(self.files[0]) != -1)
        self.assertTrue(out.find(self.files[1]) != -1)

    def test_get_files(self):
        files_i = [os.path.join(self.dir1, x) for x in self.files]
        self.assertTrue(len(files_i) == 2)
        files_o = [os.path.join(self.dir2, x) for x in self.files]
        self.conn.get_files(files_i, self.dir2, 5)
        for file in files_o:
            self.assertTrue(os.path.exists(file),
                            "file {} exists".format(file))
            os.unlink(file)

    def test_put_files(self):
        files_i = [os.path.join(self.dir1, x) for x in self.files]
        self.assertTrue(len(files_i) == 2)
        files_o = [os.path.join(self.dir2, x) for x in self.files]
        self.conn.put_files(files_i, self.dir2, 5)
        for x in files_o:
            self.assertTrue(os.path.exists(x), "file {} exists".format(x))
            os.unlink(x)


if __name__ == "__main__":
    unittest.main()
