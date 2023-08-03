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
Created on Aug 08, 2017

@author: heikok
'''

import os
import shutil
import subprocess
import sys
import unittest

from METNO.HPC import typed_property, Connection


class DirectConnection(Connection):
    '''no connection, working directly on that machine

    '''
    charset = typed_property("charset", str)
    '''charset of stdout of the machine, usually utf-8'''

    def __init__(self):
        super().__init__()
        self.charset = "utf-8"
        return

    def put_files(self, files, remote_path, timeout=None):
        for f in files:
            shutil.copy2(f, remote_path)
        return True

    def get_files(self, files, local_path=None, timeout=None):
        if not local_path:
            local_path = "."
        for f in files:
            shutil.copy2(f, local_path)
        return True

    def syscall(self, program, args, timeout=None):
        if sys.version_info > (3, 5, 0):
            proc = subprocess.run([program]+ args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=timeout)
            return (proc.stdout.decode(self.charset),
                    proc.stderr.decode(self.charset),
                    proc.returncode)
        else:
            try:
                output = subprocess.check_output([program] + args, timeout=timeout)
                return (output.decode(self.charset),
                        '',
                        0)
            except subprocess.CalledProcessError as cpe:
                return (cpe.output.decode(self.charset),
                        '',
                        cpe.returncode)



class TestDirectConnection(unittest.TestCase):
    '''Test for DirectConnection'''
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
        self.conn = DirectConnection()



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
