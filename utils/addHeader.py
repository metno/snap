#! /usr/bin/env python3
#
# SNAP: Servere Nuclear Accident Programme
# Copyright (C) 1992-2020   Norwegian Meteorological Institute
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

import argparse
import os
import sys


def insert_header(oh, ct):
    header = '''SNAP: Servere Nuclear Accident Programme
Copyright (C) 1992-2017   Norwegian Meteorological Institute

This file is part of SNAP. SNAP is free software: you can
redistribute it and/or modify it under the terms of the
GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.'''

    for line in header.splitlines():
        oh.write(ct + " " + line + "\n")
    oh.write(ct + "\n")



parser = argparse.ArgumentParser()
parser.add_argument("file", help="snap.nc file to be changed")
parser.add_argument("--type", help="file-type (python/perl/fortran)", required=True)
args = parser.parse_args()

types = {'python': '#',
         'perl': '#',
         'fortran': '!'}

if not args.type in types:
    print("unknown type: {}".format(args.type), file=sys.stderr)
    sys.exit(1)


with open(args.file, 'rt') as ih:
    with open(args.file + ".header", 'wt') as oh:
        line = ih.readline()
        if line.startswith('#!'):
            oh.write(line)
            oh.write(types[args.type] + "\n")
            insert_header(oh, types[args.type])
        else:
            insert_header(oh, types[args.type])
            oh.write(line)

        other = ih.read()
        oh.write(other)
os.rename(args.file + ".header", args.file)
