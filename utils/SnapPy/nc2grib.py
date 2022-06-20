#! /usr/bin/env python3

import os
import subprocess
import sys
from Snappy.Resources import Resources, snapNc_convert_to_grib

if __name__ == "__main__":
    ncfile = sys.argv[1]
    ident = sys.argv[2]
    isotopes = [int(x) for x in sys.argv[3:]]
    dirname = os.path.dirname(ncfile)
    snapNc_convert_to_grib(ncfile, dirname, ident, isotopes)
