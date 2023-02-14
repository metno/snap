#! /usr/bin/env python3

import argparse
import os
import sys
import netCDF4
from Snappy.Resources import Resources, snapNc_convert_to_grib

def getIsotopesFromFile(filename):
    isotop_names = []
    with netCDF4.Dataset(ncfile, 'r') as nc:
        for var in nc.variables:
            if var.endswith('acc_concentration'):
                continue
            if var.endswith('_concentration'):
                isotop_names.append(var[:-14])
    isotopes = []
    used_isotop_names = []
    for isoId, iso in Resources().getIsotopes().items():
        if iso['isotope'] in isotop_names:
            isotopes.append(isoId)
            used_isotop_names.append(iso['isotope'])
    print(f"converting isotopes: {', '.join(used_isotop_names)}", file=sys.stderr)
    dropped_names = [name for name in isotop_names if name not in used_isotop_names]
    print(f"dropping unknown isotopes: {', '.join(dropped_names)}", file=sys.stderr)
    return isotopes

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="convert a snap.nc output-file to grib, should be run after snapAddToa")
    parser.add_argument("--nc", help="snap.nc filename", required=True)
    parser.add_argument("--ident", help="output-file identifier", required=True)
    parser.add_argument("--bitmapCompress", help="enable grib bitmap-compression", action='store_true')
    
    args = parser.parse_args()

    ncfile = args.nc
    isotopes = getIsotopesFromFile(ncfile)
    ident = args.ident
    bitmapCompress = False
    if args.bitmapCompress:
        bitmapCompress= True
    dirname = os.path.dirname(ncfile)
    snapNc_convert_to_grib(ncfile, dirname, ident, isotopes, bitmapCompress=bitmapCompress)
