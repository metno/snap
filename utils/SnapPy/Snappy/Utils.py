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
Created on Oct 15, 2017

@author: heikok
'''

import datetime
import numbers
import sys
import warnings
import netCDF4
import os
import pyproj
import re
import tempfile
import unittest

import numpy


def _parseLLNumber(llstr):
    ''' parse a latitude or longitude string to a decimal returning (decimal, character)
    where character can should be NSEW or empty.
    Possible formats: -3.54, 3:5:3 S, 3° 5' 34"S

    Raises a ValueError if the format doesn't match
    '''

    if isinstance(llstr, numbers.Number):
        return (float(llstr), '')

    # remove + character coming from url-queries (space)
    llstr = llstr.replace('+','')
    llstr = llstr.strip()
    decimal = 0

    # fetch and remove last character (NSEW)
    character = ''
    if re.search(r'[A-Za-z]$', llstr):
        character = llstr[-1].upper()
        llstr = llstr[0:-1]
        llstr = llstr.strip()

    # just a number
    if re.search(r'^(-?\d+)$', llstr):
        decimal = float(llstr)
    elif re.search(r'^(-?\d+\.\d+)$', llstr):
        decimal = float(llstr)
    else:
        # degree:minutes(:second)
        m = re.search(r'^(-?\d+)\s*[:°]\s*(\d+)(\s*[\:\']\s*)?(\d+)?', llstr)
        if m:
            decimal = float(m.group(1)) + float(m.group(2))/60
            if m.group(4):
                decimal += float(m.group(4))/3600
        else:
            raise ValueError("unable to parse lon/lat number: '" + llstr + "'")
    return (decimal, character.upper())


def parseLat(latStr):
    ''' parse a latitude string to decimal degrees, raise an exception on error
    Possible formats: -3.54, 3:5:3 S, 3° 5' 34"S
    '''
    try:
        (decimal, northSouth) = _parseLLNumber(latStr)
    except TypeError as te:
        raise ValueError("cannot parse latitude: {}".format(te))

    if northSouth == 'S':
        decimal *= -1
    elif northSouth == "" or northSouth == 'N':
        pass
    else:
        raise ValueError("Not a latitude: "+latStr)
    if decimal < -90.0001 or decimal > 90.0001:
        raise ValueError("Not a latitude: "+latStr)
    return decimal

def parseLon(lonStr):
    ''' parse a longitude string to decimal degrees, raise an exception on error
    Possible formats: -3.54, 3:5:3 W, 3° 5' 34"W
    '''
    try:
        (decimal, eastWest) = _parseLLNumber(lonStr)
    except TypeError as te:
        raise ValueError("cannot parse longitude: {}".format(te))
    if eastWest == 'W':
        decimal *= -1
    elif eastWest == "" or eastWest == 'E':
        pass
    else:
        raise ValueError("Not a longitude: "+lonStr)
    if decimal < -180.0001 or decimal > 180.0001:
        raise ValueError("Not a longitude: "+lonStr)
    return decimal

class IsLatLonTests(unittest.TestCase):

    def testParseLat(self):
        self.assertAlmostEqual(parseLat(-3.54), -3.54, msg="parseLat(\"-3.54\")", delta=1e-3)
        self.assertAlmostEqual(parseLat("-3.54"), -3.54, msg="parseLat(\"-3.54\")", delta=1e-3)
        self.assertAlmostEqual(parseLat("3.54 S"), -3.54, msg="parseLat(\"-3.54\")", delta=1e-3)
        self.assertAlmostEqual(parseLat("3:5:3 S"), -3.0841, msg="parseLat(\"3:5:3 S\")", delta=1e-3)
        self.assertAlmostEqual(parseLat("3 °5' 3\" S"), -3.0841, msg="parseLat(\"3 °5' 3\" S\")", delta=1e-3)
        self.assertAlmostEqual(parseLat("60°5'5\"N"), 60.084722, msg="parseLat(\"60°5'5\"N\")", delta=1e-3)
        self.assertAlmostEqual(parseLat("8°20′2+″S+"), -8.333, msg="parseLat(\"8°20′27″S \")", delta=1e-3)
        self.assertRaises(ValueError, parseLat, "195")

    def testParseLon(self):
        self.assertAlmostEqual(parseLon(-3.54), -3.54, msg="parseLon(\"-3.54\")", delta=1e-3)
        self.assertAlmostEqual(parseLon("-3.54"), -3.54, msg="parseLon(\"-3.54\")", delta=1e-3)
        self.assertAlmostEqual(parseLon("3.54 W"), -3.54, msg="parseLon(\"-3.54\")", delta=1e-3)
        self.assertAlmostEqual(parseLon("3:5:3 W"), -3.0841, msg="parseLon(\"3:5:3 W\")", delta=1e-3)
        self.assertAlmostEqual(parseLon("10:5:5W"), -10.084722, msg="parseLon(\"10:5:5W\")", delta=1e-3)
        self.assertAlmostEqual(parseLon("10:5W"), -10.08333, msg="parseLon(\"10:5W\")", delta=1e-3)
        self.assertAlmostEqual(parseLon("3 °5' 3\" W"), -3.0841, msg="parseLon(\"3 °5' 3\" W\")", delta=1e-3)
        self.assertAlmostEqual(parseLon("10°4'W"), -10.06666, msg="parseLon(\"10°4'W\")", delta=1e-3)
        self.assertRaises(ValueError, parseLon, "370")


def dirIsWritable(directory):
    '''check if directory is writable'''
    if not directory:
        return False
    try:
        with tempfile.TemporaryFile(dir=directory) as fh:
            return True
    except:
        return False

def delete_oldfiles(dir_to_search, age_in_days):
    '''delete files older than age_in_days'''
    for dirpath, dirnames, filenames in os.walk(dir_to_search):
        for file in filenames:
            curpath = os.path.join(dirpath, file)
            try:
                file_modified = datetime.datetime.fromtimestamp(os.lstat(curpath).st_mtime)
                if datetime.datetime.now() - file_modified > datetime.timedelta(days=age_in_days):
                    os.remove(curpath)
            except FileNotFoundError:
                pass

def restrictDomainSizeAndResolution(file: str, lon: float, lat: float, gridSize: float, calcGridSize: int) -> str:
    retStr = ""
    with netCDF4.Dataset(file, 'r') as nc:
        if 'x_wind_ml' in nc.variables:
            var = nc['x_wind_ml']
        elif 'u_wind' in nc.variables:
            var = nc['u_wind']
        else:
            raise Exception('neither x_wind_ml nor u_wind in file: {file}')

        dims = var.dimensions
        # snap expects x-dim to be fastest moving dimension
        x_dim = dims[-1]
        y_dim = dims[-2]

        xvals = nc[x_dim][:]
        yvals = nc[y_dim][:]
        resolution = numpy.abs(nc[x_dim][1] - nc[x_dim][0])
        if gridSize != 0:
            if nc[x_dim].units.startswith("m"):
                resolution /= 1000 # gridSize given in deg or km, need deg or m
            supersampling = round(resolution/gridSize)
            if supersampling > 1:
                retStr += f"FIELD.OUTPUT_RESOLUTION_FACTOR= {supersampling}\n"
                calcGridSize = round(calcGridSize / supersampling) # need calcGridSize in original resolution

        if 'deg' in nc[x_dim].units:
            # latlon
            xpos = numpy.argmin(numpy.abs(xvals - lon))
            ypos = numpy.argmin(numpy.abs(yvals- lat))
            projstr = "+proj=latlon +R=6371000 +no_defs"
        else:
            # other projection (lcc) in m
            grid_mapping = nc[var.grid_mapping]
            if grid_mapping.grid_mapping_name == "lambert_conformal_conic":
                # projstr = f"+proj=lcc +lat_0={grid_mapping.latitude_of_projection_origin} +lon_0={grid_mapping.longitude_of_central_meridian} +lat_1={grid_mapping.standard_parallel[0]} +lat_2={grid_mapping.standard_parallel[1]} +x_0=0 +y_0=0 +R={grid_mapping.earth_radius} +units=m +no_defs +type=crs"
                proj = pyproj.CRS.from_cf(grid_mapping.__dict__)
                llproj = pyproj.CRS("EPSG:4326") #  pyproj.CRS.from_proj4(f"+proj=latlong +R={grid_mapping.earth_radius} +no_defs")
                with warnings.catch_warnings():
                    warnings.filterwarnings(
                        "ignore",
                        "You will likely lose important projection information when",
                        UserWarning,
                    )
                    projstr = proj.to_proj4()
            else:
                raise Exception(f"unimplemented grid_mapping_name: {grid_mapping.grid_mapping_name}")
            transformer = pyproj.Transformer.from_crs(llproj, proj, always_xy=True)
            xlonlat, ylonlat = transformer.transform(lon, lat)
            xpos = numpy.argmin(numpy.abs(xvals - xlonlat))
            ypos = numpy.argmin(numpy.abs(yvals - ylonlat))

        if calcGridSize > xvals.shape[0] or calcGridSize > yvals.shape[0]:
            calcGridSize = 0 # no change
        if calcGridSize > 0:
            calcGridSize_2 = calcGridSize//2
            xlast = xpos + calcGridSize_2 + 1
            if xlast >= xvals.shape[0]:
                xlast = xvals.shape[0] - 1
            xfirst = xpos - calcGridSize_2 - 1
            if xfirst < 0:
                xfirst = 0
            ylast = ypos + calcGridSize_2 + 1
            if ylast >= yvals.shape[0]:
                ylast = yvals.shape[0] - 1
            yfirst = ypos - calcGridSize_2 -1
            if yfirst < 0:
                yfirst = 0

            retStr += "FIELD.TYPE=fimex\n"
            retStr += "FIMEX.FILE_TYPE=netcdf\n"
            retStr += f"FIMEX.INTERPOLATION=nearest|{projstr}|{xvals[xfirst]},{xvals[xfirst+1]},...,{xvals[xlast]}|{yvals[yfirst]},{yvals[yfirst+1]},...,{yvals[ylast]}|degree\n"
    return retStr

class RestrictDomainSizeAndResolutionTests(unittest.TestCase):
    yesterday = datetime.datetime.today() - datetime.timedelta(days=1)
    latLonFile = "/lustre/storeB/project/metproduction/products/cwf-input/NRPA_EUROPE_0_1_00/meteo{year:04d}{month:02d}{day:02d}_00.nc".format(year=yesterday.year, month=yesterday.month, day=yesterday.day)
    lambertFile = "/lustre/storeB/immutable/archive/projects/metproduction/MEPS/{year:04d}/{month:02d}/{day:02d}/meps_det_2_5km_{year:04d}{month:02d}{day:02d}T00Z.nc".format(year=yesterday.year, month=yesterday.month, day=yesterday.day)

    def testLatLon(self):
        if not os.path.exists(self.latLonFile):
            self.skipTest(f"file not available: {self.latLonFile}")
        self.assertEqual(restrictDomainSizeAndResolution(self.latLonFile, 10, 60, 0, 0), "")

    def testLatLonDomainSize(self):
        if not os.path.exists(self.latLonFile):
            self.skipTest(f"file not available: {self.latLonFile}")
        self.assertTrue(restrictDomainSizeAndResolution(self.latLonFile, 10, 60, 0, 350).startswith("""FIELD.TYPE=fimex
FIMEX.FILE_TYPE=netcdf
FIMEX.INTERPOLATION=nearest|+proj=latlon +R=6371000 +no_defs|-7"""))

    def testLatLonSupersampling(self):
        if not os.path.exists(self.latLonFile):
            self.skipTest(f"file not available: {self.latLonFile}")
        #print(restrictDomainSizeAndResolution(self.latLonFile, 10, 60, 0.05, 350))
        self.assertTrue(restrictDomainSizeAndResolution(self.latLonFile, 10, 60, 0.05, 350).startswith("""FIELD.OUTPUT_RESOLUTION_FACTOR= 2
FIELD.TYPE=fimex
FIMEX.FILE_TYPE=netcdf
FIMEX.INTERPOLATION=nearest|+proj=latlon +R=6371000 +no_defs|1"""))


    def testLambert(self):
        if not os.path.exists(self.latLonFile):
            self.skipTest(f"file not available: {self.lambertFile}")
        self.assertEqual(restrictDomainSizeAndResolution(self.lambertFile, 10, 60, 0, 0), "")

    def testLambertDomainSize(self):
        if not os.path.exists(self.latLonFile):
            self.skipTest(f"file not available: {self.lambertFile}")
        self.assertTrue(restrictDomainSizeAndResolution(self.lambertFile, 10, 60, 0, 350).startswith("""FIELD.TYPE=fimex
FIMEX.FILE_TYPE=netcdf
FIMEX.INTERPOLATION=nearest|+proj=lcc +lat_0=63.3 +lon_0=15 +lat_1=63.3 +lat_2=63.3 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs|-7"""))

    def testLambertSupersampling(self):
        if not os.path.exists(self.latLonFile):
            self.skipTest(f"file not available: {self.lambertFile}")
        self.assertTrue(restrictDomainSizeAndResolution(self.lambertFile, 10, 60, 1.25, 350).startswith("""FIELD.OUTPUT_RESOLUTION_FACTOR= 2
FIELD.TYPE=fimex
FIMEX.FILE_TYPE=netcdf
FIMEX.INTERPOLATION=nearest|+proj=lcc +lat_0=63.3 +lon_0=15 +lat_1=63.3 +lat_2=63.3 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs|-4"""))


if __name__ == "__main__":
    unittest.main(verbosity=0)
