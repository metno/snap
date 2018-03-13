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
import os
import re
import tempfile
import unittest


def _parseLLNumber(llstr):
    ''' parse a latitude or longitude string to a decimal returning (decimal, character)
    where character can should be NSEW or empty.
    Possible formats: -3.54, 3:5:3 S, 3° 5' 34"S

    Raises a ValueError if the format doesn't match
    '''

    if isinstance(llstr, numbers.Number):
        return (float(llstr), '')

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





if __name__ == '__main__':
    unittest.main()