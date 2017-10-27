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
import re


def _parseLLNumber(llstr):
    ''' parse a latitude or longitude string to a desimal returning (desimal, character)
    where character can should be NSEW or empty.
    Possible formats: -3.54, 3:5:3 S, 3° 5' 34"S

    Raises a ValueError if the format doesn't match
    '''

    llstr = llstr.strip()
    desimal = 0
    character = 'N'
    if re.search(r'[A-Za-z]$', llstr):
        character = llstr[-1].upper()
        llstr = llstr[0:-2]
        llstr = llstr.strip()
    
    # just a numper
    if re.match(r'^(-?\d+)', llstr):
        desimal = float(llstr)
    elif m = re.match(r'^(-?\d+\.\d+)'):
        desimal = float(llstr)
    elif m = re.match(r'^(-?\d+)\s*:\s*(\d+)\s*(\:\s*\d+)?'):
        desimal
    return (desimal, character)


def parseLat(latStr):
    ''' parse a latitude string to desimal degrees, raise an exception on error
    Possible formats: -3.54, 3:5:3 S, 3° 5' 34"S
    '''
    (desimal, northSouth) = _parseLLNumber(latStr)
    if northSouth == 'S':
        desimal *= -1
    elif northSouth == "" or northSouth == 'N':
        pass
    else:
        raise ValueError("Not a latitude: "+str)
    if desimal < -90.0001 or desimal > 90.0001:
        raise ValueError("Not a latitude: "+str)
    return desimal


if __name__ == '__main__':
    pass
