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
