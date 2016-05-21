'''
Created on Apr 13, 2016

@author: heikok
'''

import re
import os
from time import gmtime, strftime

class Resources():
    '''
    Read the resources and combine them
    '''
    OUTPUTDIR = "/disk1/tmp"
    #OUTPUTDIR = "/lustre/storeB/project/fou/kl/snap/runs"

    def __init__(self):
        '''
        initialize
        '''
        startScreenFH = open(os.path.join(os.path.dirname(__file__),"resources/startScreen.html"),
                             mode='r', encoding="UTF-8")
        self.startScreen = startScreenFH.read()
        startScreenFH.close()
        plantBB = {'west': -60,'east': 70,'north': 90,'south': 30}
        npps = self.readNPPs(plantBB)
        nppStrings = []
        for tag, site in npps.items():
            nppStrings.append('<option value="{tag}">{site}</options>\n'.format(tag=tag, site=site['site']))
        self.startScreen = re.sub(r'%NPP_OPTIONS%',"".join(nppStrings),self.startScreen)
        self.startScreen = re.sub(r'%CURRENTTIME%',strftime("%Y-%m-%d %H:00", gmtime()),self.startScreen)

    def getStartScreen(self):
        return self.startScreen

    def getIconPath(self):
        return os.path.join(os.path.dirname(__file__),"resources/radioMapIcon.png")

    def readNPPs(self, bb={'west': -180., 'east': 180., 'north': 90., 'south': -90.}):
        nppsFile = open(os.path.join(os.path.dirname(__file__),"resources/npps.csv"),
                        mode='r', encoding="UTF-8")
        # skip header
        nppsFile.readline()
        # read rest
        npps = {}
        for line in nppsFile:
            # [site, country, long, lat, status)
            site = line.split ('|')
            if len(site) < 4:
                print("NPP not properly defined: ", line, file=sys.stderr)
            tag = site[0]
            tag = tag.replace(' ', '_')
            if ((float(site[2]) >= bb['west']) and (float(site[2]) <= bb['east']) and
                (float(site[3]) >= bb['south']) and (float(site[3]) <= bb['north'])):
                npps[tag] = {'site': site[0], 'CC': site[1], 'lon': float(site[2]), 'lat': float(site[3]), 'status': site[4]}
        nppsFile.close()
        return npps

    def getSnapInputTemplate(self):
        filename = os.path.join(os.path.dirname(__file__),"resources/snap.input.tmpl")
        f = open(filename)
        return f.read()

    def getSnapOutputDir(self):
        return self.OUTPUTDIR


if __name__ == "__main__":
    print(Resources().getStartScreen())
