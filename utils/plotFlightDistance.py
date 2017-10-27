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
import re
import matplotlib.pyplot as plt
from cycler import cycler
#from scipy import interpolate
from numpy import linspace

def readData(filename):
    #name   km      avg [Bq/m2]     max [Bq/m2]     [km2]
    data = {}
    with open(filename, 'rt') as f:
        for line in f:
            if line[0] == '#':
                continue
            (component,dist,avg,max,area) = re.split(r'\t', line)
            data[float(dist)] = float(max)
    return data


files = []
# density in g/cm3
for den in (3.3, 8.1, 11.6, 19):
    # radius sizes um
    for um in (1, 2.5, 5, 10, 20, 30, 40, 50):
        for drywet in ('dry','wet'):
            files.append({"filename": "run_{}_{}/flightDist_{}.csv".format(um,den,drywet),
                          "diameter": 2*um,
                          "density": den,
                          "type": drywet
                          })

odata = {}
for fd in files:
    if not fd["type"] in odata.keys():
        odata[fd["type"]] = {}
    if not fd["density"] in odata[fd["type"]].keys():
        odata[fd["type"]][fd["density"]] = {}
    odata[fd["type"]][fd["density"]][fd["diameter"]] = readData(fd["filename"])

#plt.rc('axes', prop_cycle=(cycler('color', ['r', 'g', 'b', 'y']) +
#                           cycler('linestyle', ['-', '--', ':', '-.'])))
colors = [ plt.cm.inferno(x) for x in linspace(0, .95, 8) ]

for drywet in sorted(odata.keys()):
    fig, axi = plt.subplots(2,2,sharex=True, sharey=True, figsize=(12, 10))
    for i, dens in enumerate(sorted(odata[drywet].keys())):
        ax = axi[int(i/2), i%2]
        ax.set_prop_cycle(cycler('color', colors))
        ax.set_yscale('log')
        ax.set_title("density {:.1f} g/cm3".format(dens))
        ax.set_ylabel("max. {} deposition [Bq/m2]".format(drywet))
        ax.set_xlabel("distance from source [km]")
        legend = []
        for diam in sorted(odata[drywet][dens].keys()):
            xyList = sorted(odata[drywet][dens][diam].items())
            x, y = zip(*xyList)
            ax.plot(x,y, lw=2)
            legend.append("{} µm".format(diam))
            #splines
            if False:
                ax2 = ax.twinx()
                tck,u = interpolate.splprep([x, y], s=0)
                unew = np.arange(0, 1.01, 0.01)
                out = interpolate.splev(unew, tck)
                ax.plot(x, y, 'orange', out[0], out[1])
        ax.set_ylim(ymin=10)
        ax.set_xlim(xmax=1800)
        ax.legend(legend)
    fig.savefig("flightDistance_{}.png".format(drywet))
    #plt.show()
