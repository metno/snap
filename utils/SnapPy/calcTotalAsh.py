#! /usr/bin/env python3

import math
import netCDF4
import numpy as np
import sys

if len(sys.argv) < 2:
    print(f"usage: {sys.argv[0]} ncfile_hour [ncfile_inst]", file=sys.stderr)
    sys.exit(1)

def cell_area(lat, lon):
    ''' cell_area in m^2'''
    area = np.zeros((lat.shape[0], lon.shape[0]), 'f8')
    latVals = np.radians(lat)
    lonVals = np.radians(lon)
    dlon = abs(lonVals[1]-lonVals[0])
    dlat = abs(latVals[1]-latVals[0])
  
    for i in range(len(latVals)):
        cl = math.cos(latVals[i])
        # formular: A = R^2 * cos(lat) * dLat * dLon
        area[i,:] = 6370*6370 * cl * dlat * dlon

    return area*1000*1000

rows = []
with netCDF4.Dataset(sys.argv[1], 'r') as nc:
    varnames = ('COLUMN_ASH_kmax', 'COLUMN_ASH')
    for var in varnames:
        if var in nc.variables:
            break
        else:
            var = None    
    lon = nc['lon'][:]
    lat = nc['lat'][:]
    area = cell_area(lat, lon)
    times = netCDF4.num2date(nc['time'][:], nc['time'].units)
    wDEP = 0
    dDEP = 0
    for i,t in enumerate(times):
        data = nc[var][i,:]
        current = np.sum(data*area)/1e12 # ug->Mg
        if 'WDEP_ASH' in nc.variables:
            wDEP = np.sum(nc['WDEP_ASH'][i,:]*area)/1e9 / 3600 # mg->Mg ; /h -> /s
            dDEP = np.sum(nc['DDEP_ASH_m2Grid'][i,:]*area)/1e9 / 3600 # mg->Mg ; /h -> /s
        rows.append({
            'current': current,
            'dDep': dDEP,
            'wDep': wDEP,
        })

if len(sys.argv) > 2:
    # instant if available
    with netCDF4.Dataset(sys.argv[2], 'r') as nc:
        varnames = ('COLUMN_ASH_kmax', 'COLUMN_ASH')
        for var in varnames:
            if var in nc.variables:
                break
            else:
                var = None    
            for i,t in enumerate(times):
                data = nc[var][i,:]
                current = np.sum(data*area)/1e12 # ug->Mg
                rows[i]['current'] = current

print('#i\tdate\ttotal_ash[Mg]\tair_rate\tddep_rate\twdep_rate\temis_rate[Mg/s]')
atmosChange = 0
for i, row in enumerate(rows):
    if i > 0:
        atmosChange = (row['current'] - rows[i-1]['current']) / 3600 # /h -> /s
    emis = atmosChange + row["dDep"] + row["wDep"]
    print(f'{i}\t{t}\t{row["current"]:.3f}\t{atmosChange:.3f}\t{row["dDep"]:.3f}\t{row["wDep"]:.3f}\t{emis:.3f}')
    

if False:
    import matplotlib.pyplot as plt
    import cartopy.crs as ccrs
    # plot area
    ax = plt.axes(projection=ccrs.PlateCarree())
    cf = plt.contourf(lon, lat, area/1000/1000, 60,
                transform=ccrs.PlateCarree())
    ax.coastlines()
    plt.colorbar(cf, orientation='horizontal')
    plt.show()
