#! /usr/bin/env python3

import argparse
import warnings
import cartopy
import datetime
import matplotlib
from pyproj import Proj
matplotlib.use('Agg')
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
from matplotlib import pyplot as plt
import netCDF4
import os
import sys

import numpy as np
import logging

# suppress some warnings
warnings.filterwarnings("ignore", category=UserWarning,
                        message="Warning: 'partition' will ignore the 'mask' of the MaskedArray.")
# shapefile.py uses root logger :-( and warns a lot about GSHHS
logging.root.setLevel(logging.ERROR)



def plotMap(data, x, y, ax, title="", title_loc="center", clevs=[10,100,300,1000,3000,10000,30000,100000, 300000, 10000000], colors=None, extend='max'):
    ax.add_feature(cartopy.feature.GSHHSFeature(scale='low', facecolor='none', edgecolor='whitesmoke', linewidth=.2), zorder=100)
    ax.add_feature(cartopy.feature.BORDERS, edgecolor="lightgray", linewidth=.5, zorder=100) 
    #ax.gridlines(draw_labels=True)
    ax.gridlines(edgecolor="lightgray", linewidth=.3, zorder=100)

    ny = data.shape[0]
    nx = data.shape[1]
    # draw filled contours.
    if colors is None:
        colors = [ plt.cm.hsv(x) for x in np.linspace(0.5, 0, len(clevs)) ]
    cs = ax.contourf(x,y,data,clevs,colors=colors, extend=extend, zorder=50)
    # add title
    ax.set_title(title, loc=title_loc)
    ax.add_feature(cartopy.feature.OCEAN,facecolor="#aecfe0", edgecolor='none', zorder=10) # #aecfe0 = osm-sea
    ax.add_feature(cartopy.feature.LAND, facecolor="#f2efe9", edgecolor='none', zorder=10) # f2efe9 = osm-land
    ax.add_feature(cartopy.feature.LAKES,facecolor="#aecfe0", edgecolor='whitesmoke', linewidth=.2, zorder=20)
    return cs


def plotMapGrid(data, x, y, ax, title="", title_loc="center", clevs=[10,100,300,1000,3000,10000,30000,100000, 300000, 10000000], colors=None):
    ax.add_feature(cartopy.feature.GSHHSFeature(scale='auto', facecolor='none', edgecolor='black'))
    ax.gridlines()
    ax.add_feature(cartopy.feature.BORDERS)
    ny = data.shape[0]
    nx = data.shape[1]
    # draw filled contours.
    if colors is None:
        colors = [ plt.cm.hsv(x) for x in np.linspace(0.5, 0, len(clevs)) ]
    cmap = plt.get_cmap('tab10')
    norm = matplotlib.colors.BoundaryNorm(clevs, ncolors=cmap.N, clip=True)

    cs = ax.pcolormesh(x,y,data,norm=norm, cmap=cmap)
    # add title
    ax.set_title(title, loc=title_loc)
    return cs


def rgbToColor(rgb):
    color = [int(x)/255 for x in rgb.split(':')]
    if len(color) != 4:
        color.append(1.)
    return color


def snapens(ncfiles, hour, outfile):
    title = ""
    pos = -1
    steps = -1
    startDT = None
    endDT = None
    toa = []
    deps = []
    for ncf in ncfiles:
        with netCDF4.Dataset(ncf, 'r') as nc:
            if not title:
                title = nc.title
            if not startDT:
                # estimate starttime
                times = netCDF4.num2date(nc["time"][:], nc["time"].units)
                steps = len(times)
                step = times[1] - times[0]
                startDT = times[0] - step
                stepH = step.seconds // 3600 + step.days * 24
                if (hour % stepH) == 0:
                    pos = hour//stepH - 1
                    endDT = times[pos]
                else:
                    print(f"cannot devide {hour} forecast_hour by {stepH}h timesteps", sys.stderr)
                    sys.exit(1)
            if not "time_of_arrival" in nc.variables:
                print(f"time_of_arrival not in {ncf}, please run 'snapAddToa {ncf}", file=sys.stderr)
                sys.exit(2)
            toa.append(nc["time_of_arrival"][0,:])
            fillvalue = nc["time_of_arrival"]._FillValue
            deps.append(nc["Cs137_acc_total_deposition"][pos,:])
            lons = nc["longitude"][:]
            lats = nc["latitude"][:]
            x = nc["x"][:]
            y = nc["y"][:]
    toa = np.stack(toa)
    toa = toa.filled(fill_value=fillvalue)
    toa[toa == fillvalue] = (steps+1)*stepH
    toaPerc = np.percentile(toa, [10,50,90], axis=0)
    deps = np.stack(deps)
    depsPerc = np.percentile(deps, [10,50,90], axis=0)
    depTH = []
    for th in [1e3, 1e4, 1e5]:
        depTH.append(np.sum(deps > th, axis=0)*100/deps.shape[0])

    # and the plot
    formatter = matplotlib.ticker.ScalarFormatter()
    formatter.set_powerlimits((-3,10))

    fig = plt.figure(figsize=(12, 10.7))
    fig.suptitle(f'{title}+{hour}hours ({endDT:%Y-%m-%d %H}:00Z). Uncertainty based upon {deps.shape[0]} members.',
        y=0.92)
    proj = cartopy.crs.PlateCarree()
    with netCDF4.Dataset(ncfiles[0], 'r') as nc:
        if "grid_mapping" in nc["Cs137_acc_total_deposition"].ncattrs():
            grid_attrs = {}
            for attr in nc[nc["Cs137_acc_total_deposition"].grid_mapping].ncattrs():
                grid_attrs[attr] = nc[nc["Cs137_acc_total_deposition"].grid_mapping].getncattr(attr)
            # print(grid_attrs)
            if grid_attrs['grid_mapping_name'] == 'lambert_conformal_conic':
                proj = cartopy.crs.LambertConformal(central_longitude=grid_attrs['longitude_of_central_meridian'],
                    central_latitude=grid_attrs['latitude_of_projection_origin'],
                    standard_parallels=[grid_attrs['standard_parallel']],
                    globe=cartopy.crs.Globe(semiminor_axis=6371000,
                                            semimajor_axis=6371000))
            data_x = x
            data_y = y
        else:
            print(f"unknown grid_mapping_name {grid_attrs}, trying latlon/PlateCaree", file=sys.stderr)
            data_x = lons
            data_y = lats

    colors=('g', 'y', 'tab:orange', 'r', 'tab:red')
    ax = fig.add_subplot(3,3, 1, projection=proj)
    ax.set_extent([x[50],x[-50],y[25],y[-25]], crs=proj)
    cs = plotMap(depsPerc[0,:]/1000,
            title="Cs-137 deps: 10 perc.",
            title_loc="left",
            ax=ax,
            colors=colors,
            clevs=[0.1,1, 10, 100, 1000],
            x=data_x, y=data_y)
    ax = fig.add_subplot(3,3, 2, projection=proj)
    ax.set_extent([x[50],x[-50],y[25],y[-25]], crs=proj)
    cs = plotMap(depsPerc[1,:]/1000,
            title="median",
            ax=ax,
            colors=colors,
            clevs=[0.1,1, 10, 100, 1000],
            x=data_x, y=data_y)
    ax = fig.add_subplot(3,3, 3, projection=proj)
    ax.set_extent([x[50],x[-50],y[25],y[-25]], crs=proj)
    cs = plotMap(depsPerc[2,:]/1000,
            title="90 percentile",
            ax=ax,
            colors=colors,
            clevs=[0.1,1, 10, 100, 1000],
            x=data_x, y=data_y)
    axins = inset_axes(ax,
            width="3%",
            height="95%",
            loc='lower right',
            bbox_to_anchor=(0., 0., 1.05, 1),
            bbox_transform=ax.transAxes,
            borderpad=0,
        )
    cbar = fig.colorbar(cs, cax=axins, format=formatter, orientation='vertical')
    cbar.set_label('kBq/m²')

    ax = fig.add_subplot(3,3, 4, projection=proj)
    ax.set_extent([x[50],x[-50],y[25],y[-25]], crs=proj)
    colors = [ plt.cm.Paired(x) for x in [1,10,7,5,9] ]
    cs = plotMap(depTH[0],
            title="Probability: dep. > 1kBq/m2",
            title_loc="left",
            ax=ax,
            colors=colors,
            clevs=[10,30,50,70,90],
            x=data_x, y=data_y)
    ax = fig.add_subplot(3,3, 5, projection=proj)
    ax.set_extent([x[50],x[-50],y[25],y[-25]], crs=proj)
    cs = plotMap(depTH[1],
            title="depo. > 10kBq/m2",
            ax=ax,
            colors=colors,
            clevs=[10,30,50,70,90],
            x=data_x, y=data_y)
    ax = fig.add_subplot(3,3, 6, projection=proj)
    ax.set_extent([x[50],x[-50],y[25],y[-25]], crs=proj)
    cs = plotMap(depTH[2],
            title="depo. > 100kBq/m2",
            ax=ax,
            colors=colors,
            clevs=[10,30,50,70,90],
            x=data_x, y=data_y)
    axins = inset_axes(ax,
            width="3%",
            height="95%",
            loc='lower right',
            bbox_to_anchor=(0., 0., 1.05, 1),
            bbox_transform=ax.transAxes,
            borderpad=0,
        )
    cbar = fig.colorbar(cs, cax=axins, format=formatter, orientation='vertical')
    cbar.set_label('%')


    # Time of arrival
    ax = fig.add_subplot(3,3, 7, projection=proj)
    ax.set_extent([x[50],x[-50],y[25],y[-25]], crs=proj)
    clevs=range(0,(steps+1)*stepH,2*stepH)
    colors = [ plt.cm.jet(x) for x in np.linspace(0.95,0.1,len(clevs)) ]
    # dsa colors up to 48 hours
    colors[0] = rgbToColor('128:0:255')
    colors[1] = rgbToColor('128:128:255')
    colors[2] = rgbToColor('128:128:192')
    colors[3] = rgbToColor('192:192:192')
    cs = plotMap(toaPerc[2,:],
            title="Time of arrival: 90 perc.",
            title_loc="left",
            ax=ax,
            colors=colors,
            clevs=clevs,
            extend=None,
            x=data_x, y=data_y)
    ax = fig.add_subplot(3,3, 8, projection=proj)
    ax.set_extent([x[50],x[-50],y[25],y[-25]], crs=proj)
    cs = plotMap(toaPerc[1,:],
            title="median",
            ax=ax,
            colors=colors,
            clevs=clevs,
            extend=None,
            x=data_x, y=data_y)
    ax = fig.add_subplot(3,3, 9, projection=proj)
    ax.set_extent([x[50],x[-50],y[25],y[-25]], crs=proj)
    cs = plotMap(toaPerc[0,:],
            title="10 percentile",
            ax=ax,
            colors=colors,
            clevs=clevs,
            extend=None,
            x=data_x, y=data_y)
    axins = inset_axes(ax,
            width="3%",
            height="95%",
            loc='lower right',
            bbox_to_anchor=(0., 0., 1.05, 1),
            bbox_transform=ax.transAxes,
            borderpad=0,
        )
    cbar = fig.colorbar(cs, cax=axins, format=formatter, orientation='vertical')
    cbar.set_label('hours')



    fig.subplots_adjust(hspace=0.12, wspace=0.01)
    fig.savefig(outfile, bbox_inches='tight')


def main():
    os.umask(0o002)

    parser = argparse.ArgumentParser(
        description="Read snap files from identical ensemble runs and create a plot with statistical analysis for a certain forecast-hour",
        usage=f"{sys.argv[0]} --hour FORECAST_HOUR snap_01.nc [snap_02.nc ... snap_xx.nc]"
    )
    parser.add_argument("--out", help="output png file", required=True)
    parser.add_argument("--hour", help="hour of output to analyse", type=int, required=True)
    #parser.add_argument("--store", help="storeA or storeB, meteo and runtime-datastore, default used from MAPP-system")
    parser.add_argument('SNAPNC', help="snap*.nc filenames", nargs='+')
    args = parser.parse_args()

    snapens(args.SNAPNC, args.hour, args.out)


if __name__ == "main":
    main()
