#! /usr/bin/env python3

import argparse
import warnings

import cartopy
import matplotlib
from pyproj import Proj

matplotlib.use('Agg')
import os
import sys

import netCDF4
import numpy as np
from matplotlib import pyplot as plt
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
from scipy.ndimage import gaussian_filter

# suppress some warnings
warnings.filterwarnings("ignore", category=UserWarning,
                        message="Warning: 'partition' will ignore the 'mask' of the MaskedArray.")
# shapefile.py uses root logger :-( and warns a lot about GSHHS
import logging

logging.root.setLevel(logging.ERROR)


def plotBasicMap(ax,title="", title_loc="center"):
    ax.set_facecolor("#f2efe9") # background, osm-land color
    ax.add_feature(cartopy.feature.NaturalEarthFeature(
            category='physical',
            name='ocean',
            scale='50m',
            facecolor="#aecfe0"),edgecolor="none", zorder=10) # osm-ocean color
    ax.add_feature(cartopy.feature.NaturalEarthFeature(
        category='physical',
        name='lakes',
        scale='10m',
        facecolor='#aecfe0'),edgecolor="whitesmoke", linewidth=.2, zorder=20)
    ax.add_feature(cartopy.feature.NaturalEarthFeature(
        category='cultural',
        name='admin_0_countries_deu',
        scale='10m',
        facecolor='none'),edgecolor="lightgray", linewidth=.5, zorder=100)

    ax.gridlines(edgecolor="lightgray", linewidth=.3, zorder=100)

    # add title
    ax.set_title(title, loc=title_loc)
    return

def plotMap(data, x, y, ax, title="", title_loc="center", clevs=[10,100,300,1000,3000,10000,30000,100000, 300000, 10000000], colors=None, extend='max'):
    """Plot multiple data on a map as contours with legends

    :param data: a list of data
    :param x: x dimension, common for all data
    :param y: y dimension, common for all data
    :param ax: plot axis, common for all data
    :param title: title of the plot, defaults to ""
    :param title_loc: location of the title, defaults to "center"
    :param clevs: levels where to put iso-lines, common for all plots
    :param colors: colors, should be a list (len(data)) of lists (len(levels))
    :param extend: extend parameter for countourf
    :return: contourf-object
    """
    plotBasicMap(ax=ax, title=title, title_loc=title_loc)

    # draw filled contours.
    if colors is None:
        colors = [ plt.cm.hsv(x) for x in np.linspace(0.5, 0, len(clevs)) ]
    cs = ax.contourf(x,y,gaussian_filter(data, 1.0),clevs,colors=colors, extend=extend, zorder=50)
    return cs


def plotMapGrid(data, x, y, ax, title="", title_loc="center", clevs=[10,100,300,1000,3000,10000,30000,100000, 300000, 10000000], colors=None):
    """Plot multiple data on a map as contours with legends

    :param data: a list of data
    :param x: x dimension, common for all data
    :param y: y dimension, common for all data
    :param ax: plot axis, common for all data
    :param title: title of the plot, defaults to ""
    :param title_loc: location of the title, defaults to "center"
    :param clevs: levels where to put iso-lines, common for all plots
    :param colors: colors, should be a list (len(data)) of lists (len(levels))
    :return: contourf-object
    """
    plotBasicMap(ax=ax, title=title, title_loc=title_loc)
    # draw filled contours.
    if colors is None:
        colors = [ plt.cm.hsv(x) for x in np.linspace(0.5, 0, len(clevs)) ]
    cmap = plt.get_cmap('tab10')
    norm = matplotlib.colors.BoundaryNorm(clevs, ncolors=cmap.N, clip=True)

    cs = ax.pcolormesh(x,y,data,norm=norm, cmap=cmap)
    return cs


def rgbToColor(rgb):
    color = [int(x)/255 for x in rgb.split(':')]
    if len(color) != 4:
        color.append(1.)
    return color


def snapens(ncfiles, hour, outfile, contours_only=False):
    title = ""
    pos = -1
    steps = -1
    startDT = None
    endDT = None
    toa = []
    fl = {
        "000-200": [],
        "200-350": [],
        "350-550": []
    }
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
            if not "MAX6h_ASH_fl350-550" in nc.variables:
                print(f"MAX6h_ASH_fl350-550 not in {ncf}, please run 'snapAddToa {ncf}", file=sys.stderr)
                sys.exit(2)
            exVar = nc["MAX6h_ASH_fl350-550"]
            toa.append(nc["time_of_arrival"][0,:])
            fillvalue = nc["time_of_arrival"]._FillValue
            # read flight-levels
            for fli, flv in fl.items():
                flv.append(np.ma.filled(nc[f"MAX6h_ASH_fl{fli}"][pos,:], np.nan))
            lons = nc["longitude"][:]
            lats = nc["latitude"][:]
            x = nc["x"][:]
            y = nc["y"][:]
            if "grid_mapping" in exVar.ncattrs():
                grid_attrs = {}
                for attr in nc[exVar.grid_mapping].ncattrs():
                    grid_attrs[attr] = nc[exVar.grid_mapping].getncattr(attr)
                # print(grid_attrs)
                if grid_attrs['grid_mapping_name'] == 'lambert_conformal_conic':
                    proj = cartopy.crs.LambertConformal(central_longitude=grid_attrs['longitude_of_central_meridian'],
                        central_latitude=grid_attrs['latitude_of_projection_origin'],
                        standard_parallels=[grid_attrs['standard_parallel']],
                        globe=cartopy.crs.Globe(semiminor_axis=6371000,
                                                semimajor_axis=6371000))
                    data_x = x
                    data_y = y
                elif grid_attrs['grid_mapping_name'] == 'latitude_longitude':
                    data_x = lons
                    data_y = lats
                    proj = cartopy.crs.PlateCarree()
                else:
                    raise Exception(f"unknown grid_mapping_name: {grid_attrs['grid_mapping_name']}")
            else:
                logging.info(f"unknown grid_mapping_name {grid_attrs}, trying latlon/PlateCaree")
                data_x = lons
                data_y = lats
                proj = cartopy.crs.PlateCarree()

    thresholds = [0.2, 2, 5, 10] # QVA ash threshold in mg/m3
    toa = np.stack(toa)
    toa = toa.filled(fill_value=fillvalue)
    toa[toa == fillvalue] = (steps+1)*stepH
    toaPerc = np.percentile(toa, [10,50,90], axis=0)
    flPerc = {}
    flTH = {}
    for fli, flv in fl.items():
        flv = np.stack(flv)
        flv *= 1e3 # g -> mg
        fl[fli] = flv
        flPerc[fli] = np.percentile(flv, [17,50,83], axis=0)
        flTH[fli] = []
        for th in thresholds:
            data = np.sum(flv > th, axis=0, dtype='f4')*100/flv.shape[0]
            # data[data < 1] = np.nan
            flTH[fli].append(data)

    # and the plot
    formatter = matplotlib.ticker.ScalarFormatter()
    formatter.set_powerlimits((-3,10))

    if not contours_only:
        fig = plt.figure(figsize=(12, 10.7))
        fig.suptitle(f'{title}+{hour}hours ({endDT:%Y-%m-%d %H}:00Z). Uncertainty based upon {fl["350-550"].shape[0]} members.',
            y=0.92)
        colors = [ plt.cm.Paired(x) for x in [1,10,7,5,9] ]
        extent = ([x[0],x[-200],y[0],y[-60]])

        for i, (fli, flv) in enumerate(fl.items()):
            for j, th in enumerate(thresholds):
                ax = fig.add_subplot(4,4, i*4+j+1, projection=proj)
                ax.set_extent(extent, crs=proj)
                title = f"> {th}mg/m³"
                if j == 0:
                    title= f"FL{fli} Probability: " + title
                cs = plotMap(flTH[fli][j],
                        title=title,
                        ax=ax,
                        colors=colors,
                        extend="max", # dropping <10% outliers, but keep values > 90%
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
        ax = fig.add_subplot(4,4, 14, projection=proj)
        ax.set_extent(extent, crs=proj)
        clevs=range(0,(steps+1)*stepH,6)
        colors = [ plt.cm.jet(x) for x in np.linspace(0.95,0.1,len(clevs)) ]
        # 255:0:255,255:128:255  0-3;3-6
        # 128:0:128,128:0:255    6-9,9-12
        # 128:128:255 12-24 (split in two)
        # 128:128:192 24-36 (split in two)
        # 192:192:192 36-48 (split in two)
        rgbs = [
            '255:128:255',
            '128:0:255',
            '128:64:255',
            '128:128:255',
            '128:128:225',
            '128:128:192',
            '160:160:192',
            '192:192:192',
        ]
        for i, (_, c) in enumerate(zip(colors, rgbs)):
            colors[i] = rgbToColor(c)
        cs = plotMap(toaPerc[2,:],
                title="Time of arrival: 90 perc.",
                title_loc="left",
                ax=ax,
                colors=colors,
                clevs=clevs,
                extend=None,
                x=data_x, y=data_y)
        ax = fig.add_subplot(4,4, 15, projection=proj)
        ax.set_extent(extent, crs=proj)
        cs = plotMap(toaPerc[1,:],
                title="median",
                ax=ax,
                colors=colors,
                clevs=clevs,
                extend=None,
                x=data_x, y=data_y)
        ax = fig.add_subplot(4,4, 16, projection=proj)
        ax.set_extent(extent, crs=proj)
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
    else: # contours_only
        fig = plt.figure(figsize=(20, 8))
        fig.suptitle(f'{title}+{hour}hours ({endDT:%Y-%m-%d %H}:00Z). Uncertainty based upon {fl["350-550"].shape[0]} members.',
            y=0.92)

        extent = ([x[0],x[-200],y[0],y[-60]])
        for i, th in enumerate(thresholds):
            ax = fig.add_subplot(2,3, i+1, projection=proj)
            ax.set_extent(extent, crs=proj)
            plotBasicMap(
                ax=ax,
                title=f"ash conc. > {th}mg/m³",
                title_loc="center",
            )
            # 000-200 red, 200-350 green, 350-550 blue
            #colors =  plt.cm.tab20c([4,5,7, 8,9,11, 0,1,3])
            colors =  ['#ff0000ff','#ff000060','#ff000030', '#00a000ff','#00a00060','#00a00030', '#0000ffff','#0000ff60','#0000ff30']
            for fli, flv in enumerate(fl):
                data = gaussian_filter(flTH[flv][i], 1.0)
                cs = ax.contour(x,y,data,[30,50,70],colors=[colors[fli*3+2-j] for j in range(3)],
                                zorder=50+fli)
                #ax.clabel(cs, cs.levels, inline=False, fmt={x: f"FL{flv} {x}%" for x in cs.levels}, fontsize=7)

        # Time of arrival
        ax = fig.add_subplot(2,3, 5, projection=proj)
        ax.set_extent(extent, crs=proj)
        clevs=range(0,(steps+1)*stepH,6)
        colors = [ plt.cm.jet(x) for x in np.linspace(0.95,0.1,len(clevs)) ]
        # 255:0:255,255:128:255  0-3;3-6
        # 128:0:128,128:0:255    6-9,9-12
        # 128:128:255 12-24 (split in two)
        # 128:128:192 24-36 (split in two)
        # 192:192:192 36-48 (split in two)
        rgbs = [
            '255:128:255',
            '128:0:255',
            '128:64:255',
            '128:128:255',
            '128:128:225',
            '128:128:192',
            '160:160:192',
            '192:192:192',
        ]
        for i, (_, c) in enumerate(zip(colors, rgbs)):
            colors[i] = rgbToColor(c)
        cs = plotMap(toaPerc[2,:],
                title="Time of arrival: 10% (fastest)",
                title_loc="center",
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



if __name__ == "__main__":
    os.umask(0o002)

    parser = argparse.ArgumentParser(
        description="Read snap files from identical ensemble runs and create a plot with statistical analysis for a certain forecast-hour",
        usage=f"{sys.argv[0]} --hour FORECAST_HOUR snap_01.nc [snap_02.nc ... snap_xx.nc]"
    )
    parser.add_argument("--out", help="output png file", required=True)
    parser.add_argument("--hour", help="hour of output to analyse", type=int, required=True)
    parser.add_argument("--contours", help="plot only contours", action=argparse.BooleanOptionalAction, default=False)
    #parser.add_argument("--store", help="storeA or storeB, meteo and runtime-datastore, default used from MAPP-system")
    parser.add_argument('SNAPNC', help="snap*.nc filenames", nargs='+')
    args = parser.parse_args()

    snapens(args.SNAPNC, args.hour, args.out, contours_only=args.contours)
