#! /usr/bin/env python3

import argparse
import datetime
import cartopy
import matplotlib

matplotlib.use('Agg')
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
from matplotlib import pyplot as plt
import netCDF4
import subprocess
import os
import numpy
from pyproj import Proj
import sys
from time import gmtime, strftime, strptime

from Snappy.EcMeteorologyCalculator import EcMeteorologyCalculator
from Snappy.ICONMeteorologyCalculator import ICONMeteorologyCalculator
from Snappy.MeteorologyCalculator import MeteorologyCalculator
from Snappy.Resources import MetModel, Resources
from Snappy.Countries import get_country_list

def rgbToColor(rgb):
    color = [int(x)/255 for x in rgb.split(':')]
    if len(color) != 4:
        color.append(1.)
    return color

def get_isotope_release(res, reldict):
    errors = ""
    for tag in ('releaseTime', 'radius', 'lowerHeight', 'upperHeight'):
        if not tag in reldict:
            errors += "Cannot interprete {}: {}".format(tag, reldict[tag])

    source_tmpl = '''
MAX.PARTICLES.PER.RELEASE= 2000
TIME.RELEASE.PROFILE.STEPS
RELEASE.HOUR= 0, {releaseTime}
RELEASE.RADIUS.M= {radius}, {radius}
RELEASE.LOWER.M= {lowerHeight}, {lowerHeight}
RELEASE.UPPER.M= {upperHeight}, {upperHeight}
'''
    source_term = source_tmpl.format(releaseTime=reldict['releaseTime'],
                                        radius=reldict['radius'],
                                        lowerHeight=reldict['lowerHeight'], upperHeight=reldict['upperHeight'])

    isotopes = {'relI131': 'I131',
                'relXE133': 'Xe133',
                'relCS137': 'Cs137'}
    for rel, iso in isotopes.items():
        emis = 0.
        try:
            emis = float(reldict[rel])
        except:
            pass
        if (emis > 0.):
            source_term += "RELEASE.BQ/SEC.COMP= {rel}, {rel}, '{iso}'\n".format(rel=reldict[rel], iso=iso)

    # add Cs137, I131 and Xe133
    source_term += res.isotopes2snapinput([169, 158, 148])

    return (source_term, errors)

def does_overlap(bbox, bboxlist):
    '''check if bbox overlaps with any bbox in bboxlist'''
    for b in bboxlist:
        if b.intersection(b, bbox):
            return True
    return False

def get_country_plotlist(fig, ax, country_listname):
    countries = []
    textboxes = []
    for country in get_country_list('europe2'):
        txt = ax.text(country.lon, country.lat, country.name,
            horizontalalignment='center',
            verticalalignment='center',
            fontsize='xx-small',
            color='gray',
            #transform=cartopy.crs.GeoDetic(),
            zorder=51)
        fig.subplots_adjust(hspace=0.12, wspace=0.01)

        fig.canvas.draw()
        renderer = fig.canvas.renderer
        bbox = txt.get_window_extent(renderer)
        if not does_overlap(bbox, textboxes):
            textboxes.append(bbox)
            countries.append(country)
        else:
            txt.set_visible(False)
    return countries


def plotMap(data, x, y, ax, countries, title="", title_loc="center", clevs=[10,100,300,1000,3000,10000,30000,100000, 300000, 10000000], colors=None, extend='max'):

    ax.add_feature(cartopy.feature.GSHHSFeature(scale='low', facecolor='none', edgecolor='whitesmoke', linewidth=.2), zorder=100)
    #ax.add_feature(cartopy.feature.BORDERS, edgecolor="black", linewidth=.5, zorder=100) 
    #ax.add_feature(cartopy.feature.BORDERS, edgecolor="black", linewidth=.5, zorder=100) 
    ax.add_feature(cartopy.feature.NaturalEarthFeature(
        category='cultural',
        name='admin_0_boundary_lines_land',
        scale='10m',
        facecolor='none'),edgecolor="dimgrey", linewidth=.5, zorder=100)
    ax.add_feature(cartopy.feature.OCEAN,facecolor="none", edgecolor='#f2efe9', zorder=100) # ocean/land-border on top
    #ax.gridlines(draw_labels=True)
    ax.gridlines(edgecolor="lightgray", linewidth=.3, zorder=100)

    ny = data.shape[0]
    nx = data.shape[1]
    # draw filled contours.
    if colors is None:
        colors = [ plt.cm.hsv(x) for x in np.linspace(0.5, 0, len(clevs)) ]
    cs = ax.contourf(x,y,data,clevs,colors=colors, extend=extend, zorder=50)

    for country in countries:
        txt = ax.text(country.lon, country.lat, country.name,
            horizontalalignment='center',
            verticalalignment='center',
            fontsize='x-small',
            color='gray',
            #transform=cartopy.crs.GeoDetic(),
            zorder=51)
        
    # add title
    ax.set_title(title, loc=title_loc)
    ax.add_feature(cartopy.feature.OCEAN,facecolor="#aecfe0", edgecolor='none', zorder=10) # #aecfe0 = osm-sea
    ax.add_feature(cartopy.feature.LAND, facecolor="#f2efe9", edgecolor='none', zorder=10) # f2efe9 = osm-land
    ax.add_feature(cartopy.feature.LAKES,facecolor="#aecfe0", edgecolor='whitesmoke', linewidth=.2, zorder=20)
    return cs


#def runsnapsingle(npp, release_dt, tag_time, dirname, res):
def runsnap(npp, met_model, outdir, release_dt, runtime, plot_from_file=None):
    tag_time = gmtime() # ensure a few secs difference for tags
    res = Resources()
    dirname = os.path.join(res.getSnapOutputDir(), "{0}_{1}".format(npp, strftime("%Y-%m-%dT%H%M%S", tag_time)))

    nPPs = res.readNPPs()
    if not npp in nPPs:
        print(f"unknown NPP: {npp}", file=sys.stderr)
        sys.exit(1)

    if not plot_from_file:
        reldict = {
            'releaseTime': 96,
            'radius': 50,
            'lowerHeight': 50,
            'upperHeight': 500,
            'relI131': 1.39e13,
            'relXE133': 1.0e13,
            'relCS137': 2.6e11
        }
        (term, errors) = get_isotope_release(res, reldict)
        if (len(errors) > 0):
            print(errors, file=sys.stderr)
            return False
        lat = float(nPPs[npp]['lat'])
        lon = float(nPPs[npp]['lon'])
        simStart = strftime("%Y-%m-%d_%H:%M:%S", tag_time)
        print(f'outputdir for {simStart}: {dirname}')
        os.mkdir(dirname)
        sourceTerm = f"""
    TITLE={npp} {release_dt:%Y %m %d %H}
    SIMULATION.START.DATE={simStart}
    SET_RELEASE.POS= P=   {lat},   {lon}
    TIME.START= {release_dt:%Y %m %d %H}
    TIME.RUN = {runtime}h
    STEP.HOUR.OUTPUT.FIELDS= 3
    """
        sourceTerm += term
        with open(os.path.join(dirname, "snap.input"),'w') as fh:
            fh.write(sourceTerm)
            metdef = res.getDefaultMetDefinitions(met_model)
            if (met_model == MetModel.NrpaEC0p1):
                files = res.getECMeteorologyFiles(dtime=release_dt, run_hours=int(runtime))
                if (len(files) == 0):
                    raise Exception("no EC met-files found for {}, runtime {}".format(release_dt, runtime))
            elif met_model == MetModel.NrpaEC0p1Global:
                ecmet = EcMeteorologyCalculator(EcMeteorologyCalculator.getGlobalMeteoResources(), release_dt, float(lon), float(lat))
                ecmet.calc()
                if ecmet.must_calc():
                    raise Exception("no EC met-files calculated for {}".format(release_dt))
                files = ecmet.get_meteorology_files()
                (metdef["startX"], metdef["startY"]) = ecmet.get_grid_startX_Y()
            elif met_model == MetModel.EC0p1Global:
                globalRes = EcMeteorologyCalculator.getGlobalMeteoResources()
                files = [x[1] for x in sorted(MeteorologyCalculator.findAllGlobalData(globalRes), key=lambda x: x[0])]
                lat0 = MeteorologyCalculator.getLat0(float(lat), globalRes.domainHeight)
                lon0 = MeteorologyCalculator.getLon0(float(lon), globalRes.domainWidth)
                fh.write(f"FIELD.TYPE=fimex\n")
                fh.write(f"FIMEX.FILE_TYPE=netcdf\n")
                fh.write(f"FIMEX.INTERPOLATION=nearest|+proj=latlon +R=6371000 +no_defs|{lon0},{lon0+0.2},...,{lon0+globalRes.domainWidth}|{lat0},{lat0+0.2},...,{lat0+globalRes.domainHeight}|degree\n")
            elif met_model == MetModel.Meps2p5:
                files = res.getMeteorologyFiles(met_model, release_dt, runtime, "best")
                if (len(files) == 0):
                    raise Exception("no MEPS2_5 met-files found for {}, runtime {}".format(release_dt, runtime))
            elif met_model == MetModel.Icon0p25Global:
                metcalc = ICONMeteorologyCalculator(ICONMeteorologyCalculator.getGlobalMeteoResources(), release_dt, float(lon), float(lat))
                metcalc.calc()
                if metcalc.must_calc():
                    raise Exception("no ICON met-files calculated for {}".format(release_dt))
                files = metcalc.get_meteorology_files()
                (metdef["startX"], metdef["startY"]) = metcalc.get_grid_startX_Y()
            elif met_model == MetModel.GfsGribFilter:
                files = res.getMeteorologyFiles(met_model, release_dt, runtime, "best")
                if (len(files) == 0):
                    raise Exception("no GFS-grib-filter-fimex met-files found for {}, runtime {}".format(release_dt, runtime))
            else:
                raise Exception('unknown met_model: {}'.format(met_model))
            fh.write(res.getSnapInputMetDefinitions(met_model, files, **metdef))

        with open(f"{dirname}/snap.stdout.log", 'wt') as stdout:
            with open(f"{dirname}/snap.stderr.log", 'wt') as stderr:
                os.environ['OMP_NUM_THREADS']='1' # no thread usage
                subprocess.run(['bsnap_naccident', 'snap.input'],
                                cwd=dirname,
                                stdout=stdout,
                                stderr=stderr
                                )
                # add time of arrival
                subprocess.run(['snapAddToa', 'snap.nc'],
                                cwd=dirname,
                                stdout=stdout,
                                stderr=stderr
                                )
        plot_from_file = os.path.join(dirname,'snap.nc')

    # and plot
    with netCDF4.Dataset(plot_from_file, 'r') as nc:
        lons = nc["longitude"][:]
        lats = nc["latitude"][:]
        x = nc["x"][:]
        y = nc["y"][:]
        toa = nc["time_of_arrival"][0,:]
        fillvalue = nc["time_of_arrival"]._FillValue
        toa = toa.filled(fill_value=fillvalue)
        title = nc.title
        times = netCDF4.num2date(nc["time"][:], nc["time"].units)
        steps = len(times)
        step = times[1] - times[0]
        # estimate starttime
        if not release_dt:
            release_dt = times[0] - step
        stepH = step.seconds // 3600 + step.days * 24
        plotsteps = [ x for x in range((steps//4)-1, steps, steps//4) ]
        if len(plotsteps) == 4:
            plotsteps[-1] = -1
        else:
            plotsteps.append(-1)
        plottimes = [ times[x] for x in plotsteps ]
        deps = [ nc['Cs137_acc_total_deposition'][x,:] for x in plotsteps ]
        

    formatter = matplotlib.ticker.ScalarFormatter()
    formatter.set_powerlimits((-3,10))

    # get the countries fitting on the figure (requires drawing)
    proj = cartopy.crs.PlateCarree()
    fig = plt.figure(figsize=(24, 9))
    ax = fig.add_subplot(2, 4, 1, projection=proj)
    ax.set_extent([x[0],x[-1],y[0],y[-1]], crs=proj)
    countries_small_fig = get_country_plotlist(fig=fig, ax=ax, country_listname='europe2')
    fig = plt.figure(figsize=(24, 9))
    ax = fig.add_subplot(2, 4, (6,7), projection=proj)
    ax.set_extent([x[50],x[-50],y[25],y[-1]], crs=proj)
    countries_large_fig = get_country_plotlist(fig=fig, ax=ax, country_listname='europe2')


    fig = plt.figure(figsize=(24, 9))
    fig.suptitle(f'{title} +{runtime}hours ({release_dt+datetime.timedelta(hours=runtime):%Y-%m-%d %H}:00Z).',
        y=0.92)
    colors=('g', 'y', 'tab:orange', 'r', 'tab:red')
    for i, tstep in enumerate(plottimes):
        ax = fig.add_subplot(2, len(plottimes), i+1, projection=proj)
        ax.set_extent([x[0],x[-1],y[0],y[-1]], crs=proj)
        cs = plotMap(deps[i]/1000,
                title=f"Cs-137 deps {tstep:%Y-%m-%d %H}Z",
                title_loc="left",
                ax=ax,
                countries=countries_small_fig,
                colors=colors,
                clevs=[0.1,1, 10, 100, 1000],
                x=lons, y=lats)
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

    # Time of arrival
    ax = fig.add_subplot(2,4, (6,7), projection=proj)
    ax.set_extent([x[50],x[-50],y[25],y[-1]], crs=proj)
    clevs=range(0,(steps+1)*stepH,2*stepH)
    colors = [ plt.cm.jet(x) for x in numpy.linspace(0.95,0.1,len(clevs)) ]
    # dsa colors up to 48 hours, here only every 6 hours
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
    cs = plotMap(toa[:],
            title="Time of arrival",
            title_loc="left",
            countries=countries_large_fig,
            ax=ax,
            colors=colors,
            clevs=clevs,
            extend=None,
            x=lons, y=lats)
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
    now = datetime.datetime.now()
    file = os.path.join(outdir, f"plots_{now:%Y-%m-%dT%HZ}_{npp}_{release_dt:%Y-%m-%dT%H}Z+{runtime}h.png")
    fig.savefig(file, bbox_inches='tight')
    return True



def main():
    os.umask(0o002)

    parser = argparse.ArgumentParser(
        description="Run snap for a single Nuclear Power Plants and receive a figure with latest dispersion and toa",
        usage=f"{sys.argv[0]} --metmodel=icon_0p25_global --npp ZAPORIZHZHYA --runtime 168 --outdir dir"
    )
    parser.add_argument("--outdir", help="output directory for file ", default='.')
    parser.add_argument("--store", help="storeA or storeB, meteo and runtime-datastore, default used from MAPP-system")
    parser.add_argument("--metmodel", help="the metmodel to use: nrpa_ec_0p1, nrpa_ec_0p1_global, ec_0p1_global, meps_2_5km, gfs_grib_filter_fimex, icon_0p25_global", default='nrpa_ec_0p1')
    parser.add_argument("--starttime", help="start time YYYY-MM-DD_HH")
    parser.add_argument("--runtime", help="the run and releasetime", required=True)
    parser.add_argument("--plotonly_ncfile", help="don't run the model plot only from this ncfile")
    parser.add_argument("--npp", help="name of Nuclear Power Plant, e.g. CHERNOBYL (see list in SnapPy)")
    args = parser.parse_args()

    if (args.starttime):
        release_dt = datetime.datetime.strptime(args.starttime, "%Y-%m-%d_%H")
    else:
        release_dt = datetime.datetime.now() + datetime.timedelta(hours=1)
    if (args.plotonly_ncfile):
        plot_from_file = args.plotonly_ncfile
    else:
        plot_from_file = None

    if (args.store):
        os.environ["STORE"] = args.store
    runsnap(outdir=args.outdir, met_model=args.metmodel, runtime=int(args.runtime), npp=args.npp, release_dt=release_dt, plot_from_file=plot_from_file)


if __name__ == "__main__":
    main()
