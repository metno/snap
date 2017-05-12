from mpl_toolkits.basemap import Basemap
# requires netcdf4-python (netcdf4-python.googlecode.com)
from netCDF4 import Dataset as NetCDFFile
import numpy as np
import matplotlib.pyplot as plt
import matplotlib

def fmt(x, pos):
    a, b = '{:.1e}'.format(x).split('e')
    b = int(b)
    return '{:2.0e}'.format(x)


def plotMap(data, lons, lats, ax, title="", bb={"south":65, "north":75, "west":10, "east":35}, clevs=[10,100,300,1000,3000,10000,30000,100000, 300000, 10000000]):
    m = Basemap(projection='cyl',llcrnrlat=bb["south"],urcrnrlat=bb["north"], \
                llcrnrlon=bb["west"],urcrnrlon=bb["east"],resolution='i', ax=ax)
    # find x,y of map projection grid.
    #lons, lats = np.meshgrid(lons, lats)
    x, y = m(lons, lats)
    # draw coastlines, state and country boundaries, edge of map.
    m.drawcoastlines()
    m.drawlsmask(resolution='i')
    m.drawcountries()
    # draw parallels.
    parallels = np.arange(0.,90,10.)
    m.drawparallels(parallels,labels=[1,0,0,0],fontsize=10)

    # draw meridians
    meridians = np.arange(-180.,180.,10.)
    m.drawmeridians(meridians,labels=[0,0,0,1],fontsize=10)
    ny = data.shape[0]
    nx = data.shape[1]
    # draw filled contours.
    colors = [ plt.cm.jet(x) for x in np.linspace(0, 1, len(clevs)) ]
    cs = m.contourf(x,y,data,clevs,colors=colors)
    # add colorbar.
    cbar = m.colorbar(cs,location='bottom',pad="10%", format=matplotlib.ticker.FuncFormatter(fmt))
    #cbar.set_label('Bq/m2')
    # add title
    ax.set_title(title)



fig, axi = plt.subplots(4,3,sharex=True, sharey=True, figsize=(14, 15.5))

for ix, radius in enumerate([5,10,20]):
    for iy, dens in enumerate([19,11.6,8.1,3.3]):
        nc = NetCDFFile('/lustre/storeB/project/fou/kl/cerad/Projects/2017_KolaRework/Runs/run_{}_{}/snap.nc'.format(radius,dens))
        # data from http://water.weather.gov/precip/
        var1 = nc.variables['Cs137_acc_wet_deposition']
        var2 = nc.variables['Cs137_acc_dry_deposition']
        data = var1[24,:,:] + var2[24,:,:]

        lons = nc.variables['longitude'][:]
        lats = nc.variables['latitude'][:]

        plotMap(data,
                title="{}µm {}g/cm3".format(2*radius, dens),
                ax=axi[iy,ix],
                bb={"south":62, "north":77, "west":12, "east":33},
                clevs=[100000, 300000, 1000000, 3000000, 10000000, 100000000],
                lons=lons, lats=lats)

fig.subplots_adjust(hspace=0.01, wspace=0.2)
fig.savefig("kolaFlightMaps.png")
#plt.show()

