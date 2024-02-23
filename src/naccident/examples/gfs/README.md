# Configuring SNAP with NOAA GFS meteorological data

This is a rough setup to use SNAP with freely available meteorological data from NOAA. The authors are not working operationally with GFS data, so this is rather a proof of concept than a full operational setup. In particularly precipitation and vertical levels has not been tuned to provide best results for the region of interest.

## Prerequisites

This guide uses fimex: https://github.com/metno/fimex for conversion of grib to netcdf and for the conversion of pressure-levels to sigma-hybrid pressure-levesl.

All input files can be found in the snap repository und [src/naccident/examples/gfs/](./)

## Downloading data via grib-filter

The NOMADS servers allow downloading data for regional subsets, which reduces the download amount largely: https://nomads.ncep.noaa.gov/ Select the grib_filter option of the desired dataset and then the desired model-run, i.e. 2019-12-02 00UTC https://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p25.pl?dir=%2Fgfs.20191202%2F00

To download the first 16 timesteps of this model and ar region from [-10,50] longitude and [50,80] latitude, use a script like:

```sh
# timesteps
DATE=20200106
HOUR=00
for x in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16; do

# 10m-winds
   wget -O uv10m.grib$x "https://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p25.pl?file=gfs.t00z.pgrb2.0p25.f0$x&lev_10_m_above_ground=on&var_UGRD=on&var_VGRD=on&subregion=&leftlon=-10&rightlon=50&toplat=80&bottomlat=50&dir=%2Fgfs.${DATE}%2F${HOUR}%2Fatmos";

# pressure-levels: wind and temp
   wget -O presLev.grib$x "https://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p25.pl?file=gfs.t00z.pgrb2.0p25.f0$x&lev_100_mb=on&lev_150_mb=on&lev_200_mb=on&lev_250_mb=on&lev_350_mb=on&lev_400_mb=on&lev_450_mb=on&lev_500_mb=on&lev_550_mb=on&lev_600_mb=on&lev_650_mb=on&lev_700_mb=on&lev_750_mb=on&lev_800_mb=on&lev_850_mb=on&lev_900_mb=on&lev_925_mb=on&lev_950_mb=on&lev_975_mb=on&var_TMP=on&var_UGRD=on&var_VGRD=on&subregion=&leftlon=-10&rightlon=50&toplat=80&bottomlat=50&dir=%2Fgfs.${DATE}%2F${HOUR}%2Fatmos" ;

# precipition and surface pressure
   wget -O surf.grib$x "https://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p25.pl?file=gfs.t00z.pgrb2.0p25.f0$x&lev_surface=on&var_PRATE=on&var_PRES=on&var_PRMSL=on&subregion=&leftlon=-10&rightlon=50&toplat=80&bottomlat=50&dir=%2Fgfs.${DATE}%2F${HOUR}%2Fatmos"

done

# cat all grib-files together, can be mixed from different model-runs
cat *.grib?? > all.grib

```

It is currently not possible to download a region at the datum-border 180deg to -180deg in the pacific ocean. This requires downloading the complete globe and re-interpolation of the region.

## Convert data to netcdf

Use fimex to convert data to netcdf, and remove some unnecessary dimensions

```sh
fimex --input.file=all.grib --input.config=cdmGribReaderConfigGFS.xml \
      --ncml.config=reduceDims.ncml \
      --output.file=gfs_0p25deg_pl_${DATE}T${HOUR}Z.nc --output.type=nc4
```

## Convert pressure levels to sigma-hybrid pressure levels

Use fimex to convert pressure levels to sigma-hybrid pressure levels as defined in sigmaHybrid.ncml

```sh
fimex --input.file=gfs_0p25deg_pl_${DATE}T${HOUR}Z.nc --input.config=sigmaHybrid.ncml \
       --verticalInterpolate.type=pressure \
       --verticalInterpolate.method=linear \
       --verticalInterpolate.templateVar=to_hybrid \
       --output.file=gfs_0p25deg_${DATE}T${HOUR}Z.nc --output.type=nc4

```


## Run the snap-model

Adapt the contributed [snap.input](./snap.input) file with the source-term, in particular TIME.START and SET_RELEASE.POS= P=
and run the model:

```sh
bsnap_naccident snap.input
```

In approximately 5min, the snap.nc file is generated and can be seen with your preferred netcdf-viewer.

It is possible to declare several meteorological input file (here allVInt.nc) with the same area/resolution in the snap.input file. SNAP will automatically use the newest timesteps available.

