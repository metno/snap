# Configuring SNAP with ECMWF ERA5 meteorological data

This is a rough setup to use SNAP with ... available meteorological data from ECMWF. *The authors are not working operationally with Era5 data, so this is rather a proof of concept than a full operational setup*. "In particularly precipitation and vertical levels has not been tuned to provide best results for the region of interest."

## Prerequisites

Access to ERA5 atmospheric reanalysis is provided through the Climate Data Store Application Program Interface (CDS API). This requires the installation of the CDS API application in the users' computer. More information on how to install can be found at [https://cds.climate.copernicus.eu/how-to-api](https://cds.climate.copernicus.eu/how-to-api).

This guide uses fimex: https://github.com/metno/fimex for conversion of grib to netcdf.

All input files can be found in the snap repository under [src/naccident/examples/era5/](./)

## Downloading data via grib-filter

The CDS servers allow downloading data for regional subsets, which reduces the download amount largely. Using cdsapi, you can download data from the [complete era5 global atmospheric reanalysis](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-complete?tab=d_download).

Check https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation for the format of the api request. 

To run SNAP, the required parameters (paramid) are: 

model levels: U- and V- winds (131/132), temperature (130)

surface levels: 10m U- V- winds (165/166), surface pressure (134), Mean seal-level pressure (151), Total precipitation (228)


To download 13 hourly timesteps of data at certain model levels and a region subset from [-10,50] longitude and [50,80] latitude, use a python script like:


```py
# timesteps
date=2025-09-07
utc=06    #can only be 06 or 18 for type=fc

dataset = "reanalysis-era5-complete"
request = {
    "date": f"{date}",
# Levels: 1 is top level, 137 the lowest model level in ERA5. Use '/' to separate values.
    'levelist': '137/136/134/132/130/128/126/124/122/120/118/116/114/112/110/108/106/104/102/100/98/96/94/92/90/88/86/84/82/80/78/76/74/72/70/68/65/62/59/56/53/50/47/44/41/38/35/32/29',                   
    "levtype": "ml",
#  parameters from https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Table12
    "param": "75/76/77/130/131/132/133/135/246/247/248",
    "step": "0/1/2/3/4/5/6/7/8/9/10/11/12/13", #runs until 19:00
    "stream": "oper",
    "time": f"{utc}:00:00",
    "type": "fc",
     "area": '50/50/-10/80',          # North, West, South, East Default: global
     "grid": '0.25/0.25',               # Latitude/longitude. Default: }
}

client = cdsapi.Client(url="https://cds.climate.copernicus.eu/api")
client.retrieve(dataset, request, f"era5_ml_{date}_{utc}.grib")

```
The surface data may be requested in a similar script, with `"levtype": "sfc"` and the required param ids in `"param":`.

```sh

# cat all grib-files together, can be mixed from different model-runs
cat *.grib?? > all.grib

```

*It is currently not possible to download a region at the datum-border 180deg to -180deg in the pacific ocean. This requires downloading the complete globe and re-interpolation of the region.*

## Convert data to netcdf

Use fimex to convert data to netcdf, and remove some unnecessary dimensions. Please note that the ncml
files use the official shape-order as needed for fimex from version 2.0[^1].

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

Adapt the contributed [snap.input](./snap.input) file to use the freshly downloaded file `gfs_0p25deg_${DATE}T${HOUR}Z.nc`
in `FIELD.INPUT=`.
Adapt also `TIME.START=` and `SET_RELEASE.POS= P=` to your needs and run the model:

```sh
bsnap_naccident snap.input
```

In approximately 5min, the snap.nc file is generated and can be seen with your preferred netcdf-viewer.

It is possible to declare several meteorological input file (here allVInt.nc) with the same area/resolution in the snap.input file. SNAP will automatically use the newest timesteps available.

[^1]: For fimex 1.x, please edit the ncml files and make sure that `time` is the last dimension in the shape, e.g. `<variable name="to_hybrid" shape="longitude latitude hybrid time" type="int">`
