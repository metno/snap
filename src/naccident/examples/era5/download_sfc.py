import sys

import cdsapi

if len(sys.argv) < 3:
    print(f"usage: {sys.argv[0]} YYYY-MM-DD HH", file=sys.stderr)
    exit(1)

date = sys.argv[1]
utc = sys.argv[2]


dataset = "reanalysis-era5-complete"
request = {
    "class": "ea",
    "date": f"{date}",
    "expver": "1",
    "levtype": "sfc",
    "param": "165.128/166.128/134.128/151.128/228.128",  # "66.128/67.128/134.128/142.128/143.128/151.128/159.128/164.128/165.128/166.128/167.128/228.128/229.128/230.128/231.128",
    "step": "0/1/2/3/4/5/6/7/8/9/10/11/12/13",
    "stream": "oper",
    "time": f"{utc}:00:00",
    "type": "fc",
    "area": "50/-10/80/50",  # North, West, South, East. Default: global
    "grid": "0.25/0.25",  # Latitude/longitude. Default: }
}


client = cdsapi.Client(url="https://cds.climate.copernicus.eu/api")
client.retrieve(dataset, request, f"era5_sfc_{date}_{utc}.grib")
