import sys

import cdsapi

if len(sys.argv) < 3:
    print(f"usage: {sys.argv[0]} YYYY-MM-DD HH", file=sys.stderr)
    exit(1)

date = sys.argv[1]
utc = sys.argv[2]

dataset = "reanalysis-era5-complete"
request = {
    "date": f"{date}",
    "expver": "1",
    # Levels: 1 is top level, 137 the lowest model level in ERA5. Use '/' to separate values.
    "levelist": "137/136/134/132/130/128/126/124/122/120/118/116/114/112/110/108/106/104/102/100/98/96/94/92/90/88/86/84/82/80/78/76/74/72/70/68/65/62/59/56/53/50/47/44/41/38/35/32/29",
    "levtype": "ml",
    #  parameters from https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Table12
    "param": "131/132/130/135",  # "75/76/77/130/131/132/133/135/246/247/248",
    "step": "0/1/2/3/4/5/6/7/8/9/10/11/12/13",
    "stream": "oper",
    "time": f"{utc}:00:00",
    "type": "fc",
    "area": "50/-10/80/50",  # North, West, South, East. Default: global
    "grid": "0.25/0.25",  # Latitude/longitude. Default: }
}


client = cdsapi.Client(url="https://cds.climate.copernicus.eu/api")
client.retrieve(dataset, request, f"era5_ml_{date}_{utc}.grib")
