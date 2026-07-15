import datetime
import numbers
import os
import re
import tempfile
import warnings

import netCDF4
import numpy
import pyproj


def _parseLLNumber(llstr):
    """parse a latitude or longitude string to a decimal returning (decimal, character)
    where character can should be NSEW or empty.
    Possible formats: -3.54, 3:5:3 S, 3° 5', 34"S, N6451

    Raises a ValueError if the format doesn't match
    """

    if isinstance(llstr, numbers.Number):
        return (float(llstr), "")

    # remove + character coming from url-queries (space)
    llstr = llstr.replace("+", "")
    llstr = llstr.strip()
    decimal = 0

    if len(llstr) > 0 and llstr[0].upper() in "NSEW":
        character = llstr[0]
        num = llstr[1:].strip()
        degrees = num[:-2]
        minutes = num[-2:]

        number = float(degrees) + float(minutes) / 60.0
        return number, character.upper()

    # fetch and remove last character (NSEW)
    character = ""
    if re.search(r"[A-Za-z]$", llstr):
        character = llstr[-1].upper()
        llstr = llstr[0:-1]
        llstr = llstr.strip()

    # just a number
    if re.search(r"^(-?\d+)$", llstr):
        decimal = float(llstr)
    elif re.search(r"^(-?\d+\.\d+)$", llstr):
        decimal = float(llstr)
    else:
        # degree:minutes(:second)
        m = re.search(r"^(-?\d+)\s*[:°]\s*(\d+)(\s*[\:\']\s*)?(\d+)?", llstr)
        if m:
            decimal = float(m.group(1)) + float(m.group(2)) / 60
            if m.group(4):
                decimal += float(m.group(4)) / 3600
        else:
            raise ValueError("unable to parse lon/lat number: '" + llstr + "'")
    return (decimal, character.upper())


def parseLat(latStr):
    """parse a latitude string to decimal degrees, raise an exception on error
    Possible formats: -3.54, 3:5:3 S, 3° 5', 34"S, N6451
    """
    try:
        (decimal, northSouth) = _parseLLNumber(latStr)
    except TypeError as te:
        raise ValueError("cannot parse latitude: {}".format(te))

    if northSouth == "S":
        decimal *= -1
    elif northSouth == "" or northSouth == "N":
        pass
    else:
        raise ValueError("Not a latitude: " + latStr)
    if decimal < -90.0001 or decimal > 90.0001:
        raise ValueError("Not a latitude: " + latStr)
    return decimal


def parseLon(lonStr):
    """parse a longitude string to decimal degrees, raise an exception on error
    Possible formats: -3.54, 3:5:3 W, 3° 5', 34"W, W01947
    """
    try:
        (decimal, eastWest) = _parseLLNumber(lonStr)
    except TypeError as te:
        raise ValueError("cannot parse longitude: {}".format(te))
    if eastWest == "W":
        decimal *= -1
    elif eastWest == "" or eastWest == "E":
        pass
    else:
        raise ValueError("Not a longitude: " + lonStr)
    if decimal < -180.0001 or decimal > 180.0001:
        raise ValueError("Not a longitude: " + lonStr)
    return decimal


def dirIsWritable(directory):
    """check if directory is writable"""
    if not directory:
        return False
    try:
        with tempfile.TemporaryFile(dir=directory):
            return True
    except Exception:
        return False


def delete_oldfiles(dir_to_search, age_in_days):
    """delete files older than age_in_days"""
    for dirpath, dirnames, filenames in os.walk(dir_to_search):
        for file in filenames:
            curpath = os.path.join(dirpath, file)
            try:
                file_modified = datetime.datetime.fromtimestamp(
                    os.lstat(curpath).st_mtime
                )
                if datetime.datetime.now() - file_modified > datetime.timedelta(
                    days=age_in_days
                ):
                    os.remove(curpath)
            except FileNotFoundError:
                pass


def restrictDomainSizeAndResolution(
    file: str,
    lon: float,
    lat: float,
    gridSize: float = 0,
    calcGridSize: int = 0,
    supersampling: int = 1,
) -> str:
    """Get snap.input parameters to increase the resolution (supersampling) or
    reduce the calculation domain. This requires the input file to be in
    netcdf and to be read in snap using fimex, i.e. FIELD.TYPE=fimex and
    FIMEX.FILE_TYPE=netcdf

    :param file: example netcdf file
    :param lon: longitude of center of new/modified domain
    :param lat: latitude of center of new/modified domain
    :param gridSize: grid-cell size in km or degree (from ARGOS), ignored if 0
    :param calcGridSize: number of grid-cells in the output domain (does not need to be exact), ignored if 0 or larger than domain in file
    :param supersampling: supersampling of output, either calculated from gridSize, or given explicitly, ignored if <= 1
    :raises Exception: on errors while reading file
    :return: sniplet for use in snap.input to supersample to new gridSize (e.g. "FIELD.OUTPUT_RESOLUTION_FACTOR= 2\n")
        and for restricting the calculation domain, (e.g. "FIMEX.INTERPOLATION=nearest|+proj=l...")
    """
    retStr = ""
    if supersampling > 1:
        retStr = f"FIELD.OUTPUT_RESOLUTION_FACTOR= {supersampling}\n"
    if calcGridSize > 0 or gridSize > 0:
        with netCDF4.Dataset(file, "r") as nc:
            if "x_wind_ml" in nc.variables:
                var = nc["x_wind_ml"]
            elif "u_wind" in nc.variables:
                var = nc["u_wind"]
            elif "x_wind_pl" in nc.variables:
                var = nc["x_wind_pl"]  # gfs ml converted from pl
            else:
                raise Exception(
                    f"neither x_wind_ml, x_wind_pl nor u_wind in file: {file}"
                )

            dims = var.dimensions
            # snap expects x-dim to be fastest moving dimension
            x_dim = dims[-1]
            y_dim = dims[-2]

            xvals = nc[x_dim][:]
            yvals = nc[y_dim][:]
            resolution = numpy.abs(nc[x_dim][1] - nc[x_dim][0])
            if gridSize != 0:
                if nc[x_dim].units.startswith("m"):
                    resolution /= 1000  # gridSize given in deg or km, need deg or m
                supersampling = round(resolution / gridSize)
                if supersampling > 1:
                    retStr = f"FIELD.OUTPUT_RESOLUTION_FACTOR= {supersampling}\n"
                    calcGridSize = round(
                        calcGridSize / supersampling
                    )  # need calcGridSize in original resolution

            if "deg" in nc[x_dim].units:
                # latlon
                units = "degree"
                xpos = numpy.argmin(numpy.abs(xvals - lon))
                ypos = numpy.argmin(numpy.abs(yvals - lat))
                projstr = "+proj=latlon +R=6371000 +no_defs"
            else:
                # other projection (lcc) in m
                units = "m"
                grid_mapping = nc[var.grid_mapping]
                if grid_mapping.grid_mapping_name == "lambert_conformal_conic":
                    # projstr = f"+proj=lcc +lat_0={grid_mapping.latitude_of_projection_origin} +lon_0={grid_mapping.longitude_of_central_meridian} +lat_1={grid_mapping.standard_parallel[0]} +lat_2={grid_mapping.standard_parallel[1]} +x_0=0 +y_0=0 +R={grid_mapping.earth_radius} +units=m +no_defs +type=crs"
                    proj = pyproj.CRS.from_cf(grid_mapping.__dict__)
                    llproj = pyproj.CRS(
                        "EPSG:4326"
                    )  #  pyproj.CRS.from_proj4(f"+proj=latlong +R={grid_mapping.earth_radius} +no_defs")
                    with warnings.catch_warnings():
                        warnings.filterwarnings(
                            "ignore",
                            "You will likely lose important projection information when",
                            UserWarning,
                        )
                        projstr = proj.to_proj4()
                else:
                    raise Exception(
                        f"unimplemented grid_mapping_name: {grid_mapping.grid_mapping_name}"
                    )
                transformer = pyproj.Transformer.from_crs(llproj, proj, always_xy=True)
                xlonlat, ylonlat = transformer.transform(lon, lat)
                xpos = numpy.argmin(numpy.abs(xvals - xlonlat))
                ypos = numpy.argmin(numpy.abs(yvals - ylonlat))

            if calcGridSize > xvals.shape[0] or calcGridSize > yvals.shape[0]:
                calcGridSize = 0  # no change
            if calcGridSize > 0:
                calcGridSize_2 = calcGridSize // 2
                xlast = xpos + calcGridSize_2 + 1
                if xlast >= xvals.shape[0]:
                    xlast = xvals.shape[0] - 1
                xfirst = xpos - calcGridSize_2 - 1
                if xfirst < 0:
                    xfirst = 0
                ylast = ypos + calcGridSize_2 + 1
                if ylast >= yvals.shape[0]:
                    ylast = yvals.shape[0] - 1
                yfirst = ypos - calcGridSize_2 - 1
                if yfirst < 0:
                    yfirst = 0

                retStr += f"FIMEX.INTERPOLATION=nearest|{projstr}|{xvals[xfirst]},{xvals[xfirst + 1]},...,{xvals[xlast]}|{yvals[yfirst]},{yvals[yfirst + 1]},...,{yvals[ylast]}|{units}\n"
    return retStr
