#! /usr/bin/env python3

"""
Convert aerosols in a SNAP file to isotopes using the fractional distribution from Tovdal (2002)
"""

import netCDF4
from Snappy.BombIsotopeFractions import BombIsotopeFractions


def snap_add_bomb_isotopes(nc: netCDF4.Dataset, argos_operational=False):
    """
    ncfile: a netcdf-file with Aerosols opened in 'a'-mode
    argos_operational: only aerosols, no isotopes for faster operation
    """
    bomb_isotopes = BombIsotopeFractions()
    aerosols = []
    for var in nc.variables:
        if var.startswith("Aerosol") and var.endswith("acc_concentration"):
            aerosols.append(var[:-18])
    isos = ["H1"]
    if not argos_operational:
        isos += bomb_isotopes.isotopes()
    hours = nc["time"][:]  # snap writes usually hours since start
    for var in [
        "concentration",
        "column_concentration",
        "acc_dry_deposition",
        "acc_wet_deposition",
        "acc_concentration",
    ]:
        basevar = nc[f"{aerosols[0]}_{var}"]
        for iso in isos:
            # declare variables
            name = f"{iso}_{var}"
            if name not in nc.variables:
                nc.createVariable(
                    name,
                    basevar.datatype,
                    basevar.dimensions,
                    zlib=True,
                    chunksizes=basevar.chunking(),
                )
                for attr in basevar.ncattrs():
                    nc[name].setncattr(attr, basevar.getncattr(attr))
        laststepdata = 0
        for t, hr in enumerate(hours):
            # convert data
            basedata = 0
            for aero in aerosols:
                basedata += nc[f"{aero}_{var}"][t, :]
            for iso in isos:
                name = f"{iso}_{var}"
                if iso == "H1":
                    # sum all aerosols, e.g. frac = 1.
                    frac = 1.0
                else:
                    frac = bomb_isotopes.fraction(iso, hr)
                if (var == "acc_concentration") and t > 1:
                    # no decay in dose-equivalent
                    nc[name][t, :] = (
                        nc[name][t - 1, :] + (basedata - laststepdata) * frac
                    )
                else:
                    nc[name][t, :] = frac * basedata
            laststepdata = basedata


def main():
    import argparse

    parser = argparse.ArgumentParser(
        description="add isotope distribution and aerosol-sums to a snap.nc with bomb-aerosols"
    )
    parser.add_argument("--nc", help="snap.nc filename", required=True)
    parser.add_argument(
        "--argos_operational",
        help="argos_operational mode, no isotopes, just H+1",
        action="store_true",
    )

    args = parser.parse_args()
    with netCDF4.Dataset(args.nc, "a") as nc:
        snap_add_bomb_isotopes(args.nc)


if __name__ == "__main__":
    main()
