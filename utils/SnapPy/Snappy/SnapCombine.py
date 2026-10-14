#! /usr/bin/env python3
"""
Combine several snap.nc output files (e.g. different release sites) into
one snap.nc file on a common time axis.

Concentration and deposition fields are summed. Time steps may be extrapolated
(before the start or after the end of a file), but they are never interpolated:
all files must share the same time step and lie on a common time grid.
"""

import argparse
import datetime
import logging
import math
import os
import re
import sys
import zipfile

import netCDF4
import numpy as np

from Snappy.Resources import Resources, snapNc_convert_to_grib

TIME = "time"
_SUM_RE = re.compile(r"_(concentration|deposition)")
_ACC_DEPO_RE = re.compile(r"^(.+?)_acc_(.*_)?deposition$")
# dropped, can be recalculated by AddToa.add_toa_to_nc
_SKIP_VARS = ("time_of_arrival",)
_SKIP_SUFFIX = "_acc_total_deposition"
_GRID_VARS = ("x", "y", "longitude", "latitude", "rlon", "rlat", "lon", "lat")


class SnapCombineError(Exception):
    pass


def _read_times(nc: netCDF4.Dataset) -> list[datetime.datetime]:
    tvar = nc.variables[TIME]
    calendar = getattr(tvar, "calendar", "standard")
    times = netCDF4.num2date(
        tvar[:],
        units=tvar.units,
        calendar=calendar,
        only_use_cftime_datetimes=False,
        only_use_python_datetimes=True,
    )
    return [t.replace(tzinfo=None) for t in np.atleast_1d(times).tolist()]


def combine_times(
    file_times: list[list[datetime.datetime]],
) -> tuple[list[datetime.datetime], datetime.timedelta]:
    """Create a combined, regular time axis from the time axes of several files.

    Raises SnapCombineError if the time-steps of the files would require
    interpolation, i.e. different time-steps or time-grids which are shifted
    against each other.

    Returns the combined times and the time-step.
    """
    if not file_times or any(len(t) == 0 for t in file_times):
        raise SnapCombineError("all input files need at least one time-step")
    dt = None
    for i, times in enumerate(file_times):
        diffs = {b - a for a, b in zip(times[:-1], times[1:])}
        if not diffs:
            continue
        if len(diffs) > 1:
            raise SnapCombineError(f"input file #{i + 1} has irregular time-steps")
        fdt = diffs.pop()
        if fdt <= datetime.timedelta(0):
            raise SnapCombineError(f"input file #{i + 1} has non-increasing times")
        if dt is None:
            dt = fdt
        elif fdt != dt:
            raise SnapCombineError(
                f"input files have different time-steps ({dt} and {fdt}), "
                "this would require interpolation"
            )

    all_times = sorted({t for times in file_times for t in times})
    t0 = all_times[0]
    if dt is None:
        # only single-time-step files
        if len(all_times) == 1:
            return all_times, datetime.timedelta(0)
        dt = min(b - a for a, b in zip(all_times[:-1], all_times[1:]))

    for t in all_times:
        if (t - t0) % dt != datetime.timedelta(0):
            raise SnapCombineError(
                f"time {t:%Y-%m-%dT%H:%M} not on time-grid starting {t0:%Y-%m-%dT%H:%M} "
                f"with step {dt}, this would require interpolation"
            )
    nsteps = (all_times[-1] - t0) // dt
    return [t0 + i * dt for i in range(nsteps + 1)], dt


def _is_summed(name: str) -> bool:
    return _SUM_RE.search(name) is not None


def _isotope_list() -> dict[int, dict]:
    """isotope-id -> {isotope, type, decay} from isotope_list.txt"""

    return Resources().getIsotopes()


def _decay_rates() -> dict[str, float]:
    """isotope-name (e.g. Cs137) -> decay-rate in 1/s, from isotope_list.txt"""
    return {iso["isotope"]: iso["decay"] for iso in _isotope_list().values()}


def _check_grids(ncs: list[netCDF4.Dataset], files: list[str]):
    ref = ncs[0]
    for nc, fname in zip(ncs[1:], files[1:]):
        for vname in _GRID_VARS:
            if vname in ref.variables or vname in nc.variables:
                if not (vname in ref.variables and vname in nc.variables):
                    raise SnapCombineError(
                        f"grid-variable {vname} not in all files: {files[0]}, {fname}"
                    )
                a = ref.variables[vname][:]
                b = nc.variables[vname][:]
                if a.shape != b.shape or not np.allclose(a, b, rtol=0, atol=1e-5):
                    raise SnapCombineError(
                        f"grids differ in variable {vname}: {files[0]}, {fname}"
                    )
        for vname, var in nc.variables.items():
            if _is_summed(vname) and vname in ref.variables:
                if var.shape[1:] != ref.variables[vname].shape[1:]:
                    raise SnapCombineError(
                        f"shape of {vname} differs: {files[0]}, {fname}"
                    )


def _copy_attrs(src, dest):
    for attr in src.ncattrs():
        if attr == "_FillValue":
            continue
        try:
            dest.setncattr(attr, src.getncattr(attr))
        except Exception:
            logging.warning(f"unable to set attribute '{attr}', ignoring")


def _components(ncs: list[netCDF4.Dataset]) -> list[str]:
    comps = []
    for nc in ncs:
        if "components" in nc.variables:
            for c in netCDF4.chartostring(nc.variables["components"][:]).tolist():
                c = str(c).strip()
                if c and c not in comps:
                    comps.append(c)
        else:
            for v in nc.variables:
                if v.endswith("_acc_concentration"):
                    c = v[: -len("_acc_concentration")]
                    if c and c not in comps:
                        comps.append(c)
    return comps


def snap_combine(
    infiles: list[str],
    outfile: str,
    grib_tag: str | None = None,
    model: str = "SNAP",
    recalc_toa: bool = False,
):
    """Combine several snap.nc files to one file outfile.

    - variables containing _concentration or _deposition are summed. Outside a
      files time-range they are 0, except accumulated (_acc_) fields after the
      end of a file, which keep the last value. Accumulated depositions of
      radioactive isotopes are decayed according to isotope_list.txt.
    - other time-dependent variables are taken from any file with the time-step
    - time_of_arrival and *_acc_total_deposition are dropped
    - recalc_toa: recalculate time_of_arrival and *_acc_total_deposition
      (as snapAddToa)
    - grib_tag: if set, convert the result to grib files with this identifier
    - model: the model to be used for the conversion (default: "SNAP")

    Raises SnapCombineError if the files cannot be combined.
    """
    if not infiles:
        raise SnapCombineError("no input files")
    if os.path.abspath(outfile) in [os.path.abspath(f) for f in infiles]:
        raise SnapCombineError("output file must not be one of the input files")

    ncs = [netCDF4.Dataset(f, "r") for f in infiles]
    try:
        file_times = [_read_times(nc) for nc in ncs]
        times, dt = combine_times(file_times)
        _check_grids(ncs, infiles)
        try:
            _write_combined(ncs, infiles, file_times, times, outfile)
        except BaseException:
            if os.path.exists(outfile):
                os.remove(outfile)
            raise
    finally:
        for nc in ncs:
            nc.close()

    if recalc_toa:
        if len(times) > 1:
            from Snappy.AddToa import add_toa_to_nc

            with netCDF4.Dataset(outfile, "a") as nc:
                add_toa_to_nc(nc)
        else:
            logging.warning("only one time-step, not adding time_of_arrival")

    if grib_tag:
        if not recalc_toa:
            logging.warning(
                "grib without --recalc-toa: time_of_arrival and total depositions missing"
            )
        if not grib_tag.endswith("_SNAP"):
            # default result_infix in modelserver config
            grib_tag += "_SNAP"
        _convert_to_grib(outfile, grib_tag, model)


def _write_combined(ncs, infiles, file_times, times, outfile):
    # reference file: earliest start
    ref_idx = int(np.argmin([ft[0] for ft in file_times]))
    ref = ncs[ref_idx]
    comps = _components(ncs)

    # union of all variables, first occurrence defines the variable
    var_src = {}
    for nc in [ref] + ncs:
        for vname, var in nc.variables.items():
            if vname in _SKIP_VARS or vname.endswith(_SKIP_SUFFIX) or vname in var_src:
                continue
            var_src[vname] = var

    with netCDF4.Dataset(outfile, "w", format="NETCDF4_CLASSIC") as out:
        dims = {}
        for nc in [ref] + ncs:
            for dname, dim in nc.dimensions.items():
                if dname not in dims:
                    dims[dname] = None if dim.isunlimited() else len(dim)
                elif dims[dname] is not None and not dim.isunlimited():
                    dims[dname] = max(dims[dname], len(dim))
        if comps and "nocomp" in dims:
            dims["nocomp"] = len(comps)
        for dname, size in dims.items():
            if dname == TIME:
                size = None
            out.createDimension(dname, size)

        _copy_attrs(ref, out)
        titles = [getattr(nc, "title", "") for nc in ncs]
        summaries = [getattr(nc, "summary", "") for nc in ncs]
        out.title = "Combined: " + " + ".join(t for t in titles if t)
        out.summary = " ".join(s.strip() for s in summaries if s)
        history = (
            f"{datetime.datetime.now(datetime.timezone.utc):%Y-%m-%dT%H:%M:%SZ} "
            f"snapCombine -o {outfile} {' '.join(infiles)}"
        )
        if "history" in ref.ncattrs():
            history = ref.getncattr("history") + "\n" + history
        out.history = history

        for vname, var in var_src.items():
            kwargs = {}
            if "_FillValue" in var.ncattrs():
                kwargs["fill_value"] = var.getncattr("_FillValue")
            out_v = out.createVariable(
                vname,
                var.dtype,
                var.dimensions,
                zlib=len(var.dimensions) > 1,
                shuffle=len(var.dimensions) > 1,
                **kwargs,
            )
            _copy_attrs(var, out_v)

        # time
        tvar = ref.variables[TIME]
        calendar = getattr(tvar, "calendar", "standard")
        out.variables[TIME][:] = netCDF4.date2num(
            times, units=tvar.units, calendar=calendar
        )

        # time-independent variables
        for vname, var in var_src.items():
            if TIME in var.dimensions:
                continue
            if vname == "components" and comps:
                out.variables[vname][:] = netCDF4.stringtochar(
                    np.array(comps, dtype=f"S{dims['compnamelenmax']}")
                )
            elif var.shape:
                out.variables[vname][:] = var[:]
            else:
                out.variables[vname].assignValue(var.getValue())

        decay = None
        for vname, var in var_src.items():
            if TIME not in var.dimensions or vname == TIME:
                continue
            if var.dimensions[0] != TIME:
                raise SnapCombineError(f"time must be first dimension in {vname}")
            out_v = out.variables[vname]
            if _is_summed(vname):
                m = _ACC_DEPO_RE.match(vname)
                rate = 0.0
                if m:
                    if decay is None:
                        decay = _decay_rates()
                    comp = m.group(1)
                    if comp in decay:
                        rate = decay[comp]
                    else:
                        logging.warning(
                            f"no decay-rate for '{comp}' found, not decaying {vname}"
                        )
                _write_summed(
                    ncs, file_times, times, vname, out_v, "_acc_" in vname, rate
                )
            else:
                _write_any(ncs, file_times, times, vname, out_v)


def _write_summed(ncs, file_times, times, vname, out_v, is_acc, decay_rate):
    shape = out_v.shape[1:]
    lasts = {}
    for i, t in enumerate(times):
        data = np.zeros(shape, dtype=np.float64)
        for j, (nc, ftimes) in enumerate(zip(ncs, file_times)):
            if vname not in nc.variables:
                continue
            if t < ftimes[0]:
                continue
            if t <= ftimes[-1]:
                k = ftimes.index(t)
                data += np.ma.filled(nc.variables[vname][k, ...], 0.0)
            elif is_acc:
                if j not in lasts:
                    lasts[j] = np.ma.filled(nc.variables[vname][-1, ...], 0.0)
                factor = 1.0
                if decay_rate > 0:
                    secs = (t - ftimes[-1]).total_seconds()
                    factor = math.exp(-decay_rate * secs)
                data += lasts[j] * factor
        out_v[i, ...] = data.astype(out_v.dtype)


def _write_any(ncs, file_times, times, vname, out_v):
    for i, t in enumerate(times):
        for nc, ftimes in zip(ncs, file_times):
            if vname in nc.variables and t in ftimes:
                out_v[i, ...] = nc.variables[vname][ftimes.index(t), ...]
                break
        else:
            logging.info(f"time {t} not available for {vname}, left undefined")


def _isotopes_in_file(ncfile: str) -> list[int]:
    names = []
    with netCDF4.Dataset(ncfile, "r") as nc:
        for var in nc.variables:
            if var.endswith("acc_concentration") or var.endswith(
                "column_concentration"
            ):
                continue
            if var.endswith("_concentration"):
                names.append(var[: -len("_concentration")])
    isotopes = []
    used = []
    for isoId, iso in _isotope_list().items():
        if iso["isotope"] in names:
            isotopes.append(isoId)
            used.append(iso["isotope"])
    dropped = set(names).difference(used)
    if dropped:
        logging.warning(f"dropping unknown isotopes for grib: {', '.join(dropped)}")
    return isotopes


def _convert_to_grib(ncfile: str, ident: str, model: str = "SNAP"):
    dirname = os.path.dirname(os.path.abspath(ncfile))
    snapNc_convert_to_grib(
        os.path.abspath(ncfile),
        dirname,
        ident,
        _isotopes_in_file(ncfile),
        bitmapCompress=True,
    )
    grib_files = [f for f in os.listdir(dirname) if f.startswith(ident)]
    output_name = ident.replace("_SNAP", f"_{model}")
    # zip the output files
    output_zip = os.path.join(dirname, f"{output_name}2ARGOS.zip")
    with zipfile.ZipFile(output_zip, "w") as zipf:
        for file in grib_files:
            if file.startswith(output_name):
                zipf.write(os.path.join(dirname, file), arcname=file)

    output_status = os.path.join(dirname, f"{output_name}_status")
    with open(output_status, "w") as f:
        f.write(
            f"202:{datetime.datetime.now():%Y%m%d%H%M}:Finished extracting {model} data for ARGOS"
        )
    print("Please ssh results to dsa manually:")
    print(f"  scp {output_zip} {output_status} dsa@dsa.met.no:")


def main(argv=None):
    parser = argparse.ArgumentParser(
        prog="snapCombine",
        description="Combine several snap.nc files to one file by summing concentrations "
        "and depositions on a combined time-axis. Time-steps may be extrapolated, "
        "but not interpolated.",
    )
    parser.add_argument("-o", "--output", required=True, help="output nc file")
    parser.add_argument(
        "--grib",
        metavar="TAGNAME",
        help='convert the combined file also to grib, using TAGNAME as identifier, e.g. "hk_test_20261014"',
    )
    parser.add_argument(
        "--model",
        metavar="MODEL",
        help="specify the model to be used, e.g. SNAP, SNAPGLOBAL, SNAPNORDIC, SNAPICONGLOBAL",
        choices=["SNAP", "SNAPGLOBAL", "SNAPNORDIC", "SNAPICONGLOBAL"],
        default="SNAP",
    )
    parser.add_argument(
        "--recalc-toa",
        action="store_true",
        help="recalculate time_of_arrival and total depositions (as snapAddToa)",
    )
    parser.add_argument("--loglevel", default="WARNING", help="logging level")
    parser.add_argument("infiles", nargs="+", help="input snap.nc files")
    args = parser.parse_args(argv)
    logging.basicConfig(level=args.loglevel)

    try:
        snap_combine(args.infiles, args.output, args.grib, args.model, args.recalc_toa)
    except SnapCombineError as e:
        print(f"snapCombine: error: {e}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
