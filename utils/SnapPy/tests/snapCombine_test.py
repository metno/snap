import datetime
import math

import netCDF4
import numpy as np
import pytest
from Snappy.SnapCombine import (
    SnapCombineError,
    _decay_rates,
    combine_times,
    main,
    snap_combine,
)

NX, NY = 4, 3
BASE = datetime.datetime(2026, 10, 12, 0)


def _hours(*hs):
    return [BASE + datetime.timedelta(hours=h) for h in hs]


def _make_snap(path, hours, comps=("Cs137",), value=1.0, nx=NX, ref_hour=0):
    with netCDF4.Dataset(path, "w") as nc:
        nc.createDimension("time", None)
        nc.createDimension("x", nx)
        nc.createDimension("y", NY)
        nc.createDimension("nocomp", len(comps))
        nc.createDimension("compnamelenmax", 32)
        nc.title = f"test {path.name}"
        nc.createVariable("x", "f4", ("x",))[:] = np.arange(nx)
        nc.createVariable("y", "f4", ("y",))[:] = np.arange(NY)
        t = nc.createVariable("time", "f4", ("time",))
        ref = BASE + datetime.timedelta(hours=ref_hour)
        t.units = f"hours since {ref:%Y-%m-%d %H:%M:%S} +0000"
        t[:] = np.array(hours) - ref_hour
        nc.createVariable("components", "S1", ("nocomp", "compnamelenmax"))[:] = (
            netCDF4.stringtochar(np.array(comps, dtype="S32"), encoding="ascii")
        )
        dims = ("time", "y", "x")
        nt = len(hours)
        steps = np.arange(1, nt + 1, dtype="f4")[:, None, None] * np.ones((NY, nx))
        nc.createVariable("lwe_precipitation_rate", "f4", dims)[:] = (
            steps * 0 + value * 100
        )
        for c in comps:
            nc.createVariable(f"{c}_concentration", "f4", dims)[:] = value
            nc.createVariable(f"{c}_concentration_bl", "f4", dims)[:] = value
            nc.createVariable(f"{c}_acc_concentration", "f4", dims)[:] = steps * value
            nc.createVariable(f"{c}_acc_wet_deposition", "f4", dims)[:] = steps * value
            nc.createVariable(f"{c}_acc_dry_deposition", "f4", dims)[:] = value
        for v in nc.variables.values():
            if v.dimensions == dims:
                v.units = "1"
        toa = nc.createVariable("time_of_arrival", "f4", dims, fill_value=-999)
        toa[:] = 1234.0
        nc.createVariable(f"{comps[0]}_acc_total_deposition", "f4", dims)[:] = 1234.0
    return path


def test_combine_times_extrapolate():
    times, dt = combine_times([_hours(0, 3, 6), _hours(3, 6, 9)])
    assert dt == datetime.timedelta(hours=3)
    assert times == _hours(0, 3, 6, 9)


def test_combine_times_gap():
    times, _ = combine_times([_hours(0, 3), _hours(9, 12)])
    assert times == _hours(0, 3, 6, 9, 12)


def test_combine_times_interpolation_error():
    with pytest.raises(SnapCombineError):
        combine_times([_hours(0, 3, 6), _hours(1, 4, 7)])


def test_combine_times_different_dt_error():
    with pytest.raises(SnapCombineError):
        combine_times([_hours(0, 3, 6), _hours(0, 6, 12)])


def test_snap_combine(tmp_path):
    # different reference times in the time-units
    f1 = _make_snap(tmp_path / "a.nc", [3, 6, 9], ("Cs137", "I131"), value=1.0)
    f2 = _make_snap(tmp_path / "b.nc", [6, 9, 12, 15], ("Cs137",), 2.0, ref_hour=3)
    out = tmp_path / "out.nc"
    snap_combine([str(f1), str(f2)], str(out), recalc_toa=True)

    decay = _decay_rates()
    with netCDF4.Dataset(out) as nc:
        times = netCDF4.num2date(nc["time"][:], nc["time"].units)
        assert [t.hour for t in times] == [3, 6, 9, 12, 15]
        comps = netCDF4.chartostring(nc["components"][:]).tolist()
        assert comps == ["Cs137", "I131"]

        conc = nc["Cs137_concentration"][:, 0, 0]
        np.testing.assert_allclose(conc, [1, 3, 3, 2, 2])
        np.testing.assert_allclose(
            nc["Cs137_concentration_bl"][:, 0, 0], [1, 3, 3, 2, 2]
        )
        # isotope only in first file, 0 after end
        np.testing.assert_allclose(nc["I131_concentration"][:, 0, 0], [1, 1, 1, 0, 0])

        # acc concentration: last value kept, no decay
        np.testing.assert_allclose(
            nc["I131_acc_concentration"][:, 0, 0], [1, 2, 3, 3, 3]
        )
        # acc deposition: last value decayed
        rate = decay["I131"]
        exp = [1, 2, 3] + [3 * math.exp(-rate * h * 3600) for h in (3, 6)]
        np.testing.assert_allclose(
            nc["I131_acc_wet_deposition"][:, 0, 0], exp, rtol=1e-6
        )
        rate = decay["Cs137"]
        exp = np.array([1, 2, 3, 3, 3]) * np.array(
            [1, 1, 1, math.exp(-rate * 3 * 3600), math.exp(-rate * 6 * 3600)]
        ) + np.array([0, 2, 4, 6, 8])
        np.testing.assert_allclose(
            nc["Cs137_acc_wet_deposition"][:, 0, 0], exp, rtol=1e-6
        )
        # acc total deposition recalculated by AddToa
        np.testing.assert_allclose(
            nc["Cs137_acc_total_deposition"][:, 0, 0],
            exp + nc["Cs137_acc_dry_deposition"][:, 0, 0],
            rtol=1e-6,
        )

        # other variables from any file
        np.testing.assert_allclose(
            nc["lwe_precipitation_rate"][:, 0, 0], [100, 100, 100, 200, 200]
        )
        # time_of_arrival recalculated
        assert nc["time_of_arrival"][0, 0, 0] < 1000


def test_snap_combine_interpolation_error(tmp_path):
    f1 = _make_snap(tmp_path / "a.nc", [0, 3, 6])
    f2 = _make_snap(tmp_path / "b.nc", [1, 4, 7])
    out = tmp_path / "out.nc"
    with pytest.raises(SnapCombineError):
        snap_combine([str(f1), str(f2)], str(out))
    assert not out.exists()
    assert main(["-o", str(out), str(f1), str(f2)]) == 1


def test_snap_combine_grid_error(tmp_path):
    f1 = _make_snap(tmp_path / "a.nc", [0, 3, 6])
    f2 = _make_snap(tmp_path / "b.nc", [3, 6, 9], nx=NX + 1)
    out = tmp_path / "out.nc"
    with pytest.raises(SnapCombineError):
        snap_combine([str(f1), str(f2)], str(out))
    assert not out.exists()


def test_main(tmp_path):
    f1 = _make_snap(tmp_path / "a.nc", [3, 6, 9])
    f2 = _make_snap(tmp_path / "b.nc", [9, 12])
    out = tmp_path / "out.nc"
    assert main(["-o", str(out), str(f1), str(f2)]) == 0
    with netCDF4.Dataset(out) as nc:
        assert len(nc["time"]) == 4
        # dropped without --recalc-toa
        assert "time_of_arrival" not in nc.variables
        assert "Cs137_acc_total_deposition" not in nc.variables
    assert main(["--recalc-toa", "-o", str(out), str(f1), str(f2)]) == 0
    with netCDF4.Dataset(out) as nc:
        assert "time_of_arrival" in nc.variables
        assert "Cs137_acc_total_deposition" in nc.variables
