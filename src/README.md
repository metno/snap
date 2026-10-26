# SNAP Fortran code

## Installation

Create a file `current.mk` in this directory. Use for example the file
[gcc_pkgconfig.mk](src/gcc_pkgconfig.mk)
as a template. The most important parameter to modify is `BINDIR`, but `FIMEXLIB` and `NETCDFLIB`
might also have to be modified depending on your development setup.

And run:

```sh
make install
```

This will install `bsnap_naccident` to `BINDIR`. Run SNAP using
the command

```sh
bsnap_naccident snap.input
```

Examples of `snap.input` can be found in the directory [src/naccident/examples/](src/naccident/examples).

## Start times and time arithmetic

`TIME.START` accepts four to six integers in UTC:

```text
TIME.START=2026 10 25 12
TIME.START=2026 10 25 12 30
TIME.START=2026 10 25 12 31 30
```

Omitted minutes and seconds default to
zero. Minutes and seconds must be in the range 0-59.
The start must align with the effective model timestep, which SNAP
adjusts to divide an hour. For example, a 900-second timestep accepts `12:30:00`
but rejects `12:30:15`. Meteorology read boundaries must also align; incompatible
timestamps cause an explicit error rather than rounding. Meteorology file
timestamps remain hourly.

Run/release durations and output intervals are still configured in hours.
Asynoptic outputs are relative to the start; synoptic outputs remain on
meteorology-relative hour boundaries. Logs, source-term epoch headers and NetCDF
time origins retain the actual minutes and seconds. NetCDF times remain in
`hours since ...`, including fractional hours where needed.

`STEP.HOUR.OUTPUT.FIELDS` sets the field-output interval in hours and may be
fractional. The requested interval is rounded to the nearest effective model
timestep. With `ASYNOPTIC.OUTPUT`, output repeats at that fixed timestep
multiple, relative to `TIME.START`. With `SYNOPTIC.OUTPUT`, output remains
aligned to whole-hour meteorology times; the rounded interval is converted to
the nearest whole hour, with a minimum interval of one hour. For example, with
a 15-minute model timestep, `STEP.HOUR.OUTPUT.FIELDS=0.3` gives 15-minute
asynoptic output and hourly synoptic output.

The Fortran [datetime module](src/common/datetime.f90) provides
`datetime_t(year, month, day, hour, minute, second)` with optional zero-defaulted
minute/second components. Durations store signed 64-bit seconds and can be
constructed as `duration_t(hours=24)`, `duration_t(seconds=90)`, or
`duration_t(hours=1, seconds=30)`. Positional duration arguments now mean
**seconds**, not hours; external callers must migrate accordingly. Read elapsed
time from `%seconds` rather than the removed `%hours` component.
