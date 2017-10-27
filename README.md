# SNAP

SNAP, the Severe Nuclear Accident Programme is a lagrangian type
atmospheric dispersion model specialized on modelling dispersion
of radioactive debris.


## Meteorological input fields

SNAP needs meteorological driver data from NWP models in sigma or
eta-hybrid model-levels, converted to netcdf format. The minimum
list of parameters are in surface layer:

  * surface-air-pressure
  * precipitation (eventually split into convective and large-scale)
  * x- and y-wind-10m

And in model layers:
  
  * x- and y-wind
  * air-temperature or potential-temperature
  * ap and b hybrid level values, or sigma level values

Parameter names can be specified in src/common/readfields_nc.f.


## Dependencies

SNAP requires at least the following libraries to be installed for
compilation

 * fortran77/90 compiler, e.g. gfortran or ifort
 * NetCDF (netcdf > 4.1.1)
 * NetCDF-fortran



## Installation

Create a file `current.mk` in the `src` directory. Use e.g the file `ubuntuXenail.mk`
as template. Most important are the NCDIR and 
the BINDIR where final files will be installed.
THE MIINC and MILIB should be uncommented.

In the `src` directory run then:

    make install

This will install `bsnap_naccident` in the `BINDIR`. To run SNAP use
the command 

    bsnap_naccident snap.input

`snap.input` examples can be found in the directory `src/naccident/examples/`


## License

SNAP is licensed under the GPL. Please see COPYING for more information.


