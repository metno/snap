# SNAP

SNAP, the Severe Nuclear Accident Programme is a lagrangian type
atmospheric dispersion model specialized on modelling dispersion
of radioactive debris. A model description can be found at 
https://drive.google.com/open?id=0B8SjSRklVkHkQXoxY1VQdE0wdnM


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

### Using CMake

Create a build folder to create an out-of-tree build
```
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Debug/RelWithDebInfo/Release -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_Fortran_COMPILER=gfortran
make -j
make install
```

To see invocations of make, use
```
env VERBOSE=1 make
```

#### Build-types
The debug option will include some extra options such as fault trapping and
bounds checking, and is meant for quick test-change-compile cycles. The
release options includes a set of optimisation flags to increase
the efficiency of the program. These should be tested on a machine basis to
ensure optimal performance of the resulting program.


#### Installing cmake (bootstrapping)
The version of cmake required by SNAP (3.5.1) is for some systems higher than what
is currently installed. This section describes how to create a more up to date
version of cmake that can build SNAP.

Download an archive of cmake from the [cmake downloads page](https://cmake.org/download/)
unpack the archive using the command
```
tar -xf cmake-M.M.P.tar.gz
```
Enter the newly created directory and start the build
```
cd cmake-M.M.P
module load gcc/6.2.0 # For certain systems
./bootstrap --prefix=???
make -j
make install
```

### Legacy

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

```
SNAP: Servere Nuclear Accident Programme
Copyright (C) 1992-2017   Norwegian Meteorological Institute

SNAP is free software: you can 
redistribute it and/or modify it under the terms of the 
GNU General Public License as published by the 
Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program, i.e. see COPYING for more information.
If not, see <https://www.gnu.org/licenses/>.
```
