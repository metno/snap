# SNAP

SNAP, the Severe Nuclear Accident Programme is a lagrangian type
atmospheric dispersion model specialized on modelling dispersion
of radioactive debris. A model description can be found at
[this link](https://drive.google.com/file/d/0B8SjSRklVkHkQXoxY1VQdE0wdnM/view?usp=sharing&resourcekey=0-BBP4nQlukt1M66uNzJz1BA).


## Meteorological input fields

SNAP needs meteorological driver data from NWP models in sigma or
eta-hybrid model-levels, in the netcdf format. The minimum
list of parameters are for the surface layer:

  * surface-air-pressure
  * precipitation (eventually split into convective and large-scale)
  * x- and y-wind-10m

And for the model layers:

  * x- and y-wind
  * air-temperature or potential-temperature
  * ap and b hybrid level values, or sigma level values

Parameter names can be specified in [readfield_nc.f90](src/common/readfield_nc.f90).

And example on how to set up downloading of freely available meteorological data
from the NOAA GFS model can be found under [src/naccident/examples/gfs/](./src/naccident/examples/gfs/)


## Dependencies

SNAP requires the following libraries and programs to be installed for
compilation

 * fortran77/90 compiler, e.g. gfortran or ifort
 * NetCDF (netcdf > 4.1.1)
 * NetCDF-fortran
 * Python3 (optional)
 * fimex (optional)

To work with this repository, please have `git-lfs` installed.


## Installation

Create a file `current.mk` in the `src` directory. Use for example the file
[gcc_pkgconfig.mk](src/gcc_pkgconfig.mk)
as a template. The most important parameter to modify is `BINDIR`, but `FIMEXLIB` and `NETCDFLIB` might also have to be modified depending on your development setup.

In the `src` directory run:

```sh
make install
```

This will install `bsnap_naccident` to `BINDIR`. Run SNAP using
the command

```sh
bsnap_naccident snap.input
```

Examples of `snap.input` can be found in the directory [src/naccident/examples/](src/naccident/examples).

### Versioning

The master branch in git is used for development. Stable versions are tagged as 'vX.YY.ZZ'. Releases should also have a DOI for citation, see https://doi.org/10.5281/zenodo.1155159 . For the user-interface snappy, we use tags like 'snappy-vX.YY.ZZ' with independent version numbers. Other tags are used internally.


The build system uses automatic versioning based on git tags and revision numbers and embeds this into the resulting program. If git or python3 is unavailable, this logic should be bypassed by setting the environment variable VERSION to some value, e.g.
```sh
env VERSION="some_version_number" make install
```


## License

```
SNAP: Servere Nuclear Accident Programme
Copyright (C) 1992-2023  Norwegian Meteorological Institute

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
