ECMWF / IFS dr.hook fortran profiler
====================================


build:
------
tar xvf drhook_CY31R2.032.tar
cd drhook_CY31R2.032
./configure -a linuxgfortran_ubuntu
make ARCH=linuxgfortran_ubuntu drhook



usage:
------
when compiling, use:
  export DR_HOOK=1
  make install

when running a DR_HOOK compiled version of bsnap, set:
  export DR_HOOK=1
  export DR_HOOK_OPT=prof
  export DR_HOOK_NOT_MPI=1
  bsnap snap.input

watch at results with:
  less drhook.prof.1



more info:
----------
https://hirlam.org/trac/wiki/HarmonieSystemDocumentation/37h1.2/DrHook
http://www.ecmwf.int/services/computing/training/material_2009/hpcf/debugging.pdf

