# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = gfortran

F77FLAGS=-DVERSION=\"$(VERSION)\" -O2 -ftree-vectorize -fno-math-errno -g -mavx2 -mfma -Wall -Wextra -fimplicit-none -fmodule-private -Wno-conversion
ifdef SNAP_PARALLEL
    F77FLAGS+=-fopenmp
endif

CONDAPATH=/modules/rhel8/conda/install/envs/atom-fimex/

FIMEXLIB = -L${CONDAPATH}/lib -lfimex -Xlinker -rpath=${CONDAPATH}/lib
FIMEXINC =

NETCDFLIB = -L${CONDAPATH}/lib -lnetcdff -lnetcdf
NETCDFINC = -I${CONDAPATH}/include -I${CONDAPATH}/include

MILIB_FLAGS = -fno-implicit-none -fno-module-private -Wno-all -Wno-extra

BINDIR=../../bin/

INCLUDES =

BLIBS += $(NETCDFLIB)
BLIBS += $(FIMEXLIB)

INCLUDES += $(NETCDFINC)
INCLUDES += $(FIMEXINC)

.SUFFIXES:

