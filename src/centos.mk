# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = gfortran

F77FLAGS=-DVERSION=\"$(VERSION)\" -O2 -ftree-vectorize -fno-math-errno -g -mavx2 -mfma -Wall -Wextra -fimplicit-none -fmodule-private -Wno-conversion

FIMEXLIB = -L/modules/centos7/conda/Feb2021/envs/production-fimex-1.6.9/lib -lfimex -Xlinker -rpath=/modules/centos7/conda/Feb2021/envs/production-fimex-1.6.9/lib
FIMEXINC =

NETCDFLIB = -L/modules/centos7/conda/Feb2021/envs/production-fimex-1.6.9/lib -lnetcdff -lnetcdf
NETCDFINC = -I/modules/centos7/conda/Feb2021/envs/production-fimex-1.6.9/include -I/modules/centos7/conda/Feb2021/envs/production-fimex-1.6.9/include

MILIB_FLAGS = -fno-implicit-none -fno-module-private -Wno-all -Wno-extra

BINDIR=../../bin/

INCLUDES =

BLIBS += $(NETCDFLIB)
BLIBS += $(FIMEXLIB)

INCLUDES += $(NETCDFINC)
INCLUDES += $(FIMEXINC)

.SUFFIXES:

