# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = gfortran

F77FLAGS=-DVERSION=\"$(VERSION)\" -O2 -ftree-vectorize -fno-math-errno -g -mavx2 -mfma -Wall -Wextra -fimplicit-none -fmodule-private -Wno-conversion

FIMEXLIB = -L/modules/rhel8/conda/install/envs/atom-fimex/lib -lfimex -Xlinker -rpath=/modules/rhel8/conda/install/envs/atom-fimex/lib
FIMEXINC =

NETCDFLIB = -L/modules/rhel8/conda/install/envs/atom-fimex/lib -lnetcdff
NETCDFINC = -I/modules/rhel8/conda/install/envs/atom-fimex/include -I/modules/rhel8/conda/install/envs/atom-fimex/include

MILIB_FLAGS = -fno-implicit-none -fno-module-private -Wno-all -Wno-extra

BINDIR=../../bin/

INCLUDES =

BLIBS += $(NETCDFLIB)
BLIBS += $(FIMEXLIB)

INCLUDES += $(NETCDFINC)
INCLUDES += $(FIMEXINC)

.SUFFIXES:

