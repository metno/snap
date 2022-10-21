# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = gfortran

F77DEBUGFLAGS=-ffpe-trap=invalid,zero,overflow -fcheck=bounds
F77FLAGS=-DVERSION=\"$(VERSION)\" -O2 -ftree-vectorize -fno-math-errno -g -mavx2 -mfma -fopt-info-optimized-vec -Wall -Wextra -fimplicit-none -fmodule-private -Wno-conversion
ifdef SNAP_DEBUG_CHECKS
  F77FLAGS+=$(F77DEBUGFLAGS)
endif

# optional fimex
FIMEXLD = $(shell pkg-config --libs-only-L fimex  | cut -c 3-)
FIMEXLIB = $(shell pkg-config --libs fimex)
FIMEXINC =

NCDIR=$(shell nf-config --prefix)
NETCDFLIB = $(shell nf-config --flibs)
NETCDFINC = $(shell nf-config --fflags)

MILIB_FLAGS = -fno-implicit-none -fno-module-private -Wno-all -Wno-extra

BINDIR?=/modules/bionic/user-apps/SnapPy/$(VERSION)/bin/

INCLUDES =

BLIBS += $(NETCDFLIB)
BLIBS += $(FIMEXLIB)

INCLUDES += $(NETCDFINC)
INCLUDES += $(FIMEXINC)

ifdef NCDIR
BLIBS += $(NCLIBS) -Wl,-rpath,$(NCDIR)/lib
INCLUDES += $(NCINC)
endif

LIBS= $(EXLIBS)
ifdef FIMEXLIB
BLIBS += $(FIMEXLIB) -Wl,-rpath,$(FIMEXLD) $(LDFLAGS)
endif


# clear out all suffixes
.SUFFIXES:

