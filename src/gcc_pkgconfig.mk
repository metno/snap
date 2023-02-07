# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = gfortran

F77DEBUGFLAGS= -ffpe-trap=invalid,zero,overflow -fbounds-check
F77FLAGS=-DVERSION=\"$(VERSION)\" -O2 -ftree-vectorize -fno-math-errno -g -mavx2 -mfma -Wall -Wextra -fimplicit-none -fmodule-private -Wno-conversion
ifdef SNAP_DEBUG_CHECKS
  F77FLAGS+=$(F77DEBUGFLAGS)
endif

# optional versioned fimex
FIMEX = fimex
ifdef SNAP_FIMEX_VERSION
  FIMEX := fimex-$(SNAP_FIMEX_VERSION)
endif
FIMEXLIB = $(shell pkg-config --libs $(FIMEX))
FIMEXINC = $(shell pkg-config --cflags-only-I $(FIMEX))
FIMEXINC =

NETCDFLIB = $(shell nf-config --flibs)
NETCDFINC = $(shell nf-config --fflags)

MILIB_FLAGS = -fno-implicit-none -fno-module-private -Wno-all -Wno-extra

BINDIR?=../../bin/

INCLUDES =

BLIBS += $(NETCDFLIB)
BLIBS += $(FIMEXLIB)

INCLUDES += $(NETCDFINC)
INCLUDES += $(FIMEXINC)

.SUFFIXES:

