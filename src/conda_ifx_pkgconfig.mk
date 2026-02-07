# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = ifx

F77FPEFLAGS= -ffpe-trap=invalid,zero,overflow -fno-openmp
F77BOUNDFLAGS= -fbounds-check -fno-openmp
#F77FLAGS=-DVERSION=\"$(VERSION)\" -O2 -ftree-vectorize -fno-math-errno -fopenmp -g -mavx2 -mfma -Wall -Wextra -fimplicit-none -fmodule-private -Wno-conversion -Wno-compare-reals
F77FLAGS=-DVERSION=\"$(VERSION)\" -O2 -fopenmp -fiopenmp -xCORE-AVX2 -ipo $(MILIB_FLAGS) # -ipo is much faster but difficult to optimize
 # -fopenmp-targets=spir64 # for GPU offloading, but not yet working
ifdef SNAP_DEBUG_CHECKS
  F77FLAGS+=$(F77BOUNDFLAGS) $(F77FPEFLAGS)
endif
ifdef SNAP_BOUND_CHECKS
  F77FLAGS+=$(F77BOUNDFLAGS)
endif
ifdef SNAP_DISABLE_OMP
  F77FLAGS+=-fno-openmp
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

MILIB_FLAGS = #-fno-implicit-none -fno-module-private -Wno-all -Wno-extra

BINDIR?=../../bin/

INCLUDES =

BLIBS += $(NETCDFLIB)
BLIBS += $(FIMEXLIB)

INCLUDES += $(NETCDFINC)
INCLUDES += $(FIMEXINC)

.SUFFIXES:

