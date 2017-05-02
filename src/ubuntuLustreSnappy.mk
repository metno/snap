# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = gfortran
CXX = g++
CC  = gcc

#F77FLAGS=-O2 -ftree-vectorize -fno-math-errno -fopenmp -cpp
#      -ftree-vectorizer-verbose=2
#-ffpe-trap=invalid,zero,overflow
#F77FLAGS=-O2 -g -msse2 -cpp -ffpe-trap=invalid,zero,overflow
F77FLAGS=-O2 -g -mavx -cpp -ftree-vectorize -fno-math-errno
CXXFLAGS=-O2 -mavx -ftree-vectorize -fno-math-errno
CCFLAGS=-O2 -mavx -ftree-vectorize -fno-math-errno

LDFLAGS=

# NCDIR not required if /usr or /usr/local
NCDIR = /modules/trusty/NETCDF/4.3.2/C/


MIINC = -I/home/heikok/local/include
MILIB = -L/home/heikok/local/lib -lmi
EXLIBS = -lpthread -ldl

DRHOOKINC = -I../../utils/drhook_CY31R2.032
DRHOOKLIB = -L../../utils/drhook_CY31R2.032 -ldrhook -lmpi_serial

##########################################################

BINDIR=/modules/trusty/user-apps/SnapPy/1.0.0/bin/

INCLUDES = -I. $(MIINC)


ifdef NCDIR
BLIBS += -L$(NCDIR)/lib -Wl,-rpath,$(NCDIR)/lib
INCLUDES += -I$(NCDIR)/include
endif

LIBS= $(MILIB) $(EXLIBS)
BLIBS += $(MILIB) -lnetcdff

ifdef DR_HOOK
$(info DR_HOOK defined)
F77FLAGS += -DDRHOOK $(DRHOOKINC)
BLIBS += $(DRHOOKLIB)
endif

# clear out all suffixes
.SUFFIXES:

