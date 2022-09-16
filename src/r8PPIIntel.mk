# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

# requires `module load netcdf-fortran-intel/4.6.0`

F77 = ifort
CXX = icc
CC  = icc

# OMP: -qopenmp
# -heap-arrays
F77FLAGS=-DVERSION=\"$(VERSION)\" -O2 -mavx -g -debug -warn all
CXXFLAGS=-O2 -g
CCFLAGS=-O2 -g

LDFLAGS=-qopenmp

# NCDIR not required if /usr or /usr/local
NCDIR=$(shell nf-config --prefix)
NCINC=$(shell nf-config --fflags)
#NCLIBS=-L/modules/rhel8/user-apps/fou-modules/netcdf/netcdf_4.9.0/lib64 -L/modules/rhel8/user-apps/fou-modules/netcdff/netcdf-fortran_intel_4.6.0/lib64 -lnetcdff -lnetcdf -lnetcdff -Wl,-rpath,/modules/rhel8/user-apps/fou-modules/netcdf/netcdf_4.9.0/lib64 -Wl,-rpath,/modules/rhel8/user-apps/fou-modules/netcdff/netcdf-fortran_intel_4.6.0/lib64
NCLIBS=$(shell nf-config --flibs)

EXLIBS = -lpthread -ldl

##########################################################

BINDIR=../../bin/

INCLUDES = -I.


ifdef NCDIR
BLIBS += $(NCLIBS) -Wl,-rpath,$(NCDIR)/lib
INCLUDES += $(NCINC)
endif

LIBS= $(MILIB) $(EXLIBS)
BLIBS += $(MILIB) $(NCLIBS)


# clear out all suffixes
.SUFFIXES:

