# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = gfortran
CXX = g++
CC  = gcc

F77FLAGS=-O3 -cpp
CXXFLAGS=-O3
CCFLAGS=-O3

LDFLAGS=

# NCDIR required even if /usr or /usr/local
NCDIR = /opt/netcdf-fortran-4.2


MIINC = -I/usr/include
# uncomment MILIB if felt-support not required
#MILIB = -L/usr/lib -lmi
EXLIBS = -lpthread -ldl

DRHOOKINC = -I../../utils/drhook_CY31R2.032
DRHOOKLIB = -L../../utils/drhook_CY31R2.032 -ldrhook -lmpi_serial

##########################################################

BINDIR=../../bin/

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

