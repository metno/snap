# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = gfortran
CXX = g++
CC  = gcc

# PROFILES not working for gcc 5.4 and fortran MODULES
#PROFILE=-fprofile-generate=/tmp/snapprof.dat
#PROFILE=-fprofile-use=/tmp/snapprof.dat -Wcoverage-mismatch
#
#PROFILE=-flto # this is a few % slower with gcc 5.4
#F77FLAGS=-O2 -g -mavx -cpp -ffpe-trap=invalid,zero,overflow -fbounds-check -Wall
F77FLAGS=-O2 -ftree-vectorize -fno-math-errno -ffpe-trap=invalid,zero,overflow -g -mavx -cpp -fopt-info-optimized-vec -fopenmp $(PROFILE) -DPETTERSEN
CXXFLAGS=-O3 $(PROFILE)
CCFLAGS=-O3 -g $(PROFILE)

LDFLAGS=-fopenmp $(PROFILE)

# NCDIR required even if /usr or /usr/local
NCDIR = /usr


MIINC = -I/usr/include
# uncomment MILIB if felt-support not required
MILIB = -L/usr/lib -lmi
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

