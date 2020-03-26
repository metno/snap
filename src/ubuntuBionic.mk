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
#F77FLAGS=-O2 -g -mavx -ffpe-trap=invalid,zero,overflow -fbounds-check -Wall
#-DPETTERSEN
# -ffpe-trap=invalid,zero,overflow no longer usable, conflict/bug in gfortran with IEEE_VALUE(IEEE_QUIET_NAN)
F77FLAGS=-DVERSION=\"$(VERSION)\" -O2 -ftree-vectorize -fno-math-errno -g -mavx -fopt-info-optimized-vec -fopenmp $(PROFILE) -Wall -Wextra -fimplicit-none -fmodule-private -std=gnu -Wuse-without-only
CXXFLAGS=-O3 $(PROFILE) -Wall -Wextra
CCFLAGS=-O3 -g $(PROFILE) -Wall -Wextra

LDFLAGS=-fopenmp $(PROFILE)

# NCDIR required even if /usr or /usr/local
NCDIR = /usr

# optional fimex
# pkg-config --cflags fimex
# pkg-config --libs fimex
#FIMEXINC = -I/usr/include/fimex-1.4 # included fimex.f90
FIMEXLIB = -lfimex-1.4 # -lfimexf-1.4 -lfimex-1.4

MIINC = -I/usr/include
MILIB_FLAGS = -fno-implicit-none -fno-module-private -Wno-all -Wno-extra
# uncomment MILIB if felt-support not required
#MILIB = -L/usr/lib -lmi
EXLIBS = -lpthread -ldl

##########################################################

BINDIR=../../bin/

INCLUDES = -I. $(MIINC) $(FIMEXINC)


ifdef NCDIR
BLIBS += -L$(NCDIR)/lib -Wl,-rpath,$(NCDIR)/lib
INCLUDES += -I$(NCDIR)/include
endif

LIBS= $(MILIB) $(EXLIBS)
BLIBS += $(MILIB) -lnetcdff $(FIMEXLIB)


# clear out all suffixes
.SUFFIXES:
