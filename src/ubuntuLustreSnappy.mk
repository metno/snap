# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = gfortran
CXX = g++
CC  = gcc

#F77FLAGS=-O2 -ftree-vectorize -fno-math-errno -fopenmp
#      -ftree-vectorizer-verbose=2
#-ffpe-trap=invalid,zero,overflow
#F77FLAGS=-O2 -g -msse2 -ffpe-trap=invalid,zero,overflow
F77FLAGS=-DVERSION=\"$(VERSION)\" -O2 -g -mavx -ftree-vectorize -fno-math-errno -Wall -Wextra -fimplicit-none -fmodule-private -Wno-conversion
CXXFLAGS=-O2 -mavx -ftree-vectorize -fno-math-errno -Wall -Wextra
CCFLAGS=-O2 -mavx -ftree-vectorize -fno-math-errno -Wall -Wextra

# some additional flags for fimex
LDFLAGS=-Wl,-rpath,/modules/xenial/user-apps/mi-programoptions/0.1.0/lib/ -Wl,-rpath,/modules/xenial/user-apps/eccodes/2.12.0/lib/ -Wl,-rpath,/modules/xenial/hdf5/1.8.17/lib

# NCDIR not required if /usr or /usr/local
#NCDIR = /modules/trusty/NETCDF/4.3.2/C/
NCDIR=$(shell nf-config --prefix)
NCINC=$(shell nf-config --fflags)
NCLIBS=$(shell nf-config --flibs)

# optional fimex
# pkg-config --cflags fimex
# pkg-config --libs fimex
#FIMEXINC = -I/usr/include/fimex-1.4 # included fimex.f90, currently not needed
FIMEXLD = $(shell pkg-config --libs-only-L fimex  | cut -c 3-)
FIMEXLIB = $(shell pkg-config --libs fimex)

MIINC = -I/usr/include
MILIB_FLAGS = -fno-implicit-none -fno-module-private -Wno-all -Wno-extra
EXLIBS = -lpthread -ldl

##########################################################

BINDIR ?= /modules/xenial/user-apps/SnapPy/$(VERSION)/bin/

INCLUDES = -I. $(MIINC) $(FIMEXINC)


ifdef NCDIR
BLIBS += $(NCLIBS) -Wl,-rpath,$(NCDIR)/lib
INCLUDES += $(NCINC)
endif

LIBS= $(MILIB) $(EXLIBS)
ifdef FIMEXLIB
BLIBS += $(MILIB) $(FIMEXLIB) -Wl,-rpath,$(FIMEXLD) $(LDFLAGS)
endif


# clear out all suffixes
.SUFFIXES:

