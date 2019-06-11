# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = gfortran
CXX = g++
CC  = gcc

#F77FLAGS=-O2 -ftree-vectorize -fno-math-errno -fopenmp
#      -ftree-vectorizer-verbose=2
#-ffpe-trap=invalid,zero,overflow
#F77FLAGS=-O2 -g -msse2 -ffpe-trap=invalid,zero,overflow
F77FLAGS=-DVERSION=\"$(VERSION)\" -O2 -g -mavx -ftree-vectorize -fno-math-errno -Wall -Wextra -fimplicit-none -fmodule-private
CXXFLAGS=-O2 -mavx -ftree-vectorize -fno-math-errno -Wall -Wextra
CCFLAGS=-O2 -mavx -ftree-vectorize -fno-math-errno -Wall -Wextra

LDFLAGS=

# NCDIR not required if /usr or /usr/local
#NCDIR = /modules/trusty/NETCDF/4.3.2/C/
NCDIR=$(shell nf-config --prefix)
NCINC=$(shell nf-config --fflags)
NCLIBS=$(shell nf-config --flibs)


MILIB = -L/home/heikok/local/lib -lmi
EXLIBS = -lpthread -ldl

##########################################################

BINDIR=/modules/xenial/user-apps/SnapPy/$(VERSION)/bin/

INCLUDES = -I. 


ifdef NCDIR
BLIBS += $(NCLIBS) -Wl,-rpath,$(NCDIR)/lib
INCLUDES += $(NCINC)
endif

LIBS= $(MILIB) $(EXLIBS)
BLIBS += $(MILIB) $(NCLIBS)

# clear out all suffixes
.SUFFIXES:

