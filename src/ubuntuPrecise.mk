# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = gfortran
CXX = g++
CC  = gcc

F77FLAGS=-O2 -g -msse2 -ffpe-trap=invalid,zero,overflow -fbounds-check -Wall -Wextra -fimplicit-none -fmodule-private
CXXFLAGS=-O3 -Wall -Wextra
CCFLAGS=-O3 -Wall -Wextra

LDFLAGS=

# NCDIR required even if /usr or /usr/local
NCDIR = /opt/netcdf-fortran-4.2


MIINC = -I/usr/include
# uncomment MILIB if felt-support not required
MILIB = -L/usr/lib -lmi
EXLIBS = -lpthread -ldl

##########################################################

BINDIR=../../bin/

INCLUDES = -I. $(MIINC)


ifdef NCDIR
BLIBS += -L$(NCDIR)/lib -Wl,-rpath,$(NCDIR)/lib
INCLUDES += -I$(NCDIR)/include
endif

LIBS= $(MILIB) $(EXLIBS)
BLIBS += $(MILIB) -lnetcdff


# clear out all suffixes
.SUFFIXES:

