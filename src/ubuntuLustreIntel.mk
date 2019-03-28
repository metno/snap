# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = ifort
CXX = icc
CC  = icc

F77FLAGS=-O2 -cpp -qopenmp -warn all -nogen-interfaces
CXXFLAGS=-O2 -qopenmp
CCFLAGS=-O2 -qopenmp

LDFLAGS=-qopenmp

# NCDIR not required if /usr or /usr/local
NCDIR=$(shell nf-config --prefix)
NCINC=$(shell nf-config --fflags)
NCLIBS=$(shell nf-config --flibs)

MILIB = #-L/home/heikok/local/lib -lmi
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

