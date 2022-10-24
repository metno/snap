# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = ifort
CXX = icc
CC  = icc

F77FLAGS=-DVERSION=\"$(VERSION)\" -O2 -qopenmp -warn all
CXXFLAGS=-O2 -qopenmp
CCFLAGS=-O2 -qopenmp

LDFLAGS=-qopenmp

# NCDIR not required if /usr or /usr/local
NCDIR=$(shell nf-config --prefix)
NCINC=$(shell nf-config --fflags)
NCLIBS=$(shell nf-config --flibs)

EXLIBS = -lpthread -ldl

##########################################################

BINDIR?=../../bin/

INCLUDES = -I.


ifdef NCDIR
BLIBS += $(NCLIBS) -Wl,-rpath,$(NCDIR)/lib
INCLUDES += $(NCINC)
endif

# optional versioned fimex
FIMEX = fimex
ifdef SNAP_FIMEX_VERSION
  FIMEX := fimex-$(SNAP_FIMEX_VERSION)
endif
FIMEXLIB = $(shell pkg-config --libs $(FIMEX))
FIMEXINC =

LIBS= $(MILIB) $(EXLIBS)
BLIBS += $(MILIB) $(NCLIBS)
BLIBS += $(FIMEXLIB)


# clear out all suffixes
.SUFFIXES:

