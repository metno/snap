# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = gfortran
CXX = g++
CC  = gcc

F77FLAGS=-O2 -g -mavx -cpp -ftree-vectorize -fno-math-errno
CXXFLAGS=-O2 -mavx -ftree-vectorize -fno-math-errno
CCFLAGS=-O2 -g -mavx -ftree-vectorize -fno-math-errno

LDFLAGS=

# NCDIR required even if /usr or /usr/local
NCDIR = /usr


MIINC = -I/usr/include
# uncomment MILIB if felt-support not required
MILIB = -L/usr/lib -lmi
EXLIBS = -lpthread -ldl

##########################################################

BINDIR=../debian/bsnap/usr/bin/

INCLUDES = -I. $(MIINC)


ifdef NCDIR
BLIBS += -L$(NCDIR)/lib -Wl,-rpath,$(NCDIR)/lib
INCLUDES += -I$(NCDIR)/include
endif

LIBS= $(MILIB) $(EXLIBS)
BLIBS += $(MILIB) -lnetcdff

# clear out all suffixes
.SUFFIXES:

