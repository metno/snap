# link this to current.mk and it will be used in the Makefiles in subdirectories
# includefile contains Compiler definitions etc.

F77 = gfortran

F77FLAGS=-DVERSION=\"$(VERSION)\" -Og -g -std=gnu -Wall -Wextra -fimplicit-none -fmodule-private -Wuse-without-only -fcheck=all -Wno-conversion -Wno-compare-reals -Wrealloc-lhs
MILIB_FLAGS = -fno-implicit-none -fno-module-private -Wno-all -Wno-extra

BINDIR=../../bin/
INCLUDES=$(shell nf-config --fflags)

BLIBS= $(shell nf-config --flibs)

.SUFFIXES:
