# includefile contains Compiler definitions etc.

F77= gfortran
CXX= g++
CC= gcc

F77FLAGS=-O3 -mcmodel=medium
CXXFLAGS=-O3
CCFLAGS=-O3

LDFLAGS=

DEFS= -D_STANDARD_C_PLUS_PLUS

MIINC = -I/usr/include
MILIB = -L/usr/lib -lmi
EXLIBS = -lpthread -ldl

USRLOCAL=/usr

##########################################################

BINDIR=../../bin

INCLUDES = -I. $(MIINC)

LIBS= $(MILIB) $(EXLIBS)
BLIBS= $(MILIB)

.SUFFIXES: .cc .c .f .F
.f.o:
	$(F77) -c $(F77FLAGS) -I. $<
.F.o:
	$(F77) -c $(F77FLAGS) -I. $<
.c.o:
	$(CC)  -c $(CCFLAGS) -I. $<
.cc.o:
	$(CXX) -c $(CXXFLAGS) $(DEFS) $(INCLUDES) $<

MODELOBJ= argoswrite.o bldp.o compheight.o copyfield.o decay.o drydep1.o drydep2.o edcomp.o \
ensemble.o epinterp.o filesort.o fldout.o forwrd.o ftest.o om2edot.o posint.o \
pselect.o readfd.o readfield.o release.o rmpart.o rwalk.o tabcon.o \
vgravtables.o videosave.o wetdep1.o wetdep2.o

BOBJ = snap_batch_copy.o

INCFILES = snapdim.inc snapfil.inc snapfld.inc snapgrd.inc snappar.inc \
snappos.inc snaptab.inc snapgrp.inc snapeps.inc snapargos.inc snapdebug.inc

all: bsnap_nbomb create_bomb_input

bsnap_nbomb: $(BOBJ) $(MODELOBJ) milib.o c2fgetvar.o c2fgetarg.o
	$(F77) $(F77FLAGS) $(BOBJ) $(MODELOBJ) $(BLIBS) -o $@

create_bomb_input:
	$(F77) create_bomb_input.f -o $@

install: bsnap_nbomb create_bomb_input
	cp $^ $(BINDIR)

#--------------------------------

snap_batch_copy.o: snap.F $(INCFILES)
	cp -p snap.F snap_batch_copy.F
	$(F77) -c $(F77FLAGS) -I. -DBATCH snap_batch_copy.F

snap.o:        $(INCFILES)
argoswrite.o:  $(INCFILES)
bldp.o:        $(INCFILES)
compheight.o:  $(INCFILES)
copyfield.o:   $(INCFILES)
decay.o:       $(INCFILES)
drydep1.o:     $(INCFILES)
drydep2.o:     $(INCFILES)
edcomp.o:      $(INCFILES)
ensemble.o:    $(INCFILES)
epinterp.o:    $(INCFILES)
filesort.o:    $(INCFILES)
fldout.o:      $(INCFILES)
forwrd.o:      $(INCFILES)
ftest.o:       $(INCFILES)
om2edot.o:     $(INCFILES)
posint.o:      $(INCFILES)
pselect.o:     $(INCFILES)
readfd.o:      $(INCFILES)
readfield.o:   $(INCFILES)
release.o:     $(INCFILES)
rmpart.o:      $(INCFILES)
rwalk.o:       $(INCFILES)
tabcon.o:      $(INCFILES)
vgravtables.o: $(INCFILES)
videosave.o:   $(INCFILES)
wetdep1.o:     $(INCFILES)
wetdep2.o:     $(INCFILES)

snapInterface.o: $(INCFILES)

milib.o: milib.h

#---------------------------------------------------------

clean:
	rm -f *.o
	rm -f *~
	rm -f snap_batch_copy.*
	rm -f bsnap_nbomb create_bomb_input
