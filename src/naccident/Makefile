# includefile contains Compiler definitions etc.

F77 = gfortran
CXX = g++
CC  = gcc

F77FLAGS=-O3 -mcmodel=medium -DACCIDENT
CXXFLAGS=-O3
CCFLAGS=-O3

LDFLAGS=

DEFS= -D_STANDARD_C_PLUS_PLUS

MIINC = -I/usr/include
MILIB = -L/usr/lib -lmi
EXLIBS = -lpthread -ldl

USRLOCAL=/usr

##########################################################

BINDIR=../../bin/

INCLUDES = -I. $(MIINC)

LIBS= $(MILIB) $(EXLIBS)
BLIBS= $(MILIB)

.SUFFIXES: .cc .c .f .F
.f.o:
	${F77} -c ${F77FLAGS} -I. $<
.F.o:
	${F77} -c ${F77FLAGS} -I. $<
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

all: bsnap_naccident create_naccident_input create_naccident_output

link_incfiles:
	for i in ../common/*.inc; do bn=`basename $$i`; if [ ! -L $$bn ]; then ln -s $$i .; fi; done

create_naccident_input: create_naccident_input.f
	${F77} ${F77FLAGS} -I. $< -o $@

create_naccident_output: create_naccident_output.f
	${F77} ${F77FLAGS} -I. $< -o $@

bsnap_naccident: link_incfiles $(BOBJ) $(MODELOBJ) milib.o c2fgetvar.o c2fgetarg.o
	${F77} $(F77FLAGS) $(BOBJ) $(MODELOBJ) $(BLIBS) -o bsnap_naccident

#--------------------------------

snap_batch_copy.o: ../common/snap.F $(INCFILES)
	cp -p ../common/snap.F snap_batch_copy.F
	${F77} -c $(F77FLAGS) -I. -DBATCH snap_batch_copy.F

argoswrite.o: ../common/argoswrite.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
bldp.o: ../common/bldp.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
compheight.o: ../common/compheight.f  $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
copyfield.o: ../common/copyfield.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
decay.o: ../common/decay.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
drydep1.o: ../common/drydep1.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
drydep2.o: ../common/drydep2.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
edcomp.o: ../common/edcomp.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
ensemble.o: ../common/ensemble.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
epinterp.o: ../common/epinterp.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
filesort.o: ../common/filesort.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
fldout.o: ../common/fldout.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
forwrd.o: ../common/forwrd.F $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
ftest.o: ../common/ftest.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
om2edot.o: ../common/om2edot.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
posint.o: ../common/posint.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
pselect.o: ../common/pselect.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
readfd.o: ../common/readfd.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
readfield.o: ../common/readfield.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
release.o: ../common/release.F $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
rmpart.o: ../common/rmpart.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
rwalk.o: ../common/rwalk.F $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
tabcon.o: ../common/tabcon.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
vgravtables.o: ../common/vgravtables.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
videosave.o: ../common/videosave.f $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
wetdep1.o: ../common/wetdep1.F $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<
wetdep2.o: ../common/wetdep2.F $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<

snapInterface.o: $(INCFILES)
	${F77} -c ${F77FLAGS} -I. $<

milib.o: ../common/milib.c ../common/milib.h
	$(CC)  -c $(CCFLAGS) -I../common $<
c2fgetvar.o: ../common/c2fgetvar.f
	${F77} -c ${F77FLAGS} -I. $<
c2fgetarg.o: ../common/c2fgetarg.f
	${F77} -c ${F77FLAGS} -I. $<

#---------------------------------------------------------

clean:
	for i in *.inc; do if [ -L $$i ]; then rm $$i; fi done
	rm -f *.o
	rm -f *~
	rm -f snap_batch_copy.* bsnap_naccident create_naccident_input create_naccident_output

install: bsnap_naccident create_naccident_input create_naccident_output
	install $^ $(BINDIR)/
