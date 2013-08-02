# includefile contains Compiler definitions etc.

F77 = gfortran
CXX = g++
CC  = gcc

F77FLAGS=-DACCIDENT -O3 -cpp -mcmodel=medium
CXXFLAGS=-O3
CCFLAGS=-O3

LDFLAGS=

DEFS= -D_STANDARD_C_PLUS_PLUS

MIINC = -I/usr/include
MILIB = -L/usr/lib -lmi
EXLIBS = -lpthread -ldl

DRHOOKINC = -I../../utils/drhook_CY31R2.032
DRHOOKLIB = -L../../utils/drhook_CY31R2.032 -ldrhook -lmpi_serial


USRLOCAL=/usr

##########################################################

BINDIR=../../bin/

INCLUDES = -I. $(MIINC)

LIBS= $(MILIB) $(EXLIBS)
BLIBS= $(MILIB) -lnetcdf -lnetcdff

ifdef DR_HOOK
$(info DR_HOOK defined)
F77FLAGS += -DDRHOOK $(DRHOOKINC)
BLIBS += $(DRHOOKLIB)
endif

# clear out all suffixes
.SUFFIXES:

#.SUFFIXES: .cc .c .f
#.f.o:
#	${F77} -c ${F77FLAGS} $(INCLUDES) $<
#.F.o:
#	${F77} -c ${F77FLAGS} $(INCLUDES) $<
#.c.o:
#	$(CC)  -c $(CCFLAGS) $(INCLUDES) $<
#.cc.o:
#	$(CXX) -c $(CXXFLAGS) $(DEFS) $(INCLUDES) $<

MODELOBJ= argoswrite.o bldp.o compheight.o copyfield.o decay.o drydep1.o drydep2.o edcomp.o \
ensemble.o epinterp.o filesort.o fldout.o forwrd.o ftest.o om2edot.o posint.o \
pselect.o readfd.o readfield.o readfield_nc.o release.o rmpart.o rwalk.o tabcon.o \
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
	${F77} -c $(F77FLAGS) $(INCLUDES) -DBATCH snap_batch_copy.F

argoswrite.o: ../common/argoswrite.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
bldp.o: ../common/bldp.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
compheight.o: ../common/compheight.f  $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
copyfield.o: ../common/copyfield.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
decay.o: ../common/decay.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
drydep1.o: ../common/drydep1.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
drydep2.o: ../common/drydep2.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
edcomp.o: ../common/edcomp.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
ensemble.o: ../common/ensemble.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
epinterp.o: ../common/epinterp.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
filesort.o: ../common/filesort.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
fldout.o: ../common/fldout.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
forwrd.o: ../common/forwrd.F $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
ftest.o: ../common/ftest.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
om2edot.o: ../common/om2edot.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
posint.o: ../common/posint.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
pselect.o: ../common/pselect.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
readfd.o: ../common/readfd.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
readfield.o: ../common/readfield.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
readfield_nc.o: ../common/readfield_nc.F $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
release.o: ../common/release.F $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
rmpart.o: ../common/rmpart.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
rwalk.o: ../common/rwalk.F $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
tabcon.o: ../common/tabcon.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
vgravtables.o: ../common/vgravtables.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
videosave.o: ../common/videosave.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
wetdep1.o: ../common/wetdep1.F $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
wetdep2.o: ../common/wetdep2.F $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<

snapInterface.o: $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<

milib.o: ../common/milib.c ../common/milib.h
	$(CC)  -c $(CCFLAGS) -I../common $<
c2fgetvar.o: ../common/c2fgetvar.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
c2fgetarg.o: ../common/c2fgetarg.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<

#---------------------------------------------------------

clean:
	for i in *.inc; do if [ -L $$i ]; then rm $$i; fi done
	rm -f *.o
	rm -f *~
	rm -f snap_batch_copy.* bsnap_naccident create_naccident_input create_naccident_output

install: bsnap_naccident create_naccident_input create_naccident_output
	install $^ $(BINDIR)/
