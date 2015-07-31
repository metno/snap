# this file contains the common parts to compile bsnap
# should be included after all: target

MODELOBJ= allocateFields.o argoswrite.o bldp.o compheight.o copyfield.o decay.o drydep1.o drydep2.o \
dateCalc.o edcomp.o \
ensemble.o epinterp.o filesort_nc.o fldout_nc.o forwrd.o ftest.o \
init_random_seed.o om2edot.o particleML.o posint.o \
readfield_nc.o release.o releasefile.o rmpart.o rwalk.o tabcon.o \
vgravtables.o wetdep1.o wetdep2.o

ifdef MILIB
  MODELOBJ += fldout.o filesort.o readfield.o readfd.o  
else
  MODELOBJ += feltio_dummy.o chcase.o  gridpar.o  keywrd.o  mapfield.o  rlunit.o  termchar.o  xyconvert.o \
     getvar.o  hrdiff.o   lenstr.o  prhelp.o    rmfile.o  vtime.o \
     earthr.o pol2sph.o sph2rot.o lam2sph.o mer2sph.o     
endif

BOBJ = snap_batch_copy.o

INCFILES = snapdim.inc snapfil.inc snapfld.inc snapgrd.inc snappar.inc \
snappos.inc snaptab.inc snapgrp.inc snapeps.inc snapargos.inc snapdebug.inc

link_incfiles:
	for i in ../common/*.inc; do bn=`basename $$i`; if [ ! -L $$bn ]; then ln -s $$i .; fi; done

clean_links:
	for i in *.inc; do if [ -L $$i ]; then rm $$i; fi done
	rm -f *.mod *.o *~

#--------------------------------

snap_batch_copy.o: ../common/snap.F dateCalc.o particleML.o $(INCFILES)
	cp -p ../common/snap.F snap_batch_copy.F
	${F77} -c $(F77FLAGS) $(INCLUDES) -DBATCH snap_batch_copy.F

allocateFields.o: ../common/allocateFields.F $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
argoswrite.o: ../common/argoswrite.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
bldp.o: ../common/bldp.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
compheight.o: ../common/compheight.f  $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
copyfield.o: ../common/copyfield.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
dateCalc.o: ../common/dateCalc.F90 $(INCFILES)
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
filesort_nc.o: ../common/filesort_nc.F $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
fldout.o: ../common/fldout.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
fldout_nc.o: ../common/fldout_nc.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
feltio_dummy.o: ../common/feltio_dummy.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
forwrd.o: ../common/forwrd.F $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
ftest.o: ../common/ftest.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
init_random_seed.o: ../common/init_random_seed.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
om2edot.o: ../common/om2edot.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
particleML.o: ../common/particleML.f90
	${F77} -c ${F77FLAGS} $<
posint.o: ../common/posint.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
readfd.o: ../common/readfd.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
readfield.o: ../common/readfield.f $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
readfield_nc.o: ../common/readfield_nc.F $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
releasefile.o: ../common/releasefile.f $(INCFILES)
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
wetdep1.o: ../common/wetdep1.F $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
wetdep2.o: ../common/wetdep2.F $(INCFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<


milib.o: ../common/milib.c ../common/milib.h
	$(CC)  -c $(CCFLAGS) -I../common $<
c2fgetvar.o: ../common/c2fgetvar.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
c2fgetarg.o: ../common/c2fgetarg.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<

# libmi
chcase.o: ../common/milib/chcase.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
gridpar.o: ../common/milib/gridpar.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
getvar.o: ../common/milib/getvar.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
hrdiff.o: ../common/milib/hrdiff.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
keywrd.o: ../common/milib/keywrd.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
lenstr.o: ../common/milib/lenstr.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
mapfield.o: ../common/milib/mapfield.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
prhelp.o: ../common/milib/prhelp.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
rlunit.o: ../common/milib/rlunit.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
rmfile.o: ../common/milib/rmfile.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
vtime.o: ../common/milib/vtime.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
xyconvert.o: ../common/milib/xyconvert.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
termchar.o: ../common/milib/termchar.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
earthr.o: ../common/milib/earthr.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
pol2sph.o: ../common/milib/pol2sph.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
sph2rot.o: ../common/milib/sph2rot.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
lam2sph.o: ../common/milib/lam2sph.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
mer2sph.o: ../common/milib/mer2sph.f
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
#---------------------------------------------------------

