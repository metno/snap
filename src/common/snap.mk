# this file contains the common parts to compile bsnap
# should be included after all: target

MODFILES = dateCalc.o fileInfoML.o particleML.o snapdimML.o snapfilML.o snapfldML.o snapgrdML.o snapmetML.o snapparML.o \
snapposML.o snaptabML.o  snapepsML.o snapargosML.o snapdebugML.o

MODELOBJ= allocateFields.o argoswrite.o bldp.o compheight.o copyfield.o checkDomain.o \
decay.o decayDeps.o drydep1.o drydep2.o edcomp.o \
ensemble.o epinterp.o filesort_nc.o fldout_nc.o forwrd_dx.o forwrd.o ftest.o \
init_random_seed.o om2edot.o posint.o \
readfield_nc.o release.o releasefile.o rmpart.o rwalk.o tabcon.o \
vgravtables.o wetdep1.o wetdep2.o $(MODFILES)

ifdef MILIB
  MODELOBJ += fldout.o filesort.o readfield.o readfd.o  
else
  MODELOBJ += feltio_dummy.o chcase.o  gridpar.o  keywrd.o  mapfield.o  rlunit.o  termchar.o  xyconvert.o \
     getvar.o  hrdiff.o   lenstr.o  prhelp.o    rmfile.o  vtime.o \
     earthr.o pol2sph.o sph2rot.o lam2sph.o mer2sph.o     
endif

BOBJ = snap_batch_copy.o

link_incfiles:
	rm -f *.inc

clean_links:
	for i in *.inc; do if [ -L $$i ]; then rm $$i; fi done
	rm -f *.mod *.o *~

#--------------------------------

snap_batch_copy.o: ../common/snap.f90 $(MODFILES)
	cp -p ../common/snap.f90 snap_batch_copy.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) -DBATCH snap_batch_copy.f90

allocateFields.o: ../common/allocateFields.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
argoswrite.o: ../common/argoswrite.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
bldp.o: ../common/bldp.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
compheight.o: ../common/compheight.f90  $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
copyfield.o: ../common/copyfield.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
checkDomain.o: ../common/checkDomain.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
dateCalc.o: ../common/dateCalc.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
decay.o: ../common/decay.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
decayDeps.o: ../common/decayDeps.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
drydep1.o: ../common/drydep1.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
drydep2.o: ../common/drydep2.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
edcomp.o: ../common/edcomp.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
ensemble.o: ../common/ensemble.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
epinterp.o: ../common/epinterp.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
fileInfoML.o: ../common/fileInfoML.f90
	${F77} -c ${F77FLAGS} $<
filesort.o: ../common/filesort.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
filesort_nc.o: ../common/filesort_nc.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
fldout.o: ../common/fldout.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
fldout_nc.o: ../common/fldout_nc.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
feltio_dummy.o: ../common/feltio_dummy.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
forwrd_dx.o: ../common/forwrd_dx.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
forwrd.o: ../common/forwrd.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
ftest.o: ../common/ftest.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
init_random_seed.o: ../common/init_random_seed.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
om2edot.o: ../common/om2edot.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
particleML.o: ../common/particleML.f90
	${F77} -c ${F77FLAGS} $<
posint.o: ../common/posint.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
readfd.o: ../common/readfd.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
readfield.o: ../common/readfield.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
readfield_nc.o: ../common/readfield_nc.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
releasefile.o: ../common/releasefile.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
release.o: ../common/release.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
rmpart.o: ../common/rmpart.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
rwalk.o: ../common/rwalk.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
snapargosML.o: ../common/snapargosML.f90
	${F77} -c ${F77FLAGS} $<
snapdebugML.o: ../common/snapdebugML.f90
	${F77} -c ${F77FLAGS} $<
snapdimML.o: ../common/snapdimML.f90
	${F77} -c ${F77FLAGS} $<
snapepsML.o: ../common/snapepsML.f90
	${F77} -c ${F77FLAGS} $<
snapfilML.o: ../common/snapfilML.f90 fileInfoML.o
	${F77} -c ${F77FLAGS} $<
snapfldML.o: ../common/snapfldML.f90
	${F77} -c ${F77FLAGS} $<
snapgrdML.o: ../common/snapgrdML.f90
	${F77} -c ${F77FLAGS} $<
snapmetML.o: ../common/snapmetML.f90 snapfilML.o
	${F77} -c ${F77FLAGS} $<
snapposML.o: ../common/snapposML.f90
	${F77} -c ${F77FLAGS} $<
snapparML.o: ../common/snapparML.f90
	${F77} -c ${F77FLAGS} $<
snaptabML.o: ../common/snaptabML.f90
	${F77} -c ${F77FLAGS} $<


tabcon.o: ../common/tabcon.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
vgravtables.o: ../common/vgravtables.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
wetdep1.o: ../common/wetdep1.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
wetdep2.o: ../common/wetdep2.f90 $(MODFILES)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<


MILIB_FLAGS = -fno-implicit-none -fno-module-private

milib.o: ../common/milib.c ../common/milib.h
	$(CC)  -c $(CCFLAGS) -I../common $<
c2fgetvar.o: ../common/c2fgetvar.f90
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
c2fgetarg.o: ../common/c2fgetarg.f90
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<

# libmi
chcase.o: ../common/milib/chcase.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
gridpar.o: ../common/milib/gridpar.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
getvar.o: ../common/milib/getvar.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
hrdiff.o: ../common/milib/hrdiff.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
keywrd.o: ../common/milib/keywrd.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
lenstr.o: ../common/milib/lenstr.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
mapfield.o: ../common/milib/mapfield.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
prhelp.o: ../common/milib/prhelp.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
rlunit.o: ../common/milib/rlunit.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
rmfile.o: ../common/milib/rmfile.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
vtime.o: ../common/milib/vtime.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
xyconvert.o: ../common/milib/xyconvert.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
termchar.o: ../common/milib/termchar.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
earthr.o: ../common/milib/earthr.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
pol2sph.o: ../common/milib/pol2sph.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
sph2rot.o: ../common/milib/sph2rot.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
lam2sph.o: ../common/milib/lam2sph.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
mer2sph.o: ../common/milib/mer2sph.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
#---------------------------------------------------------

