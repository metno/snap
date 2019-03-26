# this file contains the common parts to compile bsnap
# should be included after all: target

MODELOBJ = dateCalc.o fileInfoML.o particleML.o snapdimML.o snapfilML.o snapfldML.o snapgrdML.o snapmetML.o snapparML.o \
snapposML.o snaptabML.o snapargosML.o snapdebugML.o posint.o decay.o \
edcomp.o om2edot.o ftest.o readfield_nc.o rwalk.o epinterp.o \
ensemble.o vgravtables.o forwrd.o tabcon.o wetdep1.o wetdep2.o drydep1.o drydep2.o \
argoswrite.o bldp.o compheight.o checkDomain.o \
filesort_nc.o fldout_nc.o \
init_random_seed.o decayDeps.o\
release.o releasefile.o rmpart.o allocateFields.o copyfield.o forwrd_dx.o

ifdef MILIB
  F77FLAGS += -DMILIB
  MODELOBJ += fldout.o filesort.o readfd.o readfield.o
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

snap_batch_copy.o: ../common/snap.f90 $(MODELOBJ)
	cp -p ../common/snap.f90 snap_batch_copy.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) -DBATCH snap_batch_copy.f90 -std=gnu

#--------------------------------

allocateFields.o: ../common/allocateFields.f90 particleML.o snapparML.o fileInfoML.o snapfldML.o snapfilML.o snapgrdML.o
	${F77} -c $(F77FLAGS) $(INCLUDES) $< -std=gnu
snapdimML.o: ../common/snapdimML.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $< -std=gnu
copyfield.o: ../common/copyfield.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $< -std=gnu
decayDeps.o: ../common/decayDeps.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $< -std=gnu
forwrd_dx.o: ../common/forwrd_dx.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $< -std=gnu
snaptabML.o: ../common/snaptabML.f90 snapdimML.o
	${F77} -c $(F77FLAGS) $(INCLUDES) $< -std=gnu
snapfldML.o: ../common/snapfldML.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $< -std=gnu
ftest.o: ../common/ftest.f90 snapdebugML.o
	${F77} -c $(F77FLAGS) $(INCLUDES) $< -std=gnu
snapdebugML.o: ../common/snapdebugML.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $< -std=gnu
snapgrdML.o: ../common/snapgrdML.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $< -std=gnu
bldp.o: ../common/bldp.f90 snapdimML.o snaptabML.o snapfldML.o ftest.o snapdebugML.o snapgrdML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
edcomp.o: ../common/edcomp.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
particleML.o: ../common/particleML.f90
	${F77} -c ${F77FLAGS} $< -std=gnu
decay.o: ../common/decay.f90 snapdimML.o particleML.o snapfldML.o snapparML.o decayDeps.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
compheight.o: ../common/compheight.f90 snapgrdML.o snapfldML.o snaptabML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
snapparML.o: ../common/snapparML.f90 snapdimML.o
	${F77} -c ${F77FLAGS} $< -std=gnu
ensemble.o: ../common/ensemble.f90 particleML.o snapparML.o snapgrdML.o snapdimML.o epinterp.o ftest.o snapdebugML.o snapfldML.o snapepsML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
checkDomain.o: ../common/checkDomain.f90 snapgrdML.o snapdimML.o particleML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
snapepsML.o: ../common/snapepsML.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
dateCalc.o: ../common/dateCalc.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
drydep1.o: ../common/drydep1.f90 particleML.o snapfldML.o snapparML.o snapgrdML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
drydep2.o: ../common/drydep2.f90 particleML.o snapfldML.o snapparML.o snapgrdML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
epinterp.o: ../common/epinterp.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
argoswrite.o: ../common/argoswrite.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
fileInfoML.o: ../common/fileInfoML.f90
	${F77} -c ${F77FLAGS} $< -std=gnu
snapfilML.o: ../common/snapfilML.f90 fileInfoML.o snapdimML.o
	${F77} -c ${F77FLAGS} $< -std=gnu
filesort.o: ../common/filesort.f90 fileInfoML.o snapfldML.o snapfilML.o snapdebugML.o snapgrdML.o snapdimML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
om2edot.o: ../common/om2edot.f90 snapgrdML.o snapfldML.o snapdimML.o edcomp.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
readfield_nc.o: ../common/readfield_nc.f90 particleML.o fileInfoML.o snapfilML.o snapgrdML.o snapmetML.o snaptabML.o snapdebugML.o snapdimML.o om2edot.o ftest.o copyfield.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
filesort_nc.o: ../common/filesort_nc.f90 dateCalc.o fileInfoML.o snapfilML.o snapdimML.o snapgrdML.o snapfldML.o snapmetML.o snapdebugML.o readfield_nc.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
fldout.o: ../common/fldout.f90 particleML.o fileInfoML.o snapfilML.o snapgrdML.o snapfldML.o snapparML.o snapargosML.o snapdebugML.o snapdimML.o ftest.o argoswrite.o snaptabML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
fldout_nc.o: ../common/fldout_nc.f90 snapfilML.o snapgrdML.o snapfldML.o snapparML.o snaptabML.o snapargosML.o snapdebugML.o snapdimML.o readfield_nc.o ftest.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
feltio_dummy.o: ../common/feltio_dummy.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
forwrd.o: ../common/forwrd.f90 particleML.o snapgrdML.o snapfldML.o snapparML.o snaptabML.o snapdimML.o snapdebugML.o vgravtables.o forwrd_dx.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
init_random_seed.o: ../common/init_random_seed.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
posint.o: ../common/posint.f90 snapgrdML.o snapdimML.o snapdebugML.o particleML.o snapfldML.o snapparML.o snapgrdML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
readfd.o: ../common/readfd.f90 fileInfoML.o snapfilML.o snapgrdML.o snapdebugML.o snapdimML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
readfield.o: ../common/readfield.f90 particleML.o fileInfoML.o snapfilML.o snapgrdML.o snapfldML.o snaptabML.o snapdimML.o snapdebugML.o readfd.o om2edot.o ftest.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
releasefile.o: ../common/releasefile.f90 snapparML.o snapdimML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
release.o: ../common/release.f90 particleML.o snapgrdML.o snapfldML.o snapparML.o snapposML.o snaptabML.o snapdimML.o snapdebugML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
rmpart.o: ../common/rmpart.f90 particleML.o snapgrdML.o snapparML.o snapdimML.o decay.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
rwalk.o: ../common/rwalk.f90 particleML.o snapgrdML.o snapparML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
snapargosML.o: ../common/snapargosML.f90
	${F77} -c ${F77FLAGS} $< -std=gnu
snapmetML.o: ../common/snapmetML.f90 snapfilML.o
	${F77} -c ${F77FLAGS} $< -std=gnu
snapposML.o: ../common/snapposML.f90 snapdimML.o
	${F77} -c ${F77FLAGS} $< -std=gnu
tabcon.o: ../common/tabcon.f90 snaptabML.o snapdimML.o snapdebugML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
vgravtables.o: ../common/vgravtables.f90 snapparML.o snapdimML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
wetdep2.o: ../common/wetdep2.f90 particleML.o snapgrdML.o snapfldML.o snapparML.o snaptabML.o snapdimML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu
wetdep1.o: ../common/wetdep1.f90 particleML.o snapgrdML.o snapfldML.o snapparML.o snaptabML.o snapdimML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $< -std=gnu


MILIB_FLAGS = -fno-implicit-none -fno-module-private -Wno-all -Wno-extra

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

