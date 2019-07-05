# this file contains the common parts to compile bsnap
# should be included after all: target

MODELOBJ = dateCalc.o particleML.o snapdimML.o snapfilML.o snapfldML.o snapgrdML.o snapmetML.o snapparML.o \
snapposML.o snaptabML.o snapargosML.o snapdebugML.o posint.o decay.o \
om2edot.o ftest.o readfield_nc.o rwalk.o epinterp.o \
ensemble.o vgravtables.o forwrd.o wetdep.o drydep.o \
argoswrite.o bldp.o compheight.o checkDomain.o \
filesort_nc.o fldout_nc.o \
init_random_seed.o\
release.o releasefile.o rmpart.o allocateFields.o \
snapepsML.o find_parameters.o

ifdef MILIB
  F77FLAGS += -DMILIB
  MODELOBJ += fldout.o filesort.o readfd.o readfield.o
else
  MODELOBJ += feltio_dummy.o chcase.o  gridpar.o  keywrd.o  mapfield.o  rlunit.o  termchar.o  xyconvert.o \
     getvar.o  hrdiff.o   lenstr.o  prhelp.o    rmfile.o  vtime.o \
     earthr.o pol2sph.o sph2rot.o lam2sph.o mer2sph.o milibML.o
endif

BOBJ = snap_batch_copy.o


clean_links:
	rm -f *.mod *.o *~

snap_batch_copy.o: ../common/snap.F90 $(MODELOBJ)
	cp -p ../common/snap.F90 snap_batch_copy.F90
	${F77} -c $(F77FLAGS) $(INCLUDES) -DBATCH snap_batch_copy.F90

#--------------------------------

allocateFields.o: ../common/allocateFields.f90 particleML.o snapparML.o snapfldML.o snapfilML.o snapgrdML.o release.o
	${F77} -c $(F77FLAGS) $(INCLUDES) $<
snapdimML.o: ../common/snapdimML.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $<
snaptabML.o: ../common/snaptabML.f90 snapdimML.o snapdebugML.o
	${F77} -c $(F77FLAGS) $(INCLUDES) $<
snapfldML.o: ../common/snapfldML.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $<
ftest.o: ../common/ftest.f90 snapdebugML.o
	${F77} -c $(F77FLAGS) $(INCLUDES) $<
snapdebugML.o: ../common/snapdebugML.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $<
snapgrdML.o: ../common/snapgrdML.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $<
bldp.o: ../common/bldp.f90 snapdimML.o snaptabML.o snapfldML.o ftest.o snapdebugML.o snapgrdML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
particleML.o: ../common/particleML.f90
	${F77} -c ${F77FLAGS} $<
decay.o: ../common/decay.f90 snapdimML.o particleML.o snapfldML.o snapparML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
compheight.o: ../common/compheight.f90 snapgrdML.o snapfldML.o snaptabML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
snapparML.o: ../common/snapparML.f90 snapdimML.o
	${F77} -c ${F77FLAGS} $<
ensemble.o: ../common/ensemble.f90 particleML.o snapparML.o snapgrdML.o snapdimML.o epinterp.o ftest.o snapdebugML.o snapfldML.o snapepsML.o milibML.o release.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
checkDomain.o: ../common/checkDomain.f90 snapgrdML.o snapdimML.o particleML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
snapepsML.o: ../common/snapepsML.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
dateCalc.o: ../common/dateCalc.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
drydep.o: ../common/drydep.f90 particleML.o snapfldML.o snapparML.o snapgrdML.o vgravtables.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
epinterp.o: ../common/epinterp.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
argoswrite.o: ../common/argoswrite.f90 snapdebugML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
snapfilML.o: ../common/snapfilML.f90 snapdimML.o
	${F77} -c ${F77FLAGS} $<
filesort.o: ../common/filesort.f90 snapfldML.o snapfilML.o snapdebugML.o snapgrdML.o snapdimML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
om2edot.o: ../common/om2edot.f90 snapgrdML.o snapfldML.o snapdimML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
readfield_nc.o: ../common/readfield_nc.f90 particleML.o snapfilML.o snapgrdML.o snapmetML.o snaptabML.o snapdebugML.o snapdimML.o om2edot.o ftest.o milibML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
filesort_nc.o: ../common/filesort_nc.f90 dateCalc.o snapfilML.o snapdimML.o snapgrdML.o snapfldML.o snapmetML.o snapdebugML.o readfield_nc.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
fldout.o: ../common/fldout.f90 particleML.o snapfilML.o snapgrdML.o snapfldML.o snapparML.o snapargosML.o snapdebugML.o snapdimML.o ftest.o argoswrite.o snaptabML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
fldout_nc.o: ../common/fldout_nc.f90 snapfilML.o snapgrdML.o snapfldML.o snapparML.o snaptabML.o snapargosML.o snapdebugML.o snapdimML.o readfield_nc.o ftest.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
feltio_dummy.o: ../common/feltio_dummy.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
forwrd.o: ../common/forwrd.F90 particleML.o snapgrdML.o snapfldML.o snapparML.o snaptabML.o snapdimML.o snapdebugML.o vgravtables.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
init_random_seed.o: ../common/init_random_seed.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
posint.o: ../common/posint.f90 snapgrdML.o snapdimML.o snapdebugML.o particleML.o snapfldML.o snapparML.o snapgrdML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
readfd.o: ../common/readfd.f90 snapfilML.o snapgrdML.o snapdebugML.o snapdimML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
readfield.o: ../common/readfield.f90 particleML.o snapfilML.o snapgrdML.o snapfldML.o snaptabML.o snapdimML.o snapdebugML.o readfd.o om2edot.o ftest.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
releasefile.o: ../common/releasefile.f90 snapparML.o snapdimML.o release.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
release.o: ../common/release.f90 particleML.o snapgrdML.o snapfldML.o snapparML.o snapposML.o snaptabML.o snapdimML.o snapdebugML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
rmpart.o: ../common/rmpart.f90 particleML.o snapgrdML.o snapparML.o snapdimML.o decay.o release.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
rwalk.o: ../common/rwalk.f90 particleML.o snapgrdML.o snapparML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
snapargosML.o: ../common/snapargosML.f90
	${F77} -c ${F77FLAGS} $<
snapmetML.o: ../common/snapmetML.f90 snapfilML.o
	${F77} -c ${F77FLAGS} $<
snapposML.o: ../common/snapposML.f90 snapdimML.o
	${F77} -c ${F77FLAGS} $<
vgravtables.o: ../common/vgravtables.f90 snapparML.o snapdimML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
wetdep.o: ../common/wetdep.f90 particleML.o snapgrdML.o snapfldML.o snapparML.o snaptabML.o snapdimML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
find_parameters.o: ../common/find_parameters.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<

milibML.o: ../common/milibML.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<

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

