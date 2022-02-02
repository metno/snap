# this file contains the common parts to compile bsnap
# should be included after all: target

MODELOBJ = dateCalc.o utils.o particleML.o snapdimML.o snapfilML.o snapfimexML.o \
snapfldML.o snapgrdML.o snapmetML.o snapparML.o \
snapposML.o snaptabML.o snapdebugML.o posint.o decay.o \
om2edot.o ftest.o readfield_nc.o rwalk.o epinterp.o \
vgravtables.o forwrd.o wetdep.o drydep.o \
bldp.o compheight.o checkDomain.o \
filesort_nc.o fldout_nc.o \
init_random_seed.o\
release.o releasefile.o rmpart.o split_particles.o allocateFields.o \
find_parameters.o datetime.o

# old milib files
MODELOBJ += gridpar.o  mapfield.o  xyconvert.o \
     earthr.o pol2sph.o sph2rot.o lam2sph.o mer2sph.o milibML.o

ifdef FIMEXLIB
  ifneq "${FIMEXLIB}" ""
    F77FLAGS += -DFIMEX
    MODELOBJ += readfield_fi.o filesort_fi.o find_parameters_fi.o fimex.o
  endif
endif


BOBJ = snap.o


clean_links:
	rm -f *.mod *.o *~

snap.o: ../common/snap.F90 $(MODELOBJ)
	${F77} -c $(F77FLAGS) $(INCLUDES) $<

#--------------------------------

fimex.o: ../common/fimex.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) -fno-module-private $<
allocateFields.o: ../common/allocateFields.f90 particleML.o snapparML.o snapfldML.o snapfilML.o snapgrdML.o release.o snapdimML.o
	${F77} -c $(F77FLAGS) $(INCLUDES) $<
snapdimML.o: ../common/snapdimML.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $<
snaptabML.o: ../common/snaptabML.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $<
snapfldML.o: ../common/snapfldML.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $<
ftest.o: ../common/ftest.f90 snapdebugML.o
	${F77} -c $(F77FLAGS) $(INCLUDES) $<
snapdebugML.o: ../common/snapdebugML.F90
	${F77} -c $(F77FLAGS) $(INCLUDES) $<
snapfilML.o: ../common/snapfilML.f90 snapdimML.o datetime.o
	${F77} -c ${F77FLAGS} $<
snapfimexML.o: ../common/snapfimexML.f90
	${F77} -c ${F77FLAGS} $<
snapgrdML.o: ../common/snapgrdML.f90
	${F77} -c $(F77FLAGS) $(INCLUDES) $<
bldp.o: ../common/bldp.f90 snapdimML.o snaptabML.o snapgrdML.o snapfldML.o ftest.o snapdebugML.o snapgrdML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
particleML.o: ../common/particleML.f90
	${F77} -c ${F77FLAGS} $<
decay.o: ../common/decay.f90 particleML.o snapfldML.o snapparML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
compheight.o: ../common/compheight.f90 snapgrdML.o snapfldML.o snaptabML.o snapdimML.o ftest.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
snapparML.o: ../common/snapparML.f90 snapdimML.o
	${F77} -c ${F77FLAGS} $<
checkDomain.o: ../common/checkDomain.f90 snapgrdML.o snapdimML.o particleML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
dateCalc.o: ../common/dateCalc.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
drydep.o: ../common/drydep.f90 particleML.o snapfldML.o snapparML.o snapgrdML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
epinterp.o: ../common/epinterp.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
om2edot.o: ../common/om2edot.f90 snapgrdML.o snapfldML.o snapdimML.o snapdebugML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
readfield_nc.o: ../common/readfield_nc.f90 particleML.o snapfilML.o snapgrdML.o snapmetML.o snaptabML.o snapdebugML.o snapdimML.o om2edot.o ftest.o milibML.o snapfldML.o datetime.o drydep.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
readfield_fi.o: ../common/readfield_fi.f90 snapfimexML.o particleML.o snapfilML.o snapgrdML.o snapmetML.o snaptabML.o snapdebugML.o snapdimML.o om2edot.o ftest.o milibML.o fimex.o datetime.o readfield_nc.o utils.o drydep.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
filesort_nc.o: ../common/filesort_nc.f90 dateCalc.o snapfilML.o snapdimML.o snapgrdML.o snapfldML.o snapmetML.o snapdebugML.o readfield_nc.o datetime.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
filesort_fi.o: ../common/filesort_fi.f90 snapfimexML.o dateCalc.o snapfilML.o snapdimML.o snapgrdML.o snapfldML.o snapmetML.o snapdebugML.o readfield_fi.o datetime.o utils.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
find_parameters.o: ../common/find_parameters.f90 snapmetML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
find_parameters_fi.o: ../common/find_parameters_fi.f90 snapfimexML.o snapmetML.o fimex.o readfield_fi.o utils.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
fldout_nc.o: ../common/fldout_nc.f90 snapfilML.o snapgrdML.o snapfldML.o snapparML.o snaptabML.o snapdebugML.o snapdimML.o readfield_nc.o ftest.o release.o milibML.o datetime.o utils.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
forwrd.o: ../common/forwrd.F90 particleML.o snapgrdML.o snapfldML.o snapparML.o snaptabML.o snapdimML.o snapdebugML.o vgravtables.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
init_random_seed.o: ../common/init_random_seed.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
posint.o: ../common/posint.f90 snapgrdML.o snapdimML.o snapdebugML.o particleML.o snapfldML.o snapparML.o snapgrdML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
releasefile.o: ../common/releasefile.f90 snapparML.o snapdimML.o release.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
release.o: ../common/release.f90 particleML.o snapgrdML.o snapfldML.o snapparML.o snapposML.o snaptabML.o snapdimML.o snapdebugML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
rmpart.o: ../common/rmpart.f90 particleML.o snapgrdML.o snapparML.o snapdimML.o decay.o release.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
rwalk.o: ../common/rwalk.f90 particleML.o snapgrdML.o snapparML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
snapmetML.o: ../common/snapmetML.f90 snapfilML.o
	${F77} -c ${F77FLAGS} $<
snapposML.o: ../common/snapposML.f90 snapdimML.o
	${F77} -c ${F77FLAGS} $<
split_particles.o: ../common/split_particles.f90 snapparML.o release.o snapdebugML.o snapparML.o
	${F77} -c ${F77FLAGS} $<
utils.o: ../common/utils.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
vgravtables.o: ../common/vgravtables.f90 snapparML.o snapdimML.o drydep.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
wetdep.o: ../common/wetdep.f90 particleML.o snapgrdML.o snapfldML.o snapparML.o snaptabML.o snapdimML.o snapdebugML.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<
datetime.o: ../common/datetime.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<

milibML.o: ../common/milibML.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<

# libmi
gridpar.o: ../common/milib/gridpar.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
mapfield.o: ../common/milib/mapfield.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
rmfile.o: ../common/milib/rmfile.f
	${F77} -c ${F77FLAGS} ${MILIB_FLAGS} $(INCLUDES) $<
xyconvert.o: ../common/milib/xyconvert.f
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

