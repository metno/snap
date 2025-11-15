# includefile contains Compiler definitions etc.
include ../current.mk

F77FLAGS += -DACCIDENT

all:

include ../common/snap.mk

# export FIMEXLIB
.EXPORT_ALL_VARIABLES:

TESTFILES = snap_testdata/meteo20200415_00_ringhals.nc \
	    snap_testdata/meteo20200415_01_ringhals.nc \
            snap_testdata/meps_det_2_5km_20200316T00Z_ringhals.nc \
	    snap_testdata/snap_ecemep_expected5.nc \
	    snap_testdata/snap_meps_interpolated_expected4.nc

install:


snap_testdata/%.nc :
	mkdir -p snap_testdata
	curl --fail https://thredds.met.no/thredds/fileServer/metusers/heikok/$@ -o $@

test: testDateCalc test_array_utils find_parameters_fi_test  testGravSettling testDryDep bsnap_naccident $(TESTFILES)
	./testDateCalc && \
	./test_array_utils && \
	./find_parameters_fi_test snap_testdata/meteo20200415_00_ringhals.nc u_wind && \
	./find_parameters_fi_test snap_testdata/meps_det_2_5km_20200316T00Z_ringhals.nc x_wind_ml && \
	./testGravSettling && \
	./testDryDep && \
	python3 -m unittest snap_forward_test.py


bsnap_naccident: $(BOBJ) $(MODELOBJ)
	${F77} $(F77FLAGS) $(BOBJ) $(MODELOBJ) $(BLIBS) -o bsnap_naccident


find_parameters_fi_test: find_parameters_fi_test.o $(MODELOBJ)
	${F77} $(F77FLAGS) $(MODELOBJ) $< $(BLIBS) -o find_parameters_fi_test

testDateCalc: testDateCalc.o $(MODELOBJ)
	${F77} $(F77FLAGS) $< $(MODELOBJ) $(BLIBS) -o $@

test_array_utils: test_array_utils.o $(MODELOBJ)
	${F77} $(F77FLAGS) $(MODELOBJ) $< $(BLIBS) -o $@

testGravSettling: testGravSettling.o $(MODELOBJ)
	${F77} $(F77FLAGS) $(MODELOBJ) $< $(BLIBS) -o $@

testDryDep: testDryDep.o $(MODELOBJ)
	${F77} $(F77FLAGS) $(MODELOBJ) $< $(BLIBS) -o $@

clean: clean_links
	rm -f *.o *~ snap_batch_copy.F90 find_parameters_fi_test bsnap_naccident data/snap.nc data/snap_ecemep.nc data/snap.log testDateCalc test_array_utils
	rm -rf __pycache__

distclean: clean
	rm -f $(TESTFILES)

find_parameters_fi_test.o: find_parameters_fi_test.f90 $(MODELOBJ)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<

testDateCalc.o: testDateCalc.f90 $(MODELOBJ)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<

test_array_utils.o: test_array_utils.f90 $(MODELOBJ)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<

testGravSettling.o: testGravSettling.f90 $(MODELOBJ)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<

testDryDep.o: testDryDep.f90 $(MODELOBJ)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<

.PHONY: clean distclean test install
