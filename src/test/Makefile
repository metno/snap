# includefile contains Compiler definitions etc.
include ../current.mk

F77FLAGS += -DACCIDENT

all:

include ../common/snap.mk

# export FIMEXLIB
.EXPORT_ALL_VARIABLES:

install:

test: find_parameters_fi_test bsnap_naccident
	./find_parameters_fi_test https://thredds.met.no/thredds/dodsC/metusers/heikok/snap_testdata/meteo20200415_00_ringhals.nc u_wind && \
	./find_parameters_fi_test https://thredds.met.no/thredds/dodsC/metusers/heikok/snap_testdata/meps_det_2_5km_20200316T00Z_ringhals.nc x_wind_ml && \
	python3 -m unittest snap_forward_test.py


bsnap_naccident: $(BOBJ) $(MODELOBJ)
	${F77} $(F77FLAGS) $(BOBJ) $(MODELOBJ) $(BLIBS) -o bsnap_naccident


find_parameters_fi_test: find_parameters_fi_test.o $(MODELOBJ)
	${F77} $(F77FLAGS) $(MODELOBJ) $< $(BLIBS) -o find_parameters_fi_test

testDateCalc: testDateCalc.o
	${F77} $(F77FLAGS) $< -o find_parameters_fi_test


clean: clean_links
	rm -f *.o *~ snap_batch_copy.F90 find_parameters_fi_test bsnap_naccident data/snap.nc data/snap_ecemep.nc data/snap.log testDateCalc
	rm -rf __pycache__

distclean: clean
    
find_parameters_fi_test.o: find_parameters_fi_test.f90 $(MODELOBJ)
	${F77} -c ${F77FLAGS} $(INCLUDES) $<

testDateCalc.o: testDateCalc.f90
	${F77} -c ${F77FLAGS} $(INCLUDES) $<


.PHONY: clean distclean test install
