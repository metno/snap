# includefile contains Compiler definitions etc.
include ../current.mk

F77FLAGS += -DACCIDENT

all: 

test: find_parameters_fi_test
#	cp ../../utils/snapAddToa $(BINDIR)/


include ../common/snap.mk


find_parameters_fi_test: find_parameters_fi_test.o $(MODELOBJ)
	${F77} $(F77FLAGS) $(MODELOBJ) $< $(BLIBS) -o find_parameters_fi_test


clean: clean_links
	rm -f *.o *~ snap_batch_copy.F90 find_parameters_fi_test

distclean: clean
    
find_parameters_fi_test.o: find_parameters_fi_test.f90 snapmetML.o fimex.o
	${F77} -c ${F77FLAGS} $(INCLUDES) $<



.PHONY: clean distclean test
