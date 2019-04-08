# includefile contains Compiler definitions etc.

include ../current.mk

F77FLAGS += -DACCIDENT

all: bsnap_naccident 

# create_naccident_input create_naccident_output

include ../common/snap.mk

create_naccident_input: create_naccident_input.o
	${F77} ${F77FLAGS} -I. $< -o $@

create_naccident_output: create_naccident_output.o
	${F77} ${F77FLAGS} -I. $< -o $@

bsnap_naccident: $(BOBJ) $(MODELOBJ) milib.o
	${F77} $(F77FLAGS) $(BOBJ) $(MODELOBJ) $(BLIBS) -o bsnap_naccident


clean: clean_links
	rm -f *.o *~ snap_batch_copy.* bsnap_naccident create_naccident_input create_naccident_output


#create_naccident_input create_naccident_output
install: bsnap_naccident ../../utils/snapAddToa
	install $^ $(BINDIR)/
#	cp ../../utils/snapAddToa $(BINDIR)/


create_naccident_input.o: create_naccident_input.f
	${F77} ${F77FLAGS} -c -I. $< -o $@

create_naccident_output.o: create_naccident_output.f
	${F77} ${F77FLAGS} -c -I. $< -o $@
	
