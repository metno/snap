# includefile contains Compiler definitions etc.
include ../current.mk

all: bsnap_naccident

include ../common/snap.mk

bsnap_naccident: $(BOBJ) $(MODELOBJ)
	${F77} $(F77FLAGS) $(BOBJ) $(MODELOBJ) $(BLIBS) -o bsnap_naccident


clean: clean_links
	rm -f *.o *~ *__genmod.*

distclean: clean
	rm -f bsnap_naccident create_naccident_input create_naccident_output

install: bsnap_naccident ../../utils/SnapPy/Snappy/AddToa.py
	cp bsnap_naccident $(BINDIR)/
	cp ../../utils/SnapPy/Snappy/AddToa.py $(BINDIR)/snapAddToa
	chmod 755 $(BINDIR)/snapAddToa


.PHONY: all clean distclean install
