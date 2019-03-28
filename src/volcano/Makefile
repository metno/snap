# includefile contains Compiler definitions etc.
include ../current.mk

F77FLAGS += -DVOLCANO

all: bsnap_volcano

include ../common/snap.mk

bsnap_volcano: $(BOBJ) $(MODELOBJ)
	$(F77) $(F77FLAGS) $(BOBJ) $(MODELOBJ) $(BLIBS) -o $@

clean: clean_links
	rm -f *.o *~ snap_batch_copy.* bsnap_volcano

install: bsnap_volcano
	cp $^ $(BINDIR)

