# includefile contains Compiler definitions etc.
include ../current.mk

F77FLAGS += -DBOMB

all: bsnap_nbomb
#create_bomb_input

include ../common/snap.mk

create_bomb_input: create_bomb_input.o
	$(F77) ${F77FLAGS} -I. $< -o $@

bsnap_nbomb: $(BOBJ) $(MODELOBJ) milib.o
	$(F77) $(F77FLAGS) $(BOBJ) $(MODELOBJ) $(BLIBS) -o $@

clean: clean_links
	rm -f *.o *~ snap_batch_copy.* bsnap_nbomb create_bomb_input

install: bsnap_nbomb 
	cp $^ $(BINDIR)

create_bomb_input.o: create_bomb_input.f
	$(F77) ${F77FLAGS} -c -I. $< -o $@
