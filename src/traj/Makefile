# includefile contains Compiler definitions etc.
include ../current.mk

F77FLAGS += -DTRAJ

all: bsnap_traj
#create_traj_input

include ../common/snap.mk

snap_traj: $(CCOBJ) $(OBJ) $(MODELOBJ)
	$(CXX) $(CXXFLAGS) $(CCOBJ) $(OBJ) $(MODELOBJ) $(LIBS) -o $@

create_traj_input: create_traj_input.o
	$(F77) $< -o $@

bsnap_traj: $(BOBJ) $(MODELOBJ)
	$(F77) $(F77FLAGS) $(BOBJ) $(MODELOBJ) $(BLIBS) -o $@

clean: clean_links
	rm -f *.o *~ snap_batch_copy.* bsnap_traj snap_traj create_traj_input

install: bsnap_traj
	install $^ $(BINDIR)

create_traj_input.o: create_traj_input.f
	$(F77) -c $< -o $@
