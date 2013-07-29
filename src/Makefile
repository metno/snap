SUBDIRS = naccident nbomb traj volcano

all:
	for i in $(SUBDIRS); do \
		cd $$i && $(MAKE); \
		cd ..; \
	done

clean:
	for i in $(SUBDIRS); do \
		cd $$i && $(MAKE); \
		cd ..; \
	done

