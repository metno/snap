SUBDIRS = naccident nbomb traj volcano

all:
	for i in $(SUBDIRS); do \
		cd $$i && $(MAKE); \
		cd ..; \
	done

clean:
	for i in $(SUBDIRS); do \
		cd $$i && $(MAKE) clean; \
		cd ..; \
	done

install:
	for i in $(SUBDIRS); do \
		cd $$i && $(MAKE) install; \
		cd ..; \
	done
