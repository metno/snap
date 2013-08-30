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

naccident_install:
	for i in naccident; do \
		cd $$i && $(MAKE) install; \
		cd ..; \
	done

nbomb_install:
	for i in nbomb; do \
		cd $$i && $(MAKE) install; \
		cd ..; \
	done
