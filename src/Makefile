SUBDIRS = naccident nbomb traj volcano
# version from environment
VERSION ?= "1.0"

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

dist: clean
	cp -R . ../snap-$(VERSION); cd ..; \
	rm snap-$(VERSION)/current.mk; \
	tar cvfz snap-$(VERSION).tgz snap-$(VERSION); \
	rm -r snap-$(VERSION)

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
