SUBDIRS = naccident traj volcano
# Version from environment, fall back on git versions
VERSION ?= $(shell ./version.py)
export VERSION


all:
	for i in $(SUBDIRS); do \
		$(MAKE) -C $$i; \
	done

test:
	$(MAKE) -C test test;
	

clean:
	for i in $(SUBDIRS); do \
		$(MAKE) -C $$i clean; \
	done

dist: clean
	cp -R . ../snap-$(VERSION); cd ..; \
	rm snap-$(VERSION)/current.mk; \
	tar cvfz snap-$(VERSION).tgz snap-$(VERSION); \
	rm -r snap-$(VERSION)

install:
	for i in $(SUBDIRS); do \
		$(MAKE) -C $$i install; \
	done

naccident_install:
	for i in naccident; do \
		$(MAKE) -C $$i install; \
	done

.PHONY: all test clean dist install naccident_install
