SUBDIRS = naccident nbomb traj volcano
GIT_VERSION ?= git.$(shell git rev-parse --verify --short HEAD)
# Version from environment, fall back on commit hash
VERSION ?= $(GIT_VERSION)
export VERSION


all:
	for i in $(SUBDIRS); do \
		$(MAKE) -C $$i; \
	done

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

nbomb_install:
	for i in nbomb; do \
		$(MAKE) -C $$i install; \
	done
