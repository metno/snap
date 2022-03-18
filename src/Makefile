# Version from environment, fall back on git versions
VERSION ?= $(shell ./version.py 2>/dev/null)
export VERSION


all:
	@$(MAKE) -C naccident;

test:
	@$(MAKE) -C test test;

clean:
	@$(MAKE) -C naccident clean;

dist: clean
	cp -R . ../snap-$(VERSION); cd ..; \
	rm snap-$(VERSION)/current.mk; \
	tar cvfz snap-$(VERSION).tgz snap-$(VERSION); \
	rm -r snap-$(VERSION)

install:
	@$(MAKE) -C naccident install;

.PHONY: all test clean dist install
