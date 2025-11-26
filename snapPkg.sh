#! /bin/bash

set -e

export VERSION=2.5.7
#VERSION_=`echo -n $VERSION | tr '.' '_'`

# We do not have a way of cross-compiling,
# current platform is the one compiled for
PLATFORM=$(lsb_release --codename --short)
echo $PLATFORM

cd src || exit 1
rm --force current.mk
if [ $PLATFORM = bionic ]; then
    ln --symbolic ubuntuBionic.mk current.mk
elif [ $PLATFORM = jammy ]; then
    export SNAP_FIMEX_VERSION=1.9
    ln --symbolic gcc_pkgconfig.mk current.mk
elif [ $PLATFORM = noble ]; then
    export SNAP_FIMEX_VERSION=2.1
    ln --symbolic gcc_pkgconfig.mk current.mk
fi
make clean
make dist
cd .. || exit 1
tar xvfz snap-${VERSION}.tgz
mv snap-${VERSION}.tgz bsnap_${VERSION}.orig.tar.gz
cd snap-${VERSION} || exit 1
cp -r ../debian.${PLATFORM} .
mv debian.${PLATFORM} debian

export DEBEMAIL=${USER}@met.no
FULLVERSION=${VERSION}-1
dch --package bsnap --newversion ${FULLVERSION} "Fix NaN in iodine ddep"
dch --release ""
export DEB_BUILD_OPTIONS='nostrip'
export BINDIR="../debian/bsnap/usr/bin/"
debuild --preserve-envvar VERSION --preserve-envvar BINDIR --preserve-envvar SNAP_FIMEX_VERSION -us -uc -sa

# To publish:
# dupload --no --to $PLATFORM bsnap_$FULLVERSION_amd64.changes
