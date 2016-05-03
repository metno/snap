VERSION=1.0
cd src
make dist
cd ..
tar xvfz snap-${VERSION}.tgz
mv snap-${VERSION}.tgz bsnap_1.0.orig.tar.gz
cd snap-${VERSION}
cp -r ../debian .
debuild -us -uc -sa


