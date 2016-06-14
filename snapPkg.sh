VERSION=1.1
cd src
VERSION=$VERSION make dist
cd ..
tar xvfz snap-${VERSION}.tgz
mv snap-${VERSION}.tgz bsnap_${VERSION}.orig.tar.gz
cd snap-${VERSION}
cp -r ../debian .
dch -v 1.1-1 "new release"
dch -r ""
debuild -us -uc -sa


