VERSION=2.1.0
#VERSION_=`echo -n $VERSION | tr '.' '_'`
export VERSION
PLATFORM=bionic
#svn copy https://svn.met.no/snap/trunk \
#     https://svn.met.no/snap/tags/$VERSION -m "Release $VERSION" || exit 1
#git checkout -b "version${VERSION_}"
#git commit -a -m "Version $VERSION"
#git push origin "version${VERSION_}"
#git checkout master
#git merge version${VERSION_}
cd src || exit 1
make dist
cd .. || exit 1
tar xvfz snap-${VERSION}.tgz
mv snap-${VERSION}.tgz bsnap_${VERSION}.orig.tar.gz
cd snap-${VERSION} || exit 1
cp -r ../debian.${PLATFORM} .
mv debian.${PLATFORM} debian
dch -v ${VERSION}-1 "Add aircraft doserate"
dch -r ""
export DEB_BUILD_OPTIONS='nostrip'
debuild --preserve-envvar VERSION -us -uc -sa
