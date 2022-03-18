#! /bin/bash
# Installed from snap sources at https://github.com/metno/snap

# module load netcdf-fortran/4.4.4
# module load fimex
set -e

# This version must be the one used in SnapJobEC.py,
# this might require an update to SnapJobEC.py!
if [ -z "$1" ]; then
    VERSION="TEST"
else
    VERSION="$1"
fi
export VERSION

PREFIX=/modules/bionic/user-apps/SnapPy/"$VERSION"/
if [ -d "$PREFIX" ]; then
    echo "This version ('$VERSION') already exists, overwriting in 10 seconds"
    echo "Use ctrl-C to cancel"
    sleep 10
else
    mkdir "$PREFIX"
fi
export PREFIX

cd utils/SnapPy || exit 2

if [ ! -f "Snappy/resources/1-s2.0-S0146645313000110-mmc1.zip" ]; then
    wget https://ars.els-cdn.com/content/image/1-s2.0-S0146645313000110-mmc1.zip --output-document Snappy/resources/1-s2.0-S0146645313000110-mmc1.zip
fi

python3 setup.py install --prefix="$PREFIX"
cd ../.. || exit 2

cd src || exit 2
export BINDIR=$PREFIX/bin/
ln -sf ubuntuLustreSnappy.mk current.mk
make clean
make install

cat > /modules/MET/bionic/user-modules/SnapPy/"$VERSION"<< EOF
#%Module1.0#####################################################################
##
proc ModulesHelp { } {
   puts stderr "\tSnapPy"
}

module-whatis   "SnapPy - scripts and python modules to run snap"
set              root             /modules/bionic/user-apps/SnapPy/$VERSION
prepend-path     PATH            \$root/bin
prepend-path     PYTHONPATH      \$root/lib/python3.6/site-packages
EOF
