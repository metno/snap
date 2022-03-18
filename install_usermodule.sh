#! /bin/bash
# Installed from snap sources at https://github.com/metno/snap

# module load netcdf-fortran/4.4.4
# module load fimex

# This version must be the one used in SnapJobEC.py,
# this might require an update to SnapJobEC.py!
VERSION=2.0.8
export VERSION

PREFIX=/modules/bionic/user-apps/SnapPy/"$VERSION"/
export PREFIX

cd utils/SnapPy || exit 2

if [ ! -f "Snappy/resources/1-s2.0-S0146645313000110-mmc1.zip" ]; then
    wget https://ars.els-cdn.com/content/image/1-s2.0-S0146645313000110-mmc1.zip --output-document Snappy/resources/1-s2.0-S0146645313000110-mmc1.zip
fi

python3 setup.py install --prefix=$PREFIX
cd ../.. || exit 2

cd src || exit 2
export BINDIR=$PREFIX/bin/
ln -sf ubuntuLustreSnappy.mk current.mk
make install
