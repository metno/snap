#! /bin/bash
# Installed from snap sources at https://github.com/metno/snap

# module load netcdf-fortran/4.4.4
# module load fimex

# This version must be the one used in SnapJobEC.py,
# this might require an update to SnapJobEC.py!
VERSION=2.0.8
export VERSION

cd utils/SnapPy || exit 2
python3 setup.py install --prefix=/modules/bionic/user-apps/SnapPy/"$VERSION"/
cd ../.. || exit 2

cd src || exit 2
ln -sf ubuntuLustreSnappy.mk current.mk
make install
