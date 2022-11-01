#! /bin/bash

set -e

if [ -z "$1" ]; then
    VERSION="TEST"
    SNAPPY_VERSION="0.0.0.dev0"
else
    VERSION="$1"
    SNAPPY_VERSION="$VERSION"
fi
export VERSION

PREFIX=/modules/rhel8/user-apps/fou-modules/SnapPy/"$VERSION"/
if [ -d "$PREFIX" ]; then
    echo "This version ('$VERSION') already exists, overwriting in 10 seconds"
    echo "Use ctrl-C to cancel"
    sleep 10
else
    mkdir "$PREFIX"
fi
export PREFIX

source /modules/rhel8/conda/install/bin/activate

conda create --channel conda-forge --prefix "$PREFIX" --yes --file /dev/stdin <<EOF
python=3.10
cartopy
# fimex=1.9.0  # Regression in this fimex
fimex=1.8.1
netcdf4
matplotlib
nco
EOF
conda activate "$PREFIX"

(
    cd utils/SnapPy || exit 2

    if [ ! -f "Snappy/resources/1-s2.0-S0146645313000110-mmc1.zip" ]; then
        wget https://ars.els-cdn.com/content/image/1-s2.0-S0146645313000110-mmc1.zip --output-document Snappy/resources/1-s2.0-S0146645313000110-mmc1.zip
    fi
)

env VERSION="$SNAPPY_VERSION" pip install ./utils/SnapPy/

cd src || exit 2
module load /modules/MET/rhel8/user-modules/fou-modules/netcdf-fortran/4.6.0_conda_intel
ln --symbolic --force -- PPIifort.mk current.mk
make clean
make BINDIR="$PREFIX"/bin install

MODULEFILE=/modules/MET/rhel8/user-modules/fou-modules/SnapPy/"$VERSION"

sed "s+CONDAMODULEPATH+$PREFIX+g" > "$MODULEFILE" <<EOF
#%Module1.0

set             conda_env               CONDAMODULEPATH

proc ModulesHelp { } {
    global conda_env
    puts stderr "\tSnapPy using conda"
    puts stderr "\tRead-only view of conda environment (\$conda_env)"
    puts stderr "\tDo not try to install using this module"
    puts stderr "\tTo activate the conda module run"
    puts stderr "\tconda activate \$conda_env"
}

module-whatis   "SnapPy"
set             rootenv                 \$conda_env
setenv          UDUNITS2_XML_PATH       \$rootenv/share/udunits/udunits2.xml
setenv          PROJ_DATA               \$rootenv/share/proj
prepend-path    PATH                    \$rootenv/bin
prepend-path    LD_LIBRARY_PATH         \$rootenv/lib
prepend-path    MANPATH                 \$rootenv/man
prepend-path    PKG_CONFIG_PATH         \$rootenv/lib/pkgconfig
setenv          SNAP_MODULE             \$ModulesCurrentModulefile
EOF
