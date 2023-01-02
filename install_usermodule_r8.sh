#! /bin/bash

set -e

if [ -z "$1" ]; then
    MODULE_VERSION="TEST"
    SNAPPY_VERSION="0.0.0.dev0"
else
    MODULE_VERSION="$1"
    SNAPPY_VERSION="$MODULE_VERSION"
fi

MODULE_PREFIX=/modules/rhel8/user-apps/fou-modules/SnapPy/"$MODULE_VERSION"/
if [ -d "$MODULE_PREFIX" ]; then
    echo "This version ('$MODULE_VERSION') already exists, overwriting in 10 seconds"
    echo "Use ctrl-C to cancel"
    sleep 10
fi

# Ignore implicit loading of $1
source /modules/rhel8/conda/install/bin/activate || true

conda create --channel conda-forge --prefix "$MODULE_PREFIX" --yes --file /dev/stdin <<EOF
python=3.10
cartopy
fimex>=1.9.3
netcdf4
matplotlib
nco
openssh
EOF
conda activate "$MODULE_PREFIX"

(
    # Ensure SnapPy contains dose conversion factors
    cd utils/SnapPy || exit 2

    if [ ! -f "Snappy/resources/1-s2.0-S0146645313000110-mmc1.zip" ]; then
        wget https://ars.els-cdn.com/content/image/1-s2.0-S0146645313000110-mmc1.zip --output-document Snappy/resources/1-s2.0-S0146645313000110-mmc1.zip
    fi
)

(
    export VERSION="$SNAPPY_VERSION"
    pip install ./utils/SnapPy/
)

cat > "$MODULE_PREFIX/bin/bdiana" <<EOF
#! /bin/bash
module use /modules/MET/rhel8/user-modules/
module load singularity/3.10.2

singularity exec \
    --no-home --bind /lustre/\${STORE}:/lustre/\${STORE} \
    --bind $MODULE_PREFIX:$MODULE_PREFIX \
    --cleanenv \
    --env QT_QPA_PLATFORMTHEME='' \
    --env QT_QPA_FONTDIR='/usr/share/fonts/truetype' \
    --env QT_QPA_PLATFORM='offscreen' \
    /modules/singularityrepo/fou/kl/atom/bdiana_3.57.0.sif bdiana \$@
EOF
chmod +x -- "$MODULE_PREFIX/bin/bdiana"

cd src || exit 2
module load /modules/MET/rhel8/user-modules/fou-modules/netcdf-fortran/4.6.0_conda_intel
ln --symbolic --force -- PPIifort.mk current.mk
make clean
env VERSION="$MODULE_VERSION" make BINDIR="$MODULE_PREFIX"/bin install

MODULEFILE=/modules/MET/rhel8/user-modules/fou-modules/SnapPy/"$MODULE_VERSION"

sed "s+CONDAMODULEPATH+$MODULE_PREFIX+g" > "$MODULEFILE" <<EOF
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
