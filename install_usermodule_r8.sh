#! /bin/bash

INTEL_COMPILER=/modules/MET/rhel8/user-modules/compiler/Intel2018

set -e

install_conda_env() {
    conda create --channel conda-forge --prefix "$1" --yes --file /dev/stdin <<EOF
python=3.10
cartopy
fimex>=1.9.3
netcdf4
matplotlib
nco
openssh
pkg-config
EOF
}

install_bdiana() {
    cat > "$1/bin/bdiana" <<EOF
#! /bin/bash
module use /modules/MET/rhel8/user-modules/
module load singularity/3.10.2

singularity exec \
    --no-home --bind /lustre/\${STORE}:/lustre/\${STORE} \
    --bind $1:$1 \
    --cleanenv \
    --env QT_QPA_PLATFORMTHEME='' \
    --env QT_QPA_FONTDIR='/usr/share/fonts/truetype' \
    --env QT_QPA_PLATFORM='offscreen' \
    /modules/singularityrepo/fou/kl/atom/bdiana_3.57.0.sif bdiana \$@
EOF
    chmod +x -- "$1/bin/bdiana"
}

install_netcdf_fortran() {
    module load $INTEL_COMPILER
    mkdir --parent tmp && cd tmp
    mkdir --parent build
    NETCDFF_VERSION=4.6.0
    wget https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v$NETCDFF_VERSION.tar.gz --continue
    tar -xf v$NETCDFF_VERSION.tar.gz

    cmake -S netcdf-fortran-$NETCDFF_VERSION -B build \
        -DCMAKE_INSTALL_PREFIX=$MODULE_PREFIX \
        -DCMAKE_INSTALL_LIBDIR=lib \
        -DENABLE_TESTS=false \
        -DCMAKE_BUILD_TYPE=RelWithDebInfo
    cmake --build build -j4
    cmake --install build
    cd ..
}

patch_fimex_pkgconfig() {
    fimex_pkgconfig_path=$MODULE_PREFIX/lib/pkgconfig/fimex.pc
    sed -i 's/^libdir.*/libdir=${prefix}\/lib/' $fimex_pkgconfig_path
}

install_snappy() {
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
}

install_bsnap() {
    cd src || exit 2
    ln --symbolic --force -- PPIifort.mk current.mk
    make clean
    env VERSION="$MODULE_VERSION" make BINDIR="$MODULE_PREFIX"/bin install
}

main() {
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

    if ! conda activate "$MODULE_PREFIX" ; then
        echo "Installing a fresh conda environment"
        install_conda_env "$MODULE_PREFIX"
        conda activate "$MODULE_PREFIX"
        patch_fimex_pkgconfig
        (install_netcdf_fortran)
        install_bdiana $MODULE_PREFIX
    else
        echo "Reusing conda module, remove '$MODULE_PREFIX' to run"
        echo "a clean installation"
    fi

    module load $INTEL_COMPILER

    install_snappy
    install_bsnap

    MODULEFILE=/modules/MET/rhel8/user-modules/fou-modules/SnapPy/"$MODULE_VERSION"

    sed "s+CONDAMODULEPATH+$MODULE_PREFIX+g" > "$MODULEFILE" <<EOF
#%Module1.0

set             conda_env               CONDAMODULEPATH

proc ModulesHelp { } {
    global conda_env
    puts stderr "\tSNAP in a conda environment (using intel compiler)"
    puts stderr "\tRead-only view of conda environment (\$conda_env)"
    puts stderr "\tDo not try to install using this module"
    puts stderr "\tTo activate the conda module run"
    puts stderr "\tconda activate \$conda_env"
}

module-whatis   "SnapPy"
module          load                    $INTEL_COMPILER
set             rootenv                 \$conda_env
setenv          UDUNITS2_XML_PATH       \$rootenv/share/udunits/udunits2.xml
setenv          PROJ_DATA               \$rootenv/share/proj
prepend-path    PATH                    \$rootenv/bin
prepend-path    LD_LIBRARY_PATH         \$rootenv/lib
prepend-path    MANPATH                 \$rootenv/man
prepend-path    PKG_CONFIG_PATH         \$rootenv/lib/pkgconfig
setenv          SNAP_MODULE             \$ModulesCurrentModulefile
EOF
}

main $@
