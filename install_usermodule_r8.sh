#! /bin/bash

set -e

install_conda_env() {
    conda create --channel conda-forge --prefix "$1" --yes --file /dev/stdin <<EOF
python=3.10
cartopy=0.21.1
fimex=1.9.5
netcdf4=1.6.3
matplotlib=3.7.1
nco=5.1.4
openssh=9.2p1
openssl=3.0.8
pkg-config=0.29.2
netcdf-fortran=4.6.0
gfortran=12.2.0
EOF
# scipy installed internally (cartopy?) Used for only for smoothing plots, should be
# added when updating the dependencies
}

install_bdiana() {
    cat > "$1/bin/bdiana" <<EOF
#! /bin/bash
module use /modules/MET/rhel8/user-modules/
module load --silent singularity/3.11.3

singularity exec \
    --no-home --bind /lustre/\${STORE}:/lustre/\${STORE} \
    --bind $1:$1 \
    --cleanenv \
    --env QT_QPA_PLATFORMTHEME='' \
    --env QT_QPA_FONTDIR='/usr/share/fonts/truetype' \
    --env QT_QPA_PLATFORM='offscreen' \
    /modules/singularityrepo/fou/kl/atom/diana_3.58.0.sif bdiana \$@
EOF
    chmod +x -- "$1/bin/bdiana"
}

patch_fimex_pkgconfig() {
    fimex_pkgconfig_path=$MODULE_PREFIX/lib/pkgconfig/fimex.pc
    # shellcheck disable=SC2016,SC2086
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
        export VERSION="0.0.0.dev0"
        # Pip install messes up shebang: https://github.com/pypa/setuptools/issues/494
        # pip install ./utils/SnapPy/
        cd utils/SnapPy || exit 2
        python3 setup.py install
    )
}

install_bsnap() {
    cd src || exit 2
    ln --symbolic --force -- gcc_pkgconfig.mk current.mk
    make clean
    env VERSION="$MODULE_VERSION" make -j 4 BINDIR="$MODULE_PREFIX"/bin install

    install_bdiana "$MODULE_PREFIX"
}

install_baseenv() {
    MODULE_VERSION="$1"
    FORCEOPT="$2"

    MODULE_PREFIX=/modules/rhel8/user-apps/fou-modules/SnapPy/"$MODULE_VERSION"/
    if [ -d "$MODULE_PREFIX" ]; then
        echo "This module ('$MODULE_VERSION') already exists" >/dev/stderr
        if [ "$FORCEOPT" == "--force" ]; then
          echo "Overwriting module since --force was specified" >/dev/stderr
        else
        	exit 2
        fi
    else
        echo "Installing a fresh conda environment"
    fi

    source /modules/rhel8/conda/install/etc/profile.d/conda.sh

    install_conda_env "$MODULE_PREFIX"
    conda activate "$MODULE_PREFIX"
    patch_fimex_pkgconfig

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

module-whatis   "SnapPy - Base deps"
set             rootenv                 \$conda_env
setenv          UDUNITS2_XML_PATH       \$rootenv/share/udunits/udunits2.xml
setenv          PROJ_DATA               \$rootenv/share/proj
prepend-path    PATH                    \$rootenv/bin
prepend-path    LD_LIBRARY_PATH         \$rootenv/lib
prepend-path    MANPATH                 \$rootenv/man
prepend-path    PKG_CONFIG_PATH         \$rootenv/lib/pkgconfig
EOF
}


install_snap() {
    MODULE_VERSION="$1"
    BASE_MODULE_VERSION="$2"
    FORCEOPT="$3"
    BASE_MODULE_PREFIX=/modules/rhel8/user-apps/fou-modules/SnapPy/"$BASE_MODULE_VERSION"/

    source /modules/rhel8/conda/install/bin/activate "$BASE_MODULE_PREFIX"

    MODULE_PREFIX=/modules/rhel8/user-apps/fou-modules/SnapPy/"$MODULE_VERSION"/
    if [ -d "$MODULE_PREFIX" ]; then
        echo "Module $MODULE_PREFIX seems to already exists" >/dev/stderr
        if [ "$FORCEOPT" = "--force" ]; then
            echo "Overwriting module since --force was specified" >/dev/stderr
        else
            exit 2
        fi
    fi
    mkdir --parent -- "$MODULE_PREFIX/bin"

    python3 -m venv "$MODULE_PREFIX/" --system-site-packages
    source "$MODULE_PREFIX/bin/activate"
    install_snappy

    install_bsnap

    BASE_MODULEFILE=/modules/MET/rhel8/user-modules/fou-modules/SnapPy/"$BASE_MODULE_VERSION"
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
module          load                    $BASE_MODULEFILE
set             rootenv                 \$conda_env
prepend-path    PATH                    \$rootenv/bin
prepend-path    LD_LIBRARY_PATH         \$rootenv/lib
setenv          SNAP_MODULE             \$ModulesCurrentModulefile
EOF
}

FIXED_BASEENV=conda202305
case "${1:-help}" in
  install_baseenv)
    install_baseenv "${2:-TEST}" "${3:---no-force}"
    ;;
  install_snap)
    install_snap "${2:-TEST_SNAP}" "${FIXED_BASEENV}" "${3:---no-force}"
    ;;
  *)
    echo "Usage: ./install_usermodule_r8.sh <CMD>"
    echo "CMD: install_baseenv <BASEENVNAME>"
    echo "CMD: install_snap <ENVNAME>"
    echo "  with ENVNAME e.g. 2.3.3-dev0"
    echo " --force can be used with the above CMDs to overwrite modules"
    ;;
esac

exit
