add_library(common
    allocateFields.f90
    argoswrite.f90
    bldp.f90
    c2fgetarg.f90
    c2fgetvar.f90
    checkDomain.f90
    compheight.f90
    copyfield.f90
    decayDeps.f90
    decay.f90
    drydep1.f90
    drydep2.f90
    edcomp.f90
    ensemble.f90
    epinterp.f90
    feltio_dummy.f90
    fileInfoML.f90
    # filesort.f90 # UNUSED
    filesort_nc.f90
    fldout.f90
    fldout_nc.f90
    # forwrd_dx.f90 # DEFINE(...)
    # forwrd.f90 # DEFINE(...)
    ftest.f90
    init_random_seed.f90
    om2edot.f90
    particleML.f90
    posint.f90
    readfd.f90
    readfield.f90
    readfield_nc.f90
    release.f90
    releasefile.f90
    rmpart.f90
    rwalk.f90
    snapargosML.f90
    snapdebugML.f90
    snapdimML.f90
    snapepsML.f90
    snapfilML.f90
    snapfldML.f90
    snapgrdML.f90
    snapmetML.f90
    snapparML.f90
    snapposML.f90
    snaptabML.f90
    tabcon.f90
    vgravtables.f90
    wetdep1.f90
    wetdep2.f90
)

add_library(milib
    milib/chcase.f
    milib/earthr.f
    milib/getvar.f
    milib/gridpar.f
    milib/hrdiff.f
    milib/keywrd.f
    milib/lam2sph.f
    milib/lenstr.f
    milib/mapfield.f
    milib/mer2sph.f
    milib/pol2sph.f
    milib/prhelp.f
    milib/rlunit.f
    milib/rmfile.f
    milib/sph2rot.f
    milib/termchar.f
    milib/vtime.f
    milib/xyconvert.f
)

target_compile_options(milib
    PRIVATE
        -std=gnu
        -fno-implicit-none
        -fno-module-private
)

set_target_properties(milib
    PROPERTIES
        Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/common_modules
)

add_library(datecalc
    dateCalc.f90
)

set_target_properties(datecalc
    PROPERTIES
        Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/common_modules
)

target_include_directories(datecalc
    PUBLIC $<TARGET_PROPERTY:datecalc,Fortran_MODULE_DIRECTORY>
)

target_link_libraries(common
    PRIVATE
        netcdf::Fortran
        datecalc
        milib
)

set_target_properties(common
    PROPERTIES
        Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/common_modules
)

target_include_directories(common
    PUBLIC
        $<TARGET_PROPERTY:common,Fortran_MODULE_DIRECTORY>
)
