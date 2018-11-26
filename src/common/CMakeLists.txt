# This library contains all the shared files between the different versions of
# SNAP. Should any of these files be modified with a preprocessor directive
# (e.g. #ifdef NACCIDENT), the file must be moved out of this list and added
# to all the respective versions of SNAP.
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

target_compile_definitions(common
    PRIVATE
        $<$<BOOL:${USE_OPENMP}>:HAVE_OMP>
)

target_compile_options(common
    PRIVATE
        $<$<BOOL:${USE_OPENMP}>:${OpenMP_Fortran_FLAGS}>
)

add_subdirectory(milib)

target_link_libraries(common
    PRIVATE
        netcdf::Fortran
        datecalc
        milib
        $<$<BOOL:${USE_OPENMP}>:${OpenMP_Fortran_FLAGS}>
)

set_target_properties(common
    PROPERTIES
        Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/common_modules
)

target_include_directories(common
    PUBLIC
        $<TARGET_PROPERTY:common,Fortran_MODULE_DIRECTORY>
)
