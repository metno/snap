add_library(milib
    chcase.f
    earthr.f
    getvar.f
    gridpar.f
    hrdiff.f
    keywrd.f
    lam2sph.f
    lenstr.f
    mapfield.f
    mer2sph.f
    pol2sph.f
    prhelp.f
    rlunit.f
    rmfile.f
    sph2rot.f
    termchar.f
    vtime.f
    xyconvert.f
)

# milib is not standards compliant
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
