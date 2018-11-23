cmake_minimum_required(VERSION 3.5.1)
project(SNAP LANGUAGES Fortran VERSION 3.10.0)

option(DR_HOOK "Build with the DR hook" OFF)
option(BUILD_DOCS "Create build target for docs" ON)
option(BUILD_TESTS "Create build target for tests" ON)
option(BUILD_PACKAGE "cpack support" ON)
option(USE_OPENMP "Compile with openmp support" OFF)

# set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake)

add_compile_options(
    -cpp # Enable preprocessor
    -fopt-info-optimized-vec
    -Wall
    -Wextra
    # -fdefault-real-8
    -fimplicit-none
    # -std=f95
    # -pedantic
    -fmodule-private
    $<$<CONFIG:Debug>:-fbounds-check>
    $<$<CONFIG:Debug>:-ffpe-trap=invalid,overflow,zero>
    $<$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>>:-O3>
    $<$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>>:-march=native>
    $<$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>>:-fno-math-errno>
    $<$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>>:-ftree-vectorize>
)

find_package(netcdf REQUIRED COMPONENTS Fortran)

if (${DR_HOOK})
    find_package(DR_HOOK REQUIRED)
endif()

if (${USE_OPENMP})
    find_package(OpenMP REQUIRED)
endif()

add_subdirectory(src)

if (${BUILD_DOCS})
    add_subdirectory(docs)
endif()

if (${BUILD_TESTS})
    enable_testing()
endif()

if (${BUILD_PACKAGE})
    set(CPACK_PACKAGE_VENDOR "Norwegian Meteorological Institute")
    set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Severe Nuclear Accident Programme\n Dispersion model specialized for nuclear accidents.")
    set(CPACK_PACKAGE_DESCRIPTION "Disp for nucl")
    set(CPACK_PACKAGE_CONTACT "Heiko Klein <heiko.klein@met.no>")
    set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
    set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
    set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})

    # Debian specifig options
    # set(CPACK_DEBIAN_PACKAGE_DEPENDS "netcdf-fortran")
    set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
    set(CPACK_DEBIAN_PACKAGE_SECTION "science")
    set(CPACK_DEBIAN_PACKAGE_HOMEPAGE "https://github.com/metno/snap")
    set(CPACK_DEBIAN_PACKAGE_CONTROL_STRICT_PERMISSION TRUE)
    set(CPACK_DEBIAN_PACKAGE_SOURCE "bsnap")
    include(CPack)
endif()
