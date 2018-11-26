cmake_minimum_required(VERSION 3.5.1)
project(SNAP LANGUAGES Fortran VERSION 3.10.0)

option(DR_HOOK "Build with the DR hook" OFF)
option(BUILD_DOCS "Create build target for auto-generated documentation (doxygen)" ON)
option(BUILD_TESTS "Create build target for tests" ON)
option(BUILD_PACKAGE "Create install target for packaging (.deb)" ON)
option(USE_OPENMP "Compile with openmp support" OFF)

set(CMAKE_VERBOSE_MAKEFILE OFF) # Set to ON to see all make invocations

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake)

add_compile_options(
    -cpp # Enable preprocessor
    -fopt-info-optimized-vec # Information regarding
    -Wall # Most common warnings
    -Wextra # Useful warnings
    # -fdefault-real-8 # Use f64 as default (prevents expensive casts)
    -fimplicit-none
    # -std=f2000
    # -pedantic # "Strict" adherence to the standard
    -fmodule-private # All modules are private by default

    # These options are only used in debug builds, and tries to catch
    # indexing faults and erroneus math
    $<$<CONFIG:Debug>:-fbounds-check>
    $<$<CONFIG:Debug>:-ffpe-trap=invalid,overflow,zero>

    # Release flags
    $<$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>>:-O3>
    $<$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>>:-march=native>
    $<$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>>:-fno-math-errno>
    $<$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>>:-ftree-vectorize>
)

# Only the fortran component is used in this program
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
    # Tests can be run with `ctest` (or `make test` if using the make generator)
    enable_testing()
endif()

if (${BUILD_PACKAGE})
    # SNAP can be packaged with `cpack`
    set(CPACK_PACKAGE_VENDOR "Norwegian Meteorological Institute")
    set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Severe Nuclear Accident Programme\n Dispersion model specialized for nuclear accidents.")
    set(CPACK_PACKAGE_DESCRIPTION "Dispersion model specialized for nuclear accidents")
    set(CPACK_PACKAGE_CONTACT "Heiko Klein <heiko.klein@met.no>")
    set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
    set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
    set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})

    # Debian specific options
    # set(CPACK_DEBIAN_PACKAGE_DEPENDS "netcdf-fortran")
    set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON) # Automatic package dependencies
    set(CPACK_DEBIAN_PACKAGE_SECTION "science")
    set(CPACK_DEBIAN_PACKAGE_HOMEPAGE "https://github.com/metno/snap")
    set(CPACK_DEBIAN_PACKAGE_CONTROL_STRICT_PERMISSION TRUE) # Changes file permissions
    set(CPACK_DEBIAN_PACKAGE_SOURCE "bsnap")
    include(CPack)
endif()
