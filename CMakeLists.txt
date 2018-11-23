cmake_minimum_required(VERSION 3.5.1)
project(SNAP LANGUAGES Fortran)

option(DR_HOOK "Build with the DR hook" OFF)
option(BUILD_DOCS "Build docs using Doxygen" ON)
option(BUILD_TESTS "Build tests" ON)

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

add_subdirectory(src)

if (${BUILD_DOCS})
    add_subdirectory(docs)
endif()

if (${BUILD_TESTS})
    enable_testing()
endif()
