find_package(PkgConfig QUIET REQUIRED)

set(_find_all OFF)
set(_find_c OFF)
set(_find_fortran OFF)

foreach(COMP ${netcdf_FIND_COMPONENTS})
    if(${COMP} STREQUAL Fortran)
        set(_find_fortran ON)
        set(_find_all OFF)
    elseif(${COMP} STREQUAL C)
        set(_find_c ON)
        set(_find_ALL OFF)
    else()
        if (NOT ${netcdf_FIND_QUIETLY})
            message(STATUS "Unknown component ${COMP}")
        endif()
    endif()
endforeach()

if(NOT netcdf_FIND_COMPONENTS)
    set(_find_all ON)
endif()

if(_find_all OR _find_c)
    pkg_check_modules(PC_Config_C QUIET netcdf)

    find_path(netcdf_INCLUDE_DIR_C
        NAMES netcdf.h
        PATHS /usr/include "${PC_Config_C_INCLUDE_DIRS}" "$ENV{NETCDF_DIR}/include"
    )

    find_library(netcdf_LIBRARY_NAMES_C
        NAMES netcdf
        PATHS "${PC_Config_LIBRARY_NAMES}" "$ENV{NETCDF_DIR}/lib"
    )

    set(netcdf_VERSION_C "${PC_Config_C_VERSION}")

    if ("${netcdf_INCLUDE_DIR_C}" STREQUAL "netcdf_INCLUDE_DIR_C-NOTFOUND" OR
        "${netcdf_LIBRARY_NAMES_C}" STREQUAL "netcdf_LIBRARY_NAMES_C-NOTFOUND" OR
        "${netcdf_VERSION_C}" STREQUAL "")
        if ("${netcdf_FIND_REQUIRED}" OR "${netcdf_FIND_REQUIRED_C}")
            message(FATAL_ERROR "Netcdf::C not found")
        else()
            if ("${netcdf_FIND_QUIETLY}" OR "${netcdf_FIND_QUIETLY_C}")
                # Do nothing
            else()
                message(WARNING "Netcdf::C not found")
            endif()
        endif()
    else()
        set(netcdf_FOUND TRUE)
        set(netcdf_FOUND_C TRUE)
        mark_as_advanced(
            netcdf_FOUND
            netcdf_INCLUDE_DIR_C
            netcdf_LIBRARY_NAMES_C
            netcdf_VERSION_C
        )

        if (netcdf_FOUND_C AND NOT TARGET netcdf::C)
            add_library(netcdf::C UNKNOWN IMPORTED)
            set_target_properties(netcdf::C
                PROPERTIES
                    INTERFACE_INCLUDE_DIRECTORIES "${netcdf_INCLUDE_DIR_C}"
                    IMPORTED_LINK_INTERFACE_LANGUAGES "C"
                    IMPORTED_LOCATION "${netcdf_LIBRARY_NAMES_C}"
            )
        endif()
    endif()
endif()

if(_find_all OR _find_fortran)
    pkg_check_modules(PC_Config_Fortran QUIET netcdf-fortran)

    find_path(netcdf_INCLUDE_DIR_Fortran
        NAMES netcdf.mod
        PATHS /usr/include "${PC_Config_Fortran_INCLUDE_DIRS}" "$ENV{NETCDF_DIR}/include"
    )

    find_library(netcdf_LIBRARY_NAMES_Fortran
        NAMES netcdff
        PATHS "${PC_Config_Fortran_LIBRARY_NAMES}" "$ENV{NETCDF_DIR}/lib"
    )

    set(netcdf_VERSION_Fortran "${PC_Config_Fortran_VERSION}")

    if ("${netcdf_INCLUDE_DIR_Fortran}" STREQUAL "netcdf_INCLUDE_DIR_Fortran-NOTFOUND" OR
        "${netcdf_LIBRARY_NAMES_Fortran}" STREQUAL "netcdf_LIBRARY_NAMES_Fortran-NOTFOUND")
        if ("${netcdf_FIND_REQUIRED}" OR "${netcdf_FIND_REQUIRED_Fortran}")
            message(FATAL_ERROR "Netcdf::Fortran not found")
        else()
            if ("${netcdf_FIND_QUIETLY}" OR "${netcdf_FIND_QUIETLY_Fortran}")
                # Do nothing
            else()
                message(WARNING "Netcdf::Fortran not found")
            endif()
        endif()
    else()
        set(netcdf_FOUND TRUE)
        set(netcdf_FOUND_Fortran TRUE)
        mark_as_advanced(
            netcdf_FOUND
            netcdf_INCLUDE_DIR_Fortran
            netcdf_LIBRARY_NAMES_Fortran
            netcdf_VERSION_Fortran
        )


        if (netcdf_FOUND_Fortran AND NOT TARGET netcdf::Fortran)
            add_library(netcdf::Fortran UNKNOWN IMPORTED)
            set_target_properties(netcdf::Fortran
                PROPERTIES
                    INTERFACE_INCLUDE_DIRECTORIES "${netcdf_INCLUDE_DIR_Fortran}"
                    IMPORTED_LINK_INTERFACE_LANGUAGES "Fortran"
                    IMPORTED_LOCATION "${netcdf_LIBRARY_NAMES_Fortran}"
            )
        endif()
    endif()
endif()
