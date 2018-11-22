find_package(PkgConfig QUIET REQUIRED)

set(find_c_version OFF)
set(find_fortran_version OFF)

foreach(COMP ${netcdf_FIND_COMPONENTS})
    if(${COMP} STREQUAL Fortran)
        set(find_fortran_version ON)
    elseif(${COMP} STREQUAL C)
        set(find_c_version ON)
    else()
        message(FATAL_ERROR "Unknown component ${COMP}")
    endif()
endforeach()

if(NOT netcdf_FIND_COMPONENTS)
    set(find_fortran_version ON)
    set(find_c_version ON)
endif()

if(find_c_version)
    pkg_check_modules(PC_Config_C QUIET netcdf)

    set(netcdfc_VERSION ${PC_Config_C_VERSION})

    find_path(netcdf_INCLUDE_DIR_C
        NAMES netcdf.h
        PATHS /usr/include ${PC_Config_C_INCLUDE_DIRS}
    )

    find_library(netcdf_LIBRARY_NAMES_C
        NAMES netcdf
        PATHS ${PC_Config_LIBRARY_NAMES}
    )

    find_package_handle_standard_args(netcdfc
        REQUIRED_VARS
            netcdf_INCLUDE_DIR_C
            netcdf_LIBRARY_NAMES_C
        VERSION_VAR netcdfc_VERSION
    )

    if (netcdfc_FOUND AND NOT TARGET netcdf::C)
        add_library(netcdf::C UNKNOWN IMPORTED)
        set_target_properties(netcdf::C
            PROPERTIES
                INTERFACE_INCLUDE_DIRECTORIES "${netcdf_INCLUDE_DIR_C}"
                IMPORTED_LINK_INTERFACE_LANGUAGES "C"
                IMPORTED_LOCATION "${netcdf_LIBRARY_NAMES_C}"
        )
    endif()
endif()

if(find_fortran_version)
    pkg_check_modules(PC_Config_Fortran QUIET netcdf-fortran)

    set(netcdff_VERSION ${PC_Config_Fortran_VERSION})

    find_path(netcdf_INCLUDE_DIR_FORTRAN
        NAMES netcdf.mod
        PATHS /usr/include ${PC_Config_Fortran_INCLUDE_DIRS}
    )

    find_library(netcdf_LIBRARY_NAMES_FORTRAN
        NAMES netcdff
        PATHS ${PC_Config_Fortran_LIBRARY_NAMES}
    )

    include(FindPackageHandleStandardArgs)
    find_package_handle_standard_args(netcdff
        REQUIRED_VARS
            netcdf_INCLUDE_DIR_FORTRAN
            netcdf_LIBRARY_NAMES_FORTRAN
        VERSION_VAR netcdff_VERSION
    )

    if (netcdff_FOUND AND NOT TARGET netcdf::Fortran)
        add_library(netcdf::Fortran UNKNOWN IMPORTED)
        set_target_properties(netcdf::Fortran
            PROPERTIES
                INTERFACE_INCLUDE_DIRECTORIES "${netcdf_INCLUDE_DIR_FORTRAN}"
                IMPORTED_LINK_INTERFACE_LANGUAGES "Fortran"
                IMPORTED_LOCATION "${netcdf_LIBRARY_NAMES_FORTRAN}"
        )
    endif()
endif()
