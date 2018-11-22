add_executable(naccident_input
    create_naccident_input.f
)

add_executable(naccident_output
    create_naccident_output.f
)

add_executable(bsnap_naccident
    ../common/snap.f90
    ../common/forwrd.f90
    ../common/forwrd_dx.f90
)
target_compile_definitions(bsnap_naccident
    PRIVATE
        NACCIDENT
)
target_link_libraries(bsnap_naccident
    PUBLIC
        common
)

install(
    TARGETS bsnap_naccident naccident_input naccident_output
    RUNTIME
    DESTINATION bin
)
