add_executable(bsnap_volcano
    ../common/snap.f90
    ../common/forwrd.f90
    ../common/forwrd_dx.f90
)

target_compile_definitions(bsnap_volcano
    PRIVATE
        VOLCANO
)

target_link_libraries(bsnap_volcano
    common
)

install(
    TARGETS bsnap_volcano
    RUNTIME
    DESTINATION bin
)
