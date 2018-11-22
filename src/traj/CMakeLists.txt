add_executable(create_traj_input
    create_traj_input.f
)

add_executable(bsnap_traj
    ../common/snap.f90
    ../common/forwrd.f90
    ../common/forwrd_dx.f90
)

target_compile_definitions(bsnap_traj
    PRIVATE
        TRAJ
)

target_link_libraries(bsnap_traj
    PRIVATE
        common
)

install(
    TARGETS bsnap_traj create_traj_input
    RUNTIME
    DESTINATION bin
)
