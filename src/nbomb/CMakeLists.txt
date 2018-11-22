add_executable(create_bomb_input
    create_bomb_input.f
)

add_executable(bsnap_nbomb
    ../common/snap.f90
    ../common/forwrd.f90
    ../common/forwrd_dx.f90
)

target_compile_definitions(bsnap_nbomb
    PRIVATE
        BOMB
)

target_link_libraries(bsnap_nbomb
    common
)

install(
    TARGETS bsnap_nbomb create_bomb_input
    RUNTIME
    DESTINATION bin
)
