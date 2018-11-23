add_subdirectory(common)
add_subdirectory(naccident)
add_subdirectory(nbomb)
add_subdirectory(traj)

if (${BUILD_TESTS})
    enable_testing()
    add_subdirectory(test)
endif()
