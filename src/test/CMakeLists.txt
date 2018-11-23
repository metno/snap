add_executable(test_datecalc
    testDateCalc.f90
)

target_link_libraries(test_datecalc
    PRIVATE
        datecalc
)

add_test(NAME datecalc
    COMMAND test_datecalc
)
