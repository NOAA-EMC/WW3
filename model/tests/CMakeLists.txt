# This is the CMake file for the model/tests directory in the WW3
# project. 
#
# Ed Hartnett, 10/14/23

add_executable(test_io test_io.F90)
target_link_libraries(test_io PRIVATE ww3_lib)
add_test(NAME test_io COMMAND test_io)

