# This is the CMake file for the model/tests directory in the WW3
# project. 
#
# Ed Hartnett, 10/14/23

add_executable(test_io test_io.F90)
add_test(NAME test_io COMMAND test_io)


