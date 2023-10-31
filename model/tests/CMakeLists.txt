# This is the CMake file for the model/tests directory in the WW3
# project. 
#
# Ed Hartnett, 10/14/23

# Some very small test files may be committed to the repo. This
# function copies such a data file to the build directory.
function(copy_test_data name)
  file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/data/${name}"
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
endfunction()

# Copy test data files that are in the repo to the build directory.
copy_test_data(ww3_outp.inp)
copy_test_data(out_pnt.points)

# Build and run the test.
add_executable(test_io test_io.F90)
target_link_libraries(test_io PRIVATE ww3_lib)
add_test(NAME test_io COMMAND test_io)

