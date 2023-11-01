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

# Some very small test files may be committed to the repo. This
# function copies such a data file to the build directory.
function(copy_test_data_2 dir srcname destname)
  file(COPY "${CMAKE_SOURCE_DIR}/${dir}/${srcname}"
    DESTINATION "${CMAKE_CURRENT_BINARY_DIR}"
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
  file(RENAME "${CMAKE_CURRENT_BINARY_DIR}/${srcname}" "${CMAKE_CURRENT_BINARY_DIR}/${destname}")
endfunction()

# Copy test data files that are in the repo to the build directory.
copy_test_data(ww3_outp.inp)
copy_test_data(out_pnt.points)
copy_test_data_2(regtests/mww3_test_05/work_ST4_PR2_UQ_MPI mod_def.points mod_def.ww3)

# Build and run the test.
add_executable(test_io test_io.F90)
target_link_libraries(test_io PRIVATE ww3_lib)
add_test(NAME test_io COMMAND test_io)

