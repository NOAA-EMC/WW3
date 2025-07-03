# This is the CMake file for the model/tests directory in the WW3
# project. 
#
# Ed Hartnett, 10/14/23

# Some very small test files may be committed to the repo. This
# function copies such a data file to the build directory.
function(copy_test_data name)
  message(STATUS "Copying ${name} to ${CMAKE_CURRENT_BINARY_DIR}")
  file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/data/${name}"
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
endfunction()

# Some very small test files may be committed to the repo. This
# function copies such a data file to the build directory.
function(copy_test_data_2 srcname destname)
  message(STATUS "Copying ${srcname} to ${CMAKE_CURRENT_BINARY_DIR}/${destname}")
  file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/data/${srcname}"
    DESTINATION "${CMAKE_BINARY_DIR}"
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
  file(RENAME "${CMAKE_BINARY_DIR}/${srcname}" "${CMAKE_BINARY_DIR}/${destname}")
endfunction()

# Function to build and run a test.
function(unit_test name)
  add_executable(${name} ${name}.F90 ww3_unittest_util.F90)
  target_link_libraries(${name} PRIVATE ww3_lib)
  add_test(NAME ${name} COMMAND ${name})
endfunction()

# The binary file mod_def.ww3 is needed for testing. It's created by
# the ww3_grid utility.
#add_custom_target(create_mod_def TEST ../../bin/ww3_grid
#  COMMENT "Creating mod_def.ww3 file for testing")

# Copy test data files that are in the repo to the build directory.
copy_test_data(switch.io)
copy_test_data_2(ww3_grid.inp ww3_grid.inp)

# Build and run the tests.
unit_test(test_io_points_bin)
unit_test(test_io)
unit_test(test_io2)
unit_test(test_io3)
unit_test(test_io_restart_bin)


