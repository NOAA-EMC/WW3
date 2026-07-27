find_library(metis_lib NAMES libmetis.a libmetis.so libmetis.dylib HINTS ENV METIS_PATH PATH_SUFFIXES lib)
find_path(metis_inc metis.h HINTS ENV METIS_PATH PATH_SUFFIXES include)

add_library(METIS::METIS STATIC IMPORTED)

set_target_properties(METIS::METIS PROPERTIES
  IMPORTED_LOCATION "${metis_lib}"
  INTERFACE_INCLUDE_DIRECTORIES "${metis_inc}")

find_package(GKLIB)
if(NOT GKLIB_FOUND)
  message(STATUS "GKLIB not found")
  target_link_libraries(METIS::METIS)
else()
  target_link_libraries(METIS::METIS INTERFACE GKLIB::GKLIB)
endif()

## Finalize find_package
include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    ${CMAKE_FIND_PACKAGE_NAME}
    REQUIRED_VARS metis_lib
    metis_inc)
