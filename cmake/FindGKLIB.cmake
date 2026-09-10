find_library(gklib_lib NAMES libGKlib.a libGKlib.so libGKlib.dylib HINTS ENV METIS_PATH PATH_SUFFIXES lib)
find_path(gklib_inc GKlib.h HINTS ENV METIS_PATH PATH_SUFFIXES include)

add_library(GKLIB::GKLIB STATIC IMPORTED)

set_target_properties(GKLIB::GKLIB PROPERTIES
  IMPORTED_LOCATION "${gklib_lib}"
  INTERFACE_INCLUDE_DIRECTORIES "${gklib_inc}")

include(FindPackageHandleStandardArgs)

if(gklib_inc MATCHES "NOTFOUND" OR gklib_lib MATCHES "NOTFOUND")
  set(${CMAKE_FIND_PACKAGE_NAME}_FOUND False)
else()
  find_package_handle_standard_args(
      ${CMAKE_FIND_PACKAGE_NAME}
      REQUIRED_VARS gklib_lib
      gklib_inc)
endif()
