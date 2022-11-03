message(STATUS "Searching for SCOTCH library ...")

find_library(scotch_lib NAMES libscotch.a HINTS ENV SCOTCH_PATH PATH_SUFFIXES lib)
find_path(scotch_inc scotch.h HINTS ENV SCOTCH_PATH PATH_SUFFIXES include)

add_library(SCOTCH::SCOTCH STATIC IMPORTED)

set_target_properties(SCOTCH::SCOTCH PROPERTIES
  IMPORTED_LOCATION "${scotch_lib}"
  INTERFACE_INCLUDE_DIRECTORIES "${scotch_inc}")

## Finalize find_package
include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    ${CMAKE_FIND_PACKAGE_NAME}
    REQUIRED_VARS scotch_lib
    scotch_inc)

message(STATUS "Found SCOTCH: ${scotch_lib}")

