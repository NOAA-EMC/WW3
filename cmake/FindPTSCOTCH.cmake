message(STATUS "Searching for PTSCOTCH library ...")

find_library(ptscotch_lib NAMES libptscotch.a HINTS ENV SCOTCH_PATH PATH_SUFFIXES lib)
find_path(ptscotch_inc ptscotch.h HINTS ENV SCOTCH_PATH PATH_SUFFIXES include)

add_library(PTSCOTCH::PTSCOTCH STATIC IMPORTED)

set_target_properties(PTSCOTCH::PTSCOTCH PROPERTIES
  IMPORTED_LOCATION "${ptscotch_lib}"
  INTERFACE_INCLUDE_DIRECTORIES "${ptscotch_inc}")

## Finalize find_package
include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    ${CMAKE_FIND_PACKAGE_NAME}
    REQUIRED_VARS ptscotch_lib
    ptscotch_inc)

message(STATUS "Found PTSCOTCH: ${ptscotch_lib}")
