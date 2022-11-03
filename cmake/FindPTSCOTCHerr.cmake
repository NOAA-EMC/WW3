message(STATUS "Searching for PTSCOTCHerr library ...")

find_library(ptscotcherr_lib NAMES libptscotcherr.a HINTS ENV SCOTCH_PATH PATH_SUFFIXES lib)
find_path(ptscotcherr_inc ptscotch.h HINTS ENV SCOTCH_PATH PATH_SUFFIXES include)

add_library(PTSCOTCHerr::PTSCOTCHerr STATIC IMPORTED)

set_target_properties(PTSCOTCHerr::PTSCOTCHerr PROPERTIES
  IMPORTED_LOCATION "${ptscotcherr_lib}"
  INTERFACE_INCLUDE_DIRECTORIES "${ptscotcherr_inc}")

## Finalize find_package
include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    ${CMAKE_FIND_PACKAGE_NAME}
    REQUIRED_VARS ptscotcherr_lib
    ptscotcherr_inc)

message(STATUS "Found PTSCOTCHerr: ${ptscotcherr_lib}")
