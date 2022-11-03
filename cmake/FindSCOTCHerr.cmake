message(STATUS "Searching for SCOTCHerr library ...")

find_library(scotcherr_lib NAMES libscotcherr.a HINTS ENV SCOTCH_PATH PATH_SUFFIXES lib)
find_path(scotcherr_inc scotch.h HINTS ENV SCOTCH_PATH PATH_SUFFIXES include)

add_library(SCOTCHerr::SCOTCHerr STATIC IMPORTED)


set_target_properties(SCOTCHerr::SCOTCHerr PROPERTIES
  IMPORTED_LOCATION "${scotcherr_lib}"
  INTERFACE_INCLUDE_DIRECTORIES "${scotcherr_inc}")


## Finalize find_package
include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    ${CMAKE_FIND_PACKAGE_NAME}
    REQUIRED_VARS scotcherr_lib
    scotcherr_inc)

message(STATUS "Found SCOTCHerr: ${scotcherr_lib}")
