message(STATUS "Searching for PTSCOTCHparmetis library ...")
find_library(ptscotchparmetis_lib NAMES libptscotchparmetisv3.a  HINTS ENV SCOTCH_PATH PATH_SUFFIXES lib)
find_path(ptscotchparmetis_inc parmetis.h HINTS ENV SCOTCH_PATH PATH_SUFFIXES include)

add_library(PTSCOTCHparmetis::PTSCOTCHparmetis STATIC IMPORTED)

set_target_properties(PTSCOTCHparmetis::PTSCOTCHparmetis PROPERTIES
  IMPORTED_LOCATION "${ptscotchparmetis_lib}"
  INTERFACE_INCLUDE_DIRECTORIES "${ptscotchparmetis_inc}")

## Finalize find_package
include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    ${CMAKE_FIND_PACKAGE_NAME}
    REQUIRED_VARS ptscotchparmetis_lib
    ptscotchparmetis_inc)

message(STATUS "Found PTSCOTCHparmetis: ${ptscotchparmetis_lib}")

