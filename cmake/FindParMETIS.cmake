find_library(parmetis_lib NAMES libparmetis.a libparmetis.so libparmetis.dylib HINTS ENV METIS_PATH PATH_SUFFIXES lib)
find_path(parmetis_inc parmetis.h HINTS ENV METIS_PATH PATH_SUFFIXES include)

find_package(METIS REQUIRED)

add_library(ParMETIS::ParMETIS STATIC IMPORTED)

set_target_properties(ParMETIS::ParMETIS PROPERTIES
  IMPORTED_LOCATION "${parmetis_lib}"
  INTERFACE_INCLUDE_DIRECTORIES "${parmetis_inc}")

target_link_libraries(ParMETIS::ParMETIS INTERFACE METIS::METIS)

## Finalize find_package
include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    ${CMAKE_FIND_PACKAGE_NAME}
    REQUIRED_VARS parmetis_lib
                  parmetis_inc)

message(STATUS "Found ParMETIS: ${parmetis_lib}")
