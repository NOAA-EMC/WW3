if(USE_SHARED_SCOTCH)
  set(LIB_EXT so)
  set(LIBRARY_TYPE "Shared")
else()
  set(LIB_EXT a)
  set(LIBRARY_TYPE "Static")
endif()

message(STATUS "Searching for ${LIBRARY_TYPE} SCOTCH libraries.")

message(STATUS "Searching for PTSCOTCHparmetis library ...")
find_library(ptscotchparmetis_lib NAMES libptscotchparmetisv3.${LIB_EXT}  HINTS ENV SCOTCH_PATH PATH_SUFFIXES lib)
find_path(ptscotchparmetis_inc parmetis.h HINTS ENV SCOTCH_PATH PATH_SUFFIXES include)

message(STATUS "Searching for SCOTCH library ...")
find_library(scotch_lib NAMES libscotch.${LIB_EXT} HINTS ENV SCOTCH_PATH PATH_SUFFIXES lib)
find_path(scotch_inc scotch.h HINTS ENV SCOTCH_PATH PATH_SUFFIXES include)

message(STATUS "Searching for PTSCOTCH library ...")
find_library(ptscotch_lib NAMES libptscotch.${LIB_EXT} HINTS ENV SCOTCH_PATH PATH_SUFFIXES lib)
find_path(ptscotch_inc ptscotch.h HINTS ENV SCOTCH_PATH PATH_SUFFIXES include)

message(STATUS "Searching for SCOTCHerr library ...")
find_library(scotcherr_lib NAMES libscotcherr.${LIB_EXT} HINTS ENV SCOTCH_PATH PATH_SUFFIXES lib)
find_path(scotcherr_inc scotch.h HINTS ENV SCOTCH_PATH PATH_SUFFIXES include)

message(STATUS "Searching for PTSCOTCHerr library ...")
find_library(ptscotcherr_lib NAMES libptscotcherr.${LIB_EXT} HINTS ENV SCOTCH_PATH PATH_SUFFIXES lib)
find_path(ptscotcherr_inc ptscotch.h HINTS ENV SCOTCH_PATH PATH_SUFFIXES include)

set(required_libs ${ptscotchparmetis_lib} ${scotch_lib} ${ptscotch_lib} ${scotcherr_lib} ${ptscotcherr_lib})

foreach(lib ${required_libs})
  string(REGEX REPLACE "-NOTFOUND$" "" lib_path "${lib}")
  if(NOT EXISTS ${lib_path})
    list(APPEND not_found_libs "${lib_path}.${LIB_EXT}")
  endif()
endforeach()

if(not_found_libs)
  message(FATAL_ERROR "The following libraries were not found:\n${not_found_libs}\nPlease ensure the required libraries are available.")
endif()


if(USE_SHARED_SCOTCH)
  add_library(PTSCOTCHparmetis::PTSCOTCHparmetis SHARED IMPORTED)
  add_library(SCOTCH::SCOTCH SHARED IMPORTED)
  add_library(PTSCOTCH::PTSCOTCH SHARED IMPORTED)
  add_library(SCOTCHerr::SCOTCHerr SHARED IMPORTED)
  add_library(PTSCOTCHerr::PTSCOTCHerr SHARED IMPORTED)
else()
  add_library(PTSCOTCHparmetis::PTSCOTCHparmetis STATIC IMPORTED)
  add_library(SCOTCH::SCOTCH STATIC IMPORTED)
  add_library(PTSCOTCH::PTSCOTCH STATIC IMPORTED)
  add_library(SCOTCHerr::SCOTCHerr STATIC IMPORTED)
  add_library(PTSCOTCHerr::PTSCOTCHerr STATIC IMPORTED)
endif()

set_target_properties(SCOTCH::SCOTCH PROPERTIES
  IMPORTED_LOCATION "${scotch_lib}"
  INTERFACE_INCLUDE_DIRECTORIES "${scotch_inc}")

set_target_properties(SCOTCHerr::SCOTCHerr PROPERTIES
  IMPORTED_LOCATION "${scotcherr_lib}"
  INTERFACE_INCLUDE_DIRECTORIES "${scotcherr_inc}")

set_target_properties(PTSCOTCH::PTSCOTCH PROPERTIES
  IMPORTED_LOCATION "${ptscotch_lib}"
  INTERFACE_INCLUDE_DIRECTORIES "${ptscotch_inc}")

set_target_properties(PTSCOTCHerr::PTSCOTCHerr PROPERTIES
  IMPORTED_LOCATION "${ptscotcherr_lib}"
  INTERFACE_INCLUDE_DIRECTORIES "${ptscotcherr_inc}")

set_target_properties(PTSCOTCHparmetis::PTSCOTCHparmetis PROPERTIES
  IMPORTED_LOCATION "${ptscotchparmetis_lib}"
  INTERFACE_INCLUDE_DIRECTORIES "${ptscotchparmetis_inc}")


## Interfaces and links
target_link_libraries(PTSCOTCHparmetis::PTSCOTCHparmetis INTERFACE PTSCOTCH::PTSCOTCH PTSCOTCHerr::PTSCOTCHerr  SCOTCH::SCOTCH)


## Finalize find_package
include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    ${CMAKE_FIND_PACKAGE_NAME}
    REQUIRED_VARS scotch_lib
    scotch_inc)

find_package_handle_standard_args(
    ${CMAKE_FIND_PACKAGE_NAME}
    REQUIRED_VARS ptscotch_lib
    ptscotch_inc)

find_package_handle_standard_args(
    ${CMAKE_FIND_PACKAGE_NAME}
    REQUIRED_VARS scotcherr_lib
    scotcherr_inc)

find_package_handle_standard_args(
    ${CMAKE_FIND_PACKAGE_NAME}
    REQUIRED_VARS ptscotcherr_lib
    ptscotcherr_inc)

find_package_handle_standard_args(
    ${CMAKE_FIND_PACKAGE_NAME}
    REQUIRED_VARS ptscotchparmetis_lib
    ptscotchparmetis_inc)

message(STATUS "Found SCOTCH: ${scotch_lib}")
message(STATUS "Found PTSCOTCH: ${ptscotch_lib}")
message(STATUS "Found SCOTCHerr: ${scotcherr_lib}")
message(STATUS "Found PTSCOTCHerr: ${ptscotcherr_lib}")
message(STATUS "Found PTSCOTCHparmetis: ${ptscotchparmetis_lib}")
