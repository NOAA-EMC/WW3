# CMake build written by Kyle Gerheiser

# Requires CMake 3.19 for JSON strings
cmake_minimum_required(VERSION 3.19)

# Get VERSION from VERSION file
file(STRINGS "VERSION" pVersion)

project(
  WW3
  VERSION ${pVersion}
  LANGUAGES C Fortran)

get_directory_property(hasParent PARENT_DIRECTORY)
if(hasParent)
  # Unset flags that come from Parent (ie UFS or other coupled build)
  # for potential (-r8/-r4) conflict
  set(CMAKE_Fortran_FLAGS "")
  set(CMAKE_C_FLAGS "")
  remove_definitions(-DDEBUG)
endif()

set(MULTI_ESMF OFF CACHE BOOL "Build ww3_multi_esmf library")
set(NETCDF ON CACHE BOOL "Build NetCDF programs (requires NetCDF)")
set(ENDIAN "BIG" CACHE STRING "Endianness of unformatted output files. Valid values are 'BIG', 'LITTLE', 'NATIVE'.")
set(EXCLUDE_FIND "" CACHE STRING "Don't try and search for these libraries (assumd to be handled by the compiler/wrapper)")
set(ENABLE_DOCS OFF CACHE BOOL "Enable building of doxygen generated documentation")

# make sure all "exclude_find" entries are lower case
list(TRANSFORM EXCLUDE_FIND TOLOWER)

# Make Find modules visible to CMake
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Set switch file on command line when running CMake
set(SWITCH "" CACHE STRING "Switch file, either full path, relative path from location of top-level WW3/ dir, or a switch in model/bin")

# Search for switch file as a full path or in model/bin
if(EXISTS ${SWITCH})
  set(switch_file ${SWITCH})
else()
  set(switch_file ${CMAKE_CURRENT_SOURCE_DIR}/model/bin/switch_${SWITCH})
  if(NOT EXISTS ${switch_file})
    message(FATAL_ERROR "Switch file '${switch_file}' does not exist, set switch with -DSWITCH=<switch>")
  endif()
endif()

message(STATUS "Build with switch: ${switch_file}")
# Copy switch file to build dir
configure_file(${switch_file} ${CMAKE_BINARY_DIR}/switch COPYONLY)

# Re-configure CMake when switch changes
set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS ${CMAKE_BINARY_DIR}/switch)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Build type" FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

add_subdirectory(model)

# Turn on doxygen documentation
if (ENABLE_DOCS)
  list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/docs/cmake")
  include(EnableDoxygen)
  add_subdirectory(docs)
endif()

# Turn on unit testing.
#include(CTest)
#if(BUILD_TESTING)
#  add_subdirectory(regtests/unittests)
#endif()
