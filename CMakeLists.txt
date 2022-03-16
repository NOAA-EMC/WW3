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
  # Unset flags that come from UFS because they (-r8/-r4) conflict
  # with Ww3
  set(CMAKE_Fortran_FLAGS "")
  set(CMAKE_C_FLAGS "")
endif()

set(valid_caps "MULTI_ESMF" "NUOPC_MESH")

set(UFS_CAP "" CACHE STRING "Valid options are ${valid_caps}")
set(NETCDF ON CACHE BOOL "Build NetCDF programs (requires NetCDF)")

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

if(UFS_CAP)
  if(NOT UFS_CAP IN_LIST valid_caps)
    message(FATAL_ERROR "Invalid UFS_CAP selection. Valids options are ${valid_caps}")
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
