# CMake build written by Kyle Gerheiser

# Requires CMake 3.19 for JSON strings
cmake_minimum_required(VERSION 3.19)

# Get VERSION from VERSION file
file(STRINGS "VERSION" pVersion)

project(
  WW3
  VERSION ${pVersion}
  LANGUAGES C Fortran)

# Make Find modules visible to CMake
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Set switch file on command line when running CMake
set(SWITCH "" CACHE STRING "Switch file")

add_subdirectory(model)
