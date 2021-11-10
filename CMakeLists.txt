cmake_minimum_required(VERSION 3.19)

file(STRINGS "VERSION" pVersion)

project(
  WW3
  VERSION ${pVersion}
  LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
set(SWITCH "" CACHE STRING "Switch file")

# Enable these depending on switch/program
find_package(MPI)
find_package(g2)
find_package(PNG)
find_package(NetCDF 4.1.1 COMPONENTS C Fortran)
find_package(bacio 2.4.0)
find_package(w3nco 2.4.0)
find_package(ESMF 8.0.0 REQUIRED)

add_subdirectory(model)
