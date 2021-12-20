cmake_minimum_required(VERSION 3.15)

project(oasis3-mct
  LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "$ENV{WWATCH3_DIR}/../cmake")

find_package(MPI REQUIRED COMPONENTS Fortran)
find_package(NetCDF REQUIRED COMPONENTS Fortran)

set(WWATCH3_DIR $ENV{WWATCH3_DIR})
set(comp_mpi ${MPI_Fortran_COMPILER})
set(wwatch3_cc ${CMAKE_C_COMPILER})
set(NETCDF_CONFIG  ${NetCDF_C_CONFIG_EXECUTABLE})
set(optc_short "")
set(optl_short "")

configure_file(${CMAKE_SOURCE_DIR}/cmplr.tmpl ${CMAKE_SOURCE_DIR}/cmplr)

include(ExternalProject)
ExternalProject_Add(oasis
  PREFIX ${CMAKE_BINARY_DIR}
  SOURCE_DIR ${CMAKE_SOURCE_DIR}
  BUILD_IN_SOURCE TRUE
  BUILD_ALWAYS TRUE
  BUILD_COMMAND make realclean -f TopMakefileOasis3 && make -f TopMakefileOasis3
  INSTALL_COMMAND ""
  )
