cmake_minimum_required(VERSION 3.15)

project(oasis3-mct
  LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "$ENV{WWATCH3_DIR}/../cmake")

find_package(MPI REQUIRED COMPONENTS Fortran)
find_package(NetCDF MODULE REQUIRED COMPONENTS Fortran)

list(REVERSE NetCDF_LIBRARIES)
string(REPLACE ";" " " NetCDF_LIBRARIES_STR "${NetCDF_LIBRARIES}")

set(WWATCH3_DIR $ENV{WWATCH3_DIR})
set(comp_mpi ${MPI_Fortran_COMPILER})
set(wwatch3_cc ${CMAKE_C_COMPILER})
set(oasis_input_path $ENV{OASIS_INPUT_PATH})
set(oasis_work_path $ENV{OASIS_WORK_PATH})

# Turn on this argument mismatch flag for gfortran10.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch")
  endif()
endif()

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
