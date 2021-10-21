cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(
  WW3
  VERSION ${pVersion}
  LANGUAGES C Fortran)

find_package(MPI)
find_package(g2)
find_package(Jasper)
find_package(PNG)
find_package(NetCDF)
find_package(bacio)
find_package(w3nco)
find_package(ESMF)
