# CMake Build

A CMake build is provided with WW3 to standardize and simplify the WW3 build process.

WW3 uses #ifdef directives to configure model options. These options are specified in a 'switch file' passed to CMake with `-DSWITCH=/switch/file` during the build. CMake parses the switch options and copies it into the build to determine which files to build, and what values to pass to the pre-processor.

Switch setting configurations are stored in [switches.json](./bin/switches.json). This file is read by CMake to check the validity of input switches.

The CMake build does not offer incremental builds. If the switch file changes, on the next `make`, CMake will re-trigger and build from scratch.

Requires CMake 3.19+

## Quick Start

```
export CC=icc
export FC=ifort
export NetCDF_DIR=/path/to/netcdf

git clone https://github.com/kgerheiser/WW3.git -b feature/cmake
cd WW3
mkdir build && cd build
cmake .. -DSWITCH=/path/to/switch_NCEP_st2 -DCMAKE_INSTALL_PREFIX=install
make
make install
```

## CMake Options

Options can be passed to CMake with `-D<option>`.

* SWITCH (required) - Absolute path to a switch file, or a switch file located in model/bin. CMake will detect changes to the source switch file and re-trigger the whole build if changed.

* MULTI_ESMF (optional) - Build the ww3_multi_esmf library. Requires ESMF.

* CMAKE_INSTALL_PREFIX (optional) - Standard CMake variable for install location when running `make install`. Executables/libraries are also located in build/bin if not running `make install`.

## Setting Compiler

CMake uses the standard `CC`, `FC`, and `CXX` envrionment variables for compiler detection. May need to set if CMake picks up the wrong compiler (system GCC instead of Intel, for example).

```
export CC=icc
export FC=ifort
```

Or if using MPI

```
export CC=mpicc
export FC=mpif90
```

## Libraries

WW3 has dependent libraries depending on switch configuration.

* MPI - Required when `MPI` in switch list

* NetCDF - Optionally searched for.

* OASIS3-MCT - Required when `OASIS` in switch list. CMake searches `OASISDIR` for library path.

* METIS/ParMETIS - Required when `PDLIB` in switch list.

* NCEPLIBS (g2, bacio, w3nco) - Required when `NCEP2` in switch list.

* ESMF - Required when `MULTI_ESMF=ON`

## Finding Libraries

CMake has a standardized way of searching for external libraries including default system paths such as `/usr/local`. 

If a library is located in a non-standard location CMake will search multiple locations including `CMAKE_PREFIX_PATH` (a semi-colon separated list), `<name>_DIR` or `<name>_ROOT`. 

```
# Can set the compiler to MPI wrappers to find MPI
export CC=mpicc
export FC=mpif90

# <name>_ROOT or <name>_DIR (case sensitive)
export NetCDF_ROOT=/path/to/netcdf
export ESMF_DIR=/path/to/esmf

# CMAKE_PREFIX_PATH is another option (can also be an env variable)
cmake .. -DCMAKE_PREFIX_PATH=/path/to/g2;/path/to/w3nco;/path/to/bacio

# OASIS is a special case and OASISDIR is used
export OASISDIR=/path/to/oasis
```

## Installation

Running `make install` installs the WW3 executables, library, the switch file used to produce the executables in a standard configuration (`bin`, `lib`, etc). 

Also installed is a CMake package config file to make WW3 more easily used within other CMake projects. To access the WW3 library/executables from another CMake project all that's needed is `find_package(WW3)`.
