# CMake Build

A CMake build is provided with WW3 to standardize and simplify the WW3 build process. CMake uses an out-of-source build which separates the build directory from the source directory.

WW3 uses `#ifdef` directives to configure model options. These options are specified in a 'switch file' passed to CMake with `-DSWITCH=/switch/file` during the build. CMake parses the switch options and copies it into the build to determine which files to build, and what values to pass to the pre-processor.

Switch setting configurations are stored in [switches.json](./bin/switches.json). This file is read by CMake to check the validity of input switches.

The CMake build does not offer incremental builds. If the switch file changes, on the next `make`, CMake will re-trigger and build from scratch.

Requires CMake 3.19+

## Quick Start

```
# Optionally set compiler and env vars to locate libraries
export CC=icc
export FC=ifort
export NetCDF_ROOT=/path/to/netcdf

# Clone and build WW3
git clone https://github.com/NOAA-EMC/WW3.git
cd WW3
mkdir build && cd build
cmake .. -DSWITCH=/path/to/switch_NCEP_st2 -DCMAKE_INSTALL_PREFIX=install
make
make install
```

Note `cmake ..` is pointing to the directory containing the top-level CMakeLists.txt (just above the build dir in this case). The build directory can be located anywhere, and then the CMake command would be `cmake /path/to/WW3 -DSWITCH=/path/to/switch_NCEP_st2`.


## CMake Options

Options can be passed to CMake with `-D<option>`.

* `SWITCH` (required) - Absolute path to a switch file, or a switch file located in model/bin. CMake will detect changes to the source switch file and re-trigger the whole build if changed.

* `MULTI_ESMF=ON/OFF` (optional) - Build the ww3_multi_esmf library (off by default). Requires ESMF.

* `CMAKE_INSTALL_PREFIX` (optional) - Standard CMake variable for install location when running `make install`. Executables/libraries are also located in build/bin if not running `make install`.

* `NETCDF=ON/OFF` (optional) - Build NetCDF programs (ww3_ounf, ww3_ounp, ww3_bounc, ww3_trnc, and ww3_prnc). Requires NetCDF. Enabled by default.

* `ENDIAN=BIG/LITTLE/NATIVE` - Endianness of unformatted output files. Defaults to `BIG` 

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

* NetCDF - Optionally searched for to build NetCDF executables. Required when `SCRIPNC`, `TRKNC`, or `OASIS` are in switch list.

* OASIS3-MCT - Required when `OASIS` in switch list. CMake searches `OASISDIR` for library path.

* METIS/ParMETIS - Required when `PDLIB` in switch list.

* NCEPLIBS (g2, bacio, w3emc) - Required when `NCEP2` in switch list.

* ESMF - Required when `MULTI_ESMF=ON`

## Finding Libraries

CMake has a standardized way of searching for external libraries including default system paths such as `/usr/local`. 

If a library is located in a non-standard location CMake will search
multiple locations including `CMAKE_PREFIX_PATH` (a semi-colon
separated list) or `<name>_ROOT`. 

Note: If your system provides the paths to libraries and include directories
automatically (e.g. via a compiler wrapper script, as is the case with the
Cray Compiler Environment), then you might wish to stop CMake from trying
to automatically finding particular libraries. This can be achieved by
passing a list of libraries to ignore via the `EXCLUDE_FIND` option to
CMake (currently only implemented for the netCDF library). E.g:
```
cmake .. -DEXCLUDE_FIND="netcdf"
```

### Can set the compiler to MPI wrappers to find MPI
```
export CC=mpicc
export FC=mpif90
```

### \<name\>_ROOT

CMake will search _ROOT env variables.

```
export NetCDF_ROOT=<netcdf dir>
```

### CMAKE_PREFIX_PATH

The CMAKE_PREFIX_PATH CMake variable, or env variable, can be used to pass a list of semi-colon separated paths.
```
cmake .. -DCMAKE_PREFIX_PATH=/path/to/g2;/path/to/w3emc;/path/to/bacio
```

### OASIS is a special case and OASISDIR is used
```
export OASISDIR=/path/to/oasis
```

## Installation

Running `make install` installs the WW3 executables, library, the switch file used to produce the executables in a standard configuration (`bin`, `lib`, etc). 

Also installed is a CMake package config file to make WW3 more easily used within other CMake projects. To access the WW3 library/executables from another CMake project all that's needed is `find_package(WW3)`.
  
## Github Actions CI

  The CMake build uses Github Actions to test the build on each push/PR to Github.
  
  The Action uses Spack to install dependencies in [spack.yaml](./ci/spack.yaml) and then caches them to avoid building the dependencies each time.
  
  It builds WW3 using Intel and GNU compilers with a set of switches meant to test various parts of the build (NetCDF, ESMF, OASIS, etc).
  
  The switches built are listed in [intel.yml](https://github.com/kgerheiser/WW3/blob/9aad5635a3dac194c59833df2c3ea7ff3f4173df/.github/workflows/intel.yml#L88) and [gnu.yml](https://github.com/kgerheiser/WW3/blob/9aad5635a3dac194c59833df2c3ea7ff3f4173df/.github/workflows/gnu.yml#L68)

# FAQ

### How to add a new file to the CMake build?

Append to [src_list.cmake](./src/cmake/src_list.cmake)

### How to change compiler flags?

Compiler flags are set per compiler in [CMakeLists.txt](./src/CMakeLists.txt)

Supported compilers are Intel, GNU, PGI and Cray.

### How to build a single target?

CMake will build all available executables by default. However, CMake
offers an option to build single targets. Multiple targets may be given, separated by spaces.

Run this (from the build directory, hence the `.`, or point it to the
build dir).

`cmake --build . --target ww3_shel`

### How to create a debug build?

The CMake build type can set with the CMake variable `CMAKE_BUILD_TYPE`. Valid options are Release and Debug. The default is Release.

```
cmake .. -DCMAKE_BUILD_TYPE=Debug
cmake .. -DCMAKE_BUILD_TYPE=Release
```
