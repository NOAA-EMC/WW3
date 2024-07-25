
# Open switch file
file(STRINGS ${CMAKE_BINARY_DIR}/switch switch_strings)
separate_arguments(switches UNIX_COMMAND ${switch_strings})

# Put executables/libraries into a top-level bin/lib directory within the build directory
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Include list of src files to make file more readable
# defines variables "ftn_src", "pdlib_src", "scrip_src", and "scripnc_src"
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/src_list.cmake)
# check_switches as a function for less verbosity in this CMakeLists.txt
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/check_switches.cmake)

check_switches("${switches}" switch_files)

add_library(ww3_lib STATIC ${ftn_src} ${switch_files})
# Create alias with same name as exported target
# so that WW3 can be usead with add_subdirectory
add_library(WW3::WW3 ALIAS ww3_lib)

# Re-name library on disk to just libww3 instead of libww3_lib
set_target_properties(ww3_lib PROPERTIES OUTPUT_NAME "ww3")
set_target_properties(ww3_lib PROPERTIES Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/mod")

foreach(switch ${switches})
  target_compile_definitions(ww3_lib PUBLIC W3_${switch})
endforeach()

if(ENDIAN STREQUAL "BIG")
  set(endianness "big_endian")
elseif(ENDIAN STREQUAL "LITTLE")
  set(endianness "little_endian")
elseif(ENDIAN STREQUAL "NATIVE")
  set(endianness "native")
else()
  message(FATAL_ERROR "Unknown option specified by ENDIAN: ${ENDIAN}")
endif()

add_compile_definitions(ENDIANNESS='${endianness}')

target_include_directories(ww3_lib
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
  $<INSTALL_INTERFACE:mod>)

# Set compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(compile_flags -no-fma -ip -g -traceback -i4 -real-size 32 -fp-model precise
    -assume byterecl -fno-alias -fno-fnalias)
  # -sox only works on Linux
  if(LINUX)
    list(APPEND compile_flags -sox)
  endif()
  set(compile_flags_release -O3)
  # SHELL: prefix fixes CMake attempting to de-duplicate the repeated uses of 'all' in -warn, -debug, -check
  # See https://cmake.org/cmake/help/latest/command/target_compile_options.html#option-de-duplication
  set(compile_flags_debug -O0 "SHELL:-debug all" "SHELL:-warn all" "SHELL:-check all" -check noarg_temp_created -fp-stack-check -heap-arrays -traceback -fpe0)

  if(APPLE)
    # The linker on macOS does not include `common symbols` (usually module variables without a default value) by default
    # Passing the -c flag includes them and fixes an error with undefined symbols
    # Only ifort marks these symbols as common, compared to GCC
    set(CMAKE_Fortran_ARCHIVE_FINISH "<CMAKE_RANLIB> -c <TARGET>")
    set(CMAKE_C_ARCHIVE_FINISH "<CMAKE_RANLIB> -c <TARGET>")
  endif()

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(compile_flags -g -fno-second-underscore -ffree-line-length-none)
  set(compile_flags_release -O3)
  set(compile_flags_debug -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow -frecursive -fbacktrace)
  
  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
    target_compile_options(ww3_lib PUBLIC -fallow-argument-mismatch)
  endif()

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
  set(compile_flags -g -i4 -r4 -Kieee)
  set(compile_flags_release -O3)
  set(compile_flags_debug -O0 -Mbounds -Mchkfpstk -Mchkstk -Mdalign -Mdclchk -Mdepchk -Miomutex -Ktrap=fp -Mrecursive -traceback)
  
  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
    target_compile_options(ww3_lib PUBLIC -fallow-argument-mismatch -fallow-invalid-boz)
  endif()

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
  # TODO: Where do we set -homp for OpenMP compiles?
  set(compile_flags -g -sdefault32 -eg)
  set(compile_flags_release -O3)
  set(compile_flags_debug -O0 -Rbcps -Ktrap=fp)
endif()

target_compile_options(ww3_lib PUBLIC "$<$<COMPILE_LANGUAGE:Fortran>:${compile_flags}>")
target_compile_options(ww3_lib PUBLIC "$<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:Fortran>>:${compile_flags_debug}>")
target_compile_options(ww3_lib PUBLIC "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:Fortran>>:${compile_flags_release}>")

# Executables to always build
set(programs ww3_strt ww3_grid ww3_bound ww3_outf ww3_outp ww3_trck ww3_grib
  ww3_gint gx_outf gx_outp ww3_uprstr ww3_shel ww3_prep ww3_gspl ww3_multi ww3_systrk)

# Executables to build if NetCDF is found
set(netcdf_programs ww3_ounf ww3_ounp ww3_bounc ww3_trnc ww3_prnc)

if("OASIS" IN_LIST switches)
  find_package(OASIS REQUIRED)
  target_link_libraries(ww3_lib PUBLIC OASIS::OASIS)
endif()

if(NETCDF)
  if("netcdf" IN_LIST EXCLUDE_FIND)
    # NetCDF library flags provided by compiler or wrapper script: don't search
    set(NetCDF_Fortran_FOUND TRUE)
    message(WARNING "Not searching for NetCDF library - please ensure correct flags are provided by your compiler/wrapper")
  else()
    find_package(NetCDF REQUIRED COMPONENTS Fortran)
  endif()
endif()

if(NetCDF_Fortran_FOUND)
  target_sources(ww3_lib PRIVATE w3ounfmetamd.F90)
  if(TARGET NetCDF::NetCDF_Fortran)
    # NetCDF library flags provided by compiler or wrapper script: don't add as dependency
    target_link_libraries(ww3_lib PUBLIC NetCDF::NetCDF_Fortran)
  endif()
  list(APPEND programs ${netcdf_programs})
endif()

if(MULTI_ESMF)
  # Don't search for ESMF if target already exists (when build WW3 as UFS submodule)
  if (NOT TARGET esmf)
    find_package(ESMF MODULE REQUIRED)
  endif()
  
  target_sources(ww3_lib PRIVATE wmesmfmd.F90)
  target_link_libraries(ww3_lib PUBLIC esmf)
  set_target_properties(ww3_lib PROPERTIES OUTPUT_NAME "ww3_multi_esmf")
  # Don't build executables when building WW3 ESMF library
  set(programs "")
endif() 

set_property(SOURCE w3initmd.F90
  APPEND
  PROPERTY COMPILE_DEFINITIONS
  "__WW3_SWITCHES__=\'${switch_strings}\'"
  )

if("OMPG" IN_LIST switches)
  find_package(OpenMP REQUIRED COMPONENTS Fortran)
  target_link_libraries(ww3_lib PUBLIC OpenMP::OpenMP_Fortran)
endif()

if("MPI" IN_LIST switches)
  find_package(MPI REQUIRED COMPONENTS Fortran)
  target_link_libraries(ww3_lib PUBLIC MPI::MPI_Fortran)
endif()

# Handle PDLIB, SCRIP, SCRIPNC build files directly instead of through configuration file
if("PDLIB" IN_LIST switches)
  if("SCOTCH" IN_LIST switches)
  find_package(SCOTCH REQUIRED)
  target_sources(ww3_lib PRIVATE ${pdlib_src})
  target_link_libraries(ww3_lib PUBLIC PTSCOTCHparmetis::PTSCOTCHparmetis)
elseif("METIS"  IN_LIST switches)
  find_package(ParMETIS REQUIRED)
  target_sources(ww3_lib PRIVATE ${pdlib_src})
  target_link_libraries(ww3_lib PUBLIC ParMETIS::ParMETIS)
  else()
    message(FATAL_ERROR "PDLIB requires METIS or SCOTCH library for domain decomposition")
 endif()
endif()

if("SCRIP" IN_LIST switches)
  target_sources(ww3_lib PRIVATE ${scrip_src})
endif()

if("SCRIPNC" IN_LIST switches OR "OASIS" IN_LIST switches OR "TRKNC" IN_LIST switches OR "BIN2NC" IN_LIST switches)
  if(NOT NetCDF_Fortran_FOUND)
	  message(FATAL_ERROR "Cannot build SCRIPNC, OASIS, TRKNC, or BIN2NC without NetCDF")
  endif()
endif()

if("SCRIPNC" IN_LIST switches)
  target_sources(ww3_lib PRIVATE ${scrip_src} ${scripnc_src})
endif()

if("NCEP2" IN_LIST switches)
  find_package(g2 REQUIRED)
  find_package(bacio REQUIRED)
  find_package(w3emc REQUIRED)
  target_link_libraries(ww3_lib PUBLIC g2::g2_4 bacio::bacio_4 w3emc::w3emc_4)
endif()

if("TIDE" IN_LIST switches)
  list(APPEND programs ww3_prtide)
endif()

# Executables are not needed when building ww3_multi_esmf library
foreach(program ${programs})
  add_executable(${program} ${program}.F90)
  target_link_libraries(${program} PRIVATE ww3_lib)
endforeach()

install(
  TARGETS ${programs} ww3_lib
  EXPORT WW3Exports
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)

install(FILES ${CMAKE_BINARY_DIR}/switch DESTINATION ${CMAKE_INSTALL_PREFIX})
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod DESTINATION ${CMAKE_INSTALL_PREFIX})
  

export(EXPORT WW3Exports
  NAMESPACE WW3::
  FILE WW3-targets.cmake
  )

### Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION lib/cmake/WW3)

configure_package_config_file(${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/WW3-package-config.cmake.in
  ${CMAKE_BINARY_DIR}/WW3-config.cmake
  INSTALL_DESTINATION ${CONFIG_INSTALL_DESTINATION})

install(FILES ${CMAKE_BINARY_DIR}/WW3-config.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

write_basic_package_version_file(${CMAKE_BINARY_DIR}/WW3-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion)

install(FILES ${CMAKE_BINARY_DIR}/WW3-config-version.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

install(EXPORT WW3Exports
  NAMESPACE WW3::
  FILE WW3-targets.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})
