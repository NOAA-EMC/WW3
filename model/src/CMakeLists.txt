

# Parse switch file and add compile definitions
set(switch_file ${CMAKE_SOURCE_DIR}/model/bin/switch_${SWITCH})
if(NOT EXISTS ${switch_file})
  message(FATAL_ERROR "Switch file '${switch_file}' does not exist, set switch with -DSWITCH=<switch>")
endif()

file(STRINGS ${switch_file} switch_strings)
separate_arguments(switches UNIX_COMMAND ${switch_strings})

# Include list of src files to make file more readable
# defines variables "ftn_src", "c_src", "pdlib_src", "scrip_src", and "scripnc_src"
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/src_list.cmake)
# check_switches as a function for less verbosity in this CMakeLists.txt
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/check_switches.cmake)

check_switches("${switches}" switch_files)

add_library(ww3_lib STATIC ${ftn_src} ${c_src} ${switch_files})

foreach(switch ${switches})
  target_compile_definitions(ww3_lib PUBLIC W3_${switch})
endforeach()

#FIXME
target_compile_definitions(ww3_lib PUBLIC "-D__WW3_SWITCHES__=${switch_strings}")

if("OMPG" IN_LIST switches)
  find_package(OpenMP REQUIRED COMPONENTS Fortran)
  target_link_libraries(ww3_lib OpenMP::OpenMP_Fortran)
endif()

if("MPI" IN_LIST switches)
  find_package(MPI REQUIRED COMPONENTS Fortran)
  target_link_libraries(ww3_lib PUBLIC MPI::MPI_Fortran)
endif()

if("PDLIB" IN_LIST switches)
  find_package(ParMETIS REQUIRED)
  target_sources(ww3_lib PRIVATE ${pdlib_src})
  target_link_libraries(ww3_lib PUBLIC ParMETIS::ParMETIS)
endif()

if("SCRIPNC" IN_LIST switches)
  target_sources(ww3_lib PRIVATE ${scrip_src} ${scripnc_src})
endif()

if("SCRIP" IN_LIST switches)
  target_sources(ww3_lib PRIVATE ${scrip_src})

  # Not sure where to put FindESMF
  find_package(ESMF)
  if(ESMF_FOUND)
    target_sources(ww3_lib PRIVATE wmesmfmd.F90)
    target_link_libraries(ww3_lib PUBLIC esmf)
  endif() 
endif()

find_package(NetCDF COMPONENTS Fortran)

if("SCRIPNC" IN_LIST switches OR "OASIS" IN_LIST switches OR "TRKNC" IN_LIST switches)
  if (NOT NetCDF_Fortran_FOUND)
    find_package(NetCDF REQUIRED COMPONENTS Fortran)
    target_link_libraries(ww3_lib PUBLIC NetCDF::NetCDF_Fortran)
  endif()
endif()

if("NCEP2" IN_LIST switches)
  find_package(g2 REQUIRED)
  find_package(bacio REQUIRED)
  find_package(w3nco REQUIRED)
  target_link_libraries(ww3_lib PUBLIC g2::g2_4 bacio::bacio_4 w3nco::w3nco_4)
endif()

if("TIDE" IN_LIST switches)
  add_executable(ww3_prtide ww3_prtide.F90)
  target_link_libraries(ww3_prtide PRIVATE ww3_lib)

  target_sources(ww3_lib PRIVATE w3nmlprncmd.F90)
  add_executable(ww3_prnc ww3_prnc.F90)
  target_link_libraries(ww3_prnc PRIVATE ww3_lib)
endif()

# Set compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(compile_flags -no-fma -ip -g -traceback -i4 -real-size 32 -fp-model precise -assume byterecl -convert big_endian -fno-alias -fno-fnalias)
  # -sox only works on Linux
  if(LINUX)
    list(APPEND compile_flags -sox)
  endif()

  if(APPLE)
    # The linker on macOS does not include `common symbols` (usually module variables without a default value) by default
    # Passing the -c flag includes them and fixes an error with undefined symbols
    # Only ifort marks these symbols as common, compared to GCC
    set(CMAKE_Fortran_ARCHIVE_FINISH "<CMAKE_RANLIB> -c <TARGET>")
    set(CMAKE_C_ARCHIVE_FINISH "<CMAKE_RANLIB> -c <TARGET>")
  endif()

  set(compile_flags_release -O3)
  # SHELL: prefix fixes CMake attempting to de-duplicate the repeated uses of 'all' in -warn, -debug, -check
  # See https://cmake.org/cmake/help/latest/command/target_compile_options.html#option-de-duplication
  set(compile_flags_debug -O0 "SHELL:-debug all" "SHELL:-warn all" "SHELL:-check all" -check noarg_temp_created -fp-stack-check -heap-arrays -traceback -fpe0)

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(compile_flags -g -fno-second-underscore -ffree-line-length-none -fconvert=big-endian)
  set(compile_flags_release -O3)
  set(compile_flags_debug -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow -frecursive -fbacktrace)
  
  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
    target_compile_options(ww3_lib PUBLIC -fallow-argument-mismatch -fallow-invalid-boz)
  endif()
endif()

target_compile_options(ww3_lib PUBLIC "$<$<COMPILE_LANGUAGE:Fortran>:${compile_flags}>")
target_compile_options(ww3_lib PUBLIC "$<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:Fortran>>:${compile_flags_debug}>")
target_compile_options(ww3_lib PUBLIC "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:Fortran>>:${compile_flags_release}>")

# Executables to build
set(programs ww3_strt ww3_grid ww3_bound ww3_outf ww3_outp ww3_trck ww3_grib
  ww3_gint gx_outf gx_outp ww3_uprstr ww3_shel ww3_prep ww3_gspl)

set(netcdf_programs ww3_ounf ww3_ounp ww3_bounc ww3_trnc)
if(NetCDF_Fortran_FOUND)
  target_sources(ww3_lib PRIVATE w3ounfmetamd.F90)
  target_link_libraries(ww3_lib PUBLIC NetCDF::NetCDF_Fortran)
  list(APPEND programs ${netcdf_programs})
endif()

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

export(EXPORT WW3Exports
  NAMESPACE WW3::
  FILE WW3-targets.cmake
  )

### Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION lib/cmake/WW3)

configure_package_config_file(${CMAKE_SOURCE_DIR}/cmake/WW3-package-config.cmake.in
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
