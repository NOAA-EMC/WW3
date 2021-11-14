

# Parse switch file and add compile definitions
set(switch_file ${CMAKE_SOURCE_DIR}/model/bin/switch_${SWITCH})
if(NOT EXISTS ${switch_file})
  message(FATAL_ERROR "Switch file '${switch_file}' does not exist, set switch with -DSWITCH=<switch>")
endif()

file(STRINGS ${switch_file} switch_strings)
separate_arguments(switches UNIX_COMMAND ${switch_strings})


#TODO check for conflicts and required associated switches
# Read JSON file
file(READ ${CMAKE_SOURCE_DIR}/model/bin/switches.json json_str)
# Get length of top-level array of all switch categories
string(JSON len LENGTH ${json_str})
# CMake's foreach RANGE is inclusive, so subtract 1 when looping
math(EXPR len "${len} - 1")

# Loop over switch categories
set(switch_files "")
foreach(i RANGE ${len})
  string(JSON category GET ${json_str} ${i})
  string(JSON num_options LENGTH ${category} valid-options)

  # Loop over valid options
  math(EXPR num_options "${num_options} - 1")
  foreach(j RANGE ${num_options})
    string(JSON valid_opt GET ${category} valid-options ${j} name)

    if(valid_opt IN_LIST switches)
      string(JSON n_files ERROR_VARIABLE err LENGTH ${category} valid-options ${j} build_files)
      
      if(n_files)
        # Loop over files associated with switch and add them to build
        math(EXPR n_files "${n_files} - 1")
        foreach(k RANGE ${n_files})
          string(JSON file GET ${category} valid-options ${j} build_files ${k})
          list(APPEND switch_files "${file}.F90")
        endforeach()
      endif()
    endif()
  endforeach()
endforeach()


# Include list of src files to make file more readable
# defines variables "ftn_src", "c_src", "pdlib_src", "scrip_src", and "scripnc_src"
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/src_list.cmake)

add_library(ww3_lib STATIC ${ftn_src} ${c_src} ${switch_files})

foreach(switch ${switches})
  target_compile_definitions(ww3_lib PUBLIC W3_${switch})
endforeach()

#FIXME
#target_compile_definitions(ww3_lib PUBLIC "-D__WW3_SWITCHES__=${switch_strings}")

if("MPI" IN_LIST switches)
  find_package(MPI REQUIRED COMPONENTS Fortran)
  target_link_libraries(ww3_lib PUBLIC MPI::MPI_Fortran)
endif()

if("PDLIB" IN_LIST switches)
  find_package(ParMETIS REQUIRED)
  target_sources(ww3_lib PRIVATE ${pdlib_src})
endif()

if("SCRIPNC" IN_LIST switches)
  target_sources(ww3_lib PRIVATE ${scrip_src} ${scripnc_src})
endif()

if("SCRIP" IN_LIST switches)
  target_sources(ww3_lib PRIVATE ${scrip_src})
endif()

if("SCRIPNC" IN_LIST switches OR "OASIS" IN_LIST switches OR "TRKNC" IN_LIST switches)
  find_package(NetCDF REQUIRED COMPONENTS Fortran)
  target_link_libraries(ww3_lib PUBLIC NetCDF::NetCDF_Fortran)

  target_sources(ww3_lib PRIVATE w3ounfmetamd.F90)
  set(netcdf_programs ww3_ounf ww3_ounp ww3_bounc ww3_trnc)

  list(APPEND programs ${netcdf_programs})
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
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -fp-model precise")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -check -check noarg_temp_created -check nopointer -fp-stack-check -fstack-protector-all -fpe0 -debug -ftrapuv")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -fbacktrace -ffree-line-length-none")
  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch -fallow-invalid-boz")
  endif()
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O1 -ggdb -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -ffpe-trap=invalid,zero,overflow -fbounds-check -fsanitize=address -fno-omit-frame-pointer -fno-optimize-sibling-calls")
endif()

# Executables to build
set(programs ww3_strt ww3_grid ww3_bound ww3_outf ww3_outp ww3_trck ww3_grib
  ww3_gint gx_outf gx_outp ww3_uprstr ww3_shel ww3_prep ww3_gspl)

foreach(program ${programs})
  add_executable(${program} ${program}.F90)
  target_link_libraries(${program} PRIVATE ww3_lib)
endforeach()
