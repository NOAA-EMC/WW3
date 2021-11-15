

# Parse switch file and add compile definitions
set(switch_file ${CMAKE_SOURCE_DIR}/model/bin/switch_${SWITCH})
if(NOT EXISTS ${switch_file})
  message(FATAL_ERROR "Switch file '${switch_file}' does not exist, set switch with -DSWITCH=<switch>")
endif()

file(STRINGS ${switch_file} switch_strings)
separate_arguments(switches UNIX_COMMAND ${switch_strings})

function(len_minus_one json_array len)
  
endfunction()

#TODO check for conflicts and required associated switches
# Read JSON file
file(READ ${CMAKE_SOURCE_DIR}/model/bin/switches.json json_str)
# Get length of top-level array of all switch categories
string(JSON len LENGTH ${json_str})
# CMake's foreach RANGE is inclusive, so subtract 1 when looping
math(EXPR len "${len} - 1")

# Loop over switch categories
set(switch_files "")
foreach(i_category RANGE ${len})
  string(JSON category GET ${json_str} ${i_category})
  string(JSON num_options LENGTH ${category} valid-options)

  # Loop over valid options
  math(EXPR num_options "${num_options} - 1")
  set(n_switches_in_category 0)
  foreach(j_options RANGE ${num_options})
    string(JSON valid_opt GET ${category} valid-options ${j_options} name)

    # This option is in current switch file
    if(valid_opt IN_LIST switches)
      math(EXPR n_switches_in_category "${n_switches_in_category} + 1")
      string(JSON n_files ERROR_VARIABLE err LENGTH ${category} valid-options ${j_options} build_files)

      # Check for conflicting switches
      string(JSON n_conflicts ERROR_VARIABLE err LENGTH ${category} valid-options ${j_options} conflicts)
      if(n_conflicts)
        math(EXPR n_conflicts "${n_conflicts} -1")
        # Loop over conflcits
        foreach(i_conflict RANGE ${n_conflicts})
          string(JSON conflict GET ${category} valid-options ${j_options} conflicts ${i_conflict})
          if(conflict IN_LIST switches)
            message(FATAL_ERROR "Switch '${valid_opt}' and '${conflict}' conflict")
          endif()
        endforeach()
      endif()

      # Check for required dependent switches
      string(JSON n_requires ERROR_VARIABLE err LENGTH ${category} valid-options ${j_options} requires)
      if(n_requires)
        math(EXPR n_requires "${n_requires} - 1")
        # Loop over required switches
        foreach(i_requires RANGE ${n_requires})

          string(JSON json_type TYPE ${category} valid-options ${j_options} requires ${i_requires})

          # Can be a string or an array. String values or directly required, while if an array one of the values is required.
          if(json_type STREQUAL "STRING")
            string(JSON required_switch GET ${category} valid-options ${j_options} requires ${i_requires})
            if(NOT required_switch IN_LIST switches)
              message(FATAL_ERROR "Switch '${valid_opt}' requires '${required_switch}' to be set")
            endif()
          elseif(json_type STREQUAL "ARRAY")
            string(JSON n_requires_any LENGTH ${vategory} valid-options ${j_options} requries ${i_requires})
            math(EXPR n_requires_any "${n_requires_any} - 1")

            # Loop over array and check that one of the switches is present
            set(found false)
            set(possible_values "")
            foreach(i_requires_any RANGE ${n_requires_any})
              string(JSON required_switch GET ${category} valid-options ${j_options} requires ${i_requires} ${i_requires_any})
              list(APPEND possible_values "${required_switch}")

              if(required_switch IN_LIST switches)
                set(found true)
              endif()
            endforeach()

            if(NOT found)
              message(FATAL_ERROR "Switch ${valid_opt} requires one of ${possible_values} to be set")
            endif()
            
          endif()
        endforeach()
      endif()

      if(n_files)
        # Loop over files associated with switch and add them to build
        math(EXPR n_files "${n_files} - 1")
        foreach(i_files RANGE ${n_files})
          string(JSON file GET ${category} valid-options ${j_options} build_files ${i_files})
          list(APPEND switch_files "${file}.F90")
        endforeach()
      endif()
    endif()

  endforeach()

  # Check for the correct number of switches per category
  string(JSON num_switches GET ${category} num_switches)
  string(JSON category_name GET ${category} name)

  if(num_switches STREQUAL "one" AND NOT n_switches_in_category EQUAL 1)
    message(FATAL_ERROR "No valid ${category_name} switches found, but one is required")
  elseif(num_switches STREQUAL "upto1" AND n_switches_in_category GREATER 1)
    message(FATAL_ERROR "Too many ${category_name} switches found (max 1)")
  elseif(num_switches STREQUAL "upto2" AND n_switches_in_category GREATER 2)
    message(FATAL_ERROR "Too many ${category_name} switches found (max 2)")
  endif()
  
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

if("SCRIPNC" IN_LIST switches OR "OASIS" IN_LIST switches OR "TRKNC" IN_LIST switches)
  find_package(NetCDF REQUIRED COMPONENTS Fortran)
  target_link_libraries(ww3_lib PUBLIC NetCDF::NetCDF_Fortran)

  target_sources(ww3_lib PRIVATE w3ounfmetamd.F90)
  
  set(netcdf_programs ww3_ounf ww3_ounp ww3_bounc ww3_trnc)
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

list(APPEND programs ${netcdf_programs})

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
