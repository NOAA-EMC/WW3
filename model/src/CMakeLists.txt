

# Parse switch file and add compile definitions
set(switch_file ${CMAKE_SOURCE_DIR}/model/bin/switch_${SWITCH})
if(NOT EXISTS ${switch_file})
  message(FATAL_ERROR "Switch file '${switch_file}' does not exist, set switch with -DSWITCH=<switch>")
endif()

file(STRINGS ${switch_file} switch_strings)
separate_arguments(switches UNIX_COMMAND ${switch_strings})


# Include list of src files to make file more readable
# defines variables "ftn_src" and "c_src"
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/src_list.cmake)

add_library(ww3_lib STATIC ${ftn_src} ${c_src})

foreach(switch ${switches})
  target_compile_definitions(ww3_lib PUBLIC WW3_${switch})
endforeach()


# Read JSON file
file(READ ${CMAKE_SOURCE_DIR}/model/bin/switches.json json_str)
string(JSON len LENGTH ${json_str})
math(EXPR len "${len} - 1")

foreach(idx RANGE ${len})
  string(JSON category GET ${json_str} ${idx})
  string(JSON num_options LENGTH ${category} valid-options)

  math(EXPR num_options "${num_options} - 1")
  foreach(idy RANGE ${num_options})
    string(JSON valid_opt GET ${category} valid-options ${idy} name)
    message(${valid_opt})
  endforeach()
  
endforeach()


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
set(programs bounc bound gint grib grid gspl multi ounf ounp prep prnc prtide
  sbs1 shel strt systrk trck trnc uprstr outf outp)

# foreach(program ${programs})
#   add_executable(${program} ww3_${program}.F90)
#   target_link_libraries(${program} PRIVATE ww3_lib)
# endforeach()
