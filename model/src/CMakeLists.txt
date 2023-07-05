cmake_minimum_required(VERSION 3.12)
project(wav Fortran)

message("Using CIME in ${CIMEROOT} with compiler ${COMPILER}")

include(${CASEROOT}/Macros.cmake)
if (MPILIB STREQUAL mpi-serial)
  set(CMAKE_Fortran_COMPILER ${SFC})
  set(CMAKE_C_COMPILER ${SCC})
else()
  set(CMAKE_Fortran_COMPILER ${MPIFC})
  set(CMAKE_C_COMPILER ${MPICC})
endif()
set(CMAKE_Fortran_FLAGS "${FFLAGS} ${CPPDEFS} -I${LIBROOT}/include -I${LIBROOT}/finclude -I${LIBROOT}/nuopc/esmf/${NINST_VALUE}/include")

#-------------------------
# Set ESMF_F90COMPILEPATHS
#-------------------------
# convert esmf.mk makefile variables to cmake variables until ESMF
# provides proper cmake package
if (DEFINED ENV{ESMFMKFILE})
  message("ESMFMKFILE:   $ENV{ESMFMKFILE}")
else()
  message(FATAL_ERROR "ESMFMKFILE env variable is not defined")
endif()
set(ESMFMKFILE $ENV{ESMFMKFILE})
file(STRINGS ${ESMFMKFILE} esmf_mk_text)
foreach(line ${esmf_mk_text})
  string(REGEX REPLACE "^[ ]+" "" line ${line}) # strip leading spaces
  if (line MATCHES "^ESMF_*")                   # process only line starting with ESMF_
    string(REGEX MATCH "^ESMF_[^=]+" esmf_name ${line})
    string(REPLACE "${esmf_name}=" "" emsf_value ${line})
    set(${esmf_name} "${emsf_value}")
  endif()
endforeach()
string(REPLACE "-I" "" ESMF_F90COMPILEPATHS ${ESMF_F90COMPILEPATHS})
string(REPLACE " " ";" ESMF_F90COMPILEPATHS ${ESMF_F90COMPILEPATHS})
message("ESMF_F90COMPILEPATHS:   ${ESMF_F90COMPILEPATHS}")

#-------------------------
# Determine switches
#-------------------------
list(APPEND switches "CESMCOUPLED" "NCO" "DIST" "MPI" "PR3" "UQ" "FLX0" "SEED" "ST4" "STAB0" "NL1" "BT1" "DB1" "MLIM" "FLD2" "TR0" "BS0" "RWND" "WNX1" "WNT1" "CRX1" "CRT1" "O0" "O1" "O2" "O3" "O4" "O5" "O6" "O7" "O14" "O15" "IS0" "REF0" "NOGRB") 

# TODO: need to enamble IC4 with the unstructured implemention  
if (DEFINED USE_UNSTRUCT)
  list(APPEND switches "IC0" "PDLIB" "METIS")
else()
###  list(APPEND switches "IC4" "OMPG"  "OMPH") 
  list(APPEND switches "IC0") 
endif()

#-------------------------
# Include list of src files to make file more readable
# defines variables "ftn_src", "nuopc_mesh_cap_src", "pdlib_src", "scrip_src", and "scripnc_src"
#-------------------------
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/src_list.cmake)

list(APPEND ftn_src ${nuopc_mesh_cap_src})

# Handle PDLIB, SCRIP, SCRIPNC build files directly instead of through configuration file
if("PDLIB" IN_LIST switches)
  list(APPEND ftn_src ${pdlib_src})
  if("SCOTCH" IN_LIST switches)
    #find_package(SCOTCH REQUIRED)
    #target_link_libraries(ww3_lib PUBLIC PTSCOTCHparmetis::PTSCOTCHparmetis)
  elseif("METIS" IN_LIST switches)
    #find_package(ParMETIS REQUIRED)
    #target_link_libraries(ww3_lib PUBLIC ParMETIS::ParMETIS)
  else()
     message(FATAL_ERROR "PDLIB requires METIS or SCOTCH library for domain decomposition")
  endif()
endif()

#-------------------------
# Determine switch specific files
# Include check_switches as a function for less verbosity in this CMakeLists.txt
#-------------------------
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/check_switches.cmake)
check_switches("${switches}" switch_files)

message(STATUS "----status of source files-----")
message(STATUS "list of always source files is : ${ftn_src}")
message(STATUS "list of switch files is : ${switch_files}")

#-------------------------
# Now check in SourceMods to see if the file should be used instead
#-------------------------
list(APPEND srcfiles ${ftn_src} ${switch_files})
foreach(file ${srcfiles} )
  if(EXISTS "${CASEROOT}/SourceMods/src.ww3dev/${file}")
    message("Using ${file} from ${CASEROOT}/SourceMods/src.ww3dev")
    list(REMOVE_ITEM srcfiles ${file})
    list(APPEND srcfiles "${CASEROOT}/SourceMods/src.ww3dev/${file}")
    message(STATUS "Using ${file} from ${CASEROOT}/SourceMods/src.ww3dev")
  endif()
endforeach()

#-------------------------
# Determine target library wav
#-------------------------
add_library(wav ${srcfiles})
target_include_directories (wav PRIVATE ${ESMF_F90COMPILEPATHS})

#-------------------------
# Determine compile definitions for wav
#-------------------------
foreach(switch ${switches})
  target_compile_definitions(wav PUBLIC W3_${switch})
endforeach()
add_compile_definitions(ENDIANNESS="big_endian")
set_property(SOURCE w3initmd.F90
  APPEND
  PROPERTY COMPILE_DEFINITIONS
  "__WW3_SWITCHES__=\'\'"
  )

message("CMAKE_CURRENT_BINARY_DIR is ${CMAKE_CURRENT_BINARY_DIR}")
message("PROJECT_BINARY_DIR is ${PROJECT_BINARY_DIR}")

# Executables to always build
add_executable(ww3_grid ww3_grid.F90)
target_link_libraries(ww3_grid PRIVATE wav)

install(TARGETS wav)
