cmake_minimum_required(VERSION 3.10)
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

# Set ESMF_F90COMPILEPATHS
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

# These were the initial switch definitions in CESM
# F90 NOGRB NC4 PR3 UQ FLX0 LN1 ST4 BT1 DB1 MLIM NL1 TR0 BS0 REF0 IS0 IC4 XX0 WNT1 WNX1 CRT1 CRX1 O0 O1 O2 O3 O4 O5 O6 O7 O11 MPI DIST

add_compile_definitions(CESMCOUPLED)
add_compile_definitions(W3_NCO)
add_compile_definitions(W3_DIST)
add_compile_definitions(W3_MPI)
add_compile_definitions(W3_OMPG)
add_compile_definitions(W3_OMPH)
add_compile_definitions(W3_PR3)
add_compile_definitions(W3_UQ)
add_compile_definitions(W3_FLX0)
add_compile_definitions(W3_SEED)
add_compile_definitions(W3_ST4)
add_compile_definitions(W3_STAB0)
add_compile_definitions(W3_NL1)
add_compile_definitions(W3_BT1)
add_compile_definitions(W3_DB1)
add_compile_definitions(W3_MLIM)
add_compile_definitions(W3_FLD2)
add_compile_definitions(W3_TR0)
add_compile_definitions(W3_BS0)
add_compile_definitions(W3_RWND)
add_compile_definitions(W3_WNX1)
add_compile_definitions(W3_WNT1)
add_compile_definitions(W3_CRX1)
add_compile_definitions(W3_CRT1)
add_compile_definitions(W3_O0)
add_compile_definitions(W3_O1)
add_compile_definitions(W3_O2)
add_compile_definitions(W3_O3)
add_compile_definitions(W3_O4)
add_compile_definitions(W3_O5)
add_compile_definitions(W3_O6)
add_compile_definitions(W3_O7)
add_compile_definitions(W3_O14)
add_compile_definitions(W3_O15)
add_compile_definitions(W3_IC4)
add_compile_definitions(W3_IS0)
add_compile_definitions(W3_REF0)

# Note that w3iogocdmd is a new file for netcdf io
# Set source files
set(SRCFILES constants.F90
             w3adatmd.F90
             w3arrymd.F90
             w3cspcmd.F90
             w3dispmd.F90
             w3fld1md.F90
             w3fld2md.F90
             w3fldsmd.F90
             w3flx1md.F90
             w3gdatmd.F90
             w3gkemd.F90
             w3gsrumd.F90
             w3idatmd.F90
             w3initmd.F90
             w3iobcmd.F90
             w3iogomd.F90
             w3iogoncdmd.F90
             w3iogrmd.F90
             w3iopomd.F90
             w3iorsmd.F90
             w3iosfmd.F90
             w3iotrmd.F90
             w3meminfo.F90
             w3odatmd.F90
             w3parall.F90
             w3partmd.F90
             w3pro3md.F90
             w3profsmd.F90
             w3sdb1md.F90
             w3servmd.F90
             w3sic4md.F90
             w3snl1md.F90
             w3src0md.F90
             w3src4md.F90
             w3srcemd.F90
             w3sbt1md.F90
             w3str1md.F90
             w3strkmd.F90
             w3tidemd.F90
             w3timemd.F90
             w3triamd.F90
             w3uno2md.F90
             w3updtmd.F90
             w3uqckmd.F90
             w3wavemd.F90
             w3wdasmd.F90
             w3wdatmd.F90
             wav_comp_nuopc.F90
             wav_import_export.F90
             wav_kind_mod.F90
             wav_shel_inp.F90
             wav_shr_mod.F90)

foreach(FILE ${SRCFILES})
  if(EXISTS "${CASEROOT}/SourceMods/src.ww3/${FILE}")
    list(REMOVE_ITEM SRCFILES ${FILE})
    list(APPEND SRCFILES "${CASEROOT}/SourceMods/src.dww3/${FILE}")
    message("Using ${FILE} from ${CASEROOT}/SourceMods/src.ww3")
  endif()
endforeach()

message("CMAKE_CURRENT_BINARY_DIR is ${CMAKE_CURRENT_BINARY_DIR}")
message("PROJECT_BINARY_DIR is ${PROJECT_BINARY_DIR}")

add_library(wav ${SRCFILES})
target_include_directories (wav PRIVATE ${ESMF_F90COMPILEPATHS})

install(TARGETS wav)
