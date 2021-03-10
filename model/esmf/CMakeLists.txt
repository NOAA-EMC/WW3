cmake_minimum_required(VERSION 3.15)

project(ww3nemslib VERSION 7.00 LANGUAGES Fortran)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

find_package(NetCDF REQUIRED Fortran)
find_package(MPI REQUIRED)
find_package(ESMF MODULE REQUIRED)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(CMAKE_Fortran_FLAGS "-fno-second-underscore -ffree-line-length-none -fconvert=big-endian -O3 -march=native")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(CMAKE_Fortran_FLAGS "-no-fma -ip -g -i4 -real-size 32 -fp-model precise -assume byterecl -convert big_endian -fno-alias -fno-fnalias -sox -xhost -O3")
endif()

set(SRCS
    constants.ftn
    w3adatmd.ftn
    w3arrymd.ftn
    w3cspcmd.ftn
    w3dispmd.ftn
    w3fldsmd.ftn
    w3flx1md.ftn
    w3gdatmd.ftn
    w3gsrumd.ftn
    w3idatmd.ftn
    w3initmd.ftn
    w3iobcmd.ftn
    w3iogomd.ftn
    w3iogrmd.ftn
    w3iopomd.ftn
    w3iorsmd.ftn
    w3iosfmd.ftn
    w3iotrmd.ftn
    w3nmlmultimd.ftn
    w3odatmd.ftn
    w3parall.ftn
    w3partmd.ftn
    w3pro3md.ftn
    w3profsmd.ftn
    w3sbt1md.ftn
    w3sdb1md.ftn
    w3servmd.ftn
    w3snl1md.ftn
    w3src4md.ftn
    w3srcemd.ftn
    w3timemd.ftn
    w3triamd.ftn
    w3updtmd.ftn
    w3uqckmd.ftn
    w3wavemd.ftn
    w3wdasmd.ftn
    w3wdatmd.ftn
    wmesmfmd.ftn
    wmfinlmd.ftn
    wmgridmd.ftn
    wminiomd.ftn
    wminitmd.ftn
    wmiopomd.ftn
    wmmdatmd.ftn
    wmscrpmd.ftn
    wmunitmd.ftn
    wmupdtmd.ftn
    wmwavemd.ftn
    SCRIP/scrip_constants.f
    SCRIP/scrip_errormod.f90
    SCRIP/scrip_grids.f
    SCRIP/scrip_interface.ftn
    SCRIP/scrip_iounitsmod.f90
    SCRIP/scrip_kindsmod.f90
    SCRIP/scrip_netcdfmod.f90
    SCRIP/scrip_remap_conservative.f
    SCRIP/scrip_remap_read.f
    SCRIP/scrip_remap_vars.f
    SCRIP/scrip_remap_write.f
    SCRIP/scrip_timers.f
)

get_filename_component(aux_dir "${CMAKE_CURRENT_SOURCE_DIR}/../aux" ABSOLUTE)
get_filename_component(ftn_dir "${CMAKE_CURRENT_SOURCE_DIR}/../ftn" ABSOLUTE)

message(STATUS "aux_dir ${aux_dir}")
message(STATUS "ftn_dir ${ftn_dir}")

add_executable(w3adc ../aux/w3adc.f)

foreach(src_file ${SRCS})
    STRING(REGEX REPLACE ".ftn" ".F90" gen_src_file ${src_file})
    STRING(REGEX REPLACE "/" "_" gen_log_file ${gen_src_file})
    add_custom_command(
        OUTPUT  ${gen_src_file}
        DEPENDS w3adc ${ftn_dir}/${src_file}
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/run_w3adc.sh ${ftn_dir} ${src_file} > ${gen_log_file}.w3adc.log 2>&1
        COMMENT "Running w3adc ${src_file}")
    list(APPEND SRCS_F90 ${gen_src_file})
endforeach()

add_library(ww3_multi_esmf STATIC ${SRCS_F90})

set(mod_dir ${CMAKE_CURRENT_BINARY_DIR}/mod)
set_target_properties(ww3_multi_esmf PROPERTIES Fortran_MODULE_DIRECTORY ${mod_dir})

target_include_directories(ww3_multi_esmf PUBLIC ${mod_dir})
target_include_directories(ww3_multi_esmf PRIVATE ${ftn_dir} ${ESMF_MOD})

target_link_libraries(ww3_multi_esmf PUBLIC esmf NetCDF::NetCDF_Fortran MPI::MPI_Fortran)
