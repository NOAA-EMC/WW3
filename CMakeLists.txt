cmake_minimum_required(VERSION 3.1)
project (WW3)

execute_process (
      COMMAND sh ${CMAKE_SOURCE_DIR}/WW3/model/bin/ww3_from_ftp.sh 
)

execute_process (
      COMMAND sh ${CMAKE_SOURCE_DIR}/WW3/model/bin/w3_setup ${CMAKE_SOURCE_DIR}/WW3/model -c theia_so -s NCEP_st4
)

execute_process(
      COMMAND ${CMAKE_SOURCE_DIR}/WW3/model/bin/w3_automake
)

