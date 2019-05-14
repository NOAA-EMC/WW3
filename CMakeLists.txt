cmake_minimum_required(VERSION 3.1)
project (WW3)

execute_process (
    COMMAND sh ${CMAKE_SOURCE_DIR}/model/bin/ww3_from_ftp.sh 
)

execute_process (
    COMMAND sh ${CMAKE_SOURCE_DIR}/model/bin/w3_setup ${CMAKE_SOURCE_DIR}/model -c intel -s NCEP_st4
)

execute_process(
    COMMAND ${CMAKE_SOURCE_DIR}/model/bin/w3_automake
)

