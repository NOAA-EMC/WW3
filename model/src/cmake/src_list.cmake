set(c_src w3getmem.c)

# Core files always built
set(ftn_src
  constants.F90
  w3adatmd.F90
  w3arrymd.F90
  w3bullmd.F90
  w3cspcmd.F90
  w3dispmd.F90
  w3fldsmd.F90
  w3gdatmd.F90
  w3gridmd.F90
  w3gsrumd.F90
  w3idatmd.F90
  w3initmd.F90
  w3iobcmd.F90
  w3iogomd.F90
  w3iogrmd.F90
  w3iopomd.F90
  w3iorsmd.F90
  w3iosfmd.F90
  w3iotrmd.F90
  w3macros.h
  w3metamd.F90
  w3nmlbouncmd.F90
  w3nmlboundmd.F90
  w3nmlgridmd.F90
  w3nmlmultimd.F90
  w3nmlounfmd.F90
  w3nmlounpmd.F90
  w3nmlprncmd.F90
  w3nmlshelmd.F90
  w3nmltrncmd.F90
  w3nmluprstrmd.F90
  w3odatmd.F90
  w3parall.F90
  w3partmd.F90
  w3servmd.F90
  w3srcemd.F90
  w3strkmd.F90
  w3timemd.F90
  w3triamd.F90
  w3updtmd.F90
  w3wavemd.F90
  w3wdasmd.F90
  w3wdatmd.F90
  wmfinlmd.F90
  wmgridmd.F90
  wminiomd.F90
  wminitmd.F90
  wmiopomd.F90
  wmmdatmd.F90
  wmunitmd.F90
  wmupdtmd.F90
  wmwavemd.F90
  w3tidemd.F90
  )

# Built when PDLIB is enabled
set(pdlib_src
  ${CMAKE_CURRENT_SOURCE_DIR}/pdlib_field_vec.F90
  ${CMAKE_CURRENT_SOURCE_DIR}/w3profsmd_pdlib.F90
  ${CMAKE_CURRENT_SOURCE_DIR}/PDLIB/yowdatapool.F90
  ${CMAKE_CURRENT_SOURCE_DIR}/PDLIB/yowelementpool.F90
  ${CMAKE_CURRENT_SOURCE_DIR}/PDLIB/yowerr.F90
  ${CMAKE_CURRENT_SOURCE_DIR}/PDLIB/yowexchangeModule.F90
  ${CMAKE_CURRENT_SOURCE_DIR}/PDLIB/yowfunction.F90
  ${CMAKE_CURRENT_SOURCE_DIR}/PDLIB/yownodepool.F90
  ${CMAKE_CURRENT_SOURCE_DIR}/PDLIB/yowpd.F90
  ${CMAKE_CURRENT_SOURCE_DIR}/PDLIB/yowpdlibmain.F90
  ${CMAKE_CURRENT_SOURCE_DIR}/PDLIB/yowrankModule.F90
  ${CMAKE_CURRENT_SOURCE_DIR}/PDLIB/yowsidepool.F90
  )

# Build when SCRIP or SCRIPNC is enabled
set(scrip_src
  ${CMAKE_CURRENT_SOURCE_DIR}/SCRIP/scrip_constants.f
  ${CMAKE_CURRENT_SOURCE_DIR}/SCRIP/scrip_errormod.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/SCRIP/scrip_grids.f
  ${CMAKE_CURRENT_SOURCE_DIR}/SCRIP/scrip_interface.F90
  ${CMAKE_CURRENT_SOURCE_DIR}/SCRIP/scrip_iounitsmod.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/SCRIP/scrip_kindsmod.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/SCRIP/scrip_remap_conservative.f
  ${CMAKE_CURRENT_SOURCE_DIR}/SCRIP/scrip_remap_vars.f
  ${CMAKE_CURRENT_SOURCE_DIR}/SCRIP/scrip_timers.f
  )

# Built when SCRIPNC is enabled
set(scripnc_src
  ${CMAKE_CURRENT_SOURCE_DIR}/SCRIP/scrip_netcdfmod.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/SCRIP/scrip_remap_write.f
  ${CMAKE_CURRENT_SOURCE_DIR}/SCRIP/scrip_remap_read.f
  )
