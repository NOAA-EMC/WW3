message(STATUS "Searching for oasis3-mct")


message(STATUS "  Searching for libpsmile...")
find_library(psmile_lib NAMES libpsmile.MPI1.a HINTS ENV OASISDIR PATH_SUFFIXES lib)
if(psmile_lib)
  message(STATUS "  Found PSMILE: ${psmile_lib}")
else()
  message(STATUS "  psmile not found...")
endif()


message(STATUS "  Searching for libmct...")
find_library(mct_lib NAMES libmct.a HINTS ENV OASISDIR PATH_SUFFIXES lib)
if(mct_lib)
  message(STATUS "  Found: ${mct_lib}")
else()
  message(STATUS "  mct not found...")
endif()

message(STATUS "  Searching for libmpeu...")
find_library(mpeu_lib NAMES libmpeu.a HINTS ENV OASISDIR PATH_SUFFIXES lib)
if(mpeu_lib)
  message(STATUS "  Found: ${mpeu_lib}")
else()
  message(STATUS "  mpeu not found...")
endif()

message(STATUS "  Searching for libscrip...")
find_library(scrip_lib NAMES libscrip.a HINTS ENV OASISDIR PATH_SUFFIXES lib)
if(scrip_lib)
  message(STATUS "  Found: ${scrip_lib}")
else()
  message(STATUS "  scrip not found...")
endif()

message(STATUS "  Searching for mod_oasis...")
find_path(oasis_inc mod_oasis.mod PATHS $ENV{OASISDIR}/build/lib/psmile.MPI1)
if(oasis_inc)
  message(STATUS "  Found: ${oasis_inc}")
else()
  message(STATUS "  mod_oasis not found...")
endif()

add_library(PSMILE::PSMILE STATIC IMPORTED)
set_target_properties(PSMILE::PSMILE PROPERTIES
  IMPORTED_LOCATION "${psmile_lib}"
  )

add_library(MCT::MCT STATIC IMPORTED)
set_target_properties(MCT::MCT PROPERTIES
  IMPORTED_LOCATION "${mct_lib}"
  )
target_include_directories(MCT::MCT INTERFACE ${mct_inc})

add_library(MPEU::MPEU STATIC IMPORTED)
set_target_properties(MPEU::MPEU PROPERTIES
  IMPORTED_LOCATION "${mpeu_lib}"
  )

add_library(SCRIP::SCRIP STATIC IMPORTED)
set_target_properties(SCRIP::SCRIP PROPERTIES
  IMPORTED_LOCATION "${scrip_lib}"
  )

add_library(OASIS::OASIS INTERFACE IMPORTED)
target_link_libraries(OASIS::OASIS INTERFACE PSMILE::PSMILE MCT::MCT MPEU::MPEU SCRIP::SCRIP)
target_include_directories(OASIS::OASIS INTERFACE ${oasis_inc})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
  ${CMAKE_FIND_PACKAGE_NAME}
  REQUIRED_VARS psmile_lib mct_lib mpeu_lib scrip_lib oasis_inc)
