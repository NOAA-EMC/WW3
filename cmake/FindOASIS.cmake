find_library(psmile_lib NAMES libpsmile.MPI1.a HINTS ENV OASISDIR PATH_SUFFIXES lib)

find_library(mct_lib NAMES libmct.a HINTS OASISDIR PATH_SUFFIXES lib)
find_library(mpeu_lib NAMES libmpeu.a HINTS OASISDIR PATH_SUFFIXES lib)
find_library(scrip_lib NAMES libscrip.a HINTS OASISDIR PATH_SUFFIXES lib)

find_path(oasis_inc mod_oasis.mod HINTS ENV OASISDIR PATH_SUFFIXES psmile.MPI1)

add_library(PSMILE::PSMILE STATIC IMPORTED)
set_target_properties(PSMILE::PSMILE PROPERTIES
  IMPORTED_LOCATION "${psmile_lib}"
  )

add_library(MCT::MCT STATIC IMPORTED)
set_target_properties(MCT::MCT PROPERTIES
  IMPORTED_LOCATION "${mct_lib}"
  )
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

find_package_handle_standard_args(
  ${CMAKE_FIND_PACKAGE_NAME}
  REQUIRED_VARS psmile_lib mct_lib mpeu_lib scrip_lib oasis_inc)

message(STATUS "Found PSMILE: ${psmile_lib}")
message(STATUS "Found MCT: ${mct_lib}")
message(STATUS "Found MPEU: ${mpeu_lib}")
message(STATUS "Found SCRIP: ${scrip_lib}")
message(STATUS "Found mod_oasis: ${oasis_inc}")
