# This is the CMake file for building the docs directory of fv3atm.
#
# Alex Richert, Ed Hartnett

# Create doxyfile.
SET(abs_top_srcdir "${CMAKE_SOURCE_DIR}")
MESSAGE(STATUS "AJWR ${abs_top_srcdir}")
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/user_guide.md ${CMAKE_CURRENT_BINARY_DIR}/user_guide.md @ONLY)
ADD_CUSTOM_TARGET(doxygen_doc ALL
  ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT "Generating API Documentation with Doxygen"
  DEPENDS Doxyfile.in user_guide.md
  VERBATIM)
