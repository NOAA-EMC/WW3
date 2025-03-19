# Doxygen documentation- Matt Masarik 24-Jul-2024.
function(EnableDoxygen outdir)
  find_package(Doxygen REQUIRED)
    if (NOT DOXYGEN_FOUND)
      add_custom_target(enable_docs
          COMMAND false
          COMMENT "Doxygen not found")
      return()
    endif()

    set(src_input "${CMAKE_SOURCE_DIR}/model/src")
    set(doc_output "${CMAKE_BINARY_DIR}/${outdir}")
    file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/${outdir}/html)
    CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/docs/Doxyfile.in
        ${CMAKE_BINARY_DIR}/${outdir}/Doxyfile @ONLY)
    set(DOXYGEN_GENERATE_HTML YES)
    set(DOXYGEN_QUIET YES)
    add_custom_target(enable_docs
        COMMAND
          ${DOXYGEN_EXECUTABLE} ${CMAKE_BINARY_DIR}/${outdir}/Doxyfile
        WORKING_DIRECTORY
          ${CMAKE_BINARY_DIR}/${outdir}
        COMMENT
          "Generate Doxygen HTML documentation")
    message("-- Doxygen HTML index page: "
        ${CMAKE_BINARY_DIR}/${outdir}/html/index.html)
endfunction()
