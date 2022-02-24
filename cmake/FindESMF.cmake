# - Try to find ESMF
#
# Requires setting ESMFMKFILE to the filepath of esmf.mk. If this is NOT set,
# then ESMF_FOUND will always be FALSE. If ESMFMKFILE exists, then ESMF_FOUND=TRUE
# and all ESMF makefile variables will be set in the global scope. Optionally,
# set ESMF_MKGLOBALS to a string list to filter makefile variables. For example,
# to globally scope only ESMF_LIBSDIR and ESMF_APPSDIR variables, use this CMake
# command in CMakeLists.txt:
#
#   set(ESMF_MKGLOBALS "LIBSDIR" "APPSDIR")


# Add the ESMFMKFILE path to the cache if defined as system env variable
if(DEFINED ENV{ESMFMKFILE} AND NOT DEFINED ESMFMKFILE)
  set(ESMFMKFILE $ENV{ESMFMKFILE} CACHE FILEPATH "Path to ESMF mk file")
endif()

# If it's not explicitly set try to find esmf.mk file in default locations (ESMF_ROOT, CMAKE_PREFIX_PATH, etc)
if(NOT DEFINED ESMFMKFILE)
  find_path(ESMFMKFILE_PATH esmf.mk PATH_SUFFIXES lib lib64)
  if(ESMFMKFILE_PATH)
    set(ESMFMKFILE ${ESMFMKFILE_PATH}/esmf.mk)
    message(STATUS "Found esmf.mk file ${ESMFMKFILE}")
  else()
    message(STATUS "ESMFMKFILE not defined. This is the path to esmf.mk file. \
Without this filepath, ESMF_FOUND will always be FALSE.")
  endif()
endif()

# Only parse the mk file if it is found
if(EXISTS ${ESMFMKFILE})
  # Read the mk file
  file(STRINGS "${ESMFMKFILE}" esmfmkfile_contents)
  # Parse each line in the mk file
  foreach(str ${esmfmkfile_contents})
    # Only consider uncommented lines
    string(REGEX MATCH "^[^#]" def ${str})
    # Line is not commented
    if(def)
      # Extract the variable name
      string(REGEX MATCH "^[^=]+" esmf_varname ${str})
      # Extract the variable's value
      string(REGEX MATCH "=.+$" esmf_vardef ${str})
      # Only for variables with a defined value
      if(esmf_vardef)
        # Get rid of the assignment string
        string(SUBSTRING ${esmf_vardef} 1 -1 esmf_vardef)
        # Remove whitespace
        string(STRIP ${esmf_vardef} esmf_vardef)
        # A string or single-valued list
        if(NOT DEFINED ESMF_MKGLOBALS)
          # Set in global scope
          set(${esmf_varname} ${esmf_vardef})
          # Don't display by default in GUI
          mark_as_advanced(esmf_varname)
        else() # Need to filter global promotion
          foreach(m ${ESMF_MKGLOBALS})
            string(FIND ${esmf_varname} ${m} match)
            # Found the string
            if(NOT ${match} EQUAL -1)
              # Promote to global scope
              set(${esmf_varname} ${esmf_vardef})
              # Don't display by default in the GUI
              mark_as_advanced(esmf_varname)
              # No need to search for the current string filter
              break()
            endif()
          endforeach()
        endif()
      endif()
    endif()
  endforeach()

  # Construct ESMF_VERSION from ESMF_VERSION_STRING_GIT
  # ESMF_VERSION_MAJOR and ESMF_VERSION_MINOR are defined in ESMFMKFILE
  set(ESMF_VERSION 0)
  set(ESMF_VERSION_PATCH ${ESMF_VERSION_REVISION})
  set(ESMF_BETA_RELEASE FALSE)
  if(ESMF_VERSION_BETASNAPSHOT MATCHES "^('T')$")
    set(ESMF_BETA_RELEASE TRUE)
    string(REGEX REPLACE ".*beta_snapshot_*\([0-9]*\).*" "\\1" ESMF_BETA_SNAPSHOT "${ESMF_VERSION_STRING_GIT}")
    message(STATUS "Detected ESMF Beta snapshot ${ESMF_BETA_SNAPSHOT}")
  endif()
  set(ESMF_VERSION "${ESMF_VERSION_MAJOR}.${ESMF_VERSION_MINOR}.${ESMF_VERSION_PATCH}")

  separate_arguments(ESMF_F90COMPILEPATHS NATIVE_COMMAND ${ESMF_F90COMPILEPATHS})
  foreach(ITEM ${ESMF_F90COMPILEPATHS})
     string(REGEX REPLACE "^-I" "" ITEM "${ITEM}")
     list(APPEND tmp ${ITEM})
  endforeach()
  set(ESMF_F90COMPILEPATHS ${tmp})

  # Look for static library, if not found try dynamic library
  find_library(esmf_lib NAMES libesmf.a PATHS ${ESMF_LIBSDIR})
  if(esmf_lib MATCHES "esmf_lib-NOTFOUND")
    unset(esmf_lib)
    message(STATUS "Static ESMF library not found, searching for dynamic library instead")
    find_library(esmf_lib NAMES esmf_fullylinked libesmf.so PATHS ${ESMF_LIBSDIR})
    if(esmf_lib MATCHES "esmf_lib-NOTFOUND")
      unset(esmf_lib)
      message(STATUS "Neither the dynamic nor the static ESMF library was found")
    else()
      set(_library_type SHARED)
    endif()
  else()
    set(_library_type STATIC)
  endif()

  string(STRIP "${ESMF_F90ESMFLINKRPATHS} ${ESMF_F90ESMFLINKPATHS} ${ESMF_F90LINKPATHS} ${ESMF_F90LINKLIBS} ${ESMF_F90LINKOPTS}" ESMF_INTERFACE_LINK_LIBRARIES)
  set(ESMF_LIBRARY_LOCATION ${esmf_lib})

else()

  message(WARNING "ESMFMKFILE ${ESMFMKFILE} does not exist")

endif()

## Finalize find_package
include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    ${CMAKE_FIND_PACKAGE_NAME}
    REQUIRED_VARS ESMF_LIBRARY_LOCATION
                  ESMF_INTERFACE_LINK_LIBRARIES
                  ESMF_F90COMPILEPATHS
    VERSION_VAR ESMF_VERSION)

## If ESMF is found create imported library target
if(ESMF_FOUND)
  add_library(esmf ${_library_type} IMPORTED)
  set_target_properties(esmf PROPERTIES
    IMPORTED_LOCATION "${ESMF_LIBRARY_LOCATION}"
    INTERFACE_INCLUDE_DIRECTORIES "${ESMF_F90COMPILEPATHS}"
    INTERFACE_LINK_LIBRARIES "${ESMF_INTERFACE_LINK_LIBRARIES}")
endif()
