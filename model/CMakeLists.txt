add_subdirectory(src)

# Turn on unit testing.
include(CTest)
if(BUILD_TESTING)
  add_subdirectory(tests)
endif()

