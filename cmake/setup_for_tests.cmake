# Link test directory so everything is relative to the build tree
message("Setting up for within-build-tree testing...")
set(BUILD_TESTS_DIR ${CMAKE_CURRENT_BINARY_DIR}/afni_tests)
if(NOT EXISTS ${BUILD_TESTS_DIR})
  message("Linking tests directory (${TESTS_PATH}) in source tree into build tree (${BUILD_TESTS_DIR})...")
  execute_process(
    COMMAND ${CMAKE_COMMAND} -E create_symlink ${TESTS_PATH} ${BUILD_TESTS_DIR}
  )
endif()

# Cache directores can cause issues when running inside and outside of containers. Might
# reconsider this if the slowdown is too great. message("Removing python cache
# directories...") file(GLOB_RECURSE pycache_dirs
# ${CMAKE_CURRENT_BINARY_DIR}/afni_tests/*_pycache_* ) if(NOT "" STREQUAL
# "${pycache_dirs}") message(FATAL_ERROR error: ${pycache_dirs}) file(REMOVE_RECURSE
# "${pycache_dirs}") endif()
