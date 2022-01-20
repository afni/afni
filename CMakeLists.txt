cmake_minimum_required(VERSION 3.14.7 FATAL_ERROR)

# ##### Set policies
cmake_policy(SET CMP0077 NEW)
cmake_policy(SET CMP0048 NEW)
# ##### Set module path and import some macros
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(get_git_repo_version)
include(get_build_macros_and_functions)
include(CMakeDependentOption)

if( (NOT DEFINED ENV{CC}) )
  if(DEFINED CMAKE_TOOLCHAIN_FILE)
    # Toolchain file defines the correct compiler settings for MacOS
    option(AFNI_COMPILER_CHECK "Check that CC and CXX are set as expected" OFF)
  else()
    option(AFNI_COMPILER_CHECK "Check that CC and CXX are set as expected" ON)
  endif()
  if (AFNI_COMPILER_CHECK)
    message(FATAL_ERROR "
    Set the compilers for the build by setting the environment variables CC and CXX...
      Hint 1: export CC=$(which gcc);export CXX=$(which g++)
      Hint 2: If the above, or indeed this failure, is not useful you can consider passing -DAFNI_COMPILER_CHECK=OFF to cmake
      "
      )
  endif()
endif()

# ##### Project info
project(
  AFNI
  VERSION ${GIT_REPO_VERSION}
  DESCRIPTION "A package for the analysis of functional neuroimaging."
)
string(TIMESTAMP BUILD_DATE "%m %d %Y")

set(AFNI_VERSION_HEADER "${CMAKE_CURRENT_BINARY_DIR}/src/AFNI_version.h")
check_header_has_been_created("${AFNI_VERSION_HEADER}" "${GIT_REPO_VERSION}")

if("/usr/bin/cc" STREQUAL "${CMAKE_C_COMPILER}")
    # This should never happen, compiler should be gcc or clang.
    message(FATAL_ERROR "wrong compiler: ${CMAKE_C_COMPILER}. Please check to see if clang or gcc are installed and functioning")
endif()
# set(CMAKE_LINK_WHAT_YOU_USE $<$<C_COMPILER_ID:GNU>:ON>)
include(afni_cmake_build_options)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
add_definitions("-DDYNAMIC_suffix=\"${CMAKE_SHARED_LIBRARY_SUFFIX}\"")
# set(CMAKE_C_FLAGS_DEBUG
#     "${CMAKE_C_FLAGS_DEBUG} -DAFNI_DEBUG -DIMSEQ_DEBUG -DDISPLAY_DEBUG -DTHD_DEBUG"
# )
# set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DUSE_TRACING") # XXX not in default cflags
# if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
#   set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DREAD_WRITE_64 -DLINUX2")
# elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
#   set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DREAD_WRITE_64 -DLINUX -DDARWIN")
# endif()

# Get mapping between each target name and it's installation component
set(COMPONENTS_MAPFILE ${CMAKE_CURRENT_SOURCE_DIR}/packaging/installation_components.txt )
file(READ
  ${COMPONENTS_MAPFILE}
  CMPNT_MAPPING)
list(TRANSFORM CMPNT_MAPPING REPLACE "\n" ";")


# ##### Dependencies
include(afni_project_dependencies)

# Other details of dependencies and configuration
set(CMAKE_C_FLAGS_DEBUG
    "${CMAKE_C_FLAGS_DEBUG} -DAFNI_DEBUG -DIMSEQ_DEBUG -DDISPLAY_DEBUG -DTHD_DEBUG"
)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DUSE_TRACING -DHAVE_ZLIB") # XXX not in default cflags
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DREAD_WRITE_64 -DLINUX2")
elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DREAD_WRITE_64 -DLINUX -DDARWIN")
endif()

# ##### Docs, licence, and README
set(DOC_DIR ${PROJECT_SOURCE_DIR}/doc)
quotize("${DOC_DIR}/README/README.copyright" license)
quotize("${DOC_DIR}/README/README.environment" readme_env)
quotize("${DOC_DIR}/README/README.afnigui" readme_afnigui)
add_subdirectory(doc)
quotize("${CMAKE_CURRENT_SOURCE_DIR}/afni_papers.txt" afni_papers)

# ##### Add the source code to build
add_subdirectory(src)

# ##### Tests

if(${ENABLE_TESTS})
  enable_testing()
endif()
set(TESTS_DIR ${CMAKE_CURRENT_SOURCE_DIR}/tests)
if(EXISTS ${DOC_DIR})
  add_subdirectory(tests)
endif()

# ##### Write out package version information
write_basic_package_version_file(
  MyLibConfigVersion.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion
)

# Allow an uninstall (with some risk of messiness)
if(NOT TARGET uninstall_afni)
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake" IMMEDIATE @ONLY
  )

  add_custom_target(
    uninstall_afni COMMAND ${CMAKE_COMMAND} -P
                      ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake
  )
  if(STANDARD_PYTHON_INSTALL)
  add_custom_target(
    uninstall_python_package COMMAND ${Python_EXECUTABLE} -m pip uninstall -y afnipy
  )
  add_dependencies(uninstall_afni uninstall_python_package)
  endif()
endif()

# Generate packaging if this is the top level project
if(USE_CPACK)
  if(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    include(CMakeCPack.cmake)
  endif()
endif()

