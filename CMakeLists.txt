cmake_minimum_required(VERSION 3.11 FATAL_ERROR)

# ##### Set policies
cmake_policy(SET CMP0048 NEW)

# ##### Set module path and import some macros
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(get_git_repo_version)
include(get_build_macros_and_functions)
include(CMakeDependentOption)

# ##### Project info
project(
  AFNI
  VERSION ${GIT_REPO_VERSION}
  DESCRIPTION "A package for the analysis of functional neuroimaging."
)
string(TIMESTAMP BUILD_DATE "%m %d %Y")

set(AFNI_VERSION_HEADER "${CMAKE_CURRENT_BINARY_DIR}/src/AFNI_version.h")
check_header_has_been_created("${AFNI_VERSION_HEADER}" "${GIT_REPO_VERSION}")

set(CMAKE_LINK_WHAT_YOU_USE $<$<CXX_COMPILER_ID:GNU>:ON>)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
option(BUILD_SHARED_LIBS "Toggle building shared libraries" ON)

option(AFNI_BUILD_CORELIBS_ONLY
       "Only build core libraries, no SUMA, plugins or programs" OFF
)
cmake_dependent_option(
  BUILD_BINARIES "Build a large portion of the C executables" ON
  "NOT AFNI_BUILD_CORELIBS_ONLY" OFF
)
cmake_dependent_option(
  DO_NOT_INSTALL_SCRIPTS "Omits script installation" OFF "NOT AFNI_BUILD_CORELIBS_ONLY"
  OFF
)
mark_as_advanced(DO_NOT_INSTALL_SCRIPTS)
cmake_dependent_option(
  BUILD_X_DEPENDENT_GUI_PROGS "Build GUI applications with plugins." OFF
  "NOT AFNI_BUILD_CORELIBS_ONLY" OFF
)
cmake_dependent_option(
  BUILD_PLUGINS "Build plugins for AFNI GUI." OFF "BUILD_X_DEPENDENT_GUI_PROGS;" OFF
)
mark_as_advanced(BUILD_PLUGINS)
cmake_dependent_option(
  BUILD_OPENGL_DEPENDENT_GUI_PROGS
  "Build OPEN_GL dependent GUI applications with plugins." OFF
  "NOT AFNI_BUILD_CORELIBS_ONLY;BUILD_X_DEPENDENT_GUI_PROGS" OFF
)
option(SHOW_UNBUILT_PROGS "Display missing progs during config" OFF)
mark_as_advanced(SHOW_UNBUILT_PROGS)
option(USE_OMP "Use OpenMP to enamble <omp.h>" ON)
# set(CMAKE_C_FLAGS_DEBUG
#     "${CMAKE_C_FLAGS_DEBUG} -DAFNI_DEBUG -DIMSEQ_DEBUG -DDISPLAY_DEBUG -DTHD_DEBUG"
# )
# set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DUSE_TRACING") # XXX not in default cflags
# if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
#   set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DREAD_WRITE_64 -DLINUX2")
# elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
#   set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DREAD_WRITE_64 -DLINUX -DDARWIN")
# endif()

set(ALTERNATIVE_INSTALL_ROOT
    ""
    CACHE
      PATH
      "Directory to install into (with appropriate sub directory bin,lib, or include)"
)

set_if_not_defined(INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

set_if_not_defined(AFNI_INSTALL_LIBRARY_DIR "${INSTALL_PREFIX}/lib")
set_if_not_defined(AFNI_INSTALL_RUNTIME_DIR "${INSTALL_PREFIX}/bin")
set_if_not_defined(AFNI_INSTALL_INCLUDE_DIR "${INSTALL_PREFIX}/include")

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src)
if(CONDA_BUILD)
  get_filename_component(AFNI_INSTALL_LIBRARY_DIR ${AFNI_INSTALL_LIBRARY_DIR} DIRECTORY)
  get_filename_component(AFNI_INSTALL_INCLUDE_DIR ${AFNI_INSTALL_INCLUDE_DIR} DIRECTORY)
endif()

# Other possible directories to set
set(AFNI_INSTALL_ATLAS_DIR "${INSTALL_PREFIX}/atlases")
set(AFNI_INSTALL_PICS_DIR "${INSTALL_PREFIX}/funstuff")

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
set(DOC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/doc)
if(EXISTS ${DOC_DIR})
  quotize("${DOC_DIR}/README/README.copyright" license)
  quotize("${DOC_DIR}/README/README.environment" readme_env)
  quotize("${DOC_DIR}/README/README.afnigui" readme_afnigui)
  add_subdirectory(doc)
endif()
quotize("${CMAKE_CURRENT_SOURCE_DIR}/afni_papers.txt" afni_papers)

# ##### Add the source code to build
add_subdirectory(src)

# ##### Tests
option(ENABLE_TESTS "Enable tests" OFF)
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

if(NOT TARGET uninstall)
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake" IMMEDIATE @ONLY
  )

  add_custom_target(
    uninstall COMMAND ${CMAKE_COMMAND} -P
                      ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake
  )
endif()


# Setup for tests. Things like, clear some cache dirs, and check the tests
# from the source tree are symlinked into the build tree.
add_custom_target(
  test_setup
  COMMAND 
  ${CMAKE_COMMAND} 
  -D TESTS_PATH=${CMAKE_CURRENT_SOURCE_DIR}/tests
  -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/setup_for_tests.cmake
  )


# Define PATH modification required for within build tree tests execution
set(BUILD_BINARIES ${CMAKE_CURRENT_BINARY_DIR}/src)
set(T_SCRIPTS ${CMAKE_CURRENT_SOURCE_DIR}/src/scripts_install)
set(P_SCRIPTS ${CMAKE_CURRENT_SOURCE_DIR}/src/python_scripts/afni_python)
set(R_SCRIPTS ${CMAKE_CURRENT_SOURCE_DIR}/src/R_scripts)
set(J_SCRIPTS ${CMAKE_CURRENT_SOURCE_DIR}/src/jzosky)
set(J_SCRIPTS "${J_SCRIPTS}:${J_SCRIPTS}/lib_RetroTS")
set(TESTING_PATHS "${BUILD_BINARIES}:${T_SCRIPTS}:${P_SCRIPTS}:${R_SCRIPTS}:${J_SCRIPTS}")
set(PATH_FOR_TESTING "${TESTING_PATHS}:$ENV{PATH}")
file(WRITE ${CMAKE_BINARY_DIR}/TESTING_PATHS.txt ${TESTING_PATHS})

# Create test target for running tests within the build tree
add_custom_target(
  pytest
  DEPENDS test_setup
  WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/afni_tests"
  COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR}
  COMMAND echo Running tests with modified path to use build binaries and scripts in the source tree...
  COMMAND echo Prepending ${TESTING_PATHS}
  COMMAND echo When using 'ninja test' one can define "ARGS" to pass additional arguments to pytest...
  COMMAND ${CMAKE_COMMAND} -E env PATH=${PATH_FOR_TESTING}  ${Python_EXECUTABLE} -B -m pytest $$ARGS
  USES_TERMINAL
)

