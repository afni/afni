cmake_minimum_required(VERSION 3.11 FATAL_ERROR)
# ##### Set policies
cmake_policy(SET CMP0048 NEW)
# ##### Set module path and import some macros
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(get_git_repo_version)
include(get_build_macros_and_functions)
include(CMakeDependentOption)
# ##### Project info
if(CMAKE_GENERATOR MATCHES "Ninja")
  file(
    WRITE "${CMAKE_BINARY_DIR}/GNUMakeRulesOverwrite.cmake"
    "STRING(REPLACE \"-MD\" \"-MMD\" CMAKE_DEPFILE_FLAGS_C \"\${CMAKE_DEPFILE_FLAGS_C}\")\n"
    "STRING(REPLACE \"-MD\" \"-MMD\" CMAKE_DEPFILE_FLAGS_CXX \"\${CMAKE_DEPFILE_FLAGS_CXX}\")\n"
  )
  set(CMAKE_USER_MAKE_RULES_OVERRIDE
      "${CMAKE_BINARY_DIR}/GNUMakeRulesOverwrite.cmake"
      CACHE INTERNAL ""
  )
endif()

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
set(BUILD_SHARED_LIBS ON)

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
option(USE_OpenMP "Use OpenMP to enamble <omp.h>" ON)
set(CMAKE_C_FLAGS_DEBUG
    "${CMAKE_C_FLAGS_DEBUG} -DAFNI_DEBUG -DIMSEQ_DEBUG -DDISPLAY_DEBUG -DTHD_DEBUG"
)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DUSE_TRACING") # XXX not in default cflags
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DREAD_WRITE_64 -DLINUX2")
elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DREAD_WRITE_64 -DLINUX -DDARWIN")
endif()

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
message(${INSTALL_PREFIX})
message(${AFNI_INSTALL_RUNTIME_DIR})

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src)
if(CONDA_BUILD)
  get_filename_component(AFNI_INSTALL_LIBRARY_DIR ${AFNI_INSTALL_LIBRARY_DIR} DIRECTORY)
  get_filename_component(AFNI_INSTALL_INCLUDE_DIR ${AFNI_INSTALL_INCLUDE_DIR} DIRECTORY)
endif()

# Other possible directories to set
set(AFNI_INSTALL_ATLAS_DIR "${INSTALL_PREFIX}/atlases")
set(AFNI_INSTALL_PICS_DIR "${INSTALL_PREFIX}/funstuff")
# ##### Dependencies
include(CPack)
include(CMakePackageConfigHelpers)
find_package(OpenMP REQUIRED)
include(FindMotif)
include(FindNetCDF)
include(FindX11)
include(FindStandardMathLibrary)
include(FindLibR)
include(BuildType)

# Find or fetch nifti_clib a prefix can be used to avoid collisions with pre-existing
# installations of the nifti libraries
include(get_nifti_and_gifti_targets)

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
