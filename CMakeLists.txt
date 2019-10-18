cmake_minimum_required(VERSION 3.11 FATAL_ERROR)
################################################################################
# Set policies
cmake_policy(SET CMP0048 NEW)
################################################################################
# Set module path and import some macros
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(get_git_repo_version)
include(get_build_macros_and_functions)
################################################################################
# Project info
if (CMAKE_GENERATOR MATCHES "Ninja")
    file(
        WRITE "${CMAKE_BINARY_DIR}/GNUMakeRulesOverwrite.cmake" 
            "STRING(REPLACE \"-MD\" \"-MMD\" CMAKE_DEPFILE_FLAGS_C \"\${CMAKE_DEPFILE_FLAGS_C}\")\n"
            "STRING(REPLACE \"-MD\" \"-MMD\" CMAKE_DEPFILE_FLAGS_CXX \"\${CMAKE_DEPFILE_FLAGS_CXX}\")\n"
    )
    set(CMAKE_USER_MAKE_RULES_OVERRIDE "${CMAKE_BINARY_DIR}/GNUMakeRulesOverwrite.cmake" CACHE INTERNAL "")
endif()



project(AFNI
        VERSION ${GIT_REPO_VERSION}
        DESCRIPTION "A package for the analysis of functional neuroimaging.")
string(TIMESTAMP BUILD_DATE "%m %d %Y")

set(AFNI_VERSION_HEADER "${CMAKE_CURRENT_BINARY_DIR}/src/AFNI_version.h")
check_header_has_been_created("${AFNI_VERSION_HEADER}" "${GIT_REPO_VERSION}")

################################################################################
# Set build variables and include modules
 # need to do this only for gcc:
# set(CMAKE_LINK_WHAT_YOU_USE ON)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(BUILD_SHARED_LIBS ON)
OPTION(AFNI_BUILD_CORELIBS_ONLY
       "Only build core libraries, no SUMA, plugins or programs"
       OFF)
OPTION(BUILD_BINARIES
       "Build a large portion of the C executables"
       ON)
OPTION(BUILD_X_PROGS "Build X dependent code too." OFF)
mark_as_advanced(BUILD_X_PROGS)

OPTION(DO_NOT_INSTALL_SCRIPTS "Omits script installation" OFF)
mark_as_advanced(DO_NOT_INSTALL_SCRIPTS)

set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -DAFNI_DEBUG -DIMSEQ_DEBUG -DDISPLAY_DEBUG -DTHD_DEBUG")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DUSE_TRACING") # XXX not in default cflags
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DREAD_WRITE_64 -DLINUX2")
elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DREAD_WRITE_64 -DLINUX -DDARWIN")
endif()
set(AFNI_INSTALL_ATLAS_DIR "atlases" CACHE PATH
    "Directory to install atlases (and related information) into")
set(AFNI_INSTALL_PICS_DIR "funstuff" CACHE PATH
    "Directory to install images into")

set(AFNI_INSTALL_LIBRARY_DIR ${CMAKE_INSTALL_PREFIX}/lib CACHE PATH "")
set(AFNI_INSTALL_RUNTIME_DIR ${CMAKE_INSTALL_PREFIX}/bin CACHE PATH "")
set(AFNI_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include CACHE PATH "")

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src)
if (CONDA_BUILD)
  get_filename_component(AFNI_INSTALL_LIBRARY_DIR ${AFNI_INSTALL_LIBRARY_DIR} DIRECTORY)
  get_filename_component(AFNI_INSTALL_INCLUDE_DIR ${AFNI_INSTALL_INCLUDE_DIR} DIRECTORY)
  message("The library: " ${AFNI_INSTALL_LIBRARY_DIR})
endif()


include(CPack)
include(CMakePackageConfigHelpers)
include(FindMotif)
include(FindNetCDF)
include(FindX11)
include(FindStandardMathLibrary)
# Find or fetch nifti_clib
# a prefix can be used to avoid collisions with pre-existsing installations of
# the nifti libraries
FIND_LIBRARY(HAVE_LIBM NAMES math m libm)
message(${HAVE_LIBM})
message(have_lib: ${HAVE_LIBM})

include(get_nifti_and_gifti_targets)


################################################################################
# Docs, licence, and README
set(DOC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/doc)
if(EXISTS ${DOC_DIR})
quotize("${DOC_DIR}/README/README.copyright" license)
quotize("${DOC_DIR}/README/README.environment" readme_env)
quotize("${DOC_DIR}/README/README.afnigui" readme_afnigui)
add_subdirectory(doc)
endif()
quotize("${CMAKE_CURRENT_SOURCE_DIR}/afni_papers.txt" afni_papers)

################################################################################
# Add the source code to build
add_subdirectory(src)
################################################################################
# Tests 
option(ENABLE_TESTS "Enable tests" OFF)
if (${ENABLE_TESTS})
    enable_testing()
endif()
set(TESTS_DIR ${CMAKE_CURRENT_SOURCE_DIR}/tests)
if(EXISTS ${DOC_DIR})
add_subdirectory(tests)
endif()

################################################################################
# Using clang-tidy:
if(CMAKE_VERSION VERSION_GREATER 3.6)
    # Add clang-tidy if available
    option(CLANG_TIDY_FIX "Perform fixes for Clang-Tidy" OFF)
    find_program(
        CLANG_TIDY_EXE
        NAMES "clang-tidy"
        DOC "Path to clang-tidy executable"
    )

    if(CLANG_TIDY_EXE)
        if(CLANG_TIDY_FIX)
            set(CMAKE_CXX_CLANG_TIDY "${CLANG_TIDY_EXE}" "-fix")
        else()
            set(CMAKE_CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")
        endif()
    endif()
endif()

# Write out package version information
write_basic_package_version_file(
    MyLibConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY AnyNewerVersion
    )


