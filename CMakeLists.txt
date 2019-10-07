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
project(AFNI
        VERSION ${GIT_REPO_VERSION}
        DESCRIPTION "A package for the analysis of functional neuroimaging.")
string(TIMESTAMP BUILD_DATE "%m %d %Y")
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/src/AFNI_version.h.in "${CMAKE_CURRENT_BINARY_DIR}/src/AFNI_version.h" @ONLY)


################################################################################
# Set build variables and include modules
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(BUILD_SHARED_LIBS ON)

set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -DAFNI_DEBUG -DIMSEQ_DEBUG -DDISPLAY_DEBUG -DTHD_DEBUG")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DUSE_TRACING") # XXX not in default cflags
if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DREAD_WRITE_64 -DLINUX2")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DREAD_WRITE_64 -DLINUX -DDARWIN")
endif()

set(INSTALL_TO_ABIN 0)
if(${INSTALL_TO_ABIN})
  set(BASE_INSTALL_DIR $ENV{HOME}/abin)
  set(AFNI_INSTALL_LIBRARY_DIR ${BASE_INSTALL_DIR}/lib)
  set(AFNI_INSTALL_RUNTIME_DIR ${BASE_INSTALL_DIR})
  set(AFNI_INSTALL_INCLUDE_DIR ${BASE_INSTALL_DIR}/include)
else()
set(BASE_INSTALL_DIR ${CMAKE_INSTALL_PREFIX})
set(AFNI_INSTALL_LIBRARY_DIR ${BASE_INSTALL_DIR}/lib)
set(AFNI_INSTALL_RUNTIME_DIR ${BASE_INSTALL_DIR}/bin)
set(AFNI_INSTALL_INCLUDE_DIR ${BASE_INSTALL_DIR}/include)
endif()

if (CONDA_BUILD)
  get_filename_component(AFNI_INSTALL_LIBRARY_DIR ${AFNI_INSTALL_LIBRARY_DIR} DIRECTORY)
  get_filename_component(AFNI_INSTALL_INCLUDE_DIR ${AFNI_INSTALL_INCLUDE_DIR} DIRECTORY)
  message("The library: " ${AFNI_INSTALL_LIBRARY_DIR})
endif()


include(CPack)
include(CMakePackageConfigHelpers)
include(FindX11)
include(FindMotif)
include(FindNetCDF)

# Find or fetch nifti_clib
# a prefix can be used to avoid collisions with pre-existsing installations of
# the nifti libraries
set(NIFTI_PACKAGE_PREFIX "" CACHE STRING "Nifti package prefix for afni project.")
set(NIFTI_LIBRARY_PROPERTIES ${AFNI_LIBRARY_PROPERTIES} CACHE STRING "Set the nifti library properties to match AFNI" FORCE)
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

# WIP: managing external programs as sub-modules
# https://cliutils.gitlab.io/modern-cmake/chapters/projects/submodule.html
find_package(Git QUIET)
if(GIT_FOUND AND EXISTS "${PROJECT_SOURCE_DIR}/.git")
# Update submodules as needed
    option(GIT_SUBMODULE "Check submodules during build" OFF)
    if(GIT_SUBMODULE)
        message(STATUS "Submodule update")
        execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
                        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                        RESULT_VARIABLE GIT_SUBMOD_RESULT)
        if(NOT GIT_SUBMOD_RESULT EQUAL "0")
            message(FATAL_ERROR "git submodule update --init failed with ${GIT_SUBMOD_RESULT}, please checkout submodules")
        endif()
    endif(GIT_SUBMODULE)
endif()

# if(NOT EXISTS "${PROJECT_SOURCE_DIR}/extern/repo/CMakeLists.txt")
#     message(FATAL_ERROR "The submodules were not downloaded! GIT_SUBMODULE was turned off or failed. Please update submodules and try again.")
# endif()


# Write out package version information
write_basic_package_version_file(
    MyLibConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY AnyNewerVersion
    )


