cmake_minimum_required(VERSION 3.11 FATAL_ERROR)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)


execute_process(COMMAND git "describe" "--tags"
  OUTPUT_VARIABLE GIT_REPO_VERSION_UNCLEANED
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
string(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+(\\.[0-9]+(\\.g[0-9a-f]+)?)?"
 GIT_REPO_VERSION  "${GIT_REPO_VERSION_UNCLEANED}")

project(AFNI
        VERSION ${GIT_REPO_VERSION}
        DESCRIPTION "A package for the analysis of functional neuroimaging.")
set( AFNI_LIBRARY_PROPERTIES 
  BUILD_SHARED_LIBS TRUE
  POSITION_INDEPENDENT_CODE TRUE
)

## Needed for correct linking when installing.
## see https://gitlab.kitware.com/cmake/community/wikis/doc/cmake/RPATH-handling
SET(CMAKE_INSTALL_RPATH /usr/local/lib)

### Require out-of-source builds
file(TO_CMAKE_PATH "${PROJECT_BINARY_DIR}/CMakeLists.txt" LOC_PATH)
if(EXISTS "${LOC_PATH}")
    message(FATAL_ERROR "You cannot build in a source directory (or any directory with a CMakeLists.txt file). Please make a build subdirectory. Feel free to remove CMakeCache.txt and CMakeFiles.")
endif()

# macro(get_nifti_rpath)
# # Prepare RPATH
# file(RELATIVE_PATH _rel ${CMAKE_INSTALL_PREFIX}/${NIFTI_INSTALL_RUNTIME_DIR} ${CMAKE_INSTALL_PREFIX})
# if(APPLE)
#   set(_rpath "@loader_path/${_rel}")
# else()
#   set(_rpath "\$ORIGIN/${_rel}")
# endif()
# file(TO_NATIVE_PATH "${_rpath}/${NIFTI_INSTALL_LIBRARY_DIR}" message_RPATH)
# endmacro()

macro(add_afni_target_properties target)
  # this macro sets some default properties for targets in this project
  # get_nifti_rpath()
  # define installation prefix
  # get_target_property(TARGET_TYPE ${target} TYPE)
  # if (INCLUDE_PREFIX_IN_EXPORT)
  #   set(TARGET_EXPORT_NAME ${NIFTI_PACKAGE_PREFIX}${target})
  # else()
  #   set(TARGET_EXPORT_NAME ${target})
  # endif()

  # Set the target properties
  # Currently this includes setting up post-install linking, prefixing target names with NIFTI_PACKAGE_PREFIX if requests
  set_target_properties(${target} PROPERTIES
    # MACOSX_RPATH ON
    # SKIP_BUILD_RPATH OFF
    # BUILD_WITH_INSTALL_RPATH OFF
    # INSTALL_RPATH "${message_RPATH}"
    # INSTALL_RPATH_USE_LINK_PATH ON
    # OUTPUT_NAME "${NIFTI_PACKAGE_PREFIX}${target}"
    # EXPORT_NAME "${TARGET_EXPORT_NAME}"
    ${AFNI_LIBRARY_PROPERTIES}
    )
endmacro()


set(INSTALL_TO_ABIN 0)
if(${INSTALL_TO_ABIN})
  set(AFNI_INSTALL_RUNTIME_DIR $ENV{HOME}/abin)
  set(AFNI_INSTALL_LIBRARY_DIR $ENV{HOME}/abin)
  set(AFNI_INSTALL_INCLUDE_DIR $ENV{HOME}/abin)
  set(AFNI_INSTALL_ARCHIVE_DIR $ENV{HOME}/abin)
else()
  set(AFNI_INSTALL_RUNTIME_DIR ${CMAKE_INSTALL_PREFIX}/bin)
  set(AFNI_INSTALL_LIBRARY_DIR ${CMAKE_INSTALL_PREFIX}/lib/AFNI)
  set(AFNI_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/AFNI)
  set(AFNI_INSTALL_ARCHIVE_DIR ${CMAKE_INSTALL_PREFIX}/lib/AFNI)
endif()
# Read in the line containing the version
# file(STRINGS "${CMAKE_CURRENT_SOURCE_DIR}/include/My/Version.hpp"
#     VERSION_STRING REGEX ${VERSION_REGEX})

# # Pick out just the version
# string(REGEX REPLACE ${VERSION_REGEX} "\\1" VERSION_STRING "${VERSION_STRING}")

# Automatically getting PROJECT_VERSION_MAJOR, My_VERSION_MAJOR, etc.
# project(My LANGUAGES CXX VERSION ${VERSION_STRING})


set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_CURRENT_SOURCE_DIR}/cmake")


include(GNUInstallDirs OPTIONAL RESULT_VARIABLE GNUInstallDirs_FOUND)
if(NOT GNUInstallDirs_FOUND)
  # XXX add windows paths
endif(NOT GNUInstallDirs_FOUND)

# Add the source code to build
add_subdirectory(src)


# Docs, licence, and README
function(quotize input basename)
  set(output "${basename}.h")
  add_custom_command(OUTPUT "${output}"
    COMMAND quotize "${basename}" < "${input}" > "${output}"
    MAIN_DEPENDENCY quotize)
endfunction(quotize)
set(DOC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/doc)
if(EXISTS ${DOC_DIR})
quotize("${DOC_DIR}/README/README.copyright" license)
quotize("${DOC_DIR}/README/README.environment" readme_env)
quotize("${DOC_DIR}/README/README.afnigui" readme_afnigui)
add_subdirectory(doc)
endif()
quotize("${CMAKE_CURRENT_SOURCE_DIR}/afni_papers.txt" afni_papers)



# Tests 
enable_testing()
set(TESTS_DIR ${CMAKE_CURRENT_SOURCE_DIR}/tests)
if(EXISTS ${DOC_DIR})
add_subdirectory(tests)
endif()

add_test(3dAllineate python3 ${TESTS_DIR}/pytest_tests/test_3dAllineate.py)
add_test(3dSkullStrip python3 ${TESTS_DIR}/pytest_tests/test_3dSkullStrip.py)
add_test(help_messages python3 ${TESTS_DIR}/pytest_tests/test_help_messages.py)
add_test(3dClustSim python3 ${TESTS_DIR}/pytest_tests/test_3dClustSim.py)
add_test(3dttest python3 ${TESTS_DIR}/pytest_tests/test_3dttest++.py)
# add_test(afni_proc python3 ${TESTS_DIR}/pytest_tests/test_afni_proc.py)



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