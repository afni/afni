cmake_minimum_required(VERSION 3.11.0 FATAL_ERROR)

set(PARENTDEMO_MAX_VALIDATED_CMAKE_VERSION "3.13.2")
if("${CMAKE_VERSION}" VERSION_LESS_EQUAL "${PARENTDEMO_MAX_VALIDATED_CMAKE_VERSION}")
  # As of 2018-12-04 NIFTI has been validated to build with cmake version 3.13.1 new
  # policies. Set and use the newest cmake policies that are validated to work
  set(PARENTDEMO_CMAKE_POLICY_VERSION "${CMAKE_VERSION}")
else()
  set(PARENTDEMO_CMAKE_POLICY_VERSION "${PARENTDEMO_MAX_VALIDATED_CMAKE_VERSION}")
endif()
cmake_policy(VERSION ${PARENTDEMO_CMAKE_POLICY_VERSION})

macro(set_if_not_defined var defaultvalue)
  # Macro allowing to set a variable to its default value if not already defined. The
  # default value is set with: (1) if set, the value environment variable <var>. (2) if
  # set, the value of local variable variable <var>. (3) if none of the above, the value
  # passed as a parameter. Setting the optional parameter 'OBFUSCATE' will display
  # 'OBFUSCATED' instead of the real value.
  set(_obfuscate FALSE)
  foreach(arg ${ARGN})
    if(arg STREQUAL "OBFUSCATE")
      set(_obfuscate TRUE)
    endif()
  endforeach()
  if(DEFINED ENV{${var}} AND NOT DEFINED ${var})
    set(_value "$ENV{${var}}")
    if(_obfuscate)
      set(_value "OBFUSCATED")
    endif()
    message(
      STATUS "Setting '${var}' variable with environment variable value '${_value}'"
    )
    set(${var} $ENV{${var}})
  endif()
  if(NOT DEFINED ${var})
    set(_value "${defaultvalue}")
    if(_obfuscate)
      set(_value "OBFUSCATED")
    endif()
    message(STATUS "Setting '${var}' variable with default value '${_value}'")
    set(${var} "${defaultvalue}")
  endif()
endmacro()

set(PARENTDEMO_HOMEPAGE_URL "https://nifti-imaging.github.io")
project(
  PARENTDEMO
  VERSION 0.0.0.1
  DESCRIPTION "A demonstration of how to pull in nifti as a subdirectory"
  LANGUAGES C
)

set(SILLYDEMO_LIBRARY_PROPERTIES
    BUILD_SHARED_LIBS
    TRUE
    POSITION_INDEPENDENT_CODE
    TRUE
    VERSION
    0.1.2
    SOVERSION
    4.5.6
)
set(SILLYDEMONIFTI_INSTALL_RUNTIME_DIR mybin)
set(SILLYDEMONIFTI_INSTALL_LIBRARY_DIR mysharedlibs)
set(SILLYDEMONIFTI_INSTALL_ARCHIVE_DIR mystaticlibs)
set(SILLYDEMO_INSTALL_INCLUDE_DIR myincludes)

find_package(ZLIB REQUIRED)
set(SILLYDEMOZLIB_LIBRARIES ${ZLIB_LIBRARIES})

# set to TRUE if SILLYDEMO* development are not exposed through your package to others
set(SILLYDEMO_INSTALL_NO_DEVELOPMENT FALSE)
# set to TRUE if SILLYDEMO* libraries are not exposed through your package to others
set(SILLYDEMO_INSTALL_NO_LIBRARIES FALSE)

set(SILLYDEMO_TARGETS "SillyDemoTargets")

# STEP 1 -- set build configuration items to override nifti_clib defaults

set(USE_NIFTI2_CODE
    ON
    CACHE BOOL "Do not build NIFTI2 provided by NIFTI" FORCE
)
set(USE_FSL_CODE
    ON
    CACHE BOOL "Do not build FSL provided by NIFTI" FORCE
)
set(USE_CIFTI_CODE
    ON
    CACHE BOOL "Do not build CIFTI provided by NIFTI" FORCE
)
set(USE_NIFTICDF_CODE
    ON
    CACHE BOOL "Do not build NIFTICDF provided by NIFTI" FORCE
)
set(NIFTI_USE_PACKAGING
    OFF
    CACHE BOOL
          "Do not use packaging from nifti when included as part of another package"
          FORCE
)
set(NIFTI_BUILD_APPLICATIONS
    ON
    CACHE BOOL "Do not build applications provided by NIFTI" FORCE
)
# Testing is not compatible with SILLYDEMO at the moment
set(NIFTI_BUILD_TESTING
    ON
    CACHE BOOL "Do not build nifit testing applications" FORCE
)

add_definitions(-DHAVE_ZLIB)
set(NIFTI_INSTALL_EXPORT_NAME
    SILLYDEMOTargets
    CACHE STRING "Set the nifti package export target name " FORCE
)
set(NIFTI_PACKAGE_PREFIX
    "${SILLYDEMO_TARGETS}"
    CACHE STRING "Set the nifti package prefix" FORCE
)
set(NIFTI_LIBRARY_PROPERTIES
    ${SILLYDEMO_LIBRARY_PROPERTIES}
    CACHE STRING "Set the nifti library properties to match SILLYDEMO" FORCE
)
set(NIFTI_INSTALL_RUNTIME_DIR
    "${SILLYDEMONIFTI_INSTALL_RUNTIME_DIR}"
    CACHE PATH "Set nifti install binaries directory" FORCE
)
set(NIFTI_INSTALL_LIBRARY_DIR
    "${SILLYDEMONIFTI_INSTALL_LIBRARY_DIR}"
    CACHE PATH "Set nifti install dynamic libraries directory" FORCE
)
set(NIFTI_INSTALL_ARCHIVE_DIR
    "${SILLYDEMONIFTI_INSTALL_ARCHIVE_DIR}"
    CACHE PATH "Set nifti install dynamic libraries directory" FORCE
)

set(NIFTI_INSTALL_INCLUDE_DIR "${SILLYDEMO_INSTALL_INCLUDE_DIR}/Utilities")

set(NIFTI_ZLIB_LIBRARIES
    ${SILLYDEMOZLIB_LIBRARIES}
    CACHE STRING "Set the zlib library associated with SILLYDEMO" FORCE
)
set(NIFTI_INSTALL_NO_DEVELOPMENT
    ${SILLYDEMO_INSTALL_NO_DEVELOPMENT}
    CACHE BOOL "nifti development install preference" FORCE
)
set(NIFTI_INSTALL_NO_LIBRARIES
    ${SILLYDEMO_INSTALL_NO_LIBRARIES}
    CACHE BOOL "nifti libraries install preference" FORCE
)

# STEP 2 -- fetch nifti_clib immediately at configuration time, so add_subdirectory can
# be used.
include(FetchContent)
# If new or changed data is needed, add that data to the https://github.com/NIFTI-
# Imaging/nifti-test-data repo make a new release, and then update the URL and hash
# (shasum -a 256 <downloaded tarball>).
FetchContent_Declare(
  fetch_nifti_clib_git_repo
  GIT_REPOSITORY https://github.com/NIFTI-Imaging/nifti_clib.git
  GIT_TAG phase1-update-for-external-inclusion # or v3.0.0, or <HASH>
  SOURCE_DIR
  ${CMAKE_CURRENT_LIST_DIR}/nifti_clib_download # <- not standard,  remove this line to
                                                # download in binary tree
)
FetchContent_GetProperties(fetch_nifti_clib_git_repo)
if(NOT fetch_nifti_clib_git_repo)
  set(FETCHCONTENT_QUIET OFF)
  message(STATUS "Downloading nifti_clib from github ... please wait")
  FetchContent_Populate(fetch_nifti_clib_git_repo)
  message(STATUS "download complete.")
  add_subdirectory(
    ${fetch_nifti_clib_git_repo_SOURCE_DIR} ${fetch_nifti_clib_git_repo_BINARY_DIR}
  )
endif()

# STEP 3 -- The reset of the parent project code that depends on nifti_clib
add_subdirectory(src)

# STEP 4 -- Export the SILLYDEMO.cmake files
# ######################################################################################
# CMake itself and can use some CMake facilities for creating the package files. This
# allows for find_package(SILLYDEMO 0.0.1 NO_MODULE) to work for pulling in SILLYDEMO
# libraries into an external project (including SILLYDEMOniftiio.a etc...)
include(CMakePackageConfigHelpers)

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/SILLYDEMO/SILLYDEMOConfigVersion.cmake"
  VERSION ${SILLYDEMO_VERSION}
  COMPATIBILITY AnyNewerVersion
)

export(
  EXPORT SILLYDEMOTargets
  FILE "${CMAKE_CURRENT_BINARY_DIR}/SILLYDEMO/SILLYDEMOTargets.cmake"
  NAMESPACE ${SILLYDEMO_PACKAGE_PREFIX}NIFTI::
)
configure_file(
  cmake/SILLYDEMOConfig.cmake
  "${CMAKE_CURRENT_BINARY_DIR}/SILLYDEMO/SILLYDEMOConfig.cmake" COPYONLY
)

set(ConfigPackageLocation lib/cmake/SILLYDEMO)
install(
  EXPORT SILLYDEMOTargets
  FILE SILLYDEMOTargets.cmake
  NAMESPACE ${SILLYDEMO_PACKAGE_PREFIX}NIFTI::
  DESTINATION ${ConfigPackageLocation}
)
install(
  FILES cmake/SILLYDEMOConfig.cmake
        "${CMAKE_CURRENT_BINARY_DIR}/SILLYDEMO/SILLYDEMOConfigVersion.cmake"
  DESTINATION ${ConfigPackageLocation}
  COMPONENT Development
)
