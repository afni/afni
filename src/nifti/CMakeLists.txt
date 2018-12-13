cmake_minimum_required(VERSION 2.8.12 FATAL_ERROR)
  
set(NIFTI_MAX_VALIDATED_CMAKE_VERSION "3.13.1")
if("${CMAKE_VERSION}" VERSION_LESS_EQUAL "${NIFTI_MAX_VALIDATED_CMAKE_VERSION}")
  # As of 2018-12-04 NIFTI has been validated to build with cmake version 3.13.1 new policies.
  # Set and use the newest cmake policies that are validated to work
  set(NIFTI_CMAKE_POLICY_VERSION "${CMAKE_VERSION}")
else()
  set(NIFTI_CMAKE_POLICY_VERSION "${NIFTI_MAX_VALIDATED_CMAKE_VERSION}")
endif()
cmake_policy(VERSION ${NIFTI_CMAKE_POLICY_VERSION}) 


set(NIFTI_HOMEPAGE_URL "https://nifti-imaging.github.io")
if("${CMAKE_VERSION}" VERSION_GREATER_EQUAL 3.10.0)
  project(NIFTI
          VERSION 2.1.0
          DESCRIPTION "Niftilib is a set of i/o libraries for reading and writing files in the nifti-1 data format. nifti-1 is a binary file format for storing medical image data, e.g. magnetic resonance image (MRI) and functional MRI (fMRI) brain images."
          LANGUAGES C)
else()
  ## Provide backwards compatible equivalent variables.
  project(NIFTI)
  set(NIFTI_VERSION_MAJOR "2")
  set(NIFTI_VERSION_MINOR "1")
  set(NIFTI_VERSION_PATCH "0")
  #set(NIFTI_VERSION_TWEAK "0")
  set(NIFTI_VERSION "${NIFTI_VERSION_MAJOR}.${NIFTI_VERSION_MINOR}.${NIFTI_VERSION_PATCH}")
  set(NIFTI_PROJECT_DESCRIPTION "Niftilib is a set of i/o libraries for reading and writing files in the nifti-1 data format. nifti-1 is a binary file format for storing medical image data, e.g. magnetic resonance image (MRI) and functional MRI (fMRI) brain images.")
  enable_language(C)
endif()

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_INCLUDE_CURRENT_DIR_IN_INTERFACE ON)

add_definitions(-DHAVE_ZLIB)

# add option to build shared libraries. The default is OFF to maintain the
# current build behavior
option( BUILD_SHARED_LIBS "Toggle building shared libraries." OFF)

# Michael Hanke 2007-04-25 Note:
# 'Utilities' is merged into the include install path to be able to easily
# install into any other path if necessary. For the same reason lib, bin and
# include dir for standalone builds are only set if not yet defined.

###  USE AS PART OF ITK
if("${ITK_VERSION_MAJOR}" GREATER 0.0)
#   message(STATUS "---------------------ITK ZLIB  ${ITK_ZLIB_LIBRARIES}")
  add_definitions(-DITKZLIB)
  set(PACKAGE_PREFIX "ITK")
  set(NIFTI_LIBRARY_PROPERTIES ${ITK_LIBRARY_PROPERTIES})
  set(NIFTI_INSTALL_BIN_DIR "${ITK_INSTALL_BIN_DIR_CM24}")
  set(NIFTI_INSTALL_LIB_DIR "${ITK_INSTALL_LIB_DIR_CM24}")

  if(ITK_USE_FLAT_DIRECTORY_INSTALL)
    set(NIFTI_INSTALL_INCLUDE_DIR "${ITK_INSTALL_INCLUDE_DIR_CM24}")
  else()
    set(NIFTI_INSTALL_INCLUDE_DIR "${ITK_INSTALL_INCLUDE_DIR_CM24}/Utilities")
  endif()

  set(NIFTI_ZLIB_LIBRARIES ${ITK_ZLIB_LIBRARIES})
  set(NIFTI_INSTALL_NO_DEVELOPMENT ${ITK_INSTALL_NO_DEVELOPMENT})
  set(NIFTI_INSTALL_NO_LIBRARIES ${ITK_INSTALL_NO_LIBRARIES})

else()
  set(CPACK_PACKAGE_VERSION_MAJOR "${NIFTI_VERSION_MAJOR}")
  set(CPACK_PACKAGE_VERSION_MINOR "${NIFTI_VERSION_MINOR}")
  set(CPACK_PACKAGE_VERSION_PATCH "${NIFTI_VERSION_PATCH}")
  ###  USE AS STAND ALONE PACKAGE
  find_package(ZLIB)
  #  message(STATUS "---------------------SYSTEM ZLIB ${ZLIB_LIBRARY}")
  set(PACKAGE_PREFIX "")

  # Set default shared library version
  # This library version will be applied to all libraries in the package
  # unless it is not explicitely for a certain lib.
  set(NIFTI_SHAREDLIB_VERSION ${NIFTI_VERSION_MAJOR}.${NIFTI_VERSION_MINOR}.${NIFTI_VERSION_PATCH} )
  string( REGEX MATCH "^[0-9]+" NIFTI_SHAREDLIB_SOVERSION ${NIFTI_SHAREDLIB_VERSION})
  set(NIFTI_LIBRARY_PROPERTIES VERSION ${NIFTI_SHAREDLIB_VERSION} SOVERSION ${NIFTI_SHAREDLIB_SOVERSION})

  set(NIFTI_INSTALL_BIN_DIR "${CMAKE_INSTALL_PREFIX}/bin")
  set(NIFTI_INSTALL_LIB_DIR "${CMAKE_INSTALL_PREFIX}/lib")
  set(NIFTI_INSTALL_INCLUDE_DIR "${CMAKE_INSTALL_PREFIX}/include/nifti")
  set(NIFTI_ZLIB_LIBRARIES ${ZLIB_LIBRARIES})
  set(NIFTI_INSTALL_NO_DEVELOPMENT 0)
  set(NIFTI_INSTALL_NO_LIBRARIES 0)

  #Needs an if clause and more work before testing can take place.
  add_subdirectory(Testing)
  add_subdirectory(nifticdf utils)

  option(USE_FSL_CODE "If OFF, The copyright of this code is questionable for inclusion with nifti." OFF)
  mark_as_advanced(USE_FSL_CODE)

  # the order of add_subdirectory is important! fsliolob has to preceed examples
  # as otherwise FSLIOLIB_SOURCE_DIR is undefined and hence the examples
  # will fail to compile
  if(USE_FSL_CODE)
    add_subdirectory(fsliolib)
    add_subdirectory(examples)
  endif()
  enable_testing()
  include(CTest)

  ####################################
  # CMake itself and can use some CMake facilities for creating the package files. 
  # This allows for find_package(NIFTI 2.1.0 NO_MODULE) to work for pulling in 
  # NIFTI libraries into an external project
  include(CMakePackageConfigHelpers)
  write_basic_package_version_file(
          "${CMAKE_CURRENT_BINARY_DIR}/NIFTI/NIFTIConfigVersion.cmake"
          VERSION ${NIFTI_VERSION}
          COMPATIBILITY AnyNewerVersion
  )

  export(EXPORT NIFTITargets
          FILE "${CMAKE_CURRENT_BINARY_DIR}/NIFTI/NIFTITargets.cmake"
          NAMESPACE NIFTI::
          )
  configure_file(cmake/NIFTIConfig.cmake
          "${CMAKE_CURRENT_BINARY_DIR}/NIFTI/NIFTIConfig.cmake"
          COPYONLY
          )

  set(ConfigPackageLocation lib/cmake/NIFTI)
  install(EXPORT NIFTITargets
          FILE          NIFTITargets.cmake
          NAMESPACE     NIFTI::
          DESTINATION   ${ConfigPackageLocation}
          )
  install(FILES
             cmake/NIFTIConfig.cmake
             "${CMAKE_CURRENT_BINARY_DIR}/NIFTI/NIFTIConfigVersion.cmake"
          DESTINATION   ${ConfigPackageLocation}
          COMPONENT     Development
  )

  ####################################
  ### Define information necessary for packaging with CPACK (http://www.cmake.org/Wiki/CMake:Packaging_With_CPack)
  ### The last section is concerned with installing the binaries and making distributions.
  include(InstallRequiredSystemLibraries)

  set(CPACK_PACKAGE_NAME "${PROJECT_NAME}")

  set(CPACK_SOURCE_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-dev")
  set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${NIFTI_PROJECT_DESCRIPTION}")
  set(CPACK_PACKAGE_DESCRIPTION_FILE "${NIFTI_SOURCE_DIR}/README")
  #set(CPACK_RESOURCE_FILE_LICENSE "${NIFTI_SOURCE_DIR}/Copyright.txt")
  set(CPACK_RESOURCE_FILE_LICENSE "${NIFTI_SOURCE_DIR}/README")

  set(CPACK_PACKAGE_DEFAULT_LOCATION "/opt/${CPACK_PACKAGE_NAME}")
  #set(CPACK_PACKAGE_INSTALL_DIRECTORY "NIFTI ${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}")
  set(CPACK_PACKAGE_INSTALL_DIRECTORY "${NIFTI}")
  set(CPACK_PACKAGING_INSTALL_PREFIX "/")
  set(CPACK_SET_DESTDIR ON)

  set(CPACK_PACKAGE_VENDOR "NIFTI DFWG")

  set(CPACK_SOURCE_GENERATOR "TGZ;TZ")
  #set(CPACK_SOURCE_IGNORE_FILES "/CVS/;/\\.svn/;\\.swp$;\\.#;/#;\\.*~")
  set(CPACK_SOURCE_IGNORE_FILES "")
  set(CPACK_PACKAGE_EXECUTABLES "nifti_tool;NIFTI")

  if(WIN32 AND NOT UNIX)
    set(CPACK_PACKAGE_INSTALL_REGISTRY_KEY
  "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
    # There is a bug in NSI that does not handle full unix paths properly. Make
    # sure there is at least one set of four (4) backlasshes.
    set(CPACK_PACKAGE_ICON
  "${CMake_SOURCE_DIR}/Utilities/Release\\\\InstallIcon.bmp")
    set(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\MyExecutable.exe")
    set(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_INSTALL_DIRECTORY} NIFTI Project")
    set(CPACK_NSIS_HELP_LINK "https:\\\\\\\\www.nitrc.org")
    set(CPACK_NSIS_URL_INFO_ABOUT "https:\\\\\\\\www.nitrc.org")
    set(CPACK_NSIS_CONTACT "xyz@domain.edu")
    set(CPACK_NSIS_MODIFY_PATH ON)
  else()
    set(CPACK_STRIP_FILES OFF)
    set(CPACK_SOURCE_STRIP_FILES OFF)
  endif()
  include(CPack)
endif()

if(NOT DEFINED NIFTI_INSTALL_LIB_DIR)
  message(FATAL_ERROR "Must set NIFTI_INSTALL_LIB_DIR.")
endif()
if(NOT DEFINED NIFTI_INSTALL_BIN_DIR)
  message(FATAL_ERROR "Must set NIFTI_INSTALL_BIN_DIR.")
endif()
if(NOT DEFINED NIFTI_INSTALL_INCLUDE_DIR)
  message(FATAL_ERROR "Must set NIFTI_INSTALL_INCLUDE_DIR.")
endif()

add_subdirectory(znzlib)
add_subdirectory(niftilib)
