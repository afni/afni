####################################
### Define information necessary for packaging with CPACK (https://gitlab.kitware.com/cmake/community/wikis/home#cpack)
### The last section is concerned with installing the binaries and making distributions.

set(CPACK_SOURCE_GENERATOR "TGZ;ZIP")

include(InstallRequiredSystemLibraries)

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${gifticlib_DESCRIPTION}")
set(CPACK_PACKAGE_VENDOR "GIFTI DFWG")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${gifticlib_SOURCE_DIR}/README.md")

set(CPACK_RESOURCE_FILE_LICENSE "${gifticlib_SOURCE_DIR}/README.md")
# 
set(CPACK_PACKAGE_NAME "${PROJECT_NAME}")
set(CPACK_PACKAGE_VERSION_MAJOR "${gifticlib_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${gifticlib_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${gifticlib_VERSION_PATCH}")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "GIFTI_${gifticlib_VERSION_MAJOR}.${gifticlib_VERSION_MINOR}.${gifticlib_VERSION_PATCH}")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Hans ; Rick Reynolds; John Lee")
if(WIN32 AND NOT UNIX)
  set(CPACK_PACKAGE_INSTALL_REGISTRY_KEY
      "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
  # There is a bug in NSI that does not handle full unix paths properly. Make
  # sure there is at least one set of four (4) backlasshes.
  set(CPACK_PACKAGE_ICON
      "${CMake_SOURCE_DIR}/Utilities/Release\\\\InstallIcon.bmp")
  set(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\MyExecutable.exe")
  set(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_INSTALL_DIRECTORY} GIFTI Project")
  set(CPACK_NSIS_HELP_LINK "https:\\\\\\\\www.nitrc.org")
  set(CPACK_NSIS_URL_INFO_ABOUT "https:\\\\\\\\www.nitrc.org")
  set(CPACK_NSIS_CONTACT "xyz@domain.edu")
  set(CPACK_NSIS_MODIFY_PATH ON)
else()
  set(CPACK_STRIP_FILES OFF)
  set(CPACK_SOURCE_STRIP_FILES OFF)
endif()
set(CPACK_PACKAGE_EXECUTABLES "gifti_tool")

set(CPACK_PACKAGING_INSTALL_PREFIX ".")

set(CPACK_SOURCE_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-dev")
set(CPACK_PACKAGE_DEFAULT_LOCATION "/opt/${CPACK_PACKAGE_NAME}")
set(CPACK_SET_DESTDIR ON)

set(CPACK_SOURCE_IGNORE_FILES
        "/.git/"
        ".gitignore$"
        ".*.swp$"
        ".*~"
        "Makefile\\\\.in$"
)

include(CPack)
