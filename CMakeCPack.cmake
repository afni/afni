set(CPACK_PACKAGE_NAME "${PROJECT_NAME}")
set(CPACK_PACKAGE_VENDOR "AFNI, NIMH")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${PROJECT_SOURCE_DIR}/README.rst")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "A suite of tools for neuroimaging analysis")
set(CPACK_PACKAGE_INSTALL_DIRECTORY AFNI)
set(CPACK_VERBATIM_VARIABLES ON)
# set(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/LICENSE")
set(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/README.rst")

set(CPACK_PACKAGING_INSTALL_PREFIX "/opt/${PROJECT_NAME}")

set(CPACK_PACKAGE_VERSION_MAJOR "${PROJECT_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${PROJECT_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${PROJECT_VERSION_PATCH}")

set(CPACK_SOURCE_IGNORE_FILES "${PROJECT_BINARY_DIR};/.git/;.gitignore;menu.yml")
set(CPACK_SOURCE_GENERATOR "TGZ")
set(CPACK_GENERATOR "TGZ")

set(CPACK_SOURCE_IGNORE_FILES
      /\\.git/
      \\.swp
      \\.orig
      /CMakeLists\\.txt\\.user
      src/SUMA/gts
      src/SUMA/GLUT
      src/SUMA/GLw_local
      src/jpeg-6b
      src/volpack
      src/qhulldir
      src/netcdf-3.5.0
      src/netcdf.h
      src/dlcompat
      src/bzip2*
      src/fftw.h
      src/f2cdir
      src/f2c.h
      src/gifsicle*
      src/XmHTML
      src/pkundu/meica.libs/nibabel
      src/crorden/dcm2niix_console
      src/pkundu/meica.libs/mdp
      src/svm
      src/mpeg_encodedir
)

# if(UNIX)
  if(CMAKE_SYSTEM_NAME MATCHES Linux)
    list(APPEND CPACK_GENERATOR "DEB")
    set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Yaroslav Halchenko")
    set(CPACK_DEBIAN_PACKAGE_SECTION "devel")
    # set(CPACK_DEBIAN_PACKAGE_DEPENDS "uuid-dev")
    set (CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
    set(CPACK_DEB_COMPONENT_INSTALL ON)

    list(APPEND CPACK_GENERATOR "RPM")
    set(CPACK_RPM_PACKAGE_RELEASE "1")
    set(CPACK_RPM_PACKAGE_LICENSE "MIT")
    set(CPACK_RPM_PACKAGE_REQUIRES "uuid-devel")
  endif()
# endif()

if(WIN32 OR MINGW)
  list(APPEND CPACK_GENERATOR "NSIS")
  set(CPACK_NSIS_PACKAGE_NAME "message")
  set(CPACK_NSIS_CONTACT "robertdr")
  set(CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL ON)
endif()

if(APPLE)
  list(APPEND CPACK_GENERATOR "Bundle")
  set(CPACK_BUNDLE_NAME "message")
  configure_file(${PROJECT_SOURCE_DIR}/cmake/Info.plist.in Info.plist @ONLY)
  set(CPACK_BUNDLE_PLIST ${CMAKE_CURRENT_BINARY_DIR}/Info.plist)
  set(CPACK_BUNDLE_ICON ${PROJECT_SOURCE_DIR}/cmake/coffee.icns)
endif()

message(STATUS "CPack generators: ${CPACK_GENERATOR}")


# Add installation groups which will be packaged together
cpack_add_component_group(afni_minimal
      DISPLAY_NAME minimal
      DESCRIPTION Contains a minimal installation from the AFNI tools suite
      (core libraries, C binaries, and scripts)
      PARENT_GROUP afni_all
  )

cpack_add_component_group(afni_all
      DISPLAY_NAME Full installation
      DESCRIPTION Contains a full installation of the AFNI tools suite
      including graphical user interfaces 
  )

# Add components
cpack_add_component(afni_external_libs
      DESCRIPTION afni_corelibs
      GROUP afni_minimal
      REQUIRED
      [PLIST plistFileName]
  )
cpack_add_component(afni_corelibs
      DESCRIPTION afni_corelibs
      GROUP afni_minimal
      REQUIRED
      [PLIST plistFileName]
  )

cpack_add_component(afni_binaries
      DESCRIPTION afni_binaries
      DEPENDS afni_corelibs
      GROUP afni_minimal
      [PLIST plistFileName]
  )
cpack_add_component(afni_scripts
      DISPLAY_NAME afni_scripts
      DESCRIPTION  Shell and python scripts
      DEPENDS afni_corelibs afni_binaries
      GROUP afni_minimal
      [PLIST plistFileName]
  )

cpack_add_component(afni_rstats
      DISPLAY_NAME afni_rstats
      DESCRIPTION  Statistical programs dependent on the R language and R packages 
      DEPENDS afni_corelibs
      GROUP afni_all
      [PLIST plistFileName]
  )
cpack_add_component(afni_data
      DISPLAY_NAME afni_data
      DESCRIPTION  Atlases etc not in AFNI's source code
      GROUP afni_all
      [PLIST plistFileName]
  )

cpack_add_component(afni_gui
      DISPLAY_NAME afni_gui
      DESCRIPTION  X dependent programs
      DEPENDS afni_corelibs afni_binaries afni_scripts afni_data
      GROUP afni_all
      [PLIST plistFileName]
  )
cpack_add_component(afni_suma
      DISPLAY_NAME afni_suma
      DESCRIPTION  X, and OpenGL dependent programs
      DEPENDS afni_gui
      GROUP afni_all
      [PLIST plistFileName]
  )



include(CPack)
