# The order of things in this file matters:
# e.g. https://gitlab.kitware.com/cmake/cmake/issues/16921
set(CPACK_PACKAGE_NAME "${PROJECT_NAME}")
set(CPACK_PACKAGE_VENDOR "AFNI, NIMH")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${PROJECT_SOURCE_DIR}/README.rst")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "A suite of tools for neuroimaging analysis")
# set(CPACK_PACKAGE_INSTALL_DIRECTORY AFNI)
set(CPACK_VERBATIM_VARIABLES ON)
# set(CPACK_DEBIAN_ENABLE_COMPONENT_DEPENDS ON)
# set(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/LICENSE")
set(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/README.rst")

# set(CPACK_PACKAGING_INSTALL_PREFIX "/opt/${PROJECT_NAME}")

set(CPACK_PACKAGE_VERSION_MAJOR "${PROJECT_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${PROJECT_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${PROJECT_VERSION_PATCH}")

set(CPACK_SOURCE_IGNORE_FILES "${PROJECT_BINARY_DIR};/.git/;.gitignore;menu.yml")
# set(CPACK_SOURCE_GENERATOR "TGZ")
# set(CPACK_GENERATOR "TGZ")

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
    # set(CPACK_DEBIAN_COMPRESSION_TYPE "gzip")
    list(APPEND CPACK_GENERATOR "DEB")
    set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Yaroslav Halchenko")
    # set(CPACK_DEBIAN_PACKAGE_SECTION "devel")
    # set(CPACK_DEBIAN_PACKAGE_DEPENDS "uuid-dev")
    set (CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
    set(CPACK_DEB_COMPONENT_INSTALL ON)

    # list(APPEND CPACK_GENERATOR "RPM")
    # set(CPACK_RPM_PACKAGE_RELEASE "1")
    # set(CPACK_RPM_PACKAGE_LICENSE "MIT")
    # set(CPACK_RPM_PACKAGE_REQUIRES "uuid-devel")
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
  set(CPACK_BUNDLE_ICON ${PROJECT_SOURCE_DIR}/cmake/coffee.icns)
endif()

message(STATUS "CPack generators: ${CPACK_GENERATOR}")

# Add some manual add_dependencies
# neurodebian
set(CPACK_DEBIAN_CORELIBS_PACKAGE_DEPENDS "qhull-bin (>= 2015.2-4), libopengl0")
set(CPACK_DEBIAN_EXTERNAL_DEPENDENCIES_PACKAGE_DEPENDS "libnetcdf13 (>= 3.6.1)")
set(CPACK_DEBIAN_RSTATS_PACKAGE_DEPENDS "libnetcdf13 (>= 3.6.1)")
set(CPACK_COMPONENT_RSTATS_DEPENDS "corelibs;corebinaries")
set(CPACK_COMPONENT_PYTHON_DEPENDS "afni-corelibs;afni-corebinaries;afni_tcsh")
set(CPACK_COMPONENT_GUI_DEPENDS "corelibs;corebinaries;tcsh;python")

include(CPack)
if(CMAKE_SYSTEM_NAME MATCHES Linux)
include(CPackComponent)
# Add installation groups which will be packaged together
cpack_add_component_group(minimal
      DISPLAY_NAME minimal
      DESCRIPTION Contains a minimal installation from the AFNI tools suite
      (core libraries, C binaries, and tcsh)
      DEPENDS corelibs corebinaries 
      PARENT_GROUP all
  )

cpack_add_component_group(all
      DISPLAY_NAME Full installation
      DEPENDS minimal suma gui python rstats
      DESCRIPTION Contains a full installation of the AFNI tools suite
      including graphical user interfaces 
  )

  endif()

# Add components
# cpack_add_component(external_libs
#       DESCRIPTION corelibs
#       GROUP minimal
#       REQUIRED
#   )
# cpack_add_component(corelibs
#       DESCRIPTION corelibs
#       GROUP minimal
#       REQUIRED
#   )

# cpack_add_component(corebinaries
#       DESCRIPTION corebinaries
#       DEPENDS corelibs
#       GROUP minimal
#   )
# cpack_add_component(tcsh
#       DISPLAY_NAME tcsh
#       DESCRIPTION  Shell and python tcsh
#       DEPENDS corelibs corebinaries
#       GROUP minimal all
#   )

# cpack_add_component(rstats
#       DISPLAY_NAME rstats
#       DESCRIPTION  Statistical programs dependent on the R language and R packages 
#       DEPENDS corelibs
#       GROUP all
#   )
# cpack_add_component(data
#       DISPLAY_NAME data
#       DESCRIPTION  Atlases etc not in AFNI's source code
#       GROUP all
#   )

# cpack_add_component(gui
#       DISPLAY_NAME gui
#       DESCRIPTION  X dependent programs
#       DEPENDS corelibs corebinaries tcsh data
#       GROUP all
#   )
# cpack_add_component(suma
#       DISPLAY_NAME suma
#       DESCRIPTION  X, and OpenGL dependent programs
#       DEPENDS gui
#       GROUP all
#   )

