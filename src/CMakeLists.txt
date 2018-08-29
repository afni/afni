##
## Check for and configure for external dependencies
##

# generate version header
configure_file(AFNI_version.h.in AFNI_version.h)
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})

FIND_PACKAGE(X11 REQUIRED)
FIND_PACKAGE(Motif REQUIRED)
IF(AFNI_BUILD_WITH_LESSTIF2)
  ADD_DEFINITIONS(-DUSING_LESSTIF)
ENDIF(AFNI_BUILD_WITH_LESSTIF2)
INCLUDE_DIRECTORIES(${MOTIF_INCLUDE_DIR})
#
# Stolen from KDE
#
################################################################################
# - Try to find the GLIB2 libraries
# Once done this will define
#
#  GLIB2_FOUND - system has glib2
#  GLIB2_INCLUDE_DIR - the glib2 include directory
#  GLIB2_LIBRARIES - glib2 library

# Copyright (c) 2008 Laurent Montel, <montel@kde.org>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

if(GLIB2_INCLUDE_DIR AND GLIB2_LIBRARIES)
    # Already in cache, be silent
    set(GLIB2_FIND_QUIETLY TRUE)
endif(GLIB2_INCLUDE_DIR AND GLIB2_LIBRARIES)

find_package(PkgConfig)
pkg_check_modules(PC_LibGLIB2 QUIET glib-2.0)

find_path(GLIB2_MAIN_INCLUDE_DIR
          NAMES glib.h
          HINTS ${PC_LibGLIB2_INCLUDEDIR}
          PATH_SUFFIXES glib-2.0)

find_library(GLIB2_LIBRARY
             NAMES glib-2.0
             HINTS ${PC_LibGLIB2_LIBDIR}
)

set(GLIB2_LIBRARIES ${GLIB2_LIBRARY})

# search the glibconfig.h include dir under the same root where the library is found
get_filename_component(glib2LibDir "${GLIB2_LIBRARIES}" PATH)

find_path(GLIB2_INTERNAL_INCLUDE_DIR glibconfig.h
          PATH_SUFFIXES glib-2.0/include
          HINTS ${PC_LibGLIB2_INCLUDEDIR} "${glib2LibDir}" ${CMAKE_SYSTEM_LIBRARY_PATH})

set(GLIB2_INCLUDE_DIR "${GLIB2_MAIN_INCLUDE_DIR}")

# not sure if this include dir is optional or required
# for now it is optional
if(GLIB2_INTERNAL_INCLUDE_DIR)
  set(GLIB2_INCLUDE_DIR ${GLIB2_INCLUDE_DIR} "${GLIB2_INTERNAL_INCLUDE_DIR}")
endif(GLIB2_INTERNAL_INCLUDE_DIR)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(GLIB2  DEFAULT_MSG  GLIB2_LIBRARIES GLIB2_MAIN_INCLUDE_DIR)

mark_as_advanced(GLIB2_INCLUDE_DIR GLIB2_LIBRARIES)

################################################################################
INCLUDE_DIRECTORIES(${GLIB2_INCLUDE_DIR})

#
# NIfTI
#
IF(AFNI_BUILD_LOCAL_NIFTICLIBS)
  INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/src/nifti/niftilib
                      ${CMAKE_SOURCE_DIR}/src/nifti/nifticdf
                      ${CMAKE_SOURCE_DIR}/src/nifti/znzlib
                      ${CMAKE_SOURCE_DIR}/src/nifti/nifti2)
  MESSAGE(STATUS "Using local NIfTI libs sources")
ELSE(AFNI_BUILD_LOCAL_NIFTICLIBS)
  FIND_PATH(NIFTI_INCLUDE_DIR nifti1.h
            PATHS /usr/include /usr/local/include
            PATH_SUFFIXES nifti niftilib
            DOC "NIfTI headers location")
  INCLUDE_DIRECTORIES(${NIFTI_INCLUDE_DIR})
ENDIF(AFNI_BUILD_LOCAL_NIFTICLIBS)

#
# F2C
#
# need to force linking against a static f2c when static libs are built
# to prevent unresolved symbols
IF(BUILD_SHARED_LIBS)
  SET(F2C_LIBRARY f2c)
ELSE(BUILD_SHARED_LIBS)
  SET(F2C_LIBRARY libf2c.a)
ENDIF(BUILD_SHARED_LIBS)

#
# Volpack
#
FIND_LIBRARY(VOLPACK_LIBRARY volpack)

#
# netcdf
#
FIND_LIBRARY(NETCDF_LIBRARY netcdf)

#
# xmhtml
#
FIND_LIBRARY(XMHTML_LIBRARY XmHTML)

#
# jpeg
#
FIND_LIBRARY(JPEG_LIBRARY jpeg)

#
# 3dedge3
#
IF(AFNI_BUILD_LOCAL_3DEDGE3)
  SET(THREEDEDGE_LIBS 3DEdge)
  INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/src/3DEdge/src)
  ADD_SUBDIRECTORY(3DEdge/src)
  MESSAGE(STATUS "Using local 3DEdge3 sources")
ELSE(AFNI_BUILD_LOCAL_3DEDGE3)
  SET(THREEDEDGE_LIBS )
ENDIF(AFNI_BUILD_LOCAL_3DEDGE3)

#
# OpenMP
#
FIND_PACKAGE(OpenMP)
IF(OPENMP_FOUND)
  MESSAGE(STATUS "Using OpenMP")
  SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS} -DUSE_OMP")
ENDIF(OPENMP_FOUND)

#
# Zlib
#
FIND_PACKAGE(ZLIB)
IF(ZLIB_FOUND)
  ADD_DEFINITIONS(-DHAVE_ZLIB)
  INCLUDE_DIRECTORIES(${ZLIB_INCLUDE_DIR})
ENDIF(ZLIB_FOUND)

#
# GSL
#
FIND_LIBRARY(GSL_LIBRARY "gsl")
IF(GSL_LIBRARY)
  INCLUDE_DIRECTORIES(${GSL_INC})
  # forcing --no-as-needed  since gsl is usually not linked against gslcblas
  # but some systems have --as-needed by default
  SET(GSL_LIBRARIES  "-Wl,--no-as-needed" gsl gslcblas)
ENDIF(GSL_LIBRARY)

##
## Platform checks and config
##
IF(UNIX)
  # 32bit or 64bit?
  IF(CMAKE_SIZEOF_VOID_P EQUAL 8)
    MESSAGE(STATUS "Building for a 64bit target system")
    ADD_DEFINITIONS(-DREAD_WRITE_64)
  ENDIF(CMAKE_SIZEOF_VOID_P EQUAL 8)

  IF(APPLE)
    # TODO determine best config
  ELSE(APPLE)
    ADD_DEFINITIONS(-DLINUX2 -DUSE_TRACING -DHAVE_XDBE)
  ENDIF(APPLE)
ELSE(UNIX)
  MESSAGE(FATAL_ERROR "Only UNIX-like platforms are supported.")
ENDIF(UNIX)

# Predefined permission set to enforce proper permissions
# during install even if files in the sources have different
# settings
SET(FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)

SET(SEGTOOLS_LIBS segtools)
SET(AFNI_LIBS
   # Link to -lXm before -lXt mri is linked to. Even with explicit
   # order it is not guaranteed to be in effect if --as-needed
   -Wl,--no-as-needed ${MOTIF_LIBRARIES} -Wl,--as-needed
   mri
   ${VOLPACK_LIBRARY} ${NETCDF_LIBRARY}
   ${F2C_LIBRARY} ${XMHTML_LIBRARY} ${JPEG_LIBRARY} m)

# Showoff
ADD_DEFINITIONS(-DSHOWOFF=${AFNI_SHOWOFF})

##
## Sub-projects
##
# always there
INCLUDE(CMakeLists_libs.txt)
ADD_SUBDIRECTORY(coxplot)

# GIFTI upon request
IF(AFNI_BUILD_LOCAL_GIFTI)
  # precharge GIFTI's installation target to sync it with other AFNI libs
  SET(GIFTI_INSTALL_BIN_DIR ${AFNI_INSTALL_BIN_DIR} CACHE PATH
     "Install GIFTI programs into this directory")
  SET(GIFTI_INSTALL_LIB_DIR ${AFNI_INSTALL_LIB_DIR} CACHE PATH
     "Install GIFTI library into this directory")
  SET(GIFTI_INSTALL_INCLUDE_DIR ${AFNI_INSTALL_INCLUDE_DIR}/gifti CACHE PATH
     "Install GIFTI headers into this directory")
  MESSAGE(STATUS "Using local GIFTI sources")
  ADD_SUBDIRECTORY(gifti)

  INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/src/gifti)

  SET(AFNI_HAVE_GIFTI TRUE)
# or if there is a system lib
ELSE(AFNI_BUILD_LOCAL_GIFTI)
  FIND_LIBRARY(GIFTI_LIBRARY giftiio)
  IF(GIFTI_LIBRARY)
    FIND_PATH(GIFTI_INCLUDE_DIR gifti_io.h
              PATHS /usr/include /usr/local/include
              PATH_SUFFIXES gifti giftilib
              DOC "Gifti headers location")
    INCLUDE_DIRECTORIES(${GIFTI_INCLUDE_DIR})
    MESSAGE(STATUS "Found libgiftiio, compiling with GIFTI support using headers under ${GIFTI_INCLUDE_DIR}")
    SET(AFNI_HAVE_GIFTI TRUE)
  ELSE(GIFTI_LIBRARY)
    MESSAGE(STATUS "No libgiftiio found, compiling without GIFTI support")
    SET(AFNI_HAVE_GIFTI FALSE)
  ENDIF(GIFTI_LIBRARY)
ENDIF(AFNI_BUILD_LOCAL_GIFTI)

# tell now everybody that we have GIFTI if we do have it
IF(AFNI_HAVE_GIFTI)
  ADD_DEFINITIONS(-DHAVE_GIFTI)
ENDIF(AFNI_HAVE_GIFTI)

# everything that is not necessary for corelibs
IF(NOT AFNI_BUILD_CORELIBS_ONLY)
  ADD_SUBDIRECTORY(rickr)

  INCLUDE(CMakeLists_apps.txt)
  INCLUDE(CMakeLists_plugins.txt)

  # Install atlas/spaces information file (not worth separate CMakeLists)
  INSTALL(FILES AFNI_atlas_spaces.niml
    DESTINATION ${AFNI_INSTALL_ATLAS_DIR} COMPONENT Runtime)

  ADD_SUBDIRECTORY(html)
  ADD_SUBDIRECTORY(python_scripts)
  ADD_SUBDIRECTORY(R_scripts)
  ADD_SUBDIRECTORY(SUMA)
  ADD_SUBDIRECTORY(avovk)
  ADD_SUBDIRECTORY(ptaylor)
  ADD_SUBDIRECTORY(jzosky)
  ADD_SUBDIRECTORY(pkundu)

  ADD_SUBDIRECTORY(faces)
  ADD_SUBDIRECTORY(poems)

ENDIF(NOT AFNI_BUILD_CORELIBS_ONLY)
