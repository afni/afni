## Rudimentary cmake setup for now just to run tests

cmake_minimum_required(VERSION 2.6)
if(COMMAND cmake_policy)
  cmake_policy(SET CMP0002 NEW)
  cmake_policy(SET CMP0012 NEW)
  if(POLICY CMP0048)
    cmake_policy(SET CMP0048 OLD)
  endif(POLICY CMP0048)
endif(COMMAND cmake_policy)

# Here upstream should specify the version!
PROJECT(AFNI)

##
## Define options to customize the build-process
##
set_target_properties(${EXE} PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)

OPTION(AFNI_BUILD_LOCAL_NIFTICLIBS
       "Build and use AFNI's local copy of the NIfTI libs"
       ON)
# add option to build shared libraries. The default is OFF to maintain the
# current build behavior
OPTION(BUILD_SHARED_LIBS
       "Build using shared object libraries"
       ON)
OPTION(AFNI_BUILD_LOCAL_3DEDGE3
       "Build and use AFNI's local copy of lib3dedge3"
       ON)
OPTION(AFNI_BUILD_LOCAL_GIFTI
       "Build and use AFNI's local copy of GIFTI"
       ON)
OPTION(AFNI_BUILD_CORELIBS_ONLY
       "Only build core libraries, no SUMA, plugins or programs"
       OFF)
OPTION(AFNI_BUILD_WITH_LESSTIF2
       "Build using lesstif2, otherwise with OpenMotif"
       ON)
OPTION(AFNI_BUILD_TESTS
       "Enable testing and build available tests"
       OFF)

#
# Install destinations
#
# Use different logical targets, but by default put all of them into
# one directory -- like the old makefiles were doing it
SET(AFNI_INSTALL_BIN_DIR "bin" CACHE PATH
    "Directory to install compiled programs into")
SET(AFNI_INSTALL_LIB_DIR "bin" CACHE PATH
    "Directory to install libraries into")
SET(AFNI_INSTALL_INCLUDE_DIR "include" CACHE PATH
    "Directory to install library headers into")
SET(AFNI_INSTALL_PLUGIN_DIR "bin" CACHE PATH
    "Directory to install plugins into")
SET(AFNI_INSTALL_SCRIPT_DIR "bin" CACHE PATH
    "Directory to install interpreted scripts into")
SET(AFNI_INSTALL_PICS_DIR "funstuff" CACHE PATH
    "Directory to install images into")
SET(AFNI_INSTALL_POEMS_DIR "funstuff" CACHE PATH
    "Directory to install poems into")
SET(AFNI_INSTALL_HTML_DIR "html" CACHE PATH
    "Directory to install html documentation into")
SET(AFNI_INSTALL_ATLAS_DIR "atlases" CACHE PATH
    "Directory to install atlases (and related information) into")
SET(AFNI_SHOWOFF "${CMAKE_SYSTEM}" CACHE STRING
    "Identify this build of AFNI (no blanks)")

# main sources
ADD_SUBDIRECTORY(src)

IF(AFNI_BUILD_TESTS)
  ENABLE_TESTING()
  ADD_SUBDIRECTORY(tests)
ENDIF(AFNI_BUILD_TESTS)

