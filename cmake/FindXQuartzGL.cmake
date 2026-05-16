#[=======================================================================[.rst:
FindXQuartzGL
-------------

FindModule for XQuartz implementation of OpenGL/GLU. Specific to Geant4
to allow use of XQuartz only.

Use of the module on non-macOS systems will result in a fatal error

IMPORTED Targets
^^^^^^^^^^^^^^^^

This module defines the :prop_tgt:`IMPORTED` targets:

``XQuartz::GL``
 Defined to the XQuartz GL library
``XQuartz::GLU``
 Define to the XQuartz GLU library
``XQuartz::GLUT``
 Define to the XQuartz GLUT library

Result Variables
^^^^^^^^^^^^^^^^

This module sets the following variables:

``XQuartzGL_FOUND``
 True, if the XQuartz GL/GLU libraries were located

Cache Variables
^^^^^^^^^^^^^^^

The following cache variables may also be set

``XQuartzGL_INCLUDE_DIR``
 Path to the XQuartz GL include directory
``XQuartzGL_gl_LIBRARY``
 Path to the XQuartz GL library
``XQuartzGL_glu_LIBRARY``
 Path to the XQuartz GLU library
``XQuartzGL_glut_LIBRARY``
 Path to the XQuartz GLUT library

#]=======================================================================]

# Just don't run if we're on macOS
if(NOT APPLE)
  message(FATAL_ERROR "FindXquartzGL is only for use on macOS platforms")
endif()

# - This is for X11 GL drivers, so we DON'T want Framework!
set(CMAKE_FIND_FRAMEWORK_SAVE ${CMAKE_FIND_FRAMEWORK})
set(CMAKE_FIND_FRAMEWORK NEVER)

# ---- Mesa override ----------------------------------------------------------
# On macOS, SUMA still needs XQuartz for X11 and GLUT, but GL/GLU can come from
# Mesa.  AFNI_MESA_ROOT may provide libGL, while AFNI_GLU_ROOT may point at a
# separate mesa-glu prefix.  GLUT intentionally remains XQuartz.
if(AFNI_MESA_ROOT)
  foreach(_gl libGL.dylib libGL.1.dylib)
    if(EXISTS "${AFNI_MESA_ROOT}/lib/${_gl}")
      set(XQuartzGL_gl_LIBRARY "${AFNI_MESA_ROOT}/lib/${_gl}" CACHE FILEPATH "" FORCE)
      break()
    endif()
  endforeach()
  if(NOT AFNI_MESA_INCLUDE_DIR AND EXISTS "${AFNI_MESA_ROOT}/include/GL/gl.h")
    set(AFNI_MESA_INCLUDE_DIR "${AFNI_MESA_ROOT}/include" CACHE PATH "" FORCE)
  endif()
endif()

set(_afni_glu_roots "")
set(_afni_found_glu FALSE)
if(AFNI_GLU_ROOT)
  list(APPEND _afni_glu_roots "${AFNI_GLU_ROOT}")
endif()
if(AFNI_MESA_ROOT)
  list(APPEND _afni_glu_roots "${AFNI_MESA_ROOT}")
endif()

foreach(_afni_glu_root IN LISTS _afni_glu_roots)
  foreach(_glu libGLU.dylib libGLU.1.dylib)
    if(EXISTS "${_afni_glu_root}/lib/${_glu}")
      set(XQuartzGL_glu_LIBRARY "${_afni_glu_root}/lib/${_glu}" CACHE FILEPATH "" FORCE)
      set(_afni_found_glu TRUE)
      break()
    endif()
  endforeach()
  if(_afni_found_glu)
    break()
  endif()
endforeach()

if(AFNI_MESA_ROOT OR AFNI_GLU_ROOT)
  message(STATUS "[FindXQuartzGL] Mesa override active:")
  message(STATUS "  GL  : ${XQuartzGL_gl_LIBRARY}")
  message(STATUS "  GLU : ${XQuartzGL_glu_LIBRARY}")
  message(STATUS "  GLUT: XQuartz")
endif()
# -----------------------------------------------------------------------------

find_path(XQuartzGL_INCLUDE_DIR GL/gl.h
  PATHS /usr/X11R6/include /opt/X11/include
  NO_DEFAULT_PATH
  )

find_library(XQuartzGL_gl_LIBRARY GL
  PATHS /usr/X11R6/lib /opt/X11/lib
  NO_DEFAULT_PATH
  )

find_library(XQuartzGL_glu_LIBRARY GLU
  PATHS /usr/X11R6/lib /opt/X11/lib
  NO_DEFAULT_PATH
  )

find_library(XQuartzGL_glut_LIBRARY GLUT
  PATHS /usr/X11R6/lib /opt/X11/lib
  NO_DEFAULT_PATH
  )

set(CMAKE_FIND_FRAMEWORK ${CMAKE_FIND_FRAMEWORK_SAVE})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(XQuartzGL
  FOUND_VAR
    XQuartzGL_FOUND
  REQUIRED_VARS
    XQuartzGL_INCLUDE_DIR
    XQuartzGL_gl_LIBRARY
    XQuartzGL_glu_LIBRARY
    XQuartzGL_glut_LIBRARY
  )

mark_as_advanced(XQuartzGL_INCLUDE_DIR XQuartzGL_gl_LIBRARY XQuartzGL_glu_LIBRARY XQuartzGL_glut_LIBRARY)

if(XQuartzGL_FOUND)
  if(COMMAND afni_check_macos_dylib_deployment_target)
    afni_check_macos_dylib_deployment_target("${XQuartzGL_gl_LIBRARY}")
    afni_check_macos_dylib_deployment_target("${XQuartzGL_glu_LIBRARY}")
    afni_check_macos_dylib_deployment_target("${XQuartzGL_glut_LIBRARY}")
  endif()

  set(_xquartzgl_include_dirs "${XQuartzGL_INCLUDE_DIR}")
  if(AFNI_MESA_INCLUDE_DIR)
    list(PREPEND _xquartzgl_include_dirs "${AFNI_MESA_INCLUDE_DIR}")
  endif()

  if(NOT TARGET XQuartzGL::GL)
    add_library(XQuartzGL::GL UNKNOWN IMPORTED)
    set_target_properties(XQuartzGL::GL PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES "${_xquartzgl_include_dirs}"
      IMPORTED_LOCATION "${XQuartzGL_gl_LIBRARY}"
    )
  endif()

  if(NOT TARGET XQuartzGL::GLU)
    add_library(XQuartzGL::GLU UNKNOWN IMPORTED)
    set_target_properties(XQuartzGL::GLU PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES "${_xquartzgl_include_dirs}"
      INTERFACE_LINK_LIBRARIES XQuartzGL::GL
      IMPORTED_LOCATION "${XQuartzGL_glu_LIBRARY}"
    )
  endif()
  if(NOT TARGET XQuartzGL::GLUT)
    add_library(XQuartzGL::GLUT UNKNOWN IMPORTED)
    set_target_properties(XQuartzGL::GLUT PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES "${XQuartzGL_INCLUDE_DIR}"
      INTERFACE_LINK_LIBRARIES XQuartzGL::GL
      IMPORTED_LOCATION "${XQuartzGL_glut_LIBRARY}"
    )
  endif()
endif()

unset(_afni_glu_root)
unset(_afni_glu_roots)
unset(_afni_found_glu)
unset(_gl)
unset(_glu)
unset(_xquartzgl_include_dirs)
