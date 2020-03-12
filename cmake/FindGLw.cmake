#[=======================================================================[.rst:
FindGLw
---------

Find the native XmTML headers and library.

Imported Targets
^^^^^^^^^^^^^^^^

This module defines the following :prop_tgt:`IMPORTED` targets:

``GLw`` and ``GLw::GLw``



Result Variables
^^^^^^^^^^^^^^^^

This module will set the following variables in your project:


``GLW_FOUND``
  true if the GLw headers and libraries were found.

#]=======================================================================]

find_package(PkgConfig QUIET)

pkg_check_modules(PC_GLW QUIET glw)

find_path(
  GLW_INCLUDE_DIR
  NAMES GL/GLwDrawA.h
  HINTS ${PC_GLW_INCLUDE_DIRS}
)
# message("!!! FOUND DIRS2 ${GLW_INCLUDE_DIR}")

# Look for the library.
find_library(
  GLW_LIBRARY
  NAMES GLw
  HINTS ${PC_GLW_LIBRARY_DIRS}
)
# message("!!! FOUND LIB ${GLW_LIBRARY}")

if(GLW_INCLUDE_DIR AND EXISTS "${GLW_INCLUDE_DIR}/GL/GLwDrawA.h")
  file(STRINGS "${GLW_INCLUDE_DIR}/GL/GLwDrawA.h" _version_str
       REGEX "^#[\t ]*define[\t ]+GLw(VERSION|REVISION|UPDATE_LEVEL)[\t ]+[0-9]+$"
  )
  # message("!!!! Version string:${_version_str}")
  unset(GLW_VERSION_STRING)
  foreach(VPART VERSION REVISION UPDATE_LEVEL MINOR MICRO)

    foreach(VLINE ${_version_str})
      if(VLINE MATCHES "^#[\t ]*define[\t ]+GLw${VPART}[\t ]+([0-9]+)$")
        set(GLW_VERSION_PART "${CMAKE_MATCH_1}")
        if(GLW_VERSION_STRING)
          string(APPEND GLW_VERSION_STRING ".${GLW_VERSION_PART}")
        else()
          set(GLW_VERSION_STRING "${GLW_VERSION_PART}")
        endif()
      endif()
    endforeach()
  endforeach()
  # message("!!!!! Found GLw version ${GLW_VERSION_STRING}")
endif()

include(CheckSymbolDefined)
list(APPEND CMAKE_REQUIRED_LIBRARIES ${GLW_LIBRARY})
list(APPEND CMAKE_REQUIRED_INCLUDES ${GLW_INCLUDE_DIR})
list(APPEND CMAKE_REQUIRED_INCLUDES ${X11_INCLUDE_DIR})
check_symbol_defined(
  glwDrawingAreaWidgetClass
   "X11/IntrinsicP.h;X11/StringDefs.h;GL/GLwDrawA.h" 
   WIDGET_CLASS_IS_GLOBAL)
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
  GLw
  REQUIRED_VARS 
    GLW_LIBRARY GLW_INCLUDE_DIR WIDGET_CLASS_IS_GLOBAL
  VERSION_VAR 
    GLW_VERSION_STRING 
)

# Copy the results to the output variables and target.
if(GLW_FOUND)
  set(GLW_LIBRARIES ${GLW_LIBRARY})
  set(GLW_INCLUDE_DIRS ${GLW_INCLUDE_DIR})

  if(NOT TARGET GLw)
    # GLOBAL is needed for alias below Changing SHARED to - UNKNOWN would fail to treat
    # it as a library later on - STATIC seems to have no effect even if
    # -DBUILD_SHARED_LIBS=OFF probably because above it should find .a not .so (TODO)
    add_library(GLw SHARED IMPORTED GLOBAL)
    set_target_properties(
      GLw
      PROPERTIES IMPORTED_LINK_INTERFACE_LANGUAGES "C" IMPORTED_LOCATION
                 "${GLW_LIBRARY}" INTERFACE_INCLUDE_DIRECTORIES "${GLW_INCLUDE_DIRS}"
    )
    get_target_property(TARGET_TYPE GLw TYPE)
  endif()
  add_library(GLw::GLw ALIAS GLw)
endif()

mark_as_advanced(GLW_INCLUDE_DIR GLW_LIBRARY)
