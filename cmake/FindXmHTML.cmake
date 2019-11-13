# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:
FindXmHTML
---------

Find the native XmTML headers and library.

Imported Targets
^^^^^^^^^^^^^^^^

This module defines the following :prop_tgt:`IMPORTED` targets:

``XmHTML`` and ``XmHMTL::XmHTML``



Result Variables
^^^^^^^^^^^^^^^^

This module will set the following variables in your project:


``XMHTML_FOUND``
  true if the XmHTML headers and libraries were found.

#]=======================================================================]

find_package(PkgConfig QUIET)

pkg_check_modules(PC_XMHTML QUIET XmHTML)

# message("!!! FOUND DIRS ${PC_XMHTML_INCLUDE_DIRS}")
# Look for the header file.
find_path(XMHTML_INCLUDE_DIR NAMES XmHTML/XmHTML.h HINTS ${PC_XMHTML_INCLUDE_DIRS})
# message("!!! FOUND DIRS2 ${XMHTML_INCLUDE_DIR}  CMAKE Setting BUILD_SHARED_LIBS=${BUILD_SHARED_LIBS} ")

# Look for the library.
find_library(XMHTML_LIBRARY NAMES XmHTML HINTS ${PC_XMHTML_LIBRARY_DIRS})
# message("!!! FOUND LIB ${XMHTML_LIBRARY}")

if (XMHTML_INCLUDE_DIR AND EXISTS "${XMHTML_INCLUDE_DIR}/XmHTML/XmHTML.h")
  file(STRINGS "${XMHTML_INCLUDE_DIR}/XmHTML/XmHTML.h" _version_str
       REGEX "^#[\t ]*define[\t ]+XmHTML(VERSION|REVISION|UPDATE_LEVEL)[\t ]+[0-9]+$")

  unset(XMHTML_VERSION_STRING)
  foreach(VPART VERSION REVISION UPDATE_LEVEL MINOR MICRO)
    foreach(VLINE ${_version_str})
      if(VLINE MATCHES "^#[\t ]*define[\t ]+XmHTML${VPART}[\t ]+([0-9]+)$")
        set(XMHTML_VERSION_PART "${CMAKE_MATCH_1}")
        if(XMHTML_VERSION_STRING)
          string(APPEND XMHTML_VERSION_STRING ".${XMHTML_VERSION_PART}")
        else()
          set(XMHTML_VERSION_STRING "${XMHTML_VERSION_PART}")
        endif()
      endif()
    endforeach()
  endforeach()
  # message("!!!!! Found XmHTML version ${XMHTML_VERSION_STRING}")
endif ()

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(
  XMHTML
  REQUIRED_VARS XMHTML_LIBRARY XMHTML_INCLUDE_DIR
  VERSION_VAR XMHTML_VERSION_STRING)

# Copy the results to the output variables and target.
if(XMHTML_FOUND)
  set(XMHTML_LIBRARIES ${XMHTML_LIBRARY})
  set(XMHTML_INCLUDE_DIRS ${XMHTML_INCLUDE_DIR})

  if(NOT TARGET XMHTML)
    # GLOBAL is needed for alias below
    # Changing SHARED to
    #  - UNKNOWN would fail to treat it as a library later on
    #  - STATIC seems to have no effect even if -DBUILD_SHARED_LIBS=OFF
    #    probably because above it should find .a not .so (TODO)
    add_library(XMHTML SHARED IMPORTED GLOBAL)
    set_target_properties(XMHTML PROPERTIES
      IMPORTED_LINK_INTERFACE_LANGUAGES "C"
      IMPORTED_LOCATION "${XMHTML_LIBRARY}"
      INTERFACE_INCLUDE_DIRECTORIES "${XMHTML_INCLUDE_DIRS}")
    get_target_property(TARGET_TYPE XMHTML TYPE)
  endif()
  add_library(XMHTML::XMHTML ALIAS XMHTML)
endif()

mark_as_advanced(XMHTML_INCLUDE_DIR XMHTML_LIBRARY)
