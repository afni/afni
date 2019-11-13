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

# Look for the header file.
find_path(XMHTML_INCLUDE_DIR NAMES expat.h HINTS ${PC_XMHTML_INCLUDE_DIRS})

# Look for the library.
find_library(XMHTML_LIBRARY NAMES expat libexpat HINTS ${PC_XMHTML_LIBRARY_DIRS})

if (XMHTML_INCLUDE_DIR AND EXISTS "${XMHTML_INCLUDE_DIR}/expat.h")
    file(STRINGS "${XMHTML_INCLUDE_DIR}/expat.h" expat_version_str
         REGEX "^#[\t ]*define[\t ]+XML_(MAJOR|MINOR|MICRO)_VERSION[\t ]+[0-9]+$")

    unset(XMHTML_VERSION_STRING)
    foreach(VPART MAJOR MINOR MICRO)
        foreach(VLINE ${expat_version_str})
            if(VLINE MATCHES "^#[\t ]*define[\t ]+XML_${VPART}_VERSION[\t ]+([0-9]+)$")
                set(XMHTML_VERSION_PART "${CMAKE_MATCH_1}")
                if(XMHTML_VERSION_STRING)
                    string(APPEND XMHTML_VERSION_STRING ".${XMHTML_VERSION_PART}")
                else()
                    set(XMHTML_VERSION_STRING "${XMHTML_VERSION_PART}")
                endif()
            endif()
        endforeach()
    endforeach()
endif ()

include(${CMAKE_CURRENT_LIST_DIR}/FindPackageHandleStandardArgs.cmake)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(XMHTML
                                  REQUIRED_VARS XMHTML_LIBRARY XMHTML_INCLUDE_DIR
                                  VERSION_VAR XMHTML_VERSION_STRING)

# Copy the results to the output variables and target.
if(XMHTML_FOUND)
  set(XMHTML_LIBRARIES ${XMHTML_LIBRARY})
  set(XMHTML_INCLUDE_DIRS ${XMHTML_INCLUDE_DIR})

  if(NOT TARGET XMHTML)
    add_library(XMHTML UNKNOWN IMPORTED)
    set_target_properties(XMHTML PROPERTIES
      IMPORTED_LINK_INTERFACE_LANGUAGES "C"
      IMPORTED_LOCATION "${XMHTML_LIBRARY}"
      INTERFACE_INCLUDE_DIRECTORIES "${XMHTML_INCLUDE_DIRS}")
  endif()
  add_library(XMHTML::XMHTML ALIAS XMHTML)
endif()

mark_as_advanced(XMHTML_INCLUDE_DIR XMHTML_LIBRARY)
