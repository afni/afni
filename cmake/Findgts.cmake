#[=======================================================================[.rst:
Findgts
---------

Find the native gts headers and library.

Imported Targets
^^^^^^^^^^^^^^^^

This module defines the following :prop_tgt:`IMPORTED` targets:

``gts`` and ``gts::gts``



Result Variables
^^^^^^^^^^^^^^^^

This module will set the following variables in your project:


``GTS_FOUND``
  true if the gts headers and libraries were found.

#]=======================================================================]

find_package(PkgConfig QUIET)

pkg_check_modules(PC_GTS QUIET gts)

# message("!!! FOUND DIRS ${PC_GTS_INCLUDE_DIRS}") Look for the header file.
find_path(
  GTS_INCLUDE_DIR
  NAMES gts.h
  HINTS ${PC_GTS_INCLUDE_DIRS}
)
# message("!!! FOUND DIRS2 ${GTS_INCLUDE_DIR}  CMAKE Setting
# BUILD_SHARED_LIBS=${BUILD_SHARED_LIBS} ")

# Look for the library.
find_library(
  GTS_LIBRARY
  NAMES gts
  HINTS ${PC_GTS_LIBRARY_DIRS}
)
# message("!!! FOUND LIB ${GTS_LIBRARY}")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
  gts REQUIRED_VARS GTS_LIBRARY GTS_INCLUDE_DIR VERSION_VAR GTS_VERSION_STRING
)

# Copy the results to the output variables and target.
if(GTS_FOUND)
  set(GTS_LIBRARIES ${GTS_LIBRARY})
  set(GTS_INCLUDE_DIRS ${GTS_INCLUDE_DIR})

  if(NOT TARGET gts)
    # GLOBAL is needed for alias below Changing SHARED to - UNKNOWN would fail to treat
    # it as a library later on - STATIC seems to have no effect even if
    # -DBUILD_SHARED_LIBS=OFF probably because above it should find .a not .so (TODO)
    add_library(gts SHARED IMPORTED GLOBAL)
    set_target_properties(
      gts
      PROPERTIES IMPORTED_LINK_INTERFACE_LANGUAGES "C" IMPORTED_LOCATION
                 "${GTS_LIBRARY}" INTERFACE_INCLUDE_DIRECTORIES "${GTS_INCLUDE_DIRS}"
    )
    get_target_property(TARGET_TYPE gts TYPE)
  endif()
  add_library(gts::gts ALIAS gts)
endif()

mark_as_advanced(GTS_INCLUDE_DIR GTS_LIBRARY)
