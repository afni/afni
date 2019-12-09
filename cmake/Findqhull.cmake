#[=======================================================================[.rst:
Findqhull
---------

Find the native qhull headers and library.

Imported Targets
^^^^^^^^^^^^^^^^

This module defines the following :prop_tgt:`IMPORTED` targets:

``qhull`` and ``qhull::qhull``



Result Variables
^^^^^^^^^^^^^^^^

This module will set the following variables in your project:


``QHULL_FOUND``
  true if the qhull headers and libraries were found.

#]=======================================================================]


find_path(
  QHULL_INCLUDE_DIR
  NAMES qhull.h
)
message("!!! FOUND DIRS2 ${QHULL_INCLUDE_DIR}  CMAKE Setting
BUILD_SHARED_LIBS=${BUILD_SHARED_LIBS} ")

# Look for the library.
find_library(
  QHULL_LIBRARY
  NAMES qhull
)
message("!!! FOUND LIB ${QHULL_LIBRARY}")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
  qhull REQUIRED_VARS QHULL_LIBRARY QHULL_INCLUDE_DIR VERSION_VAR QHULL_VERSION_STRING
)

# Copy the results to the output variables and target.
if(QHULL_FOUND)
  set(QHULL_LIBRARIES ${QHULL_LIBRARY})
  set(QHULL_INCLUDE_DIRS ${QHULL_INCLUDE_DIR})

  if(NOT TARGET qhull)
    # GLOBAL is needed for alias below Changing SHARED to - UNKNOWN would fail to treat
    # it as a library later on - STATIC seems to have no effect even if
    # -DBUILD_SHARED_LIBS=OFF probably because above it should find .a not .so (TODO)
    add_library(qhull SHARED IMPORTED GLOBAL)
    set_target_properties(
      qhull
      PROPERTIES IMPORTED_LINK_INTERFACE_LANGUAGES "C" IMPORTED_LOCATION
                 "${QHULL_LIBRARY}" INTERFACE_INCLUDE_DIRECTORIES "${QHULL_INCLUDE_DIRS}"
    )
    get_target_property(TARGET_TYPE qhull TYPE)
  endif()
  add_library(qhull::qhull ALIAS qhull)
endif()

mark_as_advanced(QHULL_INCLUDE_DIR QHULL_LIBRARY)
