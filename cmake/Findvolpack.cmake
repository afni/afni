#[=======================================================================[.rst:
Findvolpack
---------

Find the native volpack headers and library.

Imported Targets
^^^^^^^^^^^^^^^^

This module defines the following :prop_tgt:`IMPORTED` targets:

``volpack`` and ``volpack::volpack``



Result Variables
^^^^^^^^^^^^^^^^

This module will set the following variables in your project:


``VOLPACK_FOUND``
  true if the volpack headers and libraries were found.

#]=======================================================================]


find_path(
  VOLPACK_INCLUDE_DIR
  NAMES volpack.h
)
message("!!! FOUND DIRS2 ${VOLPACK_INCLUDE_DIR}  CMAKE Setting
BUILD_SHARED_LIBS=${BUILD_SHARED_LIBS} ")

# Look for the library.
find_library(
  VOLPACK_LIBRARY
  NAMES volpack
)
message("!!! FOUND LIB ${VOLPACK_LIBRARY}")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
  volpack REQUIRED_VARS VOLPACK_LIBRARY VOLPACK_INCLUDE_DIR VERSION_VAR VOLPACK_VERSION_STRING
)

# Copy the results to the output variables and target.
if(VOLPACK_FOUND)
  set(VOLPACK_LIBRARIES ${VOLPACK_LIBRARY})
  set(VOLPACK_INCLUDE_DIRS ${VOLPACK_INCLUDE_DIR})

  if(NOT TARGET volpack)
    # GLOBAL is needed for alias below Changing SHARED to - UNKNOWN would fail to treat
    # it as a library later on - STATIC seems to have no effect even if
    # -DBUILD_SHARED_LIBS=OFF probably because above it should find .a not .so (TODO)
    add_library(volpack SHARED IMPORTED GLOBAL)
    set_target_properties(
      volpack
      PROPERTIES IMPORTED_LINK_INTERFACE_LANGUAGES "C" IMPORTED_LOCATION
                 "${VOLPACK_LIBRARY}" INTERFACE_INCLUDE_DIRECTORIES "${VOLPACK_INCLUDE_DIRS}"
    )
    get_target_property(TARGET_TYPE volpack TYPE)
  endif()
  add_library(volpack::volpack ALIAS volpack)
endif()

mark_as_advanced(VOLPACK_INCLUDE_DIR VOLPACK_LIBRARY)
