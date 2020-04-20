#[=======================================================================[.rst:
FindMotif
---------

Find the native XmTML headers and library.

Imported Targets
^^^^^^^^^^^^^^^^

This module defines the following :prop_tgt:`IMPORTED` targets:

``Motif`` and ``Motif::Motif``



Result Variables
^^^^^^^^^^^^^^^^

This module will set the following variables in your project:


``MOTIF_FOUND``
  true if the Motif headers and libraries were found.

#]=======================================================================]
find_package(PkgConfig QUIET)
pkg_check_modules(PC_MOTIF QUIET Motif)

# message("!!! FOUND DIRS ${PC_MOTIF_INCLUDE_DIRS}")
find_path(
  MOTIF_INCLUDE_DIR
  NAMES Xm/Xm.h
  HINTS ${PC_MOTIF_INCLUDE_DIRS}
  PATHS /usr/openwin/include
)
# message("!!! FOUND DIRS2 ${MOTIF_INCLUDE_DIR}  CMAKE Setting
# BUILD_SHARED_LIBS=${BUILD_SHARED_LIBS} ")

# Look for the library.
find_library(
  MOTIF_LIBRARIES
  NAMES Xm
  HINTS ${PC_MOTIF_LIBRARIES_DIRS}
  PATHS /usr/openwin/lib
)
# message("!!! FOUND LIB ${MOTIF_LIBRARIES}")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
  Motif REQUIRED_VARS MOTIF_LIBRARIES MOTIF_INCLUDE_DIR VERSION_VAR MOTIF_VERSION_STRING
)

# Copy the results to the output variables and target.
if(MOTIF_FOUND)
  if(NOT TARGET Motif)
    # GLOBAL is needed for alias below Changing SHARED to - UNKNOWN would fail to treat
    # it as a library later on - STATIC seems to have no effect even if
    # -DBUILD_SHARED_LIBS=OFF probably because above it should find .a not .so (TODO)
    add_library(Motif SHARED IMPORTED GLOBAL)
    set_target_properties(
      Motif
      PROPERTIES IMPORTED_LINK_INTERFACE_LANGUAGES "C" IMPORTED_LOCATION
                 "${MOTIF_LIBRARIES}" INTERFACE_INCLUDE_DIRECTORIES "${MOTIF_INCLUDE_DIR}"
    )
    get_target_property(TARGET_TYPE Motif TYPE)
    target_link_options(Motif INTERFACE $<$<NOT:$<BOOL:APPLE>>:LINKER:--no-as-needed>)
  endif()
  add_library(Motif::Motif ALIAS Motif)
endif()

mark_as_advanced(MOTIF_INCLUDE_DIR MOTIF_LIBRARIES)
