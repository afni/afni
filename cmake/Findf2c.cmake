#[=======================================================================[.rst:
Findf2c
---------

Find the native XmTML headers and library.

Imported Targets
^^^^^^^^^^^^^^^^

This module defines the following :prop_tgt:`IMPORTED` targets for the f2c library:

``f2c`` and ``f2c::f2c``



Result Variables
^^^^^^^^^^^^^^^^

This module will set the following variables in your project:


``F2C_FOUND``
  true if the f2c headers and libraries were found.
``F2C_EXECUTABLE`
  true if the f2c binary executable is found.

#]=======================================================================]
find_package(PkgConfig QUIET)
pkg_check_modules(PC_F2C QUIET f2c)

# message("!!! FOUND DIRS ${PC_F2C_INCLUDE_DIRS}")
find_path(
  F2C_INCLUDE_DIR
  NAMES f2c.h
  HINTS ${PC_F2C_INCLUDE_DIRS}
)
# message("!!! FOUND DIRS2 ${F2C_INCLUDE_DIR}  CMAKE Setting
# BUILD_SHARED_LIBS=${BUILD_SHARED_LIBS} ")

# Look for the library.
find_library(
  F2C_LIBRARIES
  NAMES f2c
  HINTS ${PC_F2C_LIBRARIES_DIRS}
)
# message("!!! FOUND LIB ${F2C_LIBRARIES}")


find_program(F2C_EXECUTABLE NAMES f2c)
# message("!!! FOUND EXECUTABLE ${F2C_EXECUTABLE}")


include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
  f2c REQUIRED_VARS F2C_LIBRARIES F2C_INCLUDE_DIR VERSION_VAR F2C_VERSION_STRING
)

# Copy the results to the output variables and target.
if(F2C_FOUND)
  if(NOT TARGET f2c)
    # GLOBAL is needed for alias below Changing SHARED to - UNKNOWN would fail to treat
    # it as a library later on - STATIC seems to have no effect even if
    # -DBUILD_SHARED_LIBS=OFF probably because above it should find .a not .so (TODO)
    add_library(f2c SHARED IMPORTED GLOBAL)
    set_target_properties(
      f2c
      PROPERTIES IMPORTED_LINK_INTERFACE_LANGUAGES "C" IMPORTED_LOCATION
                 "${F2C_LIBRARIES}" INTERFACE_INCLUDE_DIRECTORIES "${F2C_INCLUDE_DIR}"
    )
    get_target_property(TARGET_TYPE f2c TYPE)
    target_link_options(f2c INTERFACE "LINKER:--no-as-needed")
  endif()
  add_library(f2c::f2c ALIAS f2c)
endif()

mark_as_advanced(F2C_INCLUDE_DIR F2C_LIBRARIES)
