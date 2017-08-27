find_package(PkgConfig)
pkg_check_modules(PC QUIET uuid)
set(UUID_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(UUID_INCLUDE_DIRS uuid/uuid.h HINTS ${PC_INCLUDE_DIRS})
find_library(UUID_LIBRARIES NAMES uuid HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(uuid DEFAULT_MSG UUID_LIBRARIES UUID_INCLUDE_DIRS)
