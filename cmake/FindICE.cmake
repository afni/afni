find_package(bsd)

find_package(PkgConfig)
pkg_check_modules(PC QUIET ice)
set(ICE_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(ICE_INCLUDE_DIR X11/ICE/ICE.h HINTS ${PC_INCLUDE_DIRS})
find_library(ICE_LIBRARY NAMES ICE HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(ICE DEFAULT_MSG ICE_LIBRARY ICE_INCLUDE_DIR)

mark_as_advanced(ICE_INCLUDE_DIR ICE_LIBRARY)

set(ICE_LIBRARIES ${BSD_LIBRARIES} ${ICE_LIBRARY})
set(ICE_INCLUDE_DIRS ${BSD_INCLUDE_DIRS} ${ICE_INCLUDE_DIR})
