find_package(rt)

find_package(PkgConfig)
pkg_check_modules(PC QUIET bsd)
set(BSD_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(BSD_INCLUDE_DIR bsd/bsd.h HINTS ${PC_INCLUDE_DIRS})
find_library(BSD_LIBRARY NAMES bsd HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(bsd DEFAULT_MSG BSD_LIBRARY BSD_INCLUDE_DIR)

mark_as_advanced(BSD_INCLUDE_DIR BSD_LIBRARY)

set(BSD_LIBRARIES ${RT_LIBRARIES} ${BSD_LIBRARY})
set(BSD_INCLUDE_DIRS ${RT_INCLUDE_DIRS} ${BSD_INCLUDE_DIR})
