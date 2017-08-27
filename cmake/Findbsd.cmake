find_package(PkgConfig)
pkg_check_modules(PC QUIET bsd)
set(BSD_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(BSD_INCLUDE_DIRS bsd/bsd.h HINTS ${PC_INCLUDE_DIRS})
find_library(BSD_LIBRARIES NAMES bsd HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(bsd DEFAULT_MSG BSD_LIBRARIES BSD_INCLUDE_DIRS)
