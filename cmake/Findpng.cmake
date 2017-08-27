find_package(PkgConfig)
pkg_check_modules(PC QUIET libpng)
set(PNG_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(PNG_INCLUDE_DIRS png.h HINTS ${PC_INCLUDE_DIRS})
find_library(PNG_LIBRARIES NAMES png HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(png DEFAULT_MSG PNG_LIBRARIES PNG_INCLUDE_DIRS)
