find_package(PkgConfig)
pkg_check_modules(PC QUIET xau)
set(XAU_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(XAU_INCLUDE_DIRS X11/Xauth.h HINTS ${PC_INCLUDE_DIRS})
find_library(XAU_LIBRARIES NAMES Xau HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Xau DEFAULT_MSG XAU_LIBRARIES XAU_INCLUDE_DIRS)
