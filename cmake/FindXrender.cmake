find_package(X11)

find_package(PkgConfig)
pkg_check_modules(PC QUIET fontconfig)
set(XRENDER_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(XRENDER_INCLUDE_DIR X11/extensions/Xrender.h HINTS ${PC_INCLUDE_DIRS})
find_library(XRENDER_LIBRARY NAMES Xrender HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Xrender DEFAULT_MSG XRENDER_LIBRARY XRENDER_INCLUDE_DIR)

mark_as_advanced(XRENDER_INCLUDE_DIR XRENDER_LIBRARY)

set(XRENDER_LIBRARIES ${X11_LIBRARIES} ${XRENDER_LIBRARY})
set(XRENDER_INCLUDE_DIRS ${X11_INCLUDE_DIRS} ${XRENDER_INCLUDE_DIR})
