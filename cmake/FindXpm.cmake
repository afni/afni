find_package(X11)

find_package(PkgConfig)
pkg_check_modules(PC QUIET xpm)
set(XPM_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(XPM_INCLUDE_DIR X11/xpm.h HINTS ${PC_INCLUDE_DIRS})
find_library(XPM_LIBRARY NAMES Xpm HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Xpm DEFAULT_MSG XPM_LIBRARY XPM_INCLUDE_DIR)

mark_as_advanced(XPM_INCLUDE_DIR XPM_LIBRARY)

set(XPM_LIBRARIES ${X11_LIBRARIES} ${XPM_LIBRARY})
set(XPM_INCLUDE_DIRS ${X11_INCLUDE_DIRS} ${XPM_INCLUDE_DIR})
