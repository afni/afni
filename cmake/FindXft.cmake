find_package(FontConfig)
find_package(Xrender)

find_package(PkgConfig)
pkg_check_modules(PC QUIET xft)
set(XFT_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(XFT_INCLUDE_DIR X11/Xft/Xft.h HINTS ${PC_INCLUDE_DIRS})
find_library(XFT_LIBRARY NAMES Xft HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Xft DEFAULT_MSG XFT_LIBRARY XFT_INCLUDE_DIR)

mark_as_advanced(XFT_INCLUDE_DIR XFT_LIBRARY)

set(XFT_LIBRARIES ${FONTCONFIG_LIBRARIES} ${XRENDER_LIBRARIES})
set(XFT_INCLUDE_DIRS ${FONTCONFIG_INCLUDE_DIRS} ${XRENDER_INCLUDE_DIRS})
