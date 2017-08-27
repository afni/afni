find_package(X11)

find_package(PkgConfig)
pkg_check_modules(PC QUIET xext)
set(XEXT_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(XEXT_INCLUDE_DIR X11/extensions/Xext.h HINTS ${PC_INCLUDE_DIRS})
find_library(XEXT_LIBRARY NAMES Xext HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Xext DEFAULT_MSG XEXT_LIBRARY XEXT_INCLUDE_DIR)

mark_as_advanced(XEXT_INCLUDE_DIR XEXT_LIBRARY)

set(XEXT_LIBRARIES ${X11_LIBRARIES} ${XEXT_LIBRARY})
set(XEXT_INCLUDE_DIRS ${X11_INCLUDE_DIRS} ${XEXT_INCLUDE_DIR})
