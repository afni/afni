find_package(Xext)
find_package(Xau)

find_package(PkgConfig)
pkg_check_modules(PC QUIET xp)
set(XP_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(XP_INCLUDE_DIR X11/extensions/Print.h HINTS ${PC_INCLUDE_DIRS})
find_library(XP_LIBRARY NAMES Xp HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Xp DEFAULT_MSG XP_LIBRARY XP_INCLUDE_DIR)

mark_as_advanced(XP_INCLUDE_DIR XP_LIBRARY)

set(XP_LIBRARIES ${XEXT_LIBRARIES} ${XAU_LIBRARIES} ${XP_LIBRARY})
set(XP_INCLUDE_DIRS ${XEXT_INCLUDE_DIRS} ${XAU_INCLUDE_DIRS} ${XP_INCLUDE_DIR})
