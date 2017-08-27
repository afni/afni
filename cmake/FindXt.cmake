find_package(FontConfig)
find_package(Xrender)

find_package(PkgConfig)
pkg_check_modules(PC QUIET xt)
set(XT_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(XT_INCLUDE_DIR X11/Intrinsic.h HINTS ${PC_INCLUDE_DIRS})
find_library(XT_LIBRARY NAMES Xt HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Xt DEFAULT_MSG XT_LIBRARY XT_INCLUDE_DIR)

mark_as_advanced(XT_INCLUDE_DIR XT_LIBRARY)

set(XT_LIBRARIES ${FONTCONFIG_LIBRARIES} ${XRENDER_LIBRARIES} ${XT_LIBRARY})
set(XT_INCLUDE_DIRS ${FONTCONFIG_INCLUDE_DIRS} ${XRENDER_INCLUDE_DIRS} ${XT_INCLUDE_DIR})
