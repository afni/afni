find_package(PCRE)

set(PKG "glib-${glib_FIND_VERSION}")

find_package(PkgConfig)
pkg_check_modules(PC ${PKG})
set(GLIB_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(GLIB_INCLUDE_DIR_1 glib.h HINTS ${PC_INCLUDE_DIRS} PATH_SUFFIXES "${PKG}")
find_path(GLIB_INCLUDE_DIR_2 glibconfig.h HINTS ${PC_INCLUDE_DIRS} PATHS /usr/lib/x86_64-linux-gnu/ PATH_SUFFIXES "${PKG}/include")
find_library(GLIB_LIBRARY NAMES ${PKG} HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(${PKG}
	REQUIRED_VARS GLIB_LIBRARY GLIB_INCLUDE_DIR_1 GLIB_INCLUDE_DIR_2
	VERSION_VAR glib_FIND_VERSION)

mark_as_advanced(PKG GLIB_INCLUDE_DIR GLIB_LIBRARY)

set(GLIB_LIBRARIES ${PCRE_LIBRARIES} ${GLIB_LIBRARY})
set(GLIB_INCLUDE_DIRS ${PCRE_INCLUDE_DIRS} ${GLIB_INCLUDE_DIR_1} ${GLIB_INCLUDE_DIR_2})
