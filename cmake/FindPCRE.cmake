find_package(PkgConfig)
pkg_check_modules(PC QUIET libpcre)
set(PCRE_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(PCRE_INCLUDE_DIR pcre.h HINTS ${PC_INCLUDE_DIRS})
find_library(PCRE_LIBRARY NAMES pcre HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(libpcre DEFAULT_MSG PCRE_LIBRARY PCRE_INCLUDE_DIR)

mark_as_advanced(PCRE_INCLUDE_DIR PCRE_LIBRARY)

set(PCRE_LIBRARIES ${PCRE_LIBRARY})
set(PCRE_INCLUDE_DIRS ${PCRE_INCLUDE_DIR})
