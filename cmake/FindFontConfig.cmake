find_package(Freetype)
find_package(EXPAT)

find_package(PkgConfig)
pkg_check_modules(PC QUIET fontconfig)
set(FC_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(FC_INCLUDE_DIR fontconfig/fontconfig.h HINTS ${PC_INCLUDE_DIRS})
find_library(FC_LIBRARY NAMES fontconfig HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(FontConfig DEFAULT_MSG FC_LIBRARY FC_INCLUDE_DIR)

mark_as_advanced(FC_INCLUDE_DIR FC_LIBRARY)

set(FC_LIBRARIES ${FREETYPE_LIBRARIES} ${EXPAT_LIBRARIES} ${FC_LIBRARY})
set(FC_INCLUDE_DIRS ${FREETYPE_INCLUDE_DIRS} ${EXPAT_INCLUDE_DIRS} ${FC_INCLUDE_DIR})
