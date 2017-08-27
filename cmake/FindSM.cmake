find_package(ICE)
find_package(uuid)

find_package(PkgConfig)
pkg_check_modules(PC QUIET sm)
set(SM_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(SM_INCLUDE_DIR X11/SM/SM.h HINTS ${PC_INCLUDE_DIRS})
find_library(SM_LIBRARY NAMES SM HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(SM DEFAULT_MSG SM_LIBRARY SM_INCLUDE_DIR)

mark_as_advanced(SM_INCLUDE_DIR SM_LIBRARY)

set(SM_LIBRARIES ${ICE_LIBRARIES} ${UUID_LIBRARIES} ${SM_LIBRARY})
set(SM_INCLUDE_DIRS ${ICE_INCLUDE_DIRS} ${UUID_INCLUDE_DIRS} ${SM_INCLUDE_DIR})
