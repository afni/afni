find_package(Xt)

find_package(PkgConfig)
pkg_check_modules(PC QUIET xmu)
set(XMU_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(XMU_INCLUDE_DIR X11/Xmu/Xmu.h HINTS ${PC_INCLUDE_DIRS})
find_library(XMU_LIBRARY NAMES Xmu HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Xmu DEFAULT_MSG XMU_LIBRARY XMU_INCLUDE_DIR)

mark_as_advanced(XMU_INCLUDE_DIR XMU_LIBRARY)

set(XMU_LIBRARIES ${XT_LIBRARIES} ${XMU_LIBRARY})
set(XMU_INCLUDE_DIRS ${XT_INCLUDE_DIRS} ${XMU_INCLUDE_DIR})
