find_package(Xt)
find_package(OpenGL)

find_package(PkgConfig)
pkg_check_modules(PC QUIET glw)
set(GLW_DEFINITIONS ${PC_CFLAGS_OTHER})

find_path(GLW_INCLUDE_DIR GL/GLwDrawA.h HINTS ${PC_INCLUDE_DIRS})
find_library(GLW_LIBRARY NAMES GLw HINTS ${PC_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(GLw DEFAULT_MSG GLW_LIBRARY GLW_INCLUDE_DIR)

mark_as_advanced(GLW_INCLUDE_DIR GLW_LIBRARY)

set(GLW_LIBRARIES ${XT_LIBRARIES} ${OPENGL_LIBRARIES} ${GLW_LIBRARY})
set(GLW_INCLUDE_DIRS ${XT_INCLUDE_DIRS} ${OPENGL_INCLUDE_DIR} ${GLW_INCLUDE_DIR})
