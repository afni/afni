find_package(Xmu)
find_package(JPEG)
find_package(png)

find_path(XM_INCLUDE_DIR Xm/Xm.h)
find_library(XM_LIBRARY NAMES Xm)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Xm DEFAULT_MSG XM_LIBRARY XM_INCLUDE_DIR)

mark_as_advanced(XM_INCLUDE_DIR XM_LIBRARY)

set(XM_LIBRARIES ${XMU_LIBRARIES} ${JPEG_LIBRARY} ${PNG_LIBRARY} ${XM_LIBRARY})
set(XM_INCLUDE_DIRS ${XMU_INCLUDE_DIRS} ${JPEG_INCLUDE_DIR} ${PNG_INCLUDE_DIR} ${XM_INCLUDE_DIR})
