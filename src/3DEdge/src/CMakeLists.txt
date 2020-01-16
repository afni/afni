# currently not included: test-hyster-pnm.c test-edges-pnm.c test-edges.c
add_afni_library(
  3DEdge
  connexe.c
  convert.c
  extrema.c
  iopnm.c
  recbuffer.c
  recline.c
  zcross.c
)
set_target_properties(
  3DEdge
  PROPERTIES
    PUBLIC_HEADER
    "connexe.h;extrema.h;recbuffer.h;zcross.h;convert.h;iopnm.h;recline.h;typedefs.h"
)
target_include_directories(
  3DEdge
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE $<TARGET_PROPERTY:libheaders,INTERFACE_INCLUDE_DIRECTORIES>
)
target_link_libraries(3DEdge PRIVATE NIFTI::nifti2 NIFTI::nifticdf m)
