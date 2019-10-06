# currently not included: test-hyster-pnm.c test-edges-pnm.c test-edges.c
add_library(3DEdge 3dedge.c convert.c iopnm.c recline.c
     connexe.c extrema.c recbuffer.c
 zcross.c )
set_target_properties(3DEdge PROPERTIES
PUBLIC_HEADER "connexe.h;extrema.h;recbuffer.h;zcross.h;convert.h;iopnm.h;recline.h;typedefs.h")
add_afni_target_properties(3DEdge)
target_include_directories(3DEdge 
  PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE 
    $<TARGET_PROPERTY:libheaders,INTERFACE_INCLUDE_DIRECTORIES>)
 
target_link_libraries(3DEdge NIFTI::nifti2 NIFTI::nifticdf)
install(TARGETS 3DEdge
       EXPORT AFNITargets
       RUNTIME DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
       LIBRARY DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
       ARCHIVE DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
       PUBLIC_HEADER DESTINATION ${AFNI_INSTALL_INCLUDE_DIR}
) 
add_library(AFNI::3DEdge ALIAS 3DEdge)
