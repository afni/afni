project(qhull)

add_library(qhull geom.c geom2.c global.c io.c libqhull.c mem.c merge.c poly.c poly2.c qset.c random.c rboxlib.c stat.c user.c usermem.c userprintf.c)
add_afni_target_properties(qhull)
foreach(target unix qdelaun rbox)
  add_executable(${target} ${target}.c)
  target_link_libraries(${target} qhull m)
endforeach(target)

install(TARGETS ${QHULL_BINARIES} qhull
  EXPORT AFNITargets
  RUNTIME DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
  LIBRARY DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
  ARCHIVE DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
  PUBLIC_HEADER DESTINATION ${AFNI_INSTALL_INCLUDE_DIR}
) 
