cmake_minimum_required(VERSION 2.6)
project(volpack)

add_library(volpack makeopts.c vp_check.c vp_compAC00G.c vp_compAC11B.c
vp_compAC1NB.c vp_compAC1NS.c vp_compAC1PB.c vp_compAC31B.c vp_compAC32B.c
vp_compAC3NB.c vp_compAC3NS.c vp_compAC3PB.c vp_compAR00G.c vp_compAR11B.c
vp_compAR1NB.c vp_compAR1NS.c vp_compAR1PB.c vp_compAR31B.c vp_compAR32B.c
vp_compAR3NB.c vp_compAR3NS.c vp_compAR3PB.c vp_context.c vp_extract.c
vp_file.c vp_linalg.c vp_octree.c vp_opts.c vp_renderA.c vp_renderB.c
vp_renderC.c vp_renderR.c vp_resample.c vp_rle.c vp_shade.c vp_transpose.c
vp_util.c vp_view.c vp_warp.c vp_warpA101N.c vp_warpA110N.c vp_warpA111N.c
vp_warpA301N.c vp_warpA330N.c vp_warpA330R.c vp_warpA331N.c vp_warpA331R.c)

install(TARGETS volpack
  EXPORT AFNITargets
  RUNTIME DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
  LIBRARY DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
  ARCHIVE DESTINATION ${AFNI_INSTALL_ARCHIVE_DIR}
  PUBLIC_HEADER DESTINATION ${AFNI_INSTALL_INCLUDE_DIR})
add_library(AFNI::volpack ALIAS volpack) 


