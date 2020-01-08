project(avovk)
find_package(GSL REQUIRED)

add_library(segtools cluster_floatNOMASK.c thd_segtools_fNM.c) # cluster.o
target_include_directories(
  segtools INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                 $<INSTALL_INTERFACE:${AFNI_INSTALL_INCLUDE_DIR}>
)
set_target_properties(segtools PROPERTIES PUBLIC_HEADER "thd_segtools_fNM.h;cluster_floatNOMASK.h")
target_link_libraries(segtools PRIVATE AFNI::mri ${GSL_LIBRARIES})
add_afni_target_properties(segtools)
add_library(AFNI::segtools ALIAS segtools)

if(BUILD_BINARIES)
  # cluster_floatNOMASK.o
  add_executable(3dAclustering 3dAclustering.c)
  add_executable(3dfit_onesign 3dfit_onesign.c)
  add_executable(3dkmeans 3dkmeans.c)
  add_executable(Aclustering Aclustering_floatNOMASK.c cluster_floatNOMASK.c)
  add_executable(fit_onesign fit_onesign.c)

  target_link_libraries(3dAclustering segtools AFNI::mri NIFTI::nifti2 NIFTI::nifticdf)
  target_link_libraries(3dfit_onesign segtools AFNI::mri NIFTI::nifti2 NIFTI::nifticdf)
  target_link_libraries(3dkmeans segtools AFNI::mri NIFTI::nifti2 NIFTI::nifticdf)
  target_link_libraries(Aclustering AFNI::mri)
  target_link_libraries(fit_onesign AFNI::mri ${GSL_LIBRARIES})
endif()