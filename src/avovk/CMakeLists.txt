project(avovk)
find_package(GSL REQUIRED)

add_library(segtools cluster_floatNOMASK.c thd_segtools_fNM.c) # cluster.o
target_link_libraries(segtools AFNI::mri ${GSL_LIBRARIES})
add_afni_target_properties(segtools)
add_library(AFNI::segtools ALIAS segtools)

# cluster_floatNOMASK.o
add_executable(3dAclustering 3dAclustering.c)
add_executable(3dfit_onesign 3dfit_onesign.c)
add_executable(3dkmeans 3dkmeans.c)
add_executable(Aclustering Aclustering_floatNOMASK.c cluster_floatNOMASK.c)
add_executable(fit_onesign fit_onesign.c)

target_link_libraries(3dAclustering segtools)
target_link_libraries(3dfit_onesign segtools)
target_link_libraries(3dkmeans segtools)
target_link_libraries(Aclustering AFNI::mri)
target_link_libraries(fit_onesign AFNI::mri ${GSL_LIBRARIES})
