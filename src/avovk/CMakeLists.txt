project(avovk)

find_package(GSL REQUIRED)

set(AVOVKLIB segtools)

set(AVOVK_SRCS thd_segtools_fNM.c) # thd_segtools.c thd_segtools_fNM.c

include_directories(
	${NIFTI_INCLUDE_DIRS}
	${3DEDGE_INCLUDE_DIRS}
	${GTS_INCLUDE_DIRS}
	${NIML_INCLUDE_DIRS}
	${RICKR_INCLUDE_DIRS}

	${GSL_LIBRARIES}
)
       
add_library(${AVOVKLIB} ${AVOVK_SRCS} cluster_floatNOMASK.c) # cluster.o cluster_floatNOMASK.o
target_link_libraries(${AVOVKLIB} mri ${GSL_LIBRARIES})

add_executable(3dAclustering 3dAclustering.c)
add_executable(3dfit_onesign 3dfit_onesign.c)
add_executable(3dkmeans 3dkmeans.c)
add_executable(Aclustering Aclustering_floatNOMASK.c cluster_floatNOMASK.c)
add_executable(fit_onesign fit_onesign.c)

target_link_libraries(3dAclustering ${AVOVKLIB})
target_link_libraries(3dfit_onesign ${AVOVKLIB})
target_link_libraries(3dkmeans ${AVOVKLIB})
target_link_libraries(Aclustering mri)
target_link_libraries(fit_onesign mri ${GSL_LIBRARIES})
