project(ptaylor)

set(PTAYLORLIB tracktools)

file(GLOB PTAYLOR_SRCS "*.c")

find_package(GSL REQUIRED)
find_package(OpenMP REQUIRED)

include_directories(
	${NIFTI_INCLUDE_DIRS}
	${3DEDGE_INCLUDE_DIRS}
	${RICKR_INCLUDE_DIRS}
	${NIML_INCLUDE_DIRS}
	${GTS_INCLUDE_DIRS}
	${SUMA_INCLUDE_DIRS}

	${GSL_INCLUDE_DIRS}

	${PROJECT_SOURCE_DIR}
)

add_library(${PTAYLORLIB} ${PTAYLOR_SRCS})
target_link_libraries(${PTAYLORLIB} mri ${GSL_LIBRARIES})

foreach(target 3dProbTrackID 3dTrackID 3dDTtoNoisyDWI 3dTORTOISEtoHere 3dReHo
		3dRSFC 3dROIMaker map_TrackID 3dMatch 3dNetCorr 1dDW_Grad_o_Mat
		1dDW_Grad_o_Mat++ 3dVecRGB_to_HSL 3ddot_beta 3dLombScargle
		3dAmpToRSFC 3dSpaceTimeCorr)
	add_executable(${target} ${target}.c)
	target_link_libraries(${target} ${PTAYLORLIB})
endforeach(target)

foreach(target 3dEigsToDT 3dDWUncert)
	add_executable(${target} ${target}.c)
	target_compile_options(${target} PUBLIC "${OpenMP_C_FLAGS}")
	target_link_libraries(${target} ${PTAYLORLIB} ${OpenMP_C_FLAGS} )
endforeach(target)
