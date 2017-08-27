project(ptaylor)
find_package(GSL REQUIRED)
find_package(OpenMP REQUIRED)

set(PTAYLOR_INCLUDE_DIRS
  ${3DEDGE_INCLUDE_DIRS}
  ${GSL_INCLUDE_DIRS}
  ${GTS_INCLUDE_DIRS}
  ${NIFTI_INCLUDE_DIRS}
  $<TARGET_PROPERTY:AFNI::niml,INTERFACE_INCLUDE_DIRECTORIES>
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${RICKR_INCLUDE_DIRS}
  ${SUMA_INCLUDE_DIRS}
)
set(PTAYLORLIB tracktools)

set(PTAYLOR_EXECUTABLES 1dDW_Grad_o_Mat 1dDW_Grad_o_Mat++ 3dAmpToRSFC
    3ddot_beta 3dDTtoNoisyDWI 3dLombScargle 3dMatch 3dNetCorr 
    3dReHo 3dROIMaker 3dRSFC 3dSpaceTimeCorr 3dTORTOISEtoHere 3dTrackID
    3dVecRGB_to_HSL map_TrackID )

set(PTAYLOR_SRCS 3dcanvas basic_boring checks_and_balances colorbasic
    diffusiony DoTrackit Fat_Labels FuncTrac LS_funcs readglob roiing rsfc
    TrackIO)

set(PTAYLOR_EXECUTABLES_OMP 3dEigsToDT 3dDWUncert)
# TODO: The following are not yet built 3dClusterize 3dSliceNDice
# 3dZipperZapper 3dDWUncert2 3dProbTrackID
 


add_library(${PTAYLORLIB} ${PTAYLOR_SRCS})
target_link_libraries(${PTAYLORLIB} mri ${GSL_LIBRARIES})
target_include_directories(${PTAYLORLIB} PRIVATE ${PTAYLOR_INCLUDE_DIRS})

foreach(target ${PTAYLOR_EXECUTABLES})
  add_executable(${target} ${target}.c)
  target_include_directories(${target} PRIVATE ${PTAYLOR_INCLUDE_DIRS})
  target_link_libraries(${target} ${PTAYLORLIB})
endforeach(target)

foreach(target ${PTAYLOR_EXECUTABLES_OMP})
  add_executable(${target} ${target}.c)
  target_compile_options(${target} PUBLIC ${OpenMP_C_FLAGS})
  target_include_directories(${target} PRIVATE ${PTAYLOR_INCLUDE_DIRS})
	target_link_libraries(${target} ${PTAYLORLIB} ${OpenMP_C_FLAGS} )
endforeach(target)

install(TARGETS ${PTAYLOR_EXECUTABLES_OMP} ${PTAYLOR_EXECUTABLES} ${PTAYLORLIB}
       RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
       LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
       ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
       PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
) 

