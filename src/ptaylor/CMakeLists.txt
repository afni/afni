project(ptaylor)
add_afni_library(
  track_tools
  3dcanvas.c
  basic_boring.c
  checks_and_balances.c
  colorbasic.c
  diffusiony.c
  DoTrackit.c
  Fat_Labels.c
  FuncTrac.c
  LS_funcs.c
  readglob.c
  roiing.c
  rsfc.c
  TrackIO.c
)
target_link_libraries(track_tools
  PUBLIC
    AFNI::mri
    GSL::gsl
  PRIVATE
    $<TARGET_OBJECTS:suma_objs>
    AFNI::3DEdge
    NIFTI::nifti2
    )
target_include_directories(track_tools
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:${AFNI_INSTALL_INCLUDE_DIR}>
  )



if(COMP_COREBINARIES)
  foreach(
    target
    1dDW_Grad_o_Mat
    1dDW_Grad_o_Mat++
    3ddot_beta
    3dLombScargle
    3dReHo
    3dROIMaker
    3dSpaceTimeCorr
    3dTORTOISEtoHere
    3dVecRGB_to_HSL
    3dAmpToRSFC
    3dDTtoNoisyDWI
    3dMatch
    3dNetCorr
    3dRSFC
    3dTrackID
    map_TrackID
    3dZipperZapper
    3dSliceNDice
    3dClusterize
    3dProbTrackID
  )
    add_afni_executable(${target} ${target}.c)
    target_link_libraries(${target} track_tools)
  endforeach(target)

  if(OPENMP_FOUND)
    foreach(target 3dEigsToDT 3dDWUncert)
      add_afni_executable(${target} ${target}.c)
      target_link_libraries(${target} track_tools OpenMP::OpenMP_C)
    endforeach(target)
  endif()
endif()