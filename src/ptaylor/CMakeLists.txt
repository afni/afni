project(ptaylor)
find_package(GSL REQUIRED)
add_library(
  track_tools
  3dcanvas
  basic_boring
  checks_and_balances
  colorbasic
  diffusiony
  DoTrackit
  Fat_Labels
  FuncTrac
  LS_funcs
  readglob
  roiing
  rsfc
  TrackIO
)
target_link_libraries(track_tools 
  PUBLIC AFNI::mri 
  PRIVATE AFNI::3DEdge NIFTI::nifti2 GSL::gsl)
add_afni_target_properties(track_tools)
add_library(AFNI::track_tools ALIAS track_tools)

foreach(
  target
  1dDW_Grad_o_Mat
  1dDW_Grad_o_Mat++
  3dAmpToRSFC
  3ddot_beta
  3dLombScargle
  3dReHo
  3dROIMaker
  3dSpaceTimeCorr
  3dTORTOISEtoHere
  3dVecRGB_to_HSL
)
  add_executable(${target} ${target}.c)
  target_link_libraries(${target} track_tools)
endforeach(target)

foreach(
  target
  3dDTtoNoisyDWI
  3dMatch
  3dNetCorr
  3dRSFC
  3dTrackID
  map_TrackID
)
  add_executable(${target} ${target}.c)
  target_link_libraries(${target} track_tools GSL::gsl)
endforeach(target)


if(OPENMP_FOUND)
  foreach(target 3dEigsToDT 3dDWUncert)
    add_executable(${target} ${target}.c)
    target_link_libraries(${target} track_tools GSL::gsl OpenMP::OpenMP_C)
  endforeach(target)
endif()
