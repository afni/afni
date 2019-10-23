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
target_link_libraries(track_tools AFNI::mri AFNI::3DEdge NIFTI::nifti2 GSL::gsl)
add_afni_target_properties(track_tools)
add_library(AFNI::track_tools ALIAS track_tools)

foreach(
  target
  1dDW_Grad_o_Mat
  1dDW_Grad_o_Mat++
  3dAmpToRSFC
  3ddot_beta
  3dDTtoNoisyDWI
  3dLombScargle
  3dMatch
  3dNetCorr
  3dReHo
  3dROIMaker
  3dRSFC
  3dSpaceTimeCorr
  3dTORTOISEtoHere
  3dTrackID
  3dVecRGB_to_HSL
  map_TrackID
)
  add_executable(${target} ${target}.c)
  target_link_libraries(${target} track_tools)
endforeach(target)

find_package(OpenMP REQUIRED)
if(OPENMP_FOUND)
  foreach(target 3dEigsToDT 3dDWUncert)
    add_executable(${target} ${target}.c)
    target_link_libraries(${target} track_tools OpenMP::OpenMP_C)
  endforeach(target)
  set(PTAYLOR_EXECUTABLES ${PTAYLOR_EXECUTABLES_OMP} ${PTAYLOR_EXECUTABLES})
endif()
