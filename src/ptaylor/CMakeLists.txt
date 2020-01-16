project(ptaylor)
find_package(GSL REQUIRED)
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
  PUBLIC AFNI::mri 
  PRIVATE AFNI::3DEdge NIFTI::nifti2 GSL::gsl)


if(BUILD_BINARIES)
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
    add_afni_executable(${target} ${target}.c)
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
    add_afni_executable(${target} ${target}.c)
    target_link_libraries(${target} track_tools GSL::gsl)
  endforeach(target)


  add_afni_executable(3dZipperZapper 3dZipperZapper.c)
  target_link_libraries(3dZipperZapper 
    PRIVATE
      AFNI::mri
      GSL::gsl
      )

  add_afni_executable(3dSliceNDice 3dSliceNDice.c)
  target_link_libraries(3dSliceNDice 
    PRIVATE
      AFNI::mri
      track_tools
      )

  add_afni_executable(3dClusterize 3dClusterize.c)
  target_link_libraries(3dClusterize 
    PRIVATE
      AFNI::mri
      track_tools
      )

  add_afni_executable(3dProbTrackID 3dProbTrackID.c)
  target_link_libraries(3dProbTrackID 
    PRIVATE
      AFNI::mri
      )

  if(OPENMP_FOUND)
    foreach(target 3dEigsToDT 3dDWUncert)
      add_afni_executable(${target} ${target}.c)
      target_link_libraries(${target} track_tools GSL::gsl OpenMP::OpenMP_C)
    endforeach(target)
  endif()
endif()