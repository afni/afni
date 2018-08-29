PROJECT(tracktools)

IF (GSL_LIBRARY AND GTS_FOUND)
  MESSAGE(STATUS "Found GSL and GTS libraries, compiling tracktools")
  SET(SOURCES DoTrackit.c TrackIO.c rsfc.c diffusiony.c FuncTrac.c roiing.c
              Fat_Labels.c colorbasic.c LS_funcs.c
			  checks_and_balances.c readglob.c basic_boring.c)
  ADD_LIBRARY(tracktools ${SOURCES})
  TARGET_LINK_LIBRARIES(tracktools gts mri ${GSL_LIBRARIES})
  SET(PROGRAMS
    3dTrackID 3dProbTrackID 3dDWUncert 3dReHo 3dRSFC map_TrackID 3dROIMaker
    3dMatch 3dNetCorr 1dDW_Grad_o_Mat 1dDW_Grad_o_Mat++ 3dEigsToDT 3dDTtoNoisyDWI
    3dTORTOISEtoHere 3dVecRGB_to_HSL 3ddot_beta 3dLombScargle 3dAmpToRSFC
    3dSpaceTimeCorr 3dZipperZapper
  )

  FOREACH(program ${PROGRAMS})
    ADD_EXECUTABLE(${program} ${program}.c)
    TARGET_LINK_LIBRARIES(${program} ${AFNI_LIBS} ${SUMA_LIBS} tracktools ${GSL_LIBRARIES} ${GTS_LIBRARIES})
  ENDFOREACH(program)

  INSTALL(TARGETS tracktools ${PROGRAMS}
    RUNTIME DESTINATION ${AFNI_INSTALL_BIN_DIR} COMPONENT Runtime
    LIBRARY DESTINATION ${AFNI_INSTALL_LIB_DIR} COMPONENT Runtime
    ARCHIVE DESTINATION ${AFNI_INSTALL_LIB_DIR} COMPONENT Development)
ELSE (GSL_LIBRARY AND GTS_FOUND)
    MESSAGE(STATUS "GSL and/or GTS libraries not found -- not compiling tracktools")
ENDIF (GSL_LIBRARY AND GTS_FOUND)
