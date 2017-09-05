PROJECT(segtools)

if (GSL_LIBRARY)
  MESSAGE(STATUS "Found GSL library, compiling segtools")
  SET(SOURCES thd_segtools_fNM.c cluster_floatNOMASK.c)
  ADD_LIBRARY(segtools ${SOURCES})
  TARGET_LINK_LIBRARIES(segtools mri ${GSL_LIBRARIES})
  SET(PROGRAMS fit_onesign Aclustering 3dfit_onesign 3dkmeans)

  ADD_EXECUTABLE(fit_onesign fit_onesign.c)
  ADD_EXECUTABLE(Aclustering Aclustering.c)
  ADD_EXECUTABLE(3dfit_onesign 3dfit_onesign.c)
  #ADD_EXECUTABLE(3dAclustering_fNM 3dAclustering_floatNM.c)
  ADD_EXECUTABLE(3dkmeans 3dkmeans.c cluster_floatNOMASK.c thd_segtools_fNM.c)

  FOREACH(program ${PROGRAMS})
    TARGET_LINK_LIBRARIES(${program} ${AFNI_LIBS} ${SEGTOOLS_LIBS} ${GSL_LIBRARIES})
  ENDFOREACH(program)

  INSTALL(TARGETS segtools ${PROGRAMS}
    RUNTIME DESTINATION ${AFNI_INSTALL_BIN_DIR} COMPONENT Runtime
    LIBRARY DESTINATION ${AFNI_INSTALL_LIB_DIR} COMPONENT Runtime
    ARCHIVE DESTINATION ${AFNI_INSTALL_LIB_DIR} COMPONENT Development)
ELSE(GSL_LIBRARY)
  MESSAGE("GNU Scientific Library not found - not compiling segtools!")
ENDIF(GSL_LIBRARY)
