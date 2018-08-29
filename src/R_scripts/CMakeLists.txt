PROJECT(R_SCRIPTS)

# script being programs on their own
SET(SCRIPTS
  3dAOV.R 3dGC.R 1dRplot.R 3dICA.R 3dICC.R 3dICC_REML.R 3dLME.R 3dMEMA.R
  3dMVM.R 3dSignatures.R ExamineXmat.R smooth.R 3dRprogDemo.R)

# everything else that is not meant to be a standalone script
SET(MODULES
  1dGC.R 1dSEMr.R 1dSVAR.R 3dKS.R AFNIio.R AFNIplot.R Level2.R Signatures.R)

INSTALL(FILES ${MODULES}
  DESTINATION ${AFNI_INSTALL_SCRIPT_DIR} COMPONENT Runtime
  PERMISSIONS ${FILE_PERMISSIONS})
INSTALL(PROGRAMS ${SCRIPTS}
  DESTINATION ${AFNI_INSTALL_SCRIPT_DIR} COMPONENT Runtime)
