PROJECT(MEICA_LIBS)

# script being programs on their own
SET(SCRIPTS
  alignp_mepi_anat.py optcom.py optcom_surf.py select_model.py
  siemens_me2nii.py t2smap.py tedana.py tedanaX.py)


INSTALL(PROGRAMS ${SCRIPTS}
  DESTINATION ${AFNI_INSTALL_SCRIPT_DIR} COMPONENT Runtime)
