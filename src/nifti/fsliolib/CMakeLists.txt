# Michael Hanke 2004-04-25:
# Restructure the file to match those of niftilib and znzlib
set(NIFTI_FSLIOLIB_NAME ${NIFTI_PACKAGE_PREFIX}fslio)

add_nifti_library(${NIFTI_FSLIOLIB_NAME} fslio.c )
set_target_properties(
  ${NIFTI_FSLIOLIB_NAME}
  PROPERTIES
  PRIVATE_HEADER "${CMAKE_CURRENT_LIST_DIR}/fslio.h;${CMAKE_CURRENT_LIST_DIR}/dbh.h"
  )
target_link_libraries( ${NIFTI_FSLIOLIB_NAME} PUBLIC ${NIFTI_PACKAGE_PREFIX}niftiio)

# Set lib version when buildung shared libs.
if(BUILD_SHARED_LIBS)
  set_target_properties(${NIFTI_FSLIOLIB_NAME} PROPERTIES ${NIFTI_LIBRARY_PROPERTIES})
endif()
