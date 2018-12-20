# Michael Hanke 2004-04-25:
# Restructure the file to match those of niftilib and znzlib
set(NIFTI_FSLIOLIB_NAME ${NIFTI_PACKAGE_PREFIX}fslio)

add_library(${NIFTI_FSLIOLIB_NAME} fslio.c )
target_link_libraries( ${NIFTI_FSLIOLIB_NAME} PUBLIC ${NIFTI_PACKAGE_PREFIX}niftiio)
target_include_directories(${NIFTI_FSLIOLIB_NAME} PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<INSTALL_INTERFACE:${NIFTI_INSTALL_INCLUDE_DIR}>
        )

# Set lib version when buildung shared libs.
if(BUILD_SHARED_LIBS)
  set_target_properties(${NIFTI_FSLIOLIB_NAME} PROPERTIES ${NIFTI_LIBRARY_PROPERTIES})
endif()

if(NOT NIFTI_INSTALL_NO_LIBRARIES)
  install(TARGETS ${NIFTI_FSLIOLIB_NAME}
          EXPORT NIFTITargets
          RUNTIME DESTINATION ${NIFTI_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
          LIBRARY DESTINATION ${NIFTI_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
          ARCHIVE DESTINATION ${NIFTI_INSTALL_LIBRARY_DIR} COMPONENT Development
          INCLUDES DESTINATION ${NIFTI_INSTALL_INCLUDE_DIR}
          )
endif()

if(NOT NIFTI_INSTALL_NO_DEVELOPMENT)
  file(GLOB __files "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  install(FILES ${__files}
    DESTINATION ${NIFTI_INSTALL_INCLUDE_DIR}
    COMPONENT Development)
endif()

if(NIFTI_BUILD_APPLICATIONS)
  add_subdirectory(examples)
endif()
