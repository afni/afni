include(FindEXPAT)

set(NIFTI_CIFTILIB_NAME ${NIFTI_PACKAGE_PREFIX}cifti)

add_library(${NIFTI_CIFTILIB_NAME} afni_xml.c afni_xml_io.c)
target_link_libraries(
  ${NIFTI_CIFTILIB_NAME} PUBLIC EXPAT::EXPAT ${NIFTI_PACKAGE_PREFIX}nifti2
)
target_include_directories(
  ${NIFTI_CIFTILIB_NAME} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                                $<INSTALL_INTERFACE:${NIFTI_INSTALL_INCLUDE_DIR}>
)

if(NIFTI_BUILD_APPLICATIONS)
  add_executable(${NIFTI_PACKAGE_PREFIX}afni_xml_tool afni_xml_tool.c)
  target_link_libraries(
    ${NIFTI_PACKAGE_PREFIX}afni_xml_tool PUBLIC ${NIFTI_CIFTILIB_NAME}
  )
  add_executable(${NIFTI_PACKAGE_PREFIX}cifti_tool cifti_tool.c)
  target_link_libraries(${NIFTI_PACKAGE_PREFIX}cifti_tool PUBLIC ${NIFTI_CIFTILIB_NAME})

  set(CIFTI_APPS ${NIFTI_PACKAGE_PREFIX}afni_xml_tool ${NIFTI_PACKAGE_PREFIX}cifti_tool)
endif()
# Set lib version when buildung shared libs.
if(BUILD_SHARED_LIBS)
  set_target_properties(${NIFTI_CIFTILIB_NAME} PROPERTIES ${NIFTI_LIBRARY_PROPERTIES})
endif()

if(NOT NIFTI_INSTALL_NO_LIBRARIES)
  install(
    TARGETS ${NIFTI_CIFTILIB_NAME} ${CIFTI_APPS}
    EXPORT NIFTITargets
    RUNTIME DESTINATION ${NIFTI_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${NIFTI_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${NIFTI_INSTALL_LIBRARY_DIR} COMPONENT Development
    INCLUDES DESTINATION ${NIFTI_INSTALL_INCLUDE_DIR}
  )
endif()

if(NOT NIFTI_INSTALL_NO_DEVELOPMENT)
  file(GLOB __files "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  install(
    FILES ${__files}
    DESTINATION ${NIFTI_INSTALL_INCLUDE_DIR}
    COMPONENT Development
  )
endif()
