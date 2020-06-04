# This file manages the installation of all scripts. In the future the R and python
# scripts might be migrated to packages

set(SCRIPT_PERMISSIONS "OWNER_READ;OWNER_WRITE;OWNER_EXECUTE;WORLD_EXECUTE;WORLD_READ")

# tcsh scripts (R related scripts are ignored if COMP_ADD_RSTATS is not set)
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/
  DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
  COMPONENT tcsh
  FILES_MATCHING
  PATTERN "*" 
  PERMISSIONS ${SCRIPT_PERMISSIONS}
)

if(COMP_ADD_RSTATS)
# R scripts
install(
  DIRECTORY ../R_scripts/
  COMPONENT rstats
  DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
  FILES_MATCHING
  PATTERN "*R"
  PERMISSIONS ${SCRIPT_PERMISSIONS}
)

install(
  DIRECTORY ../scripts_for_r/
  COMPONENT rstats
  DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
  FILES_MATCHING
  PATTERN "*" 
  PERMISSIONS ${SCRIPT_PERMISSIONS}
)
endif()
