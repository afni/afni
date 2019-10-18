# This file manages the installation of all scripts. In the future the R and
# python scripts might be migrated to packages

set(SCRIPT_PERMISSIONS "OWNER_READ;OWNER_EXECUTE;WORLD_EXECUTE;WORLD_READ")

install(DIRECTORY
 ${CMAKE_CURRENT_SOURCE_DIR}/
  DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
 FILES_MATCHING PATTERN "*"
  PERMISSIONS ${SCRIPT_PERMISSIONS}
)  

install(DIRECTORY
  ../R_scripts/ 
  DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
  FILES_MATCHING PATTERN "*R"
  PERMISSIONS ${SCRIPT_PERMISSIONS}
)  

install(PROGRAMS ../pkundu/meica.py ../jzosky/RetroTS.py
  DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
)

# The following should be replaced by a pip install with console entrypoints:
install(DIRECTORY
  ../python_scripts/afni_python
  DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
  FILES_MATCHING PATTERN "*py"
  PERMISSIONS ${SCRIPT_PERMISSIONS}
  PATTERN "__pycache__" EXCLUDE
)  