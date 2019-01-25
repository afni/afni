# This file manages the installation of all scripts. In the future the R and
# python scripts might be migrated to packages


install(DIRECTORY
 ${CMAKE_CURRENT_SOURCE_DIR}
  ../R_scripts 
  ../python_scripts/afni_python
  DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
)  

install(PROGRAMS ../pkundu/meica.py jzosky/RetroTS.py
  DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
)
