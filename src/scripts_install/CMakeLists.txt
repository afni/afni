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

if(COMP_ADD_PYTHON)
  if(USE_PIP)
    # A pip installation is required to run the tests since they import from
    # afnipy. Using pip to install AFNI's python code has the advantage that
    # the code is installed into a specific python interpretter/environment.
    # This is useful in certain situations. To install the python components
    # of AFNI's installation using pip, execute the following command from the
    # src/python_scripts directory of the AFNI codebase: pip install ."
    add_custom_target(install_afnipy
      ALL
      COMMAND ${Python_EXECUTABLE} -m pip install  ${CMAKE_CURRENT_SOURCE_DIR}/../python_scripts
      )

  else()
    install(
        DIRECTORY ../python_scripts/scripts/
        COMPONENT python
        DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
        PATTERN "__pycache__" EXCLUDE
        PATTERN "*.pyc" EXCLUDE
        PATTERN "__init__.py" EXCLUDE
      )
    install(
          DIRECTORY ../python_scripts/afnipy
          COMPONENT python
          DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
          PATTERN "__pycache__" EXCLUDE
          PATTERN "*.pyc" EXCLUDE
        )
  endif()
# Mimic make-builds distribution of all afni python files
# RetroTS python files
  install(PROGRAMS
     ../pkundu/meica.py
     ../jzosky/RetroTS.py
     ../roopchansinghv/PsychoPy-NeuroFeedback-Demo/demoExpt.py
           DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
           PERMISSIONS ${SCRIPT_PERMISSIONS}
           COMPONENT python
   )
  install(
    DIRECTORY ../roopchansinghv/PsychoPy-NeuroFeedback-Demo/afniRTI
    DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
    PATTERN "__pycache__" EXCLUDE
    PERMISSIONS ${SCRIPT_PERMISSIONS}
   )

  install(
    DIRECTORY ../jzosky/lib_RetroTS
    COMPONENT python
    DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
    PATTERN "__pycache__" EXCLUDE
    PERMISSIONS ${SCRIPT_PERMISSIONS}
    PATTERN "*.pyc" EXCLUDE
  )

endif()
