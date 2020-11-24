# The python code in this directory  can be installed as a typical python
# package; however, it is heavily dependent on the rest of the AFNI programs
# so an alternative is to manage it's installation as part of AFNI's build.
# This cmake module is for this management and makes little sense outside of
# that context.

# At build time for the AFNI project this python package is installed into the
# active python interpreter in development mode (assuming
# STANDARD_PYTHON_INSTALL has not been changed to OFF). This allows the
# execution of tests in the build tree with "ninja pytest".

# Upon installation of AFNI using the cmake build, the python package afnipy
# is installed into the current python interpreter's site-packages. Note that
# the python interpreter used by the commands "pip install ." and "python
# setup.py install" may be different. Some additional deviations in behavior
# exist e.g. pip honours the PYTHONUSERBASE variable while a basic setuptools
# installation does not. pip is used unless the environment variable
# DO_NOT_USE_PIP is set.

if(STANDARD_PYTHON_INSTALL)
  # A standard installation of the python code is required to run the tests as
  # part of the build system (using the pytest target). Using pip (or just
  # setuptools) to install AFNI's python code has the advantage that the code
  # is installed into a specific python interpreter/environment.  This is
  # useful in many situations.

  # Check that setuptools is available for the current python executable:
  execute_process(
      COMMAND "${Python_EXECUTABLE}" "-c" "import setuptools"
      RESULT_VARIABLE resultVar
      OUTPUT_VARIABLE outputVar
      ERROR_VARIABLE errorVar
      )
  if(resultVar)
    message("${errorVar}")
    message(FATAL_ERROR "
      An error occurred when trying to find setuptools for
      the python interpreter:
      ${Python_EXECUTABLE}
      You must install it if you wish to install afnipy
      (or use a different python interpreter.")
  endif()

  add_custom_target(
    dev_install_afnipy
    ALL
    WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/src/python_scripts"
    COMMAND bash ${PROJECT_SOURCE_DIR}/cmake/make_sure_afnipy_is_installed.sh ${Python_EXECUTABLE}
    USES_TERMINAL
  )

else()
  # An alternative to installing using the current python interpreter is
  # to install the python files to abin. This has the advantage that they
  # are always available for use. The downside is that the installation
  # cannot be associated with a particular environment and so if the user
  # changes their python environment AFNI's python code may be available
  # but the pytho:n dependencies may not be.
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
         DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
         PERMISSIONS ${SCRIPT_PERMISSIONS}
         COMPONENT python
 )

install(
  DIRECTORY ../jzosky/lib_RetroTS
  COMPONENT python
  DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
  PATTERN "__pycache__" EXCLUDE
  PERMISSIONS ${SCRIPT_PERMISSIONS}
  PATTERN "*.pyc" EXCLUDE
)


if(NOT STANDARD_PYTHON_INSTALL)
  return()
endif()
##################################################
# Perform some extra steps for testing if afnipy is being installed
##################################################

# Setup for tests. Things like, clear some cache dirs, and check the tests
# from the source tree are symlinked into the build tree, and that the afnipy
# package has indeed been installed
add_custom_target(
  test_setup
  COMMAND
    ${CMAKE_COMMAND}
    -D TESTS_PATH=${PROJECT_SOURCE_DIR}/tests
    -P ${PROJECT_SOURCE_DIR}/cmake/setup_for_tests.cmake
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  VERBATIM
  )

# check that the python interpreter being used has not changed since the build
# configuration
string(REGEX MATCH ".*python" py_path ${Python_EXECUTABLE})
set_if_not_defined(FORCE_CURRENT_PY_INTERP_FOR_TESTS OFF)
if(FORCE_CURRENT_PY_INTERP_FOR_TESTS)
    set(python_for_testing "python")
else()
  add_custom_target(
    check_python_interpreter
    COMMAND
      ${CMAKE_COMMAND} -E
      echo "${py_path}" > python_interpreter_used_for_cmake_configure_step.txt
    COMMAND which python > current_python_interpreter.txt
    COMMAND
      ${CMAKE_COMMAND} -E
      compare_files python_interpreter_used_for_cmake_configure_step.txt current_python_interpreter.txt
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
    VERBATIM
    )
  add_dependencies(test_setup check_python_interpreter)
  set(python_for_testing ${Python_EXECUTABLE})
endif()

# Define PATH modification required for within build tree tests execution
# (used by the cmake file in python_scripts)
set(BUILT_BINARIES ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
set(T_SCRIPTS ${PROJECT_SOURCE_DIR}/src/scripts_install)
set(R_SCRIPTS ${PROJECT_SOURCE_DIR}/src/R_scripts)
set(J_SCRIPTS ${PROJECT_SOURCE_DIR}/src/jzosky)
set(J_SCRIPTS "${J_SCRIPTS}:${J_SCRIPTS}/lib_RetroTS")
set(TESTING_PATHS "${BUILT_BINARIES}:${T_SCRIPTS}:${R_SCRIPTS}:${J_SCRIPTS}")
set(PATH_FOR_TESTING "${TESTING_PATHS}:$ENV{PATH}")
file(WRITE ${PROJECT_BINARY_DIR}/TESTING_PATHS.txt ${PATH_FOR_TESTING})

# Create test target for running tests within the build tree
add_custom_target(
  pytest
  DEPENDS test_setup
  WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/afni_tests"
  COMMAND ${CMAKE_COMMAND} --build ${PROJECT_BINARY_DIR}
  COMMAND echo
  COMMAND echo
  COMMAND echo Running tests with modified path to use build binaries and scripts in the source tree...
  COMMAND echo Prepending PATH with the following: ${TESTING_PATHS}
  COMMAND echo When using 'ninja pytest' one can define "ARGS" to pass additional arguments to pytest...
  COMMAND ${CMAKE_COMMAND} -E env PATH=${PATH_FOR_TESTING}  ${python_for_testing} -B -m pytest $$ARGS
  USES_TERMINAL
)
