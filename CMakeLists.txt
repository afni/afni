## Rudimentary cmake setup for now just to run tests

PROJECT(AFNI)

##
## Define options to customize the build-process
##

OPTION(AFNI_BUILD_TESTS
       "Enable testing and build available tests"
       OFF)

IF(AFNI_BUILD_TESTS)
  ENABLE_TESTING()
  ADD_SUBDIRECTORY(tests)
ENDIF(AFNI_BUILD_TESTS)

