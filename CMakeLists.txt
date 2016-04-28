## Rudimentary cmake setup for now just to run tests

PROJECT(AFNI)

##
## Define options to customize the build-process
##

OPTION(AFNI_BUILD_TESTS
       "Enable testing and build available tests"
       OFF)

ADD_SUBDIRECTORY(tests)

