## Rudimentary cmake setup for now just to run tests

project(AFNI)

##
## Define options to customize the build-process
##

option(AFNI_BUILD_TESTS
       "Enable testing and build available tests"
       OFF)

if(AFNI_BUILD_TESTS)
  enable_testing()
  add_subdirectory(tests)
endif(AFNI_BUILD_TESTS)

