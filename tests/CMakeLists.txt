cmake_minimum_required(VERSION 3.13)
PROJECT(TESTS)

find_package(xvfb REQUIRED)

# TODO make tests depends on target afni

#
# Tests -- building/running is aggregated within a separate directory
#          so they do not interact/pollute main build space
#
# yoh: for some reason WORKING_DIRECTORY is not quite working for me
#      says BAD_COMMAND for the binary to be executed.  That is why
#      this solution with tests/ directory
#
install(PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/xvfb-driver
  DESTINATION ${CMAKE_INSTALL_BINDIR})

ADD_TEST(afni:start xvfb-driver --
	"afni" -com "OPEN_WINDOW axialimage; SAVE_JPEG axialimage test1; QUIT")

# TODO: figure out how to reference it correctly, or just copy to this
# target tests/ build directory
# Allow for 1 error and 2 failures reported -- due to the ones upon exit
ADD_TEST(do.examples xvfb-driver -e 0 -f 0 -- @DO.examples -auto_test)
SET_TESTS_PROPERTIES(do.examples PROPERTIES LABELS GLX)
