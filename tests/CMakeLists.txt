project(TESTS)
#
# Tests -- building/running is aggregated within a separate directory
#          so they do not interact/pollute main build space
#
# yoh: for some reason WORKING_DIRECTORY is not quite working for me
#      says BAD_COMMAND for the binary to be executed.  That is why
#      this solution with tests/ directory
#

add_test(afni:start ../../tests/xvfb-driver -- afni -com "OPEN_WINDOW axialimage; SAVE_JPEG axialimage test1; QUIT")

# TODO: figure out how to reference it correctly, or just copy to this
# target tests/ build directory
# Allow for 1 error and 2 failures reported -- due to the ones upon exit
add_test(do.examples ../../tests/xvfb-driver -e 0 -f 0 -- @DO.examples -auto_test)
set_tests_properties(do.examples PROPERTIES LABELS GLX)



