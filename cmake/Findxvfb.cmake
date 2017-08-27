find_program(XVFB_BIN xvfb-run)

set(XVFB_BIN "${XVFB_BIN}" PARENT_SCOPE)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(xvfb-run DEFAULT_MSG XVFB_BIN)
