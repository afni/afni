find_program(M4_BIN m4)

set(M4_BIN "${M4_BIN}" PARENT_SCOPE)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(M4 DEFAULT_MSG M4_BIN)
