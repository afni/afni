find_library(F2C_LIBRARIES NAMES f2c)
find_path(F2C_INCLUDE_DIRS f2c.h)
find_program(F2C_BIN f2c)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(f2c DEFAULT_MSG F2C_LIBRARIES F2C_INCLUDE_DIRS F2C_BIN)

set(F2C_BIN "${F2C_BIN}" PARENT_SCOPE)

list(APPEND F2C_LIBRARIES m)
