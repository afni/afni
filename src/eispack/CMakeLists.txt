file(GLOB EIS_SRCS "eis_*.c")

set(output_file "${CMAKE_CURRENT_BINARY_DIR}/eis_svd_slow.c")
configure_file(eis_svd.c "${output_file}" COPYONLY)
set_source_files_properties("${output_file}" PROPERTIES COMPILE_DEFINITIONS "FUNCNAME=svd_slow_")
list(APPEND EIS_SRCS "${output_file}")

add_library(eispack ${EIS_SRCS})
target_link_libraries(eispack f2c)
