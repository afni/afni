cmake_minimum_required(VERSION 3.2)

enable_testing()

set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(GNUInstallDirs OPTIONAL RESULT_VARIABLE GNUInstallDirs_FOUND)
if(NOT GNUInstallDirs_FOUND)
	# XXX add windows paths
endif(NOT GNUInstallDirs_FOUND)

add_subdirectory(doc)
add_subdirectory(src)
add_subdirectory(tests)
