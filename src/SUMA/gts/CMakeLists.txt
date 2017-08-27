set(GTS_MAJOR_VERSION 0)
set(GTS_MINOR_VERSION 4)
set(GTS_MICRO_VERSION 7)
set(GTS_INTERFACE_AGE 0)
set(GTS_BINARY_AGE 0)

configure_file(config.h.in
	"${CMAKE_CURRENT_BINARY_DIR}/config.h" @ONLY)
include_directories(
	${CMAKE_CURRENT_BINARY_DIR}
)

add_subdirectory(src)
#add_subdirectory(tools)
#add_subdirectory(examples)
#add_subdirectory(doc)
#add_subdirectory(test)
