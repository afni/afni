find_package(glib 2.0 REQUIRED)

include_directories(
	${GLIB_INCLUDE_DIRS}
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_command(
	OUTPUT predicates_init.h
	COMMAND ./predicates_init ">" predicates_init.h
	MAIN_DEPENDENCY predicates_init
	VERBATIM
)
add_executable(predicates_init predicates_init.c)

file(GLOB GTS_SRCS "*.c")
add_library(gts ${GTS_SRCS} predicates_init.h)
