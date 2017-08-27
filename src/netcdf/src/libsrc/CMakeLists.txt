find_package(m4)

file(GLOB LIB_CSRCS "*.c")
list(REMOVE_ITEM LIB_CSRCS "${CMAKE_CURRENT_SOURCE_DIR}/ffio.c")
list(REMOVE_ITEM LIB_CSRCS "${CMAKE_CURRENT_SOURCE_DIR}/ncx_cray.c")
foreach(basename putget)
	add_custom_command(
		OUTPUT ${basename}.c
		COMMAND m4 "${CMAKE_CURRENT_SOURCE_DIR}/${basename}.m4" > ${basename}.c
		MAIN_DEPENDENCY ${basename}.m4
		VERBATIM)
	list(APPEND LIB_CSRCS ${basename}.c)
endforeach(basename)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
add_library(netcdf ${LIB_CSRCS})
