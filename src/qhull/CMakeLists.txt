project(qhull)

file(GLOB QHULL_SRCS "*.c")
foreach(filename qhull unix qdelaun rbox)
	list(REMOVE_ITEM QHULL_SRCS "${PROJECT_SOURCE_DIR}/${filename}.c")
endforeach(filename)

add_library(qhull_srcs OBJECT ${QHULL_SRCS})

foreach(target unix qdelaun rbox)
	add_executable(${target} ${target}.c $<TARGET_OBJECTS:qhull_srcs>)
	target_link_libraries(${target} m)
endforeach(target)
