file(GLOB 3DEDGE_SRCS "*.c")

add_library(niml ${3DEDGE_SRCS})
target_include_directories(niml PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_link_libraries(niml mri)
