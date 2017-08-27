add_subdirectory(common)
add_subdirectory(Motif)

add_library(XmHTML ${COMMON_SRCS} ${MOTIF_SRCS})
target_link_libraries(XmHTML ${XPM_LIBRARIES})
