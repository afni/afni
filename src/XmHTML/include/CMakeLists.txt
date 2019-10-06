add_subdirectory(common)
add_subdirectory(http)
add_subdirectory(XmHTML)
target_include_directories(XmHTML PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} )
