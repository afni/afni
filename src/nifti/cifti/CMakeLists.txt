find_package(ZLIB REQUIRED)
find_package(EXPAT REQUIRED)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DHAVE_ZLIB")

include_directories(
	${CMAKE_CURRENT_SOURCE_DIR}

	${ZLIB_INCLUDE_DIRS}
	${EXPAT_INCLUDE_DIRS}

	${NIFTI_INCLUDE_DIRS}
)

set(LLIBS ${ZLIB_LIBRARIES})
add_library(OBJS OBJECT afni_xml.c afni_xml_io.c)

add_executable(afni_xml_tool afni_xml_tool.c $<TARGET_OBJECTS:OBJS>)
target_link_libraries(afni_xml_tool niftiio ${EXPAT_LIBRARIES} ${LLIBS})

add_executable(cifti_tool $<TARGET_OBJECTS:OBJS> cifti_tool.c)
target_link_libraries(cifti_tool niftiio znz ${EXPAT_LIBRARIES} ${LLIBS})
