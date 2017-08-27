cmake_minimum_required(VERSION 2.6)
project(rickr)

set(RICKR_INCLUDE_DIRS "${PROJECT_SOURCE_DIR}")
set(RICKR_INCLUDE_DIRS "${RICKR_INCLUDE_DIRS}" PARENT_SCOPE)
set(IMON_OBJS l_mcw_glob.c realtime.c)

include_directories(
	${NIFTI_INCLUDE_DIRS}
	${RICKR_INCLUDE_DIRS}
)

add_executable(file_tool file_tool.c fields.c ../ge4_header.c)
add_executable(tokens tokens.c)
#add_executable(Imon Imon.c ${IMON_OBJS} ../thd_iochan.c)
add_executable(Dimon Dimon.c realtime.c)
add_executable(Dimon1 Dimon1.c realtime1.c)
add_executable(serial_helper serial_helper.c)
add_executable(column_cat column_cat.c) 

target_include_directories(Dimon
	PRIVATE ${NIFTI_INCLUDE_DIRS}
	PUBLIC ${RICKR_INCLUDE_DIRS}
)

target_include_directories(file_tool PRIVATE ${NIFTI_INCLUDE_DIRS})
#target_include_directories(Imon
#	PRIVATE ${NIFTI_INCLUDE_DIRS}
#	PUBLIC ${RICKR_INCLUDE_DIRS}
#)

target_link_libraries(file_tool m)
target_link_libraries(Dimon mri)
target_link_libraries(Dimon1 mri)
#target_link_libraries(Imon m)

add_executable(3dresample 3dresample.c)
target_link_libraries(3dresample mri)

add_library(rickr r_new_resam_dset.c r_idisp.c r_misc.c)
target_link_libraries(rickr niml)
