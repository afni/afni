cmake_minimum_required(VERSION 2.6)
project(rickr)

set(IMON_OBJS l_mcw_glob.c realtime.c)

add_executable(file_tool file_tool.c fields.c ../ge4_header.c)
target_link_libraries(file_tool libheaders)
add_executable(tokens tokens.c)
#add_executable(Imon Imon.c ${IMON_OBJS} ../thd_iochan.c)
add_executable(serial_helper serial_helper.c)
add_executable(column_cat column_cat.c) 


#target_include_directories(Imon
#	PRIVATE ${NIFTI_INCLUDE_DIRS}
#	PUBLIC ${RICKR_INCLUDE_DIRS}
#)

target_link_libraries(file_tool PUBLIC m NIFTI::nifti2)
#target_link_libraries(Imon m)


install(TARGETS column_cat serial_helper tokens file_tool
       RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
       LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
       ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
       PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)  
