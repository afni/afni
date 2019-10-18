set(IMON_OBJS l_mcw_glob.c realtime.c)

add_executable(file_tool file_tool.c fields.c ../ge4_header.c)
target_link_libraries(file_tool PRIVATE libheaders)
add_executable(tokens tokens.c)
#add_executable(Imon Imon.c ${IMON_OBJS} ../thd_iochan.c)
add_executable(serial_helper serial_helper.c)
add_executable(column_cat column_cat.c) 


#target_include_directories(Imon
#	PRIVATE ${NIFTI_INCLUDE_DIRS}
#	PUBLIC ${RICKR_INCLUDE_DIRS}
#)

target_link_libraries(file_tool PUBLIC NIFTI::nifti2 m)
#target_link_libraries(Imon m)


install(TARGETS column_cat serial_helper tokens file_tool
       RUNTIME DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
       LIBRARY DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
       ARCHIVE DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
       PUBLIC_HEADER DESTINATION ${AFNI_INSTALL_INCLUDE_DIR}
)  
