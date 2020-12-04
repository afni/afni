
if(COMP_COREBINARIES)
  add_afni_executable(file_tool ../ge4_header.c fields.c file_tool.c)
  target_link_libraries(file_tool 
    PRIVATE libheaders
    PUBLIC NIFTI::nifti2 m
    )

  add_afni_executable(tokens tokens.c)
  add_afni_executable(serial_helper serial_helper.c)
  add_afni_executable(column_cat column_cat.c)
endif()


# set(IMON_OBJS l_mcw_glob.c realtime.c)
# add_afni_executable(Imon Imon.c ${IMON_OBJS} ../thd_iochan.c)
# target_include_directories(Imon PRIVATE ${NIFTI_INCLUDE_DIRS} PUBLIC
# ${RICKR_INCLUDE_DIRS} )
# target_link_libraries(Imon m)


