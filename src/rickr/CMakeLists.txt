PROJECT(rickr)

IF(NOT AFNI_BUILD_CORELIBS_ONLY)
  SET(PROGRAMS 3dresample file_tool Dimon1 Dimon serial_helper column_cat
               tokens)

  ADD_EXECUTABLE(3dresample 3dresample.c)
  TARGET_LINK_LIBRARIES(3dresample ${AFNI_LIBS})

  ADD_EXECUTABLE(file_tool file_tool.c fields.c)
  TARGET_LINK_LIBRARIES(file_tool ${AFNI_LIBS}) # Just for ge4_header

  SET(IMON_OBS l_mcw_glob.c realtime.c)
# yoh: Has not been built by upstream for a while
#  ADD_EXECUTABLE(Imon Imon.c ${IMON_OBS})
#  TARGET_LINK_LIBRARIES(Imon ${AFNI_LIBS}) # For thd_iochan

  ADD_EXECUTABLE(Dimon
          Dimon.c realtime.c)
  TARGET_LINK_LIBRARIES(Dimon ${AFNI_LIBS}) # For thd_iochan

  ADD_EXECUTABLE(Dimon1
          Dimon1.c realtime1.c)
  TARGET_LINK_LIBRARIES(Dimon1 ${AFNI_LIBS}) # For thd_iochan

  ADD_EXECUTABLE(serial_helper serial_helper.c)
  ADD_EXECUTABLE(column_cat column_cat.c)

  ADD_EXECUTABLE(tokens tokens.c)

ENDIF(NOT AFNI_BUILD_CORELIBS_ONLY)

INSTALL(TARGETS ${PROGRAMS}
  RUNTIME DESTINATION ${AFNI_INSTALL_BIN_DIR} COMPONENT Runtime
  LIBRARY DESTINATION ${AFNI_INSTALL_LIB_DIR} COMPONENT Runtime
  ARCHIVE DESTINATION ${AFNI_INSTALL_LIB_DIR} COMPONENT Development)
