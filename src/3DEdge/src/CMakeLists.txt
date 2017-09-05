PROJECT(3DEdge)

SET(SOURCES
 connexe.c convert.c extrema.c iopnm.c recbuffer.c recline.c zcross.c
)

ADD_LIBRARY(3DEdge ${SOURCES})
TARGET_LINK_LIBRARIES(3DEdge m)

INSTALL(TARGETS 3DEdge
  RUNTIME DESTINATION ${AFNI_INSTALL_BIN_DIR} COMPONENT Runtime
  LIBRARY DESTINATION ${AFNI_INSTALL_LIB_DIR} COMPONENT Runtime
  ARCHIVE DESTINATION ${AFNI_INSTALL_LIB_DIR} COMPONENT Development)

#
# Tests
#
IF(AFNI_BUILD_TESTS)
  ENABLE_TESTING()
ENDIF(AFNI_BUILD_TESTS)

SET(TESTS
 # Requires data files -- still waiting on upstream to provide
 # test-edges
 test-edges-pnm test-hyster-pnm
)

FOREACH(test ${TESTS})
	ADD_EXECUTABLE(${test} ${test}.c)
    TARGET_LINK_LIBRARIES(${test} 3DEdge m)
	ADD_TEST(${test} ${test})
ENDFOREACH(test)
