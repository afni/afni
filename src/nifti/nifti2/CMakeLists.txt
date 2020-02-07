set(NIFTI_NIFTILIB2_NAME ${NIFTI_PACKAGE_PREFIX}nifti2)

add_library(${NIFTI_NIFTILIB2_NAME} nifti2_io.c )
target_link_libraries( ${NIFTI_NIFTILIB2_NAME} PUBLIC ${NIFTI_PACKAGE_PREFIX}znz ${NIFTI_SYSTEM_MATH_LIB})
target_include_directories(${NIFTI_NIFTILIB2_NAME} PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<INSTALL_INTERFACE:${NIFTI_INSTALL_INCLUDE_DIR}>
        )

if(NIFTI_BUILD_APPLICATIONS)
  add_executable(${NIFTI_PACKAGE_PREFIX}nifti_tool nifti_tool.c)
  target_link_libraries(${NIFTI_PACKAGE_PREFIX}nifti_tool PUBLIC ${NIFTI_NIFTILIB2_NAME})
  set(NIFTI2_APPS ${NIFTI_PACKAGE_PREFIX}nifti_tool)
endif()

# Set library version when building shared libs.
if(BUILD_SHARED_LIBS)
  set_target_properties(${NIFTI_NIFTILIB2_NAME} PROPERTIES ${NIFTI_LIBRARY_PROPERTIES})
endif()

if(NOT NIFTI_INSTALL_NO_LIBRARIES)
  install(TARGETS ${NIFTI_NIFTILIB2_NAME} ${NIFTI2_APPS}
          EXPORT ${NIFTI_INSTALL_EXPORT_NAME}
          RUNTIME DESTINATION ${NIFTI_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
          LIBRARY DESTINATION ${NIFTI_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
          ARCHIVE DESTINATION ${NIFTI_INSTALL_LIBRARY_DIR} COMPONENT Development
          INCLUDES DESTINATION ${NIFTI_INSTALL_INCLUDE_DIR}
          )
endif()

if(NOT NIFTI_INSTALL_NO_DEVELOPMENT)
  file(GLOB __files "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  install(FILES ${__files}
    DESTINATION ${NIFTI_INSTALL_INCLUDE_DIR}
    COMPONENT Development)
endif()


if(NIFTI_BUILD_TESTING AND NIFTI_BUILD_APPLICATIONS)
  # in order to decouble nifti2 and niftilib, the nifti1.h file
  # is duplicated here.  The verify_nifti1_headers_are_same test
  # will fail if these two files are not identical.
  if( ( IS_DIRECTORY ${PROJECT_SOURCE_DIR}/nifti2 ) AND (IS_DIRECTORY ${PROJECT_SOURCE_DIR}/niftilib))
      message(STATUS "Testing same")
      add_test( NAME ${NIFTI_PACKAGE_PREFIX}verify_nifti1_headers_are_same COMMAND ${CMAKE_COMMAND} -E compare_files ${PROJECT_SOURCE_DIR}/nifti2/nifti1.h ${PROJECT_SOURCE_DIR}/niftilib/nifti1.h)
  endif()
  add_executable(${NIFTI_PACKAGE_PREFIX}clib_02_nifti2 clib_02_nifti2.c)
  target_link_libraries(${NIFTI_PACKAGE_PREFIX}clib_02_nifti2 PUBLIC ${NIFTI_NIFTILIB2_NAME})



  # Do all regression tests
  set(TOOL_SUFFIX "") # The suffxi used for this tool "1" for nifti1, and "" for nifti2
  set(TEST_PREFIX "${NIFTI_PACKAGE_PREFIX}nifti${TOOL_SUFFIX}")
  set(TOOL_NAME ${TEST_PREFIX}_tool)
  add_test( NAME ${TEST_PREFIX}_test_help COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test>)
  add_test( NAME ${TEST_PREFIX}_test_n1   COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test>  -n1 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${CMAKE_CURRENT_BINARY_DIR}/n1 )
  add_test( NAME ${TEST_PREFIX}_test_n2   COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test>  -n2 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${CMAKE_CURRENT_BINARY_DIR}/n2 )
  add_test( NAME ${TEST_PREFIX}_test_a2   COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test>  -a2 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${CMAKE_CURRENT_BINARY_DIR}/a2 )
  add_test( NAME ${TEST_PREFIX}_test_zn1  COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test> -zn1 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${CMAKE_CURRENT_BINARY_DIR}/zn1 )
  add_test( NAME ${TEST_PREFIX}_test_zn2  COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test> -zn2 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${CMAKE_CURRENT_BINARY_DIR}/zn2 )
  add_test( NAME ${TEST_PREFIX}_test_za2  COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti1_test> -za2 ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/ATestReferenceImageForReadingAndWriting.nii.gz ${CMAKE_CURRENT_BINARY_DIR}/za2 )

  add_test( NAME ${TEST_PREFIX}_ver          COMMAND $<TARGET_FILE:${TOOL_NAME}> -nifti_ver)
  add_test( NAME ${TEST_PREFIX}_tool_ver     COMMAND $<TARGET_FILE:${TOOL_NAME}> -ver)
  add_test( NAME ${TEST_PREFIX}_help         COMMAND $<TARGET_FILE:${TOOL_NAME}> -help)
  add_test( NAME ${TEST_PREFIX}_nifti_hist   COMMAND $<TARGET_FILE:${TOOL_NAME}> -nifti_hist)
  add_test( NAME ${TEST_PREFIX}_hist         COMMAND $<TARGET_FILE:${TOOL_NAME}> -hist)

  add_test( NAME ${TEST_PREFIX}_disp_hdr     COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_hdr  -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )
  add_test( NAME ${TEST_PREFIX}_disp_nim     COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_nim  -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )
  add_test( NAME ${TEST_PREFIX}_disp_ext     COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_ext  -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )
  add_test( NAME ${TEST_PREFIX}_header_check COMMAND $<TARGET_FILE:${TOOL_NAME}> -check_hdr -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )
  add_test( NAME ${TEST_PREFIX}_nim_check    COMMAND $<TARGET_FILE:${TOOL_NAME}> -check_nim -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )

  #The help screens always return 1; add_test( NAME ${NIFTI_PACKAGE_PREFIX}nifti1_tool1_help COMMAND $<TARGET_FILE:${TOOL_NAME}>)
  add_test( NAME ${TEST_PREFIX}_tool_n1  COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/n1 )
  add_test( NAME ${TEST_PREFIX}_tool_n2  COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/n2 )
  add_test( NAME ${TEST_PREFIX}_tool_a2  COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/a2 )
  add_test( NAME ${TEST_PREFIX}_tool_zn1 COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1 )
  add_test( NAME ${TEST_PREFIX}_tool_zn2 COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/zn2 )
  add_test( NAME ${TEST_PREFIX}_tool_za2 COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/za2 )

  add_test( NAME ${TEST_PREFIX}_tool_diff_hdr         COMMAND $<TARGET_FILE:${TOOL_NAME}> -diff_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1 ${CMAKE_CURRENT_BINARY_DIR}/zn1 )
  add_test( NAME ${TEST_PREFIX}_tool_diff_nims        COMMAND $<TARGET_FILE:${TOOL_NAME}> -diff_nim -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1 ${CMAKE_CURRENT_BINARY_DIR}/zn1 )

  add_test( NAME ${TEST_PREFIX}_tool_copy_brick_list COMMAND $<TARGET_FILE:${TOOL_NAME}> -cbl -prefix cbl_zn1.nii -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1.nii.gz[0,1])
  add_test( NAME ${TEST_PREFIX}_tool_disp_ci         COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_ci 2 2 2 -1 0 0 0  -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1.nii.gz )

  add_test( NAME ${TEST_PREFIX}_tool_disp_ts         COMMAND $<TARGET_FILE:${TOOL_NAME}> -disp_ts 2 2 2 -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1.nii.gz )
  add_test( NAME ${TEST_PREFIX}_tool_strip_extras    COMMAND $<TARGET_FILE:${TOOL_NAME}> -strip_extras -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1.nii.gz )

  # This test needs a file that has extensions to remove
  #add_test( NAME ${TEST_PREFIX}_tool_rm_ext /bin/sh ${NIFTI_SOURCE_DIR}/niftilib/rmthenrun rm_ext.nii COMMAND $<TARGET_FILE:${TOOL_NAME}> -rm_ext ALL -prefix rm_ext.nii -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1.nii.gz )
  add_test( NAME ${TEST_PREFIX}_tool_check_hdr COMMAND $<TARGET_FILE:${TOOL_NAME}> -check_hdr -infile ${CMAKE_CURRENT_BINARY_DIR}/za2 )
  add_test( NAME ${TEST_PREFIX}_tool_check_nim COMMAND $<TARGET_FILE:${TOOL_NAME}> -check_nim -infile ${CMAKE_CURRENT_BINARY_DIR}/za2 )

  #add_test( NAME ${TEST_PREFIX}_tool_copy_collapsed_image /bin/sh ${NIFTI_SOURCE_DIR}/niftilib/rmthenrun cci_zn1.nii COMMAND $<TARGET_FILE:${TOOL_NAME}> -cci 2 2 2 -1 -1 -1 -1 -prefix cci_zn1.nii -infile ${CMAKE_CURRENT_BINARY_DIR}/zn1.nii.gz )

  add_test( NAME cleanup_n1  COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_BINARY_DIR}/n1.nii.gz  ${CMAKE_CURRENT_BINARY_DIR}/n1.nii )
  add_test( NAME cleanup_n2  COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_BINARY_DIR}/n2.nii     ${CMAKE_CURRENT_BINARY_DIR}/n2.hdr ${CMAKE_CURRENT_BINARY_DIR}/n2.img)
  add_test( NAME cleanup_a2  COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_BINARY_DIR}/a2.img     ${CMAKE_CURRENT_BINARY_DIR}/a2.hdr )
  add_test( NAME cleanup_zn1 COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_BINARY_DIR}/zn1.nii.gz ${CMAKE_CURRENT_BINARY_DIR}/cbl_zn1.nii )
  add_test( NAME cleanup_zn2 COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_BINARY_DIR}/zn2.img.gz ${CMAKE_CURRENT_BINARY_DIR}/zn2.hdr.gz )
  add_test( NAME cleanup_za2 COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_CURRENT_BINARY_DIR}/za2.img.gz ${CMAKE_CURRENT_BINARY_DIR}/za2.hdr.gz )

  # https://crascit.com/2016/10/18/test-fixtures-with-cmake-ctest/ as of cmake 3.7 fixtures are supported
  foreach( testsuffix "n1" "n2" "a2" "zn1" "zn2" "za2")
    #message(STATUS "Setting up ${testsuffix}")
    set_tests_properties( ${TEST_PREFIX}_tool_${testsuffix}
            PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_${testsuffix} )
    set_tests_properties( ${TEST_PREFIX}_test_${testsuffix}
            PROPERTIES FIXTURES_SETUP NiftiTestGeneratedFiles_${testsuffix})
      message(STATUS "NiftiTestGeneratedFiles_${testsuffix}" )
    set_tests_properties( ${TEST_PREFIX}_tool_${testsuffix} ${TEST_PREFIX}_test_${testsuffix}
            PROPERTIES RESOURCE_LOCK Serial_${testsuffix}
            )
    set_tests_properties(cleanup_${testsuffix} PROPERTIES FIXTURES_CLEANUP  NiftiTestGeneratedFiles_${testsuffix} )
  endforeach()

  set_tests_properties( ${TEST_PREFIX}_tool_diff_hdr         PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_zn1)
  set_tests_properties( ${TEST_PREFIX}_tool_diff_nims        PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_zn1)
  set_tests_properties( ${TEST_PREFIX}_tool_copy_brick_list  PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_zn1)
  set_tests_properties( ${TEST_PREFIX}_tool_disp_ci          PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_zn1)
  set_tests_properties( ${TEST_PREFIX}_tool_disp_ts          PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_zn1)
  set_tests_properties( ${TEST_PREFIX}_tool_strip_extras     PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_zn1)
  set_tests_properties( ${TEST_PREFIX}_tool_check_hdr        PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_za2)
  set_tests_properties( ${TEST_PREFIX}_tool_check_nim        PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFiles_za2)
  #set_tests_properties( ${TEST_PREFIX}_tool_copy_collapsed_image PROPERTIES FIXTURES_REQUIRED NiftiTestGeneratedFileszn1)

  set_tests_properties( ${TEST_PREFIX}_tool_diff_hdr ${TEST_PREFIX}_tool_diff_nims
          ${TEST_PREFIX}_tool_copy_brick_list
          ${TEST_PREFIX}_tool_disp_ci ${TEST_PREFIX}_tool_disp_ts ${TEST_PREFIX}_tool_strip_extras
          PROPERTIES RESOURCE_LOCK Serial_zn1
          )
  #==END NIFTI1 and NIFTI2 common tests ============================================

  add_test( NAME ${TEST_PREFIX}_misc_tests COMMAND $<TARGET_FILE:${TOOL_NAME}> -run_misc_tests -debug 2 -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/e4.60005.nii.gz )
  if(UNIX AND NIFTI_SHELL_SCRIPT_TESTS) # unix needed to run shell scripts
      set(NIFTI_TEST_SCRIPT_DIR ${CMAKE_CURRENT_LIST_DIR}/nifti_regress_test/cmake_testscripts)
      add_test( NAME ${TEST_PREFIX}_modhdr_exts        COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/mod_header_test.sh    $<TARGET_FILE:${TOOL_NAME}> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )
      add_test( NAME ${TEST_PREFIX}_bricks_test        COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/bricks_test.sh        $<TARGET_FILE:${TOOL_NAME}> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )

      add_test( NAME ${TEST_PREFIX}_dci_test           COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/dci_test.sh           $<TARGET_FILE:${TOOL_NAME}> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )
      add_test( NAME ${TEST_PREFIX}_comment_test       COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/comment_test.sh       $<TARGET_FILE:${TOOL_NAME}> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )
      add_test( NAME ${TEST_PREFIX}_dsets_test         COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/dsets_test.sh         $<TARGET_FILE:${TOOL_NAME}> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )
      add_test( NAME ${TEST_PREFIX}_newfiles_test      COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/newfiles_test.sh      $<TARGET_FILE:${TOOL_NAME}> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )

      add_test( NAME ${TEST_PREFIX}_dts_test            COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/dts_test.sh          $<TARGET_FILE:${TOOL_NAME}> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )
      set_tests_properties( ${TEST_PREFIX}_dts_test PROPERTIES DEPENDS ${TEST_PREFIX}_bricks_test)

      add_test( NAME ${TEST_PREFIX}_mod_hdr2_test       COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/mod_header_2_test.sh $<TARGET_FILE:${TOOL_NAME}> ${fetch_testing_data_SOURCE_DIR}/CiftiLib_data )
      add_test( NAME ${TEST_PREFIX}_c21_a_info_test     COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/c21_a_info_test.sh   $<TARGET_FILE:${TOOL_NAME}> ${fetch_testing_data_SOURCE_DIR}/CiftiLib_data )
      add_test( NAME ${TEST_PREFIX}_c21_b_cifti_in_test COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/c21_b_cifti_in.sh    $<TARGET_FILE:${TOOL_NAME}> ${fetch_testing_data_SOURCE_DIR}/CiftiLib_data )
      add_test( NAME ${TEST_PREFIX}_c21_c_make_im_test  COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/c21_c_make_im.sh     $<TARGET_FILE:${TOOL_NAME}> ${fetch_testing_data_SOURCE_DIR}/CiftiLib_data )
      add_test( NAME ${TEST_PREFIX}_c21_d_misc_tests    COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/c21_d_misc_tests.sh  $<TARGET_FILE:${TOOL_NAME}> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )

  endif()
  unset(TEST_SUFFIX)
  unset(TEST_PREFIX)
  unset(TOOL_NAME)
endif()
