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

# Set lib version when buildung shared libs.
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
    # is duplicated here.  The verify_nifti1_headers_are_smame test
    # will fail if these two files are not identical.
    if( ( IS_DIRECTORY ${PROJECT_SOURCE_DIR}/nifti2 ) AND (IS_DIRECTORY ${PROJECT_SOURCE_DIR}/niftilib))
        message(STATUS "Testing same")
        add_test(verify_nifti1_headers_are_smame ${CMAKE_COMMAND} -E compare_files ${PROJECT_SOURCE_DIR}/nifti2/nifti1.h ${PROJECT_SOURCE_DIR}/niftilib/nifti1.h)
    endif()
    add_executable(${NIFTI_PACKAGE_PREFIX}clib_02_nifti2 clib_02_nifti2.c)
    target_link_libraries(${NIFTI_PACKAGE_PREFIX}clib_02_nifti2 PUBLIC ${NIFTI_NIFTILIB2_NAME})

    # Do all regression tests
    add_test( NAME nifti_ver COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> -nifti_ver)
    add_test( NAME nifti_tool_ver COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> -ver)
    add_test( NAME nifti_help COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> -help)
    add_test( NAME nifti_nifti_hist COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> -nifti_hist)
    add_test( NAME nifti_hist COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> -hist)

    if (CMAKE_VERSION GREATER_EQUAL 3.11.0) # CMAKE VERSION 3.11.0 needed for fetching data with cmake
        add_test( NAME nifti_disp_hdr COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> -disp_hdr -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )
        add_test( NAME nifti_disp_nim COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> -disp_nim -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )
        add_test( NAME nifti_disp_ext COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> -disp_ext -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )
        add_test( NAME nifti_header_check COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> -check_hdr -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )
        add_test( NAME nifti_nim_check COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> -check_nim -infiles ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data/anat0.nii )
        if(UNIX) # unix needed to run shell scripts
            set(NIFTI_TEST_SCRIPT_DIR ${CMAKE_CURRENT_LIST_DIR}/nifti_regress_test/cmake_testscripts)
            add_test( NAME nifti_modhdr_exts COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/mod_header_test.sh $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )
            add_test( NAME nifti_bricks_test COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/bricks_test.sh $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )

            add_test( NAME nifti_dci_test COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/dci_test.sh $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )
            add_test( NAME nifti_comment_test COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/comment_test.sh $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )
            add_test( NAME nifti_dsets_test COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/dsets_test.sh $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )
            add_test( NAME nifti_newfiles_test COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/newfiles_test.sh $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )

            add_test( NAME nifti_dts_test COMMAND sh ${NIFTI_TEST_SCRIPT_DIR}/dts_test.sh $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_tool> ${fetch_testing_data_SOURCE_DIR}/nifti_regress_data )
            set_tests_properties( nifti_dts_test PROPERTIES DEPENDS nifti_bricks_test)


        endif()
    endif()
endif()
