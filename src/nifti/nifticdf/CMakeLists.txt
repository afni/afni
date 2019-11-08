set(NIFTI_CDFLIB_NAME ${NIFTI_PACKAGE_PREFIX}nifticdf)

add_library(${NIFTI_CDFLIB_NAME} nifticdf.c)
target_link_libraries(${NIFTI_CDFLIB_NAME} PUBLIC ${NIFTI_PACKAGE_PREFIX}niftiio)
target_include_directories(
  ${NIFTI_CDFLIB_NAME} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                              $<INSTALL_INTERFACE:${NIFTI_INSTALL_INCLUDE_DIR}>
)
if(NIFTI_BUILD_APPLICATIONS)
  add_executable(${NIFTI_PACKAGE_PREFIX}nifti_stats nifti_stats.c)
  target_link_libraries(
    ${NIFTI_PACKAGE_PREFIX}nifti_stats PUBLIC ${NIFTI_PACKAGE_PREFIX}niftiio
                                              ${NIFTI_PACKAGE_PREFIX}nifticdf
  )
  set(NIFTICDF_APPS ${NIFTI_PACKAGE_PREFIX}nifti_stats)
endif()

if(BUILD_SHARED_LIBS)
  set_target_properties(${NIFTI_CDFLIB_NAME} PROPERTIES ${NIFTI_LIBRARY_PROPERTIES})
endif()

if(NOT NIFTI_INSTALL_NO_LIBRARIES)
  install(
    TARGETS ${NIFTI_CDFLIB_NAME} ${NIFTICDF_APPS}
    EXPORT NIFTITargets
    RUNTIME DESTINATION ${NIFTI_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${NIFTI_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${NIFTI_INSTALL_LIBRARY_DIR} COMPONENT Development
    INCLUDES DESTINATION ${NIFTI_INSTALL_INCLUDE_DIR}
  )
endif()

if(NOT NIFTI_INSTALL_NO_DEVELOPMENT)
  file(GLOB __files "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  install(
    FILES ${__files}
    DESTINATION ${NIFTI_INSTALL_INCLUDE_DIR}
    COMPONENT Development
  )
endif()

if(NIFTI_BUILD_TESTING AND NIFTI_BUILD_APPLICATIONS)
  foreach(
    DISTRIBUTION
    CORREL
    TTEST
    FTEST
    ZSCORE
    CHISQ
    BETA
    BINOM
    GAMMA
    POISSON
    NORMAL
    FTEST_NONC
    CHISQ_NONC
    LOGISTIC
    LAPLACE
    UNIFORM
    TTEST_NONC
    WEIBULL
    CHI
    INVGAUSS
    EXTVAL
    PVAL
    LOGPVAL
    LOG10PVAL
  )
    add_test(NAME ${NIFTI_PACKAGE_PREFIX}nifti_stats_${DISTRIBUTION}_test
             COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_stats> 0:4:1
                     ${DISTRIBUTION}
    )
    add_test(NAME q${NIFTI_PACKAGE_PREFIX}nifti_stats_${DISTRIBUTION}_test
             COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_stats> -q 0:4:1
                     ${DISTRIBUTION}
    )
    add_test(
      NAME q1${NIFTI_PACKAGE_PREFIX}nifti_stats_${DISTRIBUTION}_test
      COMMAND
        $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_stats> -q 0:4:1 ${DISTRIBUTION} 1 1
        1
    )
    add_test(NAME d${NIFTI_PACKAGE_PREFIX}nifti_stats_${DISTRIBUTION}_test
             COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_stats> -d 0:4:1
                     ${DISTRIBUTION}
    )
    add_test(
      NAME d1${NIFTI_PACKAGE_PREFIX}nifti_stats_${DISTRIBUTION}_test
      COMMAND
        $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_stats> -d 0:4:1 ${DISTRIBUTION} 1 1
        1
    )
    add_test(NAME 1${NIFTI_PACKAGE_PREFIX}nifti_stats_${DISTRIBUTION}_test
             COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_stats> -1 0:4:1
                     ${DISTRIBUTION}
    )
    add_test(
      NAME 11${NIFTI_PACKAGE_PREFIX}nifti_stats_${DISTRIBUTION}_test
      COMMAND
        $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_stats> -1 0:4:1 ${DISTRIBUTION} 1 1
        1
    )
    add_test(NAME z${NIFTI_PACKAGE_PREFIX}nifti_stats_${DISTRIBUTION}_test
             COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_stats> -z 0:4:1
                     ${DISTRIBUTION}
    )
    add_test(
      NAME z1${NIFTI_PACKAGE_PREFIX}nifti_stats_${DISTRIBUTION}_test
      COMMAND
        $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_stats> -z 0:4:1 ${DISTRIBUTION} 1 1
        1
    )
    add_test(NAME h${NIFTI_PACKAGE_PREFIX}nifti_stats_${DISTRIBUTION}_test
             COMMAND $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_stats> -h 0:4:1
                     ${DISTRIBUTION}
    )
    add_test(
      NAME h1${NIFTI_PACKAGE_PREFIX}nifti_stats_${DISTRIBUTION}_test
      COMMAND
        $<TARGET_FILE:${NIFTI_PACKAGE_PREFIX}nifti_stats> -h 0:4:1 ${DISTRIBUTION} 1 1
        1
    )
  endforeach()
endif()
