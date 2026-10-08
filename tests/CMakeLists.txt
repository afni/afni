project(TESTS)
#
# Tests -- building/running is aggregated within a separate directory so they do not
# interact/pollute main build space
#
# yoh: for some reason WORKING_DIRECTORY is not quite working for me says BAD_COMMAND
# for the binary to be executed.  That is why this solution with tests/ directory
#

add_test(afni:start ../../tests/scripts/utils/xvfb-driver -- afni -com
         "OPEN_WINDOW axialimage; SAVE_JPEG axialimage test1; QUIT"
)

# TODO: figure out how to reference it correctly, or just copy to this target tests/
# build directory Allow for 1 error and 2 failures reported -- due to the ones upon exit
add_test(
  do.examples
  ../../tests/scripts/utils/xvfb-driver
  -e
  0
  -f
  0
  --
  @DO.examples
  -auto_test
)
set_tests_properties(do.examples PROPERTIES LABELS GLX)

if(TARGET 3dInSync)
  add_test(
    NAME 3dInSync:edges-oracle
    COMMAND
      ${Python_EXECUTABLE}
      ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/run_insync_edges.py
      --bin $<TARGET_FILE:3dInSync>
  )
  set_tests_properties(3dInSync:edges-oracle PROPERTIES LABELS "3dInSync;numeric;unit")

  # Integration smoke tests: repeated conditions, the 3dISC pair-map bridge,
  # tail options, -missing, -memory_limit, and -zcensor. The script uses 3dinfo
  # and 1dcat from the same directory as 3dInSync, and bash process substitution.
  if(TARGET 3dinfo AND TARGET 1dcat)
    find_program(BASH_PROGRAM bash)
    if(BASH_PROGRAM)
      add_test(
        NAME 3dInSync:integration
        COMMAND
          ${BASH_PROGRAM}
          ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_insync_integration.sh
          $<TARGET_FILE:3dInSync>
      )
      set_tests_properties(3dInSync:integration PROPERTIES LABELS "3dInSync;integration")
    endif()
  endif()

  # Memory-reduction options: compact in-mask store, -roi_only, -float16, and
  # -zcensor through those paths, on small synthetic data.
  if(TARGET 3dUndump AND TARGET 3dTcat AND TARGET 3dcalc AND TARGET 3dBrickStat
     AND TARGET 3dROIstats AND TARGET 1dtranspose)
    find_program(BASH_PROGRAM bash)
    if(BASH_PROGRAM)
      add_test(
        NAME 3dInSync:memory
        COMMAND
          ${BASH_PROGRAM}
          ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_insync_memory.sh
          $<TARGET_FILE:3dInSync>
      )
      set_tests_properties(3dInSync:memory PROPERTIES LABELS "3dInSync;integration")
    endif()
  endif()
endif()

if(TARGET 3dRSA)
  add_test(
    NAME 3dRSA:1d-oracle
    COMMAND
      ${Python_EXECUTABLE}
      ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/run_1d_oracle.py
      --bin $<TARGET_FILE:3dRSA>
  )
  set_tests_properties(3dRSA:1d-oracle PROPERTIES LABELS "3dRSA;numeric;unit")

  add_test(
    NAME 3dRSA:numeric
    COMMAND
      ${Python_EXECUTABLE}
      ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/run_numeric.py
      --bin $<TARGET_FILE:3dRSA>
      --require-deps
  )
  set_tests_properties(3dRSA:numeric PROPERTIES LABELS "3dRSA;numeric")
endif()

if(TARGET 1dTrdm AND TARGET 3dRSA)
  add_test(
    NAME 1dTrdm:numeric
    COMMAND
      ${Python_EXECUTABLE}
      ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/run_trdm_numeric.py
      --bin $<TARGET_FILE:1dTrdm>
      --rsa-bin $<TARGET_FILE:3dRSA>
      --require-deps
  )
  set_tests_properties(1dTrdm:numeric PROPERTIES LABELS "1dTrdm;3dRSA;numeric")
endif()

if(ENABLE_TESTS)
  # Direct unit gate for the dataset-agnostic ISC summaries behind 3dInSync.
  add_executable(
    thd_insync_unit
    ${CMAKE_SOURCE_DIR}/src/thd_insync.c
    ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_insync.c
  )
  target_include_directories(thd_insync_unit PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(thd_insync_unit PRIVATE m)
  add_test(NAME thd_insync:unit COMMAND thd_insync_unit)
  set_tests_properties(thd_insync:unit PROPERTIES LABELS "3dInSync;numeric;unit")

  # M1b/M1d's dataset-agnostic inference and memory-ledger primitives have a
  # tiny direct unit gate in addition to their end-to-end consumers above.
  add_executable(
    thd_mapinfer_unit
    ${CMAKE_SOURCE_DIR}/src/thd_mapinfer.c
    ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mapinfer.c
  )
  target_include_directories(thd_mapinfer_unit PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(thd_mapinfer_unit PRIVATE m)
  add_test(NAME thd_mapinfer:unit COMMAND thd_mapinfer_unit)
  set_tests_properties(thd_mapinfer:unit PROPERTIES LABELS "3dRSA;1dTrdm;numeric;unit")

  add_executable(
    thd_patterns_unit
    ${CMAKE_SOURCE_DIR}/src/thd_patterns.c
    ${CMAKE_SOURCE_DIR}/src/thd_datatable.c
    ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_patterns.c
  )
  target_include_directories(thd_patterns_unit PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(
    thd_patterns_unit
    PRIVATE AFNI::mri NIFTI::nifti2 NIFTI::nifticdf m
  )
  add_test(NAME thd_patterns:unit COMMAND thd_patterns_unit)
  set_tests_properties(thd_patterns:unit PROPERTIES LABELS "3dRSA;numeric;unit")
endif()
