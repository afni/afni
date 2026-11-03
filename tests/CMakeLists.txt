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

  # Output view: follows the first input unless the -mask/-atlas is in another view.
  if(TARGET 3dUndump AND TARGET 3dTcat AND TARGET 3dcalc AND TARGET 3dcopy
     AND TARGET 3drefit AND TARGET 3dinfo)
    find_program(BASH_PROGRAM bash)
    if(BASH_PROGRAM)
      add_test(
        NAME 3dInSync:view
        COMMAND
          ${BASH_PROGRAM}
          ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_insync_view.sh
          $<TARGET_FILE:3dInSync>
      )
      set_tests_properties(3dInSync:view PROPERTIES LABELS "3dInSync;integration")
    endif()
  endif()
endif()

if(TARGET 1dXmatTool AND TARGET 3dDeconvolve AND TARGET 3dLSS)
  add_test(NAME 1dXmatTool:split-runs
    COMMAND ${Python_EXECUTABLE}
      ${CMAKE_SOURCE_DIR}/tests/scripts/check_1dXmatTool.py
      --tool $<TARGET_FILE:1dXmatTool>
      --deconvolve $<TARGET_FILE:3dDeconvolve>
      --lss $<TARGET_FILE:3dLSS>
      --workdir ${CMAKE_CURRENT_BINARY_DIR}/xmat_split_check)
  set_tests_properties(1dXmatTool:split-runs PROPERTIES
    LABELS "1dXmatTool;3dLSS;numeric;integration;xmat"
    ENVIRONMENT "OMP_NUM_THREADS=1;OPENBLAS_NUM_THREADS=1;VECLIB_MAXIMUM_THREADS=1")
endif()

if(TARGET 1dXmatTool AND TARGET 1dcat AND TARGET 3dMVPA)
  add_test(NAME 1dXmatTool:features
    COMMAND ${Python_EXECUTABLE} ${CMAKE_SOURCE_DIR}/tests/scripts/check_1dXmatTool_features.py
      --tool $<TARGET_FILE:1dXmatTool> --cat $<TARGET_FILE:1dcat> --mvpa $<TARGET_FILE:3dMVPA>)
  set_tests_properties(1dXmatTool:features PROPERTIES LABELS "1dXmatTool;3dMVPA;numeric;integration;xmat"
    ENVIRONMENT "OMP_NUM_THREADS=2;OPENBLAS_NUM_THREADS=1;VECLIB_MAXIMUM_THREADS=1")
endif()

if(TARGET 3dSRM)
  # The library driver: the same thd_srm.c with no dataset or table code, used
  # by check_3dSRM.py as the independent reference for the fit.
  add_executable(srm_cli ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/srm_cli.c)
  target_include_directories(srm_cli PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(srm_cli PRIVATE $<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_C> m)
  target_compile_definitions(srm_cli PRIVATE "$<IF:$<TARGET_EXISTS:OpenMP::OpenMP_C>,USE_OMP,>")

  if(TARGET 3dLSS AND TARGET 3dDeconvolve)
    add_test(NAME 3dSRM:lss-items
      COMMAND ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/tests/scripts/check_3dSRM_lss.py
        --prog $<TARGET_FILE:3dSRM>
        --deconvolve $<TARGET_FILE:3dDeconvolve>
        --lss $<TARGET_FILE:3dLSS>
        --workdir ${CMAKE_CURRENT_BINARY_DIR}/srm_lss_check)
    set_tests_properties(3dSRM:lss-items PROPERTIES LABELS "3dSRM;numeric;integration;xmat")
  endif()

  # End-to-end checks of 3dSRM (fit, CV, -model_in, -update_S, -apply_W) against
  # numpy and the driver, once with the default kernel and once with the fit
  # forced onto Gram matrices, which the small test data would not choose.
  foreach(kernel AUTO GRAM)
    string(TOLOWER ${kernel} kname)
    add_test(
      NAME 3dSRM:check-${kname}
      COMMAND
        ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/tests/scripts/check_3dSRM.py
        --prog $<TARGET_FILE:3dSRM>
        --srm-cli $<TARGET_FILE:srm_cli>
        --workdir ${CMAKE_CURRENT_BINARY_DIR}/srm_check_${kname}
        --require-deps
    )
    set_tests_properties(3dSRM:check-${kname} PROPERTIES
      LABELS "3dSRM;numeric;integration"
      ENVIRONMENT "AFNI_SRM_KERNEL=${kernel}")
  endforeach()
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
  add_test(
    NAME 3dRSA:subject-regression
    COMMAND ${Python_EXECUTABLE}
      ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_rsa_subject.py
      --bin $<TARGET_FILE:3dRSA>
  )
  set_tests_properties(3dRSA:subject-regression PROPERTIES LABELS "3dRSA;numeric")
endif()

if(TARGET 3dMVPA AND TARGET 3dUndump AND TARGET 3dbucket
   AND TARGET 3drefit AND TARGET 3dinfo AND TARGET 3dmaskave
   AND TARGET 3dmaskdump)
  find_program(BASH_PROGRAM bash)
  if(BASH_PROGRAM)
    add_test(
      NAME 3dMVPA:spatial-classification
      COMMAND
        ${BASH_PROGRAM}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_integration.sh
        $<TARGET_FILE:3dMVPA>
        $<TARGET_FILE:3dUndump>
        $<TARGET_FILE:3dbucket>
        $<TARGET_FILE:3drefit>
        $<TARGET_FILE:3dinfo>
        $<TARGET_FILE:3dmaskave>
        $<TARGET_FILE:3dmaskdump>
    )
    set_tests_properties(3dMVPA:spatial-classification PROPERTIES
                         LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:ridge
      COMMAND ${BASH_PROGRAM}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_ridge.sh
        $<TARGET_FILE:3dMVPA> $<TARGET_FILE:3dUndump>
        $<TARGET_FILE:3dinfo> $<TARGET_FILE:3dmaskdump>
    )
    set_tests_properties(3dMVPA:ridge PROPERTIES LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:coefficients
      COMMAND ${BASH_PROGRAM}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_coefficients.sh
        $<TARGET_FILE:3dMVPA> $<TARGET_FILE:3dUndump> $<TARGET_FILE:3dmaskdump>
    )
    set_tests_properties(3dMVPA:coefficients PROPERTIES LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:svm
      COMMAND ${BASH_PROGRAM}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_svm.sh
        $<TARGET_FILE:3dMVPA> $<TARGET_FILE:3dUndump> $<TARGET_FILE:3dmaskdump>
    )
    set_tests_properties(3dMVPA:svm PROPERTIES LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:classification-audits
      COMMAND ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_audits.py
        $<TARGET_FILE:3dMVPA> $<TARGET_FILE:3dUndump>
    )
    set_tests_properties(3dMVPA:classification-audits PROPERTIES
                         LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:validate-only
      COMMAND ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_validate.py
        $<TARGET_FILE:3dMVPA>
    )
    set_tests_properties(3dMVPA:validate-only PROPERTIES
                         LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:fixed-partitions
      COMMAND ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_partitions.py
        $<TARGET_FILE:3dMVPA>
    )
    set_tests_properties(3dMVPA:fixed-partitions PROPERTIES
                         LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:transfer
      COMMAND ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_transfer.py
        $<TARGET_FILE:3dMVPA>
    )
    set_tests_properties(3dMVPA:transfer PROPERTIES
                         LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:nuisance
      COMMAND ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_nuisance.py
        $<TARGET_FILE:3dMVPA>
    )
    set_tests_properties(3dMVPA:nuisance PROPERTIES
                         LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:feature-selection
      COMMAND ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_selection.py
        $<TARGET_FILE:3dMVPA>
    )
    set_tests_properties(3dMVPA:feature-selection PROPERTIES
                         LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:tuning-objectives
      COMMAND ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_tuning.py
        $<TARGET_FILE:3dMVPA>
    )
    set_tests_properties(3dMVPA:tuning-objectives PROPERTIES
                         LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:pca
      COMMAND ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_pca.py
        $<TARGET_FILE:3dMVPA>
    )
    set_tests_properties(3dMVPA:pca PROPERTIES
                         LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:saved-models
      COMMAND ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_models.py
        $<TARGET_FILE:3dMVPA>
    )
    set_tests_properties(3dMVPA:saved-models PROPERTIES
                         LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:response-ridge
      COMMAND ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_response_ridge.py
        $<TARGET_FILE:3dMVPA>
    )
    set_tests_properties(3dMVPA:response-ridge PROPERTIES
                         LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:paired-comparisons
      COMMAND ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_comparisons.py
        $<TARGET_FILE:3dMVPA>
    )
    set_tests_properties(3dMVPA:paired-comparisons PROPERTIES
                         LABELS "3dMVPA;numeric;integration")
    add_test(
      NAME 3dMVPA:logistic
      COMMAND ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_logistic.py
        $<TARGET_FILE:3dMVPA>
    )
    set_tests_properties(3dMVPA:logistic PROPERTIES LABELS "3dMVPA;numeric;integration")
    foreach(regression_test regression regression_null progress)
      add_test(NAME 3dMVPA:${regression_test}
        COMMAND ${Python_EXECUTABLE}
          ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_${regression_test}.py
          $<TARGET_FILE:3dMVPA>)
      set_tests_properties(3dMVPA:${regression_test} PROPERTIES
        LABELS "3dMVPA;numeric;integration")
    endforeach()
    if(TARGET CreateIcosahedron AND TARGET ConvertDset)
      add_test(NAME 3dMVPA:regression-surface
        COMMAND ${Python_EXECUTABLE}
          ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_regression_surface.py
          $<TARGET_FILE:3dMVPA> $<TARGET_FILE:CreateIcosahedron> $<TARGET_FILE:ConvertDset>)
      set_tests_properties(3dMVPA:regression-surface PROPERTIES
        LABELS "3dMVPA;numeric;integration")
    endif()
    add_test(
      NAME 3dMVPA:activation-patterns
      COMMAND ${Python_EXECUTABLE}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_patterns.py
        $<TARGET_FILE:3dMVPA>
        $<TARGET_FILE:3dmaskdump>
    )
    set_tests_properties(3dMVPA:activation-patterns PROPERTIES
                         LABELS "3dMVPA;numeric;integration")
  endif()
endif()

if(TARGET 3dMVPA AND TARGET CreateIcosahedron AND TARGET ConvertDset
   AND TARGET 3dinfo)
  find_program(BASH_PROGRAM bash)
  if(BASH_PROGRAM)
    add_test(
      NAME 3dMVPA:surface-searchlight
      COMMAND
        ${BASH_PROGRAM}
        ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa_surface.sh
        $<TARGET_FILE:3dMVPA>
        $<TARGET_FILE:CreateIcosahedron>
        $<TARGET_FILE:ConvertDset>
        $<TARGET_FILE:3dinfo>
    )
    set_tests_properties(3dMVPA:surface-searchlight PROPERTIES
                         LABELS "3dMVPA;numeric;integration;surface")
  endif()
endif()

if(TARGET 1dTrdm AND TARGET 3dRSA)
  add_test(
    NAME 3dRSA:labeled-rdms
    COMMAND ${Python_EXECUTABLE}
      ${CMAKE_SOURCE_DIR}/tests/scripts/check_rsa_labeled_rdms.py
      --bin $<TARGET_FILE:3dRSA>
      --trdm-bin $<TARGET_FILE:1dTrdm>
  )
  set_tests_properties(3dRSA:labeled-rdms PROPERTIES LABELS "3dRSA;1dTrdm;numeric;integration")
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
  # No program callers yet: exercise the shared progress module independently.
  # The unit source includes thd_progress.c to check cadence without sleeps.
  add_executable(thd_progress_unit
    ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_progress.c)
  target_include_directories(thd_progress_unit PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(thd_progress_unit PRIVATE AFNI::mri
    $<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_C> m)
  target_compile_definitions(thd_progress_unit PRIVATE
    "$<IF:$<TARGET_EXISTS:OpenMP::OpenMP_C>,USE_OMP,>")
  add_test(NAME thd_progress:unit COMMAND thd_progress_unit)
  set_tests_properties(thd_progress:unit PROPERTIES LABELS "progress;unit"
    ENVIRONMENT "OMP_NUM_THREADS=4")
  if(UNIX AND Python_EXECUTABLE)
    add_test(NAME thd_progress:terminal COMMAND ${Python_EXECUTABLE}
      ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_progress_terminal.py
      $<TARGET_FILE:thd_progress_unit>)
    set_tests_properties(thd_progress:terminal PROPERTIES LABELS "progress;unit")
  endif()

  add_executable(
    thd_datatable_unit
    ${CMAKE_SOURCE_DIR}/src/thd_datatable.c
    ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_datatable.c
  )
  target_include_directories(thd_datatable_unit PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(
    thd_datatable_unit
    PRIVATE AFNI::mri NIFTI::nifti2 NIFTI::nifticdf m
  )
  add_test(NAME thd_datatable:unit COMMAND thd_datatable_unit)
  set_tests_properties(thd_datatable:unit PROPERTIES LABELS "3dRSA;3dMVPA;3dInSync;numeric;unit")

  # Direct unit gate for the dataset-agnostic ISC summaries behind 3dInSync.
  add_executable(
    thd_insync_unit
    ${CMAKE_SOURCE_DIR}/src/thd_insync_stats.c
    ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_insync.c
  )
  target_include_directories(thd_insync_unit PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(thd_insync_unit PRIVATE m)
  add_test(NAME thd_insync:unit COMMAND thd_insync_unit)
  set_tests_properties(thd_insync:unit PROPERTIES LABELS "3dInSync;numeric;unit")

  # Shared Response Model numerics.  test_srm.c includes thd_srm.c so that it
  # can reach the static kernels, so thd_srm.c is not listed as a source.  With
  # OpenMP it also checks that results do not depend on the thread count.
  add_executable(
    thd_srm_unit
    ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_srm.c
  )
  target_include_directories(thd_srm_unit PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(thd_srm_unit PRIVATE $<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_C> m)
  target_compile_definitions(thd_srm_unit PRIVATE "$<IF:$<TARGET_EXISTS:OpenMP::OpenMP_C>,USE_OMP,>")
  add_test(NAME thd_srm:unit COMMAND thd_srm_unit)

  # Design matrices (X.xmat.1D): reading, the helpers, and the table summary columns.
  add_executable(
    thd_xmat_unit
    ${CMAKE_SOURCE_DIR}/src/thd_xmat.c
    ${CMAKE_SOURCE_DIR}/src/thd_datatable.c
    ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_xmat.c
  )
  target_include_directories(thd_xmat_unit PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(thd_xmat_unit PRIVATE AFNI::mri NIFTI::nifti2 NIFTI::nifticdf m)
  add_test(NAME thd_xmat:unit COMMAND thd_xmat_unit)
  set_tests_properties(thd_xmat:unit PROPERTIES LABELS "3dSRM;3dMVPA;3dRSA;numeric;xmat;unit")
  set_tests_properties(thd_srm:unit PROPERTIES LABELS "3dSRM;numeric;unit"
                       ENVIRONMENT "OMP_NUM_THREADS=4")

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
    thd_permute_unit
    ${CMAKE_SOURCE_DIR}/src/thd_permute.c
    ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_permute.c
  )
  target_include_directories(thd_permute_unit PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(thd_permute_unit PRIVATE AFNI::mri NIFTI::nifti2 NIFTI::nifticdf m)
  add_test(NAME thd_permute:unit COMMAND thd_permute_unit)
  set_tests_properties(thd_permute:unit PROPERTIES LABELS "3dRSA;3dMVPA;numeric;unit")

  add_executable(
    thd_patterns_unit
    ${CMAKE_SOURCE_DIR}/src/thd_patterns.c
    ${CMAKE_SOURCE_DIR}/src/thd_datatable.c
    ${CMAKE_SOURCE_DIR}/src/thd_xmat.c
    ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_patterns.c
  )
  target_include_directories(thd_patterns_unit PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(
    thd_patterns_unit
    PRIVATE AFNI::mri NIFTI::nifti2 NIFTI::nifticdf m
  )
  add_test(NAME thd_patterns:unit COMMAND thd_patterns_unit)
  set_tests_properties(thd_patterns:unit PROPERTIES LABELS "3dRSA;3dMVPA;numeric;unit")

  add_executable(
    thd_rsa_unit
    ${CMAKE_SOURCE_DIR}/src/thd_phasefft.c
    ${CMAKE_SOURCE_DIR}/src/thd_permute.c
    ${CMAKE_SOURCE_DIR}/src/thd_rsa.c
    ${CMAKE_SOURCE_DIR}/src/thd_simmatrix.c
    ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_rsa.c
  )
  target_include_directories(thd_rsa_unit PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(
    thd_rsa_unit
    PRIVATE AFNI::mri NIFTI::nifti2 NIFTI::nifticdf m
  )
  add_test(NAME thd_rsa:unit COMMAND thd_rsa_unit)
  set_tests_properties(thd_rsa:unit PROPERTIES LABELS "3dRSA;numeric;unit")

  add_executable(
    thd_sampleset_unit
    ${CMAKE_SOURCE_DIR}/src/thd_sampleset.c
    ${CMAKE_SOURCE_DIR}/src/thd_xmat.c
    ${CMAKE_SOURCE_DIR}/src/thd_datatable.c
    ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_sampleset.c
  )
  target_include_directories(thd_sampleset_unit PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(
    thd_sampleset_unit
    PRIVATE AFNI::mri NIFTI::nifti2 NIFTI::nifticdf m
  )
  add_test(NAME thd_sampleset:unit COMMAND thd_sampleset_unit)
  set_tests_properties(thd_sampleset:unit PROPERTIES LABELS "3dMVPA;numeric;unit")

  add_executable(
    thd_crossval_unit
    ${CMAKE_SOURCE_DIR}/src/thd_crossval.c
    ${CMAKE_SOURCE_DIR}/src/thd_sampleset.c
    ${CMAKE_SOURCE_DIR}/src/thd_datatable.c
    ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_crossval.c
  )
  target_include_directories(thd_crossval_unit PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(
    thd_crossval_unit
    PRIVATE AFNI::mri NIFTI::nifti2 NIFTI::nifticdf m
  )
  add_test(NAME thd_crossval:unit COMMAND thd_crossval_unit)
  set_tests_properties(thd_crossval:unit PROPERTIES LABELS "3dMVPA;numeric;unit")

  add_executable(
    thd_mvpa_unit
    ${CMAKE_SOURCE_DIR}/src/thd_mvpa.c
    ${CMAKE_SOURCE_DIR}/src/thd_crossval.c
    ${CMAKE_SOURCE_DIR}/src/thd_sampleset.c
    ${CMAKE_SOURCE_DIR}/src/thd_datatable.c
    ${CMAKE_SOURCE_DIR}/src/pmolfese/tests/test_mvpa.c
  )
  target_include_directories(thd_mvpa_unit PRIVATE ${CMAKE_SOURCE_DIR}/src)
  target_link_libraries(
    thd_mvpa_unit
    PRIVATE AFNI::mri NIFTI::nifti2 NIFTI::nifticdf m
  )
  add_test(NAME thd_mvpa:unit COMMAND thd_mvpa_unit)
  set_tests_properties(thd_mvpa:unit PROPERTIES LABELS "3dMVPA;numeric;unit")
endif()
