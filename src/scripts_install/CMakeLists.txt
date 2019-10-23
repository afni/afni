# This file manages the installation of all scripts. In the future the R and python
# scripts might be migrated to packages

set(SCRIPT_PERMISSIONS "OWNER_READ;OWNER_EXECUTE;WORLD_EXECUTE;WORLD_READ")

install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/
  DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
  FILES_MATCHING
  PATTERN "*" PERMISSIONS ${SCRIPT_PERMISSIONS}
)

install(
  DIRECTORY ../R_scripts/
  DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
  FILES_MATCHING
  PATTERN "*R" PERMISSIONS ${SCRIPT_PERMISSIONS}
)

install(PROGRAMS ../pkundu/meica.py ../jzosky/RetroTS.py
        DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
)

install(PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/../Audio/@sound_to_header.csh
        DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
)

# The following should be replaced by a pip install with console entrypoints:
set(PY_FILES
    1d_tool.py
    1dplot.py
    @djunct_calc_mont_dims.py
    @djunct_combine_str.py
    @djunct_is_label.py
    @djunct_make_script_and_rst.py
    @djunct_select_str.py
    @DoPerRoi.py
    abids_json_info.py
    abids_json_tool.py
    abids_lib.py
    abids_tool.py
    afni_base.py
    afni_proc.py
    afni_restproc.py
    afni_skeleton.py
    afni_system_check.py
    afni_util.py
    afni_xmat.py
    align_epi_anat.py
    apqc_make_html.py
    apqc_make_tcsh.py
    ask_me.py
    auto_warp.py
    BayesianGroupAna.py
    db_mod.py
    eg_main_chrono.py
    epi_b0_correct.py
    fat_lat_csv.py
    fat_mat_sel.py
    fat_mvm_gridconv.py
    fat_mvm_prep.py
    fat_mvm_review.py
    fat_mvm_scripter.py
    fat_roi_row.py
    gen_epi_review.py
    gen_group_command.py
    gen_ss_review_scripts.py
    gen_ss_review_table.py
    gui_uber_align_test.py
    gui_uber_skel.py
    gui_uber_subj.py
    gui_uber_ttest.py
    gui_xmat.py
    lib_afni1D.py
    lib_apqc_html.py
    lib_apqc_html_css.py
    lib_apqc_html_helps.py
    lib_apqc_io.py
    lib_apqc_tcsh.py
    lib_b0_corr.py
    lib_decay_timing.py
    lib_dti_sundry.py
    lib_fat_funcs.py
    lib_fat_plot_sel.py
    lib_fat_Rfactor.py
    lib_gershgorin.py
    lib_matplot.py
    lib_msar.py
    lib_plot_1D.py
    lib_qt_gui.py
    lib_rand_timing.py
    lib_realtime.py
    lib_RR_plot.py
    lib_ss_review.py
    lib_subjects.py
    lib_surf_clustsim.py
    lib_system_check.py
    lib_textdata.py
    lib_timing.py
    lib_uber_align.py
    lib_uber_skel.py
    lib_uber_subject.py
    lib_uber_ttest.py
    lib_vars_object.py
    lib_wx.py
    lpc_align.py
    make_pq_script.py
    make_random_timing.py
    make_stim_times.py
    module_test_lib.py
    neuro_deconvolve.py
    niml_parsing.py
    option_list.py
    parse_fs_lt_log.py
    python_module_test.py
    quick.alpha.vals.py
    read_matlab_files.py
    realtime_receiver.py
    slow_surf_clustsim.py
    tedana_wrapper.py
    timing_tool.py
    uber_align_test.py
    uber_proc.py
    uber_skel.py
    uber_subject.py
    uber_ttest.py
    ui_xmat.py
    unWarpEPI.py
    xmat_tool.py
)
list(TRANSFORM PY_FILES PREPEND
     "${CMAKE_CURRENT_SOURCE_DIR}/../python_scripts/afni_python/"
)

install(PROGRAMS ${PY_FILES} DESTINATION ${AFNI_INSTALL_RUNTIME_DIR})
