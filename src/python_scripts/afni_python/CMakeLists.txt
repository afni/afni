# # script being programs on their own
# SET(SCRIPTS
#   1d_tool.py afni_proc.py afni_util.py afni_restproc.py afni_system_check.py
#   align_epi_anat.py auto_warp.py  eg_main_chrono.py fat_lat_csv.py
#   fat_mat_sel.py fat_mvm_gridconv.py fat_mvm_prep.py fat_mvm_scripter.py
#   fat_roi_row.py fat_mvm_review.py gen_epi_review.py gen_group_command.py
#   gen_ss_review_scripts.py gen_ss_review_table.py make_pq_script.py
#   make_random_timing.py make_stim_times.py neuro_deconvolve.py
#   python_module_test.py quick.alpha.vals.py realtime_receiver.py
#   slow_surf_clustsim.py timing_tool.py uber_align_test.py uber_proc.py
#   uber_skel.py uber_subject.py uber_ttest.py xmat_tool.py read_matlab_files.py
#   afni_skeleton.py parse_fs_lt_log.py @DoPerRoi.py lpc_align.py
#   unWarpEPI.py
#   )

# # everything else that is not meant to be a standalone script
# SET(MODULES
#   afni_base.py afni_xmat.py ask_me.py db_mod.py
#   gui_uber_align_test.py gui_uber_skel.py gui_uber_subj.py gui_uber_ttest.py
#   gui_xmat.py lib_afni1D.py
#   lib_fat_funcs.py lib_fat_plot_sel.py lib_fat_Rfactor.py
#   lib_matplot.py lib_qt_gui.py
#   lib_realtime.py lib_RR_plot.py lib_subjects.py lib_surf_clustsim.py
#   lib_system_check.py lib_textdata.py lib_timing.py lib_uber_align.py
#   lib_uber_skel.py lib_uber_subject.py lib_uber_ttest.py
#   lib_vars_object.py lib_wx.py
#   module_test_lib.py option_list.py ui_xmat.py
#   )

# INSTALL(FILES ${MODULES}
#   DESTINATION ${AFNI_INSTALL_SCRIPT_DIR} COMPONENT Runtime
#   PERMISSIONS ${FILE_PERMISSIONS})
# INSTALL(PROGRAMS ${SCRIPTS}
#   DESTINATION ${AFNI_INSTALL_SCRIPT_DIR} COMPONENT Runtime)
