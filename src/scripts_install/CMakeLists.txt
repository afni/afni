cmake_policy(SET CMP0037 OLD)
set(PROGRAM_SCRIPTS
   1dRplot 3dLME 3dMEMA 3dMVM 3dMax 3dPAR2AFNI.pl 3dPFM 3dRprogDemo 3dSignatures
   @1dDiffMag @2dwarper @2dwarper.Allin @4Daverage
   @ANATICOR @AddEdge @AfniEnv @AfniOrient2RAImap @AfniOrientSign
   @Align_Centers @Atlasize @Center_Distance @CheckForAfniDset @CommandGlobb
   @DO.examples @DTI_studio_reposition @DeblankFileNames @DiceMetric
   @DriveAfni @DriveSuma @ElectroGrid @ExamineGenFeatDists @FS_roi_label
   @FSlabel2dset @FindAfniDsetPath @FromRAI @FullPath @GetAfniBin @GetAfniDims
   @GetAfniID @GetAfniOrient @GetAfniPrefix @GetAfniRes @GetAfniView
   @GradFlipTest @Install_3dPFM_Demo @Install_AfniRetinoDemo
   @Install_ClustScat_Demo @Install_D99_macaque @Install_DBSproc
   @Install_FATCAT_DEMO @Install_FATCAT_DEMO2 @Install_FATMVM_DEMO
   @Install_InstaCorr_Demo @Install_MEICA_Demo @Install_NIH_Marmoset
   @Install_RSFMRI_Motion_Group_Demo @Install_TSrestMovieDemo @IsoMasks
   @MakeLabelTable @NoExt @NoPound @NoisySkullStrip @Purify_1D @Quiet_Talkers
   @ROI_Corr_Mat @R_funclist @RenamePanga @Reorder @RetinoProc @SSwarper
   @SUMA_AlignToExperiment @SUMA_FSvolToBRIK @SUMA_Make_Spec_Caret
   @SUMA_Make_Spec_FS @SUMA_Make_Spec_SF @SUMA_renumber_FS @ScaleVolume
   @ScriptCheck @Shift_Volume @ShowDynamicRange @SkullStrip_TouchUp
   @Spharm.examples @SurfSmooth.HEAT_07.examples @T1scale @TimeDiff @ToRAI
   @UpdateAfni @VolCenter @afni.run.me @afni_R_package_install
   @afni_refacer_make_master @afni_refacer_make_onebigA12 @afni_refacer_run
   @align_partial_oblique @auto_align @auto_tlrc @build_afni_Xlib
   @chauffeur_afni @clean_help_dir @clip_volume @compute_OC_weights
   @compute_gcor @demo_prompt @diff.files @diff.tree @djunct_4d_imager
   @djunct_4d_slices_to_3d_vol @djunct_dwi_selector.tcsh
   @djunct_edgy_align_check @djunct_montage_coordinator @djunct_slice_space
   @djunct_vol_3slice_select @escape- @extract_meica_ortvec @fast_roi
   @fat_tract_colorize @fix_FSsphere @float_fix @get.afni.version
   @global_parse @grayplot @help.AFNI @isOblique @make_plug_diff
   @make_stim_file @measure_bb_thick @measure_erosion_thick @measure_in2out
   @move.to.series.dirs @np @parse_afni_name @parse_name @radial_correlate
   @simulate_motion @snapshot_volreg @snapshot_volreg3 @statauxcode
   @suma_reprefixize_spec @surf_to_vol_spackle @thickness_master @toMNI_Awarp
   @toMNI_Qwarpar @update.afni.binaries @xyz_to_ijk AFNI_Batch_R
   ExamineXmat afni_run_R fat_proc_align_anat_pair
   fat_proc_axialize_anat fat_proc_connec_vis fat_proc_convert_dcm_anat
   fat_proc_convert_dcm_dwis fat_proc_decmap fat_proc_dwi_to_dt
   fat_proc_filter_dwis fat_proc_grad_plot fat_proc_imit2w_from_t1w
   fat_proc_map_to_dti fat_proc_select_vols p2dsetstat rPkgsInstall
   suma_change_spec
    )


# Confirm that all files have been explicitly listed, which is necessary for
# cmake to cleanly detect additional files.
file(GLOB ALL_FILES "*")
list(REMOVE_ITEM ALL_FILES "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt")

foreach(SCRIPT ${ALL_FILES})
  get_filename_component(FILENAME ${SCRIPT} NAME)
  if (${FILENAME} IN_LIST PROGRAM_SCRIPTS)
    # add_executable(${FILENAME} ${SCRIPT})
  else()
    message(FATAL_ERROR
     "You need to add ${SCRIPT} to PROGRAM_SCRIPTS in the CMakeLists.txt file in that directory")
  endif()
endforeach()

set(PROGRAM_SCRIPTS ${PROGRAM_SCRIPTS} PARENT_SCOPE)
install(PROGRAMS ${PROGRAM_SCRIPTS} DESTINATION ${CMAKE_INSTALL_BINDIR})
