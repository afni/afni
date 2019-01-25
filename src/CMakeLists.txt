add_library(libheaders INTERFACE)
target_sources(libheaders INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/3ddata.h ${CMAKE_CURRENT_SOURCE_DIR}/AFNI_label.h ${CMAKE_CURRENT_SOURCE_DIR}/Amalloc.h ${CMAKE_CURRENT_SOURCE_DIR}/afni_environ.h ${CMAKE_CURRENT_SOURCE_DIR}/afni_suma.h
  ${CMAKE_CURRENT_SOURCE_DIR}/afni_warp.h ${CMAKE_CURRENT_SOURCE_DIR}/cs.h ${CMAKE_CURRENT_SOURCE_DIR}/cs_qsort_small.h ${CMAKE_CURRENT_SOURCE_DIR}/cs_sort_template.h
  ${CMAKE_CURRENT_SOURCE_DIR}/debugtrace.h ${CMAKE_CURRENT_SOURCE_DIR}/editvol.h ${CMAKE_CURRENT_SOURCE_DIR}/eispack.h ${CMAKE_CURRENT_SOURCE_DIR}/killer.h ${CMAKE_CURRENT_SOURCE_DIR}/list_struct.h ${CMAKE_CURRENT_SOURCE_DIR}/machdep.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mcw_glob.h ${CMAKE_CURRENT_SOURCE_DIR}/mcw_malloc.h ${CMAKE_CURRENT_SOURCE_DIR}/mri_dicom_stuff.h ${CMAKE_CURRENT_SOURCE_DIR}/mri_warpfield.h ${CMAKE_CURRENT_SOURCE_DIR}/mrilib.h
  ${CMAKE_CURRENT_SOURCE_DIR}/multivector.h  ${CMAKE_CURRENT_SOURCE_DIR}/tagset.h ${CMAKE_CURRENT_SOURCE_DIR}/thd_atlas.h ${CMAKE_CURRENT_SOURCE_DIR}/thd_compress.h ${CMAKE_CURRENT_SOURCE_DIR}/thd_iochan.h
  ${CMAKE_CURRENT_SOURCE_DIR}/thd_maker.h ${CMAKE_CURRENT_SOURCE_DIR}/thd_ttatlas_CA_EZ.h ${CMAKE_CURRENT_SOURCE_DIR}/thd_ttatlas_query.h ${CMAKE_CURRENT_SOURCE_DIR}/vecmat.h ${CMAKE_CURRENT_SOURCE_DIR}/vol2surf.h
  ${CMAKE_CURRENT_SOURCE_DIR}/xutil.h ${CMAKE_CURRENT_BINARY_DIR}/AFNI_version.h ${CMAKE_CURRENT_SOURCE_DIR}/niml.h ${CMAKE_CURRENT_SOURCE_DIR}/r_new_resam_dset.h ${CMAKE_CURRENT_SOURCE_DIR}/r_idisp.h ${CMAKE_CURRENT_SOURCE_DIR}/ge4_header.h)
target_include_directories(libheaders 
  INTERFACE
  ${CMAKE_CURRENT_SOURCE_DIR} 
  ${CMAKE_CURRENT_BINARY_DIR} 
  $<TARGET_PROPERTY:AFNI::converted_from_fortran,INTERFACE_INCLUDE_DIRECTORIES>
  $<TARGET_PROPERTY:coxplot,INTERFACE_INCLUDE_DIRECTORIES>
  $<TARGET_PROPERTY:NIFTI::nifti2,INTERFACE_INCLUDE_DIRECTORIES>
  ${X11_INCLUDE_DIR}
  )

# XXX remove dlcompat
# XXX remove jpeg-6b
add_subdirectory(scripts_install)
add_subdirectory(python_scripts)
add_subdirectory(R_scripts)
add_subdirectory(niml)
add_subdirectory(XmHTML)
add_subdirectory(coxplot)
add_subdirectory(crorden)
add_subdirectory(eispack)
add_subdirectory(Audio)
add_subdirectory(qhull)
add_subdirectory(svm)
add_subdirectory(volpack)
add_subdirectory(converted_from_fortran)
add_subdirectory(rickr)
add_subdirectory(3DEdge)


set_source_files_properties(gifti_choice.c PROPERTIES COMPILE_DEFINITIONS "LINK_AGAINST_GIFTI_LIB")

# Generate various afni_slice files and store them in the build tree and object library
add_library(afsliceobjs OBJECT "")
foreach(type byte complex float int rgba rgbyte short)
  set(output_file "${CMAKE_CURRENT_BINARY_DIR}/afni_slice_configured/afni_slice_${type}.c")
  configure_file(afni_slice.c "${output_file}" COPYONLY)
  set_source_files_properties("${output_file}" PROPERTIES COMPILE_DEFINITIONS "DTYPE=${type}")
  target_sources(afsliceobjs PUBLIC "${output_file}")
endforeach(type)
target_link_libraries(afsliceobjs PRIVATE libheaders)

# Generate various pcor files and store them in the build tree and as object library
add_library(pcor_objs OBJECT "")
set(PCOR_DIR ${CMAKE_CURRENT_BINARY_DIR}/configured_pcor)
configure_file(pcor.h  "${PCOR_DIR}/pcor.h" COPYONLY)
target_include_directories(pcor_objs PUBLIC 
  ${PCOR_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}
  $<TARGET_PROPERTY:NIFTI::nifti2,INTERFACE_INCLUDE_DIRECTORIES>
  $<TARGET_PROPERTY:AFNI::coxplot,INTERFACE_INCLUDE_DIRECTORIES>)

foreach(type byte float short)
	set(output_file "${CMAKE_CURRENT_BINARY_DIR}/configured_pcor/afni_pcor_${type}.c")
	configure_file(afni_pcor_update.c "${output_file}" COPYONLY)
	set_source_files_properties("${output_file}" PROPERTIES COMPILE_DEFINITIONS "DTYPE=${type}")
	target_sources(pcor_objs PUBLIC "${output_file}")
endforeach(type)

function(configure_pcor lib_name definitions)
	set(output_file "${CMAKE_CURRENT_BINARY_DIR}/configured_pcor/${lib_name}.c")
	configure_file(pcor.c "${output_file}" COPYONLY)
	set_source_files_properties("${output_file}" PROPERTIES COMPILE_DEFINITIONS "${definitions}")
	add_library(${lib_name} OBJECT "${output_file}")
endfunction(configure_pcor)
configure_pcor(pcor "REF_FLOAT_SINGLE")
configure_pcor(pcorsh "REF_FLOAT_SINGLE;VOX_SHORT")

# Add_plugin targets
set_os_flags(plug_threshold.c)
set_os_flags(plug_maskcalc.c)
foreach(target model_beta model_constant model_conv_PRF model_conv_PRF_6
		model_conv_PRF_DOG model_conv_cosine4 model_conv_diffgamma
		model_convgamma model_convgamma2a model_demri_3 model_diffexp
		model_diffusion model_exp model_expMEMRI model_expMEMRI3
		model_expr2 model_gammavar model_linear model_linplusort
		model_michaelis_menton model_null model_quadratic
		model_sinewave_ap model_sinewave_apf model_squarewave_ap
		model_squarewave_apf model_trnglwave_ap model_trnglwave_apf
		model_zero plug_3Ddump_V2 plug_3ddot plug_3ddup plug_4Ddump
		plug_L1fit plug_afnihistory plug_aslA3D3 plug_betafit plug_clust
		plug_compress plug_coorder plug_copy plug_crender
		plug_deconvolve plug_delay_V2 plug_drawdset plug_edit
		plug_extract plug_fourier plug_hemisub plug_histog
		plug_histog_multi plug_imreg plug_lsqfit plug_maskave
		plug_maskcalc plug_nlfit plug_notes plug_nth_dataset plug_nudge
		plug_null plug_permtest plug_power plug_realtime plug_rename
		plug_render plug_reorder plug_roiedit plug_roiplot plug_scatplot
		plug_second_dataset plug_stats plug_stavg plug_tag
		plug_threshold plug_ttget plug_vol2surf plug_volreg
		plug_wavelets plug_zeropad
)
	add_library(${target} ${target}.c)
	target_link_libraries(${target} PUBLIC mri)
endforeach(target)

foreach(target Xphace afni_open aiv)
  add_library(${target} ${target}.c)
  target_include_directories(${target} PUBLIC $<TARGET_PROPERTY:NIFTI::nifti2,INTERFACE_INCLUDE_DIRECTORIES>)
endforeach(target)


set_os_flags(thd_filestuff.c)
set_os_flags(machdep.c)
set(SOMEAFOBJS afni_warp.c pbar_color_defs.c afni_ports.c)
set(MISC_OBJS afni_suma.c vol2surf.c list_struct.c nifti_statlib.c matrix.c gifti_choice.c misc_math.c)
set(SUMA_OBJS  suma_afni_surface.c suma_algorithms.c suma_datasets.c suma_help.c suma_niml.c suma_string_manip.c suma_utils.c)
set(CS_OBJS cs_addto_args.c cs_fgets.c cs_gamfit.c cs_laguerre.c cs_misc.c cs_playsound.c cs_pv.c cs_qhull.c cs_qmed.c cs_sort_d.c cs_sort_di.c cs_sort_dv.c cs_sort_ff.c cs_sort_fi.c cs_sort_fv.c cs_sort_ii.c cs_sort_iv.c cs_sort_str.c cs_symeig.c multivector.c afni_environ.c cl2.c machdep.c afni_logger.c cl1.c powell_int.c powell_newuoa.c rhdd.c zfun.c svdlib.c )
set(EDT_OBJS  edt_addbrick.c edt_blur.c edt_buildmask.c edt_calcmask.c edt_checkargv.c edt_clust.c edt_clust2.c edt_clustalpha.c edt_clustarr.c edt_coerce.c edt_dsetitems.c edt_emptycopy.c edt_filtervol.c edt_floatize.c edt_fullcopy.c edt_geomcon.c edt_help.c edt_onedset.c edt_scl2max.c edt_sortmask.c edt_substbrick.c edt_volamax.c edt_volpad.c edt_wodcopy.c edt_zscore.c)
set(THD_OBJS thd_1Ddset.c thd_1Dtodset.c thd_3Ddset.c thd_Tcorr1D.c thd_analyzeread.c thd_atlas.c thd_atr.c thd_automask.c thd_autonudge.c thd_auxdata.c thd_avts.c thd_bandpass.c thd_brainormalize.c thd_bstats.c thd_center.c thd_checkidc.c thd_cliplevel.c thd_compress.c thd_conformist.c thd_coords.c thd_correlate.c thd_countb.c thd_ctfread.c thd_delete.c thd_despike_L1.c thd_detrend.c thd_dset_nbhd.c thd_dset_to_grayplot.c thd_dset_to_vectim.c thd_dsetatr.c thd_dsetdblk.c thd_dsetinsess.c thd_dsetinslist.c thd_dsetrow.c thd_dsetto1D.c thd_dsetto3D.c thd_dumdset.c thd_editdaxes.c thd_entropy16.c thd_fdbrick.c thd_fdrcurve.c thd_fdto1D.c thd_fdto2D.c thd_fetchdset.c thd_filestuff.c thd_fitter.c thd_floatscan.c thd_forcemalloc.c thd_get1D.c thd_getorient.c thd_getpathprogs.c thd_http.c thd_idcode.c thd_incorrelate.c thd_info.c thd_initalldir.c thd_initdblk.c thd_initdkptr.c thd_initprefix.c thd_initsess.c thd_instacorr.c thd_intlist.c thd_iochan.c thd_lasso.c thd_linecount.c thd_loaddblk.c thd_logafni.c thd_makefbuc.c thd_makefim.c thd_makefith.c thd_makemask.c thd_manydset.c thd_mastery.c thd_matdaxes.c thd_mattor.c thd_mean_dset.c thd_median.c thd_mincread.c thd_mincwrite.c thd_mismatch.c thd_mnicoords.c thd_mpegread.c thd_newprefix.c thd_niftiread.c thd_niftiwrite.c thd_niml.c thd_nimlatr.c thd_notes.c thd_opendset.c thd_openimage.c thd_opentcat.c thd_orient.c thd_outlier_count.c thd_purgedblk.c thd_read_vecmat.c thd_reconpar.c thd_remove_allzero.c thd_rot3d.c thd_rot3d_byte.c thd_rotangles.c thd_rowfillin.c thd_sarr.c thd_satcheck.c thd_selenium.c thd_shear3d.c thd_shift2.c thd_statpval.c thd_strfunc.c thd_svdblur.c thd_table.c thd_timeof.c thd_tmask.c thd_trusthost.c thd_tshift.c thd_ttatlas_query.c thd_ttest.c thd_vcheck.c thd_warp_tables.c thd_warps.c thd_winsor.c thd_writeatr.c thd_writedblk.c thd_writedset.c thd_zblock.c thd_zeropad.c thd_zfillin.c afni_vedit.c ktaub.c bilinear_warp3D.c)
set(MRI_OBJS mri_2dalign.c mri_3dalign.c mri_add_name.c mri_aff2d.c mri_align.c mri_allzero.c mri_blur3d.c mri_blur3d_variable.c mri_bport.c mri_cat2D.c mri_catvol.c mri_cfft.c mri_check.c mri_clusterize.c mri_colorsetup.c mri_complex_arith.c mri_copy.c mri_counter.c mri_coxplot.c mri_cut.c mri_dicom_hdr.c mri_dicom_stuff.c mri_drawing.c mri_dup.c mri_edit.c mri_entropy16.c mri_equal.c mri_extract.c mri_fdrize.c mri_fft_complex.c mri_filt_fft.c mri_flip3D.c mri_flippo.c mri_float_func.c mri_floatvec.c mri_free.c mri_fromstring.c mri_fwhm.c mri_genARMA11.c mri_genalign.c mri_genalign_util.c mri_get_cmass.c mri_histobyte.c mri_histog.c mri_histoshort.c mri_intedge.c mri_isgray.c mri_lsqfit.c mri_matrix.c mri_max.c mri_medianfilter.c mri_metrics.c mri_nbistats.c mri_new.c mri_nsize.c mri_nstats.c mri_nwarp.c mri_order.c mri_overlay.c mri_pcvector.c mri_percents.c mri_polyfit.c mri_purger.c mri_radial_random_field.c mri_rbfinterp.c mri_read.c mri_read_dicom.c mri_read_mpeg.c mri_read_stuff.c mri_render.c mri_rgba_compose.c mri_rota.c mri_scale.c mri_scaled_diff.c mri_scalize.c mri_sharpen3D.c mri_sharpness.c mri_shift2D.c mri_shifter.c mri_sobel.c mri_sort.c mri_stat_seq.c mri_stats.c mri_subset.c mri_swapbytes.c mri_symbolize.c mri_thresh.c mri_threshX.c mri_to_byte.c mri_to_complex.c mri_to_float.c mri_to_fvect.c mri_to_imarr.c mri_to_mri.c mri_to_pval.c mri_to_rgb.c mri_to_rgba.c mri_to_short.c mri_transpose.c mri_uncat2D.c mri_warp.c mri_warp3D.c mri_warp3D_align.c mri_warpfield.c mri_write.c mri_write_analyze.c mri_write_angif.c mri_zeropad.c csfft.c mcw_glob.c mcw_malloc.c cox_render.c debugtrace.c ge4_header.c rcmat.c dmat44.c )
set(MRIX_OBJS display.c imseq.c bbox.c xim.c xutil.c xutil_webber.c LiteClue.c)

add_library(mri "")
target_sources(mri PRIVATE ${MRI_OBJS} ${THD_OBJS} ${EDT_OBJS} ${CS_OBJS} ${SUMA_OBJS} ${MISC_OBJS} ${SOMEAFOBJS} 
   $<TARGET_OBJECTS:AFNI::niml> $<TARGET_OBJECTS:afsliceobjs> $<TARGET_OBJECTS:pcor_objs> 
   $<TARGET_OBJECTS:parser> rickr/r_new_resam_dset.c rickr/r_idisp.c rickr/r_misc.c)
set(MRIX_BROKEN 1)
if(MRIX_BROKEN)
  target_sources(mri PRIVATE ${MRIX_OBJS})
  set_target_properties(mri PROPERTIES PUBLIC_HEADER "imseq.h;display.h;xim.h;bbox.h;xutil.h")
  target_include_directories(mri PUBLIC $<TARGET_PROPERTY:AFNI::XmHTML,INTERFACE_INCLUDE_DIRECTORIES> )
  add_library(mrix ALIAS mri)
  target_link_libraries(mri PUBLIC ${X11_LIBRARIES})
# else()
#   add_library(mrix ${MRIX_OBJS})
#   add_afni_target_properties(mrix)
# set_target_properties(mrix PROPERTIES PUBLIC_HEADER "imseq.h;display.h;xim.h;bbox.h;xutil.h")
  # target_include_directories(mrix PUBLIC $<TARGET_PROPERTY:AFNI::XmHTML,INTERFACE_INCLUDE_DIRECTORIES> )
# target_link_libraries(mrix PUBLIC mri AFNI::coxplot libheaders AFNI::XmHTML ${X11_LIBRARIES})
endif()
add_afni_target_properties(mri)


# IMOBJS  = display.o imseq.o bbox.o xim.o xutil.o LiteClue.o xutil_webber.o 
# libmrix.a:$(IMOBJS) 

add_library(parser OBJECT parser.c parser_int.c)
add_library(plug_drealtime plug_realtime.c)
add_library(plug_maxima plug_maxima.c maxima.c)
add_library(plug_retroicor plug_retroicor.c retroicor.c)

foreach(target 1dAstrip 1dBandpass 1dBport 1dCorrelate 1dFlagMotion 1dMarry
		1dTsort 1dUpsample 1dcat 1ddot 1dfft 1dgenARMA11 1dnorm 1dsum
		1dsvd 1dtranspose 24swap 2dImReg 2perm 2swap 3dABoverlap
		3dAFNIto3D 3dAFNItoANALYZE 3dAFNItoMINC 3dAFNItoNIFTI
		3dAFNItoNIML 3dAFNItoRaw 3dANALYZEtoAFNI 3dAcost 3dAnatNudge
		3dAnhist 3dAttribute 3dAutoTcorrelate 3dAutobox 3dAutomask
		3dBandpass 3dBlurInMask 3dBlurToFWHM 3dBrickStat 3dCM
		3dClipLevel 3dClustCount 3dClustSim 3dConformist 3dConvolve
		3dCountSpikes 3dDFT 3dDTeig 3dDTtoDWI 3dDWItoDT
		3dDegreeCentrality 3dDespike 3dEmpty 3dEntropy 3dErrtsCormat
		3dExtractGroupInCorr 3dExtrema 3dFDR 3dFFT 3dFWHM 3dFWHMx
		3dFourier 3dFriedman 3dGetrow 3dGroupInCorr 3dIntracranial
		3dInvFMRI 3dKruskalWallis 3dLFCD 3dLRflip 3dLSS 3dLocalACF
		3dLocalBistat 3dLocalHistog 3dLocalPV 3dLocalSVD 3dLocalstat
		3dMINCtoAFNI 3dMannWhitney 3dMean 3dMedianFilter 3dMultiThresh
       3dNormalityTest 3dNotes 3dNwarpAdjust 3dNwarpApply
		3dNwarpCalc 3dNwarpCat 3dNwarpFuncs 3dNwarpXYZ 3dOverlap
		3dPeriodogram 3dPolyfit 3dPval 3dQwarp 3dREMLfit 3dROIstats
		3dRank 3dRankizer 3dRegAna 3dRetinoPhase 3dRowFillin
		3dSetupGroupInCorr 3dSharpen 3dSpatNorm 3dStatClust 3dSynthesize
       3dTRfix 3dTagalign 3dTcat 3dTcorr1D 3dTcorrMap
		3dTcorrelate 3dTfilter 3dThreetoRGB 3dTnorm 3dToutcount
		3dToyProg 3dTproject 3dTqual 3dTshift 3dTsmooth 3dTsort
		3dTsplit4D 3dTwotoComplex 3dUndump 3dUnifize 3dUniformize
		3dUpsample 3dWarp 3dWarpDrive 3dWavelets 3dWilcoxon 3dWinsor
		3dXClustSim 3dXYZcat 3dZcat 3dZcutup 3dZeropad 3dZregrid
		3daxialize 3dbuc2fim 3dbucket 3dcopy 3ddelay 3ddot 3ddup 3dfim+
		3dfractionize 3dinfo 3dmaskSVD 3dmask_tool 3dmaskave 3dmatmult
		3dnewid 3dnoise 3dnvals 3dpc 3drefit 3drename 3drotate
		3dsvm_linpredict 3dtoXdataset 3dttest++ 3dvolreg 4swap AlphaSim
		DTIStudioFibertoSegments FIRdesign GLTsymtest Ifile RSFgen
		Vecwarp abut adwarp afni_vcheck apsearch bitvec byteorder
		cat_matvec cdf count dicom_hdr dicom_hinfo dicom_to_raw ent16
		epsim exx fdrval fftest float_scan from3d ftosh ge_header
		help_format ibinom im2niml images_equal imand imaver imcalc
		imcat imcutup imdump immask imrotate imstack imstat imupsam
		mayo_analyze mritopgm mycat myget nicat niccc nidset nimel
		niml_feedme nimltest nsize plugout_drive plugout_ijk plugout_tt
		plugout_tta quotize rmz rotcom rtfeedme sfim siemens_vision
		sqwave stimband strblast test_powell testcox tfim uncomment
		uniq_images unu warping_test whereami whirlgif
)
	add_executable(${target} ${target}.c)
	target_link_libraries(${target} PUBLIC mrix)
endforeach(target)

foreach(i "" 2 3)
	set(target 3dANOVA${i}) 
	add_executable(${target} ${target}.c 3dANOVA.lib)
	target_link_libraries(${target} PUBLIC mrix)
endforeach(i)

foreach(target 1dNLfit 1deval 1dmatcalc 3dDeconvolve 3dDetrend 3dTstat 3dcalc
		3dclust 3dhistog 3dmaskdump 3dmatcalc 3dmerge 3dproject 3dttest
		ccalc waver
)
	add_executable(${target} ${target}.c $<TARGET_OBJECTS:parser>)
	target_link_libraries(${target} PUBLIC mrix)
endforeach(target)
target_link_libraries(3dproject PUBLIC AFNI::volpack)


add_executable(3dTfitter 3dTfitter.c thd_fitter.c thd_lasso.c)
add_executable(3dfim 3dfim.c ts.c "${CMAKE_CURRENT_BINARY_DIR}/configured_pcor/afni_pcor_float.c")
add_executable(FD2 FD2.c ts.c mcw.c overfim.c $<TARGET_OBJECTS:pcorsh> csfft_AJ.c)
add_executable(fim2 fim2.c ts.c $<TARGET_OBJECTS:pcor>)
add_executable(imreg imreg.c $<TARGET_OBJECTS:pcor>)
add_executable(to3d to3d.c)
add_executable(3danisosmooth 3danisosmooth.c DWIstructtensor.c)
add_executable(1dSEM 1dSEM.c sqrmat.c matrix.c)
add_executable(3dNLfim 3dNLfim.c)
add_executable(gwarp mri_gwarp.c)
add_executable(3dmaxima 3dmaxima.c maxima.c)
add_executable(3dDeconvolve_n 3dDeconvolve.c $<TARGET_OBJECTS:parser>)
add_executable(3dDeconvolve_f 3dDeconvolve.c $<TARGET_OBJECTS:parser>)
add_executable(3dAllineate 3dAllineate.c mri_genalign.c mri_genalign_util.c thd_correlate.c)
add_executable(3dECM 3dECM.c sparse_array.c)
add_executable(3dMSE 3dMSE.c sparse_array.c)
add_executable(1dgrayplot 1dgrayplot.c display.c)
add_executable(1dplot 1dplot.c display.c)
add_executable(3dretroicor 3dretroicor.c retroicor.c)
# add_executable(3dedge3 3dedge3.c)


set_source_files_properties(3dDeconvolve_f PROPERTIES COMPILE_DEFINITIONS "FLOATIZE")

### target_include_directories(afni_warp PUBLIC)
##target_include_directories(3dDeconvolve PUBLIC)
##target_include_directories(3dDeconvolve_f PUBLIC)
##target_include_directories(3dMSE PUBLIC)
target_include_directories(3dretroicor PUBLIC $<TARGET_PROPERTY:NIFTI::nifti2,INTERFACE_INCLUDE_DIRECTORIES>)
##target_include_directories(3DEdge PUBLIC)

#target_include_directories(coxplot PUBLIC)

target_include_directories(mri 
  PUBLIC ${3DEDGE_INCLUDE_DIRS} 
  $<TARGET_PROPERTY:AFNI::coxplot,INTERFACE_INCLUDE_DIRECTORIES>
  $<TARGET_PROPERTY:GIFTI::giftiio,INTERFACE_INCLUDE_DIRECTORIES>
   )

target_include_directories(parser PUBLIC $<TARGET_PROPERTY:AFNI::niml,INTERFACE_INCLUDE_DIRECTORIES> )
target_include_directories(to3d PUBLIC ${MOTIF_INCLUDE_DIR} )
target_include_directories(1dplot PUBLIC  ${XT_INCLUDE_DIR} ${X11_INCLUDE_DIRS} )

foreach(target 3dfim 3dTfitter FD2 fim2 imreg gwarp 3danisosmooth 3dmaxima
       3dAllineate 3dECM 3dMSE 1dSEM 
		plug_drealtime plug_maxima plug_retroicor
)

   target_link_libraries(${target} PUBLIC mrix NIFTI::nifticdf)
endforeach(target)
target_link_libraries(3dretroicor PUBLIC mrix AFNI::coxplot AFNI::converted_from_fortran)
target_link_libraries(3dNLfim PUBLIC mrix ${CMAKE_DL_LIBS})

target_link_libraries(parser PUBLIC AFNI::coxplot libheaders NIFTI::nifti2)
target_link_libraries(1dgrayplot PUBLIC mri AFNI::coxplot ${X11_Xt_LIB})
target_link_libraries(1dplot PUBLIC mrix AFNI::eispack NIFTI::nifti2 ${X11_Xt_LIB} ${X11_LIBRARIES})
target_link_libraries(3dDeconvolve PUBLIC mri AFNI::coxplot)
target_link_libraries(3dDeconvolve_f PUBLIC mri AFNI::coxplot)
target_link_libraries(3dDeconvolve_n PUBLIC mri AFNI::coxplot)
target_link_libraries(Xphace PUBLIC mrix NIFTI::nifti2)
target_link_libraries(afni_open PUBLIC mri)
target_link_libraries(aiv PUBLIC mrix NIFTI::nifti2)
# target_include_directories(aiv $<TARGET_PROPERTY:NIFTI::nifti2,INTERFACE_INCLUDE_DIRECTORIES>)

target_link_libraries(mri PUBLIC netcdf m GIFTI::giftiio 3DEdge volpack
   AFNI::XmHTML AFNI::coxplot audio  AFNI::eispack ${X11_Xt_LIB} NIFTI::nifti2 NIFTI::nifticdf)

target_link_libraries(to3d PUBLIC mrix NIFTI::nifti2 ${MOTIF_LIBRARIES})

# target_link_libraries(3dedge3 PUBLIC mri)


# link remaining executables from rickr
add_executable(Dimon rickr/Dimon.c rickr/realtime.c)
add_executable(Dimon1 rickr/Dimon1.c rickr/realtime1.c)
add_executable(3dresample rickr/3dresample.c)
target_include_directories(3dresample PRIVATE $<TARGET_PROPERTY:AFNI::coxplot,INTERFACE_INCLUDE_DIRECTORIES>  $<TARGET_PROPERTY:NIFTI::nifti2,INTERFACE_INCLUDE_DIRECTORIES> )
target_link_libraries(3dresample mri libheaders audio AFNI::eispack)

target_link_libraries(Dimon1 PUBLIC mri)
target_link_libraries(Dimon PUBLIC mri)


add_executable(3dTSgen 3dTSgen.c)
## target_include_directories(3dTSgen# PUBLIC)
target_link_libraries(3dTSgen mri  AFNI::coxplot NIFTI::nifti2 ${CMAKE_DL_LIBS})



# export(TARGETS afni SUMA NAMESPACE AFNI:: FILE AFNITargets.cmake)
# export(PACKAGE AFNI)
# find_package(AFNI should work because of the above)

# check the AFNITargets file and make sure you don't have the include directory included twice if you want clean cmake files : https://cliutils.gitlab.io/modern-cmake/chapters/install/installing.html
# install(TARGETS afni afobj
#         EXPORT AFNITargets
#         LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
#         ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
#         RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
#         INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
#         )

# TODO: /afni/afni_codebase/src/Audio/@sound_to_header.csh;
include(CMakeLists_apps.txt)

# Install makefile programs that are built by this project.
execute_process(
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
     COMMAND bash "-c" "make -s -f Makefile.INCLUDE prog_list")
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/prog_list.txt ALL_PROGRAMS)
string(STRIP ${ALL_PROGRAMS} ALL_PROGRAMS)
string(REPLACE "\n" " "  ALL_PROGRAMS ${ALL_PROGRAMS})
separate_arguments(ALL_PROGRAMS)
set(PROGRAMS_BUILT "")
# message(all_progs:"${ALL_PROGRAMS}")
if(MRIX_BROKEN)
  set(installable_targets ${ALL_PROGRAMS} mri)
else()
  set(installable_targets ${ALL_PROGRAMS} mri mrix)
endif()
foreach(program ${installable_targets})
  if(TARGET ${program})
  list(APPEND PROGRAMS_BUILT ${program})
  endif()
endforeach()
message("progs_built:'${PROGRAMS_BUILT}'")
install(TARGETS ${PROGRAMS_BUILT}
       RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
       LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
       ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
       PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)  

# # First, make an install targets file (very similar to the one you made in the build directory):
# install(EXPORT AFNITargets
#         FILE AFNITargets.cmake
#         NAMESPACE AFNI::
#         DESTINATION lib/cmake/AFNI
#          )