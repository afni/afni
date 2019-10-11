add_subdirectory(coxplot)
add_subdirectory(f2cdir)
add_subdirectory(niml)
add_subdirectory(eispack)
add_subdirectory(Audio)
add_subdirectory(volpack)
add_subdirectory(3DEdge)

add_library(libheaders INTERFACE)
set_source_files_properties(debugtrace.h PROPERTIES COMPILE_DEFINITIONS "SHOWOFF")
target_sources(libheaders INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/3ddata.h ${CMAKE_CURRENT_SOURCE_DIR}/AFNI_label.h ${CMAKE_CURRENT_SOURCE_DIR}/Amalloc.h ${CMAKE_CURRENT_SOURCE_DIR}/afni_environ.h ${CMAKE_CURRENT_SOURCE_DIR}/afni_suma.h
  ${CMAKE_CURRENT_SOURCE_DIR}/afni_warp.h ${CMAKE_CURRENT_SOURCE_DIR}/cs.h ${CMAKE_CURRENT_SOURCE_DIR}/cs_qsort_small.h ${CMAKE_CURRENT_SOURCE_DIR}/cs_sort_template.h
  ${CMAKE_CURRENT_SOURCE_DIR}/debugtrace.h ${CMAKE_CURRENT_SOURCE_DIR}/editvol.h ${CMAKE_CURRENT_SOURCE_DIR}/eispack.h ${CMAKE_CURRENT_SOURCE_DIR}/killer.h ${CMAKE_CURRENT_SOURCE_DIR}/list_struct.h ${CMAKE_CURRENT_SOURCE_DIR}/machdep.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mcw_glob.h ${CMAKE_CURRENT_SOURCE_DIR}/mcw_malloc.h ${CMAKE_CURRENT_SOURCE_DIR}/mri_dicom_stuff.h ${CMAKE_CURRENT_SOURCE_DIR}/mri_warpfield.h ${CMAKE_CURRENT_SOURCE_DIR}/mrilib.h
  ${CMAKE_CURRENT_SOURCE_DIR}/multivector.h  ${CMAKE_CURRENT_SOURCE_DIR}/tagset.h ${CMAKE_CURRENT_SOURCE_DIR}/thd_atlas.h ${CMAKE_CURRENT_SOURCE_DIR}/thd_compress.h ${CMAKE_CURRENT_SOURCE_DIR}/thd_iochan.h
  ${CMAKE_CURRENT_SOURCE_DIR}/thd_maker.h ${CMAKE_CURRENT_SOURCE_DIR}/thd_ttatlas_CA_EZ.h ${CMAKE_CURRENT_SOURCE_DIR}/thd_ttatlas_query.h ${CMAKE_CURRENT_SOURCE_DIR}/vecmat.h ${CMAKE_CURRENT_SOURCE_DIR}/vol2surf.h
  ${AFNI_VERSION_HEADER} ${CMAKE_CURRENT_SOURCE_DIR}/r_new_resam_dset.h ${CMAKE_CURRENT_SOURCE_DIR}/r_idisp.h ${CMAKE_CURRENT_SOURCE_DIR}/ge4_header.h
  ${CMAKE_CURRENT_SOURCE_DIR}/afni.h
  )
target_include_directories(libheaders 
  INTERFACE
  ${CMAKE_CURRENT_SOURCE_DIR} 
  ${CMAKE_CURRENT_BINARY_DIR} 
  $<TARGET_PROPERTY:NIFTI::nifti2,INTERFACE_INCLUDE_DIRECTORIES>
  $<TARGET_PROPERTY:AFNI::f2cdir,INTERFACE_INCLUDE_DIRECTORIES>
  $<TARGET_PROPERTY:AFNI::niml,INTERFACE_INCLUDE_DIRECTORIES>
  $<TARGET_PROPERTY:AFNI::coxplot,INTERFACE_INCLUDE_DIRECTORIES>
  ${X11_Xt_INCLUDE_PATH}
  ${MOTIF_INCLUDE_DIR}
  )

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
target_include_directories(pcor_objs PRIVATE 
  ${PCOR_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}
  $<TARGET_PROPERTY:NIFTI::nifti2,INTERFACE_INCLUDE_DIRECTORIES>
  $<TARGET_PROPERTY:AFNI::niml,INTERFACE_INCLUDE_DIRECTORIES>
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


set_source_files_properties(thd_notes.c thd_vcheck.c PROPERTIES COMPILE_DEFINITIONS "SHOWOFF")
set_os_flags(thd_filestuff.c)
set_os_flags(machdep.c)
set(SOMEAFOBJS afni_warp.c pbar_color_defs.c afni_ports.c)
set(MISC_OBJS afni_suma.c vol2surf.c list_struct.c nifti_statlib.c matrix.c gifti_choice.c misc_math.c)
set(SUMA_OBJS  suma_afni_surface.c suma_algorithms.c suma_datasets.c suma_help.c suma_niml.c suma_string_manip.c suma_utils.c)
set(CS_OBJS cs_addto_args.c cs_fgets.c cs_gamfit.c cs_laguerre.c cs_misc.c cs_playsound.c cs_pv.c cs_qhull.c cs_qmed.c cs_sort_d.c cs_sort_di.c cs_sort_dv.c cs_sort_ff.c cs_sort_fi.c cs_sort_fv.c cs_sort_ii.c cs_sort_iv.c cs_sort_str.c cs_symeig.c multivector.c afni_environ.c cl2.c machdep.c afni_logger.c cl1.c powell_int.c powell_newuoa.c rhdd.c zfun.c svdlib.c )
set(EDT_OBJS  edt_addbrick.c edt_blur.c edt_buildmask.c edt_calcmask.c edt_checkargv.c edt_clust.c edt_clust2.c edt_clustalpha.c edt_clustarr.c edt_coerce.c edt_dsetitems.c edt_emptycopy.c edt_filtervol.c edt_floatize.c edt_fullcopy.c edt_geomcon.c edt_help.c edt_onedset.c edt_scl2max.c edt_sortmask.c edt_substbrick.c edt_volamax.c edt_volpad.c edt_wodcopy.c edt_zscore.c)
set(THD_OBJS thd_1Ddset.c thd_1Dtodset.c thd_3Ddset.c thd_Tcorr1D.c thd_analyzeread.c thd_atlas.c thd_atr.c thd_automask.c thd_autonudge.c thd_auxdata.c thd_avts.c thd_bandpass.c thd_brainormalize.c thd_bstats.c thd_center.c thd_checkidc.c thd_cliplevel.c thd_compress.c thd_conformist.c thd_coords.c thd_correlate.c thd_countb.c thd_ctfread.c thd_delete.c thd_despike_L1.c thd_detrend.c thd_dset_nbhd.c thd_dset_to_grayplot.c thd_dset_to_vectim.c thd_dsetatr.c thd_dsetdblk.c thd_dsetinsess.c thd_dsetinslist.c thd_dsetrow.c thd_dsetto1D.c thd_dsetto3D.c thd_dumdset.c thd_editdaxes.c thd_entropy16.c thd_fdbrick.c thd_fdrcurve.c thd_fdto1D.c thd_fdto2D.c thd_fetchdset.c thd_filestuff.c thd_fitter.c thd_floatscan.c thd_forcemalloc.c thd_get1D.c thd_getorient.c thd_getpathprogs.c thd_http.c thd_idcode.c thd_incorrelate.c thd_info.c thd_initalldir.c thd_initdblk.c thd_initdkptr.c thd_initprefix.c thd_initsess.c thd_instacorr.c thd_intlist.c thd_iochan.c thd_lasso.c thd_linecount.c thd_loaddblk.c thd_logafni.c thd_makefbuc.c thd_makefim.c thd_makefith.c thd_makemask.c thd_manydset.c thd_mastery.c thd_matdaxes.c thd_mattor.c thd_mean_dset.c thd_median.c thd_mincread.c thd_mincwrite.c thd_mismatch.c thd_mnicoords.c thd_mpegread.c thd_newprefix.c thd_niftiread.c thd_niftiwrite.c thd_niml.c thd_nimlatr.c thd_notes.c thd_opendset.c thd_openimage.c thd_opentcat.c thd_orient.c thd_outlier_count.c thd_purgedblk.c thd_read_vecmat.c thd_reconpar.c thd_remove_allzero.c thd_rot3d.c thd_rot3d_byte.c thd_rotangles.c thd_rowfillin.c thd_sarr.c thd_satcheck.c thd_selenium.c thd_shear3d.c thd_shift2.c thd_statpval.c thd_strfunc.c thd_svdblur.c thd_table.c thd_timeof.c thd_tmask.c thd_trusthost.c thd_tshift.c thd_ttatlas_query.c thd_ttest.c thd_vcheck.c thd_warp_tables.c thd_warps.c thd_winsor.c thd_writeatr.c thd_writedblk.c thd_writedset.c thd_zblock.c thd_zeropad.c thd_zfillin.c afni_vedit.c ktaub.c bilinear_warp3D.c)
set(MRI_OBJS mri_2dalign.c mri_3dalign.c mri_add_name.c mri_aff2d.c mri_align.c mri_allzero.c mri_blur3d.c mri_blur3d_variable.c mri_bport.c mri_cat2D.c mri_catvol.c mri_cfft.c mri_check.c mri_clusterize.c mri_colorsetup.c mri_complex_arith.c mri_copy.c mri_counter.c mri_coxplot.c mri_cut.c mri_dicom_hdr.c mri_dicom_stuff.c mri_drawing.c mri_dup.c mri_edit.c mri_entropy16.c mri_equal.c mri_extract.c mri_fdrize.c mri_fft_complex.c mri_filt_fft.c mri_flip3D.c mri_flippo.c mri_float_func.c mri_floatvec.c mri_free.c mri_fromstring.c mri_fwhm.c mri_genARMA11.c mri_genalign.c mri_genalign_util.c mri_get_cmass.c mri_histobyte.c mri_histog.c mri_histoshort.c mri_intedge.c mri_isgray.c mri_lsqfit.c mri_matrix.c mri_max.c mri_medianfilter.c mri_metrics.c mri_nbistats.c mri_new.c mri_nsize.c mri_nstats.c mri_nwarp.c mri_order.c mri_overlay.c mri_pcvector.c mri_percents.c mri_polyfit.c mri_purger.c mri_radial_random_field.c mri_rbfinterp.c mri_read.c mri_read_dicom.c mri_read_mpeg.c mri_read_stuff.c mri_render.c mri_rgba_compose.c mri_rota.c mri_scale.c mri_scaled_diff.c mri_scalize.c mri_sharpen3D.c mri_sharpness.c mri_shift2D.c mri_shifter.c mri_sobel.c mri_sort.c mri_stat_seq.c mri_stats.c mri_subset.c mri_swapbytes.c mri_symbolize.c mri_thresh.c mri_threshX.c mri_to_byte.c mri_to_complex.c mri_to_float.c mri_to_fvect.c mri_to_imarr.c mri_to_mri.c mri_to_pval.c mri_to_rgb.c mri_to_rgba.c mri_to_short.c mri_transpose.c mri_uncat2D.c mri_warp.c mri_warp3D.c mri_warp3D_align.c mri_warpfield.c mri_write.c mri_write_analyze.c mri_write_angif.c mri_zeropad.c csfft.c mcw_glob.c mcw_malloc.c cox_render.c debugtrace.c ge4_header.c rcmat.c dmat44.c )
add_library(mri "")
target_sources(mri PRIVATE ${SOMEAFOBJS} ${MISC_OBJS} ${SUMA_OBJS} ${CS_OBJS} ${EDT_OBJS} ${THD_OBJS} ${MRI_OBJS}
   $<TARGET_OBJECTS:AFNI::niml> $<TARGET_OBJECTS:afsliceobjs> $<TARGET_OBJECTS:pcor_objs> 
   $<TARGET_OBJECTS:parser> rickr/r_new_resam_dset.c rickr/r_idisp.c rickr/r_misc.c)
target_include_directories(mri 
  PUBLIC 
    # libheaders
    # $<TARGET_PROPERTY:libheaders,INTERFACE_INCLUDE_DIRECTORIES>
    $<TARGET_PROPERTY:NIFTI::nifti2,INTERFACE_INCLUDE_DIRECTORIES>
    $<TARGET_PROPERTY:AFNI::niml,INTERFACE_INCLUDE_DIRECTORIES>
  PRIVATE 
    ${3DEDGE_INCLUDE_DIRS} 
    $<TARGET_PROPERTY:coxplot,INTERFACE_INCLUDE_DIRECTORIES>
   )

target_compile_options(mri PRIVATE "-DREPLACE_XT" )
target_link_libraries(mri 
  PRIVATE  coxplot   audio volpack 3DEdge eispack ${NETCDF_LIBRARIES_C} NIFTI::nifticdf NIFTI::nifti2 m )
add_afni_target_properties(mri)


include(FindLibR)
set_source_files_properties(R_io.c PROPERTIES COMPILE_DEFINITIONS "std=gnu99")
add_library(rio R_io.c)
target_include_directories(rio 
  PRIVATE ${LIBR_INCLUDE_DIRS} 
  PUBLIC $<TARGET_PROPERTY:AFNI::niml,INTERFACE_INCLUDE_DIRECTORIES>)
target_link_libraries(rio ${LIBR_LIBRARIES} mri)
add_afni_target_properties(rio)


add_library(parser OBJECT parser.c parser_int.c)
target_include_directories(parser PRIVATE $<TARGET_PROPERTY:AFNI::niml,INTERFACE_INCLUDE_DIRECTORIES> )
target_link_libraries(parser 
  PUBLIC m
  PRIVATE  AFNI::coxplot libheaders NIFTI::nifti2
  )
# TODO: /afni/afni_codebase/src/Audio/@sound_to_header.csh;

if(NOT AFNI_BUILD_CORELIBS_ONLY)
  ########## Additional binaries and libraries
  # XXX remove dlcompat
  # XXX remove jpeg-6b
  add_subdirectory(crorden)
  add_subdirectory(qhulldir)
  add_subdirectory(svm)
  add_subdirectory(rickr)
  add_subdirectory(XmHTML)

  set(MRIX_OBJS display.c imseq.c bbox.c xim.c xutil.c xutil_webber.c LiteClue.c coxplot/plot_motif.c coxplot/plot_ts.c)
  add_library(mrix ${MRIX_OBJS})
  add_afni_target_properties(mrix)
  set_target_properties(mrix PROPERTIES PUBLIC_HEADER "imseq.h;display.h;xim.h;bbox.h;xutil.h")
  target_include_directories(mrix PUBLIC $<TARGET_PROPERTY:AFNI::XmHTML,INTERFACE_INCLUDE_DIRECTORIES> )
  target_link_libraries(mrix PRIVATE mri AFNI::coxplot  ${MOTIF_LIBRARIES} AFNI::XmHTML ${X11_LIBRARIES}  libheaders)
  add_afni_target_properties(mrix)
  add_library(AFNI::mrix ALIAS mrix)

  ## Install scripts
  add_subdirectory(scripts_install)

  ################# Plugins
  # Add_plugin targets
  include(CMakeLists_plugins.txt)

  ############ Apps
  include(CMakeLists_apps.txt)
  
  # Install many other binaries
  if (${BUILD_BINARIES})
  message(FATAL_ERROR "Not currently supported")
  # include(CMakeLists_binaries.txt)
  endif()
endif()



# Install makefile programs that are built by this project.
execute_process(
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
     COMMAND bash "-c" "make -s -f Makefile.INCLUDE prog_list_bin")
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/prog_list_bin.txt ALL_PROGRAMS)
string(STRIP ${ALL_PROGRAMS} ALL_PROGRAMS)
string(REPLACE "\n" " "  ALL_PROGRAMS ${ALL_PROGRAMS})
separate_arguments(ALL_PROGRAMS)
set(PROGRAMS_BUILT "")
set(NOT_BUILT "")
set(installable_targets ${ALL_PROGRAMS} mri rio)


# remove some programs that should not be here
foreach(other_project_target gifti_tool giftiio gifti_test nifti_tool niftiio)
message("Removing ${other_project_target} because it is not built by this project")
list(REMOVE_ITEM installable_targets ${other_project_target})
endforeach()

foreach(program ${installable_targets})
  if(TARGET ${program})
    list(APPEND PROGRAMS_BUILT ${program})
    add_afni_target_properties(${program})
  else()
    list(APPEND NOT_BUILT ${program})
  endif()
endforeach()
# message("progs_built:'${PROGRAMS_BUILT}'")
message("programs not yet built:'${NOT_BUILT}'")

message("programs  built:'${PROGRAMS_BUILT}'")
install(TARGETS ${PROGRAMS_BUILT}
       RUNTIME DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
       LIBRARY DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
       ARCHIVE DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
       PUBLIC_HEADER DESTINATION ${AFNI_INSTALL_INCLUDE_DIR}
)  

# # First, make an install targets file (very similar to the one you made in the build directory):
# install(EXPORT AFNITargets
#         FILE AFNITargets.cmake
#         NAMESPACE AFNI::
#         DESTINATION lib/cmake/AFNI
#          )


# export(TARGETS afni SUMA NAMESPACE AFNI:: FILE AFNITargets.cmake)
# export(PACKAGE AFNI)
# find_package(AFNI should work because of the above)

# check the AFNITargets file and make sure you don't have the include directory included twice if you want clean cmake files : https://cliutils.gitlab.io/modern-cmake/chapters/install/installing.html
# install(TARGETS afni afobj
#         EXPORT AFNITargets
#         LIBRARY DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
#         ARCHIVE DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
#         RUNTIME DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
#         INCLUDES DESTINATION ${AFNI_INSTALL_INCLUDE_DIR}
#         )
