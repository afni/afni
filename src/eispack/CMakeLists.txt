set(SVD_SLOW "${CMAKE_CURRENT_BINARY_DIR}/configured_eispack/eis_svd_slow.c")
configure_file(eis_svd.c ${SVD_SLOW} COPYONLY)
set_source_files_properties(${SVD_SLOW} PROPERTIES COMPILE_DEFINITIONS "FUNCNAME=svd_slow_")
add_library(eispack eis_bakvec.c eis_balanc.c eis_balbak.c eis_bandr.c eis_bandv.c eis_bisect.c
eis_bqr.c eis_cbabk2.c eis_cbal.c eis_cdiv.c eis_cg.c eis_ch.c eis_cinvit.c
eis_combak.c eis_comhes.c eis_comlr.c eis_comlr2.c eis_comqr.c eis_comqr2.c
eis_cortb.c eis_corth.c eis_csroot.c eis_elmbak.c eis_elmhes.c eis_eltran.c
eis_epslon.c eis_figi.c eis_figi2.c eis_hqr.c eis_hqr2.c eis_htrib3.c
eis_htribk.c eis_htrid3.c eis_htridi.c eis_imtql1.c eis_imtql2.c eis_imtqlv.c
eis_invit.c eis_minfit.c eis_ortbak.c eis_orthes.c eis_ortran.c eis_pythag.c
eis_qzhes.c eis_qzit.c eis_qzval.c eis_qzvec.c eis_ratqr.c eis_rebak.c
eis_rebakb.c eis_reduc.c eis_reduc2.c eis_rg.c eis_rgg.c eis_rs.c eis_rsb.c
eis_rsg.c eis_rsgab.c eis_rsgba.c eis_rsm.c eis_rsp.c eis_rst.c eis_rt.c
eis_svd.c eis_tinvit.c eis_tql1.c eis_tql2.c eis_tqlrat.c eis_trbak1.c
eis_trbak3.c eis_tred1.c eis_tred2.c eis_tred3.c eis_tridib.c eis_tsturm.c
eis_svd.c ${SVD_SLOW})
add_afni_target_properties(eispack)
target_link_libraries(eispack AFNI::converted_from_fortran)

install(TARGETS eispack
  EXPORT AFNITargets
  RUNTIME DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
  LIBRARY DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
  ARCHIVE DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
  PUBLIC_HEADER DESTINATION ${AFNI_INSTALL_INCLUDE_DIR}) 
add_library(AFNI::eispack ALIAS eispack)