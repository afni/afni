# Unix Makefile for eispack

EIS_OBJS = eis_bakvec.o eis_balanc.o eis_balbak.o eis_bandr.o              \
           eis_bandv.o eis_bisect.o eis_bqr.o eis_cbabk2.o eis_cbal.o      \
           eis_cdiv.o eis_cg.o eis_ch.o eis_cinvit.o eis_combak.o          \
           eis_comhes.o eis_comlr.o eis_comlr2.o eis_comqr.o eis_comqr2.o  \
           eis_cortb.o eis_corth.o eis_csroot.o eis_elmbak.o eis_elmhes.o  \
           eis_eltran.o eis_epslon.o eis_figi.o eis_figi2.o eis_hqr.o      \
           eis_hqr2.o eis_htrib3.o eis_htribk.o eis_htrid3.o eis_htridi.o  \
           eis_imtql1.o eis_imtql2.o eis_imtqlv.o eis_invit.o eis_minfit.o \
           eis_ortbak.o eis_orthes.o eis_ortran.o eis_pythag.o eis_qzhes.o \
           eis_qzit.o eis_qzval.o eis_qzvec.o eis_ratqr.o eis_rebak.o      \
           eis_rebakb.o eis_reduc.o eis_reduc2.o eis_rg.o eis_rgg.o        \
           eis_rs.o eis_rsb.o eis_rsg.o eis_rsgab.o eis_rsgba.o            \
           eis_rsm.o eis_rsp.o eis_rst.o eis_rt.o eis_svd.o                \
           eis_tinvit.o eis_tql1.o eis_tql2.o eis_tqlrat.o eis_trbak1.o    \
           eis_trbak3.o eis_tred1.o eis_tred2.o eis_tred3.o                \
           eis_tridib.o eis_tsturm.o

all:$(EIS_OBJS)

.c.o:
	$(CC) -c $<

clean:
	$(RM) *.o
