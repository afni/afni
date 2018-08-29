PROJECT(coxplot)

SET(SOURCES
 color.c curve.c frame.c frstpt.c labmod.c memplt.c
 line.c perim.c periml.c phdot.c phline.c point.c
 points.c pwrit.c pwritf.c set.c setdsh.c
 setfrm.c setlin.c setw.c srface.c tick4.c vector.c
 zzaxxx.c zzaxyy.c zzchar.c zzclip.c zzlabl.c zzlgin.c
 zzline.c zzlinx.c zzliny.c zzlogx.c zzlogy.c
 zzperi.c zzphph.c zzphys.c
 ppak_perim.c ppak_pwrit.c ppak_pwritf.c ppak_srface.c
 plot_cox.c plot_ps.c plot_ps2.c plot_x11.c
 plot_motif.c plot_ts.c ppak_commons.c plot_strip.c
 find_color_name.c
)

ADD_LIBRARY(coxplot STATIC ${SOURCES})
SET_TARGET_PROPERTIES(coxplot PROPERTIES POSITION_INDEPENDENT_CODE TRUE)
