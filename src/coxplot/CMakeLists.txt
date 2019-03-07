include(FindMotif)

set(COXPLOT_SRCS  color.c curve.c find_color_name.c frame.c frstpt.c labmod.c
line.c memplt.c perim.c periml.c phdot.c phline.c plot_cox.c plot_motif.c
plot_ps.c plot_ps2.c plot_strip.c plot_ts.c plot_x11.c plotpak.inc point.c
points.c ppak_commons.c ppak_perim.c ppak_pwrit.c ppak_pwritf.c ppak_srface.c
pwrit.c pwritf.c set.c setdsh.c setfrm.c setlin.c setw.c srface.c tick4.c
vector.c zzaxxx.c zzaxyy.c zzchar.c zzclip.c zzlabl.c zzlgin.c zzline.c
zzlinx.c zzliny.c zzlogx.c zzlogy.c zzperi.c zzphph.c zzphys.c 
)

add_library(coxplot ${COXPLOT_SRCS})
target_include_directories(coxplot 
  PRIVATE ${MOTIF_INCLUDE_DIR} 
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
  $<TARGET_PROPERTY:AFNI::converted_from_fortran,INTERFACE_INCLUDE_DIRECTORIES>)
set_target_properties(coxplot PROPERTIES
	PUBLIC_HEADER "coxplot.h;../machdep.h"
)
add_afni_target_properties(coxplot)
target_link_libraries(coxplot m AFNI::converted_from_fortran ${MOTIF_LIBRARIES} ${X11_LIBRARIES} libheaders)


install(TARGETS coxplot
	EXPORT AFNITargets
	RUNTIME DESTINATION "${AFNI_INSTALL_RUNTIME_DIR}" COMPONENT bin
	LIBRARY DESTINATION "${AFNI_INSTALL_LIBRARY_DIR}" COMPONENT shlib
	ARCHIVE DESTINATION "${AFNI_INSTALL_LIBRARY_DIR}"
	PUBLIC_HEADER DESTINATION "${AFNI_INSTALL_INCLUDE_DIR}"
	COMPONENT dev
)
add_library(AFNI::coxplot ALIAS coxplot)