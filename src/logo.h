/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _AFNI_LOGO_HEADER_
#define _AFNI_LOGO_HEADER_

#ifndef MAIN
extern Pixmap logo_pixmap ;
extern Pixmap sower_pixmap ;
extern int    sower_nx , sower_ny ;

extern int    afni48_good ;
extern Pixmap afni48_pixmap ;
extern Pixmap afni48cor_pixmap ;
extern Pixmap afni48sag_pixmap ;
extern Pixmap afni48axi_pixmap ;
extern Pixmap afni48gra_pixmap ;
extern Pixmap afni48gracor_pixmap ;
extern Pixmap afni48grasag_pixmap ;
extern Pixmap afni48graaxi_pixmap ;
#else
Pixmap logo_pixmap      = XmUNSPECIFIED_PIXMAP ;
Pixmap sower_pixmap     = XmUNSPECIFIED_PIXMAP ;
int    sower_nx         = 0 ;
int    sower_ny         = 0 ;

int    afni48_good      = 0 ;
Pixmap afni48_pixmap    = XmUNSPECIFIED_PIXMAP ;
Pixmap afni48cor_pixmap = XmUNSPECIFIED_PIXMAP ;
Pixmap afni48sag_pixmap = XmUNSPECIFIED_PIXMAP ;
Pixmap afni48axi_pixmap = XmUNSPECIFIED_PIXMAP ;

Pixmap afni48gra_pixmap    = XmUNSPECIFIED_PIXMAP ;
Pixmap afni48gracor_pixmap = XmUNSPECIFIED_PIXMAP ;
Pixmap afni48grasag_pixmap = XmUNSPECIFIED_PIXMAP ;
Pixmap afni48graaxi_pixmap = XmUNSPECIFIED_PIXMAP ;
#endif

#undef  USE_MCWLOGO
#define USE_RWCLOGO
#undef  USE_NIHLOGO

#ifdef WANT_LOGO_BITMAP
#ifdef USE_MCWLOGO
#  include "mcw.xbm"
#elif defined(USE_RWCLOGO)
#  include "rwc.xbm"
#else
#  include "nih.xbm"
#endif
#endif /* WANT_LOGO_BITMAP */

#ifdef WANT_AFNI_BITMAP
#  include "afni48.xbm"
#  include "afni48cor.xbm"
#  include "afni48axi.xbm"
#  include "afni48sag.xbm"
#  include "afni48gra.xbm"
#  include "afni48gracor.xbm"
#  include "afni48grasag.xbm"
#  include "afni48graaxi.xbm"
#endif /* WANT_AFNI_BITMAP */

#endif /* _AFNI_LOGO_HEADER_ */
