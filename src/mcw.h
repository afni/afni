#ifndef _MCW_LOGO_HEADER_
#define _MCW_LOGO_HEADER_

#ifndef MAIN
extern Pixmap mcw_pixmap ;
extern Pixmap rwc_pixmap ;

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
Pixmap mcw_pixmap       = XmUNSPECIFIED_PIXMAP ;
Pixmap rwc_pixmap       = XmUNSPECIFIED_PIXMAP ;

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

#ifdef WANT_MCW_BITMAP
#include "mcw.xbm"
#endif /* WANT_MCW_BITMAP */

#ifdef WANT_RWC_BITMAP
#include "rwc.xbm"
#endif /* WANT_RWC_BITMAP */

#ifdef WANT_AFNI_BITMAP
#include "afni48.xbm"
#include "afni48cor.xbm"
#include "afni48axi.xbm"
#include "afni48sag.xbm"
#include "afni48gra.xbm"
#include "afni48gracor.xbm"
#include "afni48grasag.xbm"
#include "afni48graaxi.xbm"
#endif /* WANT_AFNI_BITMAP */

#endif /* _MCW_LOGO_HEADER_ */
