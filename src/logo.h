/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _AFNI_LOGO_HEADER_
#define _AFNI_LOGO_HEADER_

#ifndef MAIN
extern Pixmap logo_pixmap ;

extern int    afni48_good ;
extern Pixmap afni48_pixmap ;
extern Pixmap afni48cor_pixmap ;
extern Pixmap afni48sag_pixmap ;
extern Pixmap afni48axi_pixmap ;
extern Pixmap afni48gra_pixmap ;
extern Pixmap afni48gracor_pixmap ;
extern Pixmap afni48grasag_pixmap ;
extern Pixmap afni48graaxi_pixmap ;
extern Pixmap afni16_pixmap[26] ;
#else
Pixmap logo_pixmap      = XmUNSPECIFIED_PIXMAP ;

int    afni48_good      = 0 ;
Pixmap afni48_pixmap    = XmUNSPECIFIED_PIXMAP ;
Pixmap afni48cor_pixmap = XmUNSPECIFIED_PIXMAP ;
Pixmap afni48sag_pixmap = XmUNSPECIFIED_PIXMAP ;
Pixmap afni48axi_pixmap = XmUNSPECIFIED_PIXMAP ;

Pixmap afni48gra_pixmap    = XmUNSPECIFIED_PIXMAP ;
Pixmap afni48gracor_pixmap = XmUNSPECIFIED_PIXMAP ;
Pixmap afni48grasag_pixmap = XmUNSPECIFIED_PIXMAP ;
Pixmap afni48graaxi_pixmap = XmUNSPECIFIED_PIXMAP ;

Pixmap afni16_pixmap[26]   = {     /* 28 Jan 2004 */
    XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP ,
    XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP ,
    XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP ,
    XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP ,
    XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP ,
    XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP ,
    XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP ,
    XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP ,
    XmUNSPECIFIED_PIXMAP , XmUNSPECIFIED_PIXMAP
} ;
#endif  /* MAIN */

#undef  USE_MCWLOGO
#define USE_RWCLOGO    /* chooses the logo to use */
#undef  USE_NIHLOGO

#endif /* _AFNI_LOGO_HEADER_ */
