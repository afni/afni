/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _MRI_RENDER_HEADER_
#define _MRI_RENDER_HEADER_

#include "mrilib.h"
#include "volpack.h"

/*============================================================================
  MREN = MRI Renderer, a set of routines for volume rendering 3D bricks.
  Built on top of VolPack, by Philippe Lacroute.
==============================================================================*/

/*--- prototypes ---*/

extern void * new_MREN_renderer(void) ;
extern void   destroy_MREN_renderer( void * ) ;

extern int  MREN_set_graybytes( void * , MRI_IMAGE * ) ;
extern int  MREN_set_opabytes ( void * , MRI_IMAGE * ) ;
extern int  MREN_set_rgbbytes ( void * , MRI_IMAGE * ) ;
extern int  MREN_set_rgbshorts( void * , MRI_IMAGE * ) ;
extern void MREN_set_viewpoint( void * , float , float , float ) ;
extern void MREN_set_size     ( void * , float , float , float ) ;

extern void MREN_depth_cue( void * , int ) ; /* 11 Sep 2001 */

extern void MREN_set_precalculation( void * ah , int ) ;

extern MRI_IMAGE * MREN_render( void * , int ) ;

extern int MREN_needs_data( void * ah ) ;

extern void   MREN_be_verbose( void * ) ;
extern void   MREN_be_quiet  ( void * ) ;

extern void MREN_set_rgbmap( void * , int , byte * , byte * , byte * ) ;
extern void MREN_unset_rgbmap( void * ) ;

extern MRI_IMAGE * MREN_rgb_to_cmap( MRI_IMAGE * ) ;

extern void MREN_set_min_opacity( void * , float ) ;

/*--- typedefs, etc. ---*/

typedef struct {
   unsigned short rgb ;     /* packed into 15 bits */
   unsigned short alpha ;   /* opacity */
} rgbvox ;

#define EIGHT_TO_FIVE(z) ((z) >> 3)        /* maps 0..255 into 0..31  */
#define FIVE_TO_EIGHT(z) ((z) << 3)        /* maps 0..31  into 0..255 */

#define FIVE_TO_SHORT(a,b,c) ((a)<<10 | (b)<<5 | (c))
#define EIGHT_TO_SHORT(a)    (MREN_MAX_COLORS + (a))

#define TFSINV 0.00390625
#define TTINV  0.03125
#define TFFINV (1.0/255.0)
#define TOINV  (1.0/31.0)

#define SHORT_to_rrr(s) TT_TO_ZO( ((s) & 0x7c00) >> 10 )
#define SHORT_to_ggg(s) TT_TO_ZO( ((s) & 0x03e0) >>  5 )
#define SHORT_to_bbb(s) TT_TO_ZO( ((s) & 0x001f)       )

#define SHORT_isgray(s) ( ( ((s) & 0x7c00) >> 10 == ((s) & 0x03e0) >> 5 ) &&  \
                          ( ((s) & 0x03e0) >>  5 == ((s) & 0x001f)      )    )

#define MREN_MAX_CDIM   32
#define MREN_MAX_GRAYS  256
#define MREN_MAX_COLORS 32768                   /* = 32 * 32 * 32 */
#define TOT_COLORS (MREN_MAX_COLORS+MREN_MAX_GRAYS)

extern void init_MREN_colortable(void) ;

/*---------------------------*/

#define MREN_TYPE 9707312

#define PMODE_LOW    0
#define PMODE_MEDIUM 1
#define PMODE_HIGH   2

typedef struct {
   int type ;

   vpContext * vpc ;

   int nx,ny,nz , verbose , newopac,newvox , pmode ;
   float theta,phi,psi , sx,sy,sz ;

   int grayset , rgbset , opaset ;

   MRI_IMAGE * opim , * shim ;  /* opacity and shading */

   rgbvox * vox ;   /* encoded color + opacity */

   int     ncmap , newcmap ;
   float * cmap ;
   float   min_opacity ;
} MREN_stuff ;

#define ISVALID_MREN(ah) ( (ah) != NULL && (ah)->type == MREN_TYPE )

#endif /* _MRI_RENDER_HEADER_ */
