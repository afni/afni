/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#ifndef _COX_RENDER_HEADER_
#define _COX_RENDER_HEADER_

#include "mrilib.h"

/*============================================================================
  CREN = COX Renderer, a set of routines for volume rendering 3D bricks.
==============================================================================*/

/*--- prototypes ---*/

extern void * new_CREN_renderer(void) ;
extern void   destroy_CREN_renderer( void * ) ;

extern void        CREN_set_viewpoint  ( void *, int,float,int,float,int,float );
extern int         CREN_set_databytes  ( void *, MRI_IMAGE * ) ;
extern int         CREN_needs_data     ( void * ) ;
extern void        CREN_set_min_opacity( void *, float ) ;
extern void        CREN_set_rgbmap     ( void *, int, byte *, byte *, byte * ) ;
extern void        CREN_set_opamap     ( void *, float * , float ) ;
extern void        CREN_set_render_mode( void *, int ) ;
extern MRI_IMAGE * CREN_render         ( void * ) ;

/*---------------------------*/

#define CREN_TYPE 9808423

#define CREN_SUM_VOX   0
#define CREN_MIP_VOX   1
#define CREN_MIP_OPA   2

#define CREN_LAST_MODE 2

typedef struct {
   int type ;

   int   nx,ny,nz ;
   float dx,dy,dz ;

   byte * vox ;
   Tmask * vtm ;

   int nrgb ;
   byte rmap[128] , gmap[128] , bmap[128] , imap[128] ;
   float opamap[128] , opargb , min_opacity ;

   int   ax1,ax2,ax3 ; /* to be compatible with plug_render.c: */
   float th1,th2,th3 ; /* ax1=A ax2=R ax3=I, th1=yaw th2=pitch th3=roll */

   int newvox , newopa , newangles ;
   int renmode ;
} CREN_stuff ;

#define ISVALID_CREN(ah) ( (ah) != NULL && (ah)->type == CREN_TYPE )

#endif /* _COX_RENDER_HEADER_ */
