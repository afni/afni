#ifndef _AFNI_RENDER_HEADER_
#define _AFNI_RENDER_HEADER_

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "volpack.h"

/*============================================================================
  AREN = AFNI Renderer, a set of routines for volume rendering 3D bricks.
  Built on top of VolPack, by Philippe Lacroute.
==============================================================================*/

/*--- prototypes ---*/

extern void * new_AREN_renderer(void) ;
extern void   destroy_AREN_renderer( void * ) ;

extern int  AREN_set_graybrick( void * , MRI_IMAGE * ) ;
extern int  AREN_set_opabrick ( void * , MRI_IMAGE * ) ;
extern int  AREN_set_rgbbricks( void * , MRI_IMAGE * , MRI_IMAGE * , MRI_IMAGE * ) ;
extern void AREN_set_viewpoint( void * , float , float ) ;
extern void AREN_set_size     ( void * , float , float , float ) ;

extern MRI_IMAGE * AREN_render( void * , int ) ;

extern void   AREN_be_verbose( void * ) ;
extern void   AREN_be_quiet  ( void * ) ;

/*--- typedefs, etc. ---*/

typedef struct {
   unsigned short rgb ;     /* packed into 15 bits */
   unsigned short alpha ;   /* opacity */
} rgbvox ;

#define EIGHT_TO_FIVE(z) ((z) >> 3)        /* maps 0..255 into 0..31  */
#define FIVE_TO_EIGHT(z) ((z) << 3)        /* maps 0..31  into 0..255 */

#define FIVE_to_short(a,b,c) ((a)<<10 | (b)<<5 | (c))

#define TFSINV 0.00390625
#define TTINV  0.03125
#define TFFINV (1.0/255.0)
#define TOINV  (1.0/31.0)

#define SHORT_to_rrr(s) TT_TO_ZO( ((s) & 0x7c00) >> 10 )
#define SHORT_to_ggg(s) TT_TO_ZO( ((s) & 0x03e0) >>  5 )
#define SHORT_to_bbb(s) TT_TO_ZO( ((s) & 0x001f)       )

#define SHORT_isgray(s) ( ( ((s) & 0x7c00) >> 10 == ((s) & 0x03e0) >> 5 ) &&  \
                          ( ((s) & 0x03e0) >>  5 == ((s) & 0x001f)      )    )

#define MAX_CDIM   32
#define MAX_GRAYS  256
#define MAX_COLORS 32768                   /* = 32 * 32 * 32 */
#define TOT_COLORS (MAX_COLORS+MAX_GRAYS)

extern void init_AREN_colortable(void) ;

/*---------------------------*/

#define AREN_TYPE 9707312

typedef struct {
   int type ;

   vpContext * vpc ;

   int nx,ny,nz , verbose , newdata,newvox ;
   float theta,phi , sx,sy,sz ;

   MRI_IMAGE * grim , * rim , * gim , * bim , * opim ;  /* inputs */

   rgbvox * vox ;   /* encoded color + opacity */
} AREN_stuff ;

#define ISVALID_AREN(ah) ( (ah) != NULL && (ah)->type == AREN_TYPE )

#endif /* _AFNI_RENDER_HEADER_ */
