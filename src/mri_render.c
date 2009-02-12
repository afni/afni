/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mri_render.h"

/*============================================================================
  MREN = MRI Renderer, a set of routines for volume rendering 3D bricks.
  Built on top of VolPack, by Philippe Lacroute.

  (1) Use new_MREN_renderer() to create a renderer.
  (2) Load grayscale or color bricks into it with functions
      MREN_set_graybytes(), MREN_set_rgbbytes(), or MREN_set_rgbshorts.
  (3) Load the opacity brick with MREN_set_opabrick().
  (4) Set the viewing angles with MREN_set_viewpoint().
  (5) Create an image with MREN_render().
      Loop back through (2-4) as needed to create new images.
  (6) When finished, use destroy_MREN_renderer() to clean up.
==============================================================================*/

/*--------------------------------------------------------------------------
  Initialize the short -> float color table, and
             the byte  -> float gray/opacity tables for VolPack.

  N.B.: The documentation for VolPack is incorrect -- the color values
        should be in the range 0..255, NOT 0..1.  The opacity values
        should still be in the range 0..1.
----------------------------------------------------------------------------*/

static float * MREN_colorshorts = NULL ;
static float * MREN_graytable   = NULL ;
static float * MREN_opatable    = NULL ;
static float * MREN_colorbytes  = NULL ;

void init_MREN_colortable( void )
{
   int ii , rr,gg,bb , ss ;

   if( MREN_colorshorts != NULL ) return ;  /* been here already */

   MREN_colorshorts = (float *) malloc( sizeof(float) * TOT_COLORS * 3 ) ;
   MREN_graytable   = (float *) malloc( sizeof(float) * MREN_MAX_GRAYS ) ;
   MREN_opatable    = (float *) malloc( sizeof(float) * MREN_MAX_GRAYS ) ;
   MREN_colorbytes  = (float *) malloc( sizeof(float) * MREN_MAX_GRAYS * 3 ) ;

   /*-- load linear ramp for grayscale and opacity --*/

   for( ii=0 ; ii < MREN_MAX_GRAYS ; ii++ ){
      MREN_graytable[ii] = ii ;
      MREN_opatable[ii]  = ii / 255.0 ;
   }

   /*-- load a 32 x 32 x 32 color cube [indexed by unsigned shorts] --*/

   for( rr=0 ; rr < MREN_MAX_CDIM ; rr++ ){
      for( gg=0 ; gg < MREN_MAX_CDIM ; gg++ ){
         for( bb=0 ; bb < MREN_MAX_CDIM ; bb++ ){

            ss = FIVE_TO_SHORT(rr,gg,bb) ;   /* color index */

            MREN_colorshorts[3*ss  ] = (rr * 255.0) / 31.0 ;
            MREN_colorshorts[3*ss+1] = (gg * 255.0) / 31.0 ;
            MREN_colorshorts[3*ss+2] = (bb * 255.0) / 31.0 ;
         }
      }
   }

   /*-- at the end, add the pure grays (at a higher resolution) --*/

   ss = 3 * MREN_MAX_COLORS ;
   for( ii=0 ; ii < MREN_MAX_GRAYS ; ii++ ){
      MREN_colorshorts[ss++] = ii ;
      MREN_colorshorts[ss++] = ii ;
      MREN_colorshorts[ss++] = ii ;
   }

   /*-- load a short 8 x 8 x 4 color cube [indexed by unsigned chars] --*/

   for( rr=0 ; rr < 8 ; rr++ ){
      for( gg=0 ; gg < 8 ; gg++ ){
         for( bb=0 ; bb < 4 ; bb++ ){

            ss = (rr << 5) | (gg << 2) || (bb) ;  /* color index */

            MREN_colorbytes[3*ss  ] = (rr * 255.0) / 8.0 ;
            MREN_colorbytes[3*ss+1] = (gg * 255.0) / 8.0 ;
            MREN_colorbytes[3*ss+2] = (bb * 255.0) / 4.0 ;
         }
      }
   }

   return ;
}

void destroy_MREN_colortable( void )
{
   if( MREN_colorshorts == NULL ) return ;
   free( MREN_colorshorts ); MREN_colorshorts = NULL ;
   free( MREN_graytable   ); MREN_graytable   = NULL ;
   free( MREN_opatable    ); MREN_opatable    = NULL ;
   free( MREN_colorbytes  ); MREN_colorbytes  = NULL ;
   return ;
}

static int num_renderers = 0 ;  /* global count of how many are open */

/*--------------------------------------------------------------------------
  Create a new MRI renderer.
  The return pointer is passed to all other MREN routines.
----------------------------------------------------------------------------*/

#define DEFAULT_THETA 130.0
#define DEFAULT_PHI   285.0
#define DEFAULT_PSI     0.0

void * new_MREN_renderer( void )
{
   MREN_stuff * ar ;

   /* from malloc    12 Feb 2009 [lesstif patrol] */
   ar = (MREN_stuff *) calloc( 1, sizeof(MREN_stuff) ) ;
   ar->type = MREN_TYPE ;

   init_MREN_colortable() ;  /* in case it's not already setup */

   /*-- initialize VolPack --*/

   ar->vpc = vpCreateContext() ;

   vpSeti( ar->vpc , VP_CONCAT_MODE , VP_CONCAT_LEFT ) ;

   vpCurrentMatrix( ar->vpc , VP_MODEL ) ;
   vpIdentityMatrix( ar->vpc ) ;

   vpCurrentMatrix( ar->vpc , VP_VIEW ) ;
   vpIdentityMatrix( ar->vpc ) ;
   vpRotate( ar->vpc , VP_X_AXIS , DEFAULT_PHI   ) ;
   vpRotate( ar->vpc , VP_Y_AXIS , DEFAULT_THETA ) ;

#undef USE_CUEING
#ifdef USE_CUEING
   vpSetDepthCueing( ar->vpc , 1.0 , 0.5 ) ;
   vpEnable( ar->vpc , VP_DEPTH_CUE , 1 ) ;
#endif

   vpCurrentMatrix( ar->vpc , VP_PROJECT ) ;
   vpIdentityMatrix( ar->vpc ) ;
   vpWindow( ar->vpc , VP_PARALLEL , -0.55,0.55 , -0.55,0.55 , -0.55,0.55 ) ;

   /*-- initialize the rest of the data --*/

   ar->nx = ar->ny = ar->nz = ar->verbose = ar->newopac = ar->newvox = 0 ;
   ar->sx = ar->sy = ar->sz = 1.0 ;

   ar->theta = DEFAULT_THETA ;
   ar->phi   = DEFAULT_PHI ;
   ar->psi   = DEFAULT_PSI ;
   ar->shim  = ar->opim = NULL ;
   ar->vox   = NULL ;
   ar->pmode = PMODE_LOW ;

   ar->grayset = ar->rgbset = ar->opaset = 0 ;  /* nothing set yet */

   ar->ncmap = ar->newcmap = 0 ;
   ar->cmap  = NULL ;

   ar->min_opacity = 0.05 ;

   num_renderers ++ ;
   return (void *) ar ;
}

/*-----------------------------------------------------------------------------
  Turn depth cueing on or off -- 11 Sep 2001
-------------------------------------------------------------------------------*/

void MREN_depth_cue( void *ah , int onoff )
{
   MREN_stuff * ar = (MREN_stuff *) ah ;

   if( !ISVALID_MREN(ar) ) return ;

   vpSetDepthCueing( ar->vpc , 2.0 , 1.3863 ) ;
   vpEnable( ar->vpc , VP_DEPTH_CUE , onoff ) ;
   return ;
}

/*-----------------------------------------------------------------------------
   Get rid of an MRI renderer.
-------------------------------------------------------------------------------*/

void destroy_MREN_renderer( void * ah )
{
   MREN_stuff * ar = (MREN_stuff *) ah ;

   if( !ISVALID_MREN(ar) ) return ;

   if( ar->vox  != NULL ) free(ar->vox) ;
   if( ar->cmap != NULL ) free(ar->cmap) ;
   vpDestroyContext( ar->vpc ) ;
   free(ar) ;

   num_renderers-- ; if( num_renderers == 0 ) destroy_MREN_colortable() ;
   return ;
}

/*-----------------------------------------------------------------------------
   For debugging purposes
-------------------------------------------------------------------------------*/

void MREN_be_verbose( void * ah )
{
   MREN_stuff * ar = (MREN_stuff *) ah ;
   if( !ISVALID_MREN(ar) ) return ;
   ar->verbose = 1 ; return ;
}

void MREN_be_quiet( void * ah )
{
   MREN_stuff * ar = (MREN_stuff *) ah ;
   if( !ISVALID_MREN(ar) ) return ;
   ar->verbose = 0 ; return ;
}

/*-----------------------------------------------------------------------------
   Set the minimum voxel opacity to consider
-------------------------------------------------------------------------------*/

void MREN_set_min_opacity( void * ah , float opm )
{
   MREN_stuff * ar = (MREN_stuff *) ah ;

   if( !ISVALID_MREN(ar) ) return ;
   if( opm <= 0.0 || opm >= 1.0 ) opm = 0.05 ;
   ar->min_opacity = opm ;

   if( ar->verbose ) fprintf(stderr,"--MREN: min_opacity = %f\n",opm) ;
   return ;
}

/*-----------------------------------------------------------------------------
  Load an user defined colormap.
    ncol    = number of colors (2..65535)
    rmap[i] = red map for index i=0..ncol-1 (values 0..255), etc.
-------------------------------------------------------------------------------*/

void MREN_set_rgbmap( void * ah, int ncol, byte * rmap, byte * gmap, byte * bmap )
{
   MREN_stuff * ar = (MREN_stuff *) ah ;
   int ii ;

   if( !ISVALID_MREN(ar) ) return ;
   if( ncol < 2 || ncol > 65535 || rmap==NULL || gmap==NULL || bmap==NULL ) return ;

   if( ar->cmap != NULL ) free(ar->cmap) ;

   ar->cmap  = (float *) malloc( sizeof(float) * (3*ncol) ) ;
   ar->ncmap = ncol ;

   for( ii=0 ; ii < ncol ; ii++ ){
      ar->cmap[3*ii  ] = rmap[ii] ;
      ar->cmap[3*ii+1] = gmap[ii] ;
      ar->cmap[3*ii+2] = bmap[ii] ;
   }

   ar->newcmap = 1 ;

   if( ar->verbose ){
      fprintf(stderr,"--MREN: new colormap\n") ;
      for( ii=0 ; ii < ncol ; ii++ ){
         fprintf(stderr,"#%3d: %5.1f %5.1f %5.1f",
                 ii , ar->cmap[3*ii],ar->cmap[3*ii+1],ar->cmap[3*ii+2]) ;
         ii++ ;
         if( ii < ncol )
            fprintf(stderr,"  #%3d: %5.1f %5.1f %5.1f",
                    ii , ar->cmap[3*ii],ar->cmap[3*ii+1],ar->cmap[3*ii+2]) ;
         ii++ ;
         if( ii < ncol )
            fprintf(stderr,"  #%3d: %5.1f %5.1f %5.1f",
                    ii , ar->cmap[3*ii],ar->cmap[3*ii+1],ar->cmap[3*ii+2]) ;
         fprintf(stderr,"\n") ;
      }
   }
   return ;
}

void MREN_unset_rgbmap( void * ah )
{
   MREN_stuff * ar = (MREN_stuff *) ah ;

   if( !ISVALID_MREN(ar) || ar->cmap == NULL ) return ;
   if( ar->cmap != NULL ){ free(ar->cmap) ; ar->cmap = NULL ; }
   ar->ncmap = 0 ; ar->newcmap = 1 ;

   if( ar->verbose ) fprintf(stderr,"--MREN: delete colormap\n") ;
   return ;
}

/*-----------------------------------------------------------------------------
   Set the grayscale brick in a renderer;
   Returns -1 if an error occurs, otherwise returns 0.
   Note that this brick is NOT free-d by MREN at any point -- that is
   the user's responsibility.
-------------------------------------------------------------------------------*/

int MREN_set_graybytes( void * ah , MRI_IMAGE * grim )
{
   MREN_stuff * ar = (MREN_stuff *) ah ;
   int newvox=0 , nvox,ii ;
   byte    * gar ;
   rgbvox  * rvox ;

   /*-- sanity checks --*/

   if( !ISVALID_MREN(ar) || grim == NULL || grim->kind != MRI_byte ) return -1 ;

   if( grim->nx < 3 || grim->ny < 3 || grim->nz < 3 ){
      fprintf(stderr,"**MREN: illegal dimensions for a gray brick\n") ;
      return -1 ;
   }

   if( ar->verbose ){
      if( ar->rgbset ) fprintf(stderr,"--MREN: switching from rgb to gray brick\n") ;
      else             fprintf(stderr,"--MREN: input a new gray brick\n") ;
   }

   /*-- if have new dimensions, have to invalidate pre-existing opacity --*/

   if( ar->nx > 0 &&
       ( ar->nx != grim->nx || ar->ny != grim->ny || ar->nz != grim->nz ) ){

      ar->opim = NULL ; ar->opaset = 0 ;

      if( ar->vox != NULL ){ free(ar->vox) ; ar->vox = NULL ; }

      if( ar->verbose )
         fprintf(stderr,"--MREN: new gray brick changes volume dimensions\n"
                        "        nx:%d->%d  ny:%d->%d  nz:%d->%d\n",
                        ar->nx,grim->nx , ar->ny,grim->ny , ar->nz,grim->nz ) ;
   }

   /*-- set dimensions --*/

   ar->shim = grim ;
   ar->nx   = grim->nx ;
   ar->ny   = grim->ny ;
   ar->nz   = grim->nz ; nvox = ar->nx * ar->ny * ar->nz ;

   /*-- if need be, allocate a voxel array to hold the data --*/

   if( ar->vox == NULL ){
      ar->newvox = newvox = 1 ;
      ar->vox = (rgbvox *) malloc( sizeof(rgbvox) * nvox ) ;
      if( ar->vox == NULL ){
         fprintf(stderr,"**MREN: can't malloc workspace with new gray brick\n") ;
         return -1 ;
      } else if( ar->verbose ){
         fprintf(stderr,"--MREN: allocated new voxel array\n") ;
      }
   }

   /*-- copy grayscale data into voxel array --*/

   rvox = ar->vox ;
   gar  = MRI_BYTE_PTR(grim) ;
   for( ii=0 ; ii < nvox ; ii++ ) rvox[ii].rgb = (unsigned short) gar[ii] ;

   if( ar->rgbset ) ar->newvox = 1 ;  /* changed from color to gray */

   ar->grayset = 1 ; ar->rgbset = 0 ;
   return 0 ;
}

/*-----------------------------------------------------------------------------
   Set the opacity brick in a renderer.
   Returns -1 if an error occurs, otherwise returns 0.
   Note that this brick is NOT free-d by MREN at any point -- that is
   the user's responsibility.
-------------------------------------------------------------------------------*/

int MREN_set_opabytes( void * ah , MRI_IMAGE * opim )
{
   MREN_stuff * ar = (MREN_stuff *) ah ;
   int nvox,ii , newvox=0 ;
   byte    * gar ;
   rgbvox  * rvox ;

   /*-- sanity checks --*/

   if( !ISVALID_MREN(ar) || opim == NULL || opim->kind != MRI_byte ) return -1 ;

   if( opim->nx < 3 || opim->ny < 3 || opim->nz < 3 ){
      fprintf(stderr,"**MREN: illegal dimensions for an opacity brick\n") ;
      return -1 ;
   }

   /*-- if have new dimensions, toss old stuff that doesn't match --*/

   if( ar->nx > 0 &&
       ( ar->nx != opim->nx || ar->ny != opim->ny || ar->nz != opim->nz ) ){

      ar->shim = NULL ; ar->grayset = ar->rgbset = 0 ;

      if( ar->vox != NULL ){ free(ar->vox) ; ar->vox = NULL ; }

      if( ar->verbose )
         fprintf(stderr,"--MREN: new opacity brick changes volume dimensions\n"
                        "        nx:%d->%d  ny:%d->%d  nz:%d->%d\n",
                        ar->nx,opim->nx , ar->ny,opim->ny , ar->nz,opim->nz ) ;
   } else {
      if( ar->verbose ) fprintf(stderr,"--MREN: new opacity brick\n") ;
   }

   /*-- set dimensions --*/

   ar->opim = opim ;
   ar->nx   = opim->nx ;
   ar->ny   = opim->ny ;
   ar->nz   = opim->nz ; nvox = ar->nx * ar->ny * ar->nz ;

   /*-- if need be, allocate a voxel array to hold the data --*/

   if( ar->vox == NULL ){
      ar->newvox = newvox = 1 ;
      ar->vox = (rgbvox *) malloc( sizeof(rgbvox) * nvox ) ;
      if( ar->vox == NULL ){
         fprintf(stderr,"**MREN: can't malloc workspace with new opacity brick\n") ;
         return -1 ;
      } else if( ar->verbose ){
         fprintf(stderr,"--MREN: allocated new voxel array\n") ;
      }
   }

   /*-- load the opacity into voxel array --*/

   gar  = MRI_BYTE_PTR(ar->opim) ;
   rvox = ar->vox ;
   for( ii=0 ; ii < nvox ; ii++ ) rvox[ii].alpha = (unsigned short) gar[ii] ;

   ar->newopac = 1 ; ar->opaset = 1 ;
   return 0 ;
}

/*-----------------------------------------------------------------------------
   Convert an RGB image to a standard 8x8x4 (MREN_colorbytes)
   color-mapped image, suitable for use in MREN_set_rgbbytes.
   The output image is composed of bytes.
-------------------------------------------------------------------------------*/

MRI_IMAGE * MREN_rgb_to_colorbytes( MRI_IMAGE * rgbim )
{
   byte * rgbar , rb,gb,bb ;
   byte * shar ;
   MRI_IMAGE * shim ;
   int ii ;

   if( rgbim == NULL || rgbim->kind != MRI_rgb ) return NULL ;

   shim  = mri_new_conforming( rgbim , MRI_byte ) ;
   shar  = MRI_BYTE_PTR(shim) ;
   rgbar = MRI_RGB_PTR(rgbim) ;

   for( ii=0 ; ii < shim->nvox ; ii++ ){
      rb = rgbar[3*ii  ] >> 5 ;
      gb = rgbar[3*ii+1] >> 5 ;
      bb = rgbar[3*ii+2] >> 6 ;

      shar[ii] = (rb << 5) | (gb << 2) | bb ;  /* index into colorbytes */
   }

   return shim ;
}

/*-----------------------------------------------------------------------------
   Convert an RGB image to a standard 32x32x32+256 (MREN_colorshorts)
   color-mapped image, suitable for use in MREN_set_rgbshorts.
   The output image is composed of shorts.
-------------------------------------------------------------------------------*/

MRI_IMAGE * MREN_rgb_to_colorshorts( MRI_IMAGE * rgbim )
{
   byte * rgbar , rb,gb,bb ;
   unsigned short * shar ;
   MRI_IMAGE * shim ;
   int ii ;

   if( rgbim == NULL || rgbim->kind != MRI_rgb ) return NULL ;

   shim  = mri_new_conforming( rgbim , MRI_short ) ;
   shar  = (unsigned short *) MRI_SHORT_PTR(shim) ;
   rgbar = MRI_RGB_PTR(rgbim) ;

   for( ii=0 ; ii < shim->nvox ; ii++ ){
      rb = EIGHT_TO_FIVE(rgbar[3*ii  ]) ;
      gb = EIGHT_TO_FIVE(rgbar[3*ii+1]) ;
      bb = EIGHT_TO_FIVE(rgbar[3*ii+2]) ;

      if( rb == gb && rb == bb ){
         shar[ii] = MREN_MAX_COLORS + rgbar[3*ii] ; /* index into grayscale */
      } else {
         shar[ii] = FIVE_TO_SHORT( rb , gb, bb ) ;  /* index into color cube */
      }
   }

   return shim ;
}

/*-----------------------------------------------------------------------------
   Set the color brick in a renderer -- values in the input image are
   indices into a colormap of length <= 256 -- by default this will
   be MREN_colorbytes, but can be replaced by the user.
   Returns -1 if an error occurs, otherwise returns 0.
   Note that these bricks are NOT free-d by MREN at any point -- that is
   the user's responsibility.
-------------------------------------------------------------------------------*/

int MREN_set_rgbbytes( void * ah , MRI_IMAGE * rgbim )
{
   MREN_stuff * ar = (MREN_stuff *) ah ;
   int newvox=0 , nvox,ii ;
   byte    * gar ;
   rgbvox  * rvox ;

   /*-- sanity checks --*/

   if( !ISVALID_MREN(ar) || rgbim == NULL || rgbim->kind != MRI_byte ) return -1 ;

   if( rgbim->nx < 3 || rgbim->ny < 3 || rgbim->nz < 3 ){
      fprintf(stderr,"**MREN: illegal dimensions for a color brick\n") ; return -1 ;
   }

   /*-- if had an old gray brick, toss it (or at least its pointer) --*/

   if( ar->verbose ){
      if( ar->grayset ) fprintf(stderr,"--MREN: switching from gray to rgb brick\n") ;
      else              fprintf(stderr,"--MREN: input new rgb brick of bytes\n") ;
   }

   /*-- if have new dimensions, toss old stuff that doesn't match --*/

   if( ar->nx > 0 &&
       ( ar->nx != rgbim->nx || ar->ny != rgbim->ny || ar->nz != rgbim->nz ) ){

      ar->opim = NULL ; ar->opaset = 0 ;

      if( ar->vox != NULL ){ free(ar->vox) ; ar->vox = NULL ; }

      if( ar->verbose )
         fprintf(stderr,"--MREN: new rgb brick changes volume dimensions\n"
                        "        nx:%d->%d  ny:%d->%d  nz:%d->%d\n",
                        ar->nx,rgbim->nx , ar->ny,rgbim->ny , ar->nz,rgbim->nz ) ;
   }

   /*-- set dimensions --*/

   ar->shim = rgbim ;
   ar->nx   = rgbim->nx ;
   ar->ny   = rgbim->ny ;
   ar->nz   = rgbim->nz ; nvox = ar->nx * ar->ny * ar->nz ;

   /*-- if need be, allocate a voxel array to hold the data --*/

   if( ar->vox == NULL ){
      ar->newvox = newvox = 1 ;
      ar->vox = (rgbvox *) malloc( sizeof(rgbvox) * nvox ) ;
      if( ar->vox == NULL ){
         fprintf(stderr,"**MREN: can't malloc workspace with new color bricks\n") ;
         return -1 ;
      } else if( ar->verbose ){
         fprintf(stderr,"--MREN: allocated new voxel array\n") ;
      }
   }

   /*-- copy color data into voxel array --*/

   rvox = ar->vox ;
   gar  = MRI_BYTE_PTR(rgbim) ;
   for( ii=0 ; ii < nvox ; ii++ ) rvox[ii].rgb = (unsigned short) gar[ii] ;

   if( ar->grayset ) ar->newvox = 1 ;  /* changed from gray to color */

   ar->rgbset = 1 ; ar->grayset = 0 ;
   return 0 ;
}

/*-----------------------------------------------------------------------------
  Similar, but where the colormap indexes are unsigned shorts.
  They can refer to a user-supplied colormap, or to the standard
  32x32x32+256 colormap (MREN_colorshorts).
-------------------------------------------------------------------------------*/

int MREN_set_rgbshorts( void * ah , MRI_IMAGE * rgbim )
{
   MREN_stuff * ar = (MREN_stuff *) ah ;
   int newvox=0 , nvox,ii ;
   unsigned short * gar ;
   rgbvox * rvox ;

   /*-- sanity checks --*/

   if( !ISVALID_MREN(ar) || rgbim == NULL || rgbim->kind != MRI_short ) return -1 ;

   if( rgbim->nx < 3 || rgbim->ny < 3 || rgbim->nz < 3 ){
      fprintf(stderr,"**MREN: illegal dimensions for a color brick\n") ; return -1 ;
   }

   if( ar->verbose ){
      if( ar->grayset ) fprintf(stderr,"--MREN: switching from gray to rgb brick\n") ;
      else              fprintf(stderr,"--MREN: input new rgb brick of shorts\n") ;
   }

   /*-- if have new dimensions, toss old stuff that doesn't match --*/

   if( ar->nx > 0 &&
       ( ar->nx != rgbim->nx || ar->ny != rgbim->ny || ar->nz != rgbim->nz ) ){

      ar->opim = NULL ; ar->opaset = 0 ;

      if( ar->vox != NULL ){ free(ar->vox) ; ar->vox = NULL ; }

      if( ar->verbose )
         fprintf(stderr,"--MREN: new rgb brick changes volume dimensions\n"
                        "        nx:%d->%d  ny:%d->%d  nz:%d->%d\n",
                        ar->nx,rgbim->nx , ar->ny,rgbim->ny , ar->nz,rgbim->nz ) ;
   }

   /*-- set dimensions --*/

   ar->shim = rgbim ;
   ar->nx   = rgbim->nx ;
   ar->ny   = rgbim->ny ;
   ar->nz   = rgbim->nz ; nvox = ar->nx * ar->ny * ar->nz ;

   /*-- if need be, allocate a voxel array to hold the data --*/

   if( ar->vox == NULL ){
      ar->newvox = newvox = 1 ;
      ar->vox = (rgbvox *) malloc( sizeof(rgbvox) * nvox ) ;
      if( ar->vox == NULL ){
         fprintf(stderr,"**MREN: can't malloc workspace with new color bricks\n") ;
         return -1 ;
      } else if( ar->verbose ){
         fprintf(stderr,"--MREN: allocated new voxel array\n") ;
      }
   }

   /*-- copy color data into voxel array --*/

   rvox = ar->vox ;
   gar  = (unsigned short *) MRI_SHORT_PTR(rgbim) ;
   for( ii=0 ; ii < nvox ; ii++ ) rvox[ii].rgb = gar[ii] ;

   if( ar->grayset ) ar->newvox = 1 ;  /* changed from gray to color */

   ar->rgbset = 2 ; ar->grayset = 0 ;
   return 0 ;
}

/*-------------------------------------------------------------------------------
   Set the viewpoint of the user (in polar angles)
---------------------------------------------------------------------------------*/

void MREN_set_viewpoint( void * ah , float theta , float phi , float psi )
{
   MREN_stuff * ar = (MREN_stuff *) ah ;

   if( !ISVALID_MREN(ar) ) return ;

   ar->theta = theta ; ar->phi = phi ; ar->psi = psi ;

   vpCurrentMatrix( ar->vpc , VP_VIEW ) ;
   vpIdentityMatrix( ar->vpc ) ;
   vpRotate( ar->vpc , VP_Z_AXIS , psi   ) ;  /* roll  */
   vpRotate( ar->vpc , VP_X_AXIS , phi   ) ;  /* pitch */
   vpRotate( ar->vpc , VP_Y_AXIS , theta ) ;  /* yaw   */

   if( ar->verbose ){
      vpMatrix4 vpm ;

      fprintf(stderr,"--MREN: set theta=%f  phi=%f  psi=%f\n",theta,phi,psi) ;

      vpGetMatrix( ar->vpc , VP_VIEW , vpm ) ;
      fprintf(stderr,"--matrix: %8.5f %8.5f %8.5f %8.5f\n"
                     "          %8.5f %8.5f %8.5f %8.5f\n"
                     "          %8.5f %8.5f %8.5f %8.5f\n"
                     "          %8.5f %8.5f %8.5f %8.5f\n" ,
              vpm[0][0] , vpm[0][1] , vpm[0][2] , vpm[0][3] ,
              vpm[1][0] , vpm[1][1] , vpm[1][2] , vpm[1][3] ,
              vpm[2][0] , vpm[2][1] , vpm[2][2] , vpm[2][3] ,
              vpm[3][0] , vpm[3][1] , vpm[3][2] , vpm[3][3]  ) ;
   }

   return ;
}

/*-------------------------------------------------------------------------------
   Set the precalculation mode of the renderer
---------------------------------------------------------------------------------*/

void MREN_set_precalculation( void * ah , int mode )
{
   MREN_stuff * ar = (MREN_stuff *) ah ;

   if( !ISVALID_MREN(ar) || mode < PMODE_LOW || mode > PMODE_HIGH ) return ;

   if( ar->pmode != mode ){ ar->pmode = mode ; ar->newopac = 1 ; }
   return ;
}

/*-------------------------------------------------------------------------------
   Set the scale factors for each axis (default = 1).  The inputs should
   be the size of the brick along each axes (e.g., sx = nx * dx).  This is
   needed because VolPack assumes the input data is in a unit cube, but
   our data may not be so uniform.

   N.B.: This is disabled, since VolPack doesn't seem to work in this regards.
---------------------------------------------------------------------------------*/

void MREN_set_size( void * ah , float sx , float sy , float sz )
{
#if 0
   MREN_stuff * ar = (MREN_stuff *) ah ;
   float mmm ;

   if( !ISVALID_MREN(ar) ) return ;

   sx = fabs(sx) ; if( sx == 0.0 ) sx = 1.0 ;  /* don't allow non-positive sizes */
   sy = fabs(sy) ; if( sy == 0.0 ) sy = 1.0 ;
   sz = fabs(sz) ; if( sz == 0.0 ) sz = 1.0 ;

   mmm = sx ;
   if( mmm < sy ) mmm = sy ;
   if( mmm < sz ) mmm = sz ;  /* mmm = maximum size */

   ar->sx = sx / mmm ;        /* scale factors are <= 1.0 */
   ar->sy = sy / mmm ;
   ar->sz = sz / mmm ;

   vpCurrentMatrix( ar->vpc , VP_MODEL ) ;  /* scale model to world */
   vpIdentityMatrix( ar->vpc ) ;
   vpScale( ar->vpc , sx , sy , sz ) ;

   if( ar->verbose )
      fprintf(stderr,"--MREN: set scale factors = %f %f %f\n",ar->sx,ar->sy,ar->sz) ;
#endif

   return ;
}

/*-------------------------------------------------------------------------------
   Find out if a renderer needs data
---------------------------------------------------------------------------------*/

int MREN_needs_data( void * ah )
{
   MREN_stuff * ar = (MREN_stuff *) ah ;

   return (ar->vox == NULL) ;
}

/*-------------------------------------------------------------------------------
   Actually render an image.  Returns NULL if an error occurs.
   Input npix = number of pixels on a side of the image (always will be square).
   If rendering a grayscale brick, returns an image of kind MRI_byte.
   If rendering RGB bricks, returns an image of kind MRI_rgb.
---------------------------------------------------------------------------------*/

MRI_IMAGE * MREN_render( void *ah , int npix )
{
   MREN_stuff *ar = (MREN_stuff *) ah ;
   int isgray , isrgb ;
   MRI_IMAGE *im=NULL ;
   byte * imar ;
   vpResult fred ;

   /*-- sanity checks --*/

   if( !ISVALID_MREN(ar) ) return NULL ;

   if( npix < 16 ){
      fprintf(stderr,"**MREN: attempt to render with less than 16 pixels!\n") ;
      return NULL ;
   }

   isgray = (ar->grayset > 0) ;
   isrgb  = (ar->rgbset  > 0) ;

   if( isgray && isrgb ){
      fprintf(stderr,"**MREN: attempt to render gray and color simultaneously?\n");
      return NULL ;
   }

   if( (!isgray && !isrgb) || ar->vox == NULL ){
      fprintf(stderr,"**MREN: attempt to render without data being loaded!\n") ;
      return NULL ;
   }

   if( ar->opaset == 0 ){
      fprintf(stderr,"**MREN: attempt to render without opacity being loaded!\n") ;
      return NULL ;
   }

   if( ar->nx < 3 || ar->ny < 3 || ar->nz < 3 ){
      fprintf(stderr,"**MREN: attempt to render without initialization!\n") ;
      return NULL ;
   }

   if( ar->verbose ) fprintf(stderr,"--MREN: setup for rendering\n") ;

   /*-- if have new voxel array, must tell VolPack all about it --*/

   if( ar->newvox || (isrgb && ar->newcmap) ){
      int nvox = ar->nx * ar->ny * ar->nz ;
      int oct_range ;
      rgbvox vv , *rv = &vv ;

      /* 3D dimensions */

      if( ar->verbose ) fprintf(stderr,"  call vpSetVolumeSize\n") ;

      fred = vpSetVolumeSize( ar->vpc , ar->nx , ar->ny , ar->nz ) ;
      if( fred != VP_OK ){
         fprintf(stderr,"**MREN: vpSetVolumeSize failed: code=%d\n",(int)fred) ;
         return NULL ;
      }

      /* each voxel has 2 data fields; 1 for shading and 1 for opacity */

      if( ar->verbose ) fprintf(stderr,"  call vpSetVoxelSize\n") ;

      fred = vpSetVoxelSize( ar->vpc , sizeof(rgbvox) , 2 , 1 , 1 ) ;
      if( fred != VP_OK ){
         fprintf(stderr,"**MREN: vpSetVoxelSize failed: code=%d\n",(int)fred) ;
         return NULL ;
      }

      /* voxel field 1 (alpha) is an index into MREN_opatable */

      if( ar->verbose ) fprintf(stderr,"  call vpSetVoxelField(1)\n") ;

      fred = vpSetVoxelField( ar->vpc, 1, sizeof(short),
                              vpFieldOffset(rv,alpha), MREN_MAX_GRAYS-1 );
      if( fred != VP_OK ){
         fprintf(stderr,"**MREN: vpSetVoxelField(1) failed: code=%d\n",(int)fred) ;
         return NULL ;
      }

      /* tell VolPack where the voxels are */

      if( ar->verbose ) fprintf(stderr,"  call vpSetRawVoxels\n") ;

      fred = vpSetRawVoxels( ar->vpc , ar->vox ,
                             sizeof(rgbvox)*nvox ,
                             sizeof(rgbvox) ,
                             sizeof(rgbvox)*(ar->nx) ,
                             sizeof(rgbvox)*(ar->nx * ar->ny) ) ;
      if( fred != VP_OK ){
         fprintf(stderr,"**MREN: vpSetRawVoxels failed: code=%d\n",(int)fred) ;
         return NULL ;
      }

      /*--- gray scale input ---*/

      if( isgray ){

         /* voxel field 0 (rgb) is an index into MREN_graytable */

         if( ar->verbose ) fprintf(stderr,"  call vpSetVoxelField(grays)\n") ;

         fred = vpSetVoxelField( ar->vpc, 0, sizeof(short),
                                 vpFieldOffset(rv,rgb), MREN_MAX_GRAYS-1 );
         if( fred != VP_OK ){
            fprintf(stderr,"**MREN: vpSetVoxelField(0) failed: code=%d\n",(int)fred) ;
            return NULL ;
         }

         /* setup MREN_graytable to hold the colormap */

         if( ar->verbose ) fprintf(stderr,"  call vpSetLookupShader(graytable)\n") ;

         fred = vpSetLookupShader( ar->vpc , 1 , 1 , 0 ,
                                   MREN_graytable , sizeof(float)*MREN_MAX_GRAYS ,
                                   0 , NULL , 0 ) ;
         if( fred != VP_OK ){
            fprintf(stderr,"**MREN: vpSetLookupShader failed: code=%d\n",(int)fred) ;
            return NULL ;
         }

      /*--- color input ---*/

      } else if( isrgb ){

         /* There are 3 possible cases for the colormap:
              a) user supplied colormap
              b) standard 16 bit index colormap MREN_colorshorts
              c) standard 8 bit index colormap MREN_colorbytes    */

         if( ar->cmap != NULL && ar->ncmap > 1 ){   /* user supplied colormap */

            if( ar->verbose ) fprintf(stderr,"  call vpSetVoxelField(cmap)\n") ;

            fred = vpSetVoxelField( ar->vpc, 0, sizeof(short),
                                    vpFieldOffset(rv,rgb), ar->ncmap-1 );

            if( ar->verbose ) fprintf(stderr,"  call vpSetLookupShader(cmap)\n") ;

            fred = vpSetLookupShader( ar->vpc , 3 , 1 , 0 ,
                                      ar->cmap , sizeof(float)*3*ar->ncmap ,
                                      0 , NULL , 0 ) ;

         } else if( ar->rgbset == 2 ){          /* MREN_colorshorts */

            if( ar->verbose ) fprintf(stderr,"  call vpSetVoxelField(rgb shorts)\n") ;

            fred = vpSetVoxelField( ar->vpc, 0, sizeof(short),
                                    vpFieldOffset(rv,rgb), TOT_COLORS-1 );

            if( ar->verbose ) fprintf(stderr,"  call vpSetLookupShader(colorshorts)\n") ;

            fred = vpSetLookupShader( ar->vpc , 3 , 1 , 0 ,
                                      MREN_colorshorts , sizeof(float)*TOT_COLORS*3 ,
                                      0 , NULL , 0 ) ;

         } else {                               /* MREN_colorbytes */

            if( ar->verbose ) fprintf(stderr,"  call vpSetVoxelField(rgb bytes)\n") ;

            fred = vpSetVoxelField( ar->vpc, 0, sizeof(short),
                                    vpFieldOffset(rv,rgb), MREN_MAX_GRAYS-1 );

            if( ar->verbose ) fprintf(stderr,"  call vpSetLookupShader(colorbytes)\n") ;

            fred = vpSetLookupShader( ar->vpc , 3 , 1 , 0 ,
                                      MREN_colorbytes , sizeof(float)*MREN_MAX_GRAYS*3 ,
                                      0 , NULL , 0 ) ;
         }

         if( fred != VP_OK ){
            fprintf(stderr,"**MREN: vpSetLookupShader failed: code=%d\n",(int)fred) ;
            return NULL ;
         }
      }

      /*-- in all cases, voxel field 1 (alpha) is an index into MREN_opatable --*/

      if( ar->verbose ) fprintf(stderr,"  call vpSetClassifierTable\n") ;

      fred = vpSetClassifierTable( ar->vpc, 0, 1, MREN_opatable, sizeof(float)*MREN_MAX_GRAYS ) ;
      if( fred != VP_OK ){
         fprintf(stderr,"**MREN: vpSetClassifierTable failed: code=%d\n",(int)fred) ;
         return NULL ;
      }

      /* threshold for octree bins: 12 = 5% of possible opacity range */

      if( ar->verbose ) fprintf(stderr,"  call vpMinMaxOctreeThreshold\n") ;

      fred = vpMinMaxOctreeThreshold( ar->vpc , 0 , 12 ) ;
      if( fred != VP_OK ){
         fprintf(stderr,"**MREN: vpMinMaxOctreeThreshold failed: code=%d\n",(int)fred) ;
         return NULL ;
      }

      ar->newopac = 1 ;  /* for the precalculations below */
      ar->newvox  = 0 ;
      ar->newcmap = 0 ;
   }

   /*-- if have new data in the voxel array, must do precalculations --*/

   vpSetd( ar->vpc , VP_MAX_RAY_OPACITY   , 0.95 ) ;
   vpSetd( ar->vpc , VP_MIN_VOXEL_OPACITY , ar->min_opacity ) ;

   if( ar->newopac ){

      (void) vpDestroyMinMaxOctree( ar->vpc ) ;      /* toss previous work */
      (void) vpDestroyClassifiedVolume( ar->vpc ) ;

      if( ar->pmode == PMODE_MEDIUM ){

         if( ar->verbose ) fprintf(stderr,"--MREN: computing octree\n") ;

         /* make octree, down to 4x4x4 voxel bins */

         if( ar->verbose ) fprintf(stderr,"  call vpCreateMinMaxOctree\n") ;

         fred = vpCreateMinMaxOctree( ar->vpc , 0 , 4 ) ;
         if( fred != VP_OK ){
            fprintf(stderr,"**MREN: vpCreateMinMaxOctree failed: code=%d\n",(int)fred) ;
            return NULL ;
         }

      } else if( ar->pmode == PMODE_HIGH ){

         if( ar->verbose ) fprintf(stderr,"--MREN: computing classified volume\n") ;

         /* classify volume (slower than octree, but may do faster rendering) */

         if( ar->verbose ) fprintf(stderr,"  call vpClassifyVolume\n") ;

         fred = vpClassifyVolume( ar->vpc ) ;
         if( fred != VP_OK ){
            fprintf(stderr,"**MREN: vpClassifyVolume failed: code=%d\n",(int)fred) ;
            return NULL ;
         }
      }

      ar->newopac = 0 ;
   }

   /*-- create the output image --*/

#undef GET_ALPHA  /* for debugging: compute the opacity image */

   if( isgray ){
      im   = mri_new( npix , npix , MRI_byte ) ;
      imar = MRI_BYTE_PTR(im) ;
#ifndef GET_ALPHA
      if( ar->verbose ) fprintf(stderr,"  call vpSetImage(LUMINANCE)\n") ;
      vpSetImage( ar->vpc , imar , npix,npix,npix , VP_LUMINANCE ) ;
#else
      if( ar->verbose ) fprintf(stderr,"  call vpSetImage(ALPHA)\n") ;
      vpSetImage( ar->vpc , imar , npix,npix,npix , VP_ALPHA ) ;
#endif
   } else if( isrgb ){
#ifndef GET_ALPHA
      im   = mri_new( npix , npix , MRI_rgb ) ;
      imar = MRI_RGB_PTR(im) ;
      if( ar->verbose ) fprintf(stderr,"  call vpSetImage(RGB)\n") ;
      vpSetImage( ar->vpc , imar , npix,npix,3*npix , VP_RGB ) ;
#else
      im   = mri_new( npix , npix , MRI_byte ) ;
      imar = MRI_BYTE_PTR(im) ;
      if( ar->verbose ) fprintf(stderr,"  call vpSetImage(ALPHA)\n") ;
      vpSetImage( ar->vpc , imar , npix,npix,npix , VP_ALPHA ) ;
#endif
   }

   if( ar->verbose ) fprintf(stderr,"--MREN: rendering image\n") ;

   if( ar->pmode == PMODE_HIGH ){
      if( ar->verbose ) fprintf(stderr,"  call vpRenderClassifiedVolume\n") ;
      fred = vpRenderClassifiedVolume(ar->vpc) ;
      if( fred != VP_OK ){
         fprintf(stderr,"**MREN: vpRenderClassifiedVolume failed: code=%d\n",(int)fred) ;
         mri_free(im) ; return NULL ;
      }
   } else if( ar->pmode == PMODE_MEDIUM ){
      if( ar->verbose ) fprintf(stderr,"  call vpRenderRawVolume\n") ;
      fred = vpRenderRawVolume(ar->vpc) ;
      if( fred != VP_OK ){
         fprintf(stderr,"**MREN: vpRenderRawVolume failed: code=%d\n",(int)fred) ;
         mri_free(im) ; return NULL ;
      }
   } else {
      if( ar->verbose ) fprintf(stderr,"  call vpBruteForceRender\n") ;
      fred = vpBruteForceRender(ar->vpc) ;
      if( fred != VP_OK ){
         fprintf(stderr,"**MREN: vpBruteForceRender failed: code=%d\n",(int)fred) ;
         mri_free(im) ; return NULL ;
      }
   }

   return im ;
}
