/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "cox_render.h"

/*============================================================================
  CREN = Cox Renderer, a set of routines for volume rendering 3D bricks.

  (1) Use new_CREN_renderer() to create a renderer.
  (2) Load grayscale+color bricks into it with function CREN_set_databytes()
  (3) Set the viewing angles with CREN_set_viewpoint().
  (4) Create an image with CREN_render().
      Loop back through (2-4) as needed to create new images.
  (5) When finished, use destroy_CREN_renderer() to clean up.
==============================================================================*/

static int num_renderers = 0 ;  /* global count of how many are open */

/*--------------------------------------------------------------------------
  Create a new renderer.
  The return pointer is passed to all other CREN routines.
----------------------------------------------------------------------------*/

void * new_CREN_renderer( void )
{
   CREN_stuff * ar ;
   int ii ;

   /*-- make storage for rendering struct --*/

   ar = (CREN_stuff *) malloc( sizeof(CREN_stuff) ) ;
   ar->type = CREN_TYPE ;

   /*-- initialize rendering struct --*/

   ar->nx = ar->ny = ar->nz = ar->newvox = 0 ;
   ar->dx = ar->dy = ar->dz = 1.0 ;

   ar->ax1 = 0   ; ar->ax2 = 1   ; ar->ax3 = 2   ;
   ar->th1 = 0.0 ; ar->th2 = 0.0 ; ar->th3 = 0.0 ; ar->newangles = 1 ;

   ar->vox    = NULL ;  /* no data yet */
   ar->vtm    = NULL ;

   ar->newopa = 0 ;
   ar->opargb = 1.0 ;             /* colored voxels are opaque */
   for( ii=0 ; ii < 128 ; ii++ )  /* linear map for gray opacity */
      ar->opamap[ii] = ii/127.0 ;

   ar->nrgb   = 0 ;               /* no color map set */
   memset( ar->rmap , 0 , 128 ) ; memset( ar->gmap , 0 , 128 ) ;
   memset( ar->bmap , 0 , 128 ) ; memset( ar->imap , 0 , 128 ) ;

   ar->min_opacity = 0.05 ;
   ar->renmode     = CREN_SUM_VOX ;

   num_renderers ++ ; return (void *) ar ;
}

/*-----------------------------------------------------------------------------
   Get rid of a renderer and all its contents
-------------------------------------------------------------------------------*/

void destroy_CREN_renderer( void * ah )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;

   if( !ISVALID_CREN(ar) ) return ;

   if( ar->vox    != NULL ) free(ar->vox) ;
   if( ar->vtm    != NULL ) free_Tmask(ar->vtm) ;
   free(ar) ;

   num_renderers-- ; return ;
}

/*-----------------------------------------------------------------------------
   Set the minimum voxel opacity to consider
-------------------------------------------------------------------------------*/

void CREN_set_min_opacity( void * ah , float opm )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;

   if( !ISVALID_CREN(ar) ) return ;
   if( opm <= 0.0 || opm >= 1.0 ) opm = 0.05 ;
   ar->min_opacity = opm ;
   return ;
}

/*-----------------------------------------------------------------------------
   Set the rendering mode
     CREN_SUM_VOX = integral of voxel data times opacity
     CREN_MIP_VOX = maximum voxel intensity
     CREN_MIP_OPA = maximum opacity
-------------------------------------------------------------------------------*/

void CREN_set_render_mode( void * ah , int mmm )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;
   if( !ISVALID_CREN(ar) ) return ;

   if( mmm < 0 || mmm > CREN_LAST_MODE ) mmm = CREN_SUM_VOX ;
   ar->renmode = mmm ;
   return ;
}

/*-----------------------------------------------------------------------------
  Load the user defined colormap.
    ncol    = number of colors (must be from 1 to 128, inclusive)
    rmap[i] = red   map for index i=0..ncol-1 (for voxel values i+128)
    gmap[i] = green map for index i=0..ncol-1 (for voxel values i+128)
    bmap[i] = blue  map for index i=0..ncol-1 (for voxel values i+128)
-------------------------------------------------------------------------------*/

void CREN_set_rgbmap( void *ah, int ncol, byte *rmap, byte *gmap, byte *bmap )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;
   int ii ;

   if( !ISVALID_CREN(ar) ) return ;
   if( ncol<1 || ncol>128 || rmap==NULL || gmap==NULL || bmap==NULL ) return ;

   ar->nrgb = ncol ;

   /* copy into rendering struct, and compute intensity of each color */

   for( ii=0 ; ii < ncol ; ii++ ){
      ar->rmap[ii] = rmap[ii] ;
      ar->gmap[ii] = gmap[ii] ;
      ar->bmap[ii] = bmap[ii] ;
      ar->imap[ii] = (byte)(0.299*rmap[ii]+0.587*gmap[ii]+0.114*bmap[ii]) ;
   }

   /* set leftovers to 0 */

   for( ii=ncol ; ii < 128 ; ii++ )
      ar->rmap[ii] = ar->gmap[ii] = ar->bmap[ii] = ar->imap[ii] = 0 ;

   return ;
}

/*----------------------------------------------------------------------
  Set the mapping from voxel value to opacity:
    opm[vv] = opacity of voxel value vv, for vv=0..127
    opgrb   = (fixed) opacity of colored voxels, with vv=128..255
  Opacities range from 0.0 to 1.0
------------------------------------------------------------------------*/

void CREN_set_opamap( void *ah, float *opm , float oprgb )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;

   if( !ISVALID_CREN(ar) ) return ;

   if( opm != NULL )
      memcpy( ar->opamap , opm , sizeof(float)*128 ) ;

   if( oprgb >= 0.0 && oprgb <= 1.0 )
      ar->opargb = oprgb ;

#if 0
fprintf(stderr,"CREN_set_opamap: opm[0]=%g opm[127]=%g oprgb=%g\n",
opm[0],opm[127],oprgb) ;
#endif

   ar->newopa = 1 ; return ;
}

/*-----------------------------------------------------------------------
  See if the thing needs data bytes
-------------------------------------------------------------------------*/

int CREN_needs_data( void * ah )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;

   if( !ISVALID_CREN(ar) ) return -1 ;

   return (ar->vox == NULL) ;
}

/*-----------------------------------------------------------------------------
   Set the data brick in a renderer;
   Returns -1 if an error occurs, otherwise returns 0.

   Data bytes:
     values vv=0..127   correspond to grayscale 0..254 (vv << 1)
     values vv=128..255 correspond to colormap index vv-128.
-------------------------------------------------------------------------------*/

int CREN_set_databytes( void * ah , MRI_IMAGE * grim )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;
   int nvox,ii ;
   byte * gar ;

   /*-- sanity checks --*/

   if( !ISVALID_CREN(ar) || grim == NULL || grim->kind != MRI_byte ) return -1 ;

   if( grim->nx < 3 || grim->ny < 3 || grim->nz < 3 ){
      fprintf(stderr,"**CREN: illegal dimensions for a data brick\n") ;
      return -1 ;
   }

   /*-- free old data --*/

   if( ar->vox    != NULL ){ free(ar->vox)      ; ar->vox    = NULL; }
   if( ar->vtm    != NULL ){ free_Tmask(ar->vtm); ar->vtm    = NULL; }

   /*-- set dimensions --*/


   ar->nx = grim->nx ; ar->dx = fabs(grim->dx) ;
   ar->ny = grim->ny ; ar->dy = fabs(grim->dy) ;
   ar->nz = grim->nz ; ar->dz = fabs(grim->dz) ;

   if( ar->dx == 0.0 ) ar->dx = 1.0 ;
   if( ar->dy == 0.0 ) ar->dy = 1.0 ;
   if( ar->dz == 0.0 ) ar->dz = 1.0 ;

   /*-- signal we have new voxel data --*/

   ar->newvox = 1 ;

   /*-- copy data from grim into internal storage --*/

   nvox = ar->nx * ar->ny * ar->nz ;

   ar->vox = (byte *) malloc(nvox) ;
   memcpy( ar->vox , MRI_BYTE_PTR(grim) , nvox ) ;

#if 0
fprintf(stderr,"CREN_set_databytes: nx=%d ny=%d nz=%d\n",ar->nx,ar->ny,ar->nz) ;
fprintf(stderr,"CREN_set_databytes: dx=%g dy=%g dz=%g\n",ar->dx,ar->dy,ar->dz) ;
#endif

   return 0 ;
}

/*----------------------------------------------------------------------------
   Set the viewpoint of the user
     axI = axis about which rotation #I is to take place
            0 == x , 1 == y , 2 == z
     thI = angle of rotation (radians)
------------------------------------------------------------------------------*/

void CREN_set_viewpoint( void * ah , int ax1,float th1 ,
                                     int ax2,float th2 , int ax3,float th3  )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;

   if( !ISVALID_CREN(ar) ) return ;

   ar->ax1 = ax1 ; ar->ax2 = ax2 ; ar->ax3 = ax3 ;
   ar->th1 = th1 ; ar->th2 = th2 ; ar->th3 = th3 ;

#if 0
fprintf(stderr,"CREN_set_viewpoint: ax1=%d th1=%g ax2=%d th2=%g ax3=%d th3=%g\n",
ax1,th1,ax2,th2,ax3,th3) ;
#endif

   ar->newangles = 1 ; return ;
}

#if 0
/*-----------------------------------------------------------------------------
   Actually render an image.  Returns NULL if an error occurs.
   The image is always in MRI_rgb format.
-------------------------------------------------------------------------------*/

MRI_IMAGE * CREN_render( void * ah )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;
   MRI_IMAGE * im ;
   byte * rgb , * var ;
   int nvox , nxy , ii , kk , vv , vtop=128+ar->nrgb ;
   float * used=NULL , obot=ar->min_opacity ;

   /*-- sanity checks --*/

   if( !ISVALID_CREN(ar) ) return NULL ;

   var = ar->vox ;
   if( var == NULL ) return NULL ;

   if( ar->nx < 3 || ar->ny < 3 || ar->nz < 3 ) return NULL ;

   nxy  = ar->nx * ar->ny ;  /* number of voxels in one plane */
   nvox = nxy * ar->nz ;     /* number of voxels in whole volume */

   /*-- make a Tmask to show which 1D rows contain opacity --*/

   if( ar->newvox || ar->newopa || ar->vtm == NULL ){
      byte *tar, orgb ;

      free_Tmask(ar->vtm) ;
      tar  = (byte *) malloc(nvox) ;
      orgb = (ar->opargb >= ar->min_opacity) ;  /* is color opaque? */
      for( ii=0 ; ii < nvox ; ii++ ){
         vv = var[ii] ;
         if( vv < 128 ) tar[ii] = (ar->opamap[vv] >= obot) ;
         else           tar[ii] = orgb ;
      }

      ar->vtm = create_Tmask_byte( ar->nx,ar->ny,ar->nz , tar ) ;
      free(tar) ;
   }

   /*-- rotate the voxels to the correct alignment --*/

   if( ar->newvox || ar->newangles ){

      if( ar->voxrot == NULL )
         ar->voxrot = (byte *) malloc(nvox) ;

      memcpy( ar->voxrot , var , nvox ) ;

      CREN_rotate( ar->nx , ar->ny , ar->nz ,
                   ar->dx , ar->dy , ar->dz , ar->voxrot ,
                   ar->ax1, ar->th1, ar->ax2,ar->th2, ar->ax3,ar->th3 ,
                   DELTA_BEFORE , 0.0,0.0,0.0 , ar->vtm ) ;
   }

   /*-- prepare to create image --*/

   im  = mri_new( ar->nx , ar->ny , MRI_rgb ) ;
   rgb = MRI_RGB_PTR(im) ; memset(rgb,0,3*nxy) ;
   var = ar->voxrot ;

   switch( ar->renmode ){
     default:
     case CREN_SUM_VOX:
        used = (float *) malloc(sizeof(float)*nxy) ;   /* how much opacity */
        for( ii=0 ; ii < nxy ; ii++ ) used[ii] = 0.0 ; /* has been used up */
     break ;

     case CREN_MIP_VOX:
     break ;

     case CREN_MIP_OPA:
     break ;
   }

   /*-- create the output image by running down each column --*/

   for( kk=0 ; kk < ar->nz ; kk++,var+=nxy ){   /* at this level in z */

     switch( ar->renmode ){

#define MAX_OPACITY 0.95

        default:
        case CREN_SUM_VOX:{
          float opa ;

          for( ii=0 ; ii < nxy ; ii++ ){   /* loop over pixels in this level */

             if( used[ii] > MAX_OPACITY ) continue ; /* skip voxel */

             vv = var[ii] ;
                  if( vv < 128  ) opa = ar->opamap[vv] ;  /* gray  */
             else if( vv < vtop ) opa = ar->opargb ;      /* color */
             else                 continue ;         /* skip voxel */
             if( opa < obot ) continue ;             /* skip voxel */

             opa *= (1.0-used[ii]) ; used[ii] += opa ;
             if( opa < obot ) continue ;             /* skip voxel */

             if( vv < 128 ){                         /* gray */
                vv = (byte)( opa * (vv << 1) ) ;
                rgb[3*ii  ] += vv ;
                rgb[3*ii+1] += vv ;
                rgb[3*ii+2] += vv ;
             } else if( vv < vtop ){                 /* color */
                rgb[3*ii  ] += (byte)( opa * ar->rmap[vv-128] ) ;
                rgb[3*ii+1] += (byte)( opa * ar->gmap[vv-128] ) ;
                rgb[3*ii+2] += (byte)( opa * ar->bmap[vv-128] ) ;
             }

          }
        }
        break ;

        case CREN_MIP_VOX:
          for( ii=0 ; ii < nxy ; ii++ ){
             vv = var[ii] ;
                  if( vv < 128  ) vv = vv << 1 ;          /* gray  */
             else if( vv < vtop ) vv = ar->imap[vv-128] ; /* color */
             else                 continue ;              /* skip  */

             if( vv > rgb[3*ii] ) rgb[3*ii] = vv ;        /* MIP   */
          }
        break ;

        case CREN_MIP_OPA:{
          float opa ;
          for( ii=0 ; ii < nxy ; ii++ ){
             vv = var[ii] ;
                  if( vv < 128  ) opa = ar->opamap[vv] ;  /* gray  */
             else if( vv < vtop ) opa = ar->opargb ;      /* color */
             else                 continue ;              /* skip  */

             vv = (byte)(255.9*opa) ;                     /* scale */
             if( vv > rgb[3*ii] ) rgb[3*ii] = vv ;        /* MIP   */
          }
        }
        break ;

     } /* end of switch over rendering mode */

   } /* end of loop over levels */

   /*-- finalization --*/

   switch( ar->renmode ){
     default:
     case CREN_SUM_VOX:
        free(used) ;
     break ;

     case CREN_MIP_VOX:  /* fill in missing GB values */
     case CREN_MIP_OPA:
        for( ii=0 ; ii < nxy ; ii++ )
           rgb[3*ii+1] = rgb[3*ii+2] = rgb[3*ii] ;
     break ;
   }

   return im ;
}
#endif
