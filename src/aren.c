#include "aren.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*============================================================================
  AREN = AFNI Renderer, a set of routines for volume rendering 3D bricks.
  Built on top of VolPack, by Philippe Lacroute.

  (1) Use new_AREN_renderer() to create a renderer.
  (2) Load grayscale or color bricks into it with functions
      AREN_set_graybrick() or AREN_set_rgbbricks().
  (3) Load the opacity brick with AREN_set_opabrick().
  (4) Set the viewing angles with AREN_set_viewpoint().
      Set the brick dimensions with AREN_set_size().
  (5) Create an image with AREN_render().
      Loop back through (2-4) as needed to create new images.
  (6) When finished, use destroy_AREN_renderer() to clean up.
==============================================================================*/

/*--------------------------------------------------------------------------
  Initialize the short -> float color table, and
             the byte  -> float gray/opacity table for VolPack.
  This routine only does something the first time it is called -- it
  creates space for the color tables and fills them.  They will never
  be deallocated.

  N.B.: The documentation for VolPack is incorrect -- the color values
        should be in the range 0..255, NOT 0..1.  The opacity values
        should still be in the range 0..1.
----------------------------------------------------------------------------*/

static float * AREN_colortable = NULL ;
static float * AREN_graytable  = NULL ;
static float * AREN_opatable   = NULL ;

void init_AREN_colortable( void )
{
   static int been_here = 0 ;
   int ii , rr,gg,bb , ss ;

   if( been_here ) return ;  /* done that */

   AREN_colortable = (float *) malloc( sizeof(float) * TOT_COLORS * 3 ) ;
   AREN_graytable  = (float *) malloc( sizeof(float) * MAX_GRAYS ) ;
   AREN_opatable   = (float *) malloc( sizeof(float) * MAX_GRAYS ) ;

   /*-- load linear ramp for grayscale and opacity --*/

   for( ii=0 ; ii < MAX_GRAYS ; ii++ ){
      AREN_graytable[ii] = ii ;
      AREN_opatable[ii]  = ii / 255.0 ;
   }

   /*-- load a 32 x 32 x 32 color cube --*/

   for( rr=0 ; rr < MAX_CDIM ; rr++ ){
      for( gg=0 ; gg < MAX_CDIM ; gg++ ){
         for( bb=0 ; bb < MAX_CDIM ; bb++ ){

            ss = FIVE_to_short(rr,gg,bb) ;   /* color index */

            AREN_colortable[3*ss  ] = (rr * 255.0) / 31.0 ;
            AREN_colortable[3*ss+1] = (gg * 255.0) / 31.0 ;
            AREN_colortable[3*ss+2] = (bb * 255.0) / 31.0 ;
         }
      }
   }

   /*-- at the end, add the pure grays (at a higher resolution) --*/

   ss = 3 * MAX_COLORS ;
   for( ii=0 ; ii < MAX_GRAYS ; ii++ ){
      AREN_colortable[ss++] = ii ;
      AREN_colortable[ss++] = ii ;
      AREN_colortable[ss++] = ii ;
   }

   been_here = 1 ;
   return ;
}

/*--------------------------------------------------------------------------
  Create a new AFNI renderer.
  The return pointer is passed to all other AREN routines.
----------------------------------------------------------------------------*/

#define DEFAULT_THETA 130.0
#define DEFAULT_PHI   285.0

void * new_AREN_renderer( void )
{
   AREN_stuff * ar ;

   ar = (AREN_stuff *) malloc( sizeof(AREN_stuff) ) ;
   ar->type = AREN_TYPE ;

   init_AREN_colortable() ;  /* only does something 1st time in */

   /*-- initialize VolPack --*/

   ar->vpc = vpCreateContext() ;

   vpSeti( ar->vpc , VP_CONCAT_MODE , VP_CONCAT_LEFT ) ;

   vpCurrentMatrix( ar->vpc , VP_MODEL ) ;
   vpIdentityMatrix( ar->vpc ) ;

   vpCurrentMatrix( ar->vpc , VP_VIEW ) ;
   vpIdentityMatrix( ar->vpc ) ;
   vpRotate( ar->vpc , VP_Y_AXIS , DEFAULT_THETA ) ;
   vpRotate( ar->vpc , VP_X_AXIS , DEFAULT_PHI   ) ;

#undef USE_CUEING
#ifdef USE_CUEING
   vpSetDepthCueing( ar->vpc , 1.0 , 0.5 ) ;
   vpEnable( ar->vpc , VP_DEPTH_CUE , 1 ) ;
#endif

   vpCurrentMatrix( ar->vpc , VP_PROJECT ) ;
   vpIdentityMatrix( ar->vpc ) ;
   vpWindow( ar->vpc , VP_PARALLEL , -0.5,0.5 , -0.5,0.5 , -0.5,0.5 ) ;

   /*-- initialize the rest of the data --*/

   ar->nx = ar->ny = ar->nz = ar->verbose = ar->newdata = ar->newvox = 0 ;
   ar->sx = ar->sy = ar->sz = 1.0 ;

   ar->theta = DEFAULT_THETA ;
   ar->phi   = DEFAULT_PHI ;
   ar->grim  = ar->rim = ar->gim = ar->bim = ar->opim = NULL ;
   ar->vox   = NULL ;

   return (void *) ar ;
}

/*-----------------------------------------------------------------------------
   Get rid of an AFNI renderer.
-------------------------------------------------------------------------------*/

void destroy_AREN_renderer( void * ah )
{
   AREN_stuff * ar = (AREN_stuff *) ah ;

   if( !ISVALID_AREN(ar) ) return ;

   if( ar->vox != NULL ) free(ar->vox) ;
   vpDestroyContext( ar->vpc ) ;
   free(ar) ;
   return ;
}

/*-----------------------------------------------------------------------------
   For debugging purposes
-------------------------------------------------------------------------------*/

void AREN_be_verbose( void * ah )
{
   AREN_stuff * ar = (AREN_stuff *) ah ;
   if( !ISVALID_AREN(ar) ) return ;
   ar->verbose = 1 ; return ;
}

void AREN_be_quiet( void * ah )
{
   AREN_stuff * ar = (AREN_stuff *) ah ;
   if( !ISVALID_AREN(ar) ) return ;
   ar->verbose = 0 ; return ;
}

/*-----------------------------------------------------------------------------
   Set the grayscale brick in a renderer.
   Returns -1 if an error occurs, otherwise returns 0.
   Note that this brick is NOT free-d by AREN at any point -- that is
   the user's responsibility.
-------------------------------------------------------------------------------*/

int AREN_set_graybrick( void * ah , MRI_IMAGE * grim )
{
   AREN_stuff * ar = (AREN_stuff *) ah ;
   int newvox=0 , nvox,ii ;
   byte    * gar ;
   rgbvox  * gvox ;
   byte      gb ;

   /*-- sanity checks --*/

   if( !ISVALID_AREN(ar) || grim == NULL || grim->kind != MRI_byte ) return -1 ;

   if( grim->nx < 3 || grim->ny < 3 || grim->nz < 3 ){
      fprintf(stderr,"**AREN: illegal dimensions for a gray brick\n") ;
      return -1 ;
   }

   /*-- if had old color bricks, toss them (or their pointers) --*/

   if( ar->rim != NULL ){
      ar->rim = ar->gim = ar->bim = NULL ;
      if( ar->verbose )
         fprintf(stderr,"--AREN: switching from color bricks to gray brick\n") ;
   } else {
      if( ar->verbose )
         fprintf(stderr,"--AREN: input a new gray brick\n") ;
   }

   /*-- if have new dimensions, toss old stuff that doesn't match --*/

   if( ar->nx > 0 &&
       ( ar->nx != grim->nx || ar->ny != grim->ny || ar->nz != grim->nz ) ){

      ar->opim = NULL ;
      if( ar->vox != NULL ){ free(ar->vox) ; ar->vox = NULL ; }

      if( ar->verbose )
         fprintf(stderr,"--AREN: new gray brick changes volume dimensions\n"
                        "        nx:%d->%d  ny:%d->%d  nz:%d->%d\n",
                        ar->nx,grim->nx , ar->ny,grim->ny , ar->nz,grim->nz ) ;
   }

   /*-- set dimensions --*/

   ar->grim = grim ;
   ar->nx   = grim->nx ;
   ar->ny   = grim->ny ;
   ar->nz   = grim->nz ; nvox = ar->nx * ar->ny * ar->nz ;

   /*-- if need be, allocate a voxel array to hold the data --*/

   if( ar->vox == NULL ){
      ar->newvox = newvox = 1 ;
      ar->vox = (rgbvox *) malloc( sizeof(rgbvox) * nvox ) ;
      if( ar->vox == NULL ){
         fprintf(stderr,"**AREN: can't malloc workspace with new gray brick\n") ;
         return -1 ;
      } else if( ar->verbose ){
         fprintf(stderr,"--AREN: allocated new voxel array\n") ;
      }
   }

   /*-- copy grayscale data into voxel array --*/

   gvox = ar->vox ;
   gar  = MRI_BYTE_PTR(grim) ;
   for( ii=0 ; ii < nvox ; ii++ ) gvox[ii].rgb = (unsigned short) gar[ii] ;

   ar->newdata = 1 ;
   return 0 ;
}

/*-----------------------------------------------------------------------------
   Set the opacity brick in a renderer.
   Returns -1 if an error occurs, otherwise returns 0.
   Note that this brick is NOT free-d by AREN at any point -- that is
   the user's responsibility.
-------------------------------------------------------------------------------*/

int AREN_set_opabrick( void * ah , MRI_IMAGE * opim )
{
   AREN_stuff * ar = (AREN_stuff *) ah ;
   int nvox,ii , newvox ;
   byte    * gar ;
   rgbvox  * rvox ;

   /*-- sanity checks --*/

   if( !ISVALID_AREN(ar) || opim == NULL || opim->kind != MRI_byte ) return -1 ;

   if( opim->nx < 3 || opim->ny < 3 || opim->nz < 3 ){
      fprintf(stderr,"**AREN: illegal dimensions for an opacity brick\n") ;
      return -1 ;
   }

   /*-- if have new dimensions, toss old stuff that doesn't match --*/

   if( ar->nx > 0 &&
       ( ar->nx != opim->nx || ar->ny != opim->ny || ar->nz != opim->nz ) ){

      ar->grim = ar->rim = ar->gim = ar->bim = NULL ;
      if( ar->vox != NULL ){ free(ar->vox) ; ar->vox = NULL ; }

      if( ar->verbose )
         fprintf(stderr,"--AREN: new opacity brick changes volume dimensions\n"
                        "        nx:%d->%d  ny:%d->%d  nz:%d->%d\n",
                        ar->nx,opim->nx , ar->ny,opim->ny , ar->nz,opim->nz ) ;
   } else {
      if( ar->verbose )
         fprintf(stderr,"--AREN: new opacity brick\n") ;
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
         fprintf(stderr,"**AREN: can't malloc workspace with new opacity brick\n") ;
         return -1 ;
      } else if( ar->verbose ){
         fprintf(stderr,"--AREN: allocated new voxel array\n") ;
      }
   }

   /*-- load the opacity into voxel array --*/

   gar  = MRI_BYTE_PTR(ar->opim) ;
   rvox = ar->vox ;
   for( ii=0 ; ii < nvox ; ii++ ) rvox[ii].alpha = (unsigned short) gar[ii] ;

   ar->newdata = 1 ;
   return 0 ;
}

/*-----------------------------------------------------------------------------
   Set the color bricks in a renderer.
   Returns -1 if an error occurs, otherwise returns 0.
   Note that these bricks are NOT free-d by AREN at any point -- that is
   the user's responsibility.
-------------------------------------------------------------------------------*/

int AREN_set_rgbbricks( void * ah , MRI_IMAGE *rim, MRI_IMAGE *gim, MRI_IMAGE *bim )
{
   AREN_stuff * ar = (AREN_stuff *) ah ;
   int newvox=0 , nvox,ii ;
   byte    * rar , * gar , * bar ;
   rgbvox  * rvox ;
   byte      rb,gb,bb ;

   /*-- sanity checks --*/

   if( rim == NULL || rim->kind != MRI_byte ||
       gim == NULL || gim->kind != MRI_byte ||
       bim == NULL || bim->kind != MRI_byte || !ISVALID_AREN(ar) ) return -1 ;

   if( rim->nx < 3 || rim->ny < 3 || rim->nz < 3 ){
      fprintf(stderr,"**AREN: illegal dimensions for a color brick\n") ; return -1 ;
   }

   if( gim->nx != rim->nx || gim->ny != rim->ny || gim->nz != rim->nz ){
      fprintf(stderr,"**AREN: green & red rgb bricks don't match\n") ; return -1 ;
   }

   if( bim->nx != rim->nx || bim->ny != rim->ny || bim->nz != rim->nz ){
      fprintf(stderr,"**AREN: blue & red rgb bricks don't match\n") ; return -1 ;
   }

   /*-- if had an old gray brick, toss it (or at least its pointer) --*/

   if( ar->grim != NULL ){
      ar->grim = NULL ;
      if( ar->verbose )
         fprintf(stderr,"--AREN: switching from gray brick to color bricks\n") ;
   } else {
      if( ar->verbose )
         fprintf(stderr,"--AREN: input new color bricks\n") ;
   }

   /*-- if have new dimensions, toss old stuff that doesn't match --*/

   if( ar->nx > 0 &&
       ( ar->nx != rim->nx || ar->ny != rim->ny || ar->nz != rim->nz ) ){

      ar->opim = NULL ;
      if( ar->vox != NULL ){ free(ar->vox) ; ar->vox = NULL ; }

      if( ar->verbose )
         fprintf(stderr,"--AREN: new color bricks change volume dimensions\n"
                        "        nx:%d->%d  ny:%d->%d  nz:%d->%d\n",
                        ar->nx,rim->nx , ar->ny,rim->ny , ar->nz,rim->nz ) ;
   }

   /*-- set dimensions --*/

   ar->rim = rim ; ar->gim = gim ; ar->bim = bim ;
   ar->nx  = rim->nx ;
   ar->ny  = rim->ny ;
   ar->nz  = rim->nz ; nvox = ar->nx * ar->ny * ar->nz ;

   /*-- if need be, allocate a voxel array to hold the data --*/

   if( ar->vox == NULL ){
      ar->newvox = newvox = 1 ;
      ar->vox = (rgbvox *) malloc( sizeof(rgbvox) * nvox ) ;
      if( ar->vox == NULL ){
         fprintf(stderr,"**AREN: can't malloc workspace with new color bricks\n") ;
         return -1 ;
      } else if( ar->verbose ){
         fprintf(stderr,"--AREN: allocated new voxel array\n") ;
      }
   }

   /*-- copy color data into voxel array --*/

   rvox = ar->vox ;
   rar  = MRI_BYTE_PTR(rim) ;
   gar  = MRI_BYTE_PTR(gim) ;
   bar  = MRI_BYTE_PTR(bim) ;
   for( ii=0 ; ii < nvox ; ii++ ){
      rb = EIGHT_TO_FIVE(rar[ii]) ;
      gb = EIGHT_TO_FIVE(gar[ii]) ;
      bb = EIGHT_TO_FIVE(bar[ii]) ;

      if( rb == gb && rb == bb ){
         rvox[ii].rgb = MAX_COLORS + rar[ii] ;         /* index into grayscale */
      } else {
         rvox[ii].rgb = FIVE_to_short( rb , gb, bb ) ; /* index into color cube */
      }
   }

   ar->newdata = 1 ;
   return 0 ;
}

/*-------------------------------------------------------------------------------
   Set the viewpoint of the user (in polar angles)
---------------------------------------------------------------------------------*/

void AREN_set_viewpoint( void * ah , float theta , float phi )
{
   AREN_stuff * ar = (AREN_stuff *) ah ;

   if( !ISVALID_AREN(ar) ) return ;

   ar->theta = theta ; ar->phi = phi ;

   vpCurrentMatrix( ar->vpc , VP_VIEW ) ;
   vpIdentityMatrix( ar->vpc ) ;
   vpRotate( ar->vpc , VP_Y_AXIS , theta ) ;
   vpRotate( ar->vpc , VP_X_AXIS , phi   ) ;

   if( ar->verbose )
      fprintf(stderr,"--AREN: set theta=%f  phi=%f\n",theta,phi) ;

   return ;
}

/*-------------------------------------------------------------------------------
   Set the scale factors for each axis (default = 1).  The inputs should
   be the size of the brick along each axes (e.g., sx = nx * dx).  This is
   needed because VolPack assumes the input data is in a unit cube, but
   our data may not be so uniform.

   N.B.: This is disabled, since VolPack doesn't seem to work in this regards.
---------------------------------------------------------------------------------*/

void AREN_set_size( void * ah , float sx , float sy , float sz )
{
#if 0
   AREN_stuff * ar = (AREN_stuff *) ah ;
   float mmm ;

   if( !ISVALID_AREN(ar) ) return ;

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
      fprintf(stderr,"--AREN: set scale factors = %f %f %f\n",ar->sx,ar->sy,ar->sz) ;
#endif

   return ;
}

#undef USE_CALLBACKS
#ifdef USE_CALLBACKS
/*------------------------------------------------------------------------------
   Routines that can be used for callback shading, vs. lookup table shading.
   Mostly for debugging purposes.
--------------------------------------------------------------------------------*/

static int ncall = 0 ;

void AREN_grayfunc( void * vp , float * g , void * cd )
{
   rgbvox * rv = (rgbvox *) vp ;

   *g = AREN_graytable[ rv->rgb ] ;

#if 0
   ncall++ ;
   if( ncall%1000 == 0 && rv->rgb > 0 )
      fprintf(stderr,"Grayfunc: rgb=%d g=%f\n",(int)(rv->rgb) , *g ) ;
#endif

   return ;
}

void AREN_colorfunc( void * vp , float * r , float * g , float * b , void * cd )
{
   rgbvox * rv = (rgbvox *) vp ;
   *r = AREN_colortable[ 3*(rv->rgb)   ] ;
   *g = AREN_colortable[ 3*(rv->rgb)+1 ] ;
   *b = AREN_colortable[ 3*(rv->rgb)+2 ] ;

#if 0
   ncall++ ;
   if( ncall%1000 == 0 && rv->rgb > 0 )
      fprintf(stderr,"Colorfunc: rgb=%d r=%f g=%f b=%f\n",
                     (int)(rv->rgb) , *r,*g,*b ) ;
#endif

   return ;
}
#endif /* USE_CALLBACKS */

/*-------------------------------------------------------------------------------
   Actually render an image.  Returns NULL if an error occurs.
   Input npix = number of pixels on a side of the image (always will be square).
   If rendering a grayscale brick, returns an image of kind MRI_byte.
   If rendering RGB bricks, returns an image of kind MRI_rgb.
---------------------------------------------------------------------------------*/

MRI_IMAGE * AREN_render( void * ah , int npix )
{
   AREN_stuff * ar = (AREN_stuff *) ah ;
   int isgray , isrgb ;
   MRI_IMAGE * im ;
   byte * imar ;
   vpResult fred ;

   /*-- sanity checks --*/

   if( !ISVALID_AREN(ar) ) return NULL ;

   if( npix < 16 ){
      fprintf(stderr,"**AREN: attempt to render with less than 16 pixels!\n") ;
      return NULL ;
   }

   isgray = (ar->grim != NULL) ;
   isrgb  = (ar->rim  != NULL) ;

   if( isgray && isrgb ){
      fprintf(stderr,"**AREN: attempt to render gray and color simultaneously?\n");
      return NULL ;
   }

   if( (!isgray && !isrgb) || ar->vox == NULL ){
      fprintf(stderr,"**AREN: attempt to render without data being loaded!\n") ;
      return NULL ;
   }

   if( ar->opim == NULL ){
      fprintf(stderr,"**AREN: attempt to render without opacity being loaded!\n") ;
      return NULL ;
   }

   if( ar->nx < 3 || ar->ny < 3 || ar->nz < 3 ){
      fprintf(stderr,"**AREN: attempt to render without initialization!\n") ;
      return NULL ;
   }

   /*-- if have new voxel array, must tell VolPack all about it --*/

   if( ar->newvox ){
      int nvox = ar->nx * ar->ny * ar->nz ;
      rgbvox vv , *rv = &vv ;

      if( ar->verbose )
         fprintf(stderr,"--AREN: setting up new voxel array\n") ;

      /* 3D dimensions */

      fred = vpSetVolumeSize( ar->vpc , ar->nx , ar->ny , ar->nz ) ;
      if( fred != VP_OK ){
         fprintf(stderr,"**AREN: vpSetVolumeSize failed: code=%d\n",(int)fred) ;
         return NULL ;
      }

      /* each voxel has 2 data fields; 1 for shading and 1 for opacity */

      fred = vpSetVoxelSize( ar->vpc , sizeof(rgbvox) , 2 , 1 , 1 ) ;
      if( fred != VP_OK ){
         fprintf(stderr,"**AREN: vpSetVoxelSize failed: code=%d\n",(int)fred) ;
         return NULL ;
      }

      /* gray scale input */

      if( isgray ){

         /* voxel field 0 (rgb) is an index into AREN_graytable */

         fred = vpSetVoxelField( ar->vpc, 0, sizeof(short),
                                 vpFieldOffset(rv,rgb), MAX_GRAYS-1 );
         if( fred != VP_OK ){
            fprintf(stderr,"**AREN: vpSetVoxelField(0) failed: code=%d\n",(int)fred) ;
            return NULL ;
         }

         /* voxel field 1 (alpha) is an index into AREN_opatable */

         fred = vpSetVoxelField( ar->vpc, 1, sizeof(short),
                                 vpFieldOffset(rv,alpha), MAX_GRAYS-1 );
         if( fred != VP_OK ){
            fprintf(stderr,"**AREN: vpSetVoxelField(1) failed: code=%d\n",(int)fred) ;
            return NULL ;
         }

         /* tell VolPack where the voxels are */

         fred = vpSetRawVoxels( ar->vpc , ar->vox ,
                                sizeof(rgbvox)*nvox ,
                                sizeof(rgbvox) ,
                                sizeof(rgbvox)*(ar->nx) ,
                                sizeof(rgbvox)*(ar->nx * ar->ny) ) ;
         if( fred != VP_OK ){
            fprintf(stderr,"**AREN: vpSetRawVoxels failed: code=%d\n",(int)fred) ;
            return NULL ;
         }

#ifndef USE_CALLBACKS

         /* voxel field 0 (rgb) is an index into AREN_graytable */

         fred = vpSetLookupShader( ar->vpc , 1 , 1 , 0 ,
                                   AREN_graytable , sizeof(float)*MAX_GRAYS ,
                                   0 , NULL , 0 ) ;
         if( fred != VP_OK ){
            fprintf(stderr,"**AREN: vpSetLookupShader failed: code=%d\n",(int)fred) ;
            return NULL ;
         }
#else
         fred = vpSetCallback( ar->vpc , VP_GRAY_SHADE_FUNC , AREN_grayfunc ) ;
         if( fred != VP_OK ){
            fprintf(stderr,"**AREN: vpSetCallback(GRAY) failed: code=%d\n",(int)fred) ;
            return NULL ;
         }
#endif

      /* color input */

      } else if( isrgb ){

         /* voxel field 0 (rgb) is an index into AREN_colortable */

         fred = vpSetVoxelField( ar->vpc, 0, sizeof(short),
                                 vpFieldOffset(rv,rgb), TOT_COLORS-1 );
         if( fred != VP_OK ){
            fprintf(stderr,"**AREN: vpSetVoxelField(0) failed: code=%d\n",(int)fred) ;
            return NULL ;
         }

         /* voxel field 1 (alpha) is an index into AREN_opatable */

         fred = vpSetVoxelField( ar->vpc, 1, sizeof(short) ,
                                 vpFieldOffset(rv,alpha), MAX_GRAYS-1 );
         if( fred != VP_OK ){
            fprintf(stderr,"**AREN: vpSetVoxelField(1) failed: code=%d\n",(int)fred) ;
            return NULL ;
         }

         /* tell VolPack where the voxels are */

         fred = vpSetRawVoxels( ar->vpc , ar->vox ,
                                sizeof(rgbvox)*nvox ,
                                sizeof(rgbvox) ,
                                sizeof(rgbvox)*(ar->nx) ,
                                sizeof(rgbvox)*(ar->nx * ar->ny) ) ;
         if( fred != VP_OK ){
            fprintf(stderr,"**AREN: vpSetRawVoxels failed: code=%d\n",(int)fred) ;
            return NULL ;
         }

#ifndef USE_CALLBACKS

         /* voxel field 0 (rgb) is an index into AREN_colortable */

         fred = vpSetLookupShader( ar->vpc , 3 , 1 , 0 ,
                                   AREN_colortable , sizeof(float)*TOT_COLORS*3 ,
                                   0 , NULL , 0 ) ;
         if( fred != VP_OK ){
            fprintf(stderr,"**AREN: vpSetLookupShader failed: code=%d\n",(int)fred) ;
            return NULL ;
         }
#else
         fred = vpSetCallback( ar->vpc , VP_RGB_SHADE_FUNC , AREN_colorfunc ) ;
         if( fred != VP_OK ){
            fprintf(stderr,"**AREN: vpSetCallback(COLOR) failed: code=%d\n",(int)fred) ;
            return NULL ;
         }
#endif
      }

      /* voxel field 1 (alpha) is an index into AREN_opatable */

      fred = vpSetClassifierTable( ar->vpc, 0, 1, AREN_opatable, sizeof(float)*MAX_GRAYS ) ;
      if( fred != VP_OK ){
         fprintf(stderr,"**AREN: vpSetClassifierTable failed: code=%d\n",(int)fred) ;
         return NULL ;
      }

      /* threshold for octree bins: 12 = 5% of opacity range */

      fred = vpMinMaxOctreeThreshold( ar->vpc , 0 , 12 ) ;
      if( fred != VP_OK ){
         fprintf(stderr,"**AREN: vpMinMaxOctreeThreshold failed: code=%d\n",(int)fred) ;
         return NULL ;
      }

      ar->newvox = 0 ;
   }

   /*-- if have new data in the voxel array, must recreate the octree --*/

#if 1
   vpSetd( ar->vpc , VP_MAX_RAY_OPACITY   , 0.95 ) ;
   vpSetd( ar->vpc , VP_MIN_VOXEL_OPACITY , 0.05 ) ;
#endif

   if( ar->newdata ){

#undef USE_CLASSIFIED  /* for using the "classified" volume feature of VolPack */

#ifndef USE_CLASSIFIED
      if( ar->verbose )
         fprintf(stderr,"--AREN: computing octree\n") ;

      /* make octree, down to 4x4x4 voxel bins */

      fred = vpCreateMinMaxOctree( ar->vpc , 0 , 4 ) ;
      if( fred != VP_OK ){
         fprintf(stderr,"**AREN: vpCreateMinMaxOctree failed: code=%d\n",(int)fred) ;
         return NULL ;
      }
#else
      if( ar->verbose )
         fprintf(stderr,"--AREN: computing classified volume\n") ;

      /* classify volume (slower than octree-ifying) */

      fred = vpClassifyVolume( ar->vpc ) ;
      if( fred != VP_OK ){
         fprintf(stderr,"**AREN: vpClassifyVolume failed: code=%d\n",(int)fred) ;
         return NULL ;
      }
#endif

      ar->newdata = 0 ;
   }

   /*-- create the output image --*/

#undef GET_ALPHA  /* for debugging: compute the opacity image */

   if( isgray ){
      im   = mri_new( npix , npix , MRI_byte ) ;
      imar = MRI_BYTE_PTR(im) ;
#ifndef GET_ALPHA
      vpSetImage( ar->vpc , imar , npix,npix,npix , VP_LUMINANCE ) ;
#else
      vpSetImage( ar->vpc , imar , npix,npix,npix , VP_ALPHA ) ;
#endif
   } else if( isrgb ){
#ifndef GET_ALPHA
      im   = mri_new( npix , npix , MRI_rgb ) ;
      imar = MRI_RGB_PTR(im) ;
      vpSetImage( ar->vpc , imar , npix,npix,3*npix , VP_RGB ) ;
#else
      im   = mri_new( npix , npix , MRI_byte ) ;
      imar = MRI_BYTE_PTR(im) ;
      vpSetImage( ar->vpc , imar , npix,npix,npix , VP_ALPHA ) ;
#endif
   }

   if( ar->verbose )
      fprintf(stderr,"--AREN: rendering image\n") ;

#ifdef USE_CLASSIFIED
   fred = vpRenderClassifiedVolume(ar->vpc) ;
   if( fred != VP_OK ){
      fprintf(stderr,"**AREN: vpRenderClassifiedVolume failed: code=%d\n",(int)fred) ;
      mri_free(im) ; return NULL ;
   }
#else
   fred = vpRenderRawVolume(ar->vpc) ;
   if( fred != VP_OK ){
      fprintf(stderr,"**AREN: vpRenderRawVolume failed: code=%d\n",(int)fred) ;
      mri_free(im) ; return NULL ;
   }
#endif

   return im ;
}
