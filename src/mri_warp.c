#include "mrilib.h"

/*** NOT 7D SAFE ***/

/*** functions to "warp" images -- not very efficient, but quite general ***/

#ifndef MRI_DEBUG
#ifdef HP
#pragma INLINE  __MRI_scaler
#pragma INLINE  mri_warp
#pragma INLINE  mri_warp_bilinear
#pragma INLINE  mri_warp_bicubic
#pragma INLINE  __MRI_rotfunc
#pragma INLINE  mri_warp_bicubic_point
#endif
#endif

#define FINS(i,j) (  ( (i)<0 || (j)<0 || (i)>=nx || (j)>=ny ) \
                     ? 0.0 : far[(i)+(j)*nx] )

/**************************************************************************/

static float sx_scale , sy_scale ;  /* global scaler data */

void __MRI_scaler( float xpr, float ypr, float *xx , float *yy )
{
   *xx = sx_scale * xpr ;
   *yy = sy_scale * ypr ;
   return ;
}

/**************************************************************************/

MRI_IMAGE *mri_warp( MRI_IMAGE *im , int nxnew , int nynew , int wtype ,
                     void wfunc(float,float,float *,float *)            )
{
   switch( wtype ){
      case MRI_BILINEAR:
         return mri_warp_bilinear( im , nxnew , nynew , wfunc ) ;

      case MRI_BICUBIC:
         return mri_warp_bicubic( im , nxnew , nynew , wfunc ) ;

      default:
         fprintf( stderr , "mri_warp: illegal wtype %d\n" , wtype ) ;
         MRI_FATAL_ERROR ;
   }
   return NULL ;
}

/****************************************************************************/

MRI_IMAGE *mri_resize( MRI_IMAGE *im , int nxnew , int nynew )
{
   int nx,ny , nnx,nny , wtype ;

   nx  = im->nx ;  ny  = im->ny ;
   nnx = nxnew  ;  nny = nynew  ;

   if( nnx <= 0 && nny <= 0 ){
      fprintf( stderr , "mri_resize: nxnew,nynew = %d %d\n",nxnew,nynew ) ;
      MRI_FATAL_ERROR ;
   }

   sx_scale = (nnx > 0) ? ((float)nx)/nnx : 0.0 ;
   sy_scale = (nny > 0) ? ((float)ny)/nny : 0.0 ;

   if( nnx <= 0 ){
      sx_scale = sy_scale ;
      nnx      = sx_scale * nx ;
   } else if( nny <= 0 ){
      sy_scale = sx_scale ;
      nny      = sy_scale * ny ;
   }


#if 0
   wtype = MRI_BICUBIC ;

   if( ( ((nnx>=nx) && (nnx%nx==0)) || ((nnx<nx) && (nx%nnx==0)) ) &&
       ( ((nny>=ny) && (nny%ny==0)) || ((nny<ny) && (ny%nny==0)) )   ){

      wtype = MRI_BILINEAR ;
   } else {
      wtype = MRI_BICUBIC ;
   }

   return mri_warp( im , nnx,nny , wtype , __MRI_scaler ) ;
#else
   return mri_warp_bicubic( im , nnx,nny , __MRI_scaler ) ;
#endif
}

/**************************************************************************/

MRI_IMAGE *mri_warp_bicubic( MRI_IMAGE *im , int nxnew , int nynew ,
                                     void wf( float,float,float *,float *) )
{
   MRI_IMAGE *imfl , *new ;
   float *far , *nar ;
   float xpr,ypr , xx,yy , fx,fy ;
   int ii,jj, nx,ny , ix,jy ;
   float f_jm1,f_j00,f_jp1,f_jp2 , wt_m1,wt_00,wt_p1,wt_p2 ;

   nx = im->nx ;  /* input image dimensions, for convenience */
   ny = im->ny ;

   nxnew = (nxnew > 0) ? nxnew : nx ;  /* default output image sizes */
   nynew = (nynew > 0) ? nynew : ny ;

   if( im->kind == MRI_float ){    /* convert input to float, if needed */
      imfl = im ;
   } else {
      imfl = mri_to_float( im ) ;
   }
   far = mri_data_pointer( imfl ) ;  /* easy access to float data */

   new = mri_new( nxnew , nynew , MRI_float ) ;   /* output image */
   nar = mri_data_pointer( new ) ;                /* output image data */

   /*** loop over output points and warp to them ***/

   for( jj=0 ; jj < nynew ; jj++ ){
      ypr = jj ;
      for( ii=0 ; ii < nxnew ; ii++ ){
         xpr = ii ;
         wf( xpr,ypr , &xx,&yy ) ;  /* get xx,yy in original image */

         ix = floor( xx ) ;  fx = xx - ix ;   /* integer and       */
         jy = floor( yy ) ;  fy = yy - jy ;   /* fractional coords */

/* define cubic interpolation polynomials and data from original grid */

#define P_M1(x)  (-(x)*((x)-1)*((x)-2))
#define P_00(x)  (3*((x)+1)*((x)-1)*((x)-2))
#define P_P1(x)  (-3*(x)*((x)+1)*((x)-2))
#define P_P2(x)  ((x)*((x)+1)*((x)-1))

         wt_m1 = P_M1(fx) ;  wt_00 = P_00(fx) ;  /* weights for interpolating */
         wt_p1 = P_P1(fx) ;  wt_p2 = P_P2(fx) ;  /* in the x-direction        */

         f_jm1 =  wt_m1 * FINS(ix-1,jy-1)        /* interpolate to ix + fx */
                + wt_00 * FINS(ix  ,jy-1)        /* at levels jy-1 .. jy+2 */
                + wt_p1 * FINS(ix+1,jy-1)
                + wt_p2 * FINS(ix+2,jy-1) ;

         f_j00 =   wt_m1 * FINS(ix-1,jy)
                 + wt_00 * FINS(ix  ,jy)
                 + wt_p1 * FINS(ix+1,jy)
                 + wt_p2 * FINS(ix+2,jy) ;

         f_jp1 =   wt_m1 * FINS(ix-1,jy+1)
                 + wt_00 * FINS(ix  ,jy+1)
                 + wt_p1 * FINS(ix+1,jy+1)
                 + wt_p2 * FINS(ix+2,jy+1) ;

         f_jp2 =   wt_m1 * FINS(ix-1,jy+2)
                 + wt_00 * FINS(ix  ,jy+2)
                 + wt_p1 * FINS(ix+1,jy+2)
                 + wt_p2 * FINS(ix+2,jy+2) ;

         /* interpolate between y-levels to jy+fy */

         nar[ii+jj*nxnew] = (  P_M1(fy) * f_jm1 + P_00(fy) * f_j00
                             + P_P1(fy) * f_jp1 + P_P2(fy) * f_jp2 ) / 36.0 ;

      }
   }

   /*** cleanup and return ***/

   if( im != imfl ) mri_free(imfl) ;  /* throw away unneeded workspace */
   return new ;
}

/*************************************************************************/

MRI_IMAGE *mri_warp_bilinear( MRI_IMAGE *im , int nxnew , int nynew ,
                                  void wf( float,float,float *,float *) )
{
   MRI_IMAGE *imfl , *new ;
   float *far , *nar ;
   float xpr,ypr , xx,yy , fx,fx1,fy,fy1 , f00,f10,f01,f11 ;
   int ii,jj, nx,ny , ix,jy ;

   nx = im->nx ;  /* dimensions, for convenience */
   ny = im->ny ;

   nxnew = (nxnew > 0) ? nxnew : nx ;  /* default output image sizes */
   nynew = (nynew > 0) ? nynew : ny ;

   if( im->kind == MRI_float ){    /* convert input to float, if needed */
      imfl = im ;
   } else {
      imfl = mri_to_float( im ) ;
   }
   far = mri_data_pointer( imfl ) ;  /* easy access to float data */

   new = mri_new( nxnew , nynew , MRI_float ) ;   /* output image */
   nar = mri_data_pointer( new ) ;                /* output image data */

   /*** loop over output points and warp to them ***/

   for( jj=0 ; jj < nynew ; jj++ ){
      ypr = jj ;
      for( ii=0 ; ii < nxnew ; ii++ ){
         xpr = ii ;
         wf( xpr,ypr , &xx,&yy ) ;  /* get xx,yy in original image */

         ix = floor( xx ) ;  fx = xx - ix ;  fx1 = 1.0 - fx ;
         jy = floor( yy ) ;  fy = yy - jy ;  fy1 = 1.0 - fy ;

         f00 = FINS(ix  ,jy  ) ;
         f01 = FINS(ix+1,jy  ) ;
         f10 = FINS(ix  ,jy+1) ;
         f11 = FINS(ix+1,jy+1) ;

         nar[ii+jj*nxnew] = fx1 * ( fy1 * f00 + fy * f01 )
                           +fx  * ( fy1 * f10 + fy * f11 ) ;

      }
   }

   /*** cleanup and return ***/

   if( im != imfl ) mri_free(imfl) ;  /* throw away unneeded workspace */
   return new ;
}

/**********************************************************************/

static float rot_dx , rot_dy , rot_cph , rot_sph ;    /* global rotfunc data */

void __MRI_rotfunc( float xpr , float ypr , float *xx , float *yy )
{
   *xx =  rot_cph * xpr + rot_sph * ypr + rot_dx ;
   *yy = -rot_sph * xpr + rot_cph * ypr + rot_dy ;
   return ;
}

/**--------------------------------------------------------------------------------
    Rotate and shift an image, using bicubic interpolation:
       aa  = shift in x
       bb  = shift in y
       phi = angle in radians
       scl = size scale factor (0.0 --> leave the same size)
-----------------------------------------------------------------------------------**/

MRI_IMAGE *mri_rotate( MRI_IMAGE *im, float aa, float bb, float phi, float scl )
{
   MRI_IMAGE  *imwarp ;
   int nxnew , nynew ;

   rot_cph = cos(phi) ;
   rot_sph = sin(phi) ;

   rot_dx = (0.5 * im->nx) * (1.0-rot_cph) - aa*rot_cph - bb*rot_sph
           -(0.5 * im->ny) * rot_sph ;

   rot_dy = (0.5 * im->nx) * rot_sph + aa*rot_sph - bb*rot_cph
           +(0.5 * im->ny) * (1.0-rot_cph) ;

   if( scl <= 0.0 ){
      nxnew = nynew = 0 ;
   } else {
      nxnew = scl * im->nx + 0.49 ;
      nynew = scl * im->ny + 0.49 ;
      rot_cph /= scl ;
      rot_sph /= scl ;
   }

   return mri_warp_bicubic( im , nxnew,nynew , __MRI_rotfunc ) ;
}

MRI_IMAGE *mri_rotate_bilinear( MRI_IMAGE *im, float aa, float bb, float phi, float scl )
{
   MRI_IMAGE  *imwarp ;
   int nxnew , nynew ;

   rot_cph = cos(phi) ;
   rot_sph = sin(phi) ;

   rot_dx = (0.5 * im->nx) * (1.0-rot_cph) - aa*rot_cph - bb*rot_sph
           -(0.5 * im->ny) * rot_sph ;

   rot_dy = (0.5 * im->nx) * rot_sph + aa*rot_sph - bb*rot_cph
           +(0.5 * im->ny) * (1.0-rot_cph) ;

   if( scl <= 0.0 ){
      nxnew = nynew = 0 ;
   } else {
      nxnew = scl * im->nx + 0.49 ;
      nynew = scl * im->ny + 0.49 ;
      rot_cph /= scl ;
      rot_sph /= scl ;
   }

   return mri_warp_bilinear( im , nxnew,nynew , __MRI_rotfunc ) ;
}

#undef WARP_POINT_ROUTINES
#ifdef WARP_POINT_ROUTINES

/*--------------------------------------------------------------------
  Return one point from a warped image
----------------------------------------------------------------------*/

#define INSIDE(i,j) ( ( (i)<0 || (j)<0 || (i)>=nx || (j)>=ny ) ? 0.0            : \
                    (kk==MRI_byte)    ? (float) MRI_BYTE_PIXEL(im,i,j)          : \
                    (kk==MRI_short)   ? (float) MRI_SHORT_PIXEL(im,i,j)         : \
                    (kk==MRI_int)     ? (float) MRI_INT_PIXEL(im,i,j)           : \
                    (kk==MRI_float)   ? (float) MRI_FLOAT_PIXEL(im,i,j)         : \
                    (kk==MRI_double)  ? (float) MRI_DOUBLE_PIXEL(im,i,j)        : \
                    (kk==MRI_complex) ? (float) CABS(MRI_COMPLEX_PIXEL(im,i,j)) : 0.0 )

float mri_warp_bicubic_point( MRI_IMAGE *im , int ii , int jj ,
                                void wf( float,float,float *,float *) )
{
   float xx,yy , fx,fy , new ;
   int nx,ny , ix,jy , kk ;
   float f_jm1,f_j00,f_jp1,f_jp2 , wt_m1,wt_00,wt_p1,wt_p2 ;

   nx = im->nx ;  /* input image dimensions, for convenience */
   ny = im->ny ;
   kk = im->kind ;

   /*** warp to output point ***/

   wf( (float) ii , (float) jj , &xx,&yy ) ;  /* get xx,yy in original image */

   ix = floor( xx ) ;  fx = xx - ix ;   /* integer and       */
   jy = floor( yy ) ;  fy = yy - jy ;   /* fractional coords */

   wt_m1 = P_M1(fx) ;  wt_00 = P_00(fx) ;  /* weights for interpolating */
   wt_p1 = P_P1(fx) ;  wt_p2 = P_P2(fx) ;  /* in the x-direction        */

   f_jm1 =  wt_m1 * INSIDE(ix-1,jy-1)        /* interpolate to ix + fx */
          + wt_00 * INSIDE(ix  ,jy-1)        /* at levels jy-1 .. jy+2 */
          + wt_p1 * INSIDE(ix+1,jy-1)
          + wt_p2 * INSIDE(ix+2,jy-1) ;

   f_j00 =   wt_m1 * INSIDE(ix-1,jy)
           + wt_00 * INSIDE(ix  ,jy)
           + wt_p1 * INSIDE(ix+1,jy)
           + wt_p2 * INSIDE(ix+2,jy) ;

   f_jp1 =   wt_m1 * INSIDE(ix-1,jy+1)
           + wt_00 * INSIDE(ix  ,jy+1)
           + wt_p1 * INSIDE(ix+1,jy+1)
           + wt_p2 * INSIDE(ix+2,jy+1) ;

   f_jp2 =   wt_m1 * INSIDE(ix-1,jy+2)
           + wt_00 * INSIDE(ix  ,jy+2)
           + wt_p1 * INSIDE(ix+1,jy+2)
           + wt_p2 * INSIDE(ix+2,jy+2) ;

   /* interpolate between y-levels to jy+fy */

   new = (  P_M1(fy) * f_jm1 + P_00(fy) * f_j00
          + P_P1(fy) * f_jp1 + P_P2(fy) * f_jp2 ) / 36.0 ;

   return new ;
}

/*--------------------------------------------------------------------
  Return one point from a rotated image
----------------------------------------------------------------------*/

float mri_rotate_point( MRI_IMAGE *im, float aa, float bb, float phi, float scl ,
                        int ix , int jy )
{
   MRI_IMAGE  *imwarp ;
   int nxnew , nynew ;

   rot_cph = cos(phi) ;
   rot_sph = sin(phi) ;

   rot_dx = (0.5 * im->nx) * (1.0-rot_cph) - aa*rot_cph - bb*rot_sph
           -(0.5 * im->ny) * rot_sph ;

   rot_dy = (0.5 * im->nx) * rot_sph + aa*rot_sph - bb*rot_cph
           +(0.5 * im->ny) * (1.0-rot_cph) ;

   if( scl > 0.0 ){
      rot_cph /= scl ;
      rot_sph /= scl ;
   }

   return mri_warp_bicubic_point( im , ix,jy , __MRI_rotfunc ) ;
}
#endif /* WARP_POINT_ROUTINES */
