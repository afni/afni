/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** NOT 7D SAFE ***/

/*** functions to "warp" images -- not very efficient, but quite general ***/

#ifdef HP
# undef INLINE
# pragma INLINE  xxMRI_scaler
# pragma INLINE  xxMRI_rotfunc
#endif

#define FINS(i,j) (  ( (i)<0 || (j)<0 || (i)>=nx || (j)>=ny ) \
                     ? 0.0 : far[(i)+(j)*nx] )

/**************************************************************************/

static float sx_scale , sy_scale ;  /* global scaler data */

INLINE void xxMRI_scaler( float xpr, float ypr, float *xx , float *yy )
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

MRI_IMAGE *mri_resize_NN( MRI_IMAGE *im , int nxnew , int nynew )  /* 08 Jun 2004 */
{
   int nx,ny , nnx,nny , ii,jj , pp,qq , bb ;
   float fx,fy ;
   MRI_IMAGE *nim ;
   char *nar , *ar ;

   if( im == NULL ) return NULL ;

   nx  = im->nx ;  ny  = im->ny ;
   nnx = nxnew  ;  nny = nynew  ;
   fx  = ((float)nx) / (float)nnx ;
   fy  = ((float)ny) / (float)nny ;

   nim = mri_new( nnx , nny , im->kind ) ;
   nar = mri_data_pointer( nim ) ;
   ar  = mri_data_pointer( im ) ;
   bb  = im->pixel_size ;

   for( jj=0 ; jj < nny ; jj++ ){
     qq = (int)( fy*jj ) ;
     for( ii=0 ; ii < nnx ; ii++ ){
       pp = (int)( fx*ii ) ;
       memcpy( nar + (ii+jj*nnx)*bb , ar + (pp+qq*nx)*bb , bb ) ;
     }
   }

   MRI_COPY_AUX(nim,im) ;
   nim->dx *= fx ;
   nim->dy *= fy ;
   return nim ;
}

/****************************************************************************/

MRI_IMAGE *mri_squareaspect( MRI_IMAGE *im )  /* 08 Jun 2004 */
{
   int nx,ny , nxnew,nynew ;
   float dx,dy , rr ;

   if( im == NULL ) return NULL ;

   dx = fabs(im->dx) ; dy = fabs(im->dy) ;
   if( dx == 0.0 || dy == 0.0 ) return NULL ;
   rr = dy / dx ; if( rr == 1.0 ) return NULL ;

   nx = im->nx ; ny = im->ny ;

   if( rr < 1.0 ){
     nxnew = rint( nx/rr ) ; if( nxnew <= nx ) return NULL ;
     nynew = ny ;
   } else {
     nynew = rint( ny*rr ) ; if( nynew <= ny ) return NULL ;
     nxnew = nx ;
   }

   return mri_resize_NN( im , nxnew , nynew ) ;
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

   return mri_warp( im , nnx,nny , wtype , xxMRI_scaler ) ;
#else
   return mri_warp_bicubic( im , nnx,nny , xxMRI_scaler ) ;
#endif
}

/**************************************************************************/

MRI_IMAGE *mri_warp_bicubic( MRI_IMAGE *im , int nxnew , int nynew ,
                                     void wf( float,float,float *,float *) )
{
   MRI_IMAGE *imfl , *newImg ;
   float *far , *nar ;
   float xpr,ypr , xx,yy , fx,fy ;
   int ii,jj, nx,ny , ix,jy ;
   float f_jm1,f_j00,f_jp1,f_jp2 , wt_m1,wt_00,wt_p1,wt_p2 ;
   float bot,top,val ;  /* 29 Mar 2003 */

   nx = im->nx ;  /* input image dimensions, for convenience */
   ny = im->ny ;

   nxnew = (nxnew > 0) ? nxnew : nx ;  /* default output image sizes */
   nynew = (nynew > 0) ? nynew : ny ;

   switch( im->kind ){   /* 29 Mar 2003: allow for different input types */
                         /*              by doing components 1 at a time */
     case MRI_float:
       imfl = im ; break ;

     default:
       imfl = mri_to_float(im) ; break ;

     case MRI_short:{
       imfl = mri_to_float(im) ;
       newImg  = mri_warp_bicubic( imfl , nxnew,nynew , wf ) ;
       mri_free(imfl) ;
       imfl = mri_to_mri(MRI_short,newImg) ;
       mri_free(newImg) ; return imfl ;
     }

     case MRI_byte:{
       imfl = mri_to_float(im) ;
       newImg  = mri_warp_bicubic( imfl , nxnew,nynew , wf ) ;
       mri_free(imfl) ;
       imfl = mri_to_mri(MRI_byte,newImg) ;
       mri_free(newImg) ; return imfl ;
     }

     case MRI_rgb:{
       MRI_IMARR *imar = mri_rgb_to_3float(im) ;
       MRI_IMAGE *rim,*gim,*bim ;
       rim = mri_warp_bicubic( IMARR_SUBIM(imar,0), nxnew,nynew, wf ) ;
       gim = mri_warp_bicubic( IMARR_SUBIM(imar,1), nxnew,nynew, wf ) ;
       bim = mri_warp_bicubic( IMARR_SUBIM(imar,2), nxnew,nynew, wf ) ;
       DESTROY_IMARR(imar) ;
       newImg = mri_3to_rgb( rim,gim,bim ) ;
       mri_free(rim); mri_free(gim); mri_free(bim); return newImg;
     }

   }

   /* at this point, imfl is in MRI_float format */

   far = mri_data_pointer( imfl ) ;  /* easy access to float data */

   newImg = mri_new( nxnew , nynew , MRI_float ) ;   /* output image */
   nar = mri_data_pointer( newImg ) ;                /* output image data */

   bot = top = far[0] ;                        /* 29 Mar 2003: */
   for( ii=1 ; ii < imfl->nvox ; ii++ ){       /* clip output data range */
          if( far[ii] > top ) top = far[ii] ;
     else if( far[ii] < bot ) bot = far[ii] ;
   }

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

         val = (  P_M1(fy) * f_jm1 + P_00(fy) * f_j00
                + P_P1(fy) * f_jp1 + P_P2(fy) * f_jp2 ) / 36.0 ;

              if( val > top ) val = top ;  /* 29 Mar 2003 */
         else if( val < bot ) val = bot ;

         nar[ii+jj*nxnew] = val ;
      }
   }

   /*** cleanup and return ***/

   if( im != imfl ) mri_free(imfl) ;  /* throw away unneeded workspace */
   return newImg ;
}

/*************************************************************************/

MRI_IMAGE *mri_warp_bilinear( MRI_IMAGE *im , int nxnew , int nynew ,
                                  void wf( float,float,float *,float *) )
{
   MRI_IMAGE *imfl , *newImg ;
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

   newImg = mri_new( nxnew , nynew , MRI_float ) ;   /* output image */
   nar = mri_data_pointer( newImg ) ;                /* output image data */

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
   return newImg ;
}

/**********************************************************************/

static float rot_dx , rot_dy , rot_cph , rot_sph ;    /* global rotfunc data */

INLINE void xxMRI_rotfunc( float xpr , float ypr , float *xx , float *yy )
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

   return mri_warp_bicubic( im , nxnew,nynew , xxMRI_rotfunc ) ;
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

   return mri_warp_bilinear( im , nxnew,nynew , xxMRI_rotfunc ) ;
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
   float xx,yy , fx,fy , newPt ;
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

   newPt = (  P_M1(fy) * f_jm1 + P_00(fy) * f_j00
          + P_P1(fy) * f_jp1 + P_P2(fy) * f_jp2 ) / 36.0 ;

   return newPt ;
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

   return mri_warp_bicubic_point( im , ix,jy , xxMRI_rotfunc ) ;
}
#endif /* WARP_POINT_ROUTINES */
