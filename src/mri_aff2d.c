/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*** NOT 7D SAFE ***/

#define FINS(i,j) (  ( (i)<0 || (j)<0 || (i)>=nx || (j)>=ny ) \
                     ? 0.0 : far[(i)+(j)*nx] )

/*-------------------------------------------------------------------*/

static void invert2d( float  axx, float  axy, float  ayx, float  ayy ,
                      float *bxx, float *bxy, float *byx, float *byy  )
{
   float det = axx*ayy - axy*ayx ;
   if( det == 0.0 ){ *bxx=*byy=*bxy=*byx = 0.0 ; return ; }
   *bxx =  ayy / det ;
   *byy =  axx / det ;
   *bxy = -axy / det ;
   *byx = -ayx / det ;
   return ;
}

/*-------------------------------------------------------------------
    Affine transform a 2D image, using bilinear interpolation:
    If flag == 0
       [ xout ] = [ axx axy ] [ xin ]
       [ yout ] = [ ayx ayy ] [ yin ]
    If flag == 1
       [ xin ] = [ axx axy ] [ xout ]
       [ yin ] = [ ayx ayy ] [ yout ]
----------------------------------------------------------------------*/

MRI_IMAGE *mri_aff2d_byte( MRI_IMAGE *im, int flag ,
                           float axx, float axy, float ayx, float ayy )
{
   float bxx,bxy,byx,byy , xbase,ybase , xx,yy , fx,fy ;
   float f_j00,f_jp1 , wt_00,wt_p1 ;
   int ii,jj , nx,ny , ix,jy ;
   MRI_IMAGE *new ;
   byte *far , *nar ;

   if( im == NULL || !MRI_IS_2D(im) || im->kind != MRI_byte ){
      fprintf(stderr,"*** mri_aff2d_byte only works on 2D byte images!\n");
      return NULL ;
   }

   if( flag == 0 ){
      invert2d( axx,axy,ayx,ayy , &bxx,&bxy,&byx,&byy ) ;
   } else {
      bxx = axx ; bxy = axy ; byx = ayx ; byy = ayy ;
   }
   if( (bxx == 0.0 && bxy == 0.0) || (byx == 0.0 && byy == 0.0) ){
      fprintf(stderr,"*** mri_aff2d_byte: input matrix is singular!\n") ;
      return NULL ;
   }

   nx = im->nx ; ny = im->ny ;
   xbase = 0.5*nx*(1.0-bxx) - 0.5*ny*bxy ;
   ybase = 0.5*ny*(1.0-byy) - 0.5*nx*byx ;

   far = MRI_BYTE_PTR(im) ;                /* input image data */
   new = mri_new( nx , nx , MRI_byte ) ;   /* output image */
   nar = MRI_BYTE_PTR(new) ;               /* output image data */

   /*** loop over output points and warp to them ***/

   for( jj=0 ; jj < nx ; jj++ ){
      xx = xbase-bxx + bxy * jj ;
      yy = ybase-byx + byy * jj ;
      for( ii=0 ; ii < nx ; ii++ ){

         xx += bxx ;  /* get x,y in original image */
         yy += byx ;

         ix = (xx >= 0.0) ? ((int) xx) : ((int) xx)-1 ;  /* floor */
         jy = (yy >= 0.0) ? ((int) yy) : ((int) yy)-1 ;

         fx = xx-ix ; wt_00 = 1.0 - fx ; wt_p1 = fx ;

         if( ix >= 0 && ix < nx-1 && jy >= 0 && jy < ny-1 ){
            byte *fy00 , *fyp1 ;

            fy00 = far + (ix + jy*nx) ; fyp1 = fy00 + nx ;

            f_j00 = wt_00 * fy00[0] + wt_p1 * fy00[1] ;
            f_jp1 = wt_00 * fyp1[0] + wt_p1 * fyp1[1] ;

         } else {
            f_j00 = wt_00 * FINS(ix,jy  ) + wt_p1 * FINS(ix+1,jy  ) ;
            f_jp1 = wt_00 * FINS(ix,jy+1) + wt_p1 * FINS(ix+1,jy+1) ;
         }

         fy  = yy-jy ; nar[ii+jj*nx] = (1.0-fy) * f_j00 + fy * f_jp1 ;

      }
   }

   MRI_COPY_AUX(new,im) ;
   return new ;
}
