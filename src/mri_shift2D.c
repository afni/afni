#include "mrilib.h"

/*** NOT 7D SAFE ***/

#define FINS(i,j) (  ( (i)<0 || (j)<0 || (i)>=nx || (j)>=ny ) \
                     ? 0.0 : far[(i)+(j)*nx] )

/**-------------------------------------------------------------------
    Shift an image, using bilinear interpolation:
       aa  = shift in x
       bb  = shift in y
    Like mri_rota_bilinear, with phi=0 -- RWCox - 11 Sep 2001
----------------------------------------------------------------------**/

MRI_IMAGE *mri_shift2D_bilinear( MRI_IMAGE *im, float aa, float bb )
{
   float dx , dy ;
   MRI_IMAGE *imfl , *new ;
   MRI_IMARR *impair ;
   float *far , *nar ;
   float xx,yy , fx,fy ;
   int ii,jj, nx,ny , ix,jy ;
   float f_j00,f_jp1 , wt_00,wt_p1 ;

ENTRY("mri_shift2D_bilinear") ;

   if( im == NULL || !MRI_IS_2D(im) ){
      fprintf(stderr,"*** mri_shift2D_bilinear only works on 2D images!\n") ;
      EXIT(1) ;
   }

   /** if complex image, break into pairs, do each separately, put back together **/

   if( im->kind == MRI_complex ){
      MRI_IMARR *impair ;
      MRI_IMAGE *rim , *iim , *tim ;
      impair = mri_complex_to_pair( im ) ;
      if( impair == NULL ){
         fprintf(stderr,"*** mri_complex_to_pair fails in mri_shift2D_bilinear!\n");
         EXIT(1) ;
      }
      rim = IMAGE_IN_IMARR(impair,0) ;
      iim = IMAGE_IN_IMARR(impair,1) ;  FREE_IMARR(impair) ;
      tim = mri_shift2D_bilinear( rim , aa,bb ); mri_free(rim); rim = tim;
      tim = mri_shift2D_bilinear( iim , aa,bb ); mri_free(iim); iim = tim;
      new = mri_pair_to_complex( rim , iim ) ;
      mri_free(rim) ; mri_free(iim) ;
      MRI_COPY_AUX(new,im) ;
      RETURN(new) ;
   }

   /** shift params **/

   dx = - aa ;
   dy = - bb ;

   /** other initialization **/

   nx = im->nx ;  /* image dimensions */
   ny = im->ny ;

   if( im->kind == MRI_float ) imfl = im ;
   else                        imfl = mri_to_float( im ) ;

   far = MRI_FLOAT_PTR(imfl) ;              /* access to float data */
   new = mri_new( nx , nx , MRI_float ) ;   /* output image */
   nar = MRI_FLOAT_PTR(new) ;               /* output image data */

   /*** loop over output points and warp to them ***/

   for( jj=0 ; jj < nx ; jj++ ){
      xx = dx - 1.0 ;
      yy = jj + dy ;

      jy = (yy >= 0.0) ? ((int) yy) : ((int) yy)-1 ;

      for( ii=0 ; ii < nx ; ii++ ){

         xx += 1.0 ;  /* get x,y in original image */

         ix = (xx >= 0.0) ? ((int) xx) : ((int) xx)-1 ;  /* floor */

         fx = xx-ix ; wt_00 = 1.0 - fx ; wt_p1 = fx ;

         if( ix >= 0 && ix < nx-1 && jy >= 0 && jy < ny-1 ){
            float *fy00 , *fyp1 ;

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

   /*** cleanup and return ***/

   if( im != imfl ) mri_free(imfl) ;  /* throw away unneeded workspace */
   MRI_COPY_AUX(new,im) ;
   RETURN(new) ;
}
