#include "mrilib.h"

/*** NOT 7D SAFE ***/

#define FINS(i,j) (  ( (i)<0 || (j)<0 || (i)>=nx || (j)>=ny ) \
                     ? 0.0 : far[(i)+(j)*nx] )

/** option to precompute cubic interpolation weights **/

#undef USE_CGRID

#ifdef USE_CGRID
#  define CGRID 8192
   static int p_first = 1 ;
   static float p_m1[CGRID+1], p_00[CGRID+1],
                p_p1[CGRID+1], p_p2[CGRID+1] ;
#endif

   /* cubic interpolation polynomials */

#define P_M1(x)  ((x)*(1.0-(x))*((x)-2.0))
#define P_00(x)  (3.0*((x)+1.0)*((x)-1.0)*((x)-2.0))
#define P_P1(x)  (3.0*(x)*((x)+1.0)*(2.0-(x)))
#define P_P2(x)  ((x)*((x)+1.0)*((x)-1.0))

/**-------------------------------------------------------------------
    Rotate and shift an image, using bicubic interpolation:
       aa  = shift in x
       bb  = shift in y
       phi = angle in radians
    Sort of a replacement for mri_rotate in mri_warp.c; supposed
    to be faster.  If the input image is MRI_complex, the output
    will be also; otherwise, the output will be MRI_float.
----------------------------------------------------------------------**/

MRI_IMAGE *mri_rota( MRI_IMAGE *im, float aa, float bb, float phi )
{
   float rot_dx , rot_dy , rot_cph , rot_sph , top,bot,val ;
   MRI_IMAGE *imfl , *new ;
   MRI_IMARR *impair ;
   float *far , *nar ;
   float xx,yy , fx,fy ;
   int ii,jj, nx,ny , ix,jy , ifx,jfy ;
   float f_jm1,f_j00,f_jp1,f_jp2 , wt_m1,wt_00,wt_p1,wt_p2 ;

#ifdef USE_CGRID
   if( p_first ){
      p_first = 0 ;
      xx      = 1.0 / CGRID ;
      for( ii=0 ; ii <= CGRID ; ii++ ){
         yy       = ii * xx ;
         p_m1[ii] = P_M1(yy) ;
         p_00[ii] = P_00(yy) ;
         p_p1[ii] = P_P1(yy) ;
         p_p2[ii] = P_P2(yy) ;
      }
   }
#endif

   if( im == NULL || ! MRI_IS_2D(im) ){
      fprintf(stderr,"*** mri_rota only works on 2D images!\n") ; exit(1) ;
   }

   /** if complex image, break into pairs, do each separately, put back together **/

   if( im->kind == MRI_complex ){
      MRI_IMARR *impair ;
      MRI_IMAGE * rim , * iim , * tim ;
      impair = mri_complex_to_pair( im ) ;
      if( impair == NULL ){
         fprintf(stderr,"*** mri_complex_to_pair fails in mri_rota!\n") ; exit(1) ;
      }
      rim = IMAGE_IN_IMARR(impair,0) ;
      iim = IMAGE_IN_IMARR(impair,1) ;  FREE_IMARR(impair) ;
      tim = mri_rota( rim , aa,bb,phi ) ; mri_free( rim ) ; rim = tim ;
      tim = mri_rota( iim , aa,bb,phi ) ; mri_free( iim ) ; iim = tim ;
      new = mri_pair_to_complex( rim , iim ) ;
      mri_free( rim ) ; mri_free( iim ) ;
      MRI_COPY_AUX(new,im) ;
      return new ;
   }

   /** rotation params **/

   rot_cph = cos(phi) ; rot_sph = sin(phi) ;

   rot_dx  = (0.5 * im->nx) * (1.0-rot_cph) - aa*rot_cph - bb*rot_sph
            -(0.5 * im->ny) * rot_sph ;

   rot_dy  = (0.5 * im->nx) * rot_sph + aa*rot_sph - bb*rot_cph
            +(0.5 * im->ny) * (1.0-rot_cph) ;

   /** other initialization **/

   nx = im->nx ;  /* image dimensions */
   ny = im->ny ;

   if( im->kind == MRI_float ) imfl = im ;
   else                        imfl = mri_to_float( im ) ;

   far = MRI_FLOAT_PTR(imfl) ;              /* access to float data */
   new = mri_new( nx , nx , MRI_float ) ;   /* output image */
   nar = MRI_FLOAT_PTR(new) ;               /* output image data */

   bot = top = far[0] ;
   for( ii=0 ; ii < nx*ny ; ii++ )
           if( far[ii] < bot ) bot = far[ii] ;
      else if( far[ii] > top ) top = far[ii] ;

   /*** loop over output points and warp to them ***/

   for( jj=0 ; jj < nx ; jj++ ){
      xx = rot_sph * jj + rot_dx - rot_cph ;
      yy = rot_cph * jj + rot_dy + rot_sph ;
      for( ii=0 ; ii < nx ; ii++ ){

         xx += rot_cph ;  /* get x,y in original image */
         yy -= rot_sph ;

         ix = (xx >= 0.0) ? ((int) xx) : ((int) xx)-1 ;  /* floor */
         jy = (yy >= 0.0) ? ((int) yy) : ((int) yy)-1 ;

#ifdef USE_CGRID
         ifx   = (xx-ix)*CGRID + 0.499 ;
         wt_m1 = p_m1[ifx] ; wt_00 = p_00[ifx] ;
         wt_p1 = p_p1[ifx] ; wt_p2 = p_p2[ifx] ;
#else
         fx    = xx-ix ;
         wt_m1 = P_M1(fx) ; wt_00 = P_00(fx) ;
         wt_p1 = P_P1(fx) ; wt_p2 = P_P2(fx) ;
#endif

         if( ix > 0 && ix < nx-2 && jy > 0 && jy < ny-2 ){
            float * fym1, *fy00 , *fyp1 , *fyp2 ;

            fym1 = far + (ix-1 + (jy-1)*nx) ;
            fy00 = fym1 + nx ;
            fyp1 = fy00 + nx ;
            fyp2 = fyp1 + nx ;

            f_jm1 =  wt_m1 * fym1[0] + wt_00 * fym1[1]
                   + wt_p1 * fym1[2] + wt_p2 * fym1[3] ;

            f_j00 =  wt_m1 * fy00[0] + wt_00 * fy00[1]
                   + wt_p1 * fy00[2] + wt_p2 * fy00[3] ;

            f_jp1 =  wt_m1 * fyp1[0] + wt_00 * fyp1[1]
                   + wt_p1 * fyp1[2] + wt_p2 * fyp1[3] ;

            f_jp2 =  wt_m1 * fyp2[0] + wt_00 * fyp2[1]
                   + wt_p1 * fyp2[2] + wt_p2 * fyp2[3] ;

         } else {

            f_jm1 =  wt_m1 * FINS(ix-1,jy-1)
                   + wt_00 * FINS(ix  ,jy-1)
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
         }

#define THIRTYSIX 2.7777778e-2  /* 1./36.0, actually */

#ifdef USE_CGRID
         jfy = (yy-jy)*CGRID + 0.499 ;
         val = (  p_m1[jfy] * f_jm1 + p_00[jfy] * f_j00
                + p_p1[jfy] * f_jp1 + p_p2[jfy] * f_jp2 ) * THIRTYSIX ;
#else
         fy  = yy-jy ;
         val = (  P_M1(fy) * f_jm1 + P_00(fy) * f_j00
                + P_P1(fy) * f_jp1 + P_P2(fy) * f_jp2 ) * THIRTYSIX ;
#endif

              if( val < bot ) nar[ii+jj*nx] = bot ;  /* too small! */
         else if( val > top ) nar[ii+jj*nx] = top ;  /* too big!   */
         else                 nar[ii+jj*nx] = val ;  /* just right */

      }
   }

   /*** cleanup and return ***/

   if( im != imfl ) mri_free(imfl) ;  /* throw away unneeded workspace */
   MRI_COPY_AUX(new,im) ;
   return new ;
}

/**-------------------------------------------------------------------
    Rotate and shift an image, using bilinear interpolation:
       aa  = shift in x
       bb  = shift in y
       phi = angle in radians
    Sort of a replacement for mri_rotate in mri_warp.c; supposed
    to be faster.  If the input image is MRI_complex, the output
    will be also; otherwise, the output will be MRI_float.
----------------------------------------------------------------------**/

MRI_IMAGE *mri_rota_bilinear( MRI_IMAGE *im, float aa, float bb, float phi )
{
   float rot_dx , rot_dy , rot_cph , rot_sph ;
   MRI_IMAGE *imfl , *new ;
   MRI_IMARR *impair ;
   float *far , *nar ;
   float xx,yy , fx,fy ;
   int ii,jj, nx,ny , ix,jy ;
   float f_j00,f_jp1 , wt_00,wt_p1 ;

   if( im == NULL || ! MRI_IS_2D(im) ){
      fprintf(stderr,"*** mri_rota_bilinear only works on 2D images!\n") ; exit(1) ;
   }

   /** if complex image, break into pairs, do each separately, put back together **/

   if( im->kind == MRI_complex ){
      MRI_IMARR *impair ;
      MRI_IMAGE * rim , * iim , * tim ;
      impair = mri_complex_to_pair( im ) ;
      if( impair == NULL ){
         fprintf(stderr,"*** mri_complex_to_pair fails in mri_rota!\n") ; exit(1) ;
      }
      rim = IMAGE_IN_IMARR(impair,0) ;
      iim = IMAGE_IN_IMARR(impair,1) ;  FREE_IMARR(impair) ;
      tim = mri_rota_bilinear( rim , aa,bb,phi ) ; mri_free( rim ) ; rim = tim ;
      tim = mri_rota_bilinear( iim , aa,bb,phi ) ; mri_free( iim ) ; iim = tim ;
      new = mri_pair_to_complex( rim , iim ) ;
      mri_free( rim ) ; mri_free( iim ) ;
      MRI_COPY_AUX(new,im) ;
      return new ;
   }

   /** rotation params **/

   rot_cph = cos(phi) ; rot_sph = sin(phi) ;

   rot_dx  = (0.5 * im->nx) * (1.0-rot_cph) - aa*rot_cph - bb*rot_sph
            -(0.5 * im->ny) * rot_sph ;

   rot_dy  = (0.5 * im->nx) * rot_sph + aa*rot_sph - bb*rot_cph
            +(0.5 * im->ny) * (1.0-rot_cph) ;

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
      xx = rot_sph * jj + rot_dx - rot_cph ;
      yy = rot_cph * jj + rot_dy + rot_sph ;
      for( ii=0 ; ii < nx ; ii++ ){

         xx += rot_cph ;  /* get x,y in original image */
         yy -= rot_sph ;

         ix = (xx >= 0.0) ? ((int) xx) : ((int) xx)-1 ;  /* floor */
         jy = (yy >= 0.0) ? ((int) yy) : ((int) yy)-1 ;

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
   return new ;
}
