#include "mrilib.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/


/*** NOT 7D SAFE ***/

/*************************************************************************
 ** Adapted from mri_align.c, for the purpose of real-time registration **
 *************************************************************************/

#define DFILT_SIGMA  (4.0*0.42466090)  /* 4.0 = FWHM */
#define MAX_ITER     5
#define DXY_THRESH   0.15         /* pixels */
#define PHI_THRESH   0.45         /* degrees */
#define DFAC         (PI/180.0)

#define FINE_FIT

#define FINE_SIGMA   (1.0*0.42466090)
#define FINE_DXY_THRESH  0.07
#define FINE_PHI_THRESH  0.21

/*********************************************************************
  05 Nov 1997: make the parameters that control the iterations
               be alterable.
**********************************************************************/

static float dfilt_sigma     = DFILT_SIGMA ,
             dxy_thresh      = DXY_THRESH ,
             phi_thresh      = PHI_THRESH ,
             fine_sigma      = FINE_SIGMA ,
             fine_dxy_thresh = FINE_DXY_THRESH ,
             fine_phi_thresh = FINE_PHI_THRESH  ;

static int max_iter = MAX_ITER ;

void mri_2dalign_params( int maxite,
                         float sig , float dxy , float dph ,
                         float fsig, float fdxy, float fdph )
{
   if( maxite > 0   ) max_iter    = maxite ; else max_iter    = MAX_ITER    ;
   if( sig    > 0.0 ) dfilt_sigma = sig    ; else dfilt_sigma = DFILT_SIGMA ;
   if( dxy    > 0.0 ) dxy_thresh  = dxy    ; else dxy_thresh  = DXY_THRESH  ;
   if( dph    > 0.0 ) phi_thresh  = dph    ; else phi_thresh  = PHI_THRESH  ;

   fine_sigma = fsig ;
   if( fdxy > 0.0 ) fine_dxy_thresh = fdxy ; else fine_dxy_thresh = FINE_DXY_THRESH ;
   if( fdph > 0.0 ) fine_phi_thresh = fdph ; else fine_phi_thresh = FINE_PHI_THRESH ;

   return ;
}
/*********************************************************************/

/*--------------------------------------------------------------------
   Inputs: imbase = base image for alignment
           imwt   = image of weight factors to align to
                      (if NULL, will generate one internally)

   Output: pointer to a MRI_2dalign_basis struct, for later use.
           The malloc-ed data in there can be freed using
           routine mri_2dalign_cleanup.
----------------------------------------------------------------------*/

MRI_2dalign_basis * mri_2dalign_setup( MRI_IMAGE * imbase , MRI_IMAGE * imwt )
{
   MRI_IMAGE * im1 , *bim,*xim,*yim,*tim , *bim2 , * im2 , *imww ;
   float *tar,*xar,*yar ;
   int nx,ny , ii,jj , joff ;
   int use_fine_fit = (fine_sigma > 0.0) ;
   float hnx,hny ;

   MRI_IMARR * fitim  =NULL;
   double * chol_fitim=NULL ;
   MRI_IMARR * fine_fitim  =NULL ;
   double * chol_fine_fitim=NULL ;
   MRI_2dalign_basis * bout = NULL ;

   if( ! MRI_IS_2D(imbase) ){
      fprintf(stderr,"\n*** mri_2dalign_setup: cannot use nD images!\a\n") ;
      return NULL ;
   }

   im1 = mri_to_float( imbase ) ;
   nx  = im1->nx ;  hnx = 0.5 * nx ;
   ny  = im1->ny ;  hny = 0.5 * ny ;

   bim = mri_filt_fft( im1, dfilt_sigma, 0, 0, FILT_FFT_WRAPAROUND ) ; /* smooth */
   xim = mri_filt_fft( im1, dfilt_sigma, 1, 0, FILT_FFT_WRAPAROUND ) ; /* d/dx */
   yim = mri_filt_fft( im1, dfilt_sigma, 0, 1, FILT_FFT_WRAPAROUND ) ; /* d/dy */

   tim = mri_new( nx , ny , MRI_float ) ;  /* x * d/dy - y * d/dx */
   tar = mri_data_pointer( tim ) ;         /* which is d/d(theta) */
   xar = mri_data_pointer( xim ) ;
   yar = mri_data_pointer( yim ) ;
   for( jj=0 ; jj < ny ; jj++ ){
      joff = jj * nx ;
      for( ii=0 ; ii < nx ; ii++ ){
         tar[ii+joff] = DFAC * (  (ii-hnx) * yar[ii+joff]
                                - (jj-hny) * xar[ii+joff] ) ;
      }
   }
   INIT_IMARR ( fitim ) ;
   ADDTO_IMARR( fitim , bim ) ;
   ADDTO_IMARR( fitim , xim ) ;
   ADDTO_IMARR( fitim , yim ) ;
   ADDTO_IMARR( fitim , tim ) ;

   if( imwt == NULL ) imww = mri_to_float( bim ) ;  /* 28 Oct 1996 */
   else               imww = mri_to_float( imwt ) ;

   chol_fitim = mri_startup_lsqfit( fitim , imww ) ;
   mri_free(imww) ;

   if( use_fine_fit ){
     bim = mri_filt_fft( im1, fine_sigma, 0, 0, FILT_FFT_WRAPAROUND ) ; /* smooth */
     xim = mri_filt_fft( im1, fine_sigma, 1, 0, FILT_FFT_WRAPAROUND ) ; /* d/dx */
     yim = mri_filt_fft( im1, fine_sigma, 0, 1, FILT_FFT_WRAPAROUND ) ; /* d/dy */

     tim = mri_new( nx , ny , MRI_float ) ;  /* x * d/dy - y * d/dx */
     tar = mri_data_pointer( tim ) ;         /* which is d/d(theta) */
     xar = mri_data_pointer( xim ) ;
     yar = mri_data_pointer( yim ) ;
     for( jj=0 ; jj < ny ; jj++ ){
        joff = jj * nx ;
        for( ii=0 ; ii < nx ; ii++ ){
           tar[ii+joff] = DFAC * (  (ii-hnx) * yar[ii+joff]
                                  - (jj-hny) * xar[ii+joff] ) ;
        }
     }
     INIT_IMARR ( fine_fitim ) ;
     ADDTO_IMARR( fine_fitim , bim ) ;
     ADDTO_IMARR( fine_fitim , xim ) ;
     ADDTO_IMARR( fine_fitim , yim ) ;
     ADDTO_IMARR( fine_fitim , tim ) ;

     if( imwt == NULL ) imww = mri_to_float( bim ) ;  /* 03 Oct 1997 */
     else               imww = mri_to_float( imwt ) ;

     chol_fine_fitim = mri_startup_lsqfit( fine_fitim , imww ) ;
     mri_free(imww) ;
   }

   mri_free(im1) ;

   bout = (MRI_2dalign_basis *) malloc( sizeof(MRI_2dalign_basis) ) ;
   bout->fitim           = fitim ;
   bout->chol_fitim      = chol_fitim ;
   bout->fine_fitim      = fine_fitim ;
   bout->chol_fine_fitim = chol_fine_fitim ;
   return bout ;
}

/*-----------------------------------------------------------------------
   Input:   basis  = MRI_2dalign_basis * return from setup routine above.
            im     = MRI_IMAGE * to align to base image

   Output:  return value is aligned image;
            *dx , *dy, and *phi are set to rotation parameters.
            Note that the returned image is floats.
-------------------------------------------------------------------------*/

MRI_IMAGE * mri_2dalign_one( MRI_2dalign_basis * basis , MRI_IMAGE * im ,
                             float * dx , float * dy , float * phi )
{
   MRI_IMARR * fitim ;
   double * chol_fitim=NULL ;
   MRI_IMARR * fine_fitim  =NULL ;
   double * chol_fine_fitim=NULL ;

   float * fit , *dfit ;
   int nx,ny , ii,jj , joff , iter , good ;
   int use_fine_fit = (fine_sigma > 0.0) ;
   MRI_IMAGE * im2 , * bim2 , * tim ;

   nx  = im->nx ; ny  = im->ny ;

   fitim           = basis->fitim ;
   chol_fitim      = basis->chol_fitim ;
   fine_fitim      = basis->fine_fitim ;
   chol_fine_fitim = basis->chol_fine_fitim ;

   /*-- register the image: coarsely --*/

      im2  = mri_to_float( im ) ;
      bim2 = mri_filt_fft( im2, dfilt_sigma, 0,0, FILT_FFT_WRAPAROUND ) ;
      fit  = mri_delayed_lsqfit( bim2 , fitim , chol_fitim ) ;
      mri_free( bim2 ) ;

      iter = 0 ;
      good = ( fabs(fit[1]) > dxy_thresh ||
               fabs(fit[2]) > dxy_thresh || fabs(fit[3]) > phi_thresh ) ;

      /*-- iterate coarse fit --*/

      while( good ){
         tim  = mri_rota( im2 , fit[1] , fit[2] , fit[3]*DFAC ) ;
         bim2 = mri_filt_fft( tim, dfilt_sigma, 0,0, FILT_FFT_WRAPAROUND ) ;
         dfit = mri_delayed_lsqfit( bim2 , fitim , chol_fitim ) ;
         mri_free( bim2 ) ; mri_free( tim ) ;

         fit[1] += dfit[1] ;
         fit[2] += dfit[2] ;
         fit[3] += dfit[3] ;

         good = (++iter < max_iter) &&
                  ( fabs(dfit[1]) > dxy_thresh ||
                    fabs(dfit[2]) > dxy_thresh || fabs(dfit[3]) > phi_thresh ) ;

         free(dfit) ; dfit = NULL ;
      } /* end while */

      /*-- perform fine adjustments  --*/

      if( use_fine_fit ){
         good = 1 ;
         while( good ){
            tim  = mri_rota( im2 , fit[1] , fit[2] , fit[3]*DFAC ) ;
            bim2 = mri_filt_fft( tim, fine_sigma, 0,0, FILT_FFT_WRAPAROUND ) ;
            dfit = mri_delayed_lsqfit( bim2 , fine_fitim , chol_fine_fitim ) ;
            mri_free( bim2 ) ; mri_free( tim ) ;

            fit[1] += dfit[1] ;
            fit[2] += dfit[2] ;
            fit[3] += dfit[3] ;

            good = (++iter < max_iter) &&
                     ( fabs(dfit[1]) > fine_dxy_thresh ||
                       fabs(dfit[2]) > fine_dxy_thresh || fabs(dfit[3]) > fine_phi_thresh ) ;

            free(dfit) ; dfit = NULL ;
         } /* end while */
      }

      /*-- save final alignment parameters --*/

      if( dx != NULL ) *dx = fit[1] ;
      if( dy != NULL ) *dy = fit[2] ;
      if( phi!= NULL ) *phi= fit[3]*DFAC ;

   /*-- do the actual realignment --*/

   tim = mri_rota( im2 , fit[1],fit[2],fit[3]*DFAC ) ;
   mri_free( im2 ) ;
   return tim ;
}

MRI_IMARR * mri_2dalign_many( MRI_IMAGE * im , MRI_IMAGE * imwt , MRI_IMARR * ims ,
                              float * dx , float * dy , float * phi )
{
   int kim ;
   MRI_IMAGE * tim ;
   MRI_IMARR * alim ;
   MRI_2dalign_basis * basis ;

   basis = mri_2dalign_setup( im , imwt ) ;
   if( basis == NULL ) return NULL ;

   INIT_IMARR( alim ) ;

   for( kim=0 ; kim < ims->num ; kim++ ){
      tim = mri_2dalign_one( basis , ims->imarr[kim] , dx+kim , dy+kim , phi+kim ) ;
      ADDTO_IMARR(alim,tim) ;
   }

   mri_2dalign_cleanup( basis ) ;
   return alim ;
}

void mri_2dalign_cleanup( MRI_2dalign_basis * basis )
{
   if( basis == NULL ) return ;

   if( basis->fitim           != NULL ){ DESTROY_IMARR( basis->fitim ) ; }
   if( basis->chol_fitim      != NULL ){ free(basis->chol_fitim) ; }

   if( basis->fine_fitim      != NULL ){ DESTROY_IMARR( basis->fine_fitim ) ; }
   if( basis->chol_fine_fitim != NULL ){ free(basis->chol_fine_fitim) ; }

   return ;
}
