#include "mrilib.h"
#include <string.h>

#include "vecmat.h"

MRI_IMAGE * mri_align_crao( float filt_fwhm , MRI_IMARR * ims ) ;

int main( int argc , char * argv[] )
{
   MRI_IMAGE *imin ;
   MRI_IMARR *ims ;
   float fwhm ;
   int ii , iarg ;

   if( argc < 4 || strncmp(argv[1],"-help",4) == 0 ){
      printf("Usage: crao fwhm image_files ...\n") ;
      exit(0) ;
   }

   fwhm = strtod( argv[1] , NULL ) ;
   if( fwhm <= 0.0 ){fprintf(stderr,"Illegal fwhm!\a\n");exit(1);}

   INIT_IMARR(ims) ;
   for( iarg=2 ; iarg < argc ; iarg++ ){
      imin  = mri_read( argv[iarg] ) ;
      ADDTO_IMARR(ims,imin) ;
   }

   imin = mri_align_crao( fwhm , ims ) ;
   mri_write( "crao.ent" , imin ) ;

   exit(0) ;
}

#define DFAC         (PI/180.0)

/***---------------------------------------------------------------------
  Compute the Cramer-Rao bounds on registration accuracy from
  a sequence of images.  Returns an array of length 3: dx,dy,phi (degrees).
-------------------------------------------------------------------------***/

MRI_IMAGE * mri_align_crao( float filt_fwhm , MRI_IMARR * ims )
{
   MRI_IMAGE **imstat , *imbar , *imsig , *imdx,*imdy,*imphi , *iment ;
   float sthr,fac , hnx,hny , filt_rms = filt_fwhm*0.42466090 ;
   float *xar , *yar , *par , *sar , *bar , *entar ;
   int ii , npix , nx,ny , jj , joff , nzero=0 ;
   THD_dmat33 cov_all , cov_one ;
   double vdxdx,vdydy,vphiphi , vdxdy,vdxphi,vdyphi ;
   double det_all , det_one ;

   for( ii=1 ; ii < ims->num ; ii++ )
      (void) mri_stat_seq( ims->imarr[ii] ) ;

   imstat = mri_stat_seq( NULL ) ;
   imbar  = imstat[0] ;
   imsig  = imstat[1] ;

   nx = imbar->nx ; ny = imbar->ny ; npix = nx * ny ;
   hnx = 0.5*nx ; hny = 0.5*ny ;

   imdx   = mri_filt_fft( imbar , filt_rms , 1 , 0 , FILT_FFT_WRAPAROUND ) ;  /* d/dx */
   imdy   = mri_filt_fft( imbar , filt_rms , 0 , 1 , FILT_FFT_WRAPAROUND ) ;  /* d/dy */
   imphi  = mri_new( nx , ny , MRI_float ) ;

   xar = MRI_FLOAT_PTR(imdx)  ; yar = MRI_FLOAT_PTR(imdy) ;
   par = MRI_FLOAT_PTR(imphi) ; sar = MRI_FLOAT_PTR(imsig) ;

   for( jj=0 ; jj < ny ; jj++ ){
      joff = jj * nx ;
      for( ii=0 ; ii < nx ; ii++ ){
         par[ii+joff] = DFAC * (  (ii-hnx) * yar[ii+joff]
                                - (jj-hny) * xar[ii+joff] ) ;
      }
   }

   sthr = 0.01 * mri_max( imsig ) ;
   for( ii=0 ; ii < npix ; ii++ )
      if( sar[ii] < sthr ){
         sar[ii] = 0.0 ;
         nzero++ ;
      }
   if( nzero > 0 ) printf("set %d sigmas to zero\n",nzero) ;

   for( ii=0 ; ii < npix ; ii++ ){
      fac      = (sar[ii] > 0.0) ? (1.0/SQR(sar[ii])) : 0.0 ;
      xar[ii] *= fac ;
      yar[ii] *= fac ;
      par[ii] *= fac ;
   }

   mri_free(imbar) ;

   imbar = mri_filt_fft( imdx , filt_rms , 0 , 0 , FILT_FFT_WRAPAROUND ) ;
   mri_free(imdx) ; imdx = imbar ; xar = MRI_FLOAT_PTR(imdx) ;

   imbar = mri_filt_fft( imdy , filt_rms , 0 , 0 , FILT_FFT_WRAPAROUND ) ;
   mri_free(imdy) ; imdy = imbar ; yar = MRI_FLOAT_PTR(imdy) ;

   imbar = mri_filt_fft( imphi , filt_rms , 0 , 0 , FILT_FFT_WRAPAROUND ) ;
   mri_free(imphi) ; imphi = imbar ; par = MRI_FLOAT_PTR(imphi) ;

   for( ii=0 ; ii < npix ; ii++ ){
      xar[ii] *= sar[ii] ; yar[ii] *= sar[ii] ; par[ii] *= sar[ii] ;
   }
   mri_free(imsig) ;

   vdxdx = vdydy = vphiphi = vdxdy = vdxphi = vdyphi = 0.0 ;

   for( ii=0 ; ii < npix ; ii++ ){
      vdxdx   += SQR( xar[ii] ) ;
      vdydy   += SQR( yar[ii] ) ;
      vphiphi += SQR( par[ii] ) ;
      vdxdy   += xar[ii] * yar[ii] ;
      vdxphi  += xar[ii] * par[ii] ;
      vdyphi  += yar[ii] * par[ii] ;
   }
   cov_all.mat[0][0] = vdxdx ;
   cov_all.mat[1][1] = vdydy ;
   cov_all.mat[2][2] = vphiphi ;
   cov_all.mat[0][1] = vdxdy ;
   cov_all.mat[1][0] = vdxdy ;
   cov_all.mat[0][2] = vdxphi ;
   cov_all.mat[2][0] = vdxphi ;
   cov_all.mat[1][2] = vdyphi ;
   cov_all.mat[2][1] = vdyphi ;

   det_all = DMAT_DET(cov_all) ;

   iment = mri_new( nx , ny , MRI_float ) ;
   entar = MRI_FLOAT_PTR(iment) ;

   for( ii=0 ; ii < npix ; ii++ ){
      vdxdx   = SQR( xar[ii] ) ;
      vdydy   = SQR( yar[ii] ) ;
      vphiphi = SQR( par[ii] ) ;
      vdxdy   = xar[ii] * yar[ii] ;
      vdxphi  = xar[ii] * par[ii] ;
      vdyphi  = yar[ii] * par[ii] ;

      cov_one = cov_all ;

      cov_one.mat[0][0] -= vdxdx ;
      cov_one.mat[1][1] -= vdydy ;
      cov_one.mat[2][2] -= vphiphi ;
      cov_one.mat[0][1] -= vdxdy ;
      cov_one.mat[1][0] -= vdxdy ;
      cov_one.mat[0][2] -= vdxphi ;
      cov_one.mat[2][0] -= vdxphi ;
      cov_one.mat[1][2] -= vdyphi ;
      cov_one.mat[2][1] -= vdyphi ;

      det_one = DMAT_DET(cov_one) ;

      entar[ii] = log( det_all / det_one ) ;
   }

   mri_free(imdx)  ; mri_free(imdy)  ; mri_free(imphi) ;

   return iment ;
}
