#include "mrilib.h"

/*** NOT 7D SAFE ***/

/**************************************************************************/
/*
    mode = -1 for forward transform
         = +1 for inverse (including scaling)

   taper = fraction of data to taper (0 to 1)
*/

void mri_fft_complex( int mode , float taper , MRI_IMAGE *im )
{
   float *rbuf , *ibuf , *xtap , *ytap ;
   complex *cxim ;
   int ii , jj , npix , jbase , nx,ny ;

WHOAMI ; IMHEADER(im) ;

   if( im->kind != MRI_complex ){
      fprintf( stderr , "mri_fft_complex only works on complex images!\n" ) ;
      MRI_FATAL_ERROR ;
   }

   if( ! MRI_IS_2D(im) ){
      fprintf(stderr,"mri_fft_complex only works on 2D images!\n") ;
      MRI_FATAL_ERROR ;
   }

   /*** set up buffers ***/

   npix = im->nx * im->ny ;                           /* number of pixels */
   rbuf = (float *)malloc( sizeof(float) * npix ) ;   /* real and imag buffs */
   ibuf = (float *)malloc( sizeof(float) * npix ) ;
   cxim = mri_data_pointer( im ) ;                    /* easy acces to im */

   for( ii=0 ; ii < npix ; ii++ ){
      rbuf[ii] = cxim[ii].r ;
      ibuf[ii] = cxim[ii].i ;
   }

   /*** taper buffers, if desired ***/

   if( taper > 0.0 && taper <= 1.0 ){
      nx   = im->nx ;
      ny   = im->ny ;
      xtap = mri_setup_taper( nx , taper ) ;

/***
      printf( "taper" ) ;
      for( ii=0 ; ii < nx ; ii++ ){
         if( (ii%5) == 0 ) printf("\n") ;
         printf( "%12.4e " , xtap[ii] ) ;
      }
      printf("\n") ;
***/

      if( nx == ny ) ytap = xtap ;
      else           ytap = mri_setup_taper( ny , taper ) ;

      for( jj=0 ; jj < ny ; jj++ ){
         jbase = jj * nx ;
         for( ii=0 ; ii < nx ; ii++ ){
            rbuf[ii] *= xtap[ii] * ytap[jj] ;
            ibuf[ii] *= xtap[ii] * ytap[jj] ;
         }
      }
      free( xtap ) ;
      if( ytap != xtap ) free(ytap) ;
   }

   /*** FFT buffers and copy them back to original image ***/

   cfft2d( mode , im->nx , im->ny , rbuf,ibuf ) ;

   for( ii=0 ; ii < npix ; ii++ ){
      cxim[ii].r = rbuf[ii] ;
      cxim[ii].i = ibuf[ii] ;
   }

   return ;
}

/***********************************************************************/

float *mri_setup_taper( int nx , float taper )
{
   register int ii ;
   int ntap ;
   float *tap ;
   float phi ; 

   tap = (float *)malloc( sizeof(float) * nx ) ;   /* make array */

   for( ii=0 ; ii < nx ; ii++ ) tap[ii] = 1.0 ;    /* default 1's */

   ntap = (int) (nx * 0.5 * taper ) ;              /* # pts on each end */

   if( ntap == 0 ){             /* special case of no points: taper is tiny */
      tap[0] = tap[nx-1] = 0.5 ;
      return tap ;
   }

   phi = PI / ntap ;
   for( ii=0 ; ii < ntap ; ii++ ){
      tap[ii]      = 0.5 - 0.5 * cos( ii*phi ) ;
      tap[nx-1-ii] = tap[ii] ;
   }

   return tap ;
}
