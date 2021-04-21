/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

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

   cfft2d_cox( mode , im->nx , im->ny , rbuf,ibuf ) ;

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
      tap[ii]      = 0.54 - 0.46 * cos( ii*phi ) ;
      tap[nx-1-ii] = tap[ii] ;
   }

   return tap ;
}

/*----------------------------------------------------------------------------*/
/* macro to alternate signs in workspace array */

#undef  ALTERN
#define ALTERN(nn)                                                                  \
 do{ register int qq;                                                               \
     for( qq=1; qq<(nn); qq+=2 ){ cbig[qq].r=-cbig[qq].r; cbig[qq].i=-cbig[qq].i; } \
 } while(0)

/*******
   Note that thd_ballcorr.c has OpenMP compatible versions of these
   3D FFT functions, which could make your life better (or at least faster).
********/
/*----------------------------------------------------------------------------*/
/* FFT lengths are in Lxx, Lyy, Lzz; however,
     Lxx = 0 ==> no FFT in that direction (etc.).
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_fft_3D( int Sign, MRI_IMAGE *inim,
                        int Lxx,int Lyy,int Lzz, int alt )
{
   MRI_IMAGE *outim ;
   int ii,jj,kk , nx,ny,nxy,nz , nbig , fx,fy,fz,fxy , joff,koff ;
   complex *cbig , *car , *far ;

   if( inim->kind != MRI_complex ) return NULL ;

   /* input data and its dimensions */

   car = MRI_COMPLEX_PTR(inim) ;
   nx = inim->nx ; ny = inim->ny ; nz = inim->nz ; nxy = nx*ny ;

   /* output dimensions and data */

   fx = (Lxx == 0) ? nx : (Lxx > nx) ? csfft_nextup_one35(Lxx) : csfft_nextup_one35(nx);
   fy = (Lyy == 0) ? ny : (Lyy > ny) ? csfft_nextup_one35(Lyy) : csfft_nextup_one35(ny);
   fz = (Lzz == 0) ? nz : (Lzz > nz) ? csfft_nextup_one35(Lzz) : csfft_nextup_one35(nz);
   fxy = fx*fy ;

   outim = mri_new_vol( fx,fy,fz , MRI_complex ) ;  /* zero filled */
   far   = MRI_COMPLEX_PTR(outim) ;

   /* buffer space */

   nbig = MAX(fx,fy) ; nbig = MAX(nbig,fz) ; nbig = 4*nbig + 512 ;
   cbig = (complex *)malloc(sizeof(complex)*nbig) ;

   /* copy input data into output image */

   for( kk=0 ; kk < nz ; kk++ )
     for( jj=0 ; jj < ny ; jj++ )
       memcpy( far + jj*fx + kk*fxy, car + jj*nx + kk*nxy, sizeof(complex)*nx );

   /* x-direction FFTs */

   if( Lxx > 1 ){
     for( kk=0 ; kk < fz ; kk++ ){
       koff = kk*fxy ;
       for( jj=0 ; jj < fy ; jj++ ){
         joff = koff + jj*fx ;
         for( ii=0 ; ii < fx ; ii++ ) cbig[ii] = far[ii+joff] ;
         if( alt > 0 ) ALTERN(fx) ;
         csfft_cox( Sign , fx , cbig ) ;
         if( alt < 0 ) ALTERN(fx) ;
         for( ii=0 ; ii < fx ; ii++ ) far[ii+joff] = cbig[ii] ;
       }
     }
   }

   /* y-direction FFTs */

   if( Lyy > 1 ){
     for( kk=0 ; kk < fz ; kk++ ){
       koff = kk*fxy ;
       for( ii=0 ; ii < fx ; ii++ ){
         joff = koff + ii ;
         for( jj=0 ; jj < fy ; jj++ ) cbig[jj] = far[jj*fx+joff] ; /* copy data */
         if( alt > 0 ) ALTERN(fy) ;
         csfft_cox( Sign , fy , cbig ) ;                       /* FFT in buffer */
         if( alt < 0 ) ALTERN(fy) ;
         for( jj=0 ; jj < fy ; jj++ ) far[jj*fx+joff] = cbig[jj] ; /* copy back */
       }
     }
   }

   /* z-direction FFTs */

   if( Lzz > 1 ){
     for( jj=0 ; jj < fy ; jj++ ){
       joff = jj*fx ;
       for( ii=0 ; ii < fx ; ii++ ){
         koff = joff + ii ;
         for( kk=0 ; kk < fz ; kk++ ) cbig[kk] = far[kk*fxy+koff] ;
         if( alt > 0 ) ALTERN(fz) ;
         csfft_cox( Sign , fz , cbig ) ;
         if( alt < 0 ) ALTERN(fz) ;
         for( kk=0 ; kk < fz ; kk++ ) far[kk*fxy+koff] = cbig[kk] ;
       }
     }
   }

   free(cbig) ; MRI_COPY_AUX(outim,inim) ; return outim ;
}

/*----------------------------------------------------------------------------*/
/* Convolve (via FFT) image aim with bim.    [Sep 2020]
   Note output image will be at least as big as than the sum of the two sizes.
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_fft_3Dconvolve( MRI_IMAGE *aim , MRI_IMAGE *bim )
{
   MRI_IMAGE *outim=NULL ;
   MRI_IMAGE *paim , *pbim , *faim , *fbim ;
   int nxa,nya,nza , nxb,nyb,nzb , Lxx,Lyy,Lzz , Lxyz,ii ;
   complex  ac   ,  bc   , qc ;
   complex *acar , *bcar ;
   float linv ;

   if( aim == NULL || bim == NULL ) return NULL ;

   /* input dimensions */

   nxa = aim->nx ; nya = aim->ny ; nza = aim->nz ;
   nxb = bim->nx ; nyb = bim->ny ; nzb = bim->nz ;

   /* FFT and output dimensions (sum, bumped up for FFT effiency) */

   Lxx = (nxa > 1 && nxb > 1) ? csfft_nextup_one35(nxa+nxb) : 0 ;
   Lyy = (nya > 1 && nyb > 1) ? csfft_nextup_one35(nya+nyb) : 0 ;
   Lzz = (nza > 1 && nzb > 1) ? csfft_nextup_one35(nza+nzb) : 0 ;

   /* at this time, we don't allow for convolving a 3D image with a 1D
      or 2D image, for example, which is possible but more complicated */

   if( Lxx == 0 || Lyy == 0 || Lzz == 0 ) return NULL ;

   /* 1) convert A image to complex
      2) zero pad it to fit the FFT size
      3) FFT that
      Then repeat these steps for the B image */

   faim = mri_to_complex( aim ) ;                                      /* 1) */
   paim = mri_zeropad_3D( 0,Lxx-nxa , 0,Lyy-nya , 0,Lzz-nza , faim ) ; /* 2) */
   mri_free(faim) ;
   faim = mri_fft_3D( -1 , paim , Lxx,Lyy,Lzz , 0 ) ;                  /* 3) */
   mri_free(paim) ;
   acar = MRI_COMPLEX_PTR(faim) ;

   fbim = mri_to_complex( bim ) ;                                      /* 1) */
   pbim = mri_zeropad_3D( 0,Lxx-nxb , 0,Lyy-nyb , 0,Lzz-nzb , fbim ) ; /* 2) */
   mri_free(fbim) ;
   fbim = mri_fft_3D( -1 , pbim , Lxx,Lyy,Lzz , 0 ) ;                  /* 3) */
   mri_free(pbim) ;
   bcar = MRI_COMPLEX_PTR(fbim) ;

   /* multiply+scale FFTs, store back in faim/acar */

   Lxyz = Lxx * Lyy * Lzz ;
   linv = 10.f / (float)Lxyz ;       /* scaling for inverse FFT */
   for( ii=0 ; ii < Lxyz ; ii++ ){
     ac = acar[ii] ;
     bc = bcar[ii] ;
     qc.r = (ac.r * bc.r - ac.i * bc.i) * linv ; /* complex */
     qc.i = (ac.r * bc.i + ac.i * bc.r) * linv ; /* multiply */
     acar[ii] = qc ;
   }
   mri_free(fbim) ;

   /* inverse FFT back to 'real' space */

   fbim = mri_fft_3D( +1 , faim , Lxx,Lyy,Lzz , 0 ) ;
   mri_free(faim) ;

   /* convert to float-valued image (from complex FFT) and return */

   outim = mri_complex_to_real( fbim ) ;
   mri_free(fbim) ;
   return outim ;
}
