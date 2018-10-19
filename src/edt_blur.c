/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/**************************************************************************/
/***** prototypes for FIR blurring functions at the end of this file ******/

void fir_blurx( int m, float *wt,int nx, int ny, int nz, float *f ) ;
void fir_blury( int m, float *wt,int nx, int ny, int nz, float *f ) ;
void fir_blurz( int m, float *wt,int nx, int ny, int nz, float *f ) ;

static void fir_gaussian_load( int m, float dx, float *ff ) ; /* 03 Apr 2007 */

#undef  FIR_MAX
#define FIR_MAX 35  /** max length of FIR filter to use instead of FFTs **/

#undef  DO_INFO
#define DO_INFO 0   /** for debugging and timing **/

/*! If set to 0, EDIT_blur_volume() will not use the fir_blur? functions. */

static int allow_fir = 1 ;

/*! Controls whether EDIT_blur_volume() will use the fir_blur? functions. */

void EDIT_blur_allow_fir( int i ){ allow_fir = i; }

/*************************************************************************/
/*!  Routine to blur a 3D volume with a Gaussian, using FFTs.
     If the blurring parameter (sigma) is small enough, will use
     real-space FIR filter instead.  This function actually just
     calls EDIT_blur_volume_3d() to do the real work.
**************************************************************************/

void EDIT_blur_volume( int nx, int ny, int nz,
                       float dx, float dy, float dz,
                       int ftype , void * vfim , float sigma )
{
  EDIT_blur_volume_3d (nx, ny, nz, dx, dy, dz, ftype, vfim,
                      sigma, sigma, sigma);
}

/**************************************************************************/
/*! The following slightly modified version of EDIT_blur_volume allows
    independent specification of Gaussian filter widths along the three
    perpendicular axes.
     - BDW - 21 Feb 1997
     - RWC - 04 Feb 2005: use fir_blur? function if sigma? is small
     - also see EDIT_blur_allow_fir() and FIR_blur_volume_3d()
--------------------------------------------------------------------------*/

void EDIT_blur_volume_3d( int   nx, int   ny, int   nz,
                          float dx, float dy, float dz,
                          int ftype , void *vfim ,
                          float sigmax, float sigmay, float sigmaz )
{
   int jj,kk , nxy , base,nby2 ;
   float  dk , aa , k , fac ;
   register int ii , nup ;

   complex *cx = NULL ;
   float   *gg = NULL ; int ncg=0 ;

   byte     *bfim = NULL ;       /* pointers to data array vfim */
   short    *sfim = NULL ;
   float    *ffim = NULL ;
   complex  *cfim = NULL ;

   float fbot=0.0,ftop=0.0 ;     /* 10 Jan 2003: for clipping results */
   int nxyz ;            /* number of voxels */

   int   fir_m , fir_num=0 ;  /* 03 Oct 2005: for fir_blur? filtering */
   float fir_wt[FIR_MAX+1] ;
   int all_fir=0 ;            /* 06 Oct 2005 */

   int as_float = 0 ;   /* process as float (to apply FIR) 15 Jun 2009 [rickr] */
                        /* - convert short/byte to float, blur, convert back   */
   int dtype = ftype ;  /* processing datatype (ftype, if no conversion)       */

   double sfac = AFNI_numenv("AFNI_BLUR_FIRFAC") ;
   if( sfac < 2.0 ) sfac = 2.5 ;

   /***---------- initialize ----------***/

ENTRY("EDIT_blur_volume_3d") ;

   if( vfim == NULL ) EXRETURN ;  /* no data? */

   if( sigmax <= 0.0 && sigmay <= 0.0 && sigmaz <= 0.0 ) EXRETURN ;

   if( dx <= 0.0 ) dx = 1.0 ;  /* 03 Oct 2005: regularize grid sizes */
   if( dy <= 0.0 ) dy = dx  ;
   if( dz <= 0.0 ) dz = dx  ;

   switch( ftype ){            /* cast pointer to correct type */
      default: EXRETURN ;
      case MRI_short:   sfim = (short *)   vfim ; break ;
      case MRI_float:   ffim = (float *)   vfim ; break ;
      case MRI_byte:    bfim = (byte *)    vfim ; break ;
      case MRI_complex: cfim = (complex *) vfim ; break ;
   }
   nxy = nx * ny ; nxyz = nxy * nz ;

   /*** 10 Jan 2003: find bot and top of data input */

   if( allow_fir ){
     ii = (int) ceil( sfac * sigmax / dx ) ;
     jj = (int) ceil( sfac * sigmay / dy ) ;
     kk = (int) ceil( sfac * sigmaz / dz ) ;
     if( ii <= FIR_MAX && jj <= FIR_MAX && kk <= FIR_MAX ) all_fir = 1 ;

     /* try to process most data using Finite Impule Response, not with Fourier
        interpolation (leads to ringing problems both inside and outside a mask)
        - set AFNI_BLUR_INTS_AS_OLD=YES to use old method    15 Jun 2009 [rickr] */
     if( (ftype == MRI_short || ftype == MRI_byte) &&
         ! AFNI_yesenv("AFNI_BLUR_INTS_AS_OLD") ) {
        ffim = (float *)malloc(nxyz * sizeof(float));
        as_float = 1;           /* flag, redundant but clear */
        dtype = MRI_float;      /* process data as float */
     }
   }
   if( ftype != MRI_float ) all_fir = 0 ;  /* 17 Nov 2005: oopsie */

   if( !all_fir ){
    switch( ftype ){
     default:
       fbot = ftop = 0.0 ;  /* for complex */
     break ;

     case MRI_short:
       fbot = ftop = sfim[0] ;
       for( ii=1 ; ii < nxyz ; ii++ ) {
              if( sfim[ii] < fbot ) fbot = sfim[ii] ;
         else if( sfim[ii] > ftop ) ftop = sfim[ii] ;
         if( as_float ) ffim[ii] = (float)sfim[ii] ; /* copy data as int */
       }
     break ;

     case MRI_float:
       fbot = ftop = ffim[0] ;
       for( ii=1 ; ii < nxyz ; ii++ )
              if( ffim[ii] < fbot ) fbot = ffim[ii] ;
         else if( ffim[ii] > ftop ) ftop = ffim[ii] ;
     break ;

     case MRI_byte:
       fbot = ftop = bfim[0] ;
       for( ii=1 ; ii < nxyz ; ii++ ) {
              if( bfim[ii] < fbot ) fbot = bfim[ii] ;
         else if( bfim[ii] > ftop ) ftop = bfim[ii] ;
         if( as_float ) ffim[ii] = (float)bfim[ii] ; /* copy data as int */
       }
     break ;
    }
   }

   /*** do x-direction ***/

   /** 03 Oct 2005: perhaps do the x-blur in real-space? **/

   if( nx < 2 || sigmax <= 0.0 ){
     STATUS("skipping x blur") ; fir_num++ ; goto DO_Y_BLUR ;
   }

   fir_m = (int) ceil( sfac * sigmax / dx ) ;
   if( allow_fir && dtype == MRI_float && fir_m <= FIR_MAX ){
     STATUS("start x FIR") ;
     if( fir_m < 1 ) fir_m = 1 ;
     fir_gaussian_load( fir_m , dx/sigmax , fir_wt ) ;
#if 0
     fac = fir_wt[0] = 1.0f ;
     for( ii=1 ; ii <= fir_m ; ii++ ){
       fir_wt[ii] = exp(-0.5*(ii*dx)*(ii*dx)/(sigmax*sigmax)) ;
       fac += 2.0f * fir_wt[ii] ;
     }
     fac = 1.0f / fac ;
     for( ii=0 ; ii <= fir_m ; ii++ ) fir_wt[ii] *= fac ;
#endif
     fir_blurx( fir_m , fir_wt , nx,ny,nz , ffim ) ;
     fir_num++ ; goto DO_Y_BLUR ;
   }

STATUS("start x FFTs") ;

   aa  = sigmax * sigmax * 0.5 ;
   nup = nx + (int)(3.0 * sigmax / dx) ;      /* min FFT length */
   nup = csfft_nextup_one35(nup) ; nby2 = nup / 2 ;

   if(DO_INFO) INFO_message("x FFT(%d)",nup) ;

   if( nup > ncg ){
     cx = (complex *)realloc(cx,sizeof(complex)*nup) ;
     gg = (float   *)realloc(gg,sizeof(float  )*nup) ; ncg = nup ;
   }

   dk    = (2.0*PI) / (nup * dx) ;
   fac   = 1.0 / nup ;
   gg[0] = fac ;
   for( ii=1 ; ii<=nby2 ; ii++ ){ k=ii*dk; gg[nup-ii]=gg[ii]=fac*exp(-aa*k*k); }

   /** July 20: double up on FFTs **/
   /** Feb  09: extend to other data types besides shorts;
                doubling up does not apply to complex data! **/

   switch( dtype ){
      case MRI_short:{
         register short *qfim ;
         for( kk=0 ; kk < nz ; kk++ ){
            for( jj=0 ; jj < ny ; jj+=2 ){
               base = jj*nx + kk*nxy ;
               qfim = sfim + base ;
               if( jj == ny-1 )
                  for( ii=0 ; ii<nx ; ii++){ cx[ii].r = qfim[ii] ; cx[ii].i = 0.0 ; }
               else
                  for( ii=0 ; ii<nx ; ii++){ cx[ii].r = qfim[ii] ; cx[ii].i = qfim[ii+nx] ; }
               for( ii=nx; ii<nup; ii++){ cx[ii].r = cx[ii].i = 0.0 ; }
               csfft_cox( -1 , nup , cx ) ;
               for( ii=0 ; ii<nup; ii++){ cx[ii].r *= gg[ii] ; cx[ii].i *= gg[ii] ; }
               csfft_cox(  1 , nup , cx ) ;
               if( jj == ny-1 )
                  for( ii=0 ; ii<nx ; ii++){ qfim[ii] = cx[ii].r ; }
               else
                  for( ii=0 ; ii<nx ; ii++){ qfim[ii] = cx[ii].r ; qfim[ii+nx] = cx[ii].i ; }
            }
         }
      }
      break ;

      case MRI_float:{
         register float *qfim ;
         for( kk=0 ; kk < nz ; kk++ ){
            for( jj=0 ; jj < ny ; jj+=2 ){
               base = jj*nx + kk*nxy ;
               qfim = ffim + base ;
               if( jj == ny-1 )
                  for( ii=0 ; ii<nx ; ii++){ cx[ii].r = qfim[ii] ; cx[ii].i = 0.0 ; }
               else
                  for( ii=0 ; ii<nx ; ii++){ cx[ii].r = qfim[ii] ; cx[ii].i = qfim[ii+nx] ; }
               for( ii=nx; ii<nup; ii++){ cx[ii].r = cx[ii].i = 0.0 ; }
               csfft_cox( -1 , nup , cx ) ;
               for( ii=0 ; ii<nup; ii++){ cx[ii].r *= gg[ii] ; cx[ii].i *= gg[ii] ; }
               csfft_cox(  1 , nup , cx ) ;
               if( jj == ny-1 )
                  for( ii=0 ; ii<nx ; ii++){ qfim[ii] = cx[ii].r ; }
               else
                  for( ii=0 ; ii<nx ; ii++){ qfim[ii] = cx[ii].r ; qfim[ii+nx] = cx[ii].i ; }
            }
         }
      }
      break ;

      case MRI_byte:{
         register byte *qfim ;
         for( kk=0 ; kk < nz ; kk++ ){
            for( jj=0 ; jj < ny ; jj+=2 ){
               base = jj*nx + kk*nxy ;
               qfim = bfim + base ;
               if( jj == ny-1 )
                  for( ii=0 ; ii<nx ; ii++){ cx[ii].r = qfim[ii] ; cx[ii].i = 0.0 ; }
               else
                  for( ii=0 ; ii<nx ; ii++){ cx[ii].r = qfim[ii] ; cx[ii].i = qfim[ii+nx] ; }
               for( ii=nx; ii<nup; ii++){ cx[ii].r = cx[ii].i = 0.0 ; }
               csfft_cox( -1 , nup , cx ) ;
               for( ii=0 ; ii<nup; ii++){ cx[ii].r *= gg[ii] ; cx[ii].i *= gg[ii] ; }
               csfft_cox(  1 , nup , cx ) ;
               if( jj == ny-1 )
                  for( ii=0 ; ii<nx ; ii++){ qfim[ii] = cx[ii].r ; }
               else
                  for( ii=0 ; ii<nx ; ii++){ qfim[ii] = cx[ii].r ; qfim[ii+nx] = cx[ii].i ; }
            }
         }
      }
      break ;

      case MRI_complex:{
         register complex *qfim ;
         for( kk=0 ; kk < nz ; kk++ ){
            for( jj=0 ; jj < ny ; jj++ ){
               base = jj*nx + kk*nxy ;
               qfim = cfim + base ;
               for( ii=0 ; ii<nx ; ii++) { cx[ii] = qfim[ii] ; }
               for( ii=nx; ii<nup; ii++){ cx[ii].r = cx[ii].i = 0.0 ; }
               csfft_cox( -1 , nup , cx ) ;
               for( ii=0 ; ii<nup; ii++){ cx[ii].r *= gg[ii] ; cx[ii].i *= gg[ii] ; }
               csfft_cox(  1 , nup , cx ) ;
               for( ii=0 ; ii<nx ; ii++){ qfim[ii] = cx[ii] ; }
            }
         }
      }
      break ;
   }

   /*** do y-direction ***/
 DO_Y_BLUR:

   /** 03 Oct 2005: perhaps do the y-blur in real-space? **/

   if( ny < 2 || sigmay <= 0.0 ){
     STATUS("skip y blur") ; fir_num++ ; goto DO_Z_BLUR ;
   }

   fir_m = (int) ceil( sfac * sigmay / dy ) ;
   if( allow_fir && dtype == MRI_float && fir_m <= FIR_MAX ){
     STATUS("start y FIR") ;
     if( fir_m < 1 ) fir_m = 1 ;
     fir_gaussian_load( fir_m , dy/sigmay , fir_wt ) ;
#if 0
     fac = fir_wt[0] = 1.0f ;
     for( ii=1 ; ii <= fir_m ; ii++ ){
       fir_wt[ii] = exp(-0.5*(ii*dy)*(ii*dy)/(sigmay*sigmay)) ;
       fac += 2.0f * fir_wt[ii] ;
     }
     fac = 1.0f / fac ;
     for( ii=0 ; ii <= fir_m ; ii++ ) fir_wt[ii] *= fac ;
#endif
     fir_blury( fir_m , fir_wt , nx,ny,nz , ffim ) ;
     fir_num++ ; goto DO_Z_BLUR ;
   }

STATUS("start y FFTs") ;

   aa  = sigmay * sigmay * 0.5 ;
   nup = ny + (int)(3.0 * sigmay / dy) ;      /* min FFT length */
   nup = csfft_nextup_one35(nup) ; nby2 = nup / 2 ;

   if(DO_INFO) INFO_message("y FFT(%d)",nup) ;

   if( nup > ncg ){
     cx = (complex *)realloc(cx,sizeof(complex)*nup) ;
     gg = (float   *)realloc(gg,sizeof(float  )*nup) ; ncg = nup ;
   }

   dk    = (2.0*PI) / (nup * dy) ;
   fac   = 1.0 / nup ;
   gg[0] = fac ;
   for( ii=1 ; ii<=nby2 ; ii++ ){ k=ii*dk; gg[nup-ii]=gg[ii]=fac*exp(-aa*k*k); }

   switch( dtype ){
      case MRI_short:{
         register short *qfim ;
         for( kk=0 ; kk < nz ; kk++ ){
            for( jj=0 ; jj < nx ; jj+=2 ){
               base = jj + kk*nxy ;
               qfim = sfim + base ;
               if( jj == nx-1 )
                  for( ii=0 ; ii<ny ; ii++){ cx[ii].r = qfim[ii*nx] ; cx[ii].i = 0.0 ; }
               else
                  for( ii=0 ; ii<ny ; ii++){ cx[ii].r = qfim[ii*nx] ; cx[ii].i = qfim[ii*nx+1] ; }
               for( ii=ny; ii<nup; ii++){ cx[ii].r = cx[ii].i = 0.0 ; }
               csfft_cox( -1 , nup , cx ) ;
               for( ii=0 ; ii<nup; ii++){ cx[ii].r *= gg[ii] ; cx[ii].i *= gg[ii] ; }
               csfft_cox(  1 , nup , cx ) ;
               if( jj == nx-1 )
                  for( ii=0 ; ii<ny ; ii++){ qfim[ii*nx] = cx[ii].r ; }
               else
                  for( ii=0 ; ii<ny ; ii++){ qfim[ii*nx] = cx[ii].r ; qfim[ii*nx+1] = cx[ii].i ; }
            }
         }
      }
      break ;

      case MRI_byte:{
         register byte *qfim ;
         for( kk=0 ; kk < nz ; kk++ ){
            for( jj=0 ; jj < nx ; jj+=2 ){
               base = jj + kk*nxy ;
               qfim = bfim + base ;
               if( jj == nx-1 )
                  for( ii=0 ; ii<ny ; ii++){ cx[ii].r = qfim[ii*nx] ; cx[ii].i = 0.0 ; }
               else
                  for( ii=0 ; ii<ny ; ii++){ cx[ii].r = qfim[ii*nx] ; cx[ii].i = qfim[ii*nx+1] ; }
               for( ii=ny; ii<nup; ii++){ cx[ii].r = cx[ii].i = 0.0 ; }
               csfft_cox( -1 , nup , cx ) ;
               for( ii=0 ; ii<nup; ii++){ cx[ii].r *= gg[ii] ; cx[ii].i *= gg[ii] ; }
               csfft_cox(  1 , nup , cx ) ;
               if( jj == nx-1 )
                  for( ii=0 ; ii<ny ; ii++){ qfim[ii*nx] = cx[ii].r ; }
               else
                  for( ii=0 ; ii<ny ; ii++){ qfim[ii*nx] = cx[ii].r ; qfim[ii*nx+1] = cx[ii].i ; }
            }
         }
      }
      break ;

      case MRI_float:{
         register float *qfim ;
         for( kk=0 ; kk < nz ; kk++ ){
            for( jj=0 ; jj < nx ; jj+=2 ){
               base = jj + kk*nxy ;
               qfim = ffim + base ;
               if( jj == nx-1 )
                  for( ii=0 ; ii<ny ; ii++){ cx[ii].r = qfim[ii*nx] ; cx[ii].i = 0.0 ; }
               else
                  for( ii=0 ; ii<ny ; ii++){ cx[ii].r = qfim[ii*nx] ; cx[ii].i = qfim[ii*nx+1] ; }
               for( ii=ny; ii<nup; ii++){ cx[ii].r = cx[ii].i = 0.0 ; }
               csfft_cox( -1 , nup , cx ) ;
               for( ii=0 ; ii<nup; ii++){ cx[ii].r *= gg[ii] ; cx[ii].i *= gg[ii] ; }
               csfft_cox(  1 , nup , cx ) ;
               if( jj == nx-1 )
                  for( ii=0 ; ii<ny ; ii++){ qfim[ii*nx] = cx[ii].r ; }
               else
                  for( ii=0 ; ii<ny ; ii++){ qfim[ii*nx] = cx[ii].r ; qfim[ii*nx+1] = cx[ii].i ; }
            }
         }
      }
      break ;

      case MRI_complex:{
         register complex *qfim ;
         for( kk=0 ; kk < nz ; kk++ ){
            for( jj=0 ; jj < nx ; jj++ ){
               base = jj + kk*nxy ;
               qfim = cfim + base ;
               for( ii=0 ; ii<ny ; ii++){ cx[ii] = qfim[ii*nx] ; }
               for( ii=ny; ii<nup; ii++){ cx[ii].r = cx[ii].i = 0.0 ; }
               csfft_cox( -1 , nup , cx ) ;
               for( ii=0 ; ii<nup; ii++){ cx[ii].r *= gg[ii] ; cx[ii].i *= gg[ii] ; }
               csfft_cox(  1 , nup , cx ) ;
               for( ii=0 ; ii<ny ; ii++){ qfim[ii*nx] = cx[ii] ; }
            }
         }
      }
      break ;
   }

   /*** do z-direction ***/
 DO_Z_BLUR:

   /** 03 Oct 2005: perhaps do the z-blur in real-space? **/

   /* sigmay to sizmaz, noted by Patryk on MB   21 July, 2011 [rickr] */
   if( nz < 2 || sigmaz <= 0.0 ){
     STATUS("skip z blur") ; fir_num++ ; goto ALL_DONE_NOW ;
   }

   fir_m = (int) ceil( sfac * sigmaz / dz ) ;
   if( allow_fir && dtype == MRI_float && fir_m <= FIR_MAX ){
     STATUS("start z FIR") ;
     if( fir_m < 1 ) fir_m = 1 ;
     fir_gaussian_load( fir_m , dz/sigmaz , fir_wt ) ;
#if 0
     fac = fir_wt[0] = 1.0f ;
     for( ii=1 ; ii <= fir_m ; ii++ ){
       fir_wt[ii] = exp(-0.5*(ii*dz)*(ii*dz)/(sigmaz*sigmaz)) ;
       fac += 2.0f * fir_wt[ii] ;
     }
     fac = 1.0f / fac ;
     for( ii=0 ; ii <= fir_m ; ii++ ) fir_wt[ii] *= fac ;
#endif
     fir_blurz( fir_m , fir_wt , nx,ny,nz , ffim ) ;
     fir_num++ ; goto ALL_DONE_NOW ;
   }

STATUS("start z FFTs") ;

   aa  = sigmaz * sigmaz * 0.5 ;
   nup = nz + (int)(3.0 * sigmaz / dz) ;      /* min FFT length */
   nup = csfft_nextup_one35(nup) ; nby2 = nup / 2 ;

   if(DO_INFO) INFO_message("z FFT(%d)",nup) ;

   if( nup > ncg ){
     cx = (complex *)realloc(cx,sizeof(complex)*nup) ;
     gg = (float   *)realloc(gg,sizeof(float  )*nup) ; ncg = nup ;
   }

   dk    = (2.0*PI) / (nup * dz) ;
   fac   = 1.0 / nup ;
   gg[0] = fac ;
   for( ii=1 ; ii<=nby2 ; ii++ ){ k=ii*dk; gg[nup-ii]=gg[ii]=fac*exp(-aa*k*k); }

   switch( dtype ){
      case MRI_short:{
         register short *qfim ;
         for( kk=0 ; kk < ny ; kk++ ){
            for( jj=0 ; jj < nx ; jj+=2 ){
               base = jj + kk*nx ;
               qfim = sfim + base ;
               if( jj == nx-1 )
                  for( ii=0 ; ii<nz ; ii++){ cx[ii].r = qfim[ii*nxy] ; cx[ii].i = 0.0 ; }
               else
                  for( ii=0 ; ii<nz ; ii++){ cx[ii].r = qfim[ii*nxy] ; cx[ii].i = qfim[ii*nxy+1] ; }
               for( ii=nz; ii<nup; ii++){ cx[ii].r = cx[ii].i = 0.0 ; }
               csfft_cox( -1 , nup , cx ) ;
               for( ii=0 ; ii<nup; ii++){ cx[ii].r *= gg[ii] ; cx[ii].i *= gg[ii] ; }
               csfft_cox(  1 , nup , cx ) ;
               if( jj == nx-1 )
                  for( ii=0 ; ii<nz ; ii++){ qfim[ii*nxy] = cx[ii].r ; }
               else
                  for( ii=0 ; ii<nz ; ii++){ qfim[ii*nxy] = cx[ii].r ; qfim[ii*nxy+1] = cx[ii].i ; }
            }
         }
      }
      break ;

      case MRI_float:{
         register float *qfim ;
         for( kk=0 ; kk < ny ; kk++ ){
            for( jj=0 ; jj < nx ; jj+=2 ){
               base = jj + kk*nx ;
               qfim = ffim + base ;
               if( jj == nx-1 )
                  for( ii=0 ; ii<nz ; ii++){ cx[ii].r = qfim[ii*nxy] ; cx[ii].i = 0.0 ; }
               else
                  for( ii=0 ; ii<nz ; ii++){ cx[ii].r = qfim[ii*nxy] ; cx[ii].i = qfim[ii*nxy+1] ; }
               for( ii=nz; ii<nup; ii++){ cx[ii].r = cx[ii].i = 0.0 ; }
               csfft_cox( -1 , nup , cx ) ;
               for( ii=0 ; ii<nup; ii++){ cx[ii].r *= gg[ii] ; cx[ii].i *= gg[ii] ; }
               csfft_cox(  1 , nup , cx ) ;
               if( jj == nx-1 )
                  for( ii=0 ; ii<nz ; ii++){ qfim[ii*nxy] = cx[ii].r ; }
               else
                  for( ii=0 ; ii<nz ; ii++){ qfim[ii*nxy] = cx[ii].r ; qfim[ii*nxy+1] = cx[ii].i ; }
            }
         }
      }
      break ;

      case MRI_byte:{
         register byte *qfim ;
         for( kk=0 ; kk < ny ; kk++ ){
            for( jj=0 ; jj < nx ; jj+=2 ){
               base = jj + kk*nx ;
               qfim = bfim + base ;
               if( jj == nx-1 )
                  for( ii=0 ; ii<nz ; ii++){ cx[ii].r = qfim[ii*nxy] ; cx[ii].i = 0.0 ; }
               else
                  for( ii=0 ; ii<nz ; ii++){ cx[ii].r = qfim[ii*nxy] ; cx[ii].i = qfim[ii*nxy+1] ; }
               for( ii=nz; ii<nup; ii++){ cx[ii].r = cx[ii].i = 0.0 ; }
               csfft_cox( -1 , nup , cx ) ;
               for( ii=0 ; ii<nup; ii++){ cx[ii].r *= gg[ii] ; cx[ii].i *= gg[ii] ; }
               csfft_cox(  1 , nup , cx ) ;
               if( jj == nx-1 )
                  for( ii=0 ; ii<nz ; ii++){ qfim[ii*nxy] = cx[ii].r ; }
               else
                  for( ii=0 ; ii<nz ; ii++){ qfim[ii*nxy] = cx[ii].r ; qfim[ii*nxy+1] = cx[ii].i ; }
            }
         }
      }
      break ;

      case MRI_complex:{
         register complex *qfim ;
         for( kk=0 ; kk < ny ; kk++ ){
            for( jj=0 ; jj < nx ; jj++ ){
               base = jj + kk*nx ;
               qfim = cfim + base ;
               for( ii=0 ; ii<nz ; ii++){ cx[ii] = qfim[ii*nxy] ; }
               for( ii=nz; ii<nup; ii++){ cx[ii].r = cx[ii].i = 0.0 ; }
               csfft_cox( -1 , nup , cx ) ;
               for( ii=0 ; ii<nup; ii++){ cx[ii].r *= gg[ii] ; cx[ii].i *= gg[ii] ; }
               csfft_cox(  1 , nup , cx ) ;
               for( ii=0 ; ii<nz ; ii++){ qfim[ii*nxy] = cx[ii] ; }
            }
         }
      }
      break ;
   }

   /*** 10 Jan 2003: clip data to bot and top found above ***/
   /***              to minimize Gibbs ringing artifacts  ***/

 ALL_DONE_NOW:

   if( !all_fir && (fir_num < 3 || as_float) ){
     STATUS("clipping results") ;
     switch( ftype ){

       case MRI_short:
         for( ii=0 ; ii < nxyz ; ii++ ) {
           if( as_float ) {  /* return floats to rounded shorts */
              if( ffim[ii] >= 0.0f ) sfim[ii] = (short)(ffim[ii]+0.5) ;
              else                   sfim[ii] = (short)(ffim[ii]-0.5) ;
           }
                if( sfim[ii] < fbot ) sfim[ii] = fbot ;
           else if( sfim[ii] > ftop ) sfim[ii] = ftop ;
         }
       break ;

       case MRI_float:
         for( ii=0 ; ii < nxyz ; ii++ )
                if( ffim[ii] < fbot ) ffim[ii] = fbot ;
           else if( ffim[ii] > ftop ) ffim[ii] = ftop ;
       break ;

       case MRI_byte:
         for( ii=0 ; ii < nxyz ; ii++ ) {
           if( as_float ) bfim[ii] = (byte)(ffim[ii]+0.5) ; /* return to byte */
                if( bfim[ii] < fbot ) bfim[ii] = fbot ;
           else if( bfim[ii] > ftop ) bfim[ii] = ftop ;
         }
       break ;
     }
   }

   /*** done! ***/

   if( cx != NULL ) free(cx) ;
   if( gg != NULL ) free(gg) ;
   if( as_float && ffim ) free(ffim) ;
   EXRETURN ;
}

#if 0
/*-------------------------------------------------------------------*/
/*! Partition of unity function:

      n=+infinity
    sum            partu(x-n) = 1 identically
      n=-infinity

    and support of partu(x) is interval [-1,1]
*//*-----------------------------------------------------------------*/

static INLINE float partu( float x )
{
  register float ax ;
  ax = fabsf(x) ;
  if( ax >= 1.0f ) return 0.0f ;
  return (x*x*(20.0f*ax+10.0f)+4.0f*ax+1.0f) ;
}
#endif

/*-------------------------------------------------------------------*/
/*! Function to blur a 3D volume in-place with a symmetric FIR filter
    along the x-direction.
      - m = stencil size (+/- m points in each direction; m >= 1)
      - wt = array of weights
      - nx,ny,nz = dimensions of 3D array
      - f = 3D array

    f_out[i,j,k] =  wt[0] * f_in[i,j,k]
                  + wt[1] *(f_in[i+1,j,k]+f_in[i-1,j,k))
                  + wt[2] *(f_in[i+2,j,k]+f_in[i-2,j,k))
                  + ...
                  + wt[m] *(f_in[i+m,j,k]+f_in[i-m,j,k))

  Similar routines for blurring along the y- and z-directions
  are fir_blury() and fir_blurz().

  -- RWCox - 03 Oct 2005 - trying for some speedup for Daniel Glen
---------------------------------------------------------------------*/

void fir_blurx( int m, float *wt,int nx, int ny, int nz, float *f )
{
   int ii,jj,kk,qq , nxy=nx*ny , off ;
   float *r , wt0=0.0,wt1=0.0,wt2=0.0,wt3=0.0,wt4=0.0,wt5=0.0,wt6=0.0,wt7=0.0 , sum , *ff ;

ENTRY("fir_blurx") ;
if(PRINT_TRACING){char str[256];sprintf(str,"m=%d",m);STATUS(str);}

   if( m < 1 || wt == NULL || nx < (m+1) || f == NULL ) EXRETURN ;
   if( ny <= 0 || nz <= 0 ) EXRETURN ;
   switch(m){  /** assign weights to variables not arrays **/
      case 7:
           wt7 = wt[7];   /* let cases fall through to next case to assign weights */
      case 6:
           wt6 = wt[6];
      case 5:
           wt5 = wt[5];
      case 4:
           wt4 = wt[4];
      case 3:
           wt3 = wt[3];
      case 2:
           wt2 = wt[2];
      case 1:
           wt1 = wt[1];
      case 0:
           wt0 = wt[0];
      default:
      break ;
   }

   /* 1 row workspace, with m-long buffers at each end
      (so that the i-th element of the row is in r[i+m]) */

   r = (float *)calloc(sizeof(float),(nx+2*m)) ;

   switch( m ){

     default:
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){
          off = jj*nx + kk*nxy ; ff = f+off ;     /* ff = ptr to this row */
          memcpy( r+m , ff , sizeof(float)*nx ) ; /* copy row into workspace */
          r[m-1] = r[m+1] ; r[nx+m] = r[nx+m-2] ; /* mirror at ends */

          for( ii=0 ; ii < nx ; ii++ ){   /* filter at ii-th location */
            sum = wt[0]*r[ii+m] ;
            for( qq=1 ; qq <= m ; qq++ )
              sum += wt[qq] * ( r[ii+m-qq] + r[ii+m+qq] ) ;
            ff[ii] = sum ;                /* save result back in input array */
          }
        }}
     break ;

     /** for the cases below, the innermost loop
         (qq, above) is completely unrolled for speedup **/

#undef  M
#define M 7
     case 7:
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){
          off = jj*nx + kk*nxy ; ff = f+off ;
          memcpy( r+m , ff , sizeof(float)*nx ) ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( ii=0 ; ii < nx ; ii++ )
            ff[ii] = wt7*(r[ii  ]+r[ii+14])
                    +wt6*(r[ii+1]+r[ii+13])
                    +wt5*(r[ii+2]+r[ii+12])
                    +wt4*(r[ii+3]+r[ii+11])
                    +wt3*(r[ii+4]+r[ii+10])
                    +wt2*(r[ii+5]+r[ii+ 9])
                    +wt1*(r[ii+6]+r[ii+ 8])+wt0*r[ii+7] ;
        }}
     break ;

#undef  M
#define M 6
     case 6:
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){
          off = jj*nx + kk*nxy ; ff = f+off ;
          memcpy( r+m , ff , sizeof(float)*nx ) ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( ii=0 ; ii < nx ; ii++ )
            ff[ii] = wt6*(r[ii  ]+r[ii+12])
                    +wt5*(r[ii+1]+r[ii+11])
                    +wt4*(r[ii+2]+r[ii+10])
                    +wt3*(r[ii+3]+r[ii+ 9])
                    +wt2*(r[ii+4]+r[ii+ 8])
                    +wt1*(r[ii+5]+r[ii+ 7])+wt0*r[ii+6] ;
        }}
     break ;

#undef  M
#define M 5
     case 5:
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){

          off = jj*nx + kk*nxy ; ff = f+off ;
          memcpy( r+m , ff , sizeof(float)*nx ) ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( ii=0 ; ii < nx ; ii++ )
            ff[ii] = wt5*(r[ii  ]+r[ii+10])
                    +wt4*(r[ii+1]+r[ii+ 9])
                    +wt3*(r[ii+2]+r[ii+ 8])
                    +wt2*(r[ii+3]+r[ii+ 7])
                    +wt1*(r[ii+4]+r[ii+ 6])+wt0*r[ii+5] ;
        }}
     break ;

#undef  M
#define M 4
     case 4:
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){

          off = jj*nx + kk*nxy ; ff = f+off ;
          memcpy( r+m , ff , sizeof(float)*nx ) ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( ii=0 ; ii < nx ; ii++ )
            ff[ii] = wt4*(r[ii  ]+r[ii+ 8])
                    +wt3*(r[ii+1]+r[ii+ 7])
                    +wt2*(r[ii+2]+r[ii+ 6])
                    +wt1*(r[ii+3]+r[ii+ 5])+wt0*r[ii+4] ;
        }}
     break ;

#undef  M
#define M 3
     case 3:
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){

          off = jj*nx + kk*nxy ; ff = f+off ;
          memcpy( r+m , ff , sizeof(float)*nx ) ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( ii=0 ; ii < nx ; ii++ )
            ff[ii] = wt3*(r[ii  ]+r[ii+ 6])
                    +wt2*(r[ii+1]+r[ii+ 5])
                    +wt1*(r[ii+2]+r[ii+ 4])+wt0*r[ii+3] ;
        }}
     break ;

#undef  M
#define M 2
     case 2:
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){

          off = jj*nx + kk*nxy ; ff = f+off ;
          memcpy( r+m , ff , sizeof(float)*nx ) ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( ii=0 ; ii < nx ; ii++ )
            ff[ii] = wt2*(r[ii  ]+r[ii+ 4])
                    +wt1*(r[ii+1]+r[ii+ 3])+wt0*r[ii+2] ;
        }}
     break ;

#undef  M
#define M 1
     case 1:
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){

          off = jj*nx + kk*nxy ; ff = f+off ;
          memcpy( r+m , ff , sizeof(float)*nx ) ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( ii=0 ; ii < nx ; ii++ )
            ff[ii] = wt1*(r[ii]+r[ii+2])+wt0*r[ii+1] ;
        }}
     break ;

   }  /* end of switch on m */

   free((void *)r) ; EXRETURN ;
}

/*-------------------------------------------------------------------*/
#undef  D
#define D nx  /* stride along y-axis */

/*! Similar to fir_blurx(), but along the y-axis.
    For further comments, see fir_blurx() source code. */

void fir_blury( int m, float *wt,int nx, int ny, int nz, float *f )
{
   int ii,jj,kk,qq , nxy=nx*ny , off ;
   float *r, wt0=0.0,wt1=0.0,wt2=0.0,wt3=0.0,wt4=0.0,wt5=0.0,wt6=0.0,wt7=0.0 , sum , *ff ;
   float *rr, *ss;
   int ny2m = ny+2*m;

ENTRY("fir_blury") ;
if(PRINT_TRACING){char str[256];sprintf(str,"m=%d",m);STATUS(str);}

   if( m < 1 || wt == NULL || ny < (m+1) || f == NULL ) EXRETURN ;
   if( nx <= 0 || nz <= 0 ) EXRETURN ;
   switch(m){  /**assign weights to variables not arrays **/
      case 7:
           wt7 = wt[7];   /* let cases fall through to next case to assign weights */
      case 6:
           wt6 = wt[6];
      case 5:
           wt5 = wt[5];
      case 4:
           wt4 = wt[4];
      case 3:
           wt3 = wt[3];
      case 2:
           wt2 = wt[2];
      case 1:
           wt1 = wt[1];
      case 0:
           wt0 = wt[0];
      default:
      break ;
   }

   if( nx < 512 ) goto SMALLIMAGE;

   /* In this function, for each value of kk (z index), we extract a
      2D (y,x) slice, with m-long buffers on each side in the y-direction.
      The purpose of this is to get multiple lines of y-direction data into
      the CPU cache, to speed up processing (a lot).  For the x-axis, this
      was unneeded, since the x-rows are contiguous in memory. For data at 256x256
      this 2D extract/process/insert trick was nugatory. However, for 512x512 data
      this trick becomes important.
      The same method is used for the z-axis in fir_blurz(). */

   /* macro to access the input data 2D slice: (i,j) = (x,y) indexes */

#undef  RR
#define RR(i,j) rr[(j)+m+(i)*ny2m]  /*** 0 <= i <= nx-1 ; -m <= m <= ny-1+m ***/

   /* macro to access the output data 2D slice */

#undef  SS
#define SS(i,k) ss[(k)+(i)*ny]

   rr = (float *)calloc(sizeof(float),ny2m*nx) ;  /* ny2m = ny+2*m */
   ss = (float *)malloc(sizeof(float)*ny  *nx) ;

   for( kk=0 ; kk < nz ; kk++ ){  /* loop in z-direction  (an xy slice at a time) */
     off = kk*nxy ; ff = f+off ;   /* ff = ptr to start of this 2D slice */

     /* load data into 2D (y+2m,x) slice from 3D (x,y,z) array;
        inner loop is over ii so as to access in the most contiguous way */

     for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++ ) RR(ii,jj) = ff[ii+D*jj] ;  /* D = nx here */
     }
     for( ii=0 ; ii < nx ; ii++ ){
       RR(ii,-1) = RR(ii,1) ; RR(ii,ny) = RR(ii,ny-2) ; /* edge reflection - */
                                                   /* only 1 point reflected*/
     }

     /* filter data in RR along y-direction, put into 2D SS array */

     switch(m){  /** for small m, unroll the inner loop for speed **/

       default:
         for( ii=0 ; ii < nx ; ii++ ){
           for( jj=0 ; jj < ny ; jj++ ){
             sum = wt[0]*RR(ii,jj) ;
             for( qq=1 ; qq <= m ; qq++ )
               sum += wt[qq] * ( RR(ii,jj+qq) + RR(ii,jj-qq) ) ;
             SS(ii,jj) = sum ;
         }}
       break ;

       case 7:
         for( ii=0 ; ii < nx ; ii++ ){
           for( jj=0 ; jj < ny ; jj++ ){
              SS(ii,jj) =  wt7 * ( RR(ii,jj+7) + RR(ii,jj-7) )
                         + wt6 * ( RR(ii,jj+6) + RR(ii,jj-6) )
                         + wt5 * ( RR(ii,jj+5) + RR(ii,jj-5) )
                         + wt4 * ( RR(ii,jj+4) + RR(ii,jj-4) )
                         + wt3 * ( RR(ii,jj+3) + RR(ii,jj-3) )
                         + wt2 * ( RR(ii,jj+2) + RR(ii,jj-2) )
                         + wt1 * ( RR(ii,jj+1) + RR(ii,jj-1) )
                         + wt0 *   RR(ii,jj) ;
         }}
       break ;

       case 6:
         for( ii=0 ; ii < nx ; ii++ ){
           for( jj=0 ; jj < ny ; jj++ ){
              SS(ii,jj) =  wt6 * ( RR(ii,jj+6) + RR(ii,jj-6) )
                         + wt5 * ( RR(ii,jj+5) + RR(ii,jj-5) )
                         + wt4 * ( RR(ii,jj+4) + RR(ii,jj-4) )
                         + wt3 * ( RR(ii,jj+3) + RR(ii,jj-3) )
                         + wt2 * ( RR(ii,jj+2) + RR(ii,jj-2) )
                         + wt1 * ( RR(ii,jj+1) + RR(ii,jj-1) )
                         + wt0 *   RR(ii,jj) ;
         }}
       break ;

       case 5:
         for( ii=0 ; ii < nx ; ii++ ){
           for( jj=0 ; jj < ny ; jj++ ){
              SS(ii,jj) =  wt5 * ( RR(ii,jj+5) + RR(ii,jj-5) )
                         + wt4 * ( RR(ii,jj+4) + RR(ii,jj-4) )
                         + wt3 * ( RR(ii,jj+3) + RR(ii,jj-3) )
                         + wt2 * ( RR(ii,jj+2) + RR(ii,jj-2) )
                         + wt1 * ( RR(ii,jj+1) + RR(ii,jj-1) )
                         + wt0 *   RR(ii,jj) ;
         }}
       break ;

       case 4:
         for( ii=0 ; ii < nx ; ii++ ){
           for( jj=0 ; jj < ny ; jj++ ){
              SS(ii,jj) =  wt4 * ( RR(ii,jj+4) + RR(ii,jj-4) )
                         + wt3 * ( RR(ii,jj+3) + RR(ii,jj-3) )
                         + wt2 * ( RR(ii,jj+2) + RR(ii,jj-2) )
                         + wt1 * ( RR(ii,jj+1) + RR(ii,jj-1) )
                         + wt0 *   RR(ii,jj) ;
         }}
       break ;

       case 3:
         for( ii=0 ; ii < nx ; ii++ ){
           for( jj=0 ; jj < ny ; jj++ ){
              SS(ii,jj) =  wt3 * ( RR(ii,jj+3) + RR(ii,jj-3) )
                         + wt2 * ( RR(ii,jj+2) + RR(ii,jj-2) )
                         + wt1 * ( RR(ii,jj+1) + RR(ii,jj-1) )
                         + wt0 *   RR(ii,jj) ;
         }}
       break ;

       case 2:
         for( ii=0 ; ii < nx ; ii++ ){
           for( jj=0 ; jj < ny ; jj++ ){
              SS(ii,jj) =  wt2 * ( RR(ii,jj+2) + RR(ii,jj-2) )
                         + wt1 * ( RR(ii,jj+1) + RR(ii,jj-1) )
                         + wt0 *   RR(ii,jj) ;
         }}
       break ;

       case 1:
         for( ii=0 ; ii < nx ; ii++ ){
           for( jj=0 ; jj < ny ; jj++ ){
              SS(ii,jj) =  wt1 * ( RR(ii,jj+1) + RR(ii,jj-1) )
                         + wt0 *   RR(ii,jj) ;
         }}
       break ;

     } /* end of special cases of m */

     /* put SS array back into input 3D array;
        again, inner loop over ii for most contiguous access to f[] array */

     for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++ ) ff[ii+D*jj] = SS(ii,jj) ;
     }

   } /* end of loop over y-direction (zz) */

   /*** finito, cara mia mine, oh, oh, oh, each time we part, my heart wants to die...***/

   free((void *)ss) ; free((void *)rr) ; EXRETURN ;

/* for small images (nx<512), use slice as is and don't use reslicing trick*/

SMALLIMAGE:

   r = (float *)calloc(sizeof(float),(ny+2*m)) ;

   switch( m ){

     default:
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){
          off = ii + kk*nxy ; ff = f+off ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+m] = ff[D*jj] ;
          r[m-1] = r[m+1] ; r[ny+m] = r[ny+m-2] ;

          for( jj=0 ; jj < ny ; jj++ ){
            sum = wt[0]*r[jj+m] ;
            for( qq=1 ; qq <= m ; qq++ )
              sum += wt[qq] * ( r[jj+m-qq] + r[jj+m+qq] ) ;
            ff[D*jj] = sum ;
          }
        }}
     break ;

#undef  M
#define M 7
     case 7:
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){
          off = ii + kk*nxy ; ff = f+off ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+M] = ff[D*jj] ;
          r[M-1] = r[M+1] ; r[ny+M] = r[ny+M-2] ;

          for( jj=0 ; jj < ny ; jj++ )
            ff[D*jj] = wt7*(r[jj  ]+r[jj+14])
                      +wt6*(r[jj+1]+r[jj+13])
                      +wt5*(r[jj+2]+r[jj+12])
                      +wt4*(r[jj+3]+r[jj+11])
                      +wt3*(r[jj+4]+r[jj+10])
                      +wt2*(r[jj+5]+r[jj+ 9])
                      +wt1*(r[jj+6]+r[jj+ 8])+wt0*r[jj+7] ;
        }}
     break ;

#undef  M
#define M 6
     case 6:
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){
          off = ii + kk*nxy ; ff = f+off ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+M] = ff[D*jj] ;
          r[M-1] = r[M+1] ; r[ny+M] = r[ny+M-2] ;

          for( jj=0 ; jj < ny ; jj++ )
            ff[D*jj] = wt6*(r[jj  ]+r[jj+12])
                      +wt5*(r[jj+1]+r[jj+11])
                      +wt4*(r[jj+2]+r[jj+10])
                      +wt3*(r[jj+3]+r[jj+ 9])
                      +wt2*(r[jj+4]+r[jj+ 8])
                      +wt1*(r[jj+5]+r[jj+ 7])+wt0*r[jj+6] ;
        }}
     break ;

#undef  M
#define M 5
     case 5:
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){
          off = ii + kk*nxy ; ff = f+off ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+M] = ff[D*jj] ;
          r[M-1] = r[M+1] ; r[ny+M] = r[ny+M-2] ;

          for( jj=0 ; jj < ny ; jj++ )
            ff[D*jj] = wt5*(r[jj  ]+r[jj+10])
                      +wt4*(r[jj+1]+r[jj+ 9])
                      +wt3*(r[jj+2]+r[jj+ 8])
                      +wt2*(r[jj+3]+r[jj+ 7])
                      +wt1*(r[jj+4]+r[jj+ 6])+wt0*r[jj+5] ;
        }}
     break ;

#undef  M
#define M 4
     case 4:
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){
          off = ii + kk*nxy ; ff = f+off ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+M] = ff[D*jj] ;
          r[M-1] = r[M+1] ; r[ny+M] = r[ny+M-2] ;

          for( jj=0 ; jj < ny ; jj++ )
            ff[D*jj] = wt4*(r[jj  ]+r[jj+ 8])
                      +wt3*(r[jj+1]+r[jj+ 7])
                      +wt2*(r[jj+2]+r[jj+ 6])
                      +wt1*(r[jj+3]+r[jj+ 5])+wt0*r[jj+4] ;
        }}
     break ;

#undef  M
#define M 3
     case 3:
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){
          off = ii + kk*nxy ; ff = f+off ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+M] = ff[D*jj] ;
          r[M-1] = r[M+1] ; r[ny+M] = r[ny+M-2] ;

          for( jj=0 ; jj < ny ; jj++ )
            ff[D*jj] = wt3*(r[jj  ]+r[jj+ 6])
                      +wt2*(r[jj+1]+r[jj+ 5])
                      +wt1*(r[jj+2]+r[jj+ 4])+wt0*r[jj+3] ;
        }}
     break ;

#undef  M
#define M 2
     case 2:
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){

          off = ii + kk*nxy ; ff = f+off ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+M] = ff[D*jj] ;
          r[M-1] = r[M+1] ; r[ny+M] = r[ny+M-2] ;

          for( jj=0 ; jj < ny ; jj++ )
            ff[D*jj] = wt2*(r[jj  ]+r[jj+ 4])
                      +wt1*(r[jj+1]+r[jj+ 3])+wt0*r[jj+2] ;
        }}
     break ;

#undef  M
#define M 1
     case 1:
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){
          off = ii + kk*nxy ; ff = f+off ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+M] = ff[D*jj] ;
          r[M-1] = r[M+1] ; r[ny+M] = r[ny+M-2] ;

          for( jj=0 ; jj < ny ; jj++ )
            ff[D*jj] = wt1*(r[jj]+r[jj+2])+wt0*r[jj+1] ;
        }}
     break ;

   }  /* end of switch on m */

   free((void *)r) ; EXRETURN ;
}

/*-------------------------------------------------------------------*/
#undef  D
#define D nxy  /* stride along z-axis */

/*! Similar to fir_blurx(), but along z-axis. */

void fir_blurz( int m, float *wt,int nx, int ny, int nz, float *f )
{
   int ii,jj,kk,qq , nxy=nx*ny , off ;
   float *rr,*ss , wt0=0.0,wt1=0.0,wt2=0.0,wt3=0.0,wt4=0.0,wt5=0.0,wt6=0.0,wt7=0.0 , sum , *ff ;
   int nz2m = nz+2*m ;

ENTRY("fir_blurz") ;
if(PRINT_TRACING){char str[256];sprintf(str,"m=%d",m);STATUS(str);}

   if( m < 1 || wt == NULL || nz < (m+1) || f == NULL ) EXRETURN ;
   if( nxy <= 0 ) EXRETURN ;

   /* In this function, for each value of jj (y index), we extract a
      2D (z,x) slice, with m-long buffers on each side in the z-direction.
      The purpose of this is to get multiple lines of z-direction data into
      the CPU cache, to speed up processing (a lot).  For the x-axis, this
      was unneeded, since the x-rows are contiguous in memory.  For the
      y-axis, this trick might help, but only if a single (x,y) plane
      doesn't fit into cache.  For nx=ny=256, 1 plane is 256 KB, so I
      decided that this 2D extract/process/insert trick was nugatory. */

   switch(m){  /**assign weights to variables not arrays **/
      case 7:
           wt7 = wt[7];   /* let cases fall through to next case to assign weights */
      case 6:
           wt6 = wt[6];
      case 5:
           wt5 = wt[5];
      case 4:
           wt4 = wt[4];
      case 3:
           wt3 = wt[3];
      case 2:
           wt2 = wt[2];
      case 1:
           wt1 = wt[1];
      case 0:
           wt0 = wt[0];
      default:
      break ;
   }

   /* macro to access the input data 2D slice: (i,k) = (x,z) indexes */

#undef  RR
#define RR(i,k) rr[(k)+m+(i)*nz2m]  /*** 0 <= i <= nx-1 ; -m <= k <= nz-1+m ***/

   /* macro to access the output data 2D slice */

#undef  SS
#define SS(i,k) ss[(k)+(i)*nz]

   rr = (float *)calloc(sizeof(float),nz2m*nx) ;  /* nz2m = nz+2*m */
   ss = (float *)malloc(sizeof(float)*nz  *nx) ;

   for( jj=0 ; jj < ny ; jj++ ){  /* loop in y-direction */
     off = jj*nx ; ff = f+off ;   /* ff = ptr to start of this 2D slice */

     /* load data into 2D (z,x) slice from 3D (x,y,z) array;
        inner loop is over ii so as to access in the most contiguous way */

     for( kk=0 ; kk < nz ; kk++ ){
       for( ii=0 ; ii < nx ; ii++ ) RR(ii,kk) = ff[ii+D*kk] ;
     }
     for( ii=0 ; ii < nx ; ii++ ){
       RR(ii,-1) = RR(ii,1) ; RR(ii,nz) = RR(ii,nz-2) ; /* edge reflection */
     }

     /* filter data in RR along z-direction, put into 2D SS array */

     switch(m){  /** for small m, unroll the inner loop for speed **/

       default:
         for( ii=0 ; ii < nx ; ii++ ){
           for( kk=0 ; kk < nz ; kk++ ){
             sum = wt[0]*RR(ii,kk) ;
             for( qq=1 ; qq <= m ; qq++ )
               sum += wt[qq] * ( RR(ii,kk+qq) + RR(ii,kk-qq) ) ;
             SS(ii,kk) = sum ;
         }}
       break ;

       case 7:
         for( ii=0 ; ii < nx ; ii++ ){
           for( kk=0 ; kk < nz ; kk++ ){
              SS(ii,kk) =  wt7 * ( RR(ii,kk+7) + RR(ii,kk-7) )
                         + wt6 * ( RR(ii,kk+6) + RR(ii,kk-6) )
                         + wt5 * ( RR(ii,kk+5) + RR(ii,kk-5) )
                         + wt4 * ( RR(ii,kk+4) + RR(ii,kk-4) )
                         + wt3 * ( RR(ii,kk+3) + RR(ii,kk-3) )
                         + wt2 * ( RR(ii,kk+2) + RR(ii,kk-2) )
                         + wt1 * ( RR(ii,kk+1) + RR(ii,kk-1) )
                         + wt0 *   RR(ii,kk) ;
         }}
       break ;

       case 6:
         for( ii=0 ; ii < nx ; ii++ ){
           for( kk=0 ; kk < nz ; kk++ ){
              SS(ii,kk) =  wt6 * ( RR(ii,kk+6) + RR(ii,kk-6) )
                         + wt5 * ( RR(ii,kk+5) + RR(ii,kk-5) )
                         + wt4 * ( RR(ii,kk+4) + RR(ii,kk-4) )
                         + wt3 * ( RR(ii,kk+3) + RR(ii,kk-3) )
                         + wt2 * ( RR(ii,kk+2) + RR(ii,kk-2) )
                         + wt1 * ( RR(ii,kk+1) + RR(ii,kk-1) )
                         + wt0 *   RR(ii,kk) ;
         }}
       break ;

       case 5:
         for( ii=0 ; ii < nx ; ii++ ){
           for( kk=0 ; kk < nz ; kk++ ){
              SS(ii,kk) =  wt5 * ( RR(ii,kk+5) + RR(ii,kk-5) )
                         + wt4 * ( RR(ii,kk+4) + RR(ii,kk-4) )
                         + wt3 * ( RR(ii,kk+3) + RR(ii,kk-3) )
                         + wt2 * ( RR(ii,kk+2) + RR(ii,kk-2) )
                         + wt1 * ( RR(ii,kk+1) + RR(ii,kk-1) )
                         + wt0 *   RR(ii,kk) ;
         }}
       break ;

       case 4:
         for( ii=0 ; ii < nx ; ii++ ){
           for( kk=0 ; kk < nz ; kk++ ){
              SS(ii,kk) =  wt4 * ( RR(ii,kk+4) + RR(ii,kk-4) )
                         + wt3 * ( RR(ii,kk+3) + RR(ii,kk-3) )
                         + wt2 * ( RR(ii,kk+2) + RR(ii,kk-2) )
                         + wt1 * ( RR(ii,kk+1) + RR(ii,kk-1) )
                         + wt0 *   RR(ii,kk) ;
         }}
       break ;

       case 3:
         for( ii=0 ; ii < nx ; ii++ ){
           for( kk=0 ; kk < nz ; kk++ ){
              SS(ii,kk) =  wt3 * ( RR(ii,kk+3) + RR(ii,kk-3) )
                         + wt2 * ( RR(ii,kk+2) + RR(ii,kk-2) )
                         + wt1 * ( RR(ii,kk+1) + RR(ii,kk-1) )
                         + wt0 *   RR(ii,kk) ;
         }}
       break ;

       case 2:
         for( ii=0 ; ii < nx ; ii++ ){
           for( kk=0 ; kk < nz ; kk++ ){
              SS(ii,kk) =  wt2 * ( RR(ii,kk+2) + RR(ii,kk-2) )
                         + wt1 * ( RR(ii,kk+1) + RR(ii,kk-1) )
                         + wt0 *   RR(ii,kk) ;
         }}
       break ;

       case 1:
         for( ii=0 ; ii < nx ; ii++ ){
           for( kk=0 ; kk < nz ; kk++ ){
              SS(ii,kk) =  wt1 * ( RR(ii,kk+1) + RR(ii,kk-1) )
                         + wt0 *   RR(ii,kk) ;
         }}
       break ;

     } /* end of special cases of m */

     /* put SS array back into input 3D array;
        again, inner loop over ii for most contiguous access to f[] array */

     for( kk=0 ; kk < nz ; kk++ ){
       for( ii=0 ; ii < nx ; ii++ ) ff[ii+D*kk] = SS(ii,kk) ;
     }

   } /* end of loop over y-direction (jj) */

   /*** finito, cara mia ***/

   free((void *)ss) ; free((void *)rr) ; EXRETURN ;
}

/*-------------------------------------------------------------------*/
/*! Like EDIT_blur_volume(), using FIR only (no FFTs)
    and only for float arrays.
---------------------------------------------------------------------*/

void FIR_blur_volume( int nx, int ny, int nz,
                      float dx, float dy, float dz,
                      float *ffim , float sigma )
{
  if( ffim != NULL && sigma > 0.0 )
    FIR_blur_volume_3d(nx,ny,nz, dx,dy,dz, ffim, sigma,sigma,sigma) ;
}

/*-------------------------------------------------------------------*/
/* Gaussian blur in real space, of a float volume; like
   EDIT_blur_volume_3d(), but using FIR only.
    - nx,ny,nz = dimensions of array
    - dx,dy,dz = grid step sizes
    - ffim     = array
    - sigmax   = stdev for blur along x; if 0, no blurring in x; etc.
---------------------------------------------------------------------*/

void FIR_blur_volume_3d( int nx, int ny, int nz,
                         float dx, float dy, float dz,
                         float *ffim ,
                         float sigmax, float sigmay, float sigmaz )
{
   int   fir_m , ii ;
   float *fir_wt=NULL , fac ;

   double sfac = AFNI_numenv("AFNI_BLUR_FIRFAC") ;
   if( sfac < 2.0 ) sfac = 2.5 ;

   ENTRY("FIR_blur_volume_3d") ;

   if( ffim == NULL ) EXRETURN ;
   if( sigmax <= 0.0 && sigmay <= 0.0 && sigmaz <= 0.0 ) EXRETURN ;

   if( dx <= 0.0 ) dx = 1.0 ;
   if( dy <= 0.0 ) dy = dx  ;
   if( dz <= 0.0 ) dz = dx  ;

   /*-- blur along x --*/

   if( sigmax > 0.0 && nx > 1 ){
     fir_m = (int) ceil( sfac * sigmax / dx ) ;  /* about the 5% level */
     if( fir_m < 1    ) fir_m = 1 ;
     if( fir_m > nx/2 ) fir_m = nx/2 ;
     fir_wt = (float *)malloc(sizeof(float)*(fir_m+1)) ;
     fir_gaussian_load( fir_m , dx/sigmax , fir_wt ) ;
     fir_blurx( fir_m , fir_wt , nx,ny,nz , ffim ) ;
   }

   /*-- blur along y --*/

   if( sigmay > 0.0 && ny > 1 ){
     fir_m = (int) ceil( sfac * sigmay / dy ) ;
     if( fir_m < 1    ) fir_m = 1 ;
     if( fir_m > ny/2 ) fir_m = ny/2 ;
     fir_wt = (float *)realloc(fir_wt,sizeof(float)*(fir_m+1)) ;
     fir_gaussian_load( fir_m , dy/sigmay , fir_wt ) ;
     fir_blury( fir_m , fir_wt , nx,ny,nz , ffim ) ;
   }

   /*-- blur along z --*/

   if( sigmaz > 0.0 && nz > 1 ){
     fir_m = (int) ceil( sfac * sigmaz / dz ) ;
     if( fir_m < 1    ) fir_m = 1 ;
     if( fir_m > nz/2 ) fir_m = nz/2 ;
     fir_wt = (float *)realloc(fir_wt,sizeof(float)*(fir_m+1)) ;
     fir_gaussian_load( fir_m , dz/sigmaz , fir_wt ) ;
     fir_blurz( fir_m , fir_wt , nx,ny,nz , ffim ) ;
   }

   if( fir_wt != NULL ) free(fir_wt) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/
/*! Blurring, applied to a 2D RGB image. */

MRI_IMAGE * mri_rgb_blur2D( float sig , MRI_IMAGE *im )
{
   MRI_IMARR *imar ;
   MRI_IMAGE *rim , *gim , *bim , *newim ;

ENTRY("mri_rgb_blur2D") ;

   if( im == NULL || im->kind != MRI_rgb || sig <= 0.0f ) RETURN(NULL) ;

   imar = mri_rgb_to_3float(im) ;
   rim = IMARR_SUBIM(imar,0) ;
   gim = IMARR_SUBIM(imar,1) ;
   bim = IMARR_SUBIM(imar,2) ;

   FIR_blur_volume_3d( rim->nx,rim->ny,1 , 1.0f,1.0f,1.0f ,
                       MRI_FLOAT_PTR(rim) , sig,sig,0.0f   ) ;
   FIR_blur_volume_3d( gim->nx,gim->ny,1 , 1.0f,1.0f,1.0f ,
                       MRI_FLOAT_PTR(gim) , sig,sig,0.0f   ) ;
   FIR_blur_volume_3d( bim->nx,bim->ny,1 , 1.0f,1.0f,1.0f ,
                       MRI_FLOAT_PTR(bim) , sig,sig,0.0f   ) ;

   newim = mri_3to_rgb(rim,gim,bim) ; MRI_COPY_AUX(newim,im) ;
   DESTROY_IMARR(imar) ; RETURN(newim) ;
}

/*-----------------------------------------------------------------------------*/
/*! Blurring, applied to a 2D byte image. */

MRI_IMAGE * mri_byte_blur2D( float sig , MRI_IMAGE *im )
{
   MRI_IMAGE *rim, *newim ;

ENTRY("mri_byte_blur2D") ;
   if( im == NULL || im->kind != MRI_byte || sig <= 0.0f ) RETURN(NULL) ;
   
   rim = mri_to_mri( MRI_float, im  ) ; 
   FIR_blur_volume_3d( rim->nx,rim->ny,1 , 1.0f,1.0f,1.0f ,
                       MRI_FLOAT_PTR(rim) , sig,sig,0.0f   ) ;
   
   newim = mri_to_mri( MRI_byte , rim );
   MRI_COPY_AUX(newim,im) ;
   mri_free(rim);

   RETURN(newim) ;
}

/*-----------------------------------------------------------------------------*/
/*! Blurring, applied to a 2D float image. */

MRI_IMAGE * mri_float_blur2D( float sig , MRI_IMAGE *im )
{
   MRI_IMAGE *newim ;

ENTRY("mri_float_blur2D") ;

   if( im == NULL || im->kind != MRI_float || sig <= 0.0f ) RETURN(NULL) ;
   newim = mri_to_float(im) ;
   FIR_blur_volume_3d( newim->nx,newim->ny,1 , 1.0f,1.0f,1.0f ,
                       MRI_FLOAT_PTR(newim)  , sig,sig,0.0f    ) ;
   RETURN(newim) ;
}

/*-----------------------------------------------------------------------------*/
/*! Blurring, applied to a 3D float image (sig is in voxels, not mm). */

MRI_IMAGE * mri_float_blur3D( float sig , MRI_IMAGE *im )
{
   MRI_IMAGE *newim ;

ENTRY("mri_float_blur3D") ;

   if( im == NULL || im->kind != MRI_float || sig <= 0.0f ) RETURN(NULL) ;
   newim = mri_to_float(im) ;
   FIR_blur_volume_3d( newim->nx,newim->ny,newim->nz , 1.0f,1.0f,1.0f ,
                       MRI_FLOAT_PTR(newim)  , sig,sig,sig             ) ;
   RETURN(newim) ;
}

/*-----------------------------------------------------------------------------*/

#undef  NG
#define NG 11
static void fir_gaussian_load( int m, float dx, float *ff )
{
   int ii ; float fac ; double xx ;

   if( m < 0 || dx <= 0.0f || ff == NULL ) return ;

   if(DO_INFO) INFO_message("fir_gaussian_load(%d,%g)",m,dx) ;

   if( AFNI_yesenv("AFNI_BLUR_FIROLD") ){   /** the olde waye **/
     fac = ff[0] = 1.0f ;
     for( ii=1 ; ii <= m ; ii++ ){
       xx = ii*dx ;
       ff[ii] = exp(-0.5*xx*xx ) ;    /* center of each cell */
       fac += 2.0f * ff[ii] ;
     }
   } else {                           /** the newe bettere waye **/
     double sum , dxj , ee ; int jj ;
     fac = 0.0f ; dxj = dx/(2.0*NG) ; /* subdivision of each cell */
     for( ii=0 ; ii <= m ; ii++ ){
       sum = 0.0 ;
       for( jj=-NG ; jj <= NG ; jj++ ){ /* sum over subdivisions */
         xx = ii*dx + jj*dxj ;
         ee = exp( -0.5*xx*xx ) ;
         if( jj==-NG || jj==NG ) sum += 0.5*ee ; else sum += ee ;
       }
       ff[ii] = sum ;
       if( ii==0 ) fac += sum ; else fac += 2.0*sum ;
     }
   }

   fac = 1.0f / fac ;
   for( ii=0 ; ii <= m ; ii++ ) ff[ii] *= fac ;
   return ;
}

/**************************************************************************/
/**************************************************************************/
/**************************************************************************/
#if 0
/*------------------------------------------------------------------------*/
/* Below is a test program that can be used to evaluate the relative
   speed of FFT and FIR blurring.  If compiled into 'tblur', then a
   command line would be
     'tblur 240 13.0'
   meaning blur a 240x240x240 volume with sigma=13.0.  The output is
   a line indicating the input parameters and the CPU time ratio
   (FFT time)/(FIR time).

   Compilation should be something like so:

   make tblur.o
   cc -o tblur tblur.o -L. -L/usr/X11R6/lib -lmri -lXt -lm
--------------------------------------------------------------------------*/
#include "mrilib.h"

int main( int argc , char *argv[] )
{
   int nx , nxxx , ii , jj ;
   float sigma , *gar , *far ;
   double c1,c2,c3 , cg,cf ;

   EDIT_blur_allow_fir(0) ;  /* turn off auto-FIR in EDIT_blur_volume() */

   /* get command line params */

   if( argc < 3 ) exit(0) ;
   nx = (int)strtod( argv[1] , NULL) ; if( nx < 32 ) nx = 128 ;
   sigma = (float)strtod( argv[2] , NULL ) ; if( sigma <= 0.0 ) exit(0) ;

   /* allocate volumes */

   nxxx = nx*nx*nx ;
   gar = (float *)malloc( sizeof(float)*nxxx ) ;
   far = (float *)malloc( sizeof(float)*nxxx ) ;

   /* load volume for FFT and then blur 5 times */

   for( ii=0 ; ii < nxxx ; ii++ ) gar[ii] = cos(0.37382*ii) ;
   c1 = COX_cpu_time() ;
   for( jj=0 ; jj < 5 ; jj++ )
     EDIT_blur_volume( nx,nx,nx , 1.0,1.0,1.0 , MRI_float,gar , sigma ) ;
   c2 = COX_cpu_time() ; cg = c2-c1 ;
   /** printf("nx=%d sigma=%.2f Gaussian cpu=%.3f\n",nx,sigma,cg) ; **/

   /* load volume for FIR and then blur 5 times */

   for( ii=0 ; ii < nxxx ; ii++ ) far[ii] = cos(0.37382*ii) ;
   c1 = COX_cpu_time() ;
   for( jj=0 ; jj < 5 ; jj++ )
     FIR_blur_volume( nx,nx,nx , 1.0,1.0,1.0 , far , sigma ) ;
   c2 = COX_cpu_time() ; cf = c2-c1 ;
   /** printf("nx=%d sigma=%.2f FIR_blur cpu=%.3f\n",nx,sigma,cf) ; **/

   /* output CPU time ratio */

   printf("ratio: %d %.2f %.2f\n",nx,sigma,cg/cf) ;

   /* compute mean difference between the 2 approaches */

   /**
   c1 = 0.0 ;
   for( ii=0 ; ii < nxxx ; ii++ ) c1 += fabs(far[ii]-gar[ii]) ;
   c1 = c1 / nxxx ;
   printf("mean abs diff = %g\n",c1) ;
   **/

   exit(0) ;
}
#endif
