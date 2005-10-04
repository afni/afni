/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/**************************************************************************/
/** prototypes for functions at the end of this file **/

static void fir_blurx( int m, float *wt,int nx, int ny, int nz, float *f ) ;
static void fir_blury( int m, float *wt,int nx, int ny, int nz, float *f ) ;
static void fir_blurz( int m, float *wt,int nx, int ny, int nz, float *f ) ;

#undef  FIR_MAX
#define FIR_MAX 11  /* max length of FIR filter to use */

/*************************************************************************
  Routine to blur a 3D volume with a Gaussian, using FFTs.
**************************************************************************/

#define GET_AS_BIG(name,type,dim)                                       \
   do{ if( (dim) > name ## _size ){                                     \
          if( name != NULL ) free(name) ;                               \
          name = (type *) malloc( sizeof(type) * (dim) ) ;              \
          if( name == NULL ){                                           \
             fprintf(stderr,"\n*** cannot malloc EDIT workspace!\n") ;  \
             EXIT(1) ; }                                                \
          name ## _size = (dim) ; }                                     \
       break ; } while(1)


void EDIT_blur_volume( int nx, int ny, int nz,
                       float dx, float dy, float dz,
                       int ftype , void * vfim , float sigma )
{
  EDIT_blur_volume_3d (nx, ny, nz, dx, dy, dz, ftype, vfim,
                      sigma, sigma, sigma);
}

/**************************************************************************/
/*
  The following slightly modified version of EDIT_blur_volume allows
  independent specification of Gaussian filter widths along the three
  perpendicular axes.
  BDW  21 Feb 1997
*/

void EDIT_blur_volume_3d( int nx, int ny, int nz,
                          float dx, float dy, float dz,
                          int ftype , void * vfim ,
                          float sigmax, float sigmay, float sigmaz )
{
   int jj,kk , nxy , base,nby2 ;
   float  dk , aa , k , fac ;
   register int ii , nup ;

   static int cx_size  = 0 ;     /* workspaces */
   static int gg_size  = 0 ;
   static complex *cx = NULL ;
   static float   *gg = NULL ;

   byte     *bfim = NULL ;
   short    *sfim = NULL ;
   float    *ffim = NULL ;
   complex  *cfim = NULL ;

   float fbot,ftop ;     /* 10 Jan 2003 */
   int nxyz ;

   int   fir_m , fir_num=0 ;  /* 03 Oct 2005 */
   float fir_wt[FIR_MAX+1] ;

   /*** initialize ***/

ENTRY("EDIT_blur_volume_3d") ;

   if( vfim == NULL ) EXRETURN ;

   if( sigmax <= 0.0 && sigmay <= 0.0 && sigmaz <= 0.0 ) EXRETURN ;

   if( dx <= 0.0 ) dx = 1.0 ;  /* 03 Oct 2005 */
   if( dy <= 0.0 ) dy = dx  ;
   if( dz <= 0.0 ) dz = dx  ;

   switch( ftype ){
      default: EXRETURN ;
      case MRI_short:   sfim = (short *)   vfim ; break ;
      case MRI_float:   ffim = (float *)   vfim ; break ;
      case MRI_byte:    bfim = (byte *)    vfim ; break ;
      case MRI_complex: cfim = (complex *) vfim ; break ;
   }
   nxy = nx * ny ; nxyz = nxy * nz ;

   /*** 10 Jan 2003: find bot and top of data input */

   switch( ftype ){
     default:
       fbot = ftop = 0.0 ;
     break ;

     case MRI_short:
       fbot = ftop = sfim[0] ;
       for( ii=1 ; ii < nxyz ; ii++ )
              if( sfim[ii] < fbot ) fbot = sfim[ii] ;
         else if( sfim[ii] > ftop ) ftop = sfim[ii] ;
     break ;

     case MRI_float:
       fbot = ftop = ffim[0] ;
       for( ii=1 ; ii < nxyz ; ii++ )
              if( ffim[ii] < fbot ) fbot = ffim[ii] ;
         else if( ffim[ii] > ftop ) ftop = ffim[ii] ;
     break ;

     case MRI_byte:
       fbot = ftop = bfim[0] ;
       for( ii=1 ; ii < nxyz ; ii++ )
              if( bfim[ii] < fbot ) fbot = bfim[ii] ;
         else if( bfim[ii] > ftop ) ftop = bfim[ii] ;
     break ;
   }

   /*** do x-direction ***/

STATUS("start x FFTs") ;

   /** 03 Oct 2005: perhaps do the x-blur in real-space? **/

   if( nx < 2 || sigmax <= 0.0 ){ fir_num++ ;  goto DO_Y_BLUR ; }

   fir_m = (int) ceil( 2.5 * sigmax / dx ) ;
   if( ftype == MRI_float && fir_m <= FIR_MAX ){
     if( fir_m < 1 ) fir_m = 1 ;
     fac = fir_wt[0] = 1.0f ;
     for( ii=1 ; ii <= fir_m ; ii++ ){
       fir_wt[ii] = exp(-0.5*(ii*dx)*(ii*dx)/(sigmax*sigmax)) ;
       fac += 2.0f * fir_wt[ii] ;
     }
     fac = 1.0f / fac ;
     for( ii=0 ; ii <= fir_m ; ii++ ) fir_wt[ii] *= fac ;
     fir_blurx( fir_m , fir_wt , nx,ny,nz , ffim ) ;
     fir_num++ ; goto DO_Y_BLUR ;
   }

   aa  = sigmax * sigmax * 0.5 ;
   nup = nx + (int)(3.0 * sigmax / dx) ;      /* min FFT length */
   nup = csfft_nextup_one35(nup) ; nby2 = nup / 2 ;

   GET_AS_BIG(cx,complex,nup) ; GET_AS_BIG(gg,float,nup) ;

   dk    = (2.0*PI) / (nup * dx) ;
   fac   = 1.0 / nup ;
   gg[0] = fac ;
   for( ii=1 ; ii<=nby2 ; ii++ ){ k=ii*dk; gg[nup-ii]=gg[ii]=fac*exp(-aa*k*k); }

   /** July 20: double up on FFTs **/
   /** Feb  09: extend to other data types besides shorts;
                doubling up does not apply to complex data! **/

   switch( ftype ){
      case MRI_short:{
         register short * qfim ;
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
         register float * qfim ;
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
         register byte * qfim ;
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
         register complex * qfim ;
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

STATUS("start y FFTs") ;

   /** 03 Oct 2005: perhaps do the y-blur in real-space? **/

   if( ny < 2 || sigmay <= 0.0 ){ fir_num++ ; goto DO_Z_BLUR ; }

   fir_m = (int) ceil( 2.5 * sigmay / dy ) ;
   if( ftype == MRI_float && fir_m <= FIR_MAX ){
     if( fir_m < 1 ) fir_m = 1 ;
     fac = fir_wt[0] = 1.0f ;
     for( ii=1 ; ii <= fir_m ; ii++ ){
       fir_wt[ii] = exp(-0.5*(ii*dy)*(ii*dy)/(sigmay*sigmay)) ;
       fac += 2.0f * fir_wt[ii] ;
     }
     fac = 1.0f / fac ;
     for( ii=0 ; ii <= fir_m ; ii++ ) fir_wt[ii] *= fac ;
     fir_blury( fir_m , fir_wt , nx,ny,nz , ffim ) ;
     fir_num++ ; goto DO_Z_BLUR ;
   }

   aa  = sigmay * sigmay * 0.5 ;
   nup = ny + (int)(3.0 * sigmay / dy) ;      /* min FFT length */
   nup = csfft_nextup_one35(nup) ; nby2 = nup / 2 ;

   GET_AS_BIG(cx,complex,nup) ; GET_AS_BIG(gg,float,nup) ;

   dk    = (2.0*PI) / (nup * dy) ;
   fac   = 1.0 / nup ;
   gg[0] = fac ;
   for( ii=1 ; ii<=nby2 ; ii++ ){ k=ii*dk; gg[nup-ii]=gg[ii]=fac*exp(-aa*k*k); }

   switch( ftype ){
      case MRI_short:{
         register short * qfim ;
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
         register byte * qfim ;
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
         register float * qfim ;
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
         register complex * qfim ;
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

STATUS("start z FFTs") ;

   /** 03 Oct 2005: perhaps do the y-blur in real-space? **/

   if( nz < 2 || sigmay <= 0.0 ){ fir_num++ ; goto ALL_DONE_NOW ; }

   fir_m = (int) ceil( 2.5 * sigmaz / dz ) ;
   if( ftype == MRI_float && fir_m <= FIR_MAX ){
     if( fir_m < 1 ) fir_m = 1 ;
     fac = fir_wt[0] = 1.0f ;
     for( ii=1 ; ii <= fir_m ; ii++ ){
       fir_wt[ii] = exp(-0.5*(ii*dz)*(ii*dz)/(sigmaz*sigmaz)) ;
       fac += 2.0f * fir_wt[ii] ;
     }
     fac = 1.0f / fac ;
     for( ii=0 ; ii <= fir_m ; ii++ ) fir_wt[ii] *= fac ;
     fir_blurz( fir_m , fir_wt , nx,ny,nz , ffim ) ;
     fir_num++ ; goto ALL_DONE_NOW ;
   }

   aa  = sigmaz * sigmaz * 0.5 ;
   nup = nz + (int)(3.0 * sigmaz / dz) ;      /* min FFT length */
   nup = csfft_nextup_one35(nup) ; nby2 = nup / 2 ;

   GET_AS_BIG(cx,complex,nup) ; GET_AS_BIG(gg,float,nup) ;

   dk    = (2.0*PI) / (nup * dz) ;
   fac   = 1.0 / nup ;
   gg[0] = fac ;
   for( ii=1 ; ii<=nby2 ; ii++ ){ k=ii*dk; gg[nup-ii]=gg[ii]=fac*exp(-aa*k*k); }

   switch( ftype ){
      case MRI_short:{
         register short * qfim ;
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
         register float * qfim ;
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
         register byte * qfim ;
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
         register complex * qfim ;
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

   if( fir_num < 3 ){
     switch( ftype ){

       case MRI_short:
         for( ii=0 ; ii < nxyz ; ii++ )
                if( sfim[ii] < fbot ) sfim[ii] = fbot ;
           else if( sfim[ii] > ftop ) sfim[ii] = ftop ;
       break ;

       case MRI_float:
         for( ii=0 ; ii < nxyz ; ii++ )
                if( ffim[ii] < fbot ) ffim[ii] = fbot ;
           else if( ffim[ii] > ftop ) ffim[ii] = ftop ;
       break ;

       case MRI_byte:
         for( ii=0 ; ii < nxyz ; ii++ )
                if( bfim[ii] < fbot ) bfim[ii] = fbot ;
           else if( bfim[ii] > ftop ) bfim[ii] = ftop ;
       break ;
     }
   }

   /*** done! ***/

   EXRETURN ;
}

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

static void fir_blurx( int m, float *wt,int nx, int ny, int nz, float *f )
{
   int ii,jj,kk,qq , nxy=nx*ny , off ;
   float *r , wt0,wt1,wt2,wt3,wt4,wt5,wt6,wt7 , sum ;

   if( m < 1 || wt == NULL || nx < (m+1) || f == NULL ) return ;

   r = (float *)calloc(sizeof(float),(nx+2*m)) ;

   switch( m ){

     default:
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){

          off = jj*nx + kk*nxy ;
          memcpy( r+m , f+off , sizeof(float)*nx ) ;
          r[m-1] = r[m+1] ; r[nx+m] = r[nx+m-2] ;

          for( ii=0 ; ii < nx ; ii++ ){
            sum = wt[0]*r[ii+m] ;
            for( qq=1 ; qq <= m ; qq++ )
              sum += wt[qq] * ( r[ii+m-qq] + r[ii+m+qq] ) ;
            f[ii+off] = sum ;
          }
        }}
     break ;

#undef  M
#define M 7
     case 7:
       wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
       wt4 = wt[4] ; wt5 = wt[5] ; wt6 = wt[6] ; wt7 = wt[7] ;
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){

          off = jj*nx + kk*nxy ;
          memcpy( r+m , f+off , sizeof(float)*nx ) ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( ii=0 ; ii < nx ; ii++ )
            f[ii+off] = wt7*(r[ii  ]+r[ii+14])
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
       wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
       wt4 = wt[4] ; wt5 = wt[5] ; wt6 = wt[6] ;
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){

          off = jj*nx + kk*nxy ;
          memcpy( r+m , f+off , sizeof(float)*nx ) ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( ii=0 ; ii < nx ; ii++ )
            f[ii+off] = wt6*(r[ii  ]+r[ii+12])
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
       wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
       wt4 = wt[4] ; wt5 = wt[5] ;
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){

          off = jj*nx + kk*nxy ;
          memcpy( r+m , f+off , sizeof(float)*nx ) ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( ii=0 ; ii < nx ; ii++ )
            f[ii+off] = wt5*(r[ii  ]+r[ii+10])
                         +wt4*(r[ii+1]+r[ii+ 9])
                         +wt3*(r[ii+2]+r[ii+ 8])
                         +wt2*(r[ii+3]+r[ii+ 7])
                         +wt1*(r[ii+4]+r[ii+ 6])+wt0*r[ii+5] ;
        }}
     break ;

#undef  M
#define M 4
     case 4:
       wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
       wt4 = wt[4] ;
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){

          off = jj*nx + kk*nxy ;
          memcpy( r+m , f+off , sizeof(float)*nx ) ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( ii=0 ; ii < nx ; ii++ )
            f[ii+off] = wt4*(r[ii  ]+r[ii+ 8])
                         +wt3*(r[ii+1]+r[ii+ 7])
                         +wt2*(r[ii+2]+r[ii+ 6])
                         +wt1*(r[ii+3]+r[ii+ 5])+wt0*r[ii+4] ;
        }}
     break ;

#undef  M
#define M 3
     case 3:
       wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){

          off = jj*nx + kk*nxy ;
          memcpy( r+m , f+off , sizeof(float)*nx ) ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( ii=0 ; ii < nx ; ii++ )
            f[ii+off] = wt3*(r[ii  ]+r[ii+ 6])
                         +wt2*(r[ii+1]+r[ii+ 5])
                         +wt1*(r[ii+2]+r[ii+ 4])+wt0*r[ii+3] ;
        }}
     break ;

#undef  M
#define M 2
     case 2:
       wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ;
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){

          off = jj*nx + kk*nxy ;
          memcpy( r+m , f+off , sizeof(float)*nx ) ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( ii=0 ; ii < nx ; ii++ )
            f[ii+off] = wt2*(r[ii  ]+r[ii+ 4])
                         +wt1*(r[ii+1]+r[ii+ 3])+wt0*r[ii+2] ;
        }}
     break ;

#undef  M
#define M 1
     case 1:
       wt0 = wt[0] ; wt1 = wt[1] ;
       for( kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){

          off = jj*nx + kk*nxy ;
          memcpy( r+m , f+off , sizeof(float)*nx ) ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( ii=0 ; ii < nx ; ii++ )
            f[ii+off] = wt1*(r[ii]+r[ii+2])+wt0*r[ii+1] ;
        }}
     break ;

   }  /* end of switch on m */

   free((void *)r) ; return ;
}

/*-------------------------------------------------------------------*/
#undef  D
#define D nx

static void fir_blury( int m, float *wt,int nx, int ny, int nz, float *f )
{
   int ii,jj,kk,qq , nxy=nx*ny , off ;
   float *r , wt0,wt1,wt2,wt3,wt4,wt5,wt6,wt7 , sum ;

   if( m < 1 || wt == NULL || ny < (m+1) || f == NULL ) return ;

   r = (float *)calloc(sizeof(float),(ny+2*m)) ;

   switch( m ){

     default:
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){

          off = ii + kk*nxy ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+m] = f[D*jj+off] ;
          r[m-1] = r[m+1] ; r[nx+m] = r[nx+m-2] ;

          for( jj=0 ; jj < ny ; jj++ ){
            sum = wt[0]*r[jj+m] ;
            for( qq=1 ; qq <= m ; qq++ )
              sum += wt[qq] * ( r[jj+m-qq] + r[jj+m+qq] ) ;
            f[D*jj+off] = sum ;
          }
        }}
     break ;

#undef  M
#define M 7
     case 7:
       wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
       wt4 = wt[4] ; wt5 = wt[5] ; wt6 = wt[6] ; wt7 = wt[7] ;
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){

          off = ii + kk*nxy ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+m] = f[D*jj+off] ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( jj=0 ; jj < ny ; jj++ )
            f[D*jj+off] = wt7*(r[jj  ]+r[jj+14])
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
       wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
       wt4 = wt[4] ; wt5 = wt[5] ; wt6 = wt[6] ;
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){

          off = ii + kk*nxy ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+m] = f[D*jj+off] ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( jj=0 ; jj < ny ; jj++ )
            f[D*jj+off] = wt6*(r[jj  ]+r[jj+12])
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
       wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
       wt4 = wt[4] ; wt5 = wt[5] ;
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){

          off = ii + kk*nxy ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+m] = f[D*jj+off] ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( jj=0 ; jj < ny ; jj++ )
            f[D*jj+off] = wt5*(r[jj  ]+r[jj+10])
                         +wt4*(r[jj+1]+r[jj+ 9])
                         +wt3*(r[jj+2]+r[jj+ 8])
                         +wt2*(r[jj+3]+r[jj+ 7])
                         +wt1*(r[jj+4]+r[jj+ 6])+wt0*r[jj+5] ;
        }}
     break ;

#undef  M
#define M 4
     case 4:
       wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
       wt4 = wt[4] ;
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){

          off = ii + kk*nxy ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+m] = f[D*jj+off] ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( jj=0 ; jj < ny ; jj++ )
            f[D*jj+off] = wt4*(r[jj  ]+r[jj+ 8])
                         +wt3*(r[jj+1]+r[jj+ 7])
                         +wt2*(r[jj+2]+r[jj+ 6])
                         +wt1*(r[jj+3]+r[jj+ 5])+wt0*r[jj+4] ;
        }}
     break ;

#undef  M
#define M 3
     case 3:
       wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){

          off = ii + kk*nxy ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+m] = f[D*jj+off] ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( jj=0 ; jj < ny ; jj++ )
            f[D*jj+off] = wt3*(r[jj  ]+r[jj+ 6])
                         +wt2*(r[jj+1]+r[jj+ 5])
                         +wt1*(r[jj+2]+r[jj+ 4])+wt0*r[jj+3] ;
        }}
     break ;

#undef  M
#define M 2
     case 2:
       wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ;
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){

          off = ii + kk*nxy ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+m] = f[D*jj+off] ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( jj=0 ; jj < ny ; jj++ )
            f[D*jj+off] = wt2*(r[jj  ]+r[jj+ 4])
                         +wt1*(r[jj+1]+r[jj+ 3])+wt0*r[jj+2] ;
        }}
     break ;

#undef  M
#define M 1
     case 1:
       wt0 = wt[0] ; wt1 = wt[1] ;
       for( kk=0 ; kk < nz ; kk++ ){
        for( ii=0 ; ii < nx ; ii++ ){

          off = ii + kk*nxy ;
          for( jj=0 ; jj < ny ; jj++ ) r[jj+m] = f[D*jj+off] ;
          r[M-1] = r[M+1] ; r[nx+M] = r[nx+M-2] ;

          for( jj=0 ; jj < ny ; jj++ )
            f[D*jj+off] = wt1*(r[jj]+r[jj+2])+wt0*r[jj+1] ;
        }}
     break ;

   }  /* end of switch on m */

   free((void *)r) ; return ;
}

/*-------------------------------------------------------------------*/
#undef  D
#define D nxy

static void fir_blurz( int m, float *wt,int nx, int ny, int nz, float *f )
{
   int ii,jj,kk,qq , nxy=nx*ny , off ;
   float *rr,*ss , wt0,wt1,wt2,wt3,wt4,wt5,wt6,wt7 , sum ;
   int nz2m = nz+2*m ;

   if( m < 1 || wt == NULL || nz < (m+1) || f == NULL ) return ;

   /* 2D (z,x) slice, with m-long buffers on each side in the z-direction.
      The purpose of this is to get multiple lines of z-direction data into
      the CPU cache, to speed up processing (a lot).                       */

#undef  RR
#define RR(i,k) rr[(k)+m+(i)*nz2m]  /*** 0 <= i <= nx-1 ; -m <= k <= nz-1+m ***/

#undef  SS
#define SS(i,k) ss[(k)+(i)*nz]

   rr = (float *)calloc(sizeof(float),nz2m*nx) ;
   ss = (float *)malloc(sizeof(float)*nz  *nx) ;

   for( jj=0 ; jj < ny ; jj++ ){  /* loop in y-direction */
     off = jj*nx ;                /* 3D (i,j,k) is at i+j*nx+k*nxy */

     /* load data into 2D (z,x) slice from 3D (x,y,z) array */

     for( kk=0 ; kk < nz ; kk++ ){
       for( ii=0 ; ii < nx ; ii++ ) RR(ii,kk) = f[ii+off+D*kk] ;
     }
     for( ii=0 ; ii < nx ; ii++ ){
       RR(ii,-1) = RR(ii,1) ; RR(ii,nz) = RR(ii,nz-2) ;
     }

     /* filter data along z-direction, put into 2D ss array */

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
         wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
         wt4 = wt[4] ; wt5 = wt[5] ; wt6 = wt[6] ; wt7 = wt[7] ;
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
         wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
         wt4 = wt[4] ; wt5 = wt[5] ; wt6 = wt[6] ;
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
         wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
         wt4 = wt[4] ; wt5 = wt[5] ;
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
         wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
         wt4 = wt[4] ;
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
         wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ; wt3 = wt[3] ;
         for( ii=0 ; ii < nx ; ii++ ){
           for( kk=0 ; kk < nz ; kk++ ){
              SS(ii,kk) =  wt3 * ( RR(ii,kk+3) + RR(ii,kk-3) )
                         + wt2 * ( RR(ii,kk+2) + RR(ii,kk-2) )
                         + wt1 * ( RR(ii,kk+1) + RR(ii,kk-1) )
                         + wt0 *   RR(ii,kk) ;
         }}
       break ;

       case 2:
         wt0 = wt[0] ; wt1 = wt[1] ; wt2 = wt[2] ;
         for( ii=0 ; ii < nx ; ii++ ){
           for( kk=0 ; kk < nz ; kk++ ){
              SS(ii,kk) =  wt2 * ( RR(ii,kk+2) + RR(ii,kk-2) )
                         + wt1 * ( RR(ii,kk+1) + RR(ii,kk-1) )
                         + wt0 *   RR(ii,kk) ;
         }}
       break ;

       case 1:
         wt0 = wt[0] ; wt1 = wt[1] ;
         for( ii=0 ; ii < nx ; ii++ ){
           for( kk=0 ; kk < nz ; kk++ ){
              SS(ii,kk) =  wt1 * ( RR(ii,kk+1) + RR(ii,kk-1) )
                         + wt0 *   RR(ii,kk) ;
         }}
       break ;

     } /* end of special cases of m */

     /* put ss array back into 3D array */

     for( kk=0 ; kk < nz ; kk++ ){
       for( ii=0 ; ii < nx ; ii++ ) f[ii+off+D*kk] = SS(ii,kk) ;
     }

   } /* end of loop over y-direction (jj) */

   /*** finito ***/

   free((void *)ss) ; free((void *)rr) ; return ;
}

/*-------------------------------------------------------------------*/

void FIR_blur_volume( int nx, int ny, int nz,
                      float dx, float dy, float dz,
                      float *ffim , float sigma )
{
  if( sigma > 0.0 )
    FIR_blur_volume_3d(nx,ny,nz, dx,dy,dz, ffim, sigma,sigma,sigma) ;
}

/*-------------------------------------------------------------------*/

void FIR_blur_volume_3d( int nx, int ny, int nz,
                         float dx, float dy, float dz,
                         float *ffim ,
                         float sigmax, float sigmay, float sigmaz )
{
   int   fir_m , ii ;
   float *fir_wt , fac ;

   ENTRY("FIR_blur_volume_3d") ;

   if( ffim == NULL ) EXRETURN ;
   if( sigmax <= 0.0 && sigmay <= 0.0 && sigmaz <= 0.0 ) EXRETURN ;

   if( dx <= 0.0 ) dx = 1.0 ;
   if( dy <= 0.0 ) dy = dx  ;
   if( dz <= 0.0 ) dz = dx  ;

   if( sigmax > 0.0 ){
     fir_m = (int) ceil( 2.5 * sigmax / dx ) ;
     if( fir_m < 1 ) fir_m = 1 ;
     fir_wt = (float *)malloc(sizeof(float)*(fir_m+1)) ;
     fac = fir_wt[0] = 1.0f ;
     for( ii=1 ; ii <= fir_m ; ii++ ){
       fir_wt[ii] = exp(-0.5*(ii*dx)*(ii*dx)/(sigmax*sigmax)) ;
       fac += 2.0f * fir_wt[ii] ;
     }
     fac = 1.0f / fac ;
     for( ii=0 ; ii <= fir_m ; ii++ ) fir_wt[ii] *= fac ;
     fir_blurx( fir_m , fir_wt , nx,ny,nz , ffim ) ;
     free((void *)fir_wt) ;
   }

   if( sigmay > 0.0 ){
     fir_m = (int) ceil( 2.5 * sigmay / dy ) ;
     if( fir_m < 1 ) fir_m = 1 ;
     fir_wt = (float *)malloc(sizeof(float)*(fir_m+1)) ;
     fac = fir_wt[0] = 1.0f ;
     for( ii=1 ; ii <= fir_m ; ii++ ){
       fir_wt[ii] = exp(-0.5*(ii*dy)*(ii*dy)/(sigmay*sigmay)) ;
       fac += 2.0f * fir_wt[ii] ;
     }
     fac = 1.0f / fac ;
     for( ii=0 ; ii <= fir_m ; ii++ ) fir_wt[ii] *= fac ;
     fir_blury( fir_m , fir_wt , nx,ny,nz , ffim ) ;
     free((void *)fir_wt) ;
   }

   if( sigmaz > 0.0 ){
     fir_m = (int) ceil( 2.5 * sigmaz / dz ) ;
     if( fir_m < 1 ) fir_m = 1 ;
     fir_wt = (float *)malloc(sizeof(float)*(fir_m+1)) ;
     fac = fir_wt[0] = 1.0f ;
     for( ii=1 ; ii <= fir_m ; ii++ ){
       fir_wt[ii] = exp(-0.5*(ii*dz)*(ii*dz)/(sigmaz*sigmaz)) ;
       fac += 2.0f * fir_wt[ii] ;
     }
     fac = 1.0f / fac ;
     for( ii=0 ; ii <= fir_m ; ii++ ) fir_wt[ii] *= fac ;
     fir_blurz( fir_m , fir_wt , nx,ny,nz , ffim ) ;
     free((void *)fir_wt) ;
   }

   return ;
}
