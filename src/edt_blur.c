/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

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
   static complex * cx = NULL ;
   static float   * gg = NULL ;

   byte *    bfim = NULL ;
   short *   sfim = NULL ;
   float *   ffim = NULL ;
   complex * cfim = NULL ;

   float fbot,ftop ;     /* 10 Jan 2003 */
   int nxyz ;

   /*** initialize ***/

ENTRY("EDIT_blur_volume_3d") ;

   if( vfim == NULL ||
       sigmax <= 0.0 || sigmay <= 0.0 || sigmaz <= 0.0)  EXRETURN ;

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

   aa  = sigmax * sigmax * 0.5 ;
   nup = nx + (int)(3.0 * sigmax / dx) ;      /* min FFT length */
#if 0
   ii  = 2 ; while( ii < nup ){ ii *= 2 ; }  /* next power of 2 larger */
#else
   ii = csfft_nextup_one35(nup) ;
#endif
   nup = ii ; nby2 = nup / 2 ;

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

STATUS("start y FFTs") ;

   aa  = sigmay * sigmay * 0.5 ;
   nup = ny + (int)(3.0 * sigmay / dy) ;      /* min FFT length */
#if 0
   ii  = 2 ; while( ii < nup ){ ii *= 2 ; }  /* next power of 2 larger */
#else
   ii = csfft_nextup_one35(nup) ;
#endif
   nup = ii ; nby2 = nup / 2 ;

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

STATUS("start z FFTs") ;

   aa  = sigmaz * sigmaz * 0.5 ;
   nup = nz + (int)(3.0 * sigmaz / dz) ;      /* min FFT length */
#if 0
   ii  = 2 ; while( ii < nup ){ ii *= 2 ; }  /* next power of 2 larger */
#else
   ii = csfft_nextup_one35(nup) ;
#endif
   nup = ii ; nby2 = nup / 2 ;

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

   /*** done! ***/

   EXRETURN ;
}
