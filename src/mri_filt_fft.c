/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** NOT 7D SAFE ***/

/*--------------------------------------------------------------------------
 Routine to filter a 2D image in Fourier space:
  float sigma --> if > 0, sigma for Gaussian blur: exp(-k**2 * sigma**2/2)
  int   diffx --> if != 0, apply d/dx:             i * k_x
  int   diffy --> if != 0, apply d/dy:             i * k_y
  int   code  --> bit mapped options to control filter.  currently:
                    FILT_FFT_WRAPAROUND  -->  allows wraparound in FFT

 Note that the units of sigma are the same as those of im->dx, im->dy
 (defaults are 1.0 = units of pixel dimensions).

 The output is an image in the floating point format.  Input can be
 any format (but complex inputs will have CABS taken first).
----------------------------------------------------------------------------*/

#define GET_AS_BIG(name,type,dim)                                           \
   do{ if( (dim) > name ## _size ){                                         \
          if( name != NULL ) free(name) ;                                   \
          name = (type *) malloc( sizeof(type) * (dim) ) ;                  \
          if( name == NULL ){                                               \
             fprintf(stderr,"*** can't malloc mri_filt_fft workspace\n") ;  \
             EXIT(1) ; }                                                    \
          name ## _size = (dim) ; }                                         \
       break ; } while(1)

MRI_IMAGE * mri_filt_fft( MRI_IMAGE * im ,
                          float sigma , int diffx , int diffy , int code )
{
   int jj, nby2 , nx,ny ;
   float  dk , aa , k , fac , dx,dy ;
   register int ii , nup ;
   register float * bfim ;

   static int cx_size  = 0 ;     /* workspaces (will hang around between calls) */
   static int gg_size  = 0 ;
   static complex * cx = NULL ;
   static float   * gg = NULL ;

   MRI_IMAGE * flim ;
   float     * flar ;

   /*** initialize ***/

   if( im == NULL ){
      fprintf(stderr,"*** mri_filt_fft: NULL input image\n") ;
      return NULL ;
   }

   if( sigma < 0.0 ){
      fprintf(stderr,"*** mri_filt_fft: Illegal control parameters input\n");
      return NULL ;
   }

   if( ! MRI_IS_2D(im) ){
      fprintf(stderr,"*** mri_filt_fft: Only works on 2D images\n") ;
      EXIT(1) ;
   }

   nx = im->nx ; ny = im->ny ;
   dx = fabs(im->dx) ; if( dx == 0.0 ) dx = 1.0 ;
   dy = fabs(im->dy) ; if( dy == 0.0 ) dy = 1.0 ;

   aa = sigma * sigma * 0.5 ;

   flim = mri_to_float( im ) ;        /* will be output */
   flar = mri_data_pointer( flim ) ;

   if( sigma == 0.0 && diffx == 0 && diffy == 0 ) return flim ;  /* no action! */

   /*** do x-direction ***/

   if( (code & FILT_FFT_WRAPAROUND) == 0 ){
      nup = nx + (int)(3.0 * sigma / dx) ;      /* min FFT length */
   } else {
      nup = nx ;
   }
   ii  = 4 ; while( ii < nup ){ ii *= 2 ; }  /* next power of 2 larger */
   nup = ii ; nby2 = nup / 2 ;

   GET_AS_BIG(cx,complex,nup) ; GET_AS_BIG(gg,float,nup) ;

   dk    = (2.0*PI) / (nup * dx) ;
   fac   = 1.0 / nup ;
   gg[0] = fac ;
   if( aa > 0.0 ){
      for( ii=1 ; ii<=nby2 ; ii++ ){ k=ii*dk; gg[nup-ii]=gg[ii]=fac*exp(-aa*k*k); }
   } else {
      for( ii=1 ; ii < nup ; ii++ ) gg[ii] = fac ;
   }

   if( diffx ){
      gg[0] = gg[nby2] = 0.0 ;
      for( ii=1 ; ii < nby2 ; ii++ ){ k=ii*dk ; gg[ii] *= k ; gg[nup-ii] *= (-k) ; }
   }

   /** July 19 1995: double up on FFTs **/

   for( jj=0 ; jj < ny ; jj+=2 ){
      bfim = flar + jj*nx ;
      if( jj == ny-1 )
         for( ii=0 ; ii<nx ; ii++){ cx[ii].r = bfim[ii] ; cx[ii].i = 0.0 ; }  /* copy in */
      else
         for( ii=0 ; ii<nx ; ii++){ cx[ii].r = bfim[ii] ; cx[ii].i = bfim[ii+nx] ; }
      for( ii=nx; ii<nup; ii++){ cx[ii].r = cx[ii].i = 0.0 ; }                /* zero pad */
      csfft_cox( -1 , nup , cx ) ;                                            /* FFT */
      for( ii=0 ; ii<nup; ii++){ cx[ii].r *= gg[ii] ; cx[ii].i *= gg[ii] ; }  /* filter */
      if( diffx ){
         float tt ;
         for( ii=0 ; ii < nup ; ii++ ){
            tt = cx[ii].r ; cx[ii].r = -cx[ii].i ; cx[ii].i = tt ;            /* mult by i */
         }
      }
      csfft_cox(  1 , nup , cx ) ;                                            /* inv FFT */
      if( jj == ny-1 )
         for( ii=0 ; ii<nx ; ii++){ bfim[ii] = cx[ii].r ; }                   /* copy out */
      else
         for( ii=0 ; ii<nx ; ii++){ bfim[ii] = cx[ii].r ; bfim[ii+nx] = cx[ii].i ; }
   }

   /*** do y-direction ***/

   if( (code & FILT_FFT_WRAPAROUND) == 0 ){
      nup = ny + (int)(3.0 * sigma / dy) ;      /* min FFT length */
   } else {
      nup = ny ;
   }
   ii  = 2 ; while( ii < nup ){ ii *= 2 ; }  /* next power of 2 larger */
   nup = ii ; nby2 = nup / 2 ;

   GET_AS_BIG(cx,complex,nup) ; GET_AS_BIG(gg,float,nup) ;

   dk    = (2.0*PI) / (nup * dy) ;
   fac   = 1.0 / nup ;
   gg[0] = fac ;

   if( aa > 0.0 ){
      for( ii=1 ; ii<=nby2 ; ii++ ){ k=ii*dk; gg[nup-ii]=gg[ii]=fac*exp(-aa*k*k); }
   } else {
      for( ii=1 ; ii < nup ; ii++ ) gg[ii] = fac ;
   }

   if( diffy ){
      gg[0] = gg[nby2] = 0.0 ;
      for( ii=1 ; ii < nby2 ; ii++ ){ k=ii*dk ; gg[ii] *= k ; gg[nup-ii] *= (-k) ; }
   }

   for( jj=0 ; jj < nx ; jj+=2 ){
      bfim = flar + jj ;
      if( jj == nx-1 )
         for( ii=0 ; ii<ny ; ii++){ cx[ii].r = bfim[ii*nx] ; cx[ii].i = 0.0 ; }
      else
         for( ii=0 ; ii<ny ; ii++){ cx[ii].r = bfim[ii*nx] ; cx[ii].i = bfim[ii*nx+1] ; }
      for( ii=ny; ii<nup; ii++){ cx[ii].r = cx[ii].i = 0.0 ; }
      csfft_cox( -1 , nup , cx ) ;
      for( ii=0 ; ii<nup; ii++){ cx[ii].r *= gg[ii] ; cx[ii].i *= gg[ii] ; }
      if( diffy ){
         float tt ;
         for( ii=0 ; ii < nup ; ii++ ){
            tt = cx[ii].r ; cx[ii].r = -cx[ii].i ; cx[ii].i = tt ;  /* multiply by i */
         }
      }
      csfft_cox(  1 , nup , cx ) ;
      if( jj == nx-1 )
         for( ii=0 ; ii<ny ; ii++){ bfim[ii*nx] = cx[ii].r ; }
      else
         for( ii=0 ; ii<ny ; ii++){ bfim[ii*nx] = cx[ii].r ; bfim[ii*nx+1] = cx[ii].i ; }
   }

   /*** done! ***/

   return flim ;
}
