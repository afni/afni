/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*** Not 7D safe ***/

#define MAX_NUP 32

/*------------------------------------------------------------------------------
   Blow up a 2D image nup times, using 7th order polynomial for interpolation.
--------------------------------------------------------------------------------*/

MRI_IMAGE * mri_dup2D( int nup , MRI_IMAGE * imin )
{
   MRI_IMAGE * flim , * newim ;
   float     * flar , * newar , * cold , * cnew ;
   int nx,ny , nxup,nyup , ii,jj,kk ;

   /*-- sanity checks --*/

   if( nup < 1 || nup > MAX_NUP || imin == NULL ) return NULL ;

   if( nup == 1 ){ newim = mri_to_mri( imin->kind, imin ); return newim; }

   /*-- complex-valued images: do each part separately --*/

   if( imin->kind == MRI_complex ){
      MRI_IMARR *impair ; MRI_IMAGE * rim, * iim, * tim ;

      impair = mri_complex_to_pair( imin ) ;
      if( impair == NULL ){
         fprintf(stderr,"*** mri_complex_to_pair fails in mri_dup2D!\n"); EXIT(1);
      }
      rim = IMAGE_IN_IMARR(impair,0) ;
      iim = IMAGE_IN_IMARR(impair,1) ;  FREE_IMARR(impair) ;
      tim = mri_dup2D( nup, rim ); mri_free( rim ); rim = tim ;
      tim = mri_dup2D( nup, iim ); mri_free( iim ); iim = tim ;
      newim = mri_pair_to_complex( rim , iim ) ;
      mri_free( rim ) ; mri_free( iim ) ;
      MRI_COPY_AUX(newim,imin) ;
      return newim ;
   }

   /*-- rgb-valued image: do each color separately --*/

   if( imin->kind == MRI_rgb ){
      MRI_IMARR *imtriple ; MRI_IMAGE *rim, *gim, *bim, *tim ;

      imtriple = mri_rgb_to_3byte( imin ) ;
      if( imtriple == NULL ){
         fprintf(stderr,"*** mri_rgb_to_3float fails in mri_dup2D!\n"); EXIT(1);
      }
      rim = IMAGE_IN_IMARR(imtriple,0) ;
      gim = IMAGE_IN_IMARR(imtriple,1) ;
      bim = IMAGE_IN_IMARR(imtriple,2) ; FREE_IMARR(imtriple) ;
      tim = mri_dup2D( nup, rim ); mri_free(rim); rim = tim;
      tim = mri_dup2D( nup, gim ); mri_free(gim); gim = tim;
      tim = mri_dup2D( nup, bim ); mri_free(bim); bim = tim;
      newim = mri_3to_rgb( rim, gim, bim ) ;
      mri_free(rim) ; mri_free(gim) ; mri_free(bim) ;
      MRI_COPY_AUX(newim,imin) ;
      return newim ;
   }

   /*-- otherwise, make sure we operate on a float image --*/

   if( imin->kind == MRI_float ) flim = imin ;
   else                          flim = mri_to_float( imin ) ;

   flar = MRI_FLOAT_PTR(flim) ;

   nx = flim->nx ; ny = flim->ny ; nxup = nx*nup ; nyup = ny*nup ;
   newim = mri_new( nxup , nyup , MRI_float ) ;
   newar = MRI_FLOAT_PTR(newim) ;

   /*-- upsample rows --*/

   for( jj=0 ; jj < ny ; jj++ )
      mri_upsample( nup , nx , flar + jj*nx , newar + jj*nxup ) ;

   if( flim != imin ) mri_free(flim) ;

   /*-- upsample columns --*/

   cold = (float *) malloc( sizeof(float) * ny ) ;
   cnew = (float *) malloc( sizeof(float) * nyup ) ;
   if( cold == NULL || cnew == NULL ){
      fprintf(stderr,"*** mri_dup2D malloc failure!\n"); EXIT(1);
   }

   for( ii=0 ; ii < nxup ; ii++ ){
      for( jj=0 ; jj < ny ; jj++ ) cold[jj] = newar[ii + jj*nxup] ;
      mri_upsample( nup , ny , cold , cnew ) ;
      for( jj=0 ; jj < nyup ; jj++ ) newar[ii+jj*nxup] = cnew[jj] ;
   }

   free(cold) ; free(cnew) ;

   /*-- type convert output, if necessary --*/

   switch( imin->kind ){

      case MRI_byte:{
         byte * bar ; MRI_IMAGE * bim ; float fmin , fmax ;

         bim = mri_new( nxup,nyup , MRI_byte ) ; bar = MRI_BYTE_PTR(bim) ;
         fmin = mri_min(imin) ; fmax = mri_max(imin) ;
         for( ii=0 ; ii < newim->nvox ; ii++ )
            bar[ii] =  (newar[ii] < fmin) ? fmin
                     : (newar[ii] > fmax) ? fmax : newar[ii] ;
         mri_free(newim) ; newim = bim ;
      }
      break ;

      case MRI_short:{
         short * sar ; MRI_IMAGE * sim ; float fmin , fmax ;

         sim = mri_new( nxup,nyup , MRI_short ) ; sar = MRI_SHORT_PTR(sim) ;
         fmin = mri_min(imin) ; fmax = mri_max(imin) ;
         for( ii=0 ; ii < newim->nvox ; ii++ )
            sar[ii] =  (newar[ii] < fmin) ? fmin
                     : (newar[ii] > fmax) ? fmax : newar[ii] ;
         mri_free(newim) ; newim = sim ;
      }
      break ;

      case MRI_float:{
         float fmin , fmax ;

         fmin = mri_min(imin) ; fmax = mri_max(imin) ;
         for( ii=0 ; ii < newim->nvox ; ii++ )
                 if( newar[ii] < fmin ) newar[ii] = fmin ;
            else if( newar[ii] > fmax ) newar[ii] = fmax ;
      }
   }

   /*-- finito --*/

   MRI_COPY_AUX(newim,imin) ;
   return newim ;
}

/*======================================================================*/

/*-- seventh order Lagrange polynomials --*/

#define S_M3(x) (x*(x*x-1.0)*(x*x-4.0)*(x-3.0)*(4.0-x)*0.0001984126984)
#define S_M2(x) (x*(x*x-1.0)*(x-2.0)*(x*x-9.0)*(x-4.0)*0.001388888889)
#define S_M1(x) (x*(x-1.0)*(x*x-4.0)*(x*x-9.0)*(4.0-x)*0.004166666667)
#define S_00(x) ((x*x-1.0)*(x*x-4.0)*(x*x-9.0)*(x-4.0)*0.006944444444)
#define S_P1(x) (x*(x+1.0)*(x*x-4.0)*(x*x-9.0)*(4.0-x)*0.006944444444)
#define S_P2(x) (x*(x*x-1.0)*(x+2.0)*(x*x-9.0)*(x-4.0)*0.004166666667)
#define S_P3(x) (x*(x*x-1.0)*(x*x-4.0)*(x+3.0)*(4.0-x)*0.001388888889)
#define S_P4(x) (x*(x*x-1.0)*(x*x-4.0)*(x*x-9.0)*0.0001984126984)

static int nupold = -1 ;

static float fm3[MAX_NUP], fm2[MAX_NUP], fm1[MAX_NUP], f00[MAX_NUP],
             fp1[MAX_NUP], fp2[MAX_NUP], fp3[MAX_NUP], fp4[MAX_NUP] ;

#ifdef ZFILL
#  define FINS(i) ( ((i)<0 || (i)>=nar) ? 0.0 : far[(i)] )
#else
#  define FINS(i) ( ((i)<0) ? far[0] : ((i)>=nar) ? far[nar-1] : far[(i)] )
#endif

#define INT7(k,i) (  fm3[k] * far[i-3] + fm2[k] * far[i-2] \
                   + fm1[k] * far[i-1] + f00[k] * far[i  ] \
                   + fp1[k] * far[i+1] + fp2[k] * far[i+2] \
                   + fp3[k] * far[i+3] + fp4[k] * far[i+4]  )

#define FINT7(k,i) (  fm3[k] * FINS(i-3) + fm2[k] * FINS(i-2) \
                    + fm1[k] * FINS(i-1) + f00[k] * FINS(i  ) \
                    + fp1[k] * FINS(i+1) + fp2[k] * FINS(i+2) \
                    + fp3[k] * FINS(i+3) + fp4[k] * FINS(i+4)  )

/*----------------------------------------------------------------------------
  Up sample an array far[0..nar-1] nup times to produce fout[0..nar*nup-1].
  Uses 7th order polynomial interpolation.
------------------------------------------------------------------------------*/

void mri_upsample( int nup , int nar , float * far , float * fout )
{
   int kk , ii , ibot,itop ;

   /*-- sanity checks --*/

   if( nup < 1 || nup > MAX_NUP || nar < 2 || far == NULL || fout == NULL ) return ;

   if( nup == 1 ){ memcpy( fout, far, sizeof(float)*nar ); return; }

   /*-- initialize interpolation coefficient, if nup has changed --*/

   if( nup != nupold ){
      float val ;
      for( kk=0 ; kk < nup ; kk++ ){
         val = ((float)kk) / ((float)nup) ;
         fm3[kk] = S_M3(val); fm2[kk] = S_M2(val); fm1[kk] = S_M1(val);
         f00[kk] = S_00(val); fp1[kk] = S_P1(val); fp2[kk] = S_P2(val);
         fp3[kk] = S_P3(val); fp4[kk] = S_P4(val);
      }
      nupold = nup ;
   }

   /*-- interpolate the intermediate places --*/

   ibot = 3 ; itop = nar-5 ;

   switch( nup ){
      default:
         for( ii=ibot ; ii <= itop ; ii++ )
            for( kk=0 ; kk < nup ; kk++ ) fout[kk+ii*nup] = INT7(kk,ii) ;
      break ;

      case 2:
         for( ii=ibot ; ii <= itop ; ii++ ){
            fout[ii*nup]   = INT7(0,ii) ; fout[ii*nup+1] = INT7(1,ii) ;
         }
      break ;

      case 3:
         for( ii=ibot ; ii <= itop ; ii++ ){
            fout[ii*nup]   = INT7(0,ii) ; fout[ii*nup+1] = INT7(1,ii) ;
            fout[ii*nup+2] = INT7(2,ii) ;
         }
      break ;

      case 4:
         for( ii=ibot ; ii <= itop ; ii++ ){
            fout[ii*nup]   = INT7(0,ii) ; fout[ii*nup+1] = INT7(1,ii) ;
            fout[ii*nup+2] = INT7(2,ii) ; fout[ii*nup+3] = INT7(3,ii) ;
         }
      break ;
   }

   /*-- interpolate the outside edges --*/

   for( ii=0 ; ii < ibot ; ii++ )
      for( kk=0 ; kk < nup ; kk++ ) fout[kk+ii*nup] = FINT7(kk,ii) ;

   for( ii=itop+1 ; ii < nar ; ii++ )
      for( kk=0 ; kk < nup ; kk++ ) fout[kk+ii*nup] =  FINT7(kk,ii) ;

   return ;
}
