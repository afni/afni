/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*----------------------------------------------------------------------
   Input:  far   = array of floats length nar
           shift = fractional shift

   Output: Newly malloc-ed array that is supposed to be like
           far[ii-shift] for ii=0..nar-1.

   Notes: * The shift is to the RIGHT (a good Republican, of course).
          * If nar==1, then the output is an array of length 1 that is
             just a copy of the input.
          * Otherwise, cubic interpolation is used.
------------------------------------------------------------------------*/

#define GET_AS_BIG(name,type,dim)                                           \
   do{ if( (dim) > name ## _size ){                                         \
          if( name != NULL ) free(name) ;                                   \
          name = (type *) malloc( sizeof(type) * (dim) ) ;                  \
          if( name == NULL ){                                               \
             fprintf(stderr,"*** can't malloc shifter space\n"); EXIT(1); } \
          name ## _size = (dim) ; } } while(0)

/* cubic interpolation polynomials */

#define P_M1(x)  ((x)*(1.0-(x))*((x)-2.0))
#define P_00(x)  (3.0*((x)+1.0)*((x)-1.0)*((x)-2.0))
#define P_P1(x)  (3.0*(x)*((x)+1.0)*(2.0-(x)))
#define P_P2(x)  ((x)*((x)+1.0)*((x)-1.0))
#define SIXTH    0.1666667

float * shifter( int nar , float * far , float shift )
{
   int ii,jj , nup , nmid , ix ;
   float xx , wt_m1,wt_00,wt_p1,wt_p2 , fmin,fmax ;
   float * fnew ;

   static int fl_size  = 0 ;     /* workspace (will hang around between calls) */
   static float * fl = NULL ;

   /*-- sanity checks --*/

   if( nar <= 0 || far == NULL ) return NULL ;

   if( nar == 1 ){
      fnew = (float *) malloc( sizeof(float) ) ;
      if( fnew == NULL ){
          fprintf(stderr,"*** can't malloc shifter output\n"); EXIT(1);
      }
     *fnew = far[0] ;
      return fnew ;
   }

   /*-- get workspace --*/

   nup = nar + (int)( 2.0*fabs(shift) + 6.0 ) ;
   GET_AS_BIG(fl,float,nup) ;

   /*-- Insert data:
          far[0] --> fl[nmid], etc.;
          fl[] points before nmid are copies of far[0];
          fl[] points after nmid+nar-1 are copiex of far[nar-1]. --*/

   nmid  = (nup-nar) / 2 ;
   for( ii=0 ; ii < nup ; ii++ ){
      jj = ii - nmid ;
      if( jj < 0 ) jj = 0 ; else if( jj >= nar ) jj = nar-1 ;
      fl[ii] = far[jj] ;
   }

   /*-- put results into output array --*/

   fnew = (float *) malloc( sizeof(float) * nar ) ;
   if( fnew == NULL ){
       fprintf(stderr,"*** can't malloc shifter output\n"); EXIT(1);
   }

   fmax = fmin = far[0] ;          /* find min and max of input */
   for( ii=1 ; ii < nar ; ii++ ){  /* for "clipping" purposes   */
      fmax = MAX(fmax,far[ii]) ;
      fmin = MIN(fmin,far[ii]) ;
   }

   for( ii=0 ; ii < nar ; ii++ ){
      xx = ii+nmid - shift ;  /* "index" in fl we want */
      ix = (int) xx ; xx = xx - ix ;
      wt_m1 = P_M1(xx) ; wt_00 = P_00(xx) ;
      wt_p1 = P_P1(xx) ; wt_p2 = P_P2(xx) ;
      fnew[ii] = SIXTH * (  wt_m1 * fl[ix-1] + wt_00 * fl[ix]
                          + wt_p1 * fl[ix+1] + wt_p2 * fl[ix+2] ) ;

           if( fnew[ii] < fmin ) fnew[ii] = fmin ;
      else if( fnew[ii] > fmax ) fnew[ii] = fmax ;
   }

   return fnew ;
}

/*---------------------------------------------------------------------
   Shift a 1D image (timeseries).  Values that are WAY_BIG act as
   blocks to the shift.
-----------------------------------------------------------------------*/

MRI_IMAGE * mri_shift_1D( MRI_IMAGE * im , float shift )
{
   MRI_IMAGE * newim , * flim ;
   float * newar , * flar , * shar ;
   int ii , ibot,itop , nx ;

   /*-- sanity check --*/

   if( im == NULL ) return NULL ;

   /*-- create output image --*/

   if( im->kind != MRI_float ) flim = mri_to_float( im ) ;
   else                        flim = im ;
   flar = MRI_FLOAT_PTR(flim) ;

   nx    = flim->nx ;
   newim = mri_new( nx , 1 , MRI_float ) ;
   newar = MRI_FLOAT_PTR(newim) ;

   /*-- scan for unbroken blocks to shift --*/

   ibot = 0 ;
   while( ibot < nx ){

      if( flar[ibot] >= WAY_BIG ){    /* just copy values */
         newar[ibot] = flar[ibot] ;   /* that are WAY_BIG */
         ibot++ ;
         continue ;
      }

      for( ii=ibot+1 ; ii < nx ; ii++ )    /* scan for next WAY_BIG */
         if( flar[ii] >= WAY_BIG ) break ;

      itop = ii ;  /* values from ibot to itop-1 are OK to shift */

      /* shift and copy output into new image */

      shar = shifter( itop-ibot , flar+ibot , shift ) ;
      for( ii=ibot ; ii < itop ; ii++ ) newar[ii] = shar[ii-ibot] ;
      free(shar) ; shar = NULL ;

      ibot = itop ;  /* start here next loop */
   }

   /*-- cleanup and exit --*/

   if( flim != im ) mri_free(flim) ;
   return newim ;
}
