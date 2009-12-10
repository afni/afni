#include "mrilib.h"

/*----------------------------------------------------------------------------*/
/*! Various sorts of t-tests.
   - numx = number of points in the first sample (must be > 1)
   - xar  = array with first sample
   - numy = number of points in the second sample
             - numy = 0 ==> a 1 sample test of first sample against mean=0
             - numy = 1 ==> a 1 sample test of first sample against mean=yar[0]
             - numy > 1 ==> a 2 sample test; opcode determines what kind
   - opcode = 0 for unpaired test with pooled variance
   - opcode = 1 for unpaired test with unpooled variance
   - opcode = 2 for paired test (numx == numy is required)
   - The return value is a float_triple, say 'r', with
      - r.a = difference of means
      - r.b = t statistic
      - r.c = DOF parameter (if this is 0, an error transpired)
*//*--------------------------------------------------------------------------*/

float_triple student_ttest( int numx , float *xar ,
                            int numy , float *yar , int opcode )
{
   float_triple result = {0.0f,0.0f,0.0f} ;
   register int ii ; register float val ;
   float avx,sdx , avy,sdy , base=0.0f ;
   int paired=(opcode==2) , pooled=(opcode==1) ;

   /* check inputs for stoopidities or other things that need to be changed */

   if( numx < 2 || xar == NULL                 ) return result ; /* bad */
   if( paired && (numy != numx || yar == NULL) ) return result ; /* bad */

   if( numy == 1 && yar != NULL ){ base = yar[0] ; }
   if( numy  < 2 || yar == NULL ){ numy = paired = pooled = 0 ; yar = NULL ; }

   if( paired ){   /* Case 1: paired t test */

     avx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ) avx += xar[ii]-yar[ii] ;
     avx /= numx ; sdx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ){ val = xar[ii]-yar[ii]-avx; sdx += val*val; }
     val = (sdx > 0.0f) ?  val = avx / sqrtf( sdx/((numx-1.0f)*numx) ) : 0.0f ;
     result.a = avx ; result.b = val ; result.c = numx-1.0f ;

   } else if( numy == 0 ){  /* Case 2: 1 sample test against mean==base */

     avx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ) avx += xar[ii] ;
     avx /= numx ; sdx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ){ val = xar[ii]-avx ; sdx += val*val ; }
     val = (sdx > 0.0f) ? (avx-base) / sqrtf( sdx/((numx-1.0f)*numx) ) : 0.0f ;
     result.a = (avx-base) ; result.b = val ; result.c = numx-1.0f ;

   } else {  /* Case 3: 2 sample test (pooled or unpooled) */

     avx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ) avx += xar[ii] ;
     avx /= numx ; sdx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ){ val = xar[ii] - avx ; sdx += val*val ; }

     avy = 0.0f ;
     for( ii=0 ; ii < numy ; ii++ ) avy += yar[ii] ;
     avy /= numy ; sdy = 0.0f ;
     for( ii=0 ; ii < numy ; ii++ ){ val = yar[ii] - avy ; sdy += val*val ; }
     result.a = (avx-avy) ;

     if( sdx+sdy == 0.0f ){

       result.b = 0.0f ; result.c = numx+numy-2.0f ;

     } else if( pooled ){  /* Case 3a: pooled variance estimate */

       sdx = (sdx+sdy) / (numx+numy-2.0f) ;
       val = (avx-avy) / sqrtf( sdx*(1.0f/numx+1.0f/numy) ) ;
       result.b = val ; result.c = numx+numy-2.0f ;

     } else {       /* Case 3b: unpooled variance estimate */

       sdx /= (numx-1.0f)*numx ; sdy /= (numy-1.0f)*numy ; val = sdx+sdy ;
       result.b = (avx-avy) / sqrtf(val) ;
       result.c = (val*val) / (sdx*sdx/(numx-1.0f) + sdy*sdy/(numy-1.0f) ) ;

     }

   } /* end of all possible cases */

   return result ;
}
