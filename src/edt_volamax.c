#include "mrilib.h"

#undef  AFNI_DEBUG
#undef  CLUST_DEBUG
#define STATUS(x) /* nada */
#define ENTRY(x)  /* nada */
#define EXRETURN  return
#define RETURN(x) return(x)


/*********** find the largest value in a volume of data ***********/

float MCW_vol_amax( int nx, int ny, int nz , int ftype , void * fim )
{
   register int nxyz = nx * ny * nz , ii ;

ENTRY("MCW_vol_amax") ;

   if( fim == NULL ) RETURN( 0.0 );

   switch( ftype ){

      case MRI_byte:{
         register byte * bfar = (byte *) fim ;
         register byte max , val ;
         max = bfar[0] ;
         for( ii=1 ; ii < nxyz ; ii++ ){
            val = bfar[ii] ; if( val > max ) max = val ;
         }
         RETURN ((float) max) ;
      }
      break ;

      case MRI_short:{
         register short * sfar = (short *) fim ;
         register short max , val ;
         max = abs(sfar[0]) ;
         for( ii=1 ; ii < nxyz ; ii++ ){
            val = abs(sfar[ii]) ; if( val > max ) max = val ;
         }
         RETURN ((float) max) ;
      }
      break ;

      case MRI_float:{
         register float * ffar = (float *) fim ;
         register float max , val ;
         max = fabs(ffar[0]) ;
         for( ii=1 ; ii < nxyz ; ii++ ){
            val = fabs(ffar[ii]) ; if( val > max ) max = val ;
         }
         RETURN ((float) max) ;
      }
      break ;

      case MRI_complex:{
         register complex * cfar = (complex *) fim ;
         register float max , val ;
         max = CSQR(cfar[0]) ;
         for( ii=1 ; ii < nxyz ; ii++ ){
            val = CSQR(cfar[ii]) ; if( val > max ) max = val ;
         }
         RETURN( (float) sqrt(max) );
      }
      break ;
   }
   RETURN( 0.0 );
}
