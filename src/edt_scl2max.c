#include "mrilib.h"

#undef  AFNI_DEBUG
#undef  CLUST_DEBUG
#define STATUS(x) /* nada */
#define ENTRY(x)  /* nada */
#define EXRETURN  return
#define RETURN(x) return(x)


/*---------------------------------------------------------------
   Linearly scale values so that largest value in volume is 10000
   (255 for byte data volumes)
-----------------------------------------------------------------*/

void MCW_scale_to_max( int nx, int ny, int nz , int ftype , void * fim )
{
   register int nxyz = nx * ny * nz , ii ;
   register float fac ;

   if( fim == NULL ) return ;

   switch( ftype ){

      case MRI_byte:{
         register byte * bfar = (byte *) fim ;
         register byte max , val ;
         max = bfar[0] ;
         for( ii=1 ; ii < nxyz ; ii++ ){
            val = bfar[ii] ; if( val > max ) max = val ;
         }
         if( max == 0 ) return ;
         fac = 255.0 / max ;
         for( ii=0 ; ii < nxyz ; ii++ ) bfar[ii] *= fac ;
      }
      break ;

      case MRI_short:{
         register short * sfar = (short *) fim ;
         register short max , val ;
         max = abs(sfar[0]) ;
         for( ii=1 ; ii < nxyz ; ii++ ){
            val = abs(sfar[ii]) ; if( val > max ) max = val ;
         }
         if( max == 0 ) return ;
         fac = 10000.0 / max ;
         for( ii=0 ; ii < nxyz ; ii++ ) sfar[ii] *= fac ;
      }
      break ;

      case MRI_float:{
         register float * ffar = (float *) fim ;
         register float max , val ;
         max = fabs(ffar[0]) ;
         for( ii=1 ; ii < nxyz ; ii++ ){
            val = fabs(ffar[ii]) ; if( val > max ) max = val ;
         }
         if( max == 0.0 ) return ;
         fac = 10000.0 / max ;
         for( ii=0 ; ii < nxyz ; ii++ ) ffar[ii] *= fac ;
      }
      break ;

      case MRI_complex:{
         register complex * cfar = (complex *) fim ;
         register float max , val ;
         max = CSQR(cfar[0]) ;
         for( ii=1 ; ii < nxyz ; ii++ ){
            val = CSQR(cfar[ii]) ; if( val > max ) max = val ;
         }
         if( max == 0.0 ) return ;
         fac = 10000.0 / sqrt(max) ;
         for( ii=0 ; ii < nxyz ; ii++ ){
            cfar[ii].r *= fac ; cfar[ii].i *= fac ;
         }
      }
      break ;
   }

   return ;
}
