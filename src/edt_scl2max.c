/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*---------------------------------------------------------------
   Linearly scale values so that largest value in volume is 10000
   (255 for byte data volumes)
-----------------------------------------------------------------*/

void MCW_scale_to_max( int nx, int ny, int nz , int ftype , void * fim )
{
   register int nxyz = nx * ny * nz , ii ;
   register float fac ;

ENTRY("MCW_scale_to_max") ;

   if( fim == NULL ) EXRETURN ;

   switch( ftype ){

      case MRI_byte:{
         register byte * bfar = (byte *) fim ;
         register byte max , val ;
         max = bfar[0] ;
         for( ii=1 ; ii < nxyz ; ii++ ){
            val = bfar[ii] ; if( val > max ) max = val ;
         }
         if( max == 0 ) EXRETURN ;
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
         if( max == 0 ) EXRETURN ;
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
         if( max == 0.0 ) EXRETURN ;
         fac = 10000.0 / max ;
         for( ii=0 ; ii < nxyz ; ii++ ) ffar[ii] *= fac ;
      }
      break ;

      case MRI_complex:{
         register complex * cfar = (complex *) fim ;
         register float max , val ;
         max = CABS(cfar[0]) ;
         for( ii=1 ; ii < nxyz ; ii++ ){
            val = CABS(cfar[ii]) ; if( val > max ) max = val ;
         }
         if( max == 0.0 ) EXRETURN ;
         fac = 10000.0 / max ;
         for( ii=0 ; ii < nxyz ; ii++ ){
            cfar[ii].r *= fac ; cfar[ii].i *= fac ;
         }
      }
      break ;
   }

   EXRETURN ;
}
