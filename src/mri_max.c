/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include <math.h>

/*** 7D SAFE ***/

double mri_max( MRI_IMAGE *im )
{
   register int ii , npix ;
   byte   byte_max   = 0 ;
   short  short_max  = -32767 ;       /* 23 Oct 1998: changed from 0 */
   int    int_max    = -2147483647 ;  /* ditto */
   float  float_max  = -1.e+38 ;      /* changed from -9999999.0 */
   double double_max = -1.e+38 ;      /* ditto */

WHOAMI ; IMHEADER(im) ;

   npix = im->nvox ;

   switch( im->kind ){

      case MRI_byte:
         for( ii=0 ; ii < npix ; ii++ )
            byte_max = MAX( byte_max , im->im.byte_data[ii] ) ;
         return (double) byte_max ;

      case MRI_short:
         for( ii=0 ; ii < npix ; ii++ )
            short_max = MAX( short_max , im->im.short_data[ii] ) ;
         return (double) short_max ;

      case MRI_int:
         for( ii=0 ; ii < npix ; ii++ )
            int_max = MAX( int_max , im->im.int_data[ii] ) ;
         return (double) int_max ;

      case MRI_float:
         for( ii=0 ; ii < npix ; ii++ )
            float_max = MAX( float_max , im->im.float_data[ii] ) ;
         return (double) float_max ;

      case MRI_double:
         for( ii=0 ; ii < npix ; ii++ )
            double_max = MAX( double_max , im->im.double_data[ii] ) ;
         return double_max ;

      case MRI_complex:
         for( ii=0 ; ii < npix ; ii++ )
            float_max = MAX( float_max , CSQR(im->im.complex_data[ii]) ) ;
         return sqrt(float_max) ;

      case MRI_rgb:{
         byte *rgb = im->im.rgb_data ;
         double val , top=0.0 ;
         for( ii=0 ; ii < npix ; ii++ ){  /* scale to brightness */
            val =  0.299 * rgb[3*ii]      /* between 0 and 255     */
                 + 0.587 * rgb[3*ii+1]
                 + 0.114 * rgb[3*ii+2] ;
            if( val > top ) top = val ;
         }
         return top ;
      }

      default:
         fprintf( stderr , "mri_max:  unknown image kind\n" ) ;
   }
   return 0 ;
}

double mri_maxabs( MRI_IMAGE * im )
{
   register int ii , npix ;
   byte   byte_max   = 0 ;
   int    int_max    = 0 ;
   double double_max = 0.0 ;

WHOAMI ; IMHEADER(im) ;

   npix = im->nvox ;

   switch( im->kind ){

      case MRI_byte:
         for( ii=0 ; ii < npix ; ii++ )
            byte_max = MAX( byte_max , im->im.byte_data[ii] ) ;
         return (double) byte_max ;

      case MRI_short:
         for( ii=0 ; ii < npix ; ii++ )
            int_max = MAX( int_max , abs(im->im.short_data[ii]) ) ;
         return (double) int_max ;

      case MRI_int:
         for( ii=0 ; ii < npix ; ii++ )
            int_max = MAX( int_max , abs(im->im.int_data[ii]) ) ;
         return (double) int_max ;

      case MRI_float:
         for( ii=0 ; ii < npix ; ii++ )
            double_max = MAX( double_max , fabs(im->im.float_data[ii]) ) ;
         return double_max ;

      case MRI_double:
         for( ii=0 ; ii < npix ; ii++ )
            double_max = MAX( double_max , fabs(im->im.double_data[ii]) ) ;
         return double_max ;

      case MRI_complex:
         for( ii=0 ; ii < npix ; ii++ )
            double_max = MAX( double_max , CSQR(im->im.complex_data[ii]) ) ;
         return sqrt(double_max) ;

      case MRI_rgb:
         return mri_max( im ) ;

      default:
         fprintf( stderr , "mri_max:  unknown image kind\n" ) ;
   }
   return 0 ;
}

double mri_min( MRI_IMAGE *im )
{
   register int ii , npix ;
   byte   byte_min   = 255 ;
   short  short_min  = 32767 ;
   int    int_min    = 2147483647 ;
   float  float_min  = 1.e+38 ;
   double double_min = 1.e+38 ;

WHOAMI ; IMHEADER(im) ;

   npix = im->nvox ;

   switch( im->kind ){

      case MRI_byte:
         for( ii=0 ; ii < npix ; ii++ )
            byte_min = MIN( byte_min , im->im.byte_data[ii] ) ;
         return (double) byte_min ;

      case MRI_short:
         for( ii=0 ; ii < npix ; ii++ )
            short_min = MIN( short_min , im->im.short_data[ii] ) ;
         return (double) short_min ;

      case MRI_int:
         for( ii=0 ; ii < npix ; ii++ )
            int_min = MIN( int_min , im->im.int_data[ii] ) ;
         return (double) int_min ;

      case MRI_float:
         for( ii=0 ; ii < npix ; ii++ )
            float_min = MIN( float_min , im->im.float_data[ii] ) ;
         return (double) float_min ;

      case MRI_double:
         for( ii=0 ; ii < npix ; ii++ )
            double_min = MIN( double_min , im->im.double_data[ii] ) ;
         return double_min ;

      case MRI_complex:
         for( ii=0 ; ii < npix ; ii++ )
            float_min = MIN( float_min , CSQR(im->im.complex_data[ii]) ) ;
         return sqrt(float_min) ;

      case MRI_rgb:{
         byte *rgb = im->im.rgb_data ;
         double val , bot=255.9 ;
         for( ii=0 ; ii < npix ; ii++ ){  /* scale to brightness */
            val =  0.299 * rgb[3*ii]      /* between 0 and 255     */
                 + 0.587 * rgb[3*ii+1]
                 + 0.114 * rgb[3*ii+2] ;
            if( val < bot ) bot = val ;
         }
         return bot ;
      }

      default:
         fprintf( stderr , "mri_min:  unknown image kind\n" ) ;
   }
   return 0 ;
}
