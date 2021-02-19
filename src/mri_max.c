/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include <math.h>

/*** 7D SAFE ***/

/*---------------------------------------------------------------------------*/

double mri_max( MRI_IMAGE *im )
{
   register int ii , npix ;
   byte   byte_max   = 0 ;
   short  short_max  = -32767 ;       /* 23 Oct 1998: changed from 0 */
   int    int_max    = -2147483647 ;  /* ditto */
   float  float_max  = -1.e+38 ;      /* changed from -9999999.0 */
   double double_max = -1.e+38 ;      /* ditto */

ENTRY("mri_max") ;

   npix = im->nvox ;

   switch( im->kind ){

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            byte_max = MAX( byte_max , qar[ii] ) ;
         RETURN( (double) byte_max );
      }

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            short_max = MAX( short_max , qar[ii] ) ;
         RETURN( (double) short_max );
      }

      case MRI_int:{
         int *qar = MRI_INT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            int_max = MAX( int_max , qar[ii] ) ;
         RETURN( (double) int_max );
      }

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            float_max = MAX( float_max , qar[ii] ) ;
         RETURN( (double) float_max );
      }

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            double_max = MAX( double_max , qar[ii] ) ;
         RETURN( double_max );
      }

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            float_max = MAX( float_max , CABS(qar[ii]) ) ;
         RETURN( float_max );
      }

      case MRI_rgb:{
         byte *rgb = MRI_RGB_PTR(im) ;
         double val , top=0.0 ;
         for( ii=0 ; ii < npix ; ii++ ){  /* scale to brightness */
            val =  0.299 * rgb[3*ii]      /* between 0 and 255     */
                 + 0.587 * rgb[3*ii+1]
                 + 0.114 * rgb[3*ii+2] ;
            if( val > top ) top = val ;
         }
         RETURN( top );
      }

      default:
        ERROR_message("mri_max: unknown image kind") ;
   }
   RETURN( 0.0 );
}

/*---------------------------------------------------------------------------*/

double mri_maxabs( MRI_IMAGE * im )
{
   register int ii , npix ;
   byte   byte_max   = 0 ;
   int    int_max    = 0 ;
   double double_max = 0.0 ;

ENTRY("mri_maxabs") ;

   npix = im->nvox ;

   switch( im->kind ){

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            byte_max = MAX( byte_max , qar[ii] ) ;
         RETURN( (double) byte_max );
      }

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            int_max = MAX( int_max , abs(qar[ii]) ) ;
         RETURN( (double) int_max );
      }

      case MRI_int:{
         int *qar = MRI_INT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            int_max = MAX( int_max , abs(qar[ii]) ) ;
         RETURN( (double) int_max );
      }

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            double_max = MAX( double_max , fabs(qar[ii]) ) ;
         RETURN( double_max );
      }

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            double_max = MAX( double_max , fabs(qar[ii]) ) ;
         RETURN( double_max );
      }

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            double_max = MAX( double_max , CABS(qar[ii]) ) ;
         RETURN( double_max );
      }

      case MRI_rgb:
         RETURN( mri_max( im ) );

      default:
         ERROR_message("mri_max: unknown image kind") ;
   }
   RETURN( 0 );
}

/*---------------------------------------------------------------------------*/

double mri_min( MRI_IMAGE *im )
{
   register int ii , npix ;
   byte   byte_min   = 255 ;
   short  short_min  = 32767 ;
   int    int_min    = 2147483647 ;
   float  float_min  = 1.e+38 ;
   double double_min = 1.e+38 ;

ENTRY("mri_min") ;

   npix = im->nvox ;

   switch( im->kind ){

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            byte_min = MIN( byte_min , qar[ii] ) ;
         RETURN( (double) byte_min );
      }

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            short_min = MIN( short_min , qar[ii] ) ;
         RETURN( (double) short_min );
      }

      case MRI_int:{
         int *qar = MRI_INT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            int_min = MIN( int_min , qar[ii] ) ;
         RETURN( (double) int_min );
      }

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            float_min = MIN( float_min , qar[ii] ) ;
         RETURN( (double) float_min );
      }

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            double_min = MIN( double_min , qar[ii] ) ;
         RETURN( double_min );
      }

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            float_min = MIN( float_min , CABS(qar[ii]) ) ;
         RETURN( float_min );
      }

      case MRI_rgb:{
         byte *rgb = MRI_RGB_PTR(im) ;
         double val , bot=255.9 ;
         for( ii=0 ; ii < npix ; ii++ ){  /* scale to brightness */
            val =  0.299 * rgb[3*ii]      /* between 0 and 255     */
                 + 0.587 * rgb[3*ii+1]
                 + 0.114 * rgb[3*ii+2] ;
            if( val < bot ) bot = val ;
         }
         RETURN( bot );
      }

      default:
         ERROR_message("mri_min: unknown image kind") ;
   }
   RETURN( 0 );
}

/*---------------------------------------------------------------------------*/

double_pair mri_minmax( MRI_IMAGE *im )
{
   register int ii , npix ;
   byte   byte_min   = 255 ;
   short  short_min  = 32767 ;
   int    int_min    = 2147483647 ;
   float  float_min  = 1.e+38 ;
   double double_min = 1.e+38 ;
   byte   byte_max   = 0 ;
   short  short_max  = -32767 ;
   int    int_max    = -2147483647 ;
   float  float_max  = -1.e+38 ;
   double double_max = -1.e+38 ;
   double_pair dp = {0.0,0.0} ;

ENTRY("mri_minmax") ;

   npix = im->nvox ;

   switch( im->kind ){

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            byte_min = MIN( byte_min , qar[ii] ) ;
            byte_max = MAX( byte_max , qar[ii] ) ;
         }
         dp.a = (double)byte_min ; dp.b = (double)byte_max ; RETURN(dp) ;
      }

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            short_min = MIN( short_min , qar[ii] ) ;
            short_max = MAX( short_max , qar[ii] ) ;
         }
         dp.a = (double)short_min ; dp.b = (double)short_max ; RETURN(dp) ;
      }

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            float_min = MIN( float_min , qar[ii] ) ;
            float_max = MAX( float_max , qar[ii] ) ;
         }
         dp.a = (double)float_min ; dp.b = (double)float_max ; RETURN(dp) ;
      }

      default:
         ERROR_message("mri_minmax: unknown image kind") ;
   }
   RETURN( dp );
}

/*---------------------------------------------------------------------------*/

double_pair mri_minmax_nz( MRI_IMAGE *im )
{
   register int ii , npix ;
   byte   byte_min   = 255 ;
   short  short_min  = 32767 ;
   int    int_min    = 2147483647 ;
   float  float_min  = 1.e+38 ;
   double double_min = 1.e+38 ;
   byte   byte_max   = 0 ;
   short  short_max  = -32767 ;
   int    int_max    = -2147483647 ;
   float  float_max  = -1.e+38 ;
   double double_max = -1.e+38 ;
   double_pair dp = {0.0,0.0} ;

ENTRY("mri_minmax_nz") ;

   npix = im->nvox ;

   switch( im->kind ){

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            if( qar[ii] == 0 ) continue ;
            byte_min = MIN( byte_min , qar[ii] ) ;
            byte_max = MAX( byte_max , qar[ii] ) ;
         }
         if( byte_min <= byte_max ){
           dp.a = (double)byte_min ; dp.b = (double)byte_max ;
         }
         RETURN(dp) ;
      }

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            if( qar[ii] == 0 ) continue ;
            short_min = MIN( short_min , qar[ii] ) ;
            short_max = MAX( short_max , qar[ii] ) ;
         }
         if( short_min <= short_max ){
           dp.a = (double)short_min ; dp.b = (double)short_max ;
         }
         RETURN(dp) ;
      }

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            if( qar[ii] == 0 ) continue ;
            float_min = MIN( float_min , qar[ii] ) ;
            float_max = MAX( float_max , qar[ii] ) ;
         }
         if( float_min <= float_max ){
           dp.a = (double)float_min ; dp.b = (double)float_max ;
         }
         RETURN(dp) ;
      }

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(im) ; float aval ;
         for( ii=0 ; ii < npix ; ii++ ){
            aval = CABS(qar[ii]) ; if( aval == 0.0f ) continue ;
            float_min = MIN( float_min , aval ) ;
            float_max = MAX( float_max , aval ) ;
         }
         if( float_min <= float_max ){
           dp.a = (double)float_min ; dp.b = (double)float_max ;
         }
         RETURN(dp) ;
      }

      case MRI_rgb:{
         byte *rgb = MRI_RGB_PTR(im) ;
         double val ;
         for( ii=0 ; ii < npix ; ii++ ){  /* scale to brightness */
            val =  0.299 * rgb[3*ii]      /* between 0 and 255   */
                 + 0.587 * rgb[3*ii+1]
                 + 0.114 * rgb[3*ii+2] ; if( val == 0.0f ) continue ;
            float_min = MIN( float_min , val ) ;
            float_max = MAX( float_max , val ) ;
         }
         if( float_min <= float_max ){
           dp.a = (double)float_min ; dp.b = (double)float_max ;
         }
         RETURN(dp) ;
      }

      default:
         ERROR_message("mri_minmax_nz: unknown image kind") ;
   }
   RETURN( dp );
}

/*---------------------------------------------------------------------------*/

intfloat mri_indmax_nz( MRI_IMAGE *im )
{
   register int ii , npix ;
   byte   byte_max   = 0 ;
   short  short_max  = -32767 ;       /* 23 Oct 1998: changed from 0 */
   int    int_max    = -2147483647 ;  /* ditto */
   float  float_max  = -1.e+38 ;      /* changed from -9999999.0 */
   double double_max = -1.e+38 ;      /* ditto */
   intfloat result   = { -1 , 0.0f } ; int imax=-1 ;

ENTRY("mri_indmax_nz") ;

   npix = im->nvox ;

   switch( im->kind ){
      default: break ;

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            if( qar[ii] == 0 ) continue ;
            if( qar[ii] > byte_max ){ byte_max = qar[ii]; imax = ii; }
         }
         result.i = imax; result.a = (float)byte_max; RETURN(result);
      }

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            if( qar[ii] == 0 ) continue ;
            if( qar[ii] > short_max ){ short_max = qar[ii]; imax = ii; }
         }
         result.i = imax; result.a = (float)short_max; RETURN(result);
      }

      case MRI_int:{
         int *qar = MRI_INT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            if( qar[ii] == 0 ) continue ;
            if( qar[ii] > int_max ){ int_max = qar[ii]; imax = ii; }
         }
         result.i = imax; result.a = (float)int_max; RETURN(result);
      }

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            if( qar[ii] == 0.0f ) continue ;
            if( qar[ii] > float_max ){ float_max = qar[ii]; imax = ii; }
         }
         result.i = imax; result.a = float_max; RETURN(result);
      }

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            if( qar[ii] == 0.0 ) continue ;
            if( qar[ii] > double_max ){ double_max = qar[ii]; imax = ii; }
         }
         result.i = imax; result.a = (float)double_max; RETURN(result);
      }

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(im) ; float aval ;
         for( ii=0 ; ii < npix ; ii++ ){
            aval = CABS(qar[ii]) ;
            if( aval == 0.0f ) continue ;
            if( aval > float_max ){ float_max = aval; imax = ii; }
         }
         result.i = imax; result.a = float_max; RETURN(result);
      }

      case MRI_rgb:{
         byte *rgb = MRI_RGB_PTR(im) ;
         double val , top=0.0 ;
         for( ii=0 ; ii < npix ; ii++ ){  /* scale to brightness */
            val =  0.299 * rgb[3*ii]      /* between 0 and 255   */
                 + 0.587 * rgb[3*ii+1]
                 + 0.114 * rgb[3*ii+2] ;
            if( val != 0.0 && val > top ){ top = val; imax = ii; }
         }
         result.i = imax; result.a = (float)top; RETURN(result);
      }

   }
   RETURN(result);
}

/*---------------------------------------------------------------------------*/

intfloat mri_indmin_nz( MRI_IMAGE *im )
{
   register int ii , npix ;
   byte   byte_min   = 255 ;
   short  short_min  = 32767 ;       /* 23 Oct 1998: changed from 0 */
   int    int_min    = 2147483647 ;  /* ditto */
   float  float_min  = 1.e+38 ;      /* changed from -9999999.0 */
   double double_min = 1.e+38 ;      /* ditto */
   intfloat result   = { -1 , 0.0f } ; int imin=-1 ;

ENTRY("mri_indmin_nz") ;

   npix = im->nvox ;

   switch( im->kind ){
      default: break ;

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            if( qar[ii] == 0 ) continue ;
            if( qar[ii] < byte_min ){ byte_min = qar[ii]; imin = ii; }
         }
         result.i = imin; result.a = (float)byte_min; RETURN(result);
      }

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            if( qar[ii] == 0 ) continue ;
            if( qar[ii] < short_min ){ short_min = qar[ii]; imin = ii; }
         }
         result.i = imin; result.a = (float)short_min; RETURN(result);
      }

      case MRI_int:{
         int *qar = MRI_INT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            if( qar[ii] == 0 ) continue ;
            if( qar[ii] < int_min ){ int_min = qar[ii]; imin = ii; }
         }
         result.i = imin; result.a = (float)int_min; RETURN(result);
      }

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            if( qar[ii] == 0.0f ) continue ;
            if( qar[ii] < float_min ){ float_min = qar[ii]; imin = ii; }
         }
         result.i = imin; result.a = float_min; RETURN(result);
      }

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ ){
            if( qar[ii] == 0.0 ) continue ;
            if( qar[ii] < double_min ){ double_min = qar[ii]; imin = ii; }
         }
         result.i = imin; result.a = (float)double_min; RETURN(result);
      }

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(im) ; float aval ;
         for( ii=0 ; ii < npix ; ii++ ){
            aval = CABS(qar[ii]) ;
            if( aval == 0.0f ) continue ;
            if( aval < float_min ){ float_min = aval; imin = ii; }
         }
         result.i = imin; result.a = float_min; RETURN(result);
      }

      case MRI_rgb:{
         byte *rgb = MRI_RGB_PTR(im) ;
         double val , bot=255.0 ;
         for( ii=0 ; ii < npix ; ii++ ){  /* scale to brightness */
            val =  0.299 * rgb[3*ii]      /* between 0 and 255   */
                 + 0.587 * rgb[3*ii+1]
                 + 0.114 * rgb[3*ii+2] ;
            if( val != 0.0 && val < bot ){ bot = val; imin = ii; }
         }
         result.i = imin; result.a = (float)bot; RETURN(result);
      }

   }
   RETURN(result);
}
