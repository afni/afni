/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** 7D SAFE ***/

void mri_histogram( MRI_IMAGE * im , float hbot,float htop ,
                                     int initialize , int nbin, int hist[] )
{
   register int ih , npix , ii ;
   register float sbin ;
   MRI_IMAGE * lim ;

   if( im == NULL || htop <= hbot || nbin < 2 ) return ;

   /* can handle shorts and floats;  all else -> convert to float */

   switch( im->kind ){

      default:        lim = mri_to_float(im) ; break ;

      case MRI_byte:  lim = mri_to_short(1.0,im) ; break ;

      case MRI_short:
      case MRI_float: lim = im ; break ;
   }

   npix = lim->nvox ;
   sbin = 0.999999 * nbin / (htop-hbot) ;

   if( initialize ) for( ih=0 ; ih < nbin ; ih++ ) hist[ih] = 0 ;

   switch( lim->kind ){

      case MRI_short:{
         register short * shar = mri_data_pointer(lim) ;

         for( ii=0 ; ii < npix ; ii++ ){
            ih = sbin * (shar[ii]-hbot) ;
            if( ih >=0 && ih < nbin ) hist[ih]++ ;
         }
      }
      break ;

      case MRI_float:{
         register float * flar = mri_data_pointer(lim) ;

         for( ii=0 ; ii < npix ; ii++ ){
            ih = sbin * (flar[ii]-hbot) ;
            if( ih >=0 && ih < nbin ) hist[ih]++ ;
         }
      }
      break ;
   }

   if( lim != im ) mri_free( lim ) ;  /* toss temporary array */
   return ;
}
