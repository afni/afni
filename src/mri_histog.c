/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** 7D SAFE ***/

/* - hist[] should be declared as having nbin elements
   - if initialize != 0, sets hist to all zeros,
     otherwise accumulates into pre-existing histogram counts
   - values outside the range hbot..htop are not counted
*/

void mri_histogram( MRI_IMAGE *im , float hbot,float htop ,
                                    int initialize , int nbin, int hist[] )
{
   register int ih , npix , ii ;
   register float sbin ;
   MRI_IMAGE *lim ;

ENTRY("mri_histogram") ;

   if( im == NULL || htop <= hbot || nbin < 2 ) EXRETURN ;

   /* can handle shorts and floats;  all else -> convert datum */

   switch( im->kind ){

      default:
STATUS("convert to floats") ;
      lim = mri_to_float(im)     ; break ;

      case MRI_byte:
STATUS("convert to shorts") ;
      lim = mri_to_short(1.0,im) ; break ;

      case MRI_short:
      case MRI_float:
STATUS("keep original") ;
      lim = im                   ; break ;
   }

   npix = lim->nvox ;
   sbin = 0.999995f * nbin / (htop-hbot) ;

if( PRINT_TRACING ){
  char str[256]; sprintf(str,"hbot=%f htop=%f nbin=%d sbin=%f",hbot,htop,nbin,sbin); STATUS(str);
}

   if( initialize ) for( ih=0 ; ih < nbin ; ih++ ) hist[ih] = 0 ;

   switch( lim->kind ){

     case MRI_short:{
       register short *shar=mri_data_pointer(lim) , val ;

STATUS("processing shorts") ;
       for( ii=0 ; ii < npix ; ii++ ){
         val = shar[ii] ; if( val < hbot || val > htop ) continue ;
         ih = (int)(sbin*(val-hbot)) ; hist[ih]++ ;
       }
     }
     break ;

     case MRI_float:{
       register float *flar=mri_data_pointer(lim) , val ;

STATUS("processing floats") ;
       for( ii=0 ; ii < npix ; ii++ ){
         val = flar[ii] ; if( val < hbot || val > htop ) continue ;
         ih = (int)(sbin*(val-hbot)) ; hist[ih]++ ;
       }
     }
     break ;
   }

   if( lim != im ) mri_free(lim) ;  /* toss temporary array */
   EXRETURN ;
}
