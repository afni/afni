/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*** 7D SAFE ***/

/*********************************************************/
/*** Threshold image im IN PLACE based on image thrim. ***/
/*** Currently, thrim must be short or float.          ***/
/*********************************************************/

void mri_threshold( double thbot , double thtop , MRI_IMAGE * thrim , MRI_IMAGE * im )
{
   register int ii , npix ;

ENTRY("mri_threshold") ;

   if( thrim == NULL           || im == NULL ||
       thrim->nvox != im->nvox || thbot >= thtop ) EXRETURN ;

   npix = im->nvox ;

   switch( thrim->kind ){

      default: EXRETURN ;  /* don't know how to use this type of threshold image */

      case MRI_short:{                     /* threshold image is shorts */
         register short th1 , th2 ;
         register short * thar = MRI_SHORT_PTR(thrim) ;
         th1 = thbot ; th2 = thtop ;

         switch( im->kind ){

            default: EXRETURN ;  /* unknown type of data image */

            case MRI_byte:{
               register byte * ar = MRI_BYTE_PTR(im) ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0 ;
               EXRETURN ;
            }

            case MRI_short:{
               register short * ar = MRI_SHORT_PTR(im) ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0 ;
               EXRETURN ;
            }

            case MRI_int:{
               register int * ar = MRI_INT_PTR(im) ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0 ;
               EXRETURN ;
            }

            case MRI_float:{
               register float * ar = MRI_FLOAT_PTR(im) ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0.0 ;
               EXRETURN ;
            }

            case MRI_double:{
               register double * ar = MRI_DOUBLE_PTR(im) ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0.0 ;
               EXRETURN ;
            }

            case MRI_complex:{
               register complex * ar = MRI_COMPLEX_PTR(im) ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii].r = ar[ii].i = 0.0 ;
               EXRETURN ;
            }
         }
      } /* end of short thrim */

      case MRI_float:{                /* threshold image is floats */
         register float th1 , th2 ;
         register float * thar = MRI_FLOAT_PTR(thrim) ;
         th1 = thbot ; th2 = thtop ;

         switch( im->kind ){

            default: EXRETURN ;

            case MRI_byte:{
               register byte * ar = MRI_BYTE_PTR(im) ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0 ;
               EXRETURN ;
            }

            case MRI_short:{
               register short * ar = MRI_SHORT_PTR(im) ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0 ;
               EXRETURN ;
            }

            case MRI_int:{
               register int * ar = MRI_INT_PTR(im) ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0 ;
               EXRETURN ;
            }

            case MRI_float:{
               register float * ar = MRI_FLOAT_PTR(im) ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0.0 ;
               EXRETURN ;
            }

            case MRI_double:{
               register double * ar = MRI_DOUBLE_PTR(im) ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0.0 ;
               EXRETURN ;
            }

            case MRI_complex:{
               register complex * ar = MRI_COMPLEX_PTR(im) ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii].r = ar[ii].i = 0.0 ;
               EXRETURN ;
            }
         }
      } /* end of float thrim */

   }

   EXRETURN ;  /* should not be reached! */
}
