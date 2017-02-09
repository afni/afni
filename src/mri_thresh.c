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

void mri_threshold( double thbot, double thtop, MRI_IMAGE *thrim, MRI_IMAGE *im )
{
   register int ii , npix ;

ENTRY("mri_threshold") ;

   if( thrim == NULL           || im == NULL ||
       thrim->nvox != im->nvox || thbot >= thtop ) EXRETURN ;

   npix = im->nvox ;

   switch( thrim->kind ){

      default:{                                  /* stoopid, but works */
        MRI_IMAGE *qim = mri_to_float(thrim) ;
        mri_threshold( thbot,thtop , qim , im ) ;
        mri_free(qim) ;
        EXRETURN ;
      }

      case MRI_byte:{      /* 20 Dec 2004: very stupid way to do bytes */
        MRI_IMAGE *qim = mri_to_short(1.0,thrim) ;
        mri_threshold( thbot,thtop , qim , im ) ;
        mri_free(qim) ;
        EXRETURN ;
      }

      case MRI_short:{                     /* threshold image is shorts */
         register float th1 , th2 ;
         register short *thar = MRI_SHORT_PTR(thrim) ;
         th1 = THRESH_SHORTIZE(thbot) ; th2 = THRESH_SHORTIZE(thtop) ;

         if( thar == NULL ) EXRETURN ;

         switch( im->kind ){

            default: EXRETURN ;  /* unknown type of data image */

            case MRI_byte:{
               register byte *ar = MRI_BYTE_PTR(im) ;
               if( ar == NULL ) EXRETURN ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0 ;
               EXRETURN ;
            }

            case MRI_rgb:{                             /* 20 Dec 2004 */
               register byte *ar = MRI_RGB_PTR(im) ;
               if( ar == NULL ) EXRETURN ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ){
                    ar[3*ii] = ar[3*ii+1] = ar[3*ii+2] = 0 ;
                  }
               EXRETURN ;
            }

            case MRI_short:{
               register short *ar = MRI_SHORT_PTR(im) ;
               if( ar == NULL ) EXRETURN ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0 ;
               EXRETURN ;
            }

            case MRI_int:{
               register int *ar = MRI_INT_PTR(im) ;
               if( ar == NULL ) EXRETURN ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0 ;
               EXRETURN ;
            }

            case MRI_float:{
               register float *ar = MRI_FLOAT_PTR(im) ;
               if( ar == NULL ) EXRETURN ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0.0f ;
               EXRETURN ;
            }

            case MRI_double:{
               register double *ar = MRI_DOUBLE_PTR(im) ;
               if( ar == NULL ) EXRETURN ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0.0 ;
               EXRETURN ;
            }

            case MRI_complex:{
               register complex *ar = MRI_COMPLEX_PTR(im) ;
               if( ar == NULL ) EXRETURN ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii].r = ar[ii].i = 0.0f ;
               EXRETURN ;
            }
         }
      } /* end of short thrim */

      case MRI_float:{                /* threshold image is floats */
         register float th1 , th2 ;
         register float *thar = MRI_FLOAT_PTR(thrim) ;
         th1 = thbot ; th2 = thtop ;

         if( thar == NULL ) EXRETURN ;

         switch( im->kind ){

            default: EXRETURN ;

            case MRI_byte:{
               register byte *ar = MRI_BYTE_PTR(im) ;
               if( ar == NULL ) EXRETURN ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0 ;
               EXRETURN ;
            }

            case MRI_rgb:{                             /* 20 Dec 2004 */
               register byte *ar = MRI_RGB_PTR(im) ;
               if( ar == NULL ) EXRETURN ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ){
                    ar[3*ii] = ar[3*ii+1] = ar[3*ii+2] = 0 ;
                  }
               EXRETURN ;
            }

            case MRI_short:{
               register short *ar = MRI_SHORT_PTR(im) ;
               if( ar == NULL ) EXRETURN ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0 ;
               EXRETURN ;
            }

            case MRI_int:{
               register int *ar = MRI_INT_PTR(im) ;
               if( ar == NULL ) EXRETURN ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0 ;
               EXRETURN ;
            }

            case MRI_float:{
               register float *ar = MRI_FLOAT_PTR(im) ;
               if( ar == NULL ) EXRETURN ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0.0f ;
               EXRETURN ;
            }

            case MRI_double:{
               register double *ar = MRI_DOUBLE_PTR(im) ;
               if( ar == NULL ) EXRETURN ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii] = 0.0 ;
               EXRETURN ;
            }

            case MRI_complex:{
               register complex *ar = MRI_COMPLEX_PTR(im) ;
               if( ar == NULL ) EXRETURN ;
               for( ii=0 ; ii < npix ; ii++ )
                  if( thar[ii] > th1 && thar[ii] < th2 ) ar[ii].r = ar[ii].i = 0.0f ;
               EXRETURN ;
            }
         }
      } /* end of float thrim */

   }

   EXRETURN ;  /* should not be reached! */
}

/*----------------------------------------------------------------------------*/

void mri_maskify( MRI_IMAGE *im , byte *mask )  /* Jul 2010 */
{
   register int npix ,ii ;

ENTRY("mri_maskify") ;

   if( im == NULL || mask == NULL ) EXRETURN ;

   npix = im->nvox ;

   switch( im->kind ){

      default: break ;

      case MRI_byte:{
        register byte *ar = MRI_BYTE_PTR(im) ;
        for( ii=0 ; ii < npix ; ii++ ) if( mask[ii] == 0 ) ar[ii] = 0 ;
      }
      break ;

      case MRI_rgb:{
        register byte *ar = MRI_RGB_PTR(im) ;
        for( ii=0 ; ii < npix ; ii++ ) if( mask[ii] == 0 ) ar[3*ii] = ar[3*ii+1] = ar[3*ii+2] = 0 ;
      }
      break ;

      case MRI_short:{
        register short *ar = MRI_SHORT_PTR(im) ;
        for( ii=0 ; ii < npix ; ii++ ) if( mask[ii] == 0 ) ar[ii] = 0 ;
      }
      break ;

      case MRI_int:{
        register int *ar = MRI_INT_PTR(im) ;
        for( ii=0 ; ii < npix ; ii++ ) if( mask[ii] == 0 ) ar[ii] = 0 ;
      }
      break ;

      case MRI_float:{
        register float *ar = MRI_FLOAT_PTR(im) ;
        for( ii=0 ; ii < npix ; ii++ ) if( mask[ii] == 0 ) ar[ii] = 0.0f ;
      }
      break ;

      case MRI_double:{
        register double *ar = MRI_DOUBLE_PTR(im) ;
        for( ii=0 ; ii < npix ; ii++ ) if( mask[ii] == 0 ) ar[ii] = 0.0 ;
      }
      break ;

      case MRI_complex:{
        register complex *ar = MRI_COMPLEX_PTR(im) ;
        for( ii=0 ; ii < npix ; ii++ ) if( mask[ii] == 0 ) ar[ii].r = ar[ii].i = 0.0f ;
      }
      break ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Return the min and max of the thresholded image [12 Jun 2014] */

static int_pair mm_ijk = {-666,-777} ;

int_pair mri_threshold_minmax_indexes(void){ return mm_ijk ; }  /* and locations */

float_pair mri_threshold_minmax( double thbot, double thtop, MRI_IMAGE *thrim, MRI_IMAGE *im )
{
   int ii , npix , min_ijk=-666,max_ijk=-777 ;
   float bot=1.e38 , top=-1.e38 ;
   float_pair result = {666.0f,-666.0f} ;

ENTRY("mri_threshold_minmax") ;

   mm_ijk.i = -666 ; mm_ijk.j = -777 ;

   if( thrim == NULL           || im == NULL ||
       thrim->nvox != im->nvox || thbot >= thtop ) RETURN(result) ;

   npix = im->nvox ;

   switch( thrim->kind ){  /* various kinds of data in threshold */

      default:{                                  /* stoopid, but works */
        MRI_IMAGE *qim = mri_to_float(thrim) ;
        result = mri_threshold_minmax( thbot,thtop , qim , im ) ;
        mri_free(qim) ;
        RETURN(result) ;
      }

      case MRI_byte:{      /* 20 Dec 2004: very stupid way to do bytes */
        MRI_IMAGE *qim = mri_to_short(1.0,thrim) ;
        result = mri_threshold_minmax( thbot,thtop , qim , im ) ;
        mri_free(qim) ;
        RETURN(result) ;
      }

      case MRI_short:{                     /* threshold image is shorts */
         register float th1 , th2 ;
         register short *thar = MRI_SHORT_PTR(thrim) ;
         th1 = THRESH_SHORTIZE(thbot) ; th2 = THRESH_SHORTIZE(thtop) ;

         if( thar == NULL ) RETURN(result) ;

         switch( im->kind ){           /* various kinds of data in image */

            default: RETURN(result) ;  /* unknown type of data image */

            case MRI_byte:{
               register byte *ar = MRI_BYTE_PTR(im) ;
               if( ar == NULL ) RETURN(result) ;
               for( ii=0 ; ii < npix ; ii++ ){
                  if( thar[ii] > th1 && thar[ii] < th2 ) continue ;
                  if( ar[ii] < bot ){ bot = ar[ii] ; min_ijk = ii ; }
                  if( ar[ii] > top ){ top = ar[ii] ; max_ijk = ii ; }
               }
               if( bot <= top ){ result.a = bot; result.b = top; mm_ijk.i = min_ijk; mm_ijk.j = max_ijk; }
               RETURN(result) ;
            }

            case MRI_rgb:{                             /* 20 Dec 2004 */
               register byte *ar = MRI_RGB_PTR(im) ; float aval ;
               if( ar == NULL ) RETURN(result) ;
               for( ii=0 ; ii < npix ; ii++ ){
                  if( thar[ii] > th1 && thar[ii] < th2 ) continue ;
                  aval = 0.299f*ar[3*ii] + 0.587f*ar[3*ii+1] + 0.114f*ar[3*ii+2] ;
                  if( aval < bot ){ bot = aval ; min_ijk = ii ; }
                  if( aval > top ){ top = aval ; max_ijk = ii ; }
               }
               if( bot <= top ){ result.a = bot; result.b = top; mm_ijk.i = min_ijk; mm_ijk.j = max_ijk; }
               RETURN(result) ;
            }

            case MRI_short:{
               register short *ar = MRI_SHORT_PTR(im) ;
               if( ar == NULL ) RETURN(result) ;
               for( ii=0 ; ii < npix ; ii++ ){
                  if( thar[ii] > th1 && thar[ii] < th2 ) continue ;
                  if( ar[ii] < bot ){ bot = ar[ii] ; min_ijk = ii ; }
                  if( ar[ii] > top ){ top = ar[ii] ; max_ijk = ii ; }
               }
               if( bot <= top ){ result.a = bot; result.b = top; mm_ijk.i = min_ijk; mm_ijk.j = max_ijk; }
               RETURN(result) ;
            }

            case MRI_int:{
               register int *ar = MRI_INT_PTR(im) ;
               if( ar == NULL ) RETURN(result) ;
               for( ii=0 ; ii < npix ; ii++ ){
                  if( thar[ii] > th1 && thar[ii] < th2 ) continue ;
                  if( ar[ii] < bot ){ bot = ar[ii] ; min_ijk = ii ; }
                  if( ar[ii] > top ){ top = ar[ii] ; max_ijk = ii ; }
               }
               if( bot <= top ){ result.a = bot; result.b = top; mm_ijk.i = min_ijk; mm_ijk.j = max_ijk; }
               RETURN(result) ;
            }

            case MRI_float:{
               register float *ar = MRI_FLOAT_PTR(im) ;
               if( ar == NULL ) RETURN(result) ;
               for( ii=0 ; ii < npix ; ii++ ){
                  if( thar[ii] > th1 && thar[ii] < th2 ) continue ;
                  if( ar[ii] < bot ){ bot = ar[ii] ; min_ijk = ii ; }
                  if( ar[ii] > top ){ top = ar[ii] ; max_ijk = ii ; }
               }
               if( bot <= top ){ result.a = bot; result.b = top; mm_ijk.i = min_ijk; mm_ijk.j = max_ijk; }
               RETURN(result) ;
            }

            case MRI_double:{
               register double *ar = MRI_DOUBLE_PTR(im) ;
               if( ar == NULL ) RETURN(result) ;
               for( ii=0 ; ii < npix ; ii++ ){
                  if( thar[ii] > th1 && thar[ii] < th2 ) continue ;
                  if( ar[ii] < bot ){ bot = ar[ii] ; min_ijk = ii ; }
                  if( ar[ii] > top ){ top = ar[ii] ; max_ijk = ii ; }
               }
               if( bot <= top ){ result.a = bot; result.b = top; mm_ijk.i = min_ijk; mm_ijk.j = max_ijk; }
               RETURN(result) ;
            }

            case MRI_complex:{
               register complex *ar = MRI_COMPLEX_PTR(im) ; float aval ;
               if( ar == NULL ) RETURN(result) ;
               for( ii=0 ; ii < npix ; ii++ ){
                  if( thar[ii] > th1 && thar[ii] < th2 ) continue ;
                  aval = CABS(ar[ii]) ;
                  if( aval < bot ){ bot = aval ; min_ijk = ii ; }
                  if( aval > top ){ top = aval; max_ijk = ii ; }
               }
               if( bot <= top ){ result.a = bot; result.b = top; mm_ijk.i = min_ijk; mm_ijk.j = max_ijk; }
               RETURN(result) ;
            }

         }
      } /* end of short thrim */

      case MRI_float:{                /* threshold image is floats */
         register float th1 , th2 ;
         register float *thar = MRI_FLOAT_PTR(thrim) ;
         th1 = thbot ; th2 = thtop ;

         if( thar == NULL ) RETURN(result) ;

         switch( im->kind ){

            default: RETURN(result) ;

            case MRI_byte:{
               register byte *ar = MRI_BYTE_PTR(im) ;
               if( ar == NULL ) RETURN(result) ;
               for( ii=0 ; ii < npix ; ii++ ){
                  if( thar[ii] > th1 && thar[ii] < th2 ) continue ;
                  if( ar[ii] < bot ){ bot = ar[ii] ; min_ijk = ii ; }
                  if( ar[ii] > top ){ top = ar[ii] ; max_ijk = ii ; }
               }
               if( bot <= top ){ result.a = bot; result.b = top; mm_ijk.i = min_ijk; mm_ijk.j = max_ijk; }
               RETURN(result) ;
            }

            case MRI_rgb:{                             /* 20 Dec 2004 */
               register byte *ar = MRI_RGB_PTR(im) ; float aval ;
               if( ar == NULL ) RETURN(result) ;
               for( ii=0 ; ii < npix ; ii++ ){
                  if( thar[ii] > th1 && thar[ii] < th2 ) continue ;
                  aval = 0.299f*ar[3*ii] + 0.587f*ar[3*ii+1] + 0.114f*ar[3*ii+2] ;
                  if( aval < bot ){ bot = aval ; min_ijk = ii ; }
                  if( aval > top ){ top = aval ; max_ijk = ii ; }
               }
               if( bot <= top ){ result.a = bot; result.b = top; mm_ijk.i = min_ijk; mm_ijk.j = max_ijk; }
               RETURN(result) ;
            }

            case MRI_short:{
               register short *ar = MRI_SHORT_PTR(im) ;
               if( ar == NULL ) RETURN(result) ;
               for( ii=0 ; ii < npix ; ii++ ){
                  if( thar[ii] > th1 && thar[ii] < th2 ) continue ;
                  if( ar[ii] < bot ){ bot = ar[ii] ; min_ijk = ii ; }
                  if( ar[ii] > top ){ top = ar[ii] ; max_ijk = ii ; }
               }
               if( bot <= top ){ result.a = bot; result.b = top; mm_ijk.i = min_ijk; mm_ijk.j = max_ijk; }
               RETURN(result) ;
            }

            case MRI_int:{
               register int *ar = MRI_INT_PTR(im) ;
               if( ar == NULL ) RETURN(result) ;
               for( ii=0 ; ii < npix ; ii++ ){
                  if( thar[ii] > th1 && thar[ii] < th2 ) continue ;
                  if( ar[ii] < bot ){ bot = ar[ii] ; min_ijk = ii ; }
                  if( ar[ii] > top ){ top = ar[ii] ; max_ijk = ii ; }
               }
               if( bot <= top ){ result.a = bot; result.b = top; mm_ijk.i = min_ijk; mm_ijk.j = max_ijk; }
               RETURN(result) ;
            }

            case MRI_float:{
               register float *ar = MRI_FLOAT_PTR(im) ;
               if( ar == NULL ) RETURN(result) ;
               for( ii=0 ; ii < npix ; ii++ ){
                  if( thar[ii] > th1 && thar[ii] < th2 ) continue ;
                  if( ar[ii] < bot ){ bot = ar[ii] ; min_ijk = ii ; }
                  if( ar[ii] > top ){ top = ar[ii] ; max_ijk = ii ; }
               }
               if( bot <= top ){ result.a = bot; result.b = top; mm_ijk.i = min_ijk; mm_ijk.j = max_ijk; }
               RETURN(result) ;
            }

            case MRI_double:{
               register double *ar = MRI_DOUBLE_PTR(im) ;
               if( ar == NULL ) RETURN(result) ;
               for( ii=0 ; ii < npix ; ii++ ){
                  if( thar[ii] > th1 && thar[ii] < th2 ) continue ;
                  if( ar[ii] < bot ){ bot = ar[ii] ; min_ijk = ii ; }
                  if( ar[ii] > top ){ top = ar[ii] ; max_ijk = ii ; }
               }
               if( bot <= top ){ result.a = bot; result.b = top; mm_ijk.i = min_ijk; mm_ijk.j = max_ijk; }
               RETURN(result) ;
            }

            case MRI_complex:{
               register complex *ar = MRI_COMPLEX_PTR(im) ; float aval ;
               if( ar == NULL ) RETURN(result) ;
               for( ii=0 ; ii < npix ; ii++ ){
                  if( thar[ii] > th1 && thar[ii] < th2 ) continue ;
                  aval = CABS(ar[ii]) ;
                  if( aval < bot ){ bot = aval ; min_ijk = ii ; }
                  if( aval > top ){ top = aval; max_ijk = ii ; }
               }
               if( bot <= top ){ result.a = bot; result.b = top; mm_ijk.i = min_ijk; mm_ijk.j = max_ijk; }
               RETURN(result) ;
            }

         }
      } /* end of float thrim */

   }

   RETURN(result) ;  /* should not be reached! */
}
