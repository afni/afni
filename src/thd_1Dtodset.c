/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*---------------------------------------------------------------
  Routine to send a time-series (fixed index, variable ival)
  into a dataset - inverse function to THD_extract_series.
     ixyz = spatial index of desired voxel
          = ix + jy * n1 + kz * n1*n2
     npt  = # of points in array
     typ  = type of data in array [MRI_byte, MRI_short, MRI_float, MRI_complex]
     dar  = data array
     raw  = 0 if input is to be scaled, 1 if it is not

  -- RWCox - 29 Oct 1999
-----------------------------------------------------------------*/

void THD_insert_series( int ind , THD_3dim_dataset * dset ,
                        int npt , int typ , void * dar , int raw )
{
   int nv , ival , dtyp , ii ;
   float * far=NULL , * fac ;

   if( ind < 0                ||
       npt <= 0               ||
       dar == NULL            ||
       !ISVALID_DSET(dset)    ||
       ind >= DSET_NVOX(dset) ||
       !DSET_IS_MALLOC(dset)    ) return ;


   nv  = DSET_NVALS(dset) ;  if( npt > nv ) npt = nv ;  /* truncate input? */

   if( !DSET_LOADED(dset) ) DSET_load(dset) ;           /* read from disk? */
   if( !DSET_LOADED(dset) ) return ;

   dtyp = DSET_BRICK_TYPE(dset,0) ;                     /* type of dataset arrays */

   /* convert input to a floating point type */

   if( !raw && THD_need_brick_factor(dset) )
      fac = dset->dblk->brick_fac ;
   else
      raw = 1 ;

#define FAC(q) ( (fac[q] != 0.0) ? 1.0/fac[q] : 1.0 )

   if( dtyp == MRI_complex ){                             /* complex output ?! */

      complex * car = (complex *) malloc( sizeof(complex) * npt ) ;
      complex * bar ;

      switch( typ ){
         default:
            free(car) ; return ;   /* bad input */

         case MRI_complex:
            memcpy( car , dar , sizeof(complex)*npt ) ;
         break ;

         case MRI_float:{
            float * a = (float *) dar ;
            for( ii=0 ; ii < npt ; ii++ ){ car[ii].r = a[ii] ; car[ii].i = 0.0 ; }
         }
         break ;

         case MRI_short:{
            short * a = (short *) dar ;
            for( ii=0 ; ii < npt ; ii++ ){ car[ii].r = a[ii] ; car[ii].i = 0.0 ; }
         }
         break ;

         case MRI_byte:{
            byte * a = (byte *) dar ;
            for( ii=0 ; ii < npt ; ii++ ){ car[ii].r = a[ii] ; car[ii].i = 0.0 ; }
         }
         break ;
      }

      /* can now copy car into dataset, and exit */

      if( !raw )
         for( ii=0 ; ii < npt ; ii++ ){ car[ii].r *= FAC(ii) ; car[ii].i *= FAC(ii) ; }

      for( ii=0 ; ii < npt ; ii++ ){
         bar = (complex *) DSET_ARRAY(dset,ii) ;
         bar[ind] = car[ii] ;
      }

      free(car) ; return ;
   }

   /* otherwise, compute a temporary float array */

   far = (float *) malloc( sizeof(float) * npt ) ;
   switch( typ ){
      default:
         free(far) ; return ;   /* bad input */

      case MRI_complex:{
         complex * a = (complex *) dar ;
         for( ii=0 ; ii < npt ; ii++ ) far[ii] = CABS(a[ii]) ;
      }
      break ;

      case MRI_float:
         memcpy( far , dar , sizeof(float)*npt ) ;
      break ;

      case MRI_short:{
         short * a = (short *) dar ;
         for( ii=0 ; ii < npt ; ii++ ) far[ii] = a[ii] ;
      }
      break ;

      case MRI_byte:{
         byte * a = (byte *) dar ;
         for( ii=0 ; ii < npt ; ii++ ) far[ii] = a[ii] ;
      }
      break ;
   }

   if( !raw ) for( ii=0 ; ii < npt ; ii++ ) far[ii] *= FAC(ii) ;

   /* now convert this to the output */

   switch( dtyp ){

      default:
         free(far) ; return ;   /* bad dataset? */

      case MRI_float:{
         float * bar ;
         for( ii=0 ; ii < npt ; ii++ ){
            bar = (float *) DSET_ARRAY(dset,ii) ;
            bar[ind] = far[ii] ;
         }
      }
      break ;

      case MRI_short:{
         short * bar ;
         for( ii=0 ; ii < npt ; ii++ ){
            bar = (short *) DSET_ARRAY(dset,ii) ;
            bar[ind] = SHORTIZE(far[ii]) ;
         }
      }
      break ;

      case MRI_byte:{
         byte * bar ;
         for( ii=0 ; ii < npt ; ii++ ){
            bar = (byte *) DSET_ARRAY(dset,ii) ;
            bar[ind] = BYTEIZE(far[ii]) ;
         }
      }
      break ;
   }

   free(far) ; return ;
}
