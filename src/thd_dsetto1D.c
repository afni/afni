#include "mrilib.h"
#include "thd.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------
  Routine to extract a time-series (fixed index, variable ival)
    from a dataset.
  ixyz = spatial index of desired voxel
       = ix + jy * n1 + kz * n1*n2
  raw  = 0 if you always want floats
       = 1 if you want the truly stored data type
-----------------------------------------------------------------*/

MRI_IMAGE * THD_extract_series( int ind , THD_3dim_dataset * dset , int raw )
{
   MRI_IMAGE * im ;  /* output */
   MRI_TYPE typ ;
   int nv , ival ;
   char * iar ;      /* brick in the input */

ENTRY("THD_extract_series") ;

   if( ind < 0 || dset == NULL ||
       ind >= dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz ) return NULL ;

   nv  = dset->dblk->nvals ;
   iar = DSET_ARRAY(dset,0) ;
   if( iar == NULL ){  /* if data needs to be loaded from disk */
      (void) THD_load_datablock( dset->dblk , NULL ) ;
      iar = DSET_ARRAY(dset,0) ;
      if( iar == NULL ) return NULL ;
   }
   typ = DSET_BRICK_TYPE(dset,0) ;
   im  = mri_new( nv , 1 , typ ) ;

   switch( typ ){

      default:             /* don't know what to do --> return nada */
         mri_free( im ) ;
         return NULL ;

      case MRI_byte:{
         byte * ar  = MRI_BYTE_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (byte *) DSET_ARRAY(dset,ival) ;
            ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_short:{
         short * ar  = MRI_SHORT_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (short *) DSET_ARRAY(dset,ival) ;
            ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_float:{
         float * ar  = MRI_FLOAT_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (float *) DSET_ARRAY(dset,ival) ;
            ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_int:{
         int * ar  = MRI_INT_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (int *) DSET_ARRAY(dset,ival) ;
            ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_double:{
         double * ar  = MRI_DOUBLE_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (double *) DSET_ARRAY(dset,ival) ;
            ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_complex:{
         complex * ar  = MRI_COMPLEX_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (complex *) DSET_ARRAY(dset,ival) ;
            ar[ival] = bar[ind] ;
         }
      }
      break ;

   }

   if( !raw && THD_need_brick_factor(dset) ){
      MRI_IMAGE * qim ;
      qim = mri_mult_to_float( dset->dblk->brick_fac , im ) ;
      mri_free(im) ; im = qim ;
   }

   if( !raw && im->kind != MRI_float ){
      MRI_IMAGE * qim ;
      qim = mri_to_float( im ) ;
      mri_free(im) ; im = qim ;
   }

   if( dset->taxis != NULL ){  /* 21 Oct 1996 */
      float zz , tt ;
      int kz = ind / ( dset->daxes->nxx * dset->daxes->nyy ) ;

      zz = dset->daxes->zzorg + kz * dset->daxes->zzdel ;
      tt = THD_timeof( 0 , zz , dset->taxis ) ;

      im->xo = tt ; im->dx = dset->taxis->ttdel ;   /* origin and delta */

      if( dset->taxis->units_type == UNITS_MSEC_TYPE ){ /* convert to sec */
         im->xo *= 0.001 ; im->dx *= 0.001 ;
      }
   } else {
      im->xo = 0.0 ; im->dx = 1.0 ;  /* 08 Nov 1996 */
   }

   return im ;
}
