/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

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
       ind >= dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz ) RETURN( NULL );

   nv  = dset->dblk->nvals ;
   iar = DSET_ARRAY(dset,0) ;
   if( iar == NULL ){  /* if data needs to be loaded from disk */
      (void) THD_load_datablock( dset->dblk ) ;
      iar = DSET_ARRAY(dset,0) ;
      if( iar == NULL ) RETURN( NULL );
   }
   typ = DSET_BRICK_TYPE(dset,0) ;
   im  = mri_new( nv , 1 , typ ) ;
   mri_zero_image(im) ;             /* 19 Oct 2001 */

   switch( typ ){

      default:             /* don't know what to do --> return nada */
         mri_free( im ) ;
         RETURN( NULL );

      case MRI_byte:{
         byte * ar  = MRI_BYTE_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (byte *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_short:{
         short * ar  = MRI_SHORT_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (short *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_float:{
         float * ar  = MRI_FLOAT_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (float *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_int:{
         int * ar  = MRI_INT_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (int *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_double:{
         double * ar  = MRI_DOUBLE_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (double *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_complex:{
         complex * ar  = MRI_COMPLEX_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (complex *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
      }
      break ;

   }

   if( !raw && THD_need_brick_factor(dset) ){
      MRI_IMAGE * qim ;
      qim = mri_mult_to_float( dset->dblk->brick_fac , im ) ;
      mri_free(im) ; im = qim ;
   }

#if 0
#  define DDD(x) fprintf(stderr,x)
   { static int nc=0 ; fprintf(stderr,"THD_extract_series %d:",++nc) ; }
#else
#  define DDD(x) /* nada */
#endif

DDD("a") ;

   if( !raw && im->kind != MRI_float ){
      MRI_IMAGE * qim ;
DDD("b") ;
      qim = mri_to_float( im ) ;
DDD("x") ;
      mri_free(im) ; im = qim ;
DDD("c") ;
   }
DDD("d") ;

   if( dset->taxis != NULL ){  /* 21 Oct 1996 */
      float zz , tt ;
      int kz = ind / ( dset->daxes->nxx * dset->daxes->nyy ) ;

DDD("e") ;
      zz = dset->daxes->zzorg + kz * dset->daxes->zzdel ;
      tt = THD_timeof( 0 , zz , dset->taxis ) ;
DDD("f") ;

      im->xo = tt ; im->dx = dset->taxis->ttdel ;   /* origin and delta */

      if( dset->taxis->units_type == UNITS_MSEC_TYPE ){ /* convert to sec */
         im->xo *= 0.001 ; im->dx *= 0.001 ;
      }
   } else {
      im->xo = 0.0 ; im->dx = 1.0 ;  /* 08 Nov 1996 */
   }
DDD("\n");

   RETURN( im );
}

/*----------------------------------------------------------------------------
   04 Feb 2000: do a bunch of timeseries at once (for efficiency)
------------------------------------------------------------------------------*/

MRI_IMARR * THD_extract_many_series( int ns, int * ind, THD_3dim_dataset * dset )
{
   MRI_IMARR * imar ; /* output */
   MRI_IMAGE * im ;
   MRI_TYPE typ ;
   int nv , ival , kk ;
   char * iar ;      /* brick in the input */

ENTRY("THD_extract_many_series") ;

   if( ns <= 0 || ind == NULL | dset == NULL ) RETURN( NULL );

   /* try to load dataset */

   nv  = dset->dblk->nvals ;
   iar = DSET_ARRAY(dset,0) ;
   if( iar == NULL ){  /* if data needs to be loaded from disk */
      (void) THD_load_datablock( dset->dblk ) ;
      iar = DSET_ARRAY(dset,0) ;
      if( iar == NULL ) RETURN( NULL );
   }
   typ = DSET_BRICK_TYPE(dset,0) ;

   /* create output */

   INIT_IMARR(imar) ;
   for( kk=0 ; kk < ns ; kk++ ){
      im = mri_new( nv , 1 , typ ) ;
      mri_zero_image(im) ;              /* 19 Oct 2001 */
      ADDTO_IMARR(imar,im) ;
   }

   /* fill the output */

   switch( typ ){

      default:             /* don't know what to do --> return nada */
         DESTROY_IMARR(imar) ;
         RETURN( NULL );

      case MRI_byte:{
         byte * ar , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (byte *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ){
              for( kk=0 ; kk < ns ; kk++ ){
                 ar = MRI_BYTE_PTR( IMARR_SUBIMAGE(imar,kk) ) ;
                 ar[ival] = bar[ind[kk]] ;
              }
            }
         }
      }
      break ;

      case MRI_short:{
         short * ar , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (short *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ){
              for( kk=0 ; kk < ns ; kk++ ){
                 ar = MRI_SHORT_PTR( IMARR_SUBIMAGE(imar,kk) ) ;
                 ar[ival] = bar[ind[kk]] ;
              }
            }
         }
      }
      break ;

      case MRI_float:{
         float * ar , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (float *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ){
              for( kk=0 ; kk < ns ; kk++ ){
                 ar = MRI_FLOAT_PTR( IMARR_SUBIMAGE(imar,kk) ) ;
                 ar[ival] = bar[ind[kk]] ;
              }
            }
         }
      }
      break ;

      case MRI_int:{
         int * ar , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (int *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ){
              for( kk=0 ; kk < ns ; kk++ ){
                 ar = MRI_INT_PTR( IMARR_SUBIMAGE(imar,kk) ) ;
                 ar[ival] = bar[ind[kk]] ;
              }
            }
         }
      }
      break ;

      case MRI_double:{
         double * ar , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (double *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ){
              for( kk=0 ; kk < ns ; kk++ ){
                 ar = MRI_DOUBLE_PTR( IMARR_SUBIMAGE(imar,kk) ) ;
                 ar[ival] = bar[ind[kk]] ;
              }
            }
         }
      }
      break ;

      case MRI_complex:{
         complex * ar , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (complex *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ){
              for( kk=0 ; kk < ns ; kk++ ){
                 ar = MRI_COMPLEX_PTR( IMARR_SUBIMAGE(imar,kk) ) ;
                 ar[ival] = bar[ind[kk]] ;
              }
            }
         }
      }
      break ;

   }

   /* scale outputs, if needed */

   if( THD_need_brick_factor(dset) ){
      MRI_IMAGE * qim ;
      for( kk=0 ; kk < ns ; kk++ ){
         im  = IMARR_SUBIMAGE(imar,kk) ;
         qim = mri_mult_to_float( dset->dblk->brick_fac , im ) ;
         mri_free(im) ;
         IMARR_SUBIMAGE(imar,kk) = qim ;
      }
   }

   /* convert to floats, if needed */

   if( IMARR_SUBIMAGE(imar,0)->kind != MRI_float ){
      MRI_IMAGE * qim ;
      for( kk=0 ; kk < ns ; kk++ ){
         im  = IMARR_SUBIMAGE(imar,kk) ;
         qim = mri_to_float( im ) ;
         mri_free(im) ;
         IMARR_SUBIMAGE(imar,kk) = qim ;
      }
   }

   /* add time axis stuff to output images, if present */

   if( dset->taxis != NULL ){
      float zz , tt ;
      int kz ;

      for( kk=0 ; kk < ns ; kk++ ){
         kz = ind[kk] / ( dset->daxes->nxx * dset->daxes->nyy ) ;
         zz = dset->daxes->zzorg + kz * dset->daxes->zzdel ;
         tt = THD_timeof( 0 , zz , dset->taxis ) ;
         im = IMARR_SUBIMAGE(imar,kk) ;
         im->xo = tt ; im->dx = dset->taxis->ttdel ;   /* origin and delta */
         if( dset->taxis->units_type == UNITS_MSEC_TYPE ){ /* convert to sec */
            im->xo *= 0.001 ; im->dx *= 0.001 ;
         }
      }
   } else {
      for( kk=0 ; kk < ns ; kk++ ){
         im = IMARR_SUBIMAGE(imar,kk) ;
         im->xo = 0.0 ; im->dx = 1.0 ;
      }
   }

   RETURN( imar );
}
