/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*---------------------------------------------------------------
  Routine to extract a time-series (fixed index, variable ival)
    from a previously set up FD_brick structure.
  ixyz = spatial index of desired voxel (in brick coordinates)
       = ix + jy * n1 + kz * n1*n2
  Return value is an image of the type of the dataset (assumed
  uniform).
-----------------------------------------------------------------*/

MRI_IMAGE * FD_brick_to_series( int ixyz , FD_brick * br )
{
   MRI_IMAGE * im ;  /* output */
   int nv , ival ;
   char * iar ;      /* brick in the input */
   MRI_TYPE typ ;
   int ix,jy,kz , ind ;
   THD_ivec3 ind_fd , ind_ds ;

   if( ixyz < 0 || ixyz >= br->n1 * br->n2 * br->n3 ) return NULL ;

   /** otherwise, get ready for a real image **/

   ix  = ixyz % br->n1 ;
   jy  = ( ixyz % (br->n1 * br->n2) ) / br->n1 ;
   kz  = ixyz / (br->n1 * br->n2) ;
   LOAD_IVEC3( ind_fd , ix,jy,kz ) ; ind_ds = THD_fdind_to_3dind( br , ind_fd ) ;
   ix  = ind_ds.ijk[0] ;
   jy  = ind_ds.ijk[1] ;
   kz  = ind_ds.ijk[2] ;
   ind = (kz * br->dset->daxes->nyy + jy) * br->dset->daxes->nxx + ix ;

   nv = br->dset->dblk->nvals ;

   iar = DSET_ARRAY(br->dset,0) ;
   if( iar == NULL ){  /* if data needs to be loaded from disk */
      (void) THD_load_datablock( br->dset->dblk ) ;
      iar = DSET_ARRAY(br->dset,0) ;
      if( iar == NULL ) return NULL ;
   }
   typ = DSET_BRICK_TYPE(br->dset,0) ;
   im  = mri_new( nv , 1 , typ ) ;
#if 0
   mri_zero_image(im) ;             /* 18 Oct 2001 */
#endif

   switch( typ ){

      default:             /* don't know what to do --> return nada */
         mri_free( im ) ;
         return NULL ;

      case MRI_byte:{
         byte * ar  = MRI_BYTE_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (byte *) DSET_ARRAY(br->dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_short:{
         short * ar  = MRI_SHORT_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (short *) DSET_ARRAY(br->dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_float:{
         float * ar  = MRI_FLOAT_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (float *) DSET_ARRAY(br->dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_int:{
         int * ar  = MRI_INT_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (int *) DSET_ARRAY(br->dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_double:{
         double * ar  = MRI_DOUBLE_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (double *) DSET_ARRAY(br->dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_complex:{
         complex * ar  = MRI_COMPLEX_PTR(im) , * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (complex *) DSET_ARRAY(br->dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
      }
      break ;

      /* 15 Apr 2002: RGB types */

      case MRI_rgb:{
         rgbyte *ar  = (rgbyte *) MRI_RGB_PTR(im) , *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (rgbyte *) DSET_ARRAY(br->dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_rgba:{
         rgba *ar  = (rgba *) MRI_RGBA_PTR(im) , *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (rgba *) DSET_ARRAY(br->dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
      }
      break ;

   }

   if( THD_need_brick_factor(br->dset) ){
      MRI_IMAGE * qim ;
      qim = mri_mult_to_float( br->dset->dblk->brick_fac , im ) ;
      mri_free(im) ; im = qim ;
   }

   if( br->dset->taxis != NULL ){  /* 21 Oct 1996 */
      float zz , tt ;

      zz = br->dset->daxes->zzorg + kz * br->dset->daxes->zzdel ;
      tt = THD_timeof( 0 , zz , br->dset->taxis ) ;

      im->xo = tt ; im->dx = br->dset->taxis->ttdel ;   /* origin and delta */

      if( br->dset->taxis->units_type == UNITS_MSEC_TYPE ){ /* convert to sec */
         im->xo *= 0.001 ; im->dx *= 0.001 ;
      }
   } else {
      im->xo = 0.0 ; im->dx = 1.0 ;  /* 08 Nov 1996 */
   }

   return im ;
}
