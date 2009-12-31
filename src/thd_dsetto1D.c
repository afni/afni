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

  05 Nov 2001: Split into 2 functions -
      THD_extract_series() produces a new image each time
      THD_extract_array()  copies data into a user-supplied array
-----------------------------------------------------------------*/

MRI_IMAGE * THD_extract_series( int ind , THD_3dim_dataset *dset , int raw )
{
   int nv , typ , ii ;
   MRI_IMAGE *im ;
   void *imar ;

ENTRY("THD_extract_series") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;

   nv  = dset->dblk->nvals ;
   if( raw ) typ = DSET_BRICK_TYPE(dset,0) ;  /* type of output array */
   else      typ = MRI_float ;

   im   = mri_new( nv , 1 , typ ) ;           /* output image */
   imar = mri_data_pointer(im) ;

   ii = THD_extract_array( ind , dset , raw , imar ) ; /* get data */

   if( ii != 0 ){ mri_free(im) ; RETURN(NULL) ; }      /* bad */

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

   RETURN(im) ;
}

/*---------------------------------------------------------------------------
  Return value is 0 for all is good, -1 for all is bad.
  If raw == 0, uar is of type float.
  If raw != 0, uar is of the same type as the dataset brick
    (the user needs to know the type ahead of time if raw != 0).
  Data goes into a user-supplied array.
-----------------------------------------------------------------------------*/

int THD_extract_array( int ind, THD_3dim_dataset *dset, int raw, void *uar )
{
   MRI_TYPE typ ;
   int nv , ival , nb , nb1 ;
   char  *iar ;      /* brick in the input */
   float *far=NULL ; /* non-raw output */
   void *tar=NULL ;

/** ENTRY("THD_extract_array") ; **/

   if( ind < 0             || uar == NULL           ||
       !ISVALID_DSET(dset) || ind >= DSET_NVOX(dset)  ) return(-1) ;

   nv  = dset->dblk->nvals ;
   iar = DSET_ARRAY(dset,0) ;
   if( iar == NULL ){         /* load data from disk? */
     DSET_load(dset) ;
     iar = DSET_ARRAY(dset,0); if( iar == NULL ) return(-1) ;
   }
   typ = DSET_BRICK_TYPE(dset,0) ;  /* raw data type */

   /* will extract nb bytes of raw data into array tar */

   nb1 = mri_datum_size(typ); nb = nb1 * (nv+1); nb1 = nb1 * nv;
   tar = (void *)calloc(1,nb) ;

   if( !raw ) far = (float *)uar ;  /* non-raw output */

   switch( typ ){

      default:           /* don't know what to do --> return nada */
         free(tar); return(-1);
      break ;

      case MRI_byte:{
         byte *ar = (byte *)tar , *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (byte *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
         if( !raw ){
            for( ival=0 ; ival < nv ; ival++ ) far[ival] = ar[ival] ;
         }
      }
      break ;

      case MRI_short:{
         short *ar = (short *)tar , *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (short *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
         if( !raw ){
            for( ival=0 ; ival < nv ; ival++ ) far[ival] = ar[ival] ;
         }
      }
      break ;

      case MRI_float:{
         float *ar = (float *)tar , *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (float *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
         if( !raw ){
            for( ival=0 ; ival < nv ; ival++ ) far[ival] = ar[ival] ;
         }
      }
      break ;

#if 0
      case MRI_int:{
         int *ar = (int *)tar , *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (int *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
         if( !raw ){
            for( ival=0 ; ival < nv ; ival++ ) far[ival] = ar[ival] ;
         }
      }
      break ;

      case MRI_double:{
         double *ar = (double *)tar , *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (double *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
         if( !raw ){
            for( ival=0 ; ival < nv ; ival++ ) far[ival] = ar[ival] ;
         }
      }
      break ;
#endif

      case MRI_complex:{
         complex *ar = (complex *)tar , *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (complex *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) ar[ival] = bar[ind] ;
         }
         if( !raw ){
            for( ival=0 ; ival < nv ; ival++ ) far[ival] = CABS(ar[ival]) ;
         }
      }
      break ;

   }

   if( raw ){ memcpy(uar,tar,nb1); free(tar); return(0); }

   if( THD_need_brick_factor(dset) ){
     for( ival=0 ; ival < nv ; ival++ )
       if( DSET_BRICK_FACTOR(dset,ival) > 0.0 )
         far[ival] *= DSET_BRICK_FACTOR(dset,ival) ;
   }

   free(tar); return(0);
}

/*---------------------------------------------------------------------------
  Return value is 0 for all is good, -1 for all is bad.
  Data goes into a user-supplied array.
-----------------------------------------------------------------------------*/

int THD_extract_float_array( int ind, THD_3dim_dataset *dset, float *far )
{
   MRI_TYPE typ ;
   int nv , ival , nb , nb1 ;
   char  *iar ;      /* brick in the input */

   if( ind < 0             || far == NULL           ||
       !ISVALID_DSET(dset) || ind >= DSET_NVOX(dset)  ) return(-1) ;

   nv  = dset->dblk->nvals ;
   typ = DSET_BRICK_TYPE(dset,0) ;  /* raw data type */

   switch( typ ){

      default:           /* don't know what to do --> return nada */
         return(-1);
      break ;

      case MRI_byte:{
         byte *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (byte *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) far[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_short:{
         short *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (short *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) far[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_float:{
         float *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (float *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) far[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_complex:{
         complex *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (complex *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) far[ival] = CABS(bar[ind]) ;
         }
      }
      break ;

   }

   if( THD_need_brick_factor(dset) ){
     for( ival=0 ; ival < nv ; ival++ )
       if( DSET_BRICK_FACTOR(dset,ival) > 0.0 )
         far[ival] *= DSET_BRICK_FACTOR(dset,ival) ;
   }

   return(0);
}

/*----------------------------------------------------------------------------*/

int THD_voxel_is_constant( int ind , THD_3dim_dataset *dset )
{
   float *far ; int ii,nvox,nvals ;

   if( !ISVALID_DSET(dset) ) return 1 ;
   if( ind < 0 || ind >= DSET_NVOX(dset) ) return 1 ;

   nvals = DSET_NVALS(dset) ; if( nvals == 1 ) return 1 ;
   far = (float *)malloc(sizeof(float)*nvals) ;
   ii = THD_extract_array( ind , dset , 0 , far ) ;
   if( ii < 0 ){ free(far); return 1; }
   for( ii=1 ; ii < nvals && far[ii]==far[0]; ii++ ) ; /*nada*/
   free(far) ; return (ii==nvals) ;
}

/*----------------------------------------------------------------------------
   04 Feb 2000: do a bunch of timeseries at once (for efficiency)
   27 Feb 2003: rearranged slightly for more efficiency
------------------------------------------------------------------------------*/

MRI_IMARR * THD_extract_many_series( int ns, int *ind, THD_3dim_dataset *dset )
{
   MRI_IMARR *imar ; /* output */
   MRI_IMAGE *im ;
   int nv , ival , kk ;
   char *iar ;      /* brick in the input */
   float **far ;    /* 27 Feb 2003: ptrs to output */

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

   /* create output */

   far = (float **) malloc(sizeof(float *)*ns) ;  /* 27 Feb 2003 */
   INIT_IMARR(imar) ;
   for( kk=0 ; kk < ns ; kk++ ){
     im = mri_new( nv , 1 , MRI_float ) ;  /* N.B.: now does 0 fill */
     far[kk] = MRI_FLOAT_PTR(im) ;         /* ptr to kk-th output series */
     ADDTO_IMARR(imar,im) ;
   }

   /* fill the output */

   switch( DSET_BRICK_TYPE(dset,0) ){

      default:             /* don't know what to do --> return nada */
        DESTROY_IMARR(imar) ; free(far) ;
        RETURN( NULL );

      case MRI_byte:{
        byte * bar ;
        for( ival=0 ; ival < nv ; ival++ ){
          bar = (byte *) DSET_ARRAY(dset,ival) ;
          if( bar != NULL ){
            for( kk=0 ; kk < ns ; kk++ ){
              far[kk][ival] = (float)bar[ind[kk]] ;
            }
          }
        }
      }
      break ;

      case MRI_short:{
        short * bar ;
        for( ival=0 ; ival < nv ; ival++ ){
          bar = (short *) DSET_ARRAY(dset,ival) ;
          if( bar != NULL ){
            for( kk=0 ; kk < ns ; kk++ ){
              far[kk][ival] = (float)bar[ind[kk]] ;
            }
          }
        }
      }
      break ;

      case MRI_float:{
         float * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (float *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ){
              for( kk=0 ; kk < ns ; kk++ ){
                 far[kk][ival] = bar[ind[kk]] ;
              }
            }
         }
      }
      break ;

#if 0
      case MRI_int:{
         int * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (int *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ){
              for( kk=0 ; kk < ns ; kk++ ){
                 far[kk][ival] = bar[ind[kk]] ;
              }
            }
         }
      }
      break ;

      case MRI_double:{
         double * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (double *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ){
              for( kk=0 ; kk < ns ; kk++ ){
                 far[kk][ival] = (float)bar[ind[kk]] ;
              }
            }
         }
      }
      break ;
#endif

      case MRI_complex:{
         complex * bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (complex *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ){
              for( kk=0 ; kk < ns ; kk++ ){
                 far[kk][ival] = bar[ind[kk]].r ;
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

#if 0  /* 27 Feb 2003 */
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
#endif

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

   free(far) ; RETURN(imar);
}

/*---------------------------------------------------------------------------*/
/* Convert time series dataset to a 1D style MRI_IMAGE struct. [05 Mar 2008] */

MRI_IMAGE * THD_dset_to_1Dmri( THD_3dim_dataset *dset )
{
   MRI_IMAGE *im ; float *far ;
   int nx , ny , ii ;

ENTRY("THD_dset_to_1D") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ) RETURN(NULL) ;

   nx = DSET_NVALS(dset) ;
   ny = DSET_NVOX(dset) ;
   im = mri_new( nx , ny , MRI_float ) ; far = MRI_FLOAT_PTR(im) ;

   for( ii=0 ; ii < ny ; ii++ )
     THD_extract_array( ii , dset , 0 , far + ii*nx ) ;

   RETURN(im) ;
}

/*----------------------------------------------------------------------------*/

void THD_extract_many_arrays( int ns , int *ind ,
                              THD_3dim_dataset *dset , float *dsar )
{
   int nv , ival , kk ;
   char *iar ;      /* brick in the input */
   float **far ;    /* ptrs to output */
   float fac ;

ENTRY("THD_extract_many_arrays") ;

   if( ns <= 0 || ind == NULL | dset == NULL || dsar == NULL ) EXRETURN ;

   /* try to load dataset */

   DSET_load(dset) ; if( !DSET_LOADED(dset) ) EXRETURN ;
   nv = dset->dblk->nvals ;

   far = (float **) malloc(sizeof(float *)*ns) ;  /* 27 Feb 2003 */
   for( kk=0 ; kk < ns ; kk++ ) far[kk] = dsar + kk*nv ;

   /* fill the output */

   switch( DSET_BRICK_TYPE(dset,0) ){

      default:             /* don't know what to do --> return nada */
        free(far) ; EXRETURN ;

      case MRI_byte:{
        byte *bar ;
        for( ival=0 ; ival < nv ; ival++ ){
          bar = (byte *)DSET_ARRAY(dset,ival) ;
          for( kk=0 ; kk < ns ; kk++ ) far[kk][ival] = (float)bar[ind[kk]] ;
        }
      }
      break ;

      case MRI_short:{
        short *bar ;
        for( ival=0 ; ival < nv ; ival++ ){
          bar = (short *)DSET_ARRAY(dset,ival) ;
          for( kk=0 ; kk < ns ; kk++ ) far[kk][ival] = (float)bar[ind[kk]] ;
        }
      }
      break ;

      case MRI_float:{
        float *bar ;
        for( ival=0 ; ival < nv ; ival++ ){
          bar = (float *)DSET_ARRAY(dset,ival) ;
          for( kk=0 ; kk < ns ; kk++ ) far[kk][ival] = bar[ind[kk]] ;
        }
      }
      break ;

#if 0
      case MRI_int:{
        int *bar ;
        for( ival=0 ; ival < nv ; ival++ ){
          bar = (int *)DSET_ARRAY(dset,ival) ;
          for( kk=0 ; kk < ns ; kk++ ) far[kk][ival] = (float)bar[ind[kk]] ;
        }
      }
      break ;

      case MRI_double:{
        double *bar ;
        for( ival=0 ; ival < nv ; ival++ ){
          bar = (double *)DSET_ARRAY(dset,ival) ;
          for( kk=0 ; kk < ns ; kk++ ) far[kk][ival] = (float)bar[ind[kk]] ;
        }
      }
      break ;
#endif

      case MRI_complex:{
        complex *bar ;
        for( ival=0 ; ival < nv ; ival++ ){
          bar = (complex *)DSET_ARRAY(dset,ival) ;
          for( kk=0 ; kk < ns ; kk++ ) far[kk][ival] = bar[ind[kk]].r ;
        }
      }
      break ;

   }

   /* scale outputs, if needed */

   for( ival=0 ; ival < nv ; ival++ ){
     fac = DSET_BRICK_FACTOR(dset,ival) ;
     if( fac > 0.0f && fac != 1.0f ){
       for( kk=0 ; kk < ns ; kk++ ) far[kk][ival] *= fac ;
     }
   }

   free(far) ; EXRETURN ;
}
