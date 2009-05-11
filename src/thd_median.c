#include "mrilib.h"

/*-----------------------------------------------------------------*/
/*! Compute median brick of a dataset - 12 Aug 2001
    - 05 Nov 2001: Modified to use THD_extract_array() instead
                   of THD_extract_series()
-------------------------------------------------------------------*/

MRI_IMAGE * THD_median_brick( THD_3dim_dataset *dset )
{
   int nvox , nvals , ii ;
   MRI_IMAGE *tsim , *medim ;
   float *medar ;
   float *tsar ;  /* 05 Nov 2001 */

ENTRY("THD_median_brick") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ) RETURN(NULL) ;

   nvals = DSET_NVALS(dset) ;
   tsim  = DSET_BRICK(dset,0) ;

   if( nvals == 1 ){
     medim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,0), tsim ) ;
     RETURN(medim) ;
   }

   medim = mri_new_conforming( tsim , MRI_float ) ;
   medar = MRI_FLOAT_PTR(medim) ;
   nvox  = DSET_NVOX(dset) ;

   tsar = (float *) calloc( sizeof(float),nvals+1 ) ; /* 05 Nov 2001 */
   for( ii=0 ; ii < nvox ; ii++ ){
     THD_extract_array( ii , dset , 0 , tsar ) ;     /* 05 Nov 2001 */
     medar[ii] = qmed_float( nvals , tsar ) ;
   }

   free(tsar) ; RETURN(medim) ;
}

/*-----------------------------------------------------------------*/
/*! Compute MAD brick of a dataset - 07 Dec 2006
-------------------------------------------------------------------*/

MRI_IMAGE * THD_mad_brick( THD_3dim_dataset *dset )
{
   int nvox , nvals , ii ;
   MRI_IMAGE *tsim , *madim ;
   float *madar ;
   float *tsar ;

ENTRY("THD_mad_brick") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;

   nvals = DSET_NVALS(dset) ; if( nvals == 1 ) RETURN(NULL) ;

   DSET_load(dset) ;  if( !DSET_LOADED(dset) ) RETURN(NULL) ;
   tsim  = DSET_BRICK(dset,0) ;

   madim = mri_new_conforming( tsim , MRI_float ) ;
   madar = MRI_FLOAT_PTR(madim) ;
   nvox  = DSET_NVOX(dset) ;

   tsar = (float *) calloc( sizeof(float),nvals+1 ) ;
   for( ii=0 ; ii < nvox ; ii++ ){
     THD_extract_array( ii , dset , 0 , tsar ) ;
     qmedmad_float( nvals , tsar , NULL , madar+ii ) ;
   }

   free(tsar) ; RETURN(madim) ;
}

/*-----------------------------------------------------------------*/
/*! Compute median and MAD bricks of a dataset - 07 Dec 2006
-------------------------------------------------------------------*/

MRI_IMARR * THD_medmad_bricks( THD_3dim_dataset *dset )
{
   int nvox , nvals , ii ;
   MRI_IMAGE *tsim , *madim, *medim ;
   float             *madar, *medar ;
   MRI_IMARR *imar ;
   float *tsar ;

ENTRY("THD_medmad_bricks") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;

   nvals = DSET_NVALS(dset) ; if( nvals == 1 ) RETURN(NULL) ;

   DSET_load(dset) ;  if( !DSET_LOADED(dset) ) RETURN(NULL) ;
   tsim  = DSET_BRICK(dset,0) ;

   madim = mri_new_conforming( tsim , MRI_float ) ;
   madar = MRI_FLOAT_PTR(madim) ;
   medim = mri_new_conforming( tsim , MRI_float ) ;
   medar = MRI_FLOAT_PTR(medim) ;
   nvox  = DSET_NVOX(dset) ;

   tsar = (float *) calloc( sizeof(float),nvals+1 ) ;
   for( ii=0 ; ii < nvox ; ii++ ){
     THD_extract_array( ii , dset , 0 , tsar ) ;
     qmedmad_float( nvals , tsar , medar+ii , madar+ii ) ;
   }

   free(tsar) ;
   INIT_IMARR(imar) ; ADDTO_IMARR(imar,medim) ; ADDTO_IMARR(imar,madim) ;
   RETURN(imar) ;
}

/*-----------------------------------------------------------------*/
/*! Compute median and MAD bricks of an image array
-------------------------------------------------------------------*/

MRI_IMARR * IMARR_medmad_bricks( MRI_IMARR *dmar )
{
   int nvox , nvals , ii , kk ;
   MRI_IMAGE *tsim , *madim, *medim ;
   float             *madar, *medar ;
   MRI_IMARR *imar ;
   float *tsar , **dar ;

ENTRY("IMARR_medmad_bricks") ;

   if( dmar == NULL || IMARR_COUNT(dmar) < 2 ) RETURN(NULL) ;

   nvals = IMARR_COUNT(dmar) ;
   tsim  = IMARR_SUBIM(dmar,0) ;
   madim = mri_new_conforming( tsim , MRI_float ) ;
   madar = MRI_FLOAT_PTR(madim) ;
   medim = mri_new_conforming( tsim , MRI_float ) ;
   medar = MRI_FLOAT_PTR(medim) ;
   nvox  = tsim->nvox ;

   dar = (float **)malloc(sizeof(float *)*nvals) ;
   for( kk=0 ; kk < nvals ; kk++ )
     dar[kk] = MRI_FLOAT_PTR( IMARR_SUBIM(dmar,kk) ) ;

   tsar = (float *) calloc( sizeof(float),nvals+1 ) ;
   for( ii=0 ; ii < nvox ; ii++ ){
     for( kk=0 ; kk < nvals ; kk++ ) tsar[kk] = dar[kk][ii] ;
     qmedmad_float( nvals , tsar , medar+ii , madar+ii ) ;
   }

   free(tsar) ; free(dar) ;
   INIT_IMARR(imar) ; ADDTO_IMARR(imar,medim) ; ADDTO_IMARR(imar,madim) ;
   RETURN(imar) ;
}

/*-----------------------------------------------------------------*/
/*! Compute mean and sigma bricks of a dataset - 07 Dec 2006
-------------------------------------------------------------------*/

MRI_IMARR * THD_meansigma_bricks( THD_3dim_dataset *dset )
{
   int nvox , nvals , ii ;
   MRI_IMAGE *tsim , *sigim, *mnnim ;
   float             *sigar, *mnnar ;
   MRI_IMARR *imar ;
   float *tsar ;

ENTRY("THD_meansigma_bricks") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;

   nvals = DSET_NVALS(dset) ; if( nvals == 1 ) RETURN(NULL) ;

   DSET_load(dset) ;  if( !DSET_LOADED(dset) ) RETURN(NULL) ;
   tsim  = DSET_BRICK(dset,0) ;

   sigim = mri_new_conforming( tsim , MRI_float ) ;
   sigar = MRI_FLOAT_PTR(sigim) ;
   mnnim = mri_new_conforming( tsim , MRI_float ) ;
   mnnar = MRI_FLOAT_PTR(mnnim) ;
   nvox  = DSET_NVOX(dset) ;

   tsar = (float *) calloc( sizeof(float),nvals+1 ) ;
   for( ii=0 ; ii < nvox ; ii++ ){
     THD_extract_array( ii , dset , 0 , tsar ) ;
     meansigma_float( nvals , tsar , mnnar+ii , sigar+ii ) ;
   }

   free(tsar) ;
   INIT_IMARR(imar) ; ADDTO_IMARR(imar,mnnim) ; ADDTO_IMARR(imar,sigim) ;
   RETURN(imar) ;
}

/*-----------------------------------------------------------------*/
/*! Compute mean brick of a dataset.  [15 Apr 2005 - RWCox]
-------------------------------------------------------------------*/

MRI_IMAGE * THD_mean_brick( THD_3dim_dataset *dset )
{
   int nvox , nvals , ii , jj ;
   MRI_IMAGE *tsim , *medim ;
   float *medar , sum,fac ;
   float *tsar ;

ENTRY("THD_mean_brick") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ) RETURN(NULL) ;

   nvals = DSET_NVALS(dset)   ; fac = 1.0 / nvals ;
   tsim  = DSET_BRICK(dset,0) ;

   if( nvals == 1 ){
    medim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,0), tsim ) ;
    RETURN(medim) ;
   }

   medim = mri_new_conforming( tsim , MRI_float ) ;
   medar = MRI_FLOAT_PTR(medim) ;
   nvox  = DSET_NVOX(dset) ;

   tsar = (float *) calloc( sizeof(float),nvals+1 ) ;
   for( ii=0 ; ii < nvox ; ii++ ){
     THD_extract_array( ii , dset , 0 , tsar ) ;
     for( sum=0.0,jj=0 ; jj < nvals ; jj++ ) sum += tsar[jj] ;
     medar[ii] = fac * sum ;
   }

   free(tsar) ; RETURN(medim) ;
}

/*-----------------------------------------------------------------*/
/*! Compute RMS brick of a dataset.  [15 Apr 2005 - RWCox]
-------------------------------------------------------------------*/

MRI_IMAGE * THD_rms_brick( THD_3dim_dataset *dset )
{
   int nvox , nvals , ii , jj ;
   MRI_IMAGE *tsim , *medim ;
   float *medar , sum,fac ;
   float *tsar ;

ENTRY("THD_rms_brick") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ) RETURN(NULL) ;

   nvals = DSET_NVALS(dset)   ; fac = 1.0 / nvals ;
   tsim  = DSET_BRICK(dset,0) ;

   if( nvals == 1 ){
     medim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,0), tsim ) ;
     RETURN(medim) ;
   }

   medim = mri_new_conforming( tsim , MRI_float ) ;
   medar = MRI_FLOAT_PTR(medim) ;
   nvox  = DSET_NVOX(dset) ;

   tsar = (float *) calloc( sizeof(float),nvals+1 ) ;
   for( ii=0 ; ii < nvox ; ii++ ){
     THD_extract_array( ii , dset , 0 , tsar ) ;
     for( sum=0.0,jj=0 ; jj < nvals ; jj++ ) sum += tsar[jj]*tsar[jj] ;
     medar[ii] = sqrtf(fac * sum) ;
   }

   free(tsar) ; RETURN(medim) ;
}

/*-----------------------------------------------------------------*/
/*! Compute average abs brick of a dataset.  [11 May 2009 - RWCox]
-------------------------------------------------------------------*/

MRI_IMAGE * THD_aveabs_brick( THD_3dim_dataset *dset )
{
   int nvox , nvals , ii , jj ;
   MRI_IMAGE *tsim , *medim ;
   float *medar , sum,fac ;
   float *tsar ;

ENTRY("THD_rms_brick") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ) RETURN(NULL) ;

   nvals = DSET_NVALS(dset)   ; fac = 1.0 / nvals ;
   tsim  = DSET_BRICK(dset,0) ;
   nvox  = DSET_NVOX(dset) ;

   if( nvals == 1 ){
     medim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,0), tsim ) ;
     medar = MRI_FLOAT_PTR(medim) ;
     for( ii=0 ; ii < nvox ; ii++ ) medar[ii] = fabsf(medar[ii]) ;
     RETURN(medim) ;
   }

   medim = mri_new_conforming( tsim , MRI_float ) ;
   medar = MRI_FLOAT_PTR(medim) ;
   tsar  = (float *) calloc( sizeof(float),nvals+1 ) ;

   for( ii=0 ; ii < nvox ; ii++ ){
     THD_extract_array( ii , dset , 0 , tsar ) ;
     for( sum=0.0,jj=0 ; jj < nvals ; jj++ ) sum += fabsf(tsar[jj]) ;
     medar[ii] = fac * sum ;
   }

   free(tsar) ; RETURN(medim) ;
}
