#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/*! Return value is 0 if an error occurs, or number of voxels prepared. */

int THD_instacorr_prepare( ICOR_setup *iset )
{
   int iv , nmmm=0 , ntime ; byte *mmm=NULL ;
   float **dvec , **gvec=NULL ; int ngvec=0 ;

ENTRY("THD_instacorr_prepare") ;

   /*-- check inputs for a reasonable amount of usefulness --*/

   if( iset == NULL || !ISVALID_DSET(iset->dset) ) RETURN(0) ;

   /*-- (re)create mask, if needed --*/

   if( iset->mset != NULL && iset->mmm == NULL ){
     if( DSET_NVOX(iset->mset) != DSET_NVOX(iset->dset) ){
       ERROR_exit("Mask dataset '%s' doesn't match input dataset '%s'",
                  DSET_BRIKNAME(iset->mset) , DSET_BRIKNAME(iset->dset) ) ;
       RETURN(0) ;
     }
     mmm = THD_makemask( iset->mset , iset->mindex , 1.0f,-1.0f ) ;
     if( mmm == NULL ){
       ERROR_message("Can't make mask from '%s[%d]'" ,
                     DSET_BRIKNAME(iset->mset) , iset->mindex ) ;
       RETURN(0) ;
     }
     nmmm = THD_countmask( DSET_NVOX(iset->mset) , mmm ) ;
     if( nmmm < 9 ){
       ERROR_message("Mask from '%s[%d]' has %d voxels!" ,
                     DSET_BRIKNAME(iset->mset) , iset->mindex , nmmm ) ;
       free(mmm) ; RETURN(0) ;
     }
     INFO_message("Mask from '%s[%d]' has %d voxels!" ,
                   DSET_BRIKNAME(iset->mset) , iset->mindex , nmmm ) ;
     iset->mmm = mmm ;
   }

   /*-- automask? --*/

   if( iset->automask && iset->mmm == NULL ){
     mmm = THD_automask(iset->dset) ;
     if( mmm == NULL ){
       ERROR_message("Can't create automask from '%s'?!",DSET_BRIKNAME(iset->dset)) ;
       RETURN(0) ;
     }
     nmmm = THD_countmask( DSET_NVOX(iset->mset) , mmm ) ;
     if( nmmm < 9 ){
       ERROR_message("Automask from '%s' has %d voxels!" ,
                     DSET_BRIKNAME(iset->dset) , nmmm ) ;
       free(mmm) ; RETURN(0) ;
     }
     INFO_message("Automask from '%s' has %d voxels",DSET_BRIKNAME(iset->dset),nmmm) ;
     iset->mmm = mmm ;
   }

   /*--- Extract time series for analysis ---*/

   INFO_message("Extracting dataset time series") ;

   iset->mv = THD_dset_to_vectim( iset->dset , iset->mmm ) ;
   if( iset->mv == NULL ){
     ERROR_message("Can't extract dataset time series!") ; RETURN(0) ;
   }
   nmmm  = iset->mv->nvec ;
   ntime = iset->mv->nvals ;

   /*--- Filter time series ---*/

   INFO_message("Filtering %d dataset time series",nmmm) ;

   if( iset->fbot <  0.0f       ) iset->fbot = 0.0f ;
   if( iset->ftop <= iset->fbot ) iset->ftop = 999999.9f ;  /* infinity */

   dvec = (float **)malloc(sizeof(float *)*nmmm) ;
   for( iv=0 ; iv < nmmm ; iv++ ) dvec[iv] = VECTIM_PTR(iset->mv,iv) ;

   if( iset->gortim != NULL ){
     if( iset->gortim->nx < ntime ){
       ERROR_message("Global ort time series length=%d is shorter than dataset=%d",
                     iset->gortim->nx , ntime ) ;
     } else {
       ngvec = iset->gortim->ny ;
       gvec  = (float **)malloc(sizeof(float)*ngvec) ;
       for( iv=0 ; iv < ngvec ; iv++ )
         gvec[iv] = MRI_FLOAT_PTR(iset->gortim) + iv*iset->gortim->nx ;
     }
   }

   THD_bandpass_vectors( ntime, nmmm, dvec, iset->mv->dt,
                         iset->fbot, iset->ftop, 1, ngvec, gvec ) ;

   free(dvec) ; if( gvec != NULL ) free(gvec) ;

   /*--- Blur time series ---*/

   if( iset->blur > 0.0f ){
     INFO_message("Spatially blurring dataset volumes") ;
     mri_blur3D_vectim( iset->mv , iset->blur ) ;
   }

   /*-- normalize --*/

   INFO_message("Normalizing dataset time series") ;
   THD_vectim_normalize( iset->mv ) ;

   RETURN(nmmm) ;
}

/*---------------------------------------------------------------------------*/
/*! Compute the instant correlation from voxel index ijk.
    mri_free() this image when you are done with it!
*//*-------------------------------------------------------------------------*/

MRI_IMAGE * THD_instacorr( ICOR_setup *iset , int ijk , int ata )
{
   int kk ; MRI_IMAGE *qim ; float *qar , *dar , *tsar ; int *ivar ;

ENTRY("THD_instacorr") ;

   if( iset == NULL || iset->mv == NULL || ijk < 0 ) RETURN(NULL) ;

   /** extract reference time series **/

   tsar = (float *)malloc(sizeof(float)*iset->mv->nvals) ;
   kk   = THD_vectim_ifind( ijk , iset->mv ) ;

   if( kk >= 0 ){ /* direct from vectim, if available */

     memcpy( tsar , VECTIM_PTR(iset->mv,kk) , sizeof(float)*iset->mv->nvals ) ;

   } else {       /* non-vectim voxel in dataset ==> must be processed */
#if 1
     free(tsar) ; RETURN(NULL) ;  /* don't allow this */
#else
     int iv , ngvec=0 ; float **gvec=NULL ;

     kk = THD_extract_array( ijk , iset->dset , 0 , tsar ) ;
     if( kk < 0 ){ free(tsar) ; RETURN(NULL) ; }

     if( iset->gortim != NULL ){
       ngvec = iset->gortim->ny ;
       gvec  = (float **)malloc(sizeof(float)*ngvec) ;
       for( iv=0 ; iv < ngvec ; iv++ )
         gvec[iv] = MRI_FLOAT_PTR(iset->gortim) + iv*iset->gortim->nx ;
     }
     THD_bandpass_vectors( iset->mv->nvals , 1, &tsar, iset->mv->dt,
                           iset->fbot, iset->ftop, 1, ngvec, gvec ) ;
     if( gvec != NULL ) free(gvec) ;
     THD_normalize( iset->mv->nvals , tsar ) ;
#endif
   }

   /** do the dot products **/

   dar = (float *)malloc(sizeof(float)*iset->mv->nvec) ;
   THD_vectim_dotprod( iset->mv , VECTIM_PTR(iset->mv,kk) , dar , ata ) ;

   /** put them into the output image **/

   qim  = mri_new_vol( iset->mv->nx , iset->mv->ny , iset->mv->nz , MRI_float ) ;
   qar  = MRI_FLOAT_PTR(qim) ;
   ivar = iset->mv->ivec ;
   for( kk=0 ; kk < iset->mv->nvec ; kk++ ) qar[ivar[kk]] = dar[kk] ;

   /** e finito **/

   free(dar) ; free(tsar) ; RETURN(qim) ;
}
