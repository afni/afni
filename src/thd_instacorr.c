#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/*! Return value is 0 if an error occurs, or number of voxels prepared. */

int THD_instacorr_prepare( ICOR_setup *iset )
{
   int iv , nmmm=0 , ntime ; byte *mmm=NULL ;
   float dt , **dvec , **gvec=NULL ; int ngvec=0 ;

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

   iset->mv = THD_dset_to_vectim( iset->dset , iset->mmm ) ;
   if( iset->mv == NULL ){
     ERROR_message("Can't extract dataset time series!") ; RETURN(0) ;
   }
   nmmm  = iset->mv->nvec ;
   ntime = iset->mv->nvals ;
   dt    = DSET_TR(iset->dset) ;

   /*--- Filter time series ---*/

   if( iset->fbot <  0.0f       ) iset->fbot = 0.0f ;
   if( iset->ftop <= iset->fbot ) iset->ftop = 999999.9f ;

   dvec = (float **)malloc(sizeof(float *)*nmmm) ;
   for( iv=0 ; iv < nmmm ; iv++ ) dvec[iv] = VECTIM_PTR(iset->mv,iv) ;

   if( iset->gortim != NULL ){
     if( iset->gortim->nx < ntime ){
       ERROR_message("Global ort time series length=%d is shorter than dataset %d",
                     iset->gortim->nx , ntime ) ;
     } else {
       ngvec = iset->gortim->ny ;
       gvec  = (float **)malloc(sizeof(float)*ngvec) ;
       for( iv=0 ; iv < ngvec ; iv++ )
         gvec[iv] = MRI_FLOAT_PTR(iset->gortim) + iv*iset->gortim->nx ;
     }
   }

   INFO_message("Filtering dataset time series") ;
   THD_bandpass_vectors( ntime , nmmm , dvec ,
                         dt , iset->fbot , iset->ftop , 1 , ngvec , gvec ) ;

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
