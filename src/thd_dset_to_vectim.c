#include "mrilib.h"

/*! Convert a dataset to the MRI_vectim format (float timeseries). */

MRI_vectim * THD_dset_to_vectim( THD_3dim_dataset *dset , byte *mask )
{
   byte *mmm=mask ;
   MRI_vectim *mrv=NULL ;
   int kk,iv , nvals , nvox , nmask ;

ENTRY("THD_dset_to_vectim") ;

                     if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   DSET_load(dset) ; if( !DSET_LOADED(dset)  ) RETURN(NULL) ;

   nvals = DSET_NVALS(dset) ;
   nvox  = DSET_NVOX(dset) ;

   if( mmm != NULL ){
     nmask = THD_countmask( nvox , mmm ) ;
     if( nmask <= 0 ) RETURN(NULL) ;
   } else {
     nmask = nvox ;
     mmm   = (byte *)malloc(sizeof(byte)*nmask) ;
     if( mmm == NULL ){
       ERROR_message("THD_dset_to_vectim: out of memory") ;
       RETURN(NULL) ;
     }
     memset( mmm , 1 , sizeof(byte)*nmask ) ;
   }

   mrv = (MRI_vectim *)malloc(sizeof(MRI_vectim)) ;

   mrv->nvec  = nmask ;
   mrv->nvals = nvals ;
   mrv->ivec  = (int *)  malloc(sizeof(int)  *nmask) ;
   if( mrv->ivec == NULL ){
     ERROR_message("THD_dset_to_vectim: out of memory") ;
     free(mrv) ; if( mmm != mask ) free(mmm) ;
     RETURN(NULL) ;
   }
   mrv->fvec  = (float *)malloc(sizeof(float)*nmask*nvals) ;
   if( mrv->fvec == NULL ){
     ERROR_message("THD_dset_to_vectim: out of memory") ;
     free(mrv->ivec) ; free(mrv) ; if( mmm != mask ) free(mmm) ;
     RETURN(NULL) ;
   }

   for( kk=iv=0 ; iv < nvox ; iv++ ){
     if( mmm[iv] == 0 ) continue ;
     mrv->ivec[kk] = iv ;
     (void)THD_extract_array( iv , dset , 0 , MRV_PTR(mrv,kk) ) ;
     kk++ ;
   }

   if( mmm != mask ) free(mmm) ;
   RETURN(mrv) ;
}
