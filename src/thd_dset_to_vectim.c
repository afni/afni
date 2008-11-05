#include "mrilib.h"

/*-----------------------------------------------------------------*/
/*! Convert a dataset to the MRI_vectim format, where each time
    series is a contiguous set of values in an array, and the
    voxel indexes whence came the values are also stored.
*//*---------------------------------------------------------------*/

MRI_vectim * THD_dset_to_vectim( THD_3dim_dataset *dset, byte *mask )
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
     nmask = THD_countmask( nvox , mmm ) ;  /* number to keep */
     if( nmask <= 0 ) RETURN(NULL) ;
   } else {
     nmask = nvox ;                         /* keep them all */
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

   /* store desired voxel time series */

   for( kk=iv=0 ; iv < nvox ; iv++ ){
     if( mmm[iv] == 0 ) continue ;
     mrv->ivec[kk] = iv ;
     (void)THD_extract_array( iv , dset , 0 , MRV_PTR(mrv,kk) ) ;
     kk++ ;
   }

   if( mmm != mask ) free(mmm) ;
   RETURN(mrv) ;
}

/*-----------------------------------------------------------*/
/*! Determine size of a MRI_vectim struct from this dataset. */

int64_t THD_vectim_size( THD_3dim_dataset *dset , byte *mask )
{
   int nvals , nvox , nmask ;
   int64_t sz ;

   ENTRY("THD_vectim_size") ;

   if( !ISVALID_DSET(dset) ) RETURN(0) ;

   nvals = DSET_NVALS(dset) ;
   nvox  = DSET_NVOX(dset) ;
   if( mask != NULL ) nmask = THD_countmask( nvox , mask ) ;
   else               nmask = DSET_NVOX(dset) ;

   sz = ((int64_t)nmask) * ( ((int64_t)nvals) * sizeof(float) + sizeof(int) ) ;
   RETURN(sz) ;
}

/*-----------------------------------------------------------------------*/
/*! Binary search in a sorted integer array. */

int bsearch_int( int tt , int nar , int *ar )
{
   register int targ , ii , jj , kk , nn ;

   if( nar == 0 || ar == NULL ) return -1 ; /* bad inputs */

   targ = tt ; ii = 0 ; jj = nar-1 ;      /* setup */

        if( targ <  ar[0]  ) return -1 ;  /* not found */
   else if( targ == ar[0]  ) return  0 ;  /* at start! */

        if( targ >  ar[jj] ) return -1 ;  /* not found */
   else if( targ == ar[jj] ) return jj ;  /* at end!   */

   /* at the start of this loop, we've already checked
      indexes ii and jj, so check the middle of them (kk),
      and if that doesn't work, make the middle the
      new ii or the new jj -- so again we will have
      checked both ii and jj when the loop iterates back. */

   while( jj-ii > 1 ){
     kk = (ii+jj) / 2 ;         /* midpoint */
     nn = ar[kk] - targ ;       /* sign of difference */
     if( nn == 0 ) return kk ;  /* found it! */
     if( nn <  0 ) ii = kk ;    /* must be above kk */
     else          jj = kk ;    /* must be below kk */
   }

   return -1 ;
}

/*-----------------------------------------------------------------------*/
/*! Find a voxel index in a MRI_vectim struct.  If not found, return -1. */

int THD_vectim_ifind( int iv , MRI_vectim *mrv )
{
   if( mrv == NULL ) return -1 ;  /* stoopid user */
   return bsearch_int( iv , mrv->nvec , mrv->ivec ) ;
}
