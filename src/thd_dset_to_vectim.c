#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

/*--------------------------------------------------------------------------*/
/*! Convert a dataset to the MRI_vectim format, where each time
    series is a contiguous set of values in an array, and the
    voxel indexes whence came the values are also stored.
*//*------------------------------------------------------------------------*/

MRI_vectim * THD_dset_to_vectim( THD_3dim_dataset *dset, byte *mask , int ignore )
{
   byte *mmm=mask ;
   MRI_vectim *mrv=NULL ;
   int kk,iv , nvals , nvox , nmask ;

ENTRY("THD_dset_to_vectim") ;

                     if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   DSET_load(dset) ; if( !DSET_LOADED(dset)  ) RETURN(NULL) ;

   if( ignore < 0 ) ignore = 0 ;
   nvals = DSET_NVALS(dset) - ignore ; if( nvals <= 0 ) RETURN(NULL) ;
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

   mrv->nvec   = nmask ;
   mrv->nvals  = nvals ;
   mrv->ignore = ignore ;
   mrv->ivec   = (int *)malloc(sizeof(int)*nmask) ;
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
     if( mmm[iv] ) mrv->ivec[kk++] = iv ;  /* build index list */
   }

   if( ignore > 0 ){  /* extract 1 at a time, save what we want */

     float *var = (float *)malloc(sizeof(float)*(nvals+ignore)) ;
     for( kk=iv=0 ; iv < nvox ; iv++ ){
       if( mmm[iv] == 0 ) continue ;
       (void)THD_extract_array( iv , dset , 0 , var ) ;
#pragma omp critical (MEMCPY)
       memcpy( VECTIM_PTR(mrv,kk) , var+ignore , sizeof(float)*nvals ) ;
       kk++ ;
     }
     free(var) ;

   } else {  /* do all at once: this way is a lot faster */

     THD_extract_many_arrays( nmask , mrv->ivec , dset , mrv->fvec ) ;

   }

   mrv->nx = DSET_NX(dset) ; mrv->dx = fabs(DSET_DX(dset)) ;
   mrv->ny = DSET_NY(dset) ; mrv->dy = fabs(DSET_DY(dset)) ;
   mrv->nz = DSET_NZ(dset) ; mrv->dz = fabs(DSET_DZ(dset)) ;

   DSET_UNMSEC(dset) ; mrv->dt = DSET_TR(dset) ;
   if( mrv->dt <= 0.0f ) mrv->dt = 1.0f ;

   if( mmm != mask ) free(mmm) ;
   RETURN(mrv) ;
}

/*---------------------------------------------------------------------*/
/*-------- Catenates two datasets into vectim     ZSS Jan 2010 --------*/
MRI_vectim * THD_2dset_to_vectim( THD_3dim_dataset *dset1, byte *mask1 , 
                                  THD_3dim_dataset *dset2, byte *mask2 , 
                                  int ignore )
{
   byte *mmmv[2]={NULL, NULL}, *mmmt=NULL;
   THD_3dim_dataset *dsetv[2]={NULL, NULL};
   MRI_vectim *mrv=NULL ;
   int kk2, kk,iv,id, nvals , nvoxv[2]={0,0} , nmaskv[2]={0,0} ;
   int *ivvectmp=NULL;

ENTRY("THD_2dset_to_vectim") ;
   mmmv[0] = mask1;
   mmmv[1] = mask2;
   dsetv[0] = dset1;
   dsetv[1] = dset2;
   for (id=0; id<2;++id) {
                             if( !ISVALID_DSET(dsetv[id]) ) RETURN(NULL) ;
      DSET_load(dsetv[id]) ; if( !DSET_LOADED(dsetv[id])  ) RETURN(NULL) ;
      nvoxv[id] = DSET_NVOX(dsetv[id]) ;
   }
   if (DSET_NVALS(dsetv[0]) != DSET_NVALS(dsetv[1])) {
      RETURN(NULL) ;
   }
     
   if( ignore < 0 ) ignore = 0 ;
   nvals  = DSET_NVALS(dsetv[0]) - ignore ; if( nvals <= 0 ) RETURN(NULL) ;

   for (id=0; id<2; ++id) {
      if( mmmv[id] != NULL ){
         nmaskv[id] = THD_countmask( nvoxv[id] , mmmv[id] ) ;/* number to keep */
         if( nmaskv[id] <= 0 ) RETURN(NULL) ;
      } else {
         nmaskv[id] = nvoxv[id] ;                         /* keep them all */
         mmmv[id]   = (byte *)malloc(sizeof(byte)*nmaskv[id]) ;
         if( mmmv[id] == NULL ){
            ERROR_message("THD_2dset_to_vectim: out of memory") ;
            RETURN(NULL) ;
         }
         memset( mmmv[id] , 1 , sizeof(byte)*nmaskv[id] ) ;
      }
   }

   mrv = (MRI_vectim *)malloc(sizeof(MRI_vectim)) ;

   mrv->nvec   = nmaskv[0]+nmaskv[1] ;
   mrv->nvals  = nvals ;
   mrv->ignore = ignore ;
   mrv->ivec   = (int *)malloc(sizeof(int)*(nmaskv[0]+nmaskv[1])) ;
   ivvectmp    = (int *)malloc(sizeof(int)*(nmaskv[1])) ;
   if( mrv->ivec == NULL || ivvectmp == NULL){
     ERROR_message("THD_2dset_to_vectim: out of memory") ;
     if (mrv->ivec) free(mrv->ivec) ;
     if (ivvectmp)  free(ivvectmp) ;
     free(mrv) ; 
     if( mmmv[0] != mask1 ) free(mmmv[0]) ;
     if( mmmv[1] != mask2 ) free(mmmv[1]) ;
     RETURN(NULL) ;
   }
   mrv->fvec  = (float *)malloc(sizeof(float)*(nmaskv[0]+nmaskv[1])*nvals) ;
   if( mrv->fvec == NULL ){
     ERROR_message("THD_2dset_to_vectim: out of memory") ;
     if (ivvectmp)  free(ivvectmp) ;
     free(mrv->ivec) ; free(mrv) ; 
     if( mmmv[0] != mask1 ) free(mmmv[0]) ;
     if( mmmv[1] != mask2 ) free(mmmv[1]) ;
     RETURN(NULL) ;
   }

   /* store desired voxel time series */

   mmmt = mmmv[0];
   for( kk=iv=0 ; iv < nvoxv[0] ; iv++ ){
     if( mmmt[iv] ) mrv->ivec[kk++] = iv ;  /* build index list to 1st dset */
   }
   mmmt = mmmv[1]; kk2 = 0;
   for(    iv=0 ; iv < nvoxv[1] ; iv++ ){
     if( mmmt[iv] ) {
                        mrv->ivec[kk++] = iv + nvoxv[0] ;
                                             /* build index list to 2nd dset*/
                        ivvectmp[kk2++] = iv;
     }
   }

   if( ignore > 0 ){  /* extract 1 at a time, save what we want */

     float *var = (float *)malloc(sizeof(float)*(nvals+ignore)) ;
     mmmt = mmmv[0];
     for( kk=iv=0 ; iv < nvoxv[0] ; iv++ ){
       if( mmmt[iv] == 0 ) continue ;
       (void)THD_extract_array( iv , dsetv[0] , 0 , var ) ;
#pragma omp critical (MEMCPY)
       memcpy( VECTIM_PTR(mrv,kk) , var+ignore , sizeof(float)*nvals ) ;
       kk++ ;
     }
     mmmt = mmmv[1];
     for(    iv=0 ; iv < nvoxv[1] ; iv++ ){
       if( mmmt[iv] == 0 ) continue ;
       (void)THD_extract_array( iv , dsetv[1] , 0 , var ) ;
#pragma omp critical (MEMCPY)
       memcpy( VECTIM_PTR(mrv,kk) , var+ignore , sizeof(float)*nvals ) ;
       kk++ ;
     }
     
     free(var) ;

   } else {  /* do all at once: this way is a lot faster */

     THD_extract_many_arrays( nmaskv[0] ,  mrv->ivec            , 
                                dsetv[0]  ,   mrv->fvec            ) ;
     THD_extract_many_arrays( nmaskv[1] ,  ivvectmp, 
                                dsetv[1]  ,  (mrv->fvec+nmaskv[0]*mrv->nvals) ) ;

   }

   mrv->nx = DSET_NX(dsetv[0]) + DSET_NX(dsetv[1]); 
   mrv->dx = fabs(DSET_DX(dsetv[0])) ;
   mrv->ny = DSET_NY(dsetv[0]) ; mrv->dy = fabs(DSET_DY(dsetv[0])) ;
   mrv->nz = DSET_NZ(dsetv[0]) ; mrv->dz = fabs(DSET_DZ(dsetv[0])) ;

   DSET_UNMSEC(dsetv[0]) ; mrv->dt = DSET_TR(dsetv[0]) ;
   if( mrv->dt <= 0.0f ) mrv->dt = 1.0f ;

   if( mmmv[0] != mask1 ) free(mmmv[0]) ;
   if( mmmv[1] != mask2 ) free(mmmv[1]) ;
   if (ivvectmp)  free(ivvectmp) ;
        
   if (0) {
     int ShowThisTs=38001; 
     float *fff=NULL;
     fprintf(stderr,"++ ZSS mrv->nvec = %d, mrv->nvals = %d\n",
                    mrv->nvec, mrv->nvals);  
     for( kk=0 ; kk < mrv->nvals; ++kk) {
      fff=mrv->fvec+(mrv->nvals*ShowThisTs);
      fprintf(stderr," %f \t", *(fff+kk));
     }
   }

   RETURN(mrv) ;
}

/*---------------------------------------------------------------------*/

void THD_vectim_normalize( MRI_vectim *mrv )
{
   int iv , ii ;

   if( mrv == NULL ) return ;

   for( iv=0 ; iv < mrv->nvec ; iv++ )
     THD_normalize( mrv->nvals , VECTIM_PTR(mrv,iv) ) ;
}

/*---------------------------------------------------------------------*/

void THD_vectim_dotprod( MRI_vectim *mrv , float *vec , float *dp , int ata )
{
   if( mrv == NULL || vec == NULL || dp == NULL ) return ;

#pragma omp parallel if( mrv->nvec > 1 && mrv->nvec * mrv->nvals > 999999 )
 { int nvec=mrv->nvec, nvals=mrv->nvals, nv1=nvals-1, iv, ii ; float sum, *fv ;
 AFNI_OMP_START ;
#pragma omp for
   for( iv=0 ; iv < nvec ; iv++ ){
     fv = VECTIM_PTR(mrv,iv) ;
     for( sum=0.0f,ii=0 ; ii < nv1 ; ii+=2 )
       sum += fv[ii]*vec[ii] + fv[ii+1]*vec[ii+1] ;
     if( ii == nv1 ) sum += fv[ii]*vec[ii] ;
     dp[iv] = (ata) ? logf((1.0001f+sum)/(1.0001f-sum)) : sum ;
   }
 AFNI_OMP_END ;
 } /* end OpenMP */

  return ;
}

/*---------------------------------------------------------------------*/

void THD_vectim_vectim_dot( MRI_vectim *arv, MRI_vectim *brv, float *dp )
{
   int nvec , nvals , iv , ii ; float *av , *bv , sum ;

   if( arv == NULL || brv == NULL || dp == NULL ) return ;
   if( arv->nvec != brv->nvec || arv->nvals != brv->nvals ) return ;

   nvec = arv->nvec ; nvals = arv->nvals ;
   for( iv=0 ; iv < nvec ; iv++ ){
     av = VECTIM_PTR(arv,iv) ; bv = VECTIM_PTR(brv,iv) ;
     for( sum=0.0f,ii=0 ; ii < nvals ; ii++ ) sum += av[ii]*bv[ii] ;
     dp[iv] = sum ;
   }

   return ;
}

/*---------------------------------------------------------------------*/
/* 01 Mar 2010: Rank correlation. */

void THD_vectim_spearman( MRI_vectim *mrv , float *vec , float *dp )
{
   float *av , *bv , sav ;
   int nvec, nvals, iv ;

   if( mrv == NULL || vec == NULL || dp == NULL ) return ;

   nvec = mrv->nvec ; nvals = mrv->nvals ;
   av   = (float *)malloc(sizeof(float)*nvals) ;
   bv   = (float *)malloc(sizeof(float)*nvals) ;

   memcpy( av , vec , sizeof(float)*nvals ) ;
   sav = spearman_rank_prepare( nvals , av ) ; if( sav <= 0.0f ) sav = 1.e+9f ;

   for( iv=0 ; iv < nvec ; iv++ ){
     memcpy( bv , VECTIM_PTR(mrv,iv) , sizeof(float)*nvals ) ;
     dp[iv] = spearman_rank_corr( nvals , bv , sav , av ) ;
   }

  return ;
}

/*---------------------------------------------------------------------*/
/* 01 Mar 2010: Quadrant correlation. */

void THD_vectim_quadrant( MRI_vectim *mrv , float *vec , float *dp )
{
   float *av , *bv , sav ;
   int nvec, nvals, iv ;

   if( mrv == NULL || vec == NULL || dp == NULL ) return ;

   nvec = mrv->nvec ; nvals = mrv->nvals ;
   av   = (float *)malloc(sizeof(float)*nvals) ;
   bv   = (float *)malloc(sizeof(float)*nvals) ;

   memcpy( av , vec , sizeof(float)*nvals ) ;
   sav = quadrant_corr_prepare( nvals , av ) ; if( sav <= 0.0f ) sav = 1.e+9f ;

   for( iv=0 ; iv < nvec ; iv++ ){
     memcpy( bv , VECTIM_PTR(mrv,iv) , sizeof(float)*nvals ) ;
     dp[iv] = quadrant_corr( nvals , bv , sav , av ) ;
   }

  return ;
}

/*----------------------------------------------------------------------------*/

int THD_vectim_subset_average( MRI_vectim *mrv, int nind, int *ind, float *ar )
{
   int nvals , jj,kk,nkk ; register int ii ; float *fv ;

   if( mrv == NULL || nind <= 0 || ind == NULL || ar == NULL ) return 0 ;

   nvals = mrv->nvals ;

   for( ii=0 ; ii < nvals ; ii++ ) ar[ii] = 0.0f ;

   for( jj=0 ; jj < nind ; jj++ ){
     kk = THD_vectim_ifind( ind[jj] , mrv ) ; if( kk < 0 ) continue ;
     fv = VECTIM_PTR(mrv,kk) ;
     for( ii=0 ; ii < nvals ; ii++ ) ar[ii] += fv[ii] ;
     nkk++ ;
   }
   if( nkk > 1 ){
     register float fac = 1.0f/nkk ;
     for( ii=0 ; ii < nvals ; ii++ ) ar[ii] *= fac ;
   }

   return nkk ;
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
/*! Binary search in a sorted integer array;
    return index of tt in ar if found, return -1 if not found.
*//*---------------------------------------------------------------------*/

int bsearch_int( int tt , int nar , int *ar )
{
   int targ , ii , jj , kk , nn ;

   if( nar <= 0 || ar == NULL ) return -1 ; /* bad inputs */

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

/*----------------------------------------------------------------------*/
/*! Note that the dataset must already have bricks set up!  Or else!    */

void THD_vectim_to_dset( MRI_vectim *mrv , THD_3dim_dataset *dset )
{
   int nvals , nvec ,  kk , ign ;

ENTRY("THD_vectim_to_dset") ;

   if( mrv == NULL || !ISVALID_DSET(dset)           ) EXRETURN ;
   if( mrv->nvals + mrv->ignore != DSET_NVALS(dset) ) EXRETURN ;

   nvec  = mrv->nvec ;
   nvals = mrv->nvals ;
   ign   = mrv->ignore ;

   if( ign == 0 ){
     for( kk=0 ; kk < nvec ; kk++ )
       THD_insert_series( mrv->ivec[kk] , dset ,
                          nvals , MRI_float , VECTIM_PTR(mrv,kk) , 0 ) ;
   } else {
     float *var ;
#pragma omp critical (MALLOC)
     var = (float *)malloc(sizeof(float)*(nvals+ign)) ;
     for( kk=0 ; kk < nvec ; kk++ ){
       (void)THD_extract_array( mrv->ivec[kk] , dset , 0 , var ) ;
#pragma omp critical (MEMCPY)
       memcpy( var+ign , VECTIM_PTR(mrv,kk) , sizeof(float)*nvals ) ;
       THD_insert_series( mrv->ivec[kk] , dset ,
                          nvals , MRI_float , var , 0 ) ;
     }
   }

   EXRETURN ;
}
