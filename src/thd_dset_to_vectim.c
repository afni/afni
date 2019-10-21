#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#undef  VECTIM_scan
#define VECTIM_scan(vv)                                                                 \
 do{ size_t nbad = thd_floatscan((size_t)(vv)->nvals*(size_t)(vv)->nvec,(vv)->fvec) ;   \
     if( nbad > 0 ) WARNING_message("found %lld bad values in vectim",(long long)nbad); \
 } while(0)

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
   size_t ntot ;

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
#pragma omp critical (MALLOC)
     mmm   = (byte *)malloc(sizeof(byte)*nmask) ;
     if( mmm == NULL ){
       ERROR_message("THD_dset_to_vectim: out of memory A") ;
       RETURN(NULL) ;
     }
     memset( mmm , 1 , sizeof(byte)*nmask ) ;
   }

#pragma omp critical (MALLOC)
   mrv = (MRI_vectim *)malloc(sizeof(MRI_vectim)) ;

   mrv->nvec   = nmask ;
   mrv->nvals  = nvals ;
   mrv->ignore = ignore ;
#pragma omp critical (MALLOC)
   mrv->ivec   = (int *)malloc(sizeof(int)*nmask) ;
   if( mrv->ivec == NULL ){
     ERROR_message("THD_dset_to_vectim: out of memory") ;
     free(mrv) ; if( mmm != mask ) free(mmm) ;
     RETURN(NULL) ;
   }
   ntot = sizeof(float)*(size_t)nmask*(size_t)nvals ;
#pragma omp critical (MALLOC)
   mrv->fvec  = (float *)malloc(ntot) ;
   if( mrv->fvec == NULL ){
     ERROR_message("THD_dset_to_vectim: out of memory B -- tried to get %lld bytes",(long long)ntot) ;
     free(mrv->ivec) ; free(mrv) ; if( mmm != mask ) free(mmm) ;
     RETURN(NULL) ;
   } else if( ntot > 1000000000 ){  /* one BILLION bytes */
     INFO_message("THD_dset_to_vectim: allocated %lld bytes",(long long)ntot) ;
   }

   /* store desired voxel time series */

STATUS("create index list") ;
   for( kk=iv=0 ; iv < nvox ; iv++ ){
     if( mmm[iv] ) mrv->ivec[kk++] = iv ;  /* build index list */
   }

   if( ignore > 0 ){  /* extract 1 at a time, save what we want */

     float *var;
#pragma omp critical (MALLOC)
     var = (float *)malloc(sizeof(float)*(nvals+ignore)) ;
STATUS("ignore > 0 --> extracting one at a time") ;
     for( kk=iv=0 ; iv < nvox ; iv++ ){
       if( mmm[iv] == 0 ) continue ;
       (void)THD_extract_array( iv , dset , 0 , var ) ;
       AAmemcpy( VECTIM_PTR(mrv,kk) , var+ignore , sizeof(float)*nvals ) ;
       kk++ ;
     }
     free(var) ;

   } else {  /* do all at once: this way is a lot faster */

STATUS("ignore==0 --> extracting all at once") ;
     THD_extract_many_arrays( nmask , mrv->ivec , dset , mrv->fvec ) ;

   }

STATUS("setting parameters in vectim header") ;

   mrv->nx = DSET_NX(dset) ; mrv->dx = fabs(DSET_DX(dset)) ;
   mrv->ny = DSET_NY(dset) ; mrv->dy = fabs(DSET_DY(dset)) ;
   mrv->nz = DSET_NZ(dset) ; mrv->dz = fabs(DSET_DZ(dset)) ;

   DSET_UNMSEC(dset) ; mrv->dt = DSET_TR(dset) ;
   if( mrv->dt <= 0.0f ) mrv->dt = 1.0f ;


   if( mmm != mask ){
STATUS("free(mmm)") ;
     free(mmm) ;
   }

STATUS("VECTIM_scan()") ;
   VECTIM_scan(mrv) ; /* 09 Nov 2010 */
   RETURN(mrv) ;
}

/*--------------------------------------------------------------------------*/

MRI_vectim * THD_dset_censored_to_vectim( THD_3dim_dataset *dset,
                                          byte *mask , int nkeep , int *keep )
{
   byte *mmm=mask ;
   MRI_vectim *mrv=NULL ;
   int kk,iv,jj , nvals , nvox , nmask ;

ENTRY("THD_dset_censored_to_vectim") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;

   if( nkeep <= 0 || keep == NULL ){
     mrv = THD_dset_to_vectim( dset , mask , 0 ) ;
     RETURN(mrv) ;
   }

   if( ! THD_subset_loaded(dset,nkeep,keep) ){
     DSET_load(dset) ; if( !DSET_LOADED(dset)  ) RETURN(NULL) ;
   }

   nvals = nkeep ;
   nvox  = DSET_NVOX(dset) ;

   if( mmm != NULL ){
     nmask = THD_countmask( nvox , mmm ) ;  /* number to keep */
     if( nmask <= 0 ) RETURN(NULL) ;
   } else {
     nmask = nvox ;                         /* keep them all */
#pragma omp critical (MALLOC)
     mmm   = (byte *)malloc(sizeof(byte)*nmask) ;
     if( mmm == NULL ){
       ERROR_message("THD_dset_to_vectim: out of memory C") ;
       RETURN(NULL) ;
     }
     memset( mmm , 1 , sizeof(byte)*nmask ) ;
   }

#pragma omp critical (MALLOC)
   mrv = (MRI_vectim *)malloc(sizeof(MRI_vectim)) ;

   mrv->nvec   = nmask ;
   mrv->nvals  = nvals ;
   mrv->ignore = 0 ;
#pragma omp critical (MALLOC)
   mrv->ivec   = (int *)malloc(sizeof(int)*nmask) ;
   if( mrv->ivec == NULL ){
     ERROR_message("THD_dset_to_vectim: out of memory D") ;
     free(mrv) ; if( mmm != mask ) free(mmm) ;
     RETURN(NULL) ;
   }
#pragma omp critical (MALLOC)
   mrv->fvec  = (float *)malloc(sizeof(float)*(size_t)nmask*(size_t)nvals) ;
   if( mrv->fvec == NULL ){
     ERROR_message("THD_dset_to_vectim: out of memory E") ;
     free(mrv->ivec) ; free(mrv) ; if( mmm != mask ) free(mmm) ;
     RETURN(NULL) ;
   }

   /* store desired voxel time series */

   for( kk=iv=0 ; iv < nvox ; iv++ ){
     if( mmm[iv] ) mrv->ivec[kk++] = iv ;  /* build index list */
   }

#pragma omp critical (MALLOC)
  { float *var = (float *)malloc(sizeof(float)*DSET_NVALS(dset)) ;
    float *vpt ;
     for( kk=iv=0 ; iv < nvox ; iv++ ){
       if( mmm[iv] == 0 ) continue ;
       vpt = VECTIM_PTR(mrv,kk) ; kk++ ;
       if( nkeep > 1 ){
         (void)THD_extract_array( iv , dset , 0 , var ) ;
         for( jj=0 ; jj < nkeep ; jj++ ) vpt[jj] = var[keep[jj]] ;
       } else {
         vpt[0] = THD_get_float_value(iv,keep[0],dset) ;
       }
     }
     free(var) ;
   }

   mrv->nx = DSET_NX(dset) ; mrv->dx = fabs(DSET_DX(dset)) ;
   mrv->ny = DSET_NY(dset) ; mrv->dy = fabs(DSET_DY(dset)) ;
   mrv->nz = DSET_NZ(dset) ; mrv->dz = fabs(DSET_DZ(dset)) ;

   DSET_UNMSEC(dset) ; mrv->dt = DSET_TR(dset) ;
   if( mrv->dt <= 0.0f ) mrv->dt = 1.0f ;

   if( mmm != mask ) free(mmm) ;
   VECTIM_scan(mrv) ; /* 09 Nov 2010 */
   RETURN(mrv) ;
}

/*---------------------------------------------------------------------------*/

int THD_vectim_data_tofile( MRI_vectim *mrv , char *fnam )
{
   FILE *fp ; size_t nf , nw ;

   if( mrv == NULL || fnam == NULL ) return 0 ;

   fp = fopen( fnam , "w" ) ; if( fp == NULL ) return 0 ;
   nf = ((size_t)mrv->nvec) * ((size_t)mrv->nvals) ;
   nw = fwrite( mrv->fvec , sizeof(float) , nf , fp ) ;
   fclose(fp) ; if( nw == nf ) return 1 ;
   remove(fnam) ; return 0 ;
}

/*---------------------------------------------------------------------------*/

void THD_vector_fromfile( int nvals , int iv , float *vv , FILE *fp )
{
   fseeko( fp , ((off_t)iv) * ((off_t)nvals) * ((off_t)sizeof(float)) , SEEK_SET ) ;
   fread( vv , sizeof(float) , nvals , fp ) ;
   return ;
}

/*---------------------------------------------------------------------------*/

int THD_vectim_reload_fromfile( MRI_vectim *mrv , char *fnam )
{
   FILE *fp ; size_t nf , nw ;

   if( mrv == NULL || fnam == NULL ) return 0 ;

   fp = fopen( fnam , "r" ) ; if( fp == NULL ) return 0 ;
   nf = (size_t)mrv->nvec * (size_t)mrv->nvals ;
   if( mrv->fvec == NULL ) mrv->fvec = (float *)malloc(sizeof(float)*nf) ;
   nw = fread( mrv->fvec , sizeof(float) , nf , fp ) ;
   fclose(fp) ; return (int)nw ;
}

/*---------------------------------------------------------------------------*/

MRI_vectim * THD_dset_to_vectim_stend( THD_3dim_dataset *dset, byte *mask , int start, int end )
{
   byte *mmm=mask ;
   MRI_vectim *mrv=NULL ;
   int kk,iv , nvals , nvox , nmask ;

ENTRY("THD_dset_to_vectim_stend") ;

                     if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   DSET_load(dset) ; if( !DSET_LOADED(dset)  ) RETURN(NULL) ;

   if( start < 0     ) start = 0 ;
   if( end   < start ) end   = DSET_NVALS(dset)-1 ;
   nvals = end - start + 1 ; if( nvals <= 0 ) RETURN(NULL) ;
   nvox  = DSET_NVOX(dset) ;

   if( mmm != NULL ){
     nmask = THD_countmask( nvox , mmm ) ;  /* number to keep */
     if( nmask <= 0 ) RETURN(NULL) ;
   } else {
     nmask = nvox ;                         /* keep them all */
#pragma omp critical (MALLOC)
     mmm   = (byte *)malloc(sizeof(byte)*nmask) ;
     if( mmm == NULL ){
       ERROR_message("THD_dset_to_vectim: out of memory F") ;
       RETURN(NULL) ;
     }
     memset( mmm , 1 , sizeof(byte)*nmask ) ;
   }

#pragma omp critical (MALLOC)
   mrv = (MRI_vectim *)malloc(sizeof(MRI_vectim)) ;

   mrv->nvec   = nmask ;
   mrv->nvals  = nvals ;
   mrv->ignore = start ;
#pragma omp critical (MALLOC)
   mrv->ivec   = (int *)malloc(sizeof(int)*nmask) ;
   if( mrv->ivec == NULL ){
     ERROR_message("THD_dset_to_vectim: out of memory G") ;
     free(mrv) ; if( mmm != mask ) free(mmm) ;
     RETURN(NULL) ;
   }
#pragma omp critical (MALLOC)
   mrv->fvec  = (float *)malloc(sizeof(float)*(size_t)nmask*(size_t)nvals) ;
   if( mrv->fvec == NULL ){
     ERROR_message("THD_dset_to_vectim: out of memory H") ;
     free(mrv->ivec) ; free(mrv) ; if( mmm != mask ) free(mmm) ;
     RETURN(NULL) ;
   }

   /* store desired voxel time series */

   for( kk=iv=0 ; iv < nvox ; iv++ ){
     if( mmm[iv] ) mrv->ivec[kk++] = iv ;  /* build index list */
   }

   if( nvals < DSET_NVALS(dset) ){ /* extract 1 at a time, save what we want */

     float *var;
#pragma omp critical (MALLOC)
     var = (float *)malloc(sizeof(float)*(DSET_NVALS(dset))) ;
     for( kk=iv=0 ; iv < nvox ; iv++ ){
       if( mmm[iv] == 0 ) continue ;
       (void)THD_extract_array( iv , dset , 0 , var ) ;
       AAmemcpy( VECTIM_PTR(mrv,kk) , var+start , sizeof(float)*nvals ) ;
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
   VECTIM_scan(mrv) ; /* 09 Nov 2010 */
   RETURN(mrv) ;
}

/*--------------------------------------------------------------------------*/

MRI_vectim * THD_dset_to_vectim_byslice( THD_3dim_dataset *dset, byte *mask ,
                                         int ignore , int kzbot , int kztop  )
{
   byte *mmm ;
   MRI_vectim *mrv=NULL ;
   int kk,iv , nvals , nvox , nmask , nxy , nz ;

ENTRY("THD_dset_to_vectim_byslice") ;

                     if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   DSET_load(dset) ; if( !DSET_LOADED(dset)  ) RETURN(NULL) ;

   nvals = DSET_NVALS(dset) ; if( nvals <= 0 ) RETURN(NULL) ;
   nvox  = DSET_NVOX(dset) ;

   nxy = DSET_NX(dset) * DSET_NY(dset) ; nz = DSET_NZ(dset) ;

   if( kzbot <  0  ) kzbot = 0 ;
   if( kztop >= nz ) kztop = nz-1 ;
   if( kztop < kzbot ) RETURN(NULL) ;
   if( kzbot == 0 && kztop == nz-1 ){
     mrv = THD_dset_to_vectim( dset , mask, ignore ) ; RETURN(mrv) ;
   }

   /* make a mask that includes cutting out un-desirable slices */

   { int ibot , itop , ii ;
#pragma omp critical (MALLOC)
     mmm = (byte *)malloc(sizeof(byte)*nvox) ;
     if( mask == NULL ) AAmemset( mmm ,    1 , sizeof(byte)*nvox ) ;
     else               AAmemcpy( mmm , mask , sizeof(byte)*nvox ) ;
     if( kzbot > 0 )
       AAmemset( mmm               , 0 , sizeof(byte)*kzbot       *nxy ) ;
     if( kztop < nz-1 )
       AAmemset( mmm+(kztop+1)*nxy , 0 , sizeof(byte)*(nz-1-kztop)*nxy ) ;
   }

   /* and make the vectim using the standard function */

   mrv = THD_dset_to_vectim( dset , mmm , ignore ) ;
   free(mmm) ;
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
#pragma omp critical (MALLOC)
         mmmv[id]   = (byte *)malloc(sizeof(byte)*nmaskv[id]) ;
         if( mmmv[id] == NULL ){
            ERROR_message("THD_2dset_to_vectim: out of memory I") ;
            RETURN(NULL) ;
         }
         memset( mmmv[id] , 1 , sizeof(byte)*nmaskv[id] ) ;
      }
   }

#pragma omp critical (MALLOC)
   mrv = (MRI_vectim *)malloc(sizeof(MRI_vectim)) ;

   mrv->nvec   = nmaskv[0]+nmaskv[1] ;
   mrv->nvals  = nvals ;
   mrv->ignore = ignore ;
#pragma omp critical (MALLOC)
   mrv->ivec   = (int *)malloc(sizeof(int)*(nmaskv[0]+nmaskv[1])) ;
#pragma omp critical (MALLOC)
   ivvectmp    = (int *)malloc(sizeof(int)*(nmaskv[1])) ;
   if( mrv->ivec == NULL || ivvectmp == NULL){
     ERROR_message("THD_2dset_to_vectim: out of memory J") ;
     if (mrv->ivec) free(mrv->ivec) ;
     if (ivvectmp)  free(ivvectmp) ;
     free(mrv) ;
     if( mmmv[0] != mask1 ) free(mmmv[0]) ;
     if( mmmv[1] != mask2 ) free(mmmv[1]) ;
     RETURN(NULL) ;
   }
#pragma omp critical (MALLOC)
   mrv->fvec  = (float *)malloc(sizeof(float)*(nmaskv[0]+nmaskv[1])*(size_t)nvals) ;
   if( mrv->fvec == NULL ){
     ERROR_message("THD_2dset_to_vectim: out of memory K") ;
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

     float *var;
#pragma omp critical (MALLOC)
     var = (float *)malloc(sizeof(float)*(nvals+ignore)) ;
     mmmt = mmmv[0];
     for( kk=iv=0 ; iv < nvoxv[0] ; iv++ ){
       if( mmmt[iv] == 0 ) continue ;
       (void)THD_extract_array( iv , dsetv[0] , 0 , var ) ;
       AAmemcpy( VECTIM_PTR(mrv,kk) , var+ignore , sizeof(float)*nvals ) ;
       kk++ ;
     }
     mmmt = mmmv[1];
     for(    iv=0 ; iv < nvoxv[1] ; iv++ ){
       if( mmmt[iv] == 0 ) continue ;
       (void)THD_extract_array( iv , dsetv[1] , 0 , var ) ;
       AAmemcpy( VECTIM_PTR(mrv,kk) , var+ignore , sizeof(float)*nvals ) ;
       kk++ ;
     }

     free(var) ;

   } else {  /* do all at once: this way is a lot faster */

     THD_extract_many_arrays( nmaskv[0] ,  mrv->ivec  ,
                               dsetv[0] ,   mrv->fvec  ) ;
     THD_extract_many_arrays( nmaskv[1] ,  ivvectmp,
                               dsetv[1] ,  (mrv->fvec+(size_t)nmaskv[0]*(size_t)mrv->nvals) ) ;

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
      fff=mrv->fvec+((size_t)mrv->nvals*(size_t)ShowThisTs);
      fprintf(stderr," %f \t", *(fff+kk));
     }
   }

   VECTIM_scan(mrv) ; /* 09 Nov 2010 */
   RETURN(mrv) ;
}

/*---------------------------------------------------------------------*/

MRI_vectim * THD_vectim_copy( MRI_vectim *mrv )  /* 08 Apr 2010 */
{
   MRI_vectim *qrv ;

   if( mrv == NULL ) return NULL ;

   MAKE_VECTIM( qrv , mrv->nvec , mrv->nvals ) ;
   qrv->ignore = mrv->ignore ;
   AAmemcpy( qrv->ivec , mrv->ivec , sizeof(int)*mrv->nvec ) ;
   AAmemcpy( qrv->fvec , mrv->fvec , sizeof(float)*(size_t)mrv->nvec*(size_t)mrv->nvals ) ;
   qrv->nx = mrv->nx ; qrv->dx = mrv->dx ;
   qrv->ny = mrv->ny ; qrv->dy = mrv->dy ;
   qrv->nz = mrv->nz ; qrv->dz = mrv->dz ; qrv->dt = mrv->dt ;
   return qrv ;
}

/*---------------------------------------------------------------------*/

MRI_vectim * THD_vectim_copy_nonzero( MRI_vectim *mrv ) /* 21 Sep 2010 */
{
   MRI_vectim *qrv ;
   int nvals , nvec , ii,jj , ngood ;
   float *mar , *qar ;

   if( mrv == NULL ) return NULL ;
   nvals = mrv->nvals ; nvec = mrv->nvec ;

   for( ngood=ii=0 ; ii < nvec ; ii++ ){
     mar = VECTIM_PTR(mrv,ii) ;
     for( jj=0 ; jj < nvals && mar[jj] == 0.0f ; jj++ ) ; /*nada*/
     if( jj < nvals ) ngood++ ;
   }
   if( ngood == 0    ) return NULL ;                 /* nothing to do */
   if( ngood == nvec ) return THD_vectim_copy(mrv) ; /* everything to do */

   MAKE_VECTIM( qrv , ngood , nvals ) ;
   qrv->ignore = mrv->ignore ;
   for( ngood=ii=0 ; ii < nvec ; ii++ ){
     mar = VECTIM_PTR(mrv,ii) ;
     for( jj=0 ; jj < nvals && mar[jj] == 0.0f ; jj++ ) ; /*nada*/
     if( jj < nvals ){
       qrv->ivec[ngood] = mrv->ivec[ii] ;
       qar = VECTIM_PTR(qrv,ngood) ;
       AAmemcpy(qar,mar,sizeof(float)*nvals) ;
       ngood++ ;
     }
   }

   qrv->nx = mrv->nx ; qrv->dx = mrv->dx ;
   qrv->ny = mrv->ny ; qrv->dy = mrv->dy ;
   qrv->nz = mrv->nz ; qrv->dz = mrv->dz ; qrv->dt = mrv->dt ;
   return qrv ;
}

/*---------------------------------------------------------------------*/
/* Apply a function to each time series, presumably to edit
   it in-place.  For 3dGroupInCorr -- 10 May 2012 -- RWCox.
*//*-------------------------------------------------------------------*/

void THD_vectim_applyfunc( MRI_vectim *mrv , void *vp )
{
   int iv , ii ;
   void (*fp)(int,float *) = (void (*)(int,float *))(vp) ;

   if( mrv == NULL || vp == NULL ) return ;

   for( iv=0 ; iv < mrv->nvec ; iv++ ) fp( mrv->nvals, VECTIM_PTR(mrv,iv) ) ;
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

 AFNI_OMP_START ;
#pragma omp parallel if( mrv->nvec > 1 && mrv->nvec * mrv->nvals > 999999 )
 { int nvec=mrv->nvec, nvals=mrv->nvals, nv1=nvals-1, iv, ii ; float sum, *fv ;
#pragma omp for
   for( iv=0 ; iv < nvec ; iv++ ){
     fv = VECTIM_PTR(mrv,iv) ;
     for( sum=0.0f,ii=0 ; ii < nv1 ; ii+=2 )
       sum += fv[ii]*vec[ii] + fv[ii+1]*vec[ii+1] ;
     if( ii == nv1 ) sum += fv[ii]*vec[ii] ;
     dp[iv] = (ata) ? logf((1.0001f+sum)/(1.0001f-sum)) : sum ;
   }
 } /* end OpenMP */
 AFNI_OMP_END ;

  thd_floatscan(mrv->nvec,dp) ;  /* 16 May 2013 */

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

   thd_floatscan(nvec,dp) ;  /* 16 May 2013 */

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
#pragma omp critical (MALLOC)
   av   = (float *)malloc(sizeof(float)*nvals) ;
#pragma omp critical (MALLOC)
   bv   = (float *)malloc(sizeof(float)*nvals) ;

   AAmemcpy( av , vec , sizeof(float)*nvals ) ;
   sav = spearman_rank_prepare( nvals , av ) ; if( sav <= 0.0f ) sav = 1.e+9f ;

   for( iv=0 ; iv < nvec ; iv++ ){
     AAmemcpy( bv , VECTIM_PTR(mrv,iv) , sizeof(float)*nvals ) ;
     dp[iv] = spearman_rank_corr( nvals , bv , sav , av ) ;
   }

   thd_floatscan(nvec,dp) ;  /* 16 May 2013 */

   free(bv) ; free(av) ; return ;
}

/*---------------------------------------------------------------------*/
/* 01 Mar 2010: Quadrant correlation. */

void THD_vectim_quadrant( MRI_vectim *mrv , float *vec , float *dp )
{
   float *av , *bv , sav ;
   int nvec, nvals, iv ;

   if( mrv == NULL || vec == NULL || dp == NULL ) return ;

   nvec = mrv->nvec ; nvals = mrv->nvals ;
#pragma omp critical (MALLOC)
   av   = (float *)malloc(sizeof(float)*nvals) ;
#pragma omp critical (MALLOC)
   bv   = (float *)malloc(sizeof(float)*nvals) ;

   AAmemcpy( av , vec , sizeof(float)*nvals ) ;
   sav = quadrant_corr_prepare( nvals , av ) ; if( sav <= 0.0f ) sav = 1.e+9f ;

   for( iv=0 ; iv < nvec ; iv++ ){
     AAmemcpy( bv , VECTIM_PTR(mrv,iv) , sizeof(float)*nvals ) ;
     dp[iv] = quadrant_corr( nvals , bv , sav , av ) ;
   }

   thd_floatscan(nvec,dp) ;  /* 16 May 2013 */

   free(bv) ; free(av) ; return ;
}

/*---------------------------------------------------------------------*/
/* 30 Mar 2011: TicTacToe correlation. */

void THD_vectim_tictactoe( MRI_vectim *mrv , float *vec , float *dp )
{
   float *av , *bv , sav ;
   int nvec, nvals, iv ;

   if( mrv == NULL || vec == NULL || dp == NULL ) return ;

   nvec = mrv->nvec ; nvals = mrv->nvals ;
#pragma omp critical (MALLOC)
   av   = (float *)malloc(sizeof(float)*nvals) ;
#pragma omp critical (MALLOC)
   bv   = (float *)malloc(sizeof(float)*nvals) ;

   { float tbot , ttop ;
     tbot = (float)AFNI_numenv("AFNI_TICTACTOE_BOT") ;
     ttop = (float)AFNI_numenv("AFNI_TICTACTOE_TOP") ;
     tictactoe_set_thresh( tbot , ttop ) ;
   }

   AAmemcpy( av , vec , sizeof(float)*nvals ) ;
   sav = tictactoe_corr_prepare( nvals , av ) ; if( sav <= 0.0f ) sav = 1.e+9f ;

   for( iv=0 ; iv < nvec ; iv++ ){
     AAmemcpy( bv , VECTIM_PTR(mrv,iv) , sizeof(float)*nvals ) ;
     dp[iv] = tictactoe_corr( nvals , bv , sav , av ) ;
   }

   thd_floatscan(nvec,dp) ;  /* 16 May 2013 */

   free(bv) ; free(av) ; return ;
}

/*----------------------------------------------------------------------------*/
/* 29 Apr 2010: Kendall Tau-b correlation. */

void THD_vectim_ktaub( MRI_vectim *mrv , float *vec , float *dp )
{
   float *av , *aav , *bv , *dv ;
   int nvec , nvals , iv , jv , *qv ;

ENTRY("THD_vectim_ktaub") ;

   if( mrv == NULL || vec == NULL || dp == NULL ) EXRETURN ;

   nvec = mrv->nvec ; nvals = mrv->nvals ;
#pragma omp critical (MALLOC)
   av   = (float *)malloc(sizeof(float)*nvals) ;
#pragma omp critical (MALLOC)
   aav  = (float *)malloc(sizeof(float)*nvals) ;
#pragma omp critical (MALLOC)
   bv   = (float *)malloc(sizeof(float)*nvals) ;
#pragma omp critical (MALLOC)
   qv   = (int   *)malloc(sizeof(int  )*nvals) ;

   AAmemcpy( av , vec , sizeof(float)*nvals ) ;
   for( jv=0 ; jv < nvals ; jv++ ) qv[jv] = jv ;
STATUS("qsort") ;
   qsort_floatint( nvals , av , qv ) ;

STATUS("loop") ;
   for( iv=0 ; iv < nvec ; iv++ ){
     dv = VECTIM_PTR(mrv,iv) ;
     for( jv=0 ; jv < nvals ; jv++ ) bv[jv] = dv[qv[jv]] ;
     AAmemcpy( aav , av , sizeof(float)*nvals) ;
     dp[iv] = kendallNlogN( aav , bv , nvals ) ;
   }

   thd_floatscan(nvec,dp) ;  /* 16 May 2013 */

   free(qv) ; free(bv) ; free(aav) ; free(av) ; EXRETURN ;
}

/*---------------------------------------------------------------------*/
/* 12 May 2012: Quantile correlation. */

void THD_vectim_quantile( MRI_vectim *mrv , float *vec , float *dp )
{
   float *av , *bv , sav ;
   int nvec, nvals, iv ;

   if( mrv == NULL || vec == NULL || dp == NULL ) return ;

   nvec = mrv->nvec ; nvals = mrv->nvals ;
#pragma omp critical (MALLOC)
   av   = (float *)malloc(sizeof(float)*nvals) ;
#pragma omp critical (MALLOC)
   bv   = (float *)malloc(sizeof(float)*nvals) ;

   AAmemcpy( av , vec , sizeof(float)*nvals ) ;
   sav = quantile_prepare( nvals , av ) ; if( sav <= 0.0f ) sav = 1.e+9f ;

   for( iv=0 ; iv < nvec ; iv++ ){
     AAmemcpy( bv , VECTIM_PTR(mrv,iv) , sizeof(float)*nvals ) ;
     dp[iv] = quantile_corr( nvals , bv , sav , av ) ;
   }

   thd_floatscan(nvec,dp) ;  /* 16 May 2013 */

   free(bv) ; free(av) ; return ;
}

/*---------------------------------------------------------------------*/
/* 04 May 2012: Distances. */
/* Special parameters:
   abs: 0 --> Euclidian distance
        1 --> City Block distance
   xform: String flags for transforming distance.
            If string contains "n_scale", scale distance by number
            of values (dimensions) at each voxel
            If string contains "inv", return the inverse of the distance
*/
void THD_vectim_distance( MRI_vectim *mrv , float *vec ,
                          float *dp, int abs, char *xform)
{

   if( mrv == NULL || vec == NULL || dp == NULL ) return ;

 AFNI_OMP_START ;
#pragma omp parallel if( mrv->nvec > 1 && mrv->nvec * mrv->nvals > 999999 )
   {  int nvec=mrv->nvec, nvals=mrv->nvals, nv1=nvals-1, iv, ii ;
      float sum, *fv, a1, a2;
#pragma omp for
      for( iv=0 ; iv < nvec ; iv++ ){
         fv = VECTIM_PTR(mrv,iv) ;
         for( sum=0.0f,ii=0 ; ii < nv1 ; ii+=2 ) {
            a1 = fv[ii]-vec[ii]; a2 = fv[ii+1]-vec[ii+1];
            if (!abs) sum += (a1*a1+a2*a2);
            else sum += (ABS(a1)+ABS(a2));
         }
         if( ii == nv1 ) {
            a1 = fv[ii]-vec[ii];
            if (!abs) sum += a1*a1;
            else sum += ABS(a1);
         }
         if (!abs) dp[iv] = sqrt(sum) ;
         else dp[iv] = sum;
      }
   } /* end OpenMP */
   AFNI_OMP_END ;

   if (xform) {
      float a1 = 1.0; int iv, nvec=mrv->nvec;
      if (strstr(xform,"n_scale")) { a1 = (float)mrv->nvals; }
      if (strstr(xform,"inv")) {
         for( iv=0 ; iv < nvec ; iv++ ) {
            if (dp[iv] != 0.0) {
               dp[iv] = a1/dp[iv];
            }
         }
      } else if (a1 != 1.0) {
         for( iv=0 ; iv < nvec ; iv++ ) {
            if (dp[iv] != 0.0) {
               dp[iv] = dp[iv]/a1;
            }
         }
      }
   }

   thd_floatscan(mrv->nvec,dp) ;  /* 16 May 2013 */

  return ;
}

/*----------------------------------------------------------------------------*/

void THD_vectim_pearson_section( MRI_vectim *mrv, float *vec,  /* 07 Oct 2014 */
                                 float *dp, int ibot, int itop )
{
   if( mrv == NULL || vec == NULL || dp == NULL ) return ;
   if( ibot <  0          ) ibot = 0 ;
   if( itop >= mrv->nvals ) itop = mrv->nvals-1 ;

 AFNI_OMP_START ;
#pragma omp parallel if( mrv->nvec > 1 && mrv->nvec * mrv->nvals > 999999 )
 { int nvec=mrv->nvec, nvals=itop-ibot+1, iv ; float sum , *fv ;
#pragma omp for
   for( iv=0 ; iv < nvec ; iv++ ){
     fv = VECTIM_PTR(mrv,iv) ;
     dp[iv] = THD_pearson_corr( nvals , vec+ibot , fv+ibot ) ;
   }
 } /* end OpenMP */
 AFNI_OMP_END ;

  thd_floatscan(mrv->nvec,dp) ;  /* 16 May 2013 */

  return ;
}



#define USE_VSTEP
#ifdef  USE_VSTEP
/*---------------------------------------------------------------------------*/
/*! For voxel loop progress report. */

static int vstep = 0 ;

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ; static int vn=0 ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}
#endif

/*----------------------------------------------------------------------------*/

void THD_vectim_pearsonBC( MRI_vectim *mrv, float srad, int sijk, int pv, float *par )
{
   MCW_cluster *smask ;
   int sqq,qq,pp,pijk,nsar,nyar,nlen,nx,ny,nz,nxy,ii,ik,ij,qi,qj,qk,qjk,mm,nmask ;
   float **sar, **yar ;

ENTRY("THD_vectim_pearsonBC") ;

   if( mrv == NULL || par == NULL ) EXRETURN ;
   sqq = THD_vectim_ifind( sijk , mrv ) ; if( sqq < 0 ) EXRETURN ;
   if( srad >= 0.0f ){
     srad  = MAX(srad,mrv->dx); srad = MAX(srad,mrv->dy); srad = MAX(srad,mrv->dz);
     smask = MCW_spheremask(mrv->dx,mrv->dy,mrv->dz,1.001f*srad) ;
   } else {
     srad  = MAX(-srad,1.01f) ;
     smask = MCW_spheremask(1.0f,1.0f,1.0f,srad) ;
   }
   nmask = smask->num_pt ;
   nlen = mrv->nvals ;

   nx = mrv->nx ; ny = mrv->ny ; nz = mrv->nz ; nxy = nx*ny ;
   ii = sijk % nx ; ik = sijk / nxy ; ij = (sijk-ik*nxy) / nx ;

#pragma omp critical (MALLOC)
   sar = (float **)malloc(sizeof(float *)*nmask) ;
#pragma omp critical (MALLOC)
   yar = (float **)malloc(sizeof(float *)*nmask) ;

   for( nsar=mm=0 ; mm < nmask ; mm++ ){
     qi  = ii + smask->i[mm] ; if( qi < 0 || qi >= nx ) continue ;
     qj  = ij + smask->j[mm] ; if( qj < 0 || qj >= ny ) continue ;
     qk  = ik + smask->k[mm] ; if( qk < 0 || qk >= nz ) continue ;
     qjk = qi + qj*nx + qk*nxy ;
     qq  = THD_vectim_ifind( qjk , mrv ) ;
     if( qq >= 0 ) sar[nsar++] = VECTIM_PTR(mrv,qq) ;
   }

#ifdef USE_VSTEP
   vstep = (mrv->nvec > 999) ? mrv->nvec/50 : 0 ;
   if( vstep ) fprintf(stderr," + Voxel loop [nmask=%d]: ",nmask) ;
#endif

   for( pp=0 ; pp < mrv->nvec ; pp++ ){
     if( pp == sqq ){ par[pp] = 1.0f ; continue ; }
#ifdef USE_VSTEP
     if( vstep && pp%vstep==vstep-1 ) vstep_print() ;
#endif
     pijk = mrv->ivec[pp] ;
     ii = pijk % nx ; ik = pijk / nxy ; ij = (pijk-ik*nxy) / nx ;
     for( nyar=mm=0 ; mm < nmask ; mm++ ){
       qi  = ii + smask->i[mm] ; if( qi < 0 || qi >= nx ) continue ;
       qj  = ij + smask->j[mm] ; if( qj < 0 || qj >= ny ) continue ;
       qk  = ik + smask->k[mm] ; if( qk < 0 || qk >= nz ) continue ;
       qjk = qi + qj*nx + qk*nxy ;
       qq  = THD_vectim_ifind( qjk , mrv ) ;
       if( qq >= 0 ) yar[nyar++] = VECTIM_PTR(mrv,qq) ;
     }
     par[pp] = THD_bootstrap_vectcorr( nlen , 50 , pv , 1 ,
                                       nsar,sar , nyar,yar ) ;
   }
#ifdef USE_VSTEP
   fprintf(stderr,"\n") ;
#endif

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

int THD_vectim_subset_average( MRI_vectim *mrv, int nind, int *ind, float *ar )
{
   int nvals , jj,kk,nkk=0 ; register int ii ; float *fv ;

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
       AAmemcpy( var+ign , VECTIM_PTR(mrv,kk) , sizeof(float)*nvals ) ;
       THD_insert_series( mrv->ivec[kk] , dset ,
                          nvals , MRI_float , var , 0 ) ;
     }
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------*/
/* The jj-th time point in the vectim goes to the tlist[jj]-th time
   point in the output dataset [06 Aug 2013].
*//*--------------------------------------------------------------------*/

void THD_vectim_to_dset_indexed( MRI_vectim *mrv ,
                                 THD_3dim_dataset *dset , int *tlist )
{
   int nvals , nvec ,  jj,kk , tmax=0 ;
   float *tar , *var ;

ENTRY("THD_vectim_to_dset_indexed") ;

   if( mrv == NULL || !ISVALID_DSET(dset) || tlist == NULL ) EXRETURN ;

   nvec  = mrv->nvec ;
   nvals = mrv->nvals ;

   for( kk=0 ; kk < nvals ; kk++ ){
     if( tlist[kk] < 0    ) EXRETURN ;
     if( tlist[kk] > tmax ) tmax = tlist[kk] ;
   }
   tmax++ ; if( DSET_NVALS(dset) < tmax ) EXRETURN ;

   tar = (float *)malloc(sizeof(float)*tmax) ;

   for( kk=0 ; kk < nvec ; kk++ ){
     var = VECTIM_PTR(mrv,kk) ;
     for( jj=0 ; jj < tmax  ; jj++ ) tar[jj]        = 0.0f    ;
     for( jj=0 ; jj < nvals ; jj++ ) tar[tlist[jj]] = var[jj] ;
     THD_insert_series( mrv->ivec[kk] , dset ,
                        tmax , MRI_float , tar , 0 ) ;
   }

   free(tar) ; EXRETURN ;
}

/*----------------------------------------------------------------------*/
/* The ilist[jj]-th point in the vectim goes into the jj-th index
   in the output dataset [06 Feb 2014].
*//*--------------------------------------------------------------------*/

void THD_vectim_indexed_to_dset( MRI_vectim *mrv, int nlist, int *ilist,
                                 THD_3dim_dataset *dset )
{
   int nvals , nvec ,  jj,kk ;
   float *tar , *var ;

ENTRY("THD_vectim_indexed_to_dset") ;

   if( mrv   == NULL || !ISVALID_DSET(dset) ||
       nlist <= 0    || ilist == NULL       || nlist > DSET_NVALS(dset)  ){
     ERROR_message("THD_vectim_indexed_to_dset: illegal inputs (nlist=%d)",nlist) ;
     EXRETURN ;
   }

   nvec  = mrv->nvec ;
   nvals = mrv->nvals ;

   for( kk=0 ; kk < nlist ; kk++ ){
     if( ilist[kk] < 0 || ilist[kk] >= nvals ){
       ERROR_message("THD_vectim_indexed_to_dset: illegal ilist[%d]=%d",kk,ilist[kk]) ;
       EXRETURN ;
     }
   }

   tar = (float *)malloc(sizeof(float)*nlist) ;

   for( kk=0 ; kk < nvec ; kk++ ){
     var = VECTIM_PTR(mrv,kk) ;
     for( jj=0 ; jj < nlist ; jj++ ) tar[jj] = var[ilist[jj]] ;
     THD_insert_series( mrv->ivec[kk] , dset ,
                        nlist , MRI_float , tar , 0 ) ;
   }

   free(tar) ; EXRETURN ;
}

/*---------------------------------------------------------------------------*/

MRI_vectim * THD_tcat_vectims( int nvim , MRI_vectim **vim )
{
   MRI_vectim *vout ;
   int iv , nvec , nvsum , vv , nvals ; size_t nvv ;
   float *vout_ptr , *vin_ptr ;

   if( nvim <= 0 || vim == NULL ) return NULL ;

   if( nvim == 1 ){
     vout = THD_vectim_copy( vim[0] ) ; return vout ;
   }

   nvec  = vim[0]->nvec ;
   nvsum = vim[0]->nvals ;
   for( iv=1 ; iv < nvim ; iv++ ){
     if( vim[iv]->nvec != nvec ) return NULL ;
     nvsum += vim[iv]->nvals ;
   }

   MAKE_VECTIM(vout,nvec,nvsum) ;
   vout->ignore = 0 ;
   vout->nx = vim[0]->nx ; vout->dx = vim[0]->dx ;
   vout->ny = vim[0]->ny ; vout->dy = vim[0]->dy ;
   vout->nz = vim[0]->nz ; vout->dz = vim[0]->dz ; vout->dt = vim[0]->dt ;
   AAmemcpy( vout->ivec , vim[0]->ivec , sizeof(int)*vim[0]->nvec ) ;

   for( nvv=iv=0 ; iv < nvim ; iv++,nvv+=nvals ){
     nvals = vim[iv]->nvals ;
     for( vv=0 ; vv < nvec ; vv++ ){
       vout_ptr = VECTIM_PTR(vout,vv) + nvv ;
       vin_ptr  = VECTIM_PTR(vim[iv],vv) ;
       AAmemcpy( vout_ptr , vin_ptr , sizeof(float)*nvals ) ;
     }
   }

   return vout ;
}

/*---------------------------------------------------------------------------*/

MRI_vectim * THD_dset_list_to_vectim( int nds, THD_3dim_dataset **ds, byte *mask )
{
   MRI_vectim *vout , **vim ;
   int kk , jj ;

   if( nds < 1 || ds == NULL ) return NULL ;

   if( nds == 1 ) return THD_dset_to_vectim( ds[0] , mask , 0 ) ;

   for( kk=0 ; kk < nds ; kk++ )
     if( !ISVALID_DSET(ds[kk]) ) return NULL ;

#pragma omp critical (MALLOC)
   vim = (MRI_vectim **)malloc(sizeof(MRI_vectim *)*nds) ;
   for( kk=0 ; kk < nds ; kk++ ){
     vim[kk] = THD_dset_to_vectim( ds[kk] , mask , 0 ) ;
     /** DSET_unload( ds[kk] ) ; **/
     if( vim[kk] == NULL ){
       for( jj=0 ; jj < kk ; jj++ ) VECTIM_destroy(vim[jj]) ;
       free(vim) ; return NULL ;
     }
   }

   vout = THD_tcat_vectims( nds , vim ) ;
   for( jj=0 ; jj < nds ; jj++ ) VECTIM_destroy(vim[jj]) ;
   free(vim) ; return vout ;
}

/*---------------------------------------------------------------------------*/

MRI_vectim * THD_dset_list_censored_to_vectim( int nds, THD_3dim_dataset **ds,
                                               byte *mask , int nkeep , int *keep )
{
   MRI_vectim *vout , **vim ;
   int kk , jj ;

   if( nds < 1 || ds == NULL ) return NULL ;

   if( nds == 1 )   /* trivial case */
     return THD_dset_censored_to_vectim( ds[0],mask,nkeep,keep );

   for( kk=0 ; kk < nds ; kk++ ){
     if( !ISVALID_DSET(ds[kk]) ) return NULL ;
     if( DSET_NVALS(ds[kk]) != DSET_NVALS(ds[0]) ) return NULL ;
   }

#pragma omp critical (MALLOC)
   vim = (MRI_vectim **)malloc(sizeof(MRI_vectim *)*nds) ;
   for( kk=0 ; kk < nds ; kk++ ){
     vim[kk] = THD_dset_censored_to_vectim( ds[kk] , mask , nkeep,keep ) ;
     /** DSET_unload( ds[kk] ) ; **/
     if( vim[kk] == NULL ){
       for( jj=0 ; jj < kk ; jj++ ) VECTIM_destroy(vim[jj]) ;
       free(vim) ; return NULL ;
     }
   }

   vout = THD_tcat_vectims( nds , vim ) ;
   for( jj=0 ; jj < nds ; jj++ ) VECTIM_destroy(vim[jj]) ;
   free(vim) ; return vout ;
}

/*---------------------------------------------------------------------------*/

void THD_check_vectim( MRI_vectim *mv , char *fname )
{
   int nvec , nvals ;
   float *vpt , vz ;
   int nbad , ii,jj ;

   if( fname == NULL ) fname = "vectim check" ;

   if( mv == NULL ){
     WARNING_message("%s :: bad input vector",fname); return;
   }

   nvec  = mv->nvec ;
   nvals = mv->nvals ;

   /* scan each time series for constancy */

   for( nbad=jj=0 ; jj < nvec ; jj++ ){
     vpt = VECTIM_PTR(mv,jj) ; vz = vpt[0] ;
     for( ii=1 ; ii < nvals && vpt[ii] == vz ; ii++ ) ; /*nada*/
     if( ii == nvals ) nbad++ ;
   }
   if( nbad > 0 && nvals > 1 )
     WARNING_message("%s :: %d vector%s constant",
                     fname , nbad , (nbad==1) ? " is" : "s are" ) ;

   /* scan each time point for constancy */

   for( nbad=ii=0 ; ii < nvals ; ii++ ){
     vpt = VECTIM_PTR(mv,0) ; vz = vpt[ii] ;
     for( jj=1 ; jj < nvec ; jj++ ){
       vpt = VECTIM_PTR(mv,jj) ; if( vpt[ii] != vz ) break ;
     }
     if( jj == nvec ) nbad++ ;
   }
   if( nbad > 0 && nvec > 1 )
     WARNING_message("%s :: %d volume%s constant",
                     fname , nbad , (nbad==1) ? " is" : "s are" ) ;
   return ;
}

/*---------------------------------------------------------------------*/

MRI_vectim * THD_xyzcat_vectims( int nvim , MRI_vectim **vim )
{
   MRI_vectim *vout ;
   int nx,ny,nz,nv , nvectot , iv,ii,jj ;
   float *vout_ptr , *vin_ptr ;

   if( nvim <= 0 || vim == NULL ) return NULL ;

   if( nvim == 1 ){
     vout = THD_vectim_copy( vim[0] ) ; return vout ;
   }

   nx = vim[0]->nx ;
   ny = vim[0]->ny ;
   nz = vim[0]->nz ;
   nv = vim[0]->nvals ;
   nvectot = vim[0]->nvec ;

   for( iv=1 ; iv < nvim ; iv++ ){
     if( vim[iv]->nx    != nx ||
         vim[iv]->ny    != ny ||
         vim[iv]->nz    != nz ||
         vim[iv]->nvals != nv   ) return NULL ;

     nvectot += vim[iv]->nvec ;
   }

   MAKE_VECTIM( vout , nvectot , nv ) ;
   vout->nx = nx ; vout->dx = vim[0]->dx ;
   vout->ny = ny ; vout->dy = vim[0]->dy ;
   vout->nz = nz ; vout->dz = vim[0]->dz ; vout->dt = vim[0]->dt ;

   for( jj=iv=0 ; iv < nvim ; iv++ ){
     for( ii=0 ; ii < vim[iv]->nvec ; ii++,jj++ ){
       vout->ivec[jj] = vim[iv]->ivec[ii] ;
       memcpy( VECTIM_PTR(vout,jj) , VECTIM_PTR(vim[iv],ii) , sizeof(float)*nv ) ;
     }
   }

   return vout ;
}
