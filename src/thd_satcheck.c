#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

float THD_saturation_check( THD_3dim_dataset *dset , byte *xmask )
{
   byte *mask=xmask ;
   int nvals , nvox , nchek , nmask , qq ; byte *nbig ; float sum ;

   if( !ISVALID_DSET(dset) ) return 0.0f ;
   nvals = DSET_NVALS(dset) ; if( nvals < 9 ) return 0.0f ;
   nvox  = DSET_NVOX(dset) ;
   nchek = nvals / 4 ; nchek = MAX(nchek,3) ; nchek = MIN(nchek,255) ;

   if( mask == NULL ){
     mask = THD_automask(dset) ; if( mask == NULL ) return 0.0f ;
   }
   nmask = THD_countmask( nvox , mask ) ;
   if( nmask <= 0 ){
     if( mask != xmask ) free(mask) ;
     return 0.0f ;
   }
   nbig = (byte *)calloc(sizeof(byte),nvox) ;

#pragma omp parallel if( nvox > 666 )
 { float med,mad,thr,*far ; int kk , vv ;
 AFNI_OMP_START ;

#pragma omp critical(MALLOC)
   far = (float *)malloc(sizeof(float)*nvals) ;

   for( kk=0 ; kk < nvox ; kk++ ){
     if( !mask[kk] ) continue ;
     (void)THD_extract_array( kk , dset , 0 , far ) ;
     qmedmad_float( nvals , far , &med , &mad ) ;
     thr = med + 4.444f*mad ;
     for( vv=0 ; vv < nchek && far[vv] > thr ; vv++ ) ; /*nada*/
     nbig[kk] = (byte)vv ;
   }
   free(far) ;

 AFNI_OMP_END ;
 }

   if( mask != xmask ) free(mask) ;
   for( sum=0.0f,qq=0 ; qq < nvox ; qq++ ) sum += nbig[qq] ;
   return (sum/nmask) ;
}
