#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

/*--------------------------------------------------------------------------*/
/* Check for initial positive transients.  Return value is average number
   of "large" values at the start of dataset time series.  Purely for
   informational purposes.  [08 Feb 2010]
*//*------------------------------------------------------------------------*/

float THD_saturation_check( THD_3dim_dataset *dset , byte *xmask )
{
   byte *mask=xmask ;
   int nvals, nuse, nvox, nchek, nmask, qq ; byte *nbig ; float sum ;

   if( !ISVALID_DSET(dset) ) return 0.0f ;
   nvals = DSET_NVALS(dset) ; if( nvals < 9 ) return 0.0f ;
   nvox  = DSET_NVOX(dset) ;
   nchek = nvals / 8 ; nchek = MAX(nchek,3) ; nchek = MIN(nchek,16) ;
   nuse  = MIN(nvals,100) - nchek ;

   if( mask == NULL ){
     THD_automask_set_cheapo(1) ;
     mask = THD_automask(dset) ;
     if( mask == NULL ) return 0.0f ;
   }
   nmask = THD_countmask( nvox , mask ) ;
   if( nmask <= 0 ){
     if( mask != xmask ) free(mask) ;
     return 0.0f ;
   }
   nbig = (byte *)calloc(sizeof(byte),nvox) ;

 AFNI_OMP_START ;
#pragma omp parallel if( nvox > 666 )
 { float med,mad,thp,thm,*far ; int kk , vv ;

#pragma omp critical(MALLOC)
   far = (float *)malloc(sizeof(float)*nvals) ;

   for( kk=0 ; kk < nvox ; kk++ ){
     if( !mask[kk] ) continue ;
     (void)THD_extract_array( kk , dset , 0 , far ) ;
     qmedmad_float( nuse , far+nchek , &med , &mad ) ;
     if( mad == 0.0f ) continue ;
     thp = med + 5.678f*mad ;
#if 0
     thm = med - 9.876f*mad ;
     for( vv=0 ; vv < nchek && (far[vv] > thp || far[vv] < thm); vv++ ) ; /*nada*/
#else
     for( vv=0 ; vv < nchek && far[vv] > thp ; vv++ ) ; /*nada*/
#endif
     nbig[kk] = (byte)vv ;
   }
   free(far) ;

 }
 AFNI_OMP_END ;

   if( mask != xmask ) free(mask) ;
   for( sum=0.0f,qq=0 ; qq < nvox ; qq++ ) sum += nbig[qq] ;
   free(nbig) ;
   return (sum/nmask) ;
}
