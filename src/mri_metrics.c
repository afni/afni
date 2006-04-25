#include "mrilib.h"

void mri_metrics( MRI_IMAGE *imp , MRI_IMAGE *imq , float *met )
{
   int nvox ;
   int *rst , *pst , *qst ;
   byte *par, *qar ;
   float qj,rij , rat,tmp ;
   float esum,tsum,hsum ;
   register int ii,jj,kk ;
   MRI_IMAGE *imqq, *impp ;

   if( imp == NULL || imq == NULL            ) return ;
   if( met == NULL || imp->nvox != imq->nvox ) return ;

   nvox = imp->nvox ;

   impp = (imp->kind==MRI_byte) ? imp : mri_to_byte(imp) ;
   imqq = (imq->kind==MRI_byte) ? imq : mri_to_byte(imq) ;
   par  = MRI_BYTE_PTR(impp) ;
   qar  = MRI_BYTE_PTR(imqq) ;

   pst = (int *)calloc(256    ,sizeof(int)) ;
   qst = (int *)calloc(256    ,sizeof(int)) ;
   rst = (int *)calloc(256*256,sizeof(int)) ;

   for( kk=0 ; kk < nvox ; kk++ ){
     ii = par[kk] ; jj = qar[kk] ;
     pst[ii]++ ; qst[jj]++ ; rst[ii+256*jj]++ ;
   }

   esum = tsum = hsum = 0.0f ;
   for( jj=0 ; jj < 256 ; jj++ ){
     qj = (float)qst[jj] ;
     if( qj > 0.0f ){
       kk = 256*jj ;
       for( ii=0 ; ii < 256 ; ii++ ){
         rij = (float)rst[ii+kk] ;
         if( rij > 0.0f ){
           rat   = (qj *(float)pst[ii]) / rij ;
                                    esum -= rij * log(rat) ;
           tmp   = sqrt(rat)-1.0f ; hsum += rij * tmp*tmp ;
           tmp   = rat-1.0f ;       tsum += rij * tmp*tmp/(rat+1.0f) ;
         }
       }
     }
   }

   free((void*)rst); free((void*)qst); free((void*)pst);
   if( impp != imp ) mri_free(impp);
   if( imqq != imq ) mri_free(imqq);

   tmp = 1.0f / nvox ; tmp = tmp*tmp ;

   met[METRIC_KULL] = tmp * esum ;
   met[METRIC_HELL] = tmp * hsum * 2.0f ;
   met[METRIC_TRIA] = tmp * tsum ;
   return ;
}
