#include "mrilib.h"

/*------------------------------------------------------------------------*/
/*! Computes various metrics betweeen the histograms (joint and marginal)
    of two images.  At this stage, experimental -- RWCox - 25 Apr 2006.
--------------------------------------------------------------------------*/

void mri_metrics( MRI_IMAGE *imp , MRI_IMAGE *imq , float *met )
{
   int nvox ;
   int *rst , *pst , *qst ;
   byte *par, *qar ;
   float qj,rij , rat,tmp,lrr,rm1,rp1 , fac ;
   float esum,tsum,hsum , jsum,dsum,xsum , qsum,asum ;
   register int ii,jj,kk ;
   MRI_IMAGE *imqq, *impp ;

   if( imp == NULL || imq == NULL            ) return ;
   if( met == NULL || imp->nvox != imq->nvox ) return ;

   nvox = imp->nvox ; fac = 1.0f / nvox ;

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
   jsum = dsum = xsum = 0.0f ;
   qsum = asum =        0.0f ;
   for( jj=0 ; jj < 256 ; jj++ ){
     qj = (float)qst[jj] ;
     if( qj > 0.0f ){
       kk = 256*jj ; qj *= fac ;
       for( ii=0 ; ii < 256 ; ii++ ){
         rij = (float)rst[ii+kk] ;
         if( rij > 0.0f ){
           rat = (qj *(float)pst[ii]) / rij ;
           lrr = logf(rat) ; rm1 = rat-1.0f ; rp1 = rat+1.0f ;

           esum += rij * lrr ;
           jsum += rij * lrr*rm1 ;
           tsum += rij * rm1*rm1/rp1 ;
           dsum += rij * (rat*lrr - rp1*logf(0.5*rp1)) ;
           xsum += rij * rm1*rm1*rp1/rat ;
#if 0
           tmp   = sqrtf(rat)-1.0f ; hsum += rij * tmp*tmp ;
#else
           tmp   = 1.0f/sqrtf(rat)-1.0f ; hsum += rij * tmp ;
#endif

           qsum += rij * (1.0f/rat-1.0f) ;
           tmp   = 0.5*rp1 ; asum += rij * tmp * logf(tmp/sqrtf(rat)) ;
         }
       }
     }
   }

   free((void*)rst); free((void*)qst); free((void*)pst);
   if( impp != imp ) mri_free(impp);
   if( imqq != imq ) mri_free(imqq);

   met[METRIC_KULL] = -fac * esum ;
   met[METRIC_HELL] =  fac * hsum ;
   met[METRIC_TRIA] =  fac * tsum ;
   met[METRIC_JDIV] =  fac * jsum * 0.50f ;
   met[METRIC_JSDV] =  fac * dsum * 2.00f ;
   met[METRIC_XISQ] =  fac * xsum * 0.25f ;
   met[METRIC_XXSQ] =  fac * xsum * 0.50f ;
   met[METRIC_AGDV] =  fac * asum ;
   return ;
}
