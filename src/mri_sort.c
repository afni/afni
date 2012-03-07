#include "mrilib.h"

void mri_xsort_inplace( MRI_IMAGE *im , int rev )
{
   float *far , *car ;
   int nx , nc , ii,jj ;

   if( im == NULL ) return ;

   nx  = im->nx ; if( nx < 2 ) return ;
   nc  = im->nvox / nx ;

   switch( im->kind ){

     default: break ;

     case MRI_float:{
       float *far = MRI_FLOAT_PTR(im) , *car ;
       for( jj=0 ; jj < nc ; jj++ ){
         car = far+jj*nx ;
         if( rev ) for( ii=0 ; ii < nx ; ii++ ) car[ii] = -car[ii] ;
         qsort_float( nx , car ) ;
         if( rev ) for( ii=0 ; ii < nx ; ii++ ) car[ii] = -car[ii] ;
       }
     }
     break ;

     case MRI_short:{
       short *sar = MRI_SHORT_PTR(im) , *car ;
       for( jj=0 ; jj < nc ; jj++ ){
         car = sar+jj*nx ;
         if( rev ) for( ii=0 ; ii < nx ; ii++ ) car[ii] = -car[ii] ;
         qsort_short( nx , car ) ;
         if( rev ) for( ii=0 ; ii < nx ; ii++ ) car[ii] = -car[ii] ;
       }
     }
     break ;
   }

   return ;
}

/*-------------------------------------------------------------------------*/
/* Sort on column #jc -- only works for float data at this time. */

void mri_csort_inplace( MRI_IMAGE *im , int rev , int jc )
{
   float *far , *car , *qar ;
   int nx , nc , ii,jj,kk , *iar ;
   MRI_IMAGE *qim ;

   if( im == NULL || im->kind != MRI_float ) return ;

   nx  = im->nx ; if( nx < 2 ) return ;
   nc  = im->nvox / nx ;

   if( nc == 1 ){ mri_xsort_inplace(im,rev); return; }

   car = (float *)malloc(nx*sizeof(float)) ;
   iar = (int   *)malloc(nx*sizeof(int  )) ;
   far = MRI_FLOAT_PTR(im) ;

   if( jc < 0 ) jc = 0 ; else if( jc >= nc ) jc = nc-1 ;

   for( ii=0 ; ii < nx ; ii++ ){
     iar[ii] = ii ;
     car[ii] = far[ii+jc*nx] ; if( rev ) car[ii] = -car[ii] ;
   }

   qsort_floatint(nx,car,iar) ; free(car) ;

   qim = mri_new_conforming(im,MRI_float) ;
   qar = MRI_FLOAT_PTR(qim) ;

   for( ii=0 ; ii < nx ; ii++ ){
     kk = iar[ii] ;
     for( jj=0 ; jj < nc ; jj++ ) qar[ii+jj*nx] = far[kk+jj*nx] ;
   }

   free(iar) ;

   memcpy(far,qar,sizeof(float)*nx*nc) ; mri_free(qim) ;
   return ;
}
