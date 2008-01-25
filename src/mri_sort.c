#include "mrilib.h"

void mri_xsort_inplace( MRI_IMAGE *im , int rev )
{
   float *far , *car ;
   int nx , nc , ii,jj ;

   if( im == NULL ) return ;

   nx  = im->nx ; if( nx < 2 ) return ;
   nc  = im->nvox / nx ;

   switch( im->kind ){

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
