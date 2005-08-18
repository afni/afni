#include "mrilib.h"

float mri_nstat( int code , MRI_IMAGE *im )
{
   MRI_IMAGE *fim ;
   float     *far , *tar , outval=0.0f ;
   int npt , ii ;

   if( im == NULL || im->nvox == 0 ) return 0.0f ;

   if( im->kind != MRI_float ) fim = mri_to_float(im) ;
   else                        fim = im ;
   tar = far = MRI_FLOAT_PTR(fim) ;
   npt = fim->nvox ;

   if( fim == im ){   /* copy far into tar, if calculation is destructive */
     switch( code ){
       case NSTAT_MEDIAN:
       case NSTAT_MAD:
         tar = (float *)malloc(sizeof(float)*npt) ;
         memcpy(tar,far,sizeof(float)*npt) ;
       break ;
     }
   }


   /* cleanup and exit */

   if( tar != far ) free((void *)tar) ;
   if( fim != im  ) mri_free(fim) ;
   return outval ;
}
