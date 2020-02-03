#include "mrilib.h"

MRI_IMAGE * mri_to_pval( MRI_IMAGE *im , int statcode , float *stataux )
{
   int nvox , ii ;
   MRI_IMAGE *oim ;
   float *far , *oar ;

ENTRY("mri_to_pval") ;

   if( im == NULL || im->kind != MRI_float || !FUNC_IS_STAT(statcode) ) RETURN(NULL) ;
   far = MRI_FLOAT_PTR(im) ; if( im == NULL ) RETURN(NULL) ;
   nvox = im->nvox ;

   oim = mri_new_conforming( im , MRI_float ) ;
   oar = MRI_FLOAT_PTR(oim) ;

   for( ii=0 ; ii < nvox ; ii++ ){
     if( far[ii] != 0.0f ) oar[ii] = THD_stat_to_pval( fabsf(far[ii]), statcode,stataux ) ;
     else                  oar[ii] = 1.0f ;
   }

   RETURN(oim) ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_to_zscore( MRI_IMAGE *im , int statcode , float *stataux )
{
   int nvox , ii ;
   MRI_IMAGE *oim ;
   float *far , *oar ;

ENTRY("mri_to_zscore") ;

   if( im == NULL || im->kind != MRI_float || !FUNC_IS_STAT(statcode) ) RETURN(NULL) ;
   far = MRI_FLOAT_PTR(im) ; if( im == NULL ) RETURN(NULL) ;
   nvox = im->nvox ;

   oim = mri_new_conforming( im , MRI_float ) ;
   oar = MRI_FLOAT_PTR(oim) ;

   for( ii=0 ; ii < nvox ; ii++ )
     oar[ii] = THD_stat_to_zscore( far[ii], statcode,stataux ) ;

   RETURN(oim) ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_to_qval( MRI_IMAGE *im , floatvec *fdrcurve ) /* 01 Feb 2020 */
{
   int nvox , ii ;
   MRI_IMAGE *oim ;
   float *far , *oar ;
   float zval , qval ;

ENTRY("mri_to_qval") ;

   if( im == NULL || im->kind != MRI_float || fdrcurve == NULL ) RETURN(NULL) ;
   far = MRI_FLOAT_PTR(im) ; if( im == NULL ) RETURN(NULL) ;
   nvox = im->nvox ;

   oim = mri_new_conforming( im , MRI_float ) ;
   oar = MRI_FLOAT_PTR(oim) ;

   for( ii=0 ; ii < nvox ; ii++ ){
     qval = 1.0f ;
     if( far[ii] != 0.0f ){
       zval = interp_floatvec(fdrcurve,fabsf(far[ii])) ;
       if( zval > 0.0f ) qval = 2.0*qg(zval) ;
     }
     oar[ii] = qval ;
   }

   RETURN(oim) ;
}
