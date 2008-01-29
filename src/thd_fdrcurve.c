#include "mrilib.h"

/*-------------------------------------------------------------------------*/
/*! Create the FDR curve for the iv-th sub-brick, if it is a statistical
    sub-brick, and store it in the dataset struct.
*//*-----------------------------------------------------------------------*/

int THD_create_one_fdrcurve( THD_3dim_dataset *dset , int iv )
{
   int sc ;
   floatvec *fv ;
   MRI_IMAGE *bim , *qim=NULL ;

ENTRY("THD_create_one_fdrcurve") ;

   if( !ISVALID_DSET(dset) ) RETURN(0) ;
   if( iv < 0 || iv >= DSET_NVALS(dset) ) RETURN(0) ;
   sc = DSET_BRICK_STATCODE(dset,iv) ;
   if( !FUNC_IS_STAT(sc) ) RETURN(0) ;
   if( !DSET_LOADED(dset) ) DSET_load(dset) ;
   bim = DSET_BRICK(dset,iv) ;
   if( bim->kind != MRI_float ){
     float fac = DSET_BRICK_FACTOR(dset,iv) ;
     qim = mri_scale_to_float( fac , bim ) ;
   } else {
     qim = bim ;
   }

   fv = mri_fdr_curve( qim , sc , DSET_BRICK_STATAUX(dset,iv) ) ;

   if( qim != bim ) mri_free(qim) ;

   if( fv != NULL ){

     if( dset->dblk->brick_fdrcurve == NULL )
       dset->dblk->brick_fdrcurve = (floatvec **)calloc(sizeof(floatvec *),
                                                        dset->dblk->nvals  ) ;
     else if( dset->dblk->brick_fdrcurve[iv] != NULL )
       KILL_floatvec( dset->dblk->brick_fdrcurve[iv] ) ;

     dset->dblk->brick_fdrcurve[iv] = fv ;
   }

   RETURN(1) ;
}

/*-------------------------------------------------------------------------*/

int THD_create_all_fdrcurves( THD_3dim_dataset *dset )
{
   int iv , nfdr=0 ;

ENTRY("THD_create_all_fdrcurves") ;

   if( !ISVALID_DSET(dset) ) RETURN(0) ;

   for( iv=0 ; iv < dset->dblk->nvals ; iv++ )
     nfdr += THD_create_one_fdrcurve( dset , iv ) ;

   RETURN(nfdr) ;
}

/*-------------------------------------------------------------------------*/

float THD_fdrcurve_zval( THD_3dim_dataset *dset , int iv , float thresh )
{
   floatvec *fv ; float val ;

   if( !ISVALID_DSET(dset) || iv < 0 || iv >= DSET_NVALS(dset) ) return 0.0f ;

   fv = DSET_BRICK_FDRCURVE(dset,iv) ; if( fv == NULL ) return 0.0f ;

   return ( interp_floatvec(fv,thresh) ) ;
}
