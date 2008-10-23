#include "mrilib.h"

/*-------------------------------------------------------------------------*/
/*! Create the FDR curve for the iv-th sub-brick, if it is a statistical
    sub-brick, and store it in the dataset struct.
*//*-----------------------------------------------------------------------*/

int THD_create_one_fdrcurve( THD_3dim_dataset *dset , int iv )
{
   int sc ;
   floatvec *fv , *mv ;
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
     if( qim == NULL ) RETURN(0) ;
   } else {
     qim = bim ;
   }

   if( PRINT_TRACING ){
     char str[256]; sprintf(str,"FDR-izing sub-brick #%d",iv); STATUS(str);
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

     fv = mri_fdr_getmdf() ;  /* 22 Oct 2008 */
     if( fv != NULL ){
       COPY_floatvec(mv,fv) ;
       if( dset->dblk->brick_mdfcurve == NULL )
         dset->dblk->brick_mdfcurve = (floatvec **)calloc(sizeof(floatvec *),
                                                          dset->dblk->nvals  ) ;
       else if( dset->dblk->brick_mdfcurve[iv] != NULL )
         KILL_floatvec( dset->dblk->brick_mdfcurve[iv] ) ;

       if( PRINT_TRACING ){
         char str[256]; sprintf(str,"MDF-izing sub-brick #%d",iv); STATUS(str);
       }

       dset->dblk->brick_mdfcurve[iv] = mv ;
     }

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

   fv = DSET_BRICK_FDRCURVE(dset,iv) ;
   if( fv == NULL ){
     if( dset->warp_parent != NULL )
       fv = DSET_BRICK_FDRCURVE(dset->warp_parent,iv) ;
     if( fv == NULL ) return 0.0f ;
   }

   return ( interp_floatvec(fv,thresh) ) ;
}

/*-------------------------------------------------------------------------*/

float THD_mdfcurve_mval( THD_3dim_dataset *dset , int iv , float pval )
{
   floatvec *fv ; float plog , val ;

   if( !ISVALID_DSET(dset) || iv < 0 || iv >= DSET_NVALS(dset) ) return -1.0f ;

   fv = DSET_BRICK_MDFCURVE(dset,iv) ;
   if( fv == NULL ){
     if( dset->warp_parent != NULL )
       fv = DSET_BRICK_MDFCURVE(dset->warp_parent,iv) ;
     if( fv == NULL ) return -3.0f ;
   }
        if( pval <= 0.0f ) return 0.999f ;
   else if( pval >= 1.0f ) return 0.000f ;

   plog = log10(pval) ;
   return ( interp_floatvec(fv,plog) ) ;
}
