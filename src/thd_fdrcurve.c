#include "mrilib.h"

/*-------------------------------------------------------------------------*/
/*! Count how many FDR curves need to be created. [12 Nov 2008] */

int THD_count_fdrwork( THD_3dim_dataset *dset )
{
   int iv , nfdr , sc ;

ENTRY("THD_count_fdrwork") ;

   if( !ISVALID_DSET(dset) ) RETURN(0) ;

   for( nfdr=iv=0 ; iv < DSET_NVALS(dset) ; iv++ ){
     sc = DSET_BRICK_STATCODE(dset,iv) ;
     if( FUNC_IS_STAT(sc) ) nfdr++ ;
   }

   RETURN(nfdr) ;
}

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
   int iv , nfdr , kk ; float qmin ;

ENTRY("THD_create_all_fdrcurves") ;

   if( !ISVALID_DSET(dset) ) RETURN(0) ;

   for( nfdr=iv=0 ; iv < dset->dblk->nvals ; iv++ ){
     kk = THD_create_one_fdrcurve( dset , iv ) ; nfdr += kk ;

     if( kk ){     /* print warning message for 'best' q that is big */
       qmin = DSET_BRICK_FDRMIN(dset,iv) ;
       if( qmin > 0.1f )
         WARNING_message(
          "Smallest FDR q [%d %s] = %g ==> very few (if any) true detections" ,
          iv , DSET_BRICK_LABEL(dset,iv) , qmin ) ;
     }

   }

   RETURN(nfdr) ;
}

/*-------------------------------------------------------------------------*/
/* convert thresh to z(q) by interpolation */

float THD_fdrcurve_zval( THD_3dim_dataset *dset , int iv , float thresh )
{
   floatvec *fv ;

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
/* convert z(q) to thresh by inverse interpolation
   (I don't recall why I set the suffix of this function to 'zqtot'.)
*//*-----------------------------------------------------------------------*/

float THD_fdrcurve_zqtot( THD_3dim_dataset *dset , int iv , float zval )
{
   floatvec *fv ; float thr ;

   if( !ISVALID_DSET(dset) || iv < 0 || iv >= DSET_NVALS(dset) ) return 0.0f ;

   fv = DSET_BRICK_FDRCURVE(dset,iv) ;
   if( fv == NULL ){
     if( dset->warp_parent != NULL )
       fv = DSET_BRICK_FDRCURVE(dset->warp_parent,iv) ;
     if( fv == NULL ) return 0.0f ;
   }

   if( zval > fv->ar[fv->nar-1] ){           /* larger than the largest value */
     float mab = DSET_BSTAT_MAXABS(dset,iv) ; /* (that is, qval is too small) */
     thr = fv->x0 + fv->dx*fv->nar ;
     if( mab >= thr ) thr = mab*1.000002f ;
   } else if( zval < fv->ar[0] ){                /* smaller than the smallest */
     thr = 0.0f ;                               /* (that is, qval is too big) */
   } else {
     thr = interp_inverse_floatvec(fv,zval) ;            /* qval is copasetic */
   }

#if 0
{ int itop = fv->nar - 1 ;
INFO_message("zqtot: zval=%g  x0=%g z0=%g  xtop=%g ztop=%g  thr=%g",
              zval , fv->x0,fv->ar[0] , fv->x0+fv->dx*itop,fv->ar[itop] , thr ) ;
}
#endif

   return thr ;
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
