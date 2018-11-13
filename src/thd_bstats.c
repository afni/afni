/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*-------------------------------------------------------------------------*/
/*! Load the statistics of a dataset [modified Nov 15 1995]
---------------------------------------------------------------------------*/

void THD_load_statistics( THD_3dim_dataset *dset )
{
   int ii , mmin , mmax , ibr , nbr ;
   short *brkk ;
   THD_brick_stats *bsold ;

   /*-- sanity checks --*/

   if( ! ISVALID_3DIM_DATASET(dset) ) return ;

   nbr = THD_count_databricks( dset->dblk ) ;

   /*-- 3/24/95: if don't get data, try for the warp parent --*/

   if( nbr == 0 ){

     if( ! ISVALID_3DIM_DATASET(dset->warp_parent) ) return ;     /* nothing */
     if( dset->warp_parent == dset )                 return ;

     RELOAD_STATS( dset->warp_parent ) ;                       /* recursion! */
     if( ! ISVALID_STATISTIC(dset->warp_parent->stats) ) return ; /* nothing */

     if( dset->stats == NULL ){                     /* create if not present */
       dset->stats = myRwcNew( THD_statistics ) ;
       ADDTO_KILL( dset->kl , dset->stats ) ;
       dset->stats->type   = STATISTICS_TYPE ;
       dset->stats->parent = (RwcPointer) dset ;
       dset->stats->bstat  = NULL ;
     }

     bsold               = dset->stats->bstat ;
     dset->stats->nbstat = dset->dblk->nvals ;
     dset->stats->bstat  = (THD_brick_stats *)
                            RwcRealloc( (char *) bsold ,
                                sizeof(THD_brick_stats) * dset->dblk->nvals ) ;
     if( bsold != dset->stats->bstat )
        REPLACE_KILL( dset->kl , bsold , dset->stats->bstat ) ;

     /** copy stats from warp parent for each brick **/

     for( ibr=0 ; ibr < dset->dblk->nvals ; ibr++ ){
       if( ibr < dset->warp_parent->stats->nbstat )
         dset->stats->bstat[ibr] = dset->warp_parent->stats->bstat[ibr] ;
       else
         INVALIDATE_BSTAT( dset->stats->bstat[ibr] ) ;
     }

     return ;
   }

   /*-- if here, have good data in this dataset --*/

   if( dset->stats == NULL ){                  /* create if not present */
     dset->stats = myRwcNew( THD_statistics ) ;
     ADDTO_KILL( dset->kl , dset->stats ) ;
     dset->stats->type   = STATISTICS_TYPE ;
     dset->stats->parent = (RwcPointer) dset ;
     dset->stats->bstat  = NULL ;
   }

   bsold               = dset->stats->bstat ;
   dset->stats->nbstat = dset->dblk->nvals ;
   dset->stats->bstat  = (THD_brick_stats *)
                          RwcRealloc( (char *) bsold ,
                              sizeof(THD_brick_stats) * dset->dblk->nvals ) ;
   if( bsold != dset->stats->bstat )
     REPLACE_KILL( dset->kl , bsold , dset->stats->bstat ) ;

   /* 3/24/95: load stats for each sub-brick, not just the first */

   for( ibr=0 ; ibr < dset->dblk->nvals ; ibr++ ){  /* for each sub-brick */
     dset->stats->bstat[ibr] = THD_get_brick_stats( DSET_BRICK(dset,ibr) ) ;

     /* 11/21/95: allow for scaling factor that may be applied to data */

     if( DSET_BRICK_FACTOR(dset,ibr) > 0.0 ){
       dset->stats->bstat[ibr].min *= DSET_BRICK_FACTOR(dset,ibr) ;
       dset->stats->bstat[ibr].max *= DSET_BRICK_FACTOR(dset,ibr) ;
     }
   }
   return ;
}

/*--------------------------------------------------------------*/
/*! Compute statistics for a 3D brick of varying data type
----------------------------------------------------------------*/

THD_brick_stats THD_get_brick_stats( MRI_IMAGE *im )
{
   int64_t ii , nvox ;  /* allow for big volumes    26 Mar 2016 [rickr] */
   register float bot , top ;
   void *br ;
   THD_brick_stats bst ;

   bst.min = bst.max = 0 ;  /* got to give them something */

   if( im == NULL ) return bst ;
   br   = mri_data_pointer( im ) ; if( br == NULL ) return bst ;
   nvox = im->nvox ;

   switch( im->kind ){

     default:
        INVALIDATE_BSTAT(bst) ; bot = top = 0 ;
     break ;

     case MRI_rgb:{
       register byte *ar = (byte *) br ; register float val ;
       bot = top = 0 ;
       for( ii=0 ; ii < nvox ; ii++ ){      /* scale to brightness */
         val =  0.299 * ar[3*ii]           /* between 0 and 255   */
              + 0.587 * ar[3*ii+1]
              + 0.114 * ar[3*ii+2] ;
              if( bot > val ) bot = val ;
         else if( top < val ) top = val ;
       }
     }
     break ;

     case MRI_byte:{
       register byte *ar = (byte *) br ;
       bot = top = ar[0] ;
       for( ii=1 ; ii < nvox ; ii++ ){
              if( bot > ar[ii] ) bot = ar[ii] ;
         else if( top < ar[ii] ) top = ar[ii] ;
       }
     }
     break ;

     case MRI_short:{
       register short *ar = (short *) br ;
       bot = top = ar[0] ;
       for( ii=1 ; ii < nvox ; ii++ ){
              if( bot > ar[ii] ) bot = ar[ii] ;
         else if( top < ar[ii] ) top = ar[ii] ;
       }
     }
     break ;

#if 0
     case MRI_int:{
       register int *ar = (int *) br ;
       bot = top = ar[0] ;
       for( ii=1 ; ii < nvox ; ii++ ){
              if( bot > ar[ii] ) bot = ar[ii] ;
         else if( top < ar[ii] ) top = ar[ii] ;
       }
     }
     break ;
#endif

     case MRI_float:{
       register float *ar = (float *) br ;
       bot = top = ar[0] ;
       for( ii=1 ; ii < nvox ; ii++ ){
              if( bot > ar[ii] ) bot = ar[ii] ;
         else if( top < ar[ii] ) top = ar[ii] ;
       }
     }
     break ;

#if 0
     case MRI_double:{
       register double *ar = (double *) br ;
       bot = top = ar[0] ;
       for( ii=1 ; ii < nvox ; ii++ ){
              if( bot > ar[ii] ) bot = ar[ii] ;
         else if( top < ar[ii] ) top = ar[ii] ;
       }
     }
     break ;
#endif

     case MRI_complex:{
       register complex *ar = (complex *) br ;
       register float zz ;
       bot = top = CABS(ar[0]) ;
       for( ii=1 ; ii < nvox ; ii++ ){
          zz = CABS(ar[ii]) ;
               if( bot > zz ) bot = zz ;
          else if( top < zz ) top = zz ;
       }
     }
     break ;

   } /* end of switch on sub-brick type */

   bst.min = bot ; bst.max = top ;
   return bst ;
}

/*----------------------------------------------------------------------*/
/*! Update the statistics of a dataset
   (use only if new bricks are added -- see EDIT_add_bricklist)
------------------------------------------------------------------------*/

void THD_update_statistics( THD_3dim_dataset *dset )
{
   Boolean good ;
   int ii , mmin , mmax , ibr , nbsold , nbr ;
   THD_brick_stats *bsold ;
   short *brkk ;

   /*-- sanity checks --*/

   if( ! ISVALID_3DIM_DATASET(dset) ) return ;

   nbr = THD_count_databricks( dset->dblk ) ;

   if( nbr == 0 ) return ;

   /*-- if here, have good data in this dataset --*/

   if( dset->stats == NULL ){                  /* create if not present */
     dset->stats = myRwcNew( THD_statistics ) ;
     ADDTO_KILL( dset->kl , dset->stats ) ;
     dset->stats->type   = STATISTICS_TYPE ;
     dset->stats->parent = (RwcPointer) dset ;
     dset->stats->bstat  = NULL ;
     dset->stats->nbstat = 0 ;
     nbsold              = 0 ;
   } else {
     nbsold              = dset->stats->nbstat ;
   }

   if( dset->dblk->nvals > nbsold ){
     bsold               = dset->stats->bstat ;
     dset->stats->nbstat = dset->dblk->nvals ;
     dset->stats->bstat  = (THD_brick_stats *)
                           RwcRealloc( (char *) bsold ,
                                sizeof(THD_brick_stats) * dset->dblk->nvals ) ;
     if( bsold != dset->stats->bstat )
       REPLACE_KILL( dset->kl , bsold , dset->stats->bstat ) ;

     for( ibr=nbsold ; ibr < dset->dblk->nvals ; ibr++ )  /* 11 Mar 2005 */
       INVALIDATE_BSTAT( dset->stats->bstat[ibr] ) ;
   }

   /* 28 Apr 1997: load stats for new sub-bricks, not all */

   for( ibr=0 ; ibr < dset->dblk->nvals ; ibr++ ){

     if( ibr >= nbsold || ! ISVALID_BSTAT(dset->stats->bstat[ibr]) ){
        dset->stats->bstat[ibr] = THD_get_brick_stats( DSET_BRICK(dset,ibr) ) ;

        if( DSET_BRICK_FACTOR(dset,ibr) > 0.0 ){
          dset->stats->bstat[ibr].min *= DSET_BRICK_FACTOR(dset,ibr) ;
          dset->stats->bstat[ibr].max *= DSET_BRICK_FACTOR(dset,ibr) ;
        }
     }
   }
   return ;
}

/*----------------------------------------------------------------------*/
/*! Update the statistics of one sub-brick in a dataset. [29 Mar 2005]
------------------------------------------------------------------------*/

void THD_update_one_bstat( THD_3dim_dataset *dset , int iv )
{
   Boolean good ;
   int ii , mmin , mmax , ibr , nbsold , nbr ;
   THD_brick_stats *bsold ;
   short *brkk ;

   /*-- sanity checks --*/

   if( ! ISVALID_3DIM_DATASET(dset)     ) return ;
   if( iv < 0 || iv >= DSET_NVALS(dset) ) return ;

   /*-- if here, have good data in this dataset --*/

   if( dset->stats == NULL ){                  /* create if not present */
     dset->stats = myRwcNew( THD_statistics ) ;
     ADDTO_KILL( dset->kl , dset->stats ) ;
     dset->stats->type   = STATISTICS_TYPE ;
     dset->stats->parent = (RwcPointer) dset ;
     dset->stats->bstat  = NULL ;
     dset->stats->nbstat = 0 ;
     nbsold              = 0 ;
   } else {
     nbsold              = dset->stats->nbstat ;
   }

   if( dset->dblk->nvals > nbsold ){
     bsold               = dset->stats->bstat ;
     dset->stats->nbstat = dset->dblk->nvals ;
     dset->stats->bstat  = (THD_brick_stats *)
                           RwcRealloc( (char *) bsold ,
                                sizeof(THD_brick_stats) * dset->dblk->nvals ) ;
     if( bsold != dset->stats->bstat )
       REPLACE_KILL( dset->kl , bsold , dset->stats->bstat ) ;

     for( ibr=nbsold ; ibr < dset->dblk->nvals ; ibr++ )  /* 11 Mar 2005 */
       INVALIDATE_BSTAT( dset->stats->bstat[ibr] ) ;
   }

   if( iv >= nbsold || ! ISVALID_BSTAT(dset->stats->bstat[iv]) ){
     dset->stats->bstat[iv] = THD_get_brick_stats( DSET_BRICK(dset,iv) ) ;

     if( DSET_BRICK_FACTOR(dset,iv) > 0.0 ){
       dset->stats->bstat[iv].min *= DSET_BRICK_FACTOR(dset,iv) ;
       dset->stats->bstat[iv].max *= DSET_BRICK_FACTOR(dset,iv) ;
     }
   }

   return ;
}

/*
   Multiply values in dset by fac
   Return number of sub-bricks for which scaling could not be done.
*/
int THD_dset_scale(THD_3dim_dataset *aset, float fac)
{
   int ii, jj, err=0;
   float fac0 = 1.0f, *fv=NULL;

ENTRY("THD_dset_scale");

   if( !ISVALID_DSET(aset) ){
     ERROR_message("THD_dset_scale: invalid dataset input :(") ;
     RETURN(666) ;
   }
   if( fac == 0.0f ){
     ERROR_message("THD_dset_scale: can't scale by 0 :(") ;
     RETURN(DSET_NVALS(aset)) ;
   }

   DSET_load(aset) ;
   if( !DSET_LOADED(aset) ){
     ERROR_message("THD_dset_scale: can't load dataset :(") ;
     RETURN(DSET_NVALS(aset)) ;
   }

   for (ii=0; ii<DSET_NVALS(aset); ++ii) {
      switch (DSET_BRICK_TYPE(aset,ii)) {
         case MRI_short:
         case MRI_byte:
            fac0 = DSET_BRICK_FACTOR(aset,ii);
            if (fac0 == 0.0f) fac0 = 1.0f ;
            EDIT_BRICK_FACTOR( aset,ii,fac0*fac ) ;
            break;
         case MRI_float:
            fv = (float *)DSET_ARRAY(aset,ii);
            if( fv == NULL ){
              ERROR_message("THD_dset_scale: bad float array at ii=%d",ii) ;
              err++ ;
            } else {
              for (jj=0; jj<DSET_NVOX(aset); ++jj) {
                fv[jj] *= fac;
              }
            }
            break;
         default:
            if(!err)
               ERROR_message("THD_dset_scale: not ready for data type %d [%s]\n"
                             "   --> Sub-bricks of such types are untouched :(",
                             DSET_BRICK_TYPE(aset,ii) ,
                             MRI_type_string(DSET_BRICK_TYPE(aset,ii)) );
            ++err;
      }
   }
   DSET_KILL_STATS(aset); THD_load_statistics(aset);
   if (err > 0) {
      ERROR_message("THD_dset_scale: A total of %d sub-bricks were not scaled", err);
   }

   RETURN(err);
}

/*--------------------------------------------------------------*/
/* Number of sub-bricks with nonzero data somewhere inside.
   If this returns zero, the dataset is all zero. [17 Jan 2017]
*//*------------------------------------------------------------*/

int THD_count_nonzero_bricks( THD_3dim_dataset *dset )
{
   int nzc=0 , iv , nvals ;

   if( !ISVALID_DSET(dset) ) return 0 ;

   DSET_load(dset) ;
   iv = THD_count_databricks( dset->dblk ) ;
   if( iv == 0 ) return 0 ;

   nvals = DSET_NVALS(dset) ;
   for( iv=0 ; iv < nvals ; iv++ ){
     nzc += ( mri_allzero(DSET_BRICK(dset,iv)) == 0 ) ;
   }

   return nzc ;
}
