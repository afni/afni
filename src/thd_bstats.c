#include "mrilib.h"
#include "thd.h"


/*----------------------------------------------------------------------
   load the statistics of a dataset (modified Nov 15 1995)
------------------------------------------------------------------------*/

void THD_load_statistics( THD_3dim_dataset * dset )
{
   Boolean good ;
   int ii , mmin , mmax , ibr ;
   short * brkk ;
   THD_brick_stats * bsold ;

   /*-- sanity checks --*/

   if( ! ISVALID_3DIM_DATASET(dset) ) return ;

   good = THD_load_datablock( dset->dblk , NULL ) ;  /* must have data */

   /*-- 3/24/95: if don't get data, try for the warp parent --*/

   if( ! good ){

      if( ! ISVALID_3DIM_DATASET(dset->warp_parent) ) return ;     /* nothing */
      if( dset->warp_parent == dset )                 return ;

      RELOAD_STATS( dset->warp_parent ) ;                          /* recursion! */
      if( ! ISVALID_STATISTIC(dset->warp_parent->stats) ) return ; /* nothing */

      if( dset->stats == NULL ){                  /* create if not present */
         dset->stats = myXtNew( THD_statistics ) ;
         ADDTO_KILL( dset->kl , dset->stats ) ;
         dset->stats->type   = STATISTICS_TYPE ;
         dset->stats->parent = (XtPointer) dset ;
         dset->stats->bstat  = NULL ;
      }

      bsold               = dset->stats->bstat ;
      dset->stats->nbstat = dset->dblk->nvals ;
      dset->stats->bstat  = (THD_brick_stats *)
                              XtRealloc( (char *) bsold ,
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
      dset->stats = myXtNew( THD_statistics ) ;
      ADDTO_KILL( dset->kl , dset->stats ) ;
      dset->stats->type   = STATISTICS_TYPE ;
      dset->stats->parent = (XtPointer) dset ;
      dset->stats->bstat  = NULL ;
   }

   bsold               = dset->stats->bstat ;
   dset->stats->nbstat = dset->dblk->nvals ;
   dset->stats->bstat  = (THD_brick_stats *)
                           XtRealloc( (char *) bsold ,
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

/*--------------------------------------------------------------
   Compute statistics for a 3D brick of varying data type
----------------------------------------------------------------*/

THD_brick_stats THD_get_brick_stats( MRI_IMAGE * im )
{
   register int ii , nvox ;
   register float bot , top ;
   void * br ;
   THD_brick_stats bst ;

ENTRY("THD_get_brick_stats") ;

   bst.min = bst.max = 0 ;

   if( im == NULL ) return bst ;
   br   = mri_data_pointer( im ) ; if( br == NULL ) return bst ;
   nvox = im->nvox ;

   switch( im->kind ){

      default:
         bot = top = 0.0 ;
      break ;

      case MRI_byte:{
         register byte * ar = (byte *) br ;
         bot = top = ar[0] ;
         for( ii=1 ; ii < nvox ; ii++ ){
                 if( bot > ar[ii] ) bot = ar[ii] ;
            else if( top < ar[ii] ) top = ar[ii] ;
         }
      }
      break ;

      case MRI_short:{
         register short * ar = (short *) br ;
         bot = top = ar[0] ;
         for( ii=1 ; ii < nvox ; ii++ ){
                 if( bot > ar[ii] ) bot = ar[ii] ;
            else if( top < ar[ii] ) top = ar[ii] ;
         }
      }
      break ;

      case MRI_int:{
         register int * ar = (int *) br ;
         bot = top = ar[0] ;
         for( ii=1 ; ii < nvox ; ii++ ){
                 if( bot > ar[ii] ) bot = ar[ii] ;
            else if( top < ar[ii] ) top = ar[ii] ;
         }
      }
      break ;

      case MRI_float:{
         register float * ar = (float *) br ;
         bot = top = ar[0] ;
         for( ii=1 ; ii < nvox ; ii++ ){
                 if( bot > ar[ii] ) bot = ar[ii] ;
            else if( top < ar[ii] ) top = ar[ii] ;
         }
      }
      break ;

      case MRI_double:{
         register double * ar = (double *) br ;
         bot = top = ar[0] ;
         for( ii=1 ; ii < nvox ; ii++ ){
                 if( bot > ar[ii] ) bot = ar[ii] ;
            else if( top < ar[ii] ) top = ar[ii] ;
         }
      }
      break ;

      case MRI_complex:{
         register complex * ar = (complex *) br ;
         register float zz ;
         bot = top = CSQR(ar[0]) ;
         for( ii=1 ; ii < nvox ; ii++ ){
            zz = CSQR(ar[ii]) ;
                 if( bot > zz ) bot = zz ;
            else if( top < zz ) top = zz ;
         }
         bot = sqrt(bot) ; top = sqrt(top) ;
      }
      break ;

   }

#ifdef THD_DEBUG
printf("  -- new stats min=%g max=%g\n",bot,top) ;
#endif

   bst.min = bot ; bst.max = top ;
   return bst ;
}

/*----------------------------------------------------------------------
   Update the statistics of a dataset
   (use only if new bricks are added -- see EDIT_add_bricklist)
------------------------------------------------------------------------*/

void THD_update_statistics( THD_3dim_dataset * dset )
{
   Boolean good ;
   int ii , mmin , mmax , ibr , nbsold ;
   short * brkk ;
   THD_brick_stats * bsold ;

   /*-- sanity checks --*/

   if( ! ISVALID_3DIM_DATASET(dset) ) return ;

   good = THD_load_datablock( dset->dblk , NULL ) ;  /* must have data */

   if( !good) return ;

   /*-- if here, have good data in this dataset --*/

   if( dset->stats == NULL ){                  /* create if not present */
      dset->stats = myXtNew( THD_statistics ) ;
      ADDTO_KILL( dset->kl , dset->stats ) ;
      dset->stats->type   = STATISTICS_TYPE ;
      dset->stats->parent = (XtPointer) dset ;
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
                            XtRealloc( (char *) bsold ,
                                        sizeof(THD_brick_stats) * dset->dblk->nvals ) ;
      if( bsold != dset->stats->bstat )
         REPLACE_KILL( dset->kl , bsold , dset->stats->bstat ) ;
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
