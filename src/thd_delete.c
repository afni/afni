/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*---------------------------------------------------------------
  erase the insides of a diskptr from the earth
-----------------------------------------------------------------*/

void THD_delete_diskptr( THD_diskptr *dkptr )
{
   if( ! ISVALID_DISKPTR(dkptr) ) return ;
}

/*---------------------------------------------------------------
  erase the insides of a datablock from the earth
-----------------------------------------------------------------*/

void THD_delete_datablock( THD_datablock *dblk )
{
   int ibr ;

ENTRY("THD_delete_datablock") ;

   if( ! ISVALID_DATABLOCK(dblk) ) EXRETURN ;

   /** free the actual brick data (method depends on how it is stored) **/

   if( dblk->brick != NULL ){
      dblk->locked = 0 ;

      switch( dblk->malloc_type ){

         default:
            STATUS("count bricks") ;
            ibr = THD_count_databricks( dblk ) ;
            if( ibr > 0 )
               fprintf(stderr,
                "** attempt to delete non-NULL unknown type of datablock **\n");

            if( dblk->brick != NULL ) FREE_IMARR( dblk->brick ) ;
         break ;

         case DATABLOCK_MEM_MALLOC:
         case DATABLOCK_MEM_MMAP:
            THD_purge_datablock( dblk , dblk->malloc_type ) ;
            DESTROY_IMARR( dblk->brick ) ;
         break ;

         case DATABLOCK_MEM_SHARED:   /* 02 May 2003 */
           /* ??? */
         break ;
      }
   }

   /** free the other information **/

STATUS("free brick_ stuff") ;

   myXtFree( dblk->brick_fac ) ;
   myXtFree( dblk->brick_bytes ) ;

   /** 30 Nov 1997 **/

   if( dblk->brick_lab != NULL ){
      for( ibr=0 ; ibr < dblk->nvals ; ibr++ ) myXtFree( dblk->brick_lab[ibr] ) ;
      myXtFree( dblk->brick_lab ) ;
   }

   if( dblk->brick_keywords != NULL ){
      for( ibr=0 ; ibr < dblk->nvals ; ibr++ ) myXtFree( dblk->brick_keywords[ibr] ) ;
      myXtFree( dblk->brick_keywords ) ;
   }

   if( dblk->brick_statcode != NULL ) myXtFree( dblk->brick_statcode ) ;
   if( dblk->brick_stataux  != NULL ){
      for( ibr=0 ; ibr < dblk->nvals ; ibr++ ) myXtFree( dblk->brick_stataux[ibr] ) ;
      myXtFree( dblk->brick_stataux ) ;
   }

   if( dblk->brick_fdrcurve != NULL ){ /* 23 Jan 2008 */
     floatvec *fv ;
     for( ibr=0 ; ibr < dblk->nvals ; ibr++ ){
       fv = dblk->brick_fdrcurve[ibr] ; KILL_floatvec(fv) ;
     }
     free((void *)dblk->brick_fdrcurve) ;
   }
   if( dblk->brick_mdfcurve != NULL ){ /* 22 Oct 2008 */
     floatvec *fv ;
     for( ibr=0 ; ibr < dblk->nvals ; ibr++ ){
       fv = dblk->brick_mdfcurve[ibr] ; KILL_floatvec(fv);
     }
     free((void *)dblk->brick_mdfcurve) ;
   }

   if( DBLK_IS_MASTERED(dblk) ){       /* 11 Jan 1999 */
      myXtFree( dblk->master_ival ) ;
      myXtFree( dblk->master_bytes ) ;
   }

   THD_delete_diskptr( dblk->diskptr ) ;

   if( dblk->nnodes > 0 && dblk->node_list ){    /* Jul 12 2006 [rickr] */
      myXtFree( dblk->node_list ) ;
      dblk->nnodes = 0 ;
   }

STATUS("KILL_KILL") ;
   KILL_KILL( dblk->kl ) ;

STATUS("free attributes") ;
   myXtFree( dblk->atr ) ;    /* not on the kill list */

   EXRETURN ;
}

/*-------------------------------------------------------------------
   destroy a 3D dataset (possibly including files)
---------------------------------------------------------------------*/

void THD_delete_3dim_dataset( THD_3dim_dataset *dset, Boolean kill_files )
{
ENTRY("THD_delete_3dim_dataset") ;

   if( ! ISVALID_3DIM_DATASET(dset) ) EXRETURN ;

   if( DSET_IS_MINC(dset)    ) kill_files = False ;  /* 29 Oct 2001 */
   if( DSET_IS_VOLUMES(dset) ) kill_files = False ;  /* 20 Jun 2002 */
   if( DSET_IS_ANALYZE(dset) ) kill_files = False ;  /* 27 Aug 2002 */
   if( DSET_IS_1D(dset)      ) kill_files = False ;
   if( DSET_IS_3D(dset)      ) kill_files = False ;
   if( DSET_IS_CTFMRI(dset)  ) kill_files = False ;
   if( DSET_IS_CTFSAM(dset)  ) kill_files = False ;
   if( DSET_IS_NIFTI(dset)   ) kill_files = False ;  /* 28 Aug 2003 */
   if( DSET_IS_NIML(dset)    ) kill_files = False ;  /* 12 Jul 2006 [rickr] */
   if( DSET_IS_NI_SURF_DSET (dset) ) kill_files = False ;
   if( DSET_IS_GIFTI (dset)  ) kill_files = False ;  /* 13 Feb 2008 [rickr] */

   if( kill_files ){
      THD_diskptr *dkptr = dset->dblk->diskptr ;

STATUS("killing files") ;
      unlink( dkptr->header_name ) ;
      COMPRESS_unlink(dkptr->brick_name) ;
   }

#ifdef ALLOW_DATASET_VLIST
STATUS("destroy vlist") ;
   DESTROY_VLIST(dset->pts) ;
#endif

   if( ISVALID_TIMEAXIS(dset->taxis) ){
STATUS("destroy taxis") ;
      myXtFree( dset->taxis->toff_sl ) ;
      myXtFree( dset->taxis ) ;
   }

   THD_delete_datablock( dset->dblk ) ;

   /* EDIT_empty_copy() is only vox_warp not in KILL_list, add it there */
   /*                                                5 Mar 2008 [rickr] */
   /* myXtFree( dset->vox_warp ) ;  * 23 Sep 2008                       */

STATUS("KILL_KILL") ;
   KILL_KILL( dset->kl ) ;

   EXRETURN ;
}
