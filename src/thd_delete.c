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

void THD_delete_diskptr( THD_diskptr * dkptr )
{
   if( ! ISVALID_DISKPTR(dkptr) ) return ;
}

/*---------------------------------------------------------------
  erase the insides of a datablock from the earth
-----------------------------------------------------------------*/

void THD_delete_datablock( THD_datablock * dblk )
{
   int ibr ;

   if( ! ISVALID_DATABLOCK(dblk) ) return ;

   /** free the actual brick data (method depends on how it is stored) **/

   if( dblk->brick != NULL ){
      switch( dblk->malloc_type ){

         default:
            ibr = THD_count_databricks( dblk ) ;
            if( ibr > 0 )
               fprintf(stderr,
                "** attempt to delete non-NULL unknown type of datablock **\n");

            if( dblk->brick != NULL ) FREE_IMARR( dblk->brick ) ;
         break ;

         case DATABLOCK_MEM_MALLOC:
            DESTROY_IMARR( dblk->brick ) ;
         break ;

         case DATABLOCK_MEM_MMAP:
            if( DBLK_ARRAY(dblk,0) != NULL )
               munmap( DBLK_ARRAY(dblk,0) , dblk->total_bytes ) ;
            for( ibr=0 ; ibr < dblk->brick->num ; ibr++ )
               mri_clear_data_pointer( DBLK_BRICK(dblk,ibr) ) ;
            DESTROY_IMARR( dblk->brick ) ;
         break ;
      }
   }

   /** free the other information **/

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

   if( DBLK_IS_MASTERED(dblk) ){       /* 11 Jan 1999 */
      myXtFree( dblk->master_ival ) ;
      myXtFree( dblk->master_bytes ) ;
   }

   THD_delete_diskptr( dblk->diskptr ) ;
   KILL_KILL( dblk->kl ) ;
   myXtFree( dblk->atr ) ;    /* not on the kill list */
}

/*-------------------------------------------------------------------
   destroy a 3D dataset (possibly including files)
---------------------------------------------------------------------*/

void THD_delete_3dim_dataset( THD_3dim_dataset * dset, Boolean kill_files )
{
   if( ! ISVALID_3DIM_DATASET(dset) ) return ;

   if( DSET_IS_MINC(dset) ) kill_files = False ;  /* 29 Oct 2001 */

   if( kill_files ){
      THD_diskptr * dkptr = dset->dblk->diskptr ;

      unlink( dkptr->header_name ) ;
      COMPRESS_unlink(dkptr->brick_name) ;
   }

   DESTROY_VLIST(dset->pts) ;

   if( ISVALID_TIMEAXIS(dset->taxis) ){
      myXtFree( dset->taxis->toff_sl ) ;
      myXtFree( dset->taxis ) ;
   }

   myXtFree( dset->merger_list ) ;
   THD_delete_datablock( dset->dblk ) ;
   KILL_KILL( dset->kl ) ;
}
