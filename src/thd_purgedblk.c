/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

Boolean THD_purge_datablock( THD_datablock * blk , int mem_type )
{
   int ibr , nfreed ;
   void * ptr ;

ENTRY("THD_purge_datablock") ;

   /*-- sanity checks --*/

   if( ! ISVALID_DATABLOCK(blk) || blk->brick == NULL ) return False ;
   if( (blk->malloc_type & mem_type) == 0 )             return False ;
   if( DBLK_LOCKED(blk) )                               return False ;

   /*-- free the data space --*/

   nfreed = 0 ;
   switch( blk->malloc_type ){

      case DATABLOCK_MEM_MALLOC:
         for( ibr=0 ; ibr < blk->nvals ; ibr++ ){
            ptr = DBLK_ARRAY(blk,ibr) ;
            if( ptr != NULL ){ free(ptr) ; nfreed++ ; }
            mri_clear_data_pointer( DBLK_BRICK(blk,ibr) ) ;
         }
#ifdef THD_DEBUG
printf("  -- free-d %d sub-bricks\n",nfreed) ;
#endif
         return True ;

      case DATABLOCK_MEM_MMAP:
         ptr = DBLK_ARRAY(blk,0) ;
         if( ptr != NULL ){ munmap( ptr , blk->total_bytes ) ; nfreed++ ; }
         for( ibr=0 ; ibr < blk->nvals ; ibr++ )
            mri_clear_data_pointer( DBLK_BRICK(blk,ibr) ) ;
#ifdef THD_DEBUG
if( nfreed ) printf("  -- munmap-ed sub-bricks starting at address %p\n",ptr) ;
#endif
         return True ;
   }

   return False ;  /* shouldn't be reached */
}

/*----------------------------------------------------------
   04 May 1998: purge just one sub-brick, if possible
------------------------------------------------------------*/

Boolean THD_purge_one_brick( THD_datablock * blk , int iv )
{
   void * ptr ;

   /* sanity checks */

   if( ! ISVALID_DATABLOCK(blk) || blk->brick == NULL ) return False ;
   if( DBLK_LOCKED(blk) )                               return False ;
   if( iv < 0 || iv >= blk->nvals )                     return False ;
   if( blk->malloc_type != DATABLOCK_MEM_MALLOC )       return False ;

   ptr = DBLK_ARRAY(blk,iv) ;
   if( ptr != NULL ) free(ptr) ;
   mri_clear_data_pointer( DBLK_BRICK(blk,iv) ) ;
   return True ;
}
