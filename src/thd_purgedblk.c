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
