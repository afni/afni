#include "mrilib.h"
#include "thd.h"


void THD_force_malloc_type( THD_datablock * blk , int mem_type )
{
   int new_type ;

   /*-- sanity checks --*/

   if( ! ISVALID_DATABLOCK(blk) ) return ;

   if( mem_type == DATABLOCK_MEM_ANY ){  /* 14 Oct 1996 */
#if MMAP_THRESHOLD > 0
      new_type = (blk->total_bytes > MMAP_THRESHOLD)
                    ? DATABLOCK_MEM_MMAP : DATABLOCK_MEM_MALLOC ;
#else
      new_type = DATABLOCK_MEM_MALLOC ;
#endif

   } else {
      new_type = mem_type ;
   }

   if( COMPRESS_filecode(blk->diskptr->brick_name) >= 0 )
      new_type = DATABLOCK_MEM_MALLOC ;

   if( blk->malloc_type == new_type ) return ;
   (void) THD_purge_datablock( blk , blk->malloc_type ) ;
   blk->malloc_type = new_type ;
   return ;
}
