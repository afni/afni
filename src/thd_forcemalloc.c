#include "mrilib.h"
#include "thd.h"

static int native_order = -1 ;
static int no_mmap      = -1 ;

void THD_force_malloc_type( THD_datablock * blk , int mem_type )
{
   int new_type ;

   if( native_order < 0 ) native_order = mri_short_order() ;

   if( no_mmap < 0 ){
      char * hh = my_getenv("AFNI_NOMMAP") ;
      if( hh == NULL ) no_mmap = 0 ;
      else             no_mmap = (strcmp(hh,"YES") == 0) ;
   }

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

   /* 25 April 1998: byte order issues */

   if( blk->diskptr->byte_order <= 0 )
      blk->diskptr->byte_order = native_order ;
   else if( blk->diskptr->byte_order != native_order )
      new_type = DATABLOCK_MEM_MALLOC ;

   if( no_mmap )
      new_type = DATABLOCK_MEM_MALLOC ;

   if( blk->malloc_type == new_type ) return ;
   (void) THD_purge_datablock( blk , blk->malloc_type ) ;
   blk->malloc_type = new_type ;
   return ;
}
