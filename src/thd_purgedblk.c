/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

RwcBoolean THD_purge_datablock( THD_datablock *blk , int mem_type )
{
   int ibr ;
   void *ptr ;

ENTRY("THD_purge_datablock") ;

   /*-- sanity checks --*/

   if( ! ISVALID_DATABLOCK(blk) || blk->brick == NULL ) RETURN( False );
   if( (blk->malloc_type & mem_type) == 0 )             RETURN( False );
   if( DBLK_LOCKED(blk) )                               RETURN( False );

   /*-- free the data space --*/

   switch( blk->malloc_type ){

      case DATABLOCK_MEM_MALLOC:
STATUS("MEM_MALLOC: clearing sub-bricks") ;
         for( ibr=0 ; ibr < blk->nvals ; ibr++ ){
#if 1
            mri_clear( DBLK_BRICK(blk,ibr) ) ;  /* 31 Jan 2007 */
#else
            ptr = DBLK_ARRAY(blk,ibr) ;
            if( ptr != NULL ){
              free(ptr); mri_clear_data_pointer(DBLK_BRICK(blk,ibr));
            }
#endif
         }
      RETURN( True );

      case DATABLOCK_MEM_MMAP:
STATUS("MEM_MMAP: unmapping") ;
         ptr = DBLK_ARRAY(blk,0) ;
         if( ptr != NULL ) munmap( ptr , (size_t)blk->total_bytes ) ;
         for( ibr=0 ; ibr < blk->nvals ; ibr++ )
           mri_clear_data_pointer( DBLK_BRICK(blk,ibr) ) ;
      RETURN( True );

      case DATABLOCK_MEM_SHARED:   /* can't be purged */
      RETURN( False );
   }

   RETURN( False );  /* shouldn't be reached */
}

/*----------------------------------------------------------
   04 May 1998: purge just one sub-brick, if possible
------------------------------------------------------------*/

RwcBoolean THD_purge_one_brick( THD_datablock *blk , int iv )
{
   void *ptr ;

ENTRY("THD_purge_one_brick") ;

   /* sanity checks */

   if( ! ISVALID_DATABLOCK(blk) || blk->brick == NULL ) RETURN( False );
   if( DBLK_LOCKED(blk) )                               RETURN( False );
   if( iv < 0 || iv >= blk->nvals )                     RETURN( False );
   if( blk->malloc_type != DATABLOCK_MEM_MALLOC )       RETURN( False );

#if 1
   mri_clear( DBLK_BRICK(blk,iv) ) ;  /* 31 Jan 2007 */
#else
   ptr = DBLK_ARRAY(blk,iv) ;
   if( ptr != NULL ) free(ptr) ;
   mri_clear_data_pointer( DBLK_BRICK(blk,iv) ) ;
#endif
   RETURN( True );
}
