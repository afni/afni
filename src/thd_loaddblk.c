#include "mrilib.h"
#include "thd.h"


/*---------------------------------------------------------------*/

Boolean THD_load_datablock( THD_datablock * blk , generic_func * freeup )
{
   THD_diskptr * dkptr ;
   int id , offset ;
   int nx,ny,nz , nxy,nxyz,nxyzv , nv,vv , ii , ntot , ibr ;
   char * ptr ;
   MRI_IMAGE * im ;

ENTRY("THD_load_datablock") ;

   /*-- sanity checks --*/

   if( ! ISVALID_DATABLOCK(blk) || blk->brick == NULL ) return False ;

   ii = THD_count_databricks( blk ) ;
   if( ii == blk->nvals ) return True ;

   if( blk->malloc_type == DATABLOCK_MEM_UNDEFINED ) return False ;

   dkptr = blk->diskptr ;
   if( ! ISVALID_DISKPTR(dkptr) || dkptr->storage_mode == STORAGE_UNDEFINED )
      return False ;

   if( dkptr->rank != 3 ){
      fprintf(stderr,"\n*** Cannot read non 3D datablocks ***\n") ;
      return False ;
   }

   /*-- allocate data space --*/

   nx = dkptr->dimsizes[0] ;
   ny = dkptr->dimsizes[1] ;  nxy   = nx * ny   ;
   nz = dkptr->dimsizes[2] ;  nxyz  = nxy * nz  ;
   nv = dkptr->nvals       ;  nxyzv = nxyz * nv ; ntot = blk->total_bytes ;

   if( blk->malloc_type == DATABLOCK_MEM_MALLOC ){

      /** malloc space for each brick separately **/

      for( ibr=0 ; ibr < nv ; ibr++ ){
         if( DBLK_ARRAY(blk,ibr) == NULL ){
            ptr = malloc( DBLK_BRICK_BYTES(blk,ibr) ) ;
            mri_fix_data_pointer( ptr ,  DBLK_BRICK(blk,ibr) ) ;
         }
      }

      if( THD_count_databricks(blk) < nv ){
         fprintf(stderr,  "\n*** failure to malloc dataset memory\n") ;
         if( freeup != NULL ){
            freeup() ;
            for( ibr=0 ; ibr < nv ; ibr++ ){
               if( DBLK_ARRAY(blk,ibr) == NULL ){
                  ptr = malloc( DBLK_BRICK_BYTES(blk,ibr) ) ;
                  mri_fix_data_pointer( ptr ,  DBLK_BRICK(blk,ibr) ) ;
               }
            }
            if( THD_count_databricks(blk) < nv ){
               fprintf(stderr,"*** cannot free up enough memory\n") ;
               return False ;
            } else {
               fprintf(stderr,"*** was able to free up enough memory\n") ;
            }
         } else {
            return False ;
         }
      }

      /** mmap the whole file at once **/

   } else if( blk->malloc_type == DATABLOCK_MEM_MMAP ){
      int fd ;
      fd = open( dkptr->brick_name , O_RDONLY ) ;
      if( fd < 0 ){
         fprintf( stderr , "\n*** cannot open brick file %s\n" ,
                  dkptr->brick_name ) ;
         perror("*** Unix error message") ;
         return False ;
      }

      for( ibr=0 ; ibr < nv ; ibr++ )
         mri_clear_data_pointer( DBLK_BRICK(blk,ibr) ) ;

      ptr = (char *) mmap( 0 , blk->total_bytes ,
                               PROT_READ , THD_MMAP_FLAG , fd , 0 ) ;

      if( ptr == (char *)(-1) ){
         fprintf(stderr , "\n*** cannot mmap brick file %s\n" ,
                 dkptr->brick_name ) ;
         perror("*** Unix error message") ;
         if( freeup != NULL ){
            freeup() ;
            ptr = (char *) mmap( 0 , blk->total_bytes ,
                                     PROT_READ , THD_MMAP_FLAG , fd , 0 ) ;
            if( ptr == (char *)(-1) ){
               fprintf(stderr,"*** cannot fix problem!\n") ;
               close(fd) ;
               return False ;
            } else {
               fprintf(stderr,"*** was able to fix problem!\n") ;
            }
         } else {
            close(fd) ;
            return False ;
         }
      }

      close(fd) ;  /* can close file after mmap-ing it */

      /* create pointers to all sub-bricks */

      offset = 0 ;
      for( ibr=0 ; ibr < nv ; ibr++ ){
         mri_fix_data_pointer( ptr , DBLK_BRICK(blk,ibr) ) ;
         ptr += DBLK_BRICK_BYTES(blk,ibr) ;
      }

#if defined(THD_DEBUG)
printf("THD_load_datablock: mmap-ed file %s\n",dkptr->brick_name) ;
#endif
      return True ;
   }

   /*** read data into newly malloc-ed bricks ***/

   switch( dkptr->storage_mode ){

      default:
         fprintf(stderr,"\n*** illegal storage mode in read ***\n") ;
         return False ;
      break ;

     /*-- read brick file --*/

      case STORAGE_BY_BRICK:{
         FILE * far ;

         far = COMPRESS_fopen_read( dkptr->brick_name ) ;

         if( far == NULL ){
            for( ibr=0 ; ibr < nv ; ibr++ ){
               free( DBLK_ARRAY(blk,ibr) ) ;
               mri_clear_data_pointer( DBLK_BRICK(blk,ibr) ) ;
            }
            fprintf(stderr,"\n*** failure while opening brick file %s\n",
                    dkptr->brick_name ) ;
            perror("*** Unix error message") ;
            return False ;
         }

         /* read each sub-brick all at once */

         id = 0 ;
         for( ibr=0 ; ibr < nv ; ibr++ ){
            id += fread( DBLK_ARRAY(blk,ibr), 1,
                         DBLK_BRICK_BYTES(blk,ibr), far ) ;
#ifdef THD_DEBUG
  printf("  -- Reading: # bytes desired = %d  total read = %d\n",
         DBLK_BRICK_BYTES(blk,ibr),id) ;
#endif
         }

         COMPRESS_fclose(far) ;

         if( id != blk->total_bytes ){
            for( ibr=0 ; ibr < nv ; ibr++ ){
               free( DBLK_ARRAY(blk,ibr) ) ;
               mri_clear_data_pointer( DBLK_BRICK(blk,ibr) ) ;
            }
            fprintf(stderr ,
                    "\n*** failure while reading from brick file %s\n"
                      "*** desired %d bytes but only got %d\n" ,
                    dkptr->brick_name , blk->total_bytes , id ) ;
            perror("*** Unix error message") ;
            return False ;
         }

#if defined(THD_DEBUG)
printf("THD_load_datablock: malloc-ed/read file %s\n",dkptr->brick_name) ;
#endif
         return True ;
      }
      break ;

   } /* end of switch on storage modes */

   return False ;  /* should NEVER be reached */
}
