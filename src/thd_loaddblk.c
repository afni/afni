#include "mrilib.h"
#include "thd.h"

static int native_order = -1 ;
static int no_mmap      = -1 ;
static int floatscan    = -1 ;  /* 30 Jul 1999 */

/*---------------------------------------------------------------*/

Boolean THD_load_datablock( THD_datablock * blk , generic_func * freeup )
{
   THD_diskptr * dkptr ;
   int id , offset ;
   int nx,ny,nz , nxy,nxyz,nxyzv , nv,vv , ii , ntot , ibr ;
   char * ptr ;
   MRI_IMAGE * im ;

ENTRY("THD_load_datablock") ;

   if( native_order < 0 ) native_order = mri_short_order() ;
   if( no_mmap < 0 ){
      char * hh = my_getenv("AFNI_NOMMAP") ;
      if( hh == NULL ) no_mmap = 0 ;
      else             no_mmap = (strcmp(hh,"YES") == 0) ;
   }

   if( floatscan < 0 ){                         /* 30 Jul 1999 */
      char * hh = my_getenv("AFNI_FLOATSCAN") ;
      if( hh == NULL ) floatscan = 0 ;
      else             floatscan = 1 ;
      if( floatscan ) no_mmap = 1 ;
   }

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

   /* 25 April 1998: byte order issues */

   if( dkptr->byte_order <= 0 ) dkptr->byte_order = native_order ;
   if( dkptr->byte_order != native_order || no_mmap )
      blk->malloc_type = DATABLOCK_MEM_MALLOC ;

   /*-- allocate data space --*/

   nx = dkptr->dimsizes[0] ;
   ny = dkptr->dimsizes[1] ;  nxy   = nx * ny   ;
   nz = dkptr->dimsizes[2] ;  nxyz  = nxy * nz  ;
   nv = dkptr->nvals       ;  nxyzv = nxyz * nv ; ntot = blk->total_bytes ;

   if( DBLK_IS_MASTERED(blk) )                  /* 11 Jan 1999 */
      blk->malloc_type = DATABLOCK_MEM_MALLOC ;

   /** set up space for bricks via malloc **/

   if( blk->malloc_type == DATABLOCK_MEM_MALLOC ){

      /** malloc space for each brick separately **/

      for( ibr=0 ; ibr < nv ; ibr++ ){
         if( DBLK_ARRAY(blk,ibr) == NULL ){
            ptr = malloc( DBLK_BRICK_BYTES(blk,ibr) ) ;
            mri_fix_data_pointer( ptr ,  DBLK_BRICK(blk,ibr) ) ;
         }
      }

      if( THD_count_databricks(blk) < nv ){
         fprintf(stderr,
                "\n*** failure to malloc dataset memory - is memory exhausted?\n") ;
         if( freeup != NULL ){  /* try to free some space? */
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

   /** mmap the whole file at once (makes space and reads it all at once) **/

   } else if( blk->malloc_type == DATABLOCK_MEM_MMAP ){
      int fd ;
      fd = open( dkptr->brick_name , O_RDONLY ) ;
      if( fd < 0 ){
         fprintf( stderr , "\n*** cannot open brick file %s for mmap\n"
                           "   - do you have permission? does file exist?\n" ,
                  dkptr->brick_name ) ;
         perror("*** Unix error message") ;
         return False ;
      }

      for( ibr=0 ; ibr < nv ; ibr++ )
         mri_clear_data_pointer( DBLK_BRICK(blk,ibr) ) ;

      ptr = (char *) mmap( 0 , blk->total_bytes ,
                               PROT_READ , THD_MMAP_FLAG , fd , 0 ) ;

      if( ptr == (char *)(-1) ){
         fprintf(stderr ,
                 "\n*** cannot mmap brick file %s - maybe hit a system limit?\n" ,
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

     /*-- read brick file (there is no other option)--*/

      case STORAGE_BY_BRICK:{
         FILE * far ;

         far = COMPRESS_fopen_read( dkptr->brick_name ) ;

         if( far == NULL ){
            for( ibr=0 ; ibr < nv ; ibr++ ){
               free( DBLK_ARRAY(blk,ibr) ) ;
               mri_clear_data_pointer( DBLK_BRICK(blk,ibr) ) ;
            }
            fprintf(stderr,
                    "\n*** failure while opening brick file %s "
                    "- do you have permission?\n" ,
                    dkptr->brick_name ) ;
            perror("*** Unix error message") ;
            return False ;
         }

         /* read each sub-brick all at once */

         id = 0 ;
         if( ! DBLK_IS_MASTERED(blk) ){      /* read each brick */

            for( ibr=0 ; ibr < nv ; ibr++ )
               id += fread( DBLK_ARRAY(blk,ibr), 1,
                            DBLK_BRICK_BYTES(blk,ibr), far ) ;

         } else {  /* 11 Jan 1999: read brick from master, put into place(s) */

            int nfilled = 0 , nbuf=0 , jbr, nbr ;
            char * buf=NULL ;

            /* loop over master sub-bricks until dataset is filled */

            for( ibr=0 ; nfilled < nv && ibr < blk->master_nvals ; ibr++ ){

               if( nbuf < blk->master_bytes[ibr] ){  /* make more space for it */
                  if( buf != NULL ) free(buf) ;
                  nbuf = blk->master_bytes[ibr] ;
                  buf  = malloc( sizeof(char) * nbuf ) ;
                  if( buf == NULL ) break ;
               }

               /* read the master sub-brick */

               nbr = fread( buf , 1 , blk->master_bytes[ibr] , far ) ;
               if( nbr < blk->master_bytes[ibr] ) break ;

               /* find all the dataset sub-bricks that are copies of this */

               for( jbr=0 ; jbr < nv ; jbr++ ){
                  if( blk->master_ival[jbr] == ibr ){  /* copy it in */
                     memcpy( DBLK_ARRAY(blk,jbr) , buf , blk->master_bytes[ibr] ) ;
                     nfilled++ ;  /* number of bricks filled */
                     id += nbr ;  /* number of bytes read into dataset */
                  }
               }
            }  /* end of loop over master sub-bricks */

            if( buf != NULL ) free(buf) ;
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

         /* 25 April 1998: check and fix byte ordering */

         if( dkptr->byte_order != native_order ){
            for( ibr=0 ; ibr < nv ; ibr++ ){
               switch( DBLK_BRICK_TYPE(blk,ibr) ){
                  case MRI_short:
                     mri_swap2( DBLK_BRICK_NVOX(blk,ibr) , DBLK_ARRAY(blk,ibr) ) ;
                  break ;

                  case MRI_complex:  /* 14 Sep 1999: swap complex also! */
                     mri_swap4( 2*DBLK_BRICK_NVOX(blk,ibr), DBLK_ARRAY(blk,ibr)) ;
                  break ;

                  case MRI_float:
                  case MRI_int:
                     mri_swap4( DBLK_BRICK_NVOX(blk,ibr) , DBLK_ARRAY(blk,ibr) ) ;
                  break ;
               }
            }
         }

         /* 30 July 1999: check float sub-brick for errors? */
         /* 14 Sep  1999: also check complex sub-bricks!    */

         if( floatscan ){
            int nerr=0 ;
            for( ibr=0 ; ibr < nv ; ibr++ ){
               if( DBLK_BRICK_TYPE(blk,ibr) == MRI_float ){
                  nerr += thd_floatscan( DBLK_BRICK_NVOX(blk,ibr) ,
                                         DBLK_ARRAY(blk,ibr)        ) ;

               } else if( DBLK_BRICK_TYPE(blk,ibr) == MRI_complex ) {  /* 14 Sep 1999 */
                  nerr += thd_complexscan( DBLK_BRICK_NVOX(blk,ibr) ,
                                           DBLK_ARRAY(blk,ibr)        ) ;
               }
            }
            if( nerr > 0 )
               fprintf(stderr ,
                       "*** %s: found %d float errors -- see program float_scan\n" ,
                       dkptr->brick_name , nerr ) ;
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
