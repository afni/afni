/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*---------------------------------------------------------------------*/

static int compress_mode = COMPRESS_NOFILE ;

void THD_set_write_compression( int mm )
{
   if( mm >= COMPRESS_NONE && mm <= COMPRESS_LASTCODE )
      compress_mode = mm ;
   else
      compress_mode = COMPRESS_NONE ;
   return ;
}

/*---------------------------------------------------------------------*/

int THD_get_write_compression(void)
{
   if( compress_mode == COMPRESS_NOFILE ) THD_enviro_write_compression() ;
   return compress_mode ;
}

/*---------------------------------------------------------------------*/

int THD_enviro_write_compression(void)
{
   char *hh = my_getenv("AFNI_COMPRESSOR") ;
   int ii ;

   compress_mode = COMPRESS_NONE ;
   if( hh == NULL ) return COMPRESS_NONE ;

   for( ii=0 ; ii <= COMPRESS_LASTCODE ; ii++ ){
      if( strcmp(hh,COMPRESS_enviro[ii]) == 0 ){
         compress_mode = ii ;
         return ii ;
      }
   }

   return COMPRESS_NONE ;
}

/*---------------------------------------------------------------------*/

static int native_order = -1 ;
static int output_order = -1 ;

void THD_set_write_order( int mm )
{
   if( mm == LSB_FIRST || mm == MSB_FIRST )
      output_order = mm ;
   else
      output_order = -1 ;
   return ;
}

/*---------------------------------------------------------------------*/

void THD_enviro_write_order(void)
{
   char *hh = my_getenv("AFNI_BYTEORDER") ;

   if( hh == NULL ){ output_order = -1 ; return ; }

   if( strcmp(hh,LSB_FIRST_STRING) == 0 ){ output_order = LSB_FIRST; return; }
   if( strcmp(hh,MSB_FIRST_STRING) == 0 ){ output_order = MSB_FIRST; return; }

   output_order = -1 ; return ;
}

/*---------------------------------------------------------------------*/

int THD_get_write_order(void)
{
   if( native_order < 0 ) native_order = mri_short_order() ;
   if( output_order < 0 ) THD_enviro_write_order() ;

   return (output_order > 0) ? output_order
                             : native_order ;
}

/*---------------------------------------------------------------------*/
/*! Write an AFNI datablock to disk in the .HEAD/.BRIK format:
     - Returns True if OK, False if an error.
     - See also AFNI_refashion_dataset.
     - All attributes must now be set prior to calling this,
       via function THD_set_dataset_attributes().
     - The one exception to the above rule is the BYTEORDER attribute.
-----------------------------------------------------------------------*/

Boolean THD_write_datablock( THD_datablock *blk , Boolean write_brick )
{
   THD_diskptr *dkptr ;
   Boolean good ;
   int id , nx , ny , nz , nv , nxy , nxyz , ibr ;
   int atrank[ATRSIZE_DATASET_RANK] , atdims[ATRSIZE_DATASET_DIMENSIONS] ;
   MRI_IMAGE *im ;
   int save_order ;
   int64_t nb , idone ;
   int do_mripurge ;

   /*-- sanity checks --*/

   if( ! ISVALID_DATABLOCK(blk) ) return False ;
   if( DBLK_IS_MASTERED(blk) )    return False ;  /* 11 Jan 1999 */
   if( DBLK_IS_MINC(blk) )        return False ;  /* 29 Oct 2001 */
   if( DBLK_IS_ANALYZE(blk) )     return False ;  /* 27 Aug 2002 */
   if( DBLK_IS_NIFTI(blk) )       return False ;  /* 28 Aug 2003 */

   dkptr = blk->diskptr ;
   if( ! ISVALID_DISKPTR(dkptr) ) WRITE_ERR("illegal file type") ;

   if( strlen(dkptr->directory_name) == 0 ||
       strlen(dkptr->header_name)    == 0 ||
       strlen(dkptr->filecode)       == 0   )
     WRITE_ERR("illegal file names stored in dataset") ;

   if( dkptr->rank != 3 )
     WRITE_ERR("cannot write non-3D datablock") ;

   /*-- create directory if necessary --*/

   if( ! THD_is_directory(dkptr->directory_name) ){
     id = mkdir( dkptr->directory_name , THD_MKDIR_MODE ) ;
     if( id != 0 ){
       fprintf(stderr,
            "\n"
            "*** cannot mkdir new directory: %s\n"
            "  - Do you have permission to write to this disk?\n"
            "  - Is the disk full?\n" ,
            dkptr->directory_name) ;
       return False ;
     }
   }

   /* 25 April 1998: deal with byte order issues */

   if( native_order < 0 ){                /* initialization */
     native_order = mri_short_order() ;
     if( output_order < 0 ) THD_enviro_write_order() ;
   }
   if( dkptr->byte_order <= 0 ) dkptr->byte_order = native_order ;
   save_order = (output_order > 0) ? output_order
                                   : dkptr->byte_order ;

#if 0
fprintf(stderr,"THD_write_datablock: save_order=%d  dkptr->byte_order=%d\n",
               save_order, dkptr->byte_order ) ;
#endif

   if( save_order != LSB_FIRST && save_order != MSB_FIRST )
     save_order = native_order ;

   if( save_order == LSB_FIRST )
     THD_set_string_atr( blk , ATRNAME_BYTEORDER , LSB_FIRST_STRING ) ;
   else if( save_order == MSB_FIRST )
     THD_set_string_atr( blk , ATRNAME_BYTEORDER , MSB_FIRST_STRING ) ;

   /*-- actually write attributes to disk --*/

   good = THD_write_atr( blk ) ;
   if( good == False )
     WRITE_ERR(
     "failure to write attributes - is disk full? do you have write permission?");

   /*-- if not writing data, can exit --*/

   if( write_brick == False || blk->brick == NULL ||
       dkptr->storage_mode == STORAGE_UNDEFINED     ) return True ;

   if( dkptr->storage_mode == STORAGE_BY_VOLUMES ){  /* 20 Jun 2002 */
     fprintf(stderr,"** Writing dataset by VOLUMES not yet supported.\n") ;
     return False ;
   }

   /*-- check each brick for existence:
          if none exist, cannot write, but is OK
          if some but not all exist, cannot write, and is an error --*/

   id = THD_count_potential_databricks( blk ) ;
   if( id <= 0 )         return True ;
   if( id < blk->nvals ){
     ERROR_message("Write dataset error: only %d out of %d bricks in memory",
                   id,blk->nvals) ; return False ;
   }

   if( blk->malloc_type == DATABLOCK_MEM_UNDEFINED )
     WRITE_ERR("undefined data exists in memory") ;

   /*-- 13 Mar 2006: check for free disk space --*/

   { int mm = THD_freemegabytes( dkptr->header_name ) ;
     int rr = blk->total_bytes / (1024l * 1024l) ;
     if( mm >= 0 && mm <= rr )
       WARNING_message("Disk space: writing file %s (%d MB),"
                       " but only %d free MB on disk"        ,
         dkptr->brick_name , rr , mm ) ;
   }

   /*-- write data out in whatever format is ordered --*/

   nx = dkptr->dimsizes[0] ;
   ny = dkptr->dimsizes[1] ;  nxy  = nx * ny ;
   nz = dkptr->dimsizes[2] ;  nxyz = nxy * nz ;
   nv = dkptr->nvals ;        nb   = blk->total_bytes ;

   switch( dkptr->storage_mode ){

      default: WRITE_ERR("illegal storage_mode!") ; break ;

      case STORAGE_BY_BRICK:{
         FILE *far ;
         Boolean purge_when_done = False , ok ;
         int force_gzip=0 , csave=COMPRESS_NONE ;

         /** if we have a mmap-ed file, copy into RAM (ugh) **/

         if( blk->malloc_type == DATABLOCK_MEM_MMAP ){
            char *bnew , *bold ;
            int offset ;

            bnew = (char *) malloc( (size_t)nb ) ;  /* work space */
            bold = DBLK_ARRAY(blk,0) ;              /* start of mapped file */

            if( bnew == NULL )
              WRITE_ERR("cannot rewrite due to malloc failure - is memory exhausted?") ;

            memcpy( bnew , bold , (size_t)nb ) ;    /* make a copy,    */
            munmap( (void *) bold , (size_t)nb ) ;  /* then unmap file */

            /* fix sub-brick pointers */

            offset = 0 ;
            for( ibr=0 ; ibr < nv ; ibr++ ){
               mri_fix_data_pointer( (void *)(bnew+offset) , DBLK_BRICK(blk,ibr) ) ;
               offset += DBLK_BRICK_BYTES(blk,ibr) ;
               DBLK_BRICK(blk,ibr)->fondisk = 0 ;   /* 31 Jan 2007 */
            }

            purge_when_done = True ;

         } /** fall thru to here if have a malloc-ed dataset **/

         if( save_order != native_order ) purge_when_done = True ;

         /** delete old file, if any **/

         COMPRESS_unlink( dkptr->brick_name ) ; /* Feb 1998 */

         /** create new file **/

         id = strlen(dkptr->directory_name) ;
         ok = ( dkptr->directory_name[id-1] == '/' ) ;
         if( ok ) sprintf( dkptr->brick_name , "%s%s.%s",
                           dkptr->directory_name ,
                           dkptr->filecode , DATASET_BRICK_SUFFIX );

         else     sprintf( dkptr->brick_name , "%s/%s.%s",
                           dkptr->directory_name ,
                           dkptr->filecode , DATASET_BRICK_SUFFIX );

      /** COMPRESS for output added Feb 1998 */

         if( compress_mode == COMPRESS_NOFILE ) THD_enviro_write_compression() ;

#ifdef COMPRESS_GZIP
         /*-- 02 Mar 2001: check if we will force gzip --*/

         if( compress_mode == COMPRESS_NONE && AFNI_yesenv("AFNI_AUTOGZIP") ){
            double entrop = ENTROPY_datablock(blk) ;
            force_gzip = (entrop < 2.7) ;
#if 0
fprintf(stderr,"Entropy=%g ==> forcing write gzip on %s\n",entrop,dkptr->brick_name) ;
#endif
         } else {
            force_gzip = 0 ;
         }
         if( force_gzip ){
            csave = compress_mode ; compress_mode = COMPRESS_GZIP ;
         }
#endif

         far = COMPRESS_fopen_write( dkptr->brick_name , compress_mode ) ;
         if( far == NULL ){
           if( compress_mode != COMPRESS_NONE ){
             compress_mode = COMPRESS_NONE ; force_gzip = 0 ;
             far = COMPRESS_fopen_write( dkptr->brick_name , compress_mode ) ;
           }
         }
         if( far == NULL )
           WRITE_ERR("cannot open output brick file - do you have write permission?") ;

         /** write each brick out in a separate operation **/

         idone = 0 ;
         for( ibr=0 ; ibr < nv ; ibr++ ){

           do_mripurge = MRI_IS_PURGED( DBLK_BRICK(blk,ibr) ) ;
           if( do_mripurge ) mri_unpurge( DBLK_BRICK(blk,ibr) ) ;

           if( save_order != native_order ){       /* 25 April 1998 */
             switch( DBLK_BRICK_TYPE(blk,ibr) ){
               case MRI_short:
                 mri_swap2( DBLK_BRICK_NVOX(blk,ibr) , DBLK_ARRAY(blk,ibr) ) ;
               break ;

               case MRI_complex:   /* 23 Nov 1999 */
                 mri_swap4( 2*DBLK_BRICK_NVOX(blk,ibr), DBLK_ARRAY(blk,ibr)) ;
               break ;

               case MRI_float:     /* 23 Nov 1999 */
               case MRI_int:
                 mri_swap4( DBLK_BRICK_NVOX(blk,ibr) , DBLK_ARRAY(blk,ibr) ) ;
               break ;
             }
           }

           idone += fwrite( DBLK_ARRAY(blk,ibr), 1, DBLK_BRICK_BYTES(blk,ibr), far );

           if( do_mripurge ){                    /* 31 Jan 2007 */
             if( !purge_when_done ) mri_purge( DBLK_BRICK(blk,ibr) ) ;
             else                   mri_clear( DBLK_BRICK(blk,ibr) ) ;
           }
         } /* end of loop over sub-bricks */

         COMPRESS_fclose(far) ;

         if( purge_when_done ){
           if( blk->malloc_type == DATABLOCK_MEM_MMAP ){
             free( DBLK_ARRAY(blk,0) ) ;
             for( ibr=0 ; ibr < nv ; ibr++ )
               mri_clear_data_pointer( DBLK_BRICK(blk,ibr) ) ;
           } else {
             THD_purge_datablock( blk , DATABLOCK_MEM_MALLOC ) ;
           }
         }

         if( compress_mode >= 0 || save_order != native_order ){
           blk->malloc_type = DATABLOCK_MEM_MALLOC ;
         }
         DBLK_mmapfix(blk) ;  /* 28 Mar 2005 */

         if( force_gzip ) compress_mode = csave ; /* 02 Mar 2001 */

         if( idone != blk->total_bytes )
           WRITE_ERR("Write error in brick file: Is disk full, or write_protected?") ;

         dkptr->byte_order = save_order ;  /* 23 Nov 1999 */

         return True ;
      }
      break ;

   }  /* end of switch over data storage mode */

   return False ;  /* should NEVER be reached */
}
