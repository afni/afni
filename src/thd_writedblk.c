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

int THD_get_write_compression(void)
{
   if( compress_mode == COMPRESS_NOFILE ) THD_enviro_write_compression() ;
   return compress_mode ;
}

int THD_enviro_write_compression(void)
{
   char * hh = my_getenv("AFNI_COMPRESSOR") ;
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

void THD_enviro_write_order(void)
{
   char * hh = my_getenv("AFNI_BYTEORDER") ;

   if( hh == NULL ){ output_order = -1 ; return ; }

   if( strcmp(hh,LSB_FIRST_STRING) == 0 ){ output_order = LSB_FIRST; return; }
   if( strcmp(hh,MSB_FIRST_STRING) == 0 ){ output_order = MSB_FIRST; return; }

   output_order = -1 ; return ;
}

int THD_get_write_order(void)
{
   if( native_order < 0 ) native_order = mri_short_order() ;
   if( output_order < 0 ) THD_enviro_write_order() ;

   return (output_order > 0) ? output_order
                             : native_order ;
}

/*---------------------------------------------------------------------
   Write a datablock to disk.
   Returns True if OK, False if an error.
   [see also AFNI_refashion_dataset]
-----------------------------------------------------------------------*/

Boolean THD_write_datablock( THD_datablock * blk , Boolean write_brick )
{
   THD_diskptr * dkptr ;
   Boolean good ;
   int id , nx , ny , nz , nv , nxy , nxyz , ibr , nb ;
   int atrank[ATRSIZE_DATASET_RANK] , atdims[ATRSIZE_DATASET_DIMENSIONS] ;
   MRI_IMAGE * im ;
   int save_order ;

   /*-- sanity checks --*/

   if( ! ISVALID_DATABLOCK(blk) ) return False ;
   if( DBLK_IS_MASTERED(blk) )    return False ;  /* 11 Jan 1999 */

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

   /*-- write attributes (first, set those stored in the diskptr or dblk ) --*/

   atrank[0] = dkptr->rank ;
   atrank[1] = dkptr->nvals ;
   for( id=2 ; id < ATRSIZE_DATASET_RANK ; id++ ) atrank[id] = 0 ;

   THD_set_int_atr( blk , ATRNAME_DATASET_RANK ,
                          ATRSIZE_DATASET_RANK , atrank ) ;

   for( id=0 ; id < ATRSIZE_DATASET_DIMENSIONS ; id++ )
      atdims[id] = (id < dkptr->rank) ? dkptr->dimsizes[id] : 0 ;

   THD_set_int_atr( blk , ATRNAME_DATASET_DIMENSIONS ,
                          ATRSIZE_DATASET_DIMENSIONS , atdims ) ;

   { int * datum_type ;
     datum_type = malloc( sizeof(int) * blk->nvals ) ;
     for( id=0 ; id < blk->nvals ; id++ )
        datum_type[id] = DBLK_BRICK_TYPE(blk,id) ;
     THD_set_int_atr(   blk , ATRNAME_BRICK_TYPES  , blk->nvals , datum_type ) ;
     free( datum_type ) ;
   }
   THD_set_float_atr( blk , ATRNAME_BRICK_FLTFAC , blk->nvals , blk->brick_fac ) ;

   /** 30 Nov 1997: write out brick labels **/

   if( blk->brick_lab != NULL ){

      int ibr , nch , ipos , ll ;
      char * car ;

      for( ibr=0,nch=0 ; ibr < blk->nvals ; ibr++ )   /* total length  */
         nch += strlen(blk->brick_lab[ibr]) + 1 ;     /* of all labels */

      car = (char *) malloc( sizeof(char) * nch ) ;   /* space for all labels */

      for( ibr=0,ipos=0 ; ibr < blk->nvals ; ibr++ ){   /* put all labels */
         ll = strlen(blk->brick_lab[ibr]) + 1 ;         /* together       */
         memcpy( car+ipos , blk->brick_lab[ibr] , ll ) ;
         ipos += ll ;
      }

      THD_set_char_atr( blk , ATRNAME_BRICK_LABS , nch , car ) ;
      free(car) ;
   } else {
      THD_erase_one_atr( blk , ATRNAME_BRICK_LABS ) ;
   }

   /** and write out brick keywords **/

   if( blk->brick_keywords != NULL ){

      int ibr , nch , ipos , ll ;
      char * car ;

      for( ibr=0,nch=0 ; ibr < blk->nvals ; ibr++ ){
         if( blk->brick_keywords[ibr] != NULL )
            nch += strlen(blk->brick_keywords[ibr]) + 1 ;
         else
            nch += 1 ;
      }

      car = (char *) malloc( sizeof(char) * nch ) ;

      for( ibr=0,ipos=0 ; ibr < blk->nvals ; ibr++ ){
         if( blk->brick_keywords[ibr] != NULL ){
            ll = strlen(blk->brick_keywords[ibr]) + 1 ;
            memcpy( car+ipos , blk->brick_keywords[ibr] , ll ) ;
            ipos += ll ;
         } else {
            car[ipos++] = '\0' ;
         }
      }

      THD_set_char_atr( blk , ATRNAME_BRICK_KEYWORDS , nch , car ) ;
      free(car) ;
   } else {
      THD_erase_one_atr( blk , ATRNAME_BRICK_KEYWORDS ) ;
   }


   /* and write out brick stataux parameters */

   if( blk->brick_statcode != NULL &&    /* write out brick stataux */
       blk->brick_stataux  != NULL   ){  /* stuff, if it exists.    */

      int ibr , nfl , jv , ipos , iv ;
      float * far ;

      for( ibr=0,nfl=0 ; ibr < blk->nvals ; ibr++ ){    /* compute total */
         jv = blk->brick_statcode[ibr] ;                /* space needed  */
         if( FUNC_IS_STAT(jv) ) nfl += FUNC_need_stat_aux[jv] + 3 ;
      }

      if( nfl > 0 ){
         far = (float *) malloc( sizeof(float) * nfl ) ;

         for( ibr=0,ipos=0 ; ibr < blk->nvals ; ibr++ ){
            jv = blk->brick_statcode[ibr] ;
            if( FUNC_IS_STAT(jv) ){
               far[ipos++] = ibr ;                      /* save index */
               far[ipos++] = jv ;                       /* save statcode */
               far[ipos++] = FUNC_need_stat_aux[jv] ;   /* save # of params */

               if( blk->brick_stataux[ibr] != NULL ){   /* if have params, save */
                  for( iv=0 ; iv < FUNC_need_stat_aux[jv] ; iv++ )
                     far[ipos++] = blk->brick_stataux[ibr][iv] ;
               } else {                                 /* should never be used */
                  for( iv=0 ; iv < FUNC_need_stat_aux[jv] ; iv++ )
                     far[ipos++] = 0.0 ;
               }
            }
         }

         THD_set_float_atr( blk , ATRNAME_BRICK_STATAUX , nfl , far ) ;
         free(far) ;
      } else {
         THD_erase_one_atr( blk , ATRNAME_BRICK_STATAUX ) ;
      }
   } else {
      THD_erase_one_atr( blk , ATRNAME_BRICK_STATAUX ) ;
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
      WRITE_ERR("failure to write attributes - is disk full? do you have write permission?") ;

   /*-- if not writing data, can exit --*/

   if( write_brick == False || blk->brick == NULL ||
       dkptr->storage_mode == STORAGE_UNDEFINED     ) return True ;

   /*-- check each brick for existence:
          if none exist, cannot write, but is OK
          if some but not all exist, cannot write, and is an error --*/

   id = THD_count_databricks( blk ) ;
   if( id <= 0 )         return True ;
   if( id < blk->nvals ) WRITE_ERR("only partial data exists in memory") ;

   if( blk->malloc_type == DATABLOCK_MEM_UNDEFINED )
      WRITE_ERR("undefined data exists in memory") ;

   /*-- write data out in whatever format is ordered --*/

   nx = dkptr->dimsizes[0] ;
   ny = dkptr->dimsizes[1] ;  nxy  = nx * ny ;
   nz = dkptr->dimsizes[2] ;  nxyz = nxy * nz ;
   nv = dkptr->nvals ;        nb   = blk->total_bytes ;

   switch( dkptr->storage_mode ){

      default: WRITE_ERR("illegal storage_mode!") ; break ;

      case STORAGE_BY_BRICK:{
         FILE * far ;
         Boolean purge_when_done = False , ok ;

         /** if we have a mmap-ed file, copy into RAM **/

         if( blk->malloc_type == DATABLOCK_MEM_MMAP ){
            char * bnew , * bold ;
            int offset ;

            bnew = (char *) malloc( nb ) ;  /* work space */
            bold = DBLK_ARRAY(blk,0) ;      /* start of mapped file */

            if( bnew == NULL )
              WRITE_ERR("cannot rewrite due to malloc failure - is memory exhausted?") ;

            memcpy( bnew , bold , nb ) ;    /* make a copy,    */
            munmap( (void *) bold , nb ) ;  /* then unmap file */

            /* fix sub-brick pointers */

            offset = 0 ;
            for( ibr=0 ; ibr < nv ; ibr++ ){
               mri_fix_data_pointer( (void *)(bnew+offset) , DBLK_BRICK(blk,ibr) ) ;
               offset += DBLK_BRICK_BYTES(blk,ibr) ;
            }

            purge_when_done = True ;
         }

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

         far = COMPRESS_fopen_write( dkptr->brick_name , compress_mode ) ;
         if( far == NULL )
           WRITE_ERR("cannot open output brick file - do you have write permission?") ;

         /** write each brick out in a separate operation **/

         id = 0 ;
         for( ibr=0 ; ibr < nv ; ibr++ ){

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

            id += fwrite( DBLK_ARRAY(blk,ibr), 1, DBLK_BRICK_BYTES(blk,ibr), far );
         }

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

         if( id != blk->total_bytes )
            WRITE_ERR("write error in brick file - is disk full?") ;

         dkptr->byte_order = save_order ;  /* 23 Nov 1999 */

         return True ;
      }
      break ;

   }  /* end of switch over data storage mode */

   return False ;  /* should NEVER be reached */
}
