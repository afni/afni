#include "3ddata.h"
#include "thd.h"


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

   /*-- sanity checks --*/

   if( ! ISVALID_DATABLOCK(blk) ) return False ;

   dkptr = blk->diskptr ;
   if( ! ISVALID_DISKPTR(dkptr) ) WRITE_ERR("illegal file type") ;

   if( strlen(dkptr->directory_name) == 0 ||
       strlen(dkptr->header_name)    == 0 ||
       strlen(dkptr->filecode)       == 0   ) WRITE_ERR("illegal file names") ;

   if( dkptr->rank != 3 ) WRITE_ERR("cannot write non-3D datablock") ;

   /*-- create directory if necessary --*/

   if( ! THD_is_directory(dkptr->directory_name) ){
      id = mkdir( dkptr->directory_name , THD_MKDIR_MODE ) ;
      if( id != 0 ){
         fprintf(stderr,
              "\n*** cannot mkdir new directory: %s\n",dkptr->directory_name) ;
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
      }
   }

   /*-- actually write attributes to disk --*/

   good = THD_write_atr( blk ) ;
   if( good == False ) WRITE_ERR("failure to write attributes!") ;

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
               WRITE_ERR("cannot rewrite due to malloc failure") ;

            memcpy( bnew , bold , nb ) ;    /* make a copy,    */
            munmap( (void *) bold , nb ) ;  /* then unmap file */

            offset = 0 ;
            for( ibr=0 ; ibr < nv ; ibr++ ){
               mri_fix_data_pointer( (void *)(bnew+offset) , DBLK_BRICK(blk,ibr) ) ;
               offset += DBLK_BRICK_BYTES(blk,ibr) ;
            }

            purge_when_done = True ;
         }

         /** delete old file, if any **/

#ifdef  ALLOW_COMPRESSOR
         { char * fff = COMPRESS_filename(dkptr->brick_name) ;
           if( THD_is_file(fff) ){ unlink(fff) ; free(fff) ; }
         }
#else
         if( THD_is_file(dkptr->brick_name) ) unlink( dkptr->brick_name ) ;
#endif

         /** create new file **/

         id = strlen(dkptr->directory_name) ;
         ok = ( dkptr->directory_name[id-1] == '/' ) ;
         if( ok ) sprintf( dkptr->brick_name , "%s%s.%s",
                           dkptr->directory_name ,
                           dkptr->filecode , DATASET_BRICK_SUFFIX );

         else     sprintf( dkptr->brick_name , "%s/%s.%s",
                           dkptr->directory_name ,
                           dkptr->filecode , DATASET_BRICK_SUFFIX );

      /** COMPRESS not supported for output (yet) **/

         far = fopen( dkptr->brick_name , "w" ) ;
         if( far == NULL ) WRITE_ERR("cannot open output brick file") ;

         /** write each brick out in a separate operation **/

         id = 0 ;
         for( ibr=0 ; ibr < nv ; ibr++ )
            id += fwrite( DBLK_ARRAY(blk,ibr), 1, DBLK_BRICK_BYTES(blk,ibr), far ) ;

#if 0
         fsync(fileno(far)) ;
#endif
         fclose(far) ;

         if( purge_when_done ){
            free( DBLK_ARRAY(blk,0) ) ;
            for( ibr=0 ; ibr < nv ; ibr++ )
               mri_clear_data_pointer( DBLK_BRICK(blk,ibr) ) ;
         }

         if( id != blk->total_bytes )
            WRITE_ERR("write error in brick file (is disk full?)") ;

         return True ;
      }
      break ;

   }  /* end of switch over data storage mode */

   return False ;  /* should NEVER be reached */
}
