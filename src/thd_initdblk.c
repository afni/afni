/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include "thd.h"

/*----------------------------------------------------------------
  Given a directory name, and a header filename, create a
  datablock that corresponds (return NULL if impossible).
------------------------------------------------------------------*/

static int native_order = -1 ;
static int no_mmap      = -1 ;
static int no_ordwarn   = -1 ;

THD_datablock * THD_init_one_datablock( char * dirname , char * headname )
{
   THD_datablock     * dblk ;
   THD_diskptr       * dkptr ;
   ATR_int           * atr_rank , * atr_dimen , * atr_scene , * atr_btype ;
   ATR_float         * atr_flt ;
   ATR_string        * atr_labs ;
   int ii , view_type , func_type , dset_type , nx,ny,nz,nvox , nvals , ibr,typ ;
   Boolean ok ;
   char prefix[THD_MAX_NAME] ;
   MRI_IMAGE * qim ;
   int brick_ccode ;
   int default_order ;   /* 21 Jun 2000 */

   if( native_order < 0 ) native_order = mri_short_order() ;

   no_mmap    = AFNI_yesenv("AFNI_NOMMAP") ;
   no_ordwarn = AFNI_yesenv("AFNI_NO_BYTEORDER_WARNING") ;

   { char * hh = getenv("AFNI_BYTEORDER_INPUT") ;    /* 21 Jun 2000 */
     default_order = native_order ;
     if( hh != NULL ){
        if( strncmp(hh,LSB_FIRST_STRING,ORDER_LEN) == 0 )
           default_order = LSB_FIRST ;
        else if( strncmp(hh,MSB_FIRST_STRING,ORDER_LEN) == 0 )
           default_order = MSB_FIRST ;
     }
   }

   /*-- sanity check --*/

   if( dirname  == NULL || strlen(dirname)  == 0 ||
       headname == NULL || strlen(headname) == 0   ) return NULL ;

   FILENAME_TO_PREFIX(headname,prefix) ;
   if( strlen(prefix) == 0 ||
       strstr(headname,DATASET_HEADER_SUFFIX) == NULL ) return NULL ;

   /*-- create output datablock --*/

   dblk              = myXtNew( THD_datablock ) ;
   dblk->type        = DATABLOCK_TYPE ;
   dblk->brick       = NULL ;  /* will be filled in below */
   dblk->brick_bytes = NULL ;  /* ditto */
   dblk->brick_fac   = NULL ;  /* ditto */
   dblk->total_bytes = 0 ;     /* ditto */
   dblk->malloc_type = DATABLOCK_MEM_UNDEFINED ;
   dblk->parent      = NULL ;

   dblk->brick_lab      = NULL ;  /* 30 Nov 1997 */
   dblk->brick_keywords = NULL ;
   dblk->brick_statcode = NULL ;
   dblk->brick_stataux  = NULL ;

   dblk->master_nvals = 0 ;     /* 11 Jan 1999 */
   dblk->master_ival  = NULL ;
   dblk->master_bytes = NULL ;

   DBLK_unlock(dblk) ;  /* Feb 1998 */

   INIT_KILL(dblk->kl) ;

   dblk->diskptr       = dkptr = myXtNew( THD_diskptr ) ;
   dkptr->type         = DISKPTR_TYPE ;
   dkptr->storage_mode = STORAGE_UNDEFINED ;
#if 0
   dkptr->byte_order   = native_order ;  /* 25 April 1998 */
#else
   dkptr->byte_order   = default_order;  /* 21 June 2000 */
#endif

   ADDTO_KILL(dblk->kl,dkptr) ;

   THD_read_all_atr( headname , dblk ) ;

   if( dblk->natr <= 0 ){
      THD_delete_datablock( dblk ) ;
      myXtFree(dblk) ;
      return NULL ;
   }

   /*-- get relevant attributes: rank, dimensions, view_type & func_type --*/

   atr_rank  = THD_find_int_atr( dblk , ATRNAME_DATASET_RANK ) ;
   atr_dimen = THD_find_int_atr( dblk , ATRNAME_DATASET_DIMENSIONS ) ;
   atr_scene = THD_find_int_atr( dblk , ATRNAME_SCENE_TYPE ) ;

   /*-- missing an attribute ==> quit now --*/

   if( atr_rank == NULL || atr_dimen == NULL || atr_scene == NULL ){
      THD_delete_datablock( dblk ) ;
      myXtFree(dblk) ;
      return NULL ;
   }

   /*-- load type codes from SCENE attribute --*/

   view_type = atr_scene->in[0] ;
   func_type = atr_scene->in[1] ;
   dset_type = atr_scene->in[2] ;

   /*-- load other values from attributes into relevant places --*/

   ok   = True ;
   nvox = 1 ;

   dkptr->rank = atr_rank->in[0] ;                /* N.B.: rank isn't used much */
   for( ii=0 ; ii < dkptr->rank ; ii++ ){
      dkptr->dimsizes[ii] = atr_dimen->in[ii] ;
      ok                  = ( ok && dkptr->dimsizes[ii] > 1 ) ;
      nvox               *= dkptr->dimsizes[ii] ;
   }
   dkptr->nvals = dblk->nvals = nvals = atr_rank->in[1] ;  /* but nvals is used */

   if( !ok || nvals < 1 ||
       dkptr->rank < THD_MIN_RANK || dkptr->rank > THD_MAX_RANK ){

      fprintf(stderr,"\n*** Illegal dataset rank or dimen in file %s ***\n",
              headname ) ;
      THD_delete_datablock( dblk ) ;
      myXtFree(dblk) ;
      return NULL ;
   }

   /*-- create the storage filenames --*/

   THD_init_diskptr_names( dkptr, dirname,NULL,prefix , view_type , True ) ;

   /*-- determine if the BRICK file exists --*/

   brick_ccode = COMPRESS_filecode(dkptr->brick_name) ;
   if( brick_ccode != COMPRESS_NOFILE ){
       dkptr->storage_mode = STORAGE_BY_BRICK ;
   }

   /*-- now set the memory allocation codes, etc. --*/

   dblk->brick_fac = (float *) XtMalloc( sizeof(float) * nvals ) ;
   for( ibr=0 ; ibr < nvals ; ibr++ ) dblk->brick_fac[ibr] = 0.0 ;

   /* scaling factors from short type to float type, if nonzero */

   atr_flt = THD_find_float_atr( dblk , ATRNAME_BRICK_FLTFAC ) ;
   if( atr_flt != NULL ){
      for( ibr=0 ; ibr < nvals && ibr < atr_flt->nfl ; ibr++ )
         dblk->brick_fac[ibr] = atr_flt->fl[ibr] ;
   }

   /** Now create an empty shell of the "brick" == the data structure
       that will hold all the voxel data.  Note that all datablocks
       will have a brick, even if they never actually contain data
       themselves (are only warp-on-demand).

       If the BRICK_TYPES input attribute doesn't exist, then all
       sub-bricks are shorts.  This makes the code work with old-style
       datasets, which were always made up of shorts.
   **/

   atr_btype = THD_find_int_atr( dblk , ATRNAME_BRICK_TYPES ) ;

   if( atr_btype == NULL ){
      THD_init_datablock_brick( dblk , MRI_short , NULL ) ;
   } else {
      THD_init_datablock_brick( dblk , atr_btype->nin , atr_btype->in ) ;
   }

   /* 25 April 1998: check if the byte order is stored inside */

   atr_labs = THD_find_string_atr( dblk , ATRNAME_BYTEORDER ) ;
   if( atr_labs != NULL && atr_labs->nch > 0 ){

      if( strncmp(atr_labs->ch,LSB_FIRST_STRING,ORDER_LEN) == 0 )
         dkptr->byte_order = LSB_FIRST ;
      else if( strncmp(atr_labs->ch,MSB_FIRST_STRING,ORDER_LEN) == 0 )
         dkptr->byte_order = MSB_FIRST ;
      else
         fprintf(stderr,"*** Unknown %s found in dataset %s\n",
                 ATRNAME_BYTEORDER , headname ) ;

   } else if( !no_ordwarn &&
              DBLK_BRICK_TYPE(dblk,0) != MRI_byte ){ /* 20 Sep 1999 */

      static int first=1 ;
      if( first ){
         fprintf(stderr,
           "\n*** The situation below can be rectified with program '3drefit -byteorder':\n");
         first = 0 ;
      }
      fprintf(stderr,"*** Dataset %s: assuming byteorder %s\n",
              headname , BYTE_ORDER_STRING(dkptr->byte_order)  ) ;
   }

   /* if the data is not on disk, the flag remains at DATABLOCK_MEM_UNDEFINED,
      otherwise the flag says how the memory for the bricks is to be created. */

   if( dkptr->storage_mode == STORAGE_BY_BRICK ){
#if MMAP_THRESHOLD > 0
      dblk->malloc_type = (dblk->total_bytes > MMAP_THRESHOLD)
                          ? DATABLOCK_MEM_MMAP : DATABLOCK_MEM_MALLOC ;
#else
      dblk->malloc_type = DATABLOCK_MEM_MALLOC ;
#endif

      /* must be malloc-ed if:
            data is compressed,
            data is not in native byte order, or
            user explicity forbids use of mmap   */

      if( brick_ccode >= 0 || dkptr->byte_order != native_order || no_mmap )
         dblk->malloc_type = DATABLOCK_MEM_MALLOC ;
   }

   /* 30 Nov 1997: create the labels for sub-bricks */

   THD_init_datablock_labels( dblk ) ;

   atr_labs = THD_find_string_atr( dblk , ATRNAME_BRICK_LABS ) ;

   if( atr_labs != NULL && atr_labs->nch > 0 ){  /* create labels from attribute */
      int ipos = -1 , ipold , ngood ;

      for( ibr=0 ; ibr < nvals ; ibr++ ){  /* loop over bricks */

         for( ipold = ipos++ ;                                     /* skip to */
              ipos < atr_labs->nch && atr_labs->ch[ipos] != '\0' ; /* next \0 */
              ipos++ ) /* nada */ ;                                /* or end. */

         ngood = ipos - ipold - 1 ;                   /* number of good chars */
         if( ngood > 0 ){
            XtFree(dblk->brick_lab[ibr]) ;
            dblk->brick_lab[ibr] = (char *) XtMalloc(sizeof(char)*(ngood+2)) ;
            memcpy( dblk->brick_lab[ibr] , atr_labs->ch+(ipold+1) , ngood ) ;
            dblk->brick_lab[ibr][ngood] = '\0' ;
         }

         if( ipos >= atr_labs->nch ) break ;  /* nothing more to do */
      } /* end of loop over sub-bricks */
   }

   /* create the keywords for sub-bricks */

   THD_init_datablock_keywords( dblk ) ;

   atr_labs = THD_find_string_atr( dblk , ATRNAME_BRICK_KEYWORDS ) ;

   if( atr_labs != NULL && atr_labs->nch > 0 ){  /* create keywords from attribute */
      int ipos = -1 , ipold , ngood ;

      for( ibr=0 ; ibr < nvals ; ibr++ ){  /* loop over bricks */

         for( ipold = ipos++ ;                                     /* skip to */
              ipos < atr_labs->nch && atr_labs->ch[ipos] != '\0' ; /* next \0 */
              ipos++ ) /* nada */ ;                                /* or end. */

         ngood = ipos - ipold - 1 ;                   /* number of good chars */
         if( ngood > 0 ){
            XtFree(dblk->brick_keywords[ibr]) ;
            dblk->brick_keywords[ibr] = (char *) XtMalloc(sizeof(char)*(ngood+2)) ;
            memcpy( dblk->brick_keywords[ibr] , atr_labs->ch+(ipold+1) , ngood ) ;
            dblk->brick_keywords[ibr][ngood] = '\0' ;
         }

         if( ipos >= atr_labs->nch ) break ;  /* nothing more to do */
      } /* end of loop over sub-bricks */
   }

   /* create the auxiliary statistics stuff for each brick, if present */

   atr_flt = THD_find_float_atr( dblk , ATRNAME_BRICK_STATAUX ) ;
   if( atr_flt != NULL && atr_flt->nfl >= 3 ){
      int ipos=0 , iv,nv,jv ;

      /* attribute stores all stataux stuff as follows:
           sub-brick-index  statcode  no.-of-values value ... value
           sub-brick-index  statcode  no.-of-values value ... value, etc. */

      while( ipos <= atr_flt->nfl - 3 ){
         iv = (int) ( atr_flt->fl[ipos++] ) ;  /* which sub-brick */
         jv = (int) ( atr_flt->fl[ipos++] ) ;  /* statcode */
         nv = (int) ( atr_flt->fl[ipos++] ) ;  /* # of values that follow */

         if( nv > atr_flt->nfl - ipos ) nv = atr_flt->nfl - ipos ;

         THD_store_datablock_stataux( dblk , iv , jv , nv , atr_flt->fl + ipos ) ;
         ipos += nv ;
      }
   }

   return dblk ;
}

/*----------------------------------------------------------------
  Initialize the brick structure of a datablock.  This will
  contain no data for now, but provides a place for it to go.
  This routine presumes that the datablock is already set up
  with the dimension information, etc.

  Inputs ntype and btype determine the types of data in the
  sub-bricks.
    If btype == NULL, then the type code for all sub-bricks
                        is given by ntype;
    If ntype < 0,     then the type code for all sub-bricks
                        is taken from the datablock pointed
                        to by (THD_datablock *) btype;
    If btype != NULL  then the type code for each sub-brick
      && ntype > 0      is taken from the array pointed to
                        by (int *) btype.
------------------------------------------------------------------*/

void THD_init_datablock_brick( THD_datablock * dblk ,
                               int ntype , void * btype )
{
   int ibr , nx,ny,nz , typ , nvals ;
   MRI_IMAGE * qim ;
   THD_datablock * pblk = NULL ;
   int * itype = NULL ;

   if( ! ISVALID_DATABLOCK(dblk)   ) return ;   /* bad inputs */
   if( ntype <  0 && btype == NULL ) return ;
   if( ntype == 0 && btype != NULL ) return ;

   if( ntype < 0 ){                             /* copy types from */
      pblk = (THD_datablock *) btype ;          /* datablock pblk  */
      if( ! ISVALID_DATABLOCK(pblk) ) return ;
   } else {
      itype = (int *) btype ;
   }

   nx    = dblk->diskptr->dimsizes[0] ;
   ny    = dblk->diskptr->dimsizes[1] ;
   nz    = dblk->diskptr->dimsizes[2] ;
   nvals = dblk->nvals ; if( nvals < 1 ) return ; /* something wrong */

   /** make brick information arrays, if not pre-existing **/

   if( dblk->brick_bytes == NULL ){
STATUS("making dblk->brick_bytes") ;
      dblk->brick_bytes = (int *) XtMalloc( sizeof(int) * nvals ) ;
   }

   if( dblk->brick_fac == NULL ){
STATUS("making dblk->brick_fac") ;
      dblk->brick_fac = (float *) XtMalloc( sizeof(float) * nvals ) ;
      for( ibr=0 ; ibr < nvals ; ibr++ )
         dblk->brick_fac[ibr] = (ntype < 0) ? pblk->brick_fac[ibr] : 0.0 ;
   }

   dblk->total_bytes = 0 ;

   if( dblk->brick != NULL ){
STATUS("destroying old dblk->brick") ;
     DESTROY_IMARR( dblk->brick ) ;
   }

STATUS("making new dblk->brick") ;
   INIT_IMARR( dblk->brick ) ;  /* make the new brick */

   /** set up each sub-brick **/

STATUS("starting sub-brick creations") ;
   for( ibr=0 ; ibr < nvals ; ibr++ ){
      if( ntype < 0 ){     typ = DBLK_BRICK_TYPE(pblk,ibr) ;
      } else if( itype == NULL ){
                           typ = ntype ;  /* all types are the same */
      } else {
         if( ibr < ntype ) typ = itype[ibr] ;     /* types may vary */
         else              typ = itype[ntype-1] ;
      }

      qim = mri_new_vol_empty( nx,ny,nz , typ ) ;  /* image with no data */
      ADDTO_IMARR( dblk->brick , qim ) ;

      dblk->brick_bytes[ibr] = qim->pixel_size * qim->nvox ;
      dblk->total_bytes     += dblk->brick_bytes[ibr] ;
   }

STATUS("exiting") ;
   return ;
}

/*----------------------------------------------------------------
  Determine if the brick factors are needed.
------------------------------------------------------------------*/

int THD_need_brick_factor( THD_3dim_dataset * dset )
{
   int ii , nval ;

   if( ! ISVALID_DSET(dset)            ) return 0 ;
   if( ! ISVALID_DATABLOCK(dset->dblk) ) return 0 ;
   if( dset->dblk->brick_fac == NULL   ) return 0 ;

   nval = DSET_NVALS(dset) ;
   for( ii=0 ; ii < nval ; ii++ )
      if( DSET_BRICK_FACTOR(dset,ii) != 0.0 &&
          DSET_BRICK_FACTOR(dset,ii) != 1.0   ) return 1 ;

   return 0 ;
}
