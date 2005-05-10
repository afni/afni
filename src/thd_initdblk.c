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

THD_datablock * THD_init_one_datablock( char *dirname , char *headname )
{
   THD_datablock *dblk ;
   THD_diskptr   *dkptr ;
   int ii ;
   char prefix[THD_MAX_NAME] = "\0" ;
   int default_order ;   /* 21 Jun 2000 */

ENTRY("THD_init_one_datablock") ;

   /*-- sanity checks --*/

   if( dirname  == NULL || strlen(dirname)  == 0 ||
       headname == NULL || strlen(headname) == 0   ) RETURN( NULL ) ;

   FILENAME_TO_PREFIX(headname,prefix) ;
   if( strlen(prefix) == 0 ||
       strstr(headname,DATASET_HEADER_SUFFIX) == NULL ) RETURN( NULL ) ;

   /*-- byte ordering stuff --*/

   if( native_order < 0 ) native_order = mri_short_order() ;

   no_mmap    = AFNI_yesenv("AFNI_NOMMAP") ;
   no_ordwarn = AFNI_yesenv("AFNI_NO_BYTEORDER_WARNING") ;

   { char *hh = getenv("AFNI_BYTEORDER_INPUT") ;    /* 21 Jun 2000 */
     default_order = native_order ;
     if( hh != NULL ){
       if( strncmp(hh,LSB_FIRST_STRING,ORDER_LEN) == 0 )
         default_order = LSB_FIRST ;
       else if( strncmp(hh,MSB_FIRST_STRING,ORDER_LEN) == 0 )
         default_order = MSB_FIRST ;
     }
   }

#if 1

   dblk  = EDIT_empty_datablock() ;    /* 11 Mar 2005 -- the new way */
   dkptr = dblk->diskptr ;

#else
   /*-- create output datablock (the old way) --*/

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

   dblk->master_bot = 1.0 ;     /* 21 Feb 2001 */
   dblk->master_top = 0.0 ;

   DBLK_unlock(dblk) ;  /* Feb 1998 */

   dblk->shm_idcode[0] = '\0' ;  /* 02 May 2003 */

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

#endif  /* end of initializing empty datablock and diskptr */

   /*-- read attributes from disk, store in the datablock --*/

   THD_read_all_atr( headname , dblk ) ;

   /*-- 09 Mar 2005: all the attribute processing is moved away --*/

   ii = THD_datablock_from_atr( dblk, dirname, headname ) ;
   if( ii == 0 ){
     THD_delete_datablock( dblk ) ;
     myXtFree(dblk) ;
     RETURN( NULL ) ;
   }

#if 0
   if( PRINT_TRACING ){
     char str[256] ;
     sprintf(str,"rank=%d nvals=%d dim[0]=%d dim[1]=%d dim[2]=%d",
             dkptr->rank , dkptr->nvals ,
             dkptr->dimsizes[0] , dkptr->dimsizes[1] , dkptr->dimsizes[2] ) ;
     STATUS(str) ;
   }
#endif

   RETURN( dblk ) ;
}

/*-----*/

#undef  MYHEAD
#define MYHEAD ((headname==NULL) ? "UNKNOWN" : headname)

/*---------------------------------------------------------------------------*/
/*! Take the internal attributes and load the datablock struct up.
-----------------------------------------------------------------------------*/

int THD_datablock_from_atr( THD_datablock *dblk, char *dirname, char *headname )
{
   THD_diskptr       *dkptr ;
   ATR_int           *atr_rank , *atr_dimen , *atr_scene , *atr_btype ;
   ATR_float         *atr_flt ;
   ATR_string        *atr_labs ;
   int ii , view_type , func_type , dset_type , nx,ny,nz,nvox , nvals , ibr,typ ;
   Boolean ok ;
   char prefix[THD_MAX_NAME]="Unknown" ;
   MRI_IMAGE *qim ;
   int brick_ccode ;

ENTRY("THD_datablock_from_atr") ;

   if( dblk == NULL || dblk->natr <= 0 ) RETURN(0) ; /* bad input */

   dkptr = dblk->diskptr ;

   /*-- get relevant attributes: rank, dimensions, view_type & func_type --*/

   atr_rank  = THD_find_int_atr( dblk , ATRNAME_DATASET_RANK ) ;
   atr_dimen = THD_find_int_atr( dblk , ATRNAME_DATASET_DIMENSIONS ) ;
   atr_scene = THD_find_int_atr( dblk , ATRNAME_SCENE_TYPE ) ;

   /*-- missing an attribute ==> quit now --*/

   if( atr_rank == NULL || atr_dimen == NULL || atr_scene == NULL ) RETURN(0) ;

   /*-- load type codes from SCENE attribute --*/

   STATUS("loading *_type from SCENE") ;

   view_type = atr_scene->in[0] ;
   func_type = atr_scene->in[1] ;
   dset_type = atr_scene->in[2] ;

   /*-- load other values from attributes into relevant places --*/

   ok   = True ;
   nvox = 1 ;

   STATUS("loading from RANK") ;

   dkptr->rank = atr_rank->in[0] ;                /* N.B.: rank isn't used much */
   dkptr->nvals = dblk->nvals = nvals = atr_rank->in[1] ;  /* but nvals is used */

   STATUS("loading from DIMENSIONS") ;

   for( ii=0 ; ii < dkptr->rank ; ii++ ){
     dkptr->dimsizes[ii] = atr_dimen->in[ii] ;
     ok                  = ( ok && dkptr->dimsizes[ii] >= 1 ) ;
     nvox               *= dkptr->dimsizes[ii] ;
   }

#if 0
   if( PRINT_TRACING ){
     char str[256] ;
     sprintf(str,"rank=%d nvals=%d dim[0]=%d dim[1]=%d dim[2]=%d nvox=%d",
             dkptr->rank , dkptr->nvals ,
             dkptr->dimsizes[0] , dkptr->dimsizes[1] , dkptr->dimsizes[2] , nvox ) ;
     STATUS(str) ;
   }
#endif

   if( !ok || nvals < 1 ||
       dkptr->rank < THD_MIN_RANK || dkptr->rank > THD_MAX_RANK ){
     STATUS("bad rank!!??") ;
     RETURN(0) ;
   }

   /*-- create the storage filenames --*/

   STATUS("creating storage filenames") ;

   if( headname != NULL && strchr(headname,'+') != NULL ){
     FILENAME_TO_PREFIX(headname,prefix) ;
     THD_init_diskptr_names( dkptr, dirname,NULL,prefix , view_type , True ) ;
   } else {
     if( headname != NULL ) MCW_strncpy(prefix,headname,THD_MAX_NAME) ;
     THD_init_diskptr_names( dkptr, dirname,NULL,prefix , view_type , True ) ;
   }

   /*-- determine if the BRIK file exists --*/

   STATUS("checking if .BRIK file exists") ;

   brick_ccode = COMPRESS_filecode(dkptr->brick_name) ;
   if( brick_ccode != COMPRESS_NOFILE )
     dkptr->storage_mode = STORAGE_BY_BRICK ;  /* a .BRIK file */

   /*-- if VOLUME_FILENAMES attribute exists, make it so [20 Jun 2002] --*/

   if( headname != NULL && dkptr->storage_mode == STORAGE_UNDEFINED ){
     atr_labs = THD_find_string_atr(dblk,"VOLUME_FILENAMES") ;
     if( atr_labs != NULL ){
       dkptr->storage_mode = STORAGE_BY_VOLUMES ;
       dblk->malloc_type   = DATABLOCK_MEM_MALLOC ;
     }
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

   if( !THD_datum_constant(dblk) ){ /* 15 Sep 2004 */
     fprintf(stderr,
             "\n** WARNING: File %s has mixed-type sub-bricks. ", MYHEAD ) ;
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
               ATRNAME_BYTEORDER , MYHEAD ) ;

   } else if( !no_ordwarn                         &&
              DBLK_BRICK_TYPE(dblk,0) != MRI_byte &&
              dblk->diskptr->storage_mode == STORAGE_BY_BRICK ){ /* 20 Sep 1999 */

     static int first=1 ;
     if( first ){
       fprintf(stderr,
         "\n*** The situation below can be rectified with program '3drefit -byteorder':\n");
       first = 0 ;
     }
     fprintf(stderr," ** Dataset %s: assuming byteorder %s\n",
             MYHEAD , BYTE_ORDER_STRING(dkptr->byte_order)  ) ;
   }

   /* if the data is not on disk, the flag remains at DATABLOCK_MEM_UNDEFINED,
      otherwise the flag says how the memory for the bricks is to be created. */

   if( dkptr->storage_mode == STORAGE_BY_BRICK ){
#if MMAP_THRESHOLD > 0
     dblk->malloc_type = (dblk->total_bytes > MMAP_THRESHOLD)
                         ? DATABLOCK_MEM_MMAP : DATABLOCK_MEM_MALLOC ;
     DBLK_mmapfix(dblk) ;  /* 18 Mar 2005 */
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
         if( ngood > 32 ) ngood = 32 ;      /* 02 Sep 2004 */
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

#if 0
   if( PRINT_TRACING ){
     char str[256] ;
     sprintf(str,"rank=%d nvals=%d dim[0]=%d dim[1]=%d dim[2]=%d",
             dkptr->rank , dkptr->nvals ,
             dkptr->dimsizes[0] , dkptr->dimsizes[1] , dkptr->dimsizes[2] ) ;
     STATUS(str) ;
   }
#endif

   RETURN(1) ;
}

/*---------------------------------------------------------------------------*/
/* Macros to fetch an attribute named nnn and test if it exists. */

#define ATR_IS_STR(nnn) ( (atr_str = THD_find_string_atr(blk,nnn)) != NULL )
#define ATR_IS_FLT(nnn) ( (atr_flt = THD_find_float_atr (blk,nnn)) != NULL )
#define ATR_IS_INT(nnn) ( (atr_int = THD_find_int_atr   (blk,nnn)) != NULL )

/*---------------------------------------------------------------------------*/
/*! Apply attributes to modify an existing datablock.
    Only some attributes have an effect.
    09 May 2005 -- written to support NIfTI-ization, by allowing
                   attributes to be applied AFTER a dataset is created.
-----------------------------------------------------------------------------*/

void THD_datablock_apply_atr( THD_3dim_dataset *dset )
{
   THD_datablock     *blk ;
   THD_diskptr       *dkptr ;
   THD_dataxes       *daxes ;
   ATR_int           *atr_int = NULL ;
   ATR_float         *atr_flt = NULL;
   ATR_string        *atr_str = NULL ;
   int ii , view_type , func_type , dset_type , nx,ny,nz,nvox , nvals , ibr,typ ;
   Boolean ok ;
   char prefix[THD_MAX_NAME]="Unknown" ;
   MRI_IMAGE *qim ;
   int brick_ccode ;

ENTRY("THD_datablock_apply_atr") ;

   if( !ISVALID_DSET(dset) ) EXRETURN ; /* bad input */

   blk   = dset->dblk   ;  if( blk == NULL   ) EXRETURN ;
   nvals = blk->nvals   ;  if( nvals <= 0    ) EXRETURN ;
   daxes = dset->daxes  ;  if( daxes == NULL ) EXRETURN ;
   dkptr = blk->diskptr ;

   /*-- brick labels --*/

   if( ATR_IS_STR(ATRNAME_BRICK_LABS) ){
     int ipos = -1 , ipold , ngood ;

     STATUS("brick labels") ;
     if( blk->brick_lab == NULL ) THD_init_datablock_labels( blk ) ;

     for( ibr=0 ; ibr < nvals ; ibr++ ){  /* loop over bricks */

       for( ipold = ipos++ ;                                   /* skip to */
            ipos < atr_str->nch && atr_str->ch[ipos] != '\0' ; /* next \0 */
            ipos++ ) /* nada */ ;                              /* or end. */

       ngood = ipos - ipold - 1 ;                 /* number of good chars */
       if( ngood > 0 ){
         XtFree(blk->brick_lab[ibr]) ;
         if( ngood > 32 ) ngood = 32 ;
         blk->brick_lab[ibr] = (char *) XtMalloc(sizeof(char)*(ngood+2)) ;
         memcpy( blk->brick_lab[ibr] , atr_str->ch+(ipold+1) , ngood ) ;
         blk->brick_lab[ibr][ngood] = '\0' ;
       }

        if( ipos >= atr_str->nch ) break ;  /* nothing more to do */
     } /* end of loop over sub-bricks */
   }

   /*-- keywords for the dataset itself --*/

   if( ATR_IS_STR(ATRNAME_KEYWORDS) ){
     STATUS("dataset keywords") ;
     dset->keywords = XtNewString( atr_str->ch ) ;
   }

   /*-- keywords for sub-bricks --*/

   if( ATR_IS_STR(ATRNAME_BRICK_KEYWORDS) ){
     int ipos = -1 , ipold , ngood ;

     STATUS("brick keywords") ;
     if( blk->brick_keywords == NULL ) THD_init_datablock_keywords( blk ) ;

     for( ibr=0 ; ibr < nvals ; ibr++ ){  /* loop over bricks */

       for( ipold = ipos++ ;                                   /* skip to */
            ipos < atr_str->nch && atr_str->ch[ipos] != '\0' ; /* next \0 */
            ipos++ ) /* nada */ ;                              /* or end. */

       ngood = ipos - ipold - 1 ;                 /* number of good chars */
       if( ngood > 0 ){
         XtFree(blk->brick_keywords[ibr]) ;
         blk->brick_keywords[ibr] = (char *) XtMalloc(sizeof(char)*(ngood+2)) ;
         memcpy( blk->brick_keywords[ibr] , atr_str->ch+(ipold+1) , ngood ) ;
         blk->brick_keywords[ibr][ngood] = '\0' ;
       }

       if( ipos >= atr_str->nch ) break ;  /* nothing more to do */
     } /* end of loop over sub-bricks */
   }

   /*-- auxiliary statistics stuff for each brick --*/

   if( ATR_IS_FLT(ATRNAME_BRICK_STATAUX) ){
     int ipos=0 , iv,nv,jv ;

     STATUS("brick stataux") ;

     /* attribute stores all stataux stuff as follows:
          sub-brick-index  statcode  no.-of-values value ... value
          sub-brick-index  statcode  no.-of-values value ... value, etc. */

     while( ipos <= atr_flt->nfl - 3 ){
       iv = (int) ( atr_flt->fl[ipos++] ) ;  /* which sub-brick */
       jv = (int) ( atr_flt->fl[ipos++] ) ;  /* statcode */
       nv = (int) ( atr_flt->fl[ipos++] ) ;  /* # of values that follow */

       if( nv > atr_flt->nfl - ipos ) nv = atr_flt->nfl - ipos ;

       THD_store_datablock_stataux( blk , iv , jv , nv , atr_flt->fl + ipos ) ;
       ipos += nv ;
     }
   }

   /*-- ID codes --*/

   if( ATR_IS_STR(ATRNAME_IDSTRING) )
     MCW_strncpy( dset->idcode.str , atr_str->ch , MCW_IDSIZE ) ;

   if( ATR_IS_STR(ATRNAME_IDDATE) )
     MCW_strncpy( dset->idcode.date , atr_str->ch , MCW_IDDATE ) ;

   if( ATR_IS_STR(ATRNAME_IDANATPAR) )
     MCW_strncpy( dset->anat_parent_idcode.str , atr_str->ch , MCW_IDSIZE ) ;

   if( ATR_IS_STR(ATRNAME_IDWARPPAR) )
     MCW_strncpy( dset->warp_parent_idcode.str , atr_str->ch , MCW_IDSIZE ) ;

   /*-- parent names --*/

   if( ATR_IS_STR(ATRNAME_ANATOMY_PARENT) &&
       ISZERO_IDCODE(dset->anat_parent_idcode) )
     MCW_strncpy( dset->anat_parent_name , atr_str->ch , THD_MAX_NAME ) ;

   if( ATR_IS_STR(ATRNAME_WARP_PARENT) &&
       ISZERO_IDCODE(dset->warp_parent_idcode) )
     MCW_strncpy( dset->warp_parent_name , atr_str->ch , THD_MAX_NAME ) ;

   if( ATR_IS_STR(ATRNAME_DATANAME) )
     MCW_strncpy( dset->self_name , atr_str->ch , THD_MAX_NAME ) ;

   /*-- markers --*/

   if( ATR_IS_FLT(ATRNAME_MARKSXYZ) && ATR_IS_STR(ATRNAME_MARKSLAB) ){
     int im , llen ;
     THD_ivec3 iv ;
     float xxdown,xxup , yydown,yyup , zzdown,zzup ;

     STATUS("markers") ;

     if( dset->markers == NULL ){
       dset->markers = myXtNew( THD_marker_set ) ;  /* new set */
       ADDTO_KILL(dset->kl , dset->markers) ;
     }

     COPY_INTO_STRUCT( *(dset->markers) ,  /* actual struct */
                       MARKS_FSTART ,      /* byte offset */
                       float ,             /* type being copied */
                       atr_flt->fl ,       /* start of source */
                       MARKS_FSIZE  ) ;    /* number of floats */

     COPY_INTO_STRUCT( *(dset->markers) ,
                       MARKS_LSTART ,
                       char ,
                       atr_str->ch ,
                       MARKS_LSIZE  ) ;

     xxdown = daxes->xxmin ; xxup = daxes->xxmax ;
     yydown = daxes->yymin ; yyup = daxes->yymax ;
     zzdown = daxes->zzmin ; zzup = daxes->zzmax ;

     dset->markers->numdef = dset->markers->numset = 0 ;

     for( im=0 ; im < MARKS_MAXNUM ; im++ ){
       llen = strlen( &(dset->markers->label[im][0]) ) ;
       dset->markers->valid[im]   =
          (llen > 0) &&
          ( dset->markers->xyz[im][0] >= xxdown ) &&
          ( dset->markers->xyz[im][0] <= xxup   ) &&
          ( dset->markers->xyz[im][1] >= yydown ) &&
          ( dset->markers->xyz[im][1] <= yyup   ) &&
          ( dset->markers->xyz[im][2] >= zzdown ) &&
          ( dset->markers->xyz[im][2] <= zzup   )    ;

       if( dset->markers->valid[im] ) (dset->markers->numset)++ ;

       if( llen > 0 ) (dset->markers->numdef)++ ;

       dset->markers->ovcolor[im] = -1 ;  /* default color */
     }

     if( ATR_IS_STR(ATRNAME_MARKSHELP) ){
       COPY_INTO_STRUCT( *(dset->markers) ,
                          MARKS_HSTART ,
                          char ,
                          atr_str->ch ,
                          MARKS_HSIZE  ) ;
     } else {
       for( im=0 ; im < MARKS_MAXNUM ; im++ )   /* no help */
         dset->markers->help[im][0] = '\0' ;
     }

     if( ATR_IS_INT(ATRNAME_MARKSFLAG) ){
       COPY_INTO_STRUCT( *(dset->markers) ,
                         MARKS_ASTART ,
                         int ,
                         atr_int->in ,
                         MARKS_ASIZE  ) ;
       dset->markers->type = dset->markers->aflags[0] ;
     } else {
       for( im=0 ; im < MARKS_MAXFLAG ; im++ )
         dset->markers->aflags[im] = -1 ;
     }
   }

   /*-- warp --*/

   if( ATR_IS_INT(ATRNAME_WARP_TYPE) && ATR_IS_FLT(ATRNAME_WARP_DATA) ){
     int wtype = atr_int->in[0] , rtype = atr_int->in[1]  ;

     STATUS("warp") ;

     dset->warp = myXtNew( THD_warp ) ;
     ADDTO_KILL( dset->kl , dset->warp ) ;
     switch( wtype ){
       case WARP_AFFINE_TYPE:{
         THD_affine_warp *ww = (THD_affine_warp *) dset->warp ;
         ww->type       = wtype ;
         ww->resam_type = rtype ;
         ww->warp.type  = MAPPING_LINEAR_TYPE ;

         COPY_INTO_STRUCT( ww->warp ,
                           MAPPING_LINEAR_FSTART ,
                           float ,
                           atr_flt->fl ,
                           MAPPING_LINEAR_FSIZE ) ;
       }
       break ;  /* end affine warp */

       case WARP_TALAIRACH_12_TYPE:{
         THD_talairach_12_warp *ww =
            (THD_talairach_12_warp *) dset->warp ;
         int iw , ioff ;
         ww->type       = wtype ;
         ww->resam_type = rtype ;
         for( iw=0 ; iw < 12 ; iw++ ){
            ww->warp[iw].type = MAPPING_LINEAR_TYPE ;

            ioff = iw * MAPPING_LINEAR_FSIZE ;

            COPY_INTO_STRUCT( ww->warp[iw] ,
                              MAPPING_LINEAR_FSTART ,
                              float ,
                              &(atr_flt->fl[ioff]) ,
                              MAPPING_LINEAR_FSIZE ) ;

         }  /* end loop over 12 warps */
       }
       break ;  /* end talairach_12 warp */

     } /* end of switch on warp type */
   }

   /*-- brick stats --*/

   if( ATR_IS_FLT(ATRNAME_BRICK_STATS) ){
     int qq ;
     STATUS("brick statistics") ;
     dset->stats         = myXtNew( THD_statistics ) ;
     dset->stats->type   = STATISTICS_TYPE ;
     dset->stats->parent = (XtPointer) dset ;
     dset->stats->nbstat = blk->nvals ;
     dset->stats->bstat  = (THD_brick_stats *)
                              XtMalloc( sizeof(THD_brick_stats) * blk->nvals ) ;
     for( qq=0 ; qq < blk->nvals ; qq++ ){
       if( 2*qq+1 < atr_flt->nfl ){
           dset->stats->bstat[qq].min = atr_flt->fl[2*qq] ;
           dset->stats->bstat[qq].max = atr_flt->fl[2*qq+1] ;
       } else {
           INVALIDATE_BSTAT( dset->stats->bstat[qq] ) ;
       }
     }
     ADDTO_KILL( dset->kl , dset->stats->bstat ) ;
     ADDTO_KILL( dset->kl , dset->stats ) ;
   }

   /*-- tagset --*/

   if( ATR_IS_INT(ATRNAME_TAGSET_NUM)    &&
       ATR_IS_FLT(ATRNAME_TAGSET_FLOATS) &&
       ATR_IS_STR(ATRNAME_TAGSET_LABELS)    ){

     int nin=atr_int->nin , nfl=atr_flt->nfl , nch=atr_str->nch ;
     int ii , ntag , nfper , jj , kk ;

     STATUS("tagset") ;
     ntag  = atr_int->in[0] ;  /* number of tags */
     nfper = atr_int->in[1] ;  /* number of floats per tag */

     if( ntag > MAX_TAG_NUM ) ntag = MAX_TAG_NUM ;

     dset->tagset = myXtNew( THD_usertaglist ) ;  /* create tagset */
     ADDTO_KILL( dset->kl , dset->tagset ) ;

     dset->tagset->num = ntag ;
     strcpy( dset->tagset->label , "Bebe Rebozo" ) ;  /* not used */

     /* read out tag values; allow for chance there isn't enough data */

#undef  TF
#define TF(i,j) \
  ( ((j)<nfper && (i)*nfper+(j)<nfl) ? atr_flt->fl[(i)*nfper+(j)] : -666.0 )

     for( ii=0 ; ii < ntag ; ii++ ){
       dset->tagset->tag[ii].x   = TF(ii,0) ; /* coords */
       dset->tagset->tag[ii].y   = TF(ii,1) ;
       dset->tagset->tag[ii].z   = TF(ii,2) ;
       dset->tagset->tag[ii].val = TF(ii,3) ; /* value */
       dset->tagset->tag[ii].ti  = TF(ii,4) ; /* time index: if < 0, not set */
       if( dset->tagset->tag[ii].ti >= 0 ){
         dset->tagset->tag[ii].set = 1 ;
       } else {
         dset->tagset->tag[ii].set = 0 ; dset->tagset->tag[ii].ti = 0 ;
       }
     }
#undef TF

     /* read out tag labels; allow for empty labels */

     jj = 0 ;
     for( ii=0 ; ii < ntag ; ii++ ){
       if( jj < nch ){
         kk = strlen( atr_str->ch + jj ) ;
         if( kk > 0 ) TAG_SETLABEL( dset->tagset->tag[ii] , atr_str->ch + jj );
         else         sprintf( dset->tagset->tag[ii].label , "Tag %d" , ii+1 );
         jj += kk+1 ;
       } else {
         sprintf( dset->tagset->tag[ii].label , "Tag %d" , ii+1 ) ;
       }
     }
   }

   EXRETURN ;
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

void THD_init_datablock_brick( THD_datablock *dblk, int ntype, void *btype )
{
   int ibr , nx,ny,nz , typ , nvals ;
   MRI_IMAGE *qim ;
   THD_datablock *pblk = NULL ;
   int *itype = NULL ;

ENTRY("THD_init_datablock_brick") ;

   if( ! ISVALID_DATABLOCK(dblk)   ) EXRETURN ;   /* bad inputs */
   if( ntype <  0 && btype == NULL ) EXRETURN ;
   if( ntype == 0 && btype != NULL ) EXRETURN ;

   if( ntype < 0 ){                            /* copy types from */
     pblk = (THD_datablock *) btype ;          /* datablock pblk  */
     if( ! ISVALID_DATABLOCK(pblk) ) EXRETURN ;
   } else {
     itype = (int *) btype ;
   }

   nx    = dblk->diskptr->dimsizes[0] ;
   ny    = dblk->diskptr->dimsizes[1] ;
   nz    = dblk->diskptr->dimsizes[2] ;
   nvals = dblk->nvals ; if( nvals < 1 ) EXRETURN ; /* something wrong */

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
   EXRETURN ;
}

/*----------------------------------------------------------------
  Determine if the brick factors are needed.
------------------------------------------------------------------*/

int THD_need_brick_factor( THD_3dim_dataset * dset )
{
   int ii , nval ;

ENTRY("THD_need_brick_factor") ;

   if( ! ISVALID_DSET(dset)            ) RETURN( 0 ) ;
   if( ! ISVALID_DATABLOCK(dset->dblk) ) RETURN( 0 ) ;
   if( dset->dblk->brick_fac == NULL   ) RETURN( 0 ) ;

   nval = DSET_NVALS(dset) ;
   for( ii=0 ; ii < nval ; ii++ )
      if( DSET_BRICK_FACTOR(dset,ii) != 0.0 &&
          DSET_BRICK_FACTOR(dset,ii) != 1.0   ) RETURN( 1 ) ;

   RETURN( 0 ) ;
}
