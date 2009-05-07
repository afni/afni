/*****************************************************************************
Major portions of this software are copyrighted by the Medical College of
Wisconsin, 1994-2000, and are released under the Gnu General Public License,
Version 2.  See the file README.Copyright for details.
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

   dblk->vedim = NULL ; /* 05 Sep 2006 */

   dblk->brick_fdrcurve = NULL ; /* 23 Jan 2008 */
   dblk->brick_mdfcurve = NULL ; /* 22 Oct 2008 */

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
   int   ii , view_type , func_type , dset_type , 
         nx,ny,nz,nvox , nvals , ibr,typ ;
   Boolean ok ;
   char prefix[THD_MAX_NAME]="Unknown" ;
   MRI_IMAGE *qim ;
   int brick_ccode ;
   char name[666] ;

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

   if( !AFNI_yesenv("AFNI_IGNORE_BRICK_FLTFAC") ){
     atr_flt = THD_find_float_atr( dblk , ATRNAME_BRICK_FLTFAC ) ;
     if( atr_flt != NULL ){
       for( ibr=0 ; ibr < nvals && ibr < atr_flt->nfl ; ibr++ )
         dblk->brick_fac[ibr] = atr_flt->fl[ibr] ;
     }
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

   atr_labs = THD_find_string_atr( dblk , "BRICK_STATSYM" ) ;  /* 01 Jun 2005 */
   if( atr_labs != NULL && atr_labs->nch > 0 ){
     NI_str_array *sar ; int scode,np ; float parm[3] ;
     sar = NI_decode_string_list( atr_labs->ch , ";" ) ;
     if( sar != NULL && sar->num > 0 ){
       for( ibr=0 ; ibr < nvals && ibr < sar->num ; ibr++ ){
         NI_stat_decode( sar->str[ibr] , &scode , parm,parm+1,parm+2 ) ;
         if( scode >= AFNI_FIRST_STATCODE && scode <= AFNI_LAST_STATCODE ){
           np = NI_stat_numparam(scode) ;
           THD_store_datablock_stataux( dblk , ibr,scode,np,parm ) ;
         }
       }
       NI_delete_str_array(sar) ;
     }
   } else {          /*--- the olde way to get ye brick stataux parameters ---*/
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

   /*-- FDR curves [23 Jan 2008] --*/

   for( ibr=0 ; ibr < dblk->nvals ; ibr++ ){
     sprintf(name,"FDRCURVE_%06d",ibr) ;
     atr_flt = THD_find_float_atr( dblk , name ) ;
     if( atr_flt != NULL && atr_flt->nfl > 3 ){
       int nv = atr_flt->nfl - 2 ; floatvec *fv ;
       MAKE_floatvec(fv,nv) ;
       fv->x0 = atr_flt->fl[0] ; fv->dx = atr_flt->fl[1] ;
       memcpy( fv->ar , atr_flt->fl + 2 , sizeof(float)*nv ) ;
       if( dblk->brick_fdrcurve == NULL )
         dblk->brick_fdrcurve = (floatvec **)calloc(sizeof(floatvec *),dblk->nvals);
       dblk->brick_fdrcurve[ibr] = fv ;
     }
   }

   for( ibr=0 ; ibr < dblk->nvals ; ibr++ ){
     sprintf(name,"MDFCURVE_%06d",ibr) ;
     atr_flt = THD_find_float_atr( dblk , name ) ;
     if( atr_flt != NULL && atr_flt->nfl > 3 ){
       int nv = atr_flt->nfl - 2 ; floatvec *fv ;
       MAKE_floatvec(fv,nv) ;
       fv->x0 = atr_flt->fl[0] ; fv->dx = atr_flt->fl[1] ;
       memcpy( fv->ar , atr_flt->fl + 2 , sizeof(float)*nv ) ;
       if( dblk->brick_mdfcurve == NULL )
         dblk->brick_mdfcurve = (floatvec **)calloc(sizeof(floatvec *),dblk->nvals);
       dblk->brick_mdfcurve[ibr] = fv ;
     }
   }

   RETURN(1) ;
}

#if 0
/* update daxes structure in dataset header from datablock attributes */
int THD_daxes_from_atr( THD_datablock *dblk, THD_dataxes *daxes)
{
   ATR_int           *atr_rank , *atr_dimen , *atr_scene , *atr_btype ;
   ATR_float         *atr_flt ;
   ATR_string        *atr_labs ;
   int   ii , view_type , func_type , dset_type , 
         nx,ny,nz,nvox , nvals , ibr,typ ;
   Boolean ok ;
   char prefix[THD_MAX_NAME]="Unknown" ;
   MRI_IMAGE *qim ;
   int brick_ccode ;

ENTRY("THD_daxes_from_atr") ;

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

   RETURN(1) ;
}

#endif

/*-------------------------------------------------------------------*/
/*!  Given a 12 parameter affine transform created a la 3dWarpDrive's
WARPDRIVE_MATVEC_INV_000000, turn it into a WARP_DATA type array of 30
parameters. See matlab function MATVEC_to_WARP.m for more inspiration.

ZSS: June 06 
---------------------------------------------------------------------*/

int Matvec_2_WarpData(ATR_float  *atr_flo, THD_affine_warp *ww, float *wdv)
{
   int ans=0;
   mat44 Mfor, Mbac;
   int k;
   int dbg = 0;
    
   ENTRY("Matvec_2_WarpData") ;
   if (!atr_flo) {
      fprintf(stderr,"NULL atr_flo!\n");
      RETURN(ans);
   }
   if (atr_flo->nfl != 12) {
      fprintf(stderr,"atr_flo->nfl != 12\n");
      RETURN(ans);
   }
   
   if (!ww) {
      fprintf(stderr,"NULL ww\n");
      RETURN(ans);
   }     
   
   ww->type       = WARP_AFFINE_TYPE;
   ww->resam_type = 0 ;   /* not used */
   ww->warp.type  = MAPPING_LINEAR_TYPE ;

   k = 0;
   Mfor.m[0][0] = atr_flo->fl[k]; ++k;
   Mfor.m[0][1] = atr_flo->fl[k]; ++k;
   Mfor.m[0][2] = atr_flo->fl[k]; ++k;
   Mfor.m[0][3] = atr_flo->fl[k]; ++k;
   Mfor.m[1][0] = atr_flo->fl[k]; ++k;
   Mfor.m[1][1] = atr_flo->fl[k]; ++k;
   Mfor.m[1][2] = atr_flo->fl[k]; ++k;
   Mfor.m[1][3] = atr_flo->fl[k]; ++k;
   Mfor.m[2][0] = atr_flo->fl[k]; ++k;
   Mfor.m[2][1] = atr_flo->fl[k]; ++k;
   Mfor.m[2][2] = atr_flo->fl[k]; ++k;
   Mfor.m[2][3] = atr_flo->fl[k]; ++k;
   Mfor.m[3][0] = 0.0;
   Mfor.m[3][1] = 0.0;
   Mfor.m[3][2] = 0.0;
   Mfor.m[3][3] = 0.0;
   if (dbg) { 
      DUMP_MAT44("Mfor:\n",Mfor);
   }
   
   /* calculate the backward transform */
   Mbac = nifti_mat44_inverse(Mfor);
   if (dbg) {
      DUMP_MAT44("Mbac:\n",Mbac);                              
   }

   if (wdv) {
      /* Load the forward transform */
      /* Now load the 30 values of Wd */
      k=0;
      wdv[k] = Mfor.m[0][0]; ++k;
      wdv[k] = Mfor.m[0][1]; ++k;
      wdv[k] = Mfor.m[0][2]; ++k;
      wdv[k] = Mfor.m[1][0]; ++k;
      wdv[k] = Mfor.m[1][1]; ++k;
      wdv[k] = Mfor.m[1][2]; ++k;
      wdv[k] = Mfor.m[2][0]; ++k;
      wdv[k] = Mfor.m[2][1]; ++k;
      wdv[k] = Mfor.m[2][2]; ++k;

      wdv[k] = Mbac.m[0][0]; ++k;
      wdv[k] = Mbac.m[0][1]; ++k;
      wdv[k] = Mbac.m[0][2]; ++k;
      wdv[k] = Mbac.m[1][0]; ++k;
      wdv[k] = Mbac.m[1][1]; ++k;
      wdv[k] = Mbac.m[1][2]; ++k;
      wdv[k] = Mbac.m[2][0]; ++k;
      wdv[k] = Mbac.m[2][1]; ++k;
      wdv[k] = Mbac.m[2][2]; ++k;

      wdv[k] = -Mfor.m[0][3]; ++k;
      wdv[k] = -Mfor.m[1][3]; ++k;
      wdv[k] = -Mfor.m[2][3]; ++k;

      wdv[k] = -Mbac.m[0][3]; ++k;
      wdv[k] = -Mbac.m[1][3]; ++k;
      wdv[k] = -Mbac.m[2][3]; ++k;

      /* bot and top are filled as to not cause trouble for Talairach bounding box at a minimum from: 
         [-80 ; -80 ; -65] to [80 ; 110 ; 85] */
      wdv[k] = -80 * 2; ++k; /* x 2 ? be generous, it is free! */
      wdv[k] = -80 * 2; ++k; 
      wdv[k] = -65 * 2; ++k; 

      wdv[k] =  80 * 2; ++k; 
      wdv[k] = 110 * 2; ++k; 
      wdv[k] =  85 * 2; ++k; 
   }
   
   /* Now load the 30 values into warp */
   ww->warp.mfor.mat[0][0] = Mfor.m[0][0]; 
   ww->warp.mfor.mat[0][1] = Mfor.m[0][1]; 
   ww->warp.mfor.mat[0][2] = Mfor.m[0][2]; 
   ww->warp.mfor.mat[1][0] = Mfor.m[1][0]; 
   ww->warp.mfor.mat[1][1] = Mfor.m[1][1]; 
   ww->warp.mfor.mat[1][2] = Mfor.m[1][2]; 
   ww->warp.mfor.mat[2][0] = Mfor.m[2][0]; 
   ww->warp.mfor.mat[2][1] = Mfor.m[2][1]; 
   ww->warp.mfor.mat[2][2] = Mfor.m[2][2]; 
   
   ww->warp.mbac.mat[0][0] = Mbac.m[0][0]; 
   ww->warp.mbac.mat[0][1] = Mbac.m[0][1]; 
   ww->warp.mbac.mat[0][2] = Mbac.m[0][2]; 
   ww->warp.mbac.mat[1][0] = Mbac.m[1][0]; 
   ww->warp.mbac.mat[1][1] = Mbac.m[1][1]; 
   ww->warp.mbac.mat[1][2] = Mbac.m[1][2]; 
   ww->warp.mbac.mat[2][0] = Mbac.m[2][0]; 
   ww->warp.mbac.mat[2][1] = Mbac.m[2][1]; 
   ww->warp.mbac.mat[2][2] = Mbac.m[2][2]; 
   
   ww->warp.bvec.xyz[0] = -Mfor.m[0][3]; 
   ww->warp.bvec.xyz[1] = -Mfor.m[1][3]; 
   ww->warp.bvec.xyz[2] = -Mfor.m[2][3]; 
   
   ww->warp.svec.xyz[0] = -Mbac.m[0][3]; 
   ww->warp.svec.xyz[1] = -Mbac.m[1][3]; 
   ww->warp.svec.xyz[2] = -Mbac.m[2][3]; 
   
   /* bot and top are filled as to not cause trouble for Talairach bounding box at a minimum from: 
      [-80 ; -80 ; -65] to [80 ; 110 ; 85] */
   ww->warp.bot.xyz[0] = -80 * 2;  /* x 2 ? be generous, it is free! */
   ww->warp.bot.xyz[1] = -80 * 2;  
   ww->warp.bot.xyz[2] = -65 * 2;  
   
   ww->warp.top.xyz[0] =  80 * 2;  
   ww->warp.top.xyz[1] = 110 * 2;  
   ww->warp.top.xyz[2] =  85 * 2;  

   RETURN(1);
} 

int THD_WarpData_From_3dWarpDrive(THD_3dim_dataset *dset, ATR_float *atr_flt)
{
   int dbg = 0;
   
   ENTRY("THD_WarpData_From_3dWarpDrive");

   if (!dset) {
      fprintf(stderr,"NULL dset!");
      RETURN(0);
   }
   if (dset->warp) {
      fprintf(stderr,"Warp already there!");
      RETURN(0);
   }
   if (!atr_flt) {
      fprintf(stderr,"No attribute!");
      RETURN(0);
   }
   if (atr_flt->nfl != 12) {
      fprintf( stderr,
               "Number of parameters in TLRC transform is not 12.\n"
               "I won't float your boat.\n");
      RETURN(0); 
   }
   dset->warp = myXtNew( THD_warp ) ;
   ADDTO_KILL( dset->kl , dset->warp ) ;
   {   
      THD_affine_warp *ww = (THD_affine_warp *) dset->warp ;
      if (!Matvec_2_WarpData(atr_flt, ww, NULL)) {
         fprintf(stderr,"Failed to create warp!");
         RETURN(0);
      } 
   }
   /* If you have a warp, you must have a warp_parent 
   However, previous versions of @auto_tlrc did not set
   that, so use some defaults */
   if( strlen(dset->warp_parent_name) <= 0 
         && ISZERO_IDCODE(dset->warp_parent_idcode)) {
      if (dbg) fprintf(stderr,"Assigning a dummy warp parent name\n");
      sprintf(dset->warp_parent_name,"Not_Set");
   }

   RETURN(1);
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
   char name[666] ;

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

   /*-- FDR curves [23 Jan 2008] --*/

   for( ibr=0 ; ibr < blk->nvals ; ibr++ ){
     sprintf(name,"FDRCURVE_%06d",ibr) ;
     atr_flt = THD_find_float_atr( blk , name ) ;
     if( atr_flt != NULL && atr_flt->nfl > 3 ){
       int nv = atr_flt->nfl - 2 ; floatvec *fv ;
       MAKE_floatvec(fv,nv) ;
       fv->x0 = atr_flt->fl[0] ; fv->dx = atr_flt->fl[1] ;
       memcpy( fv->ar , atr_flt->fl + 2 , sizeof(float)*nv ) ;
       if( blk->brick_fdrcurve == NULL )
         blk->brick_fdrcurve = (floatvec **)calloc(sizeof(floatvec *),blk->nvals);
       blk->brick_fdrcurve[ibr] = fv ;
     }
   }

   for( ibr=0 ; ibr < blk->nvals ; ibr++ ){
     sprintf(name,"MDFCURVE_%06d",ibr) ;
     atr_flt = THD_find_float_atr( blk , name ) ;
     if( atr_flt != NULL && atr_flt->nfl > 3 ){
       int nv = atr_flt->nfl - 2 ; floatvec *fv ;
       MAKE_floatvec(fv,nv) ;
       fv->x0 = atr_flt->fl[0] ; fv->dx = atr_flt->fl[1] ;
       memcpy( fv->ar , atr_flt->fl + 2 , sizeof(float)*nv ) ;
       if( blk->brick_mdfcurve == NULL )
         blk->brick_mdfcurve = (floatvec **)calloc(sizeof(floatvec *),blk->nvals);
       blk->brick_mdfcurve[ibr] = fv ;
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
   } /* end of if on warp existing */   else { /* But perhaps there is a little 
                                                something from auto talairaching                                                 ZSS, June 06 */
      if (dset->view_type == VIEW_TALAIRACH_TYPE) { /* something to do */
         int dbg = 0;
         atr_flt = THD_find_float_atr( blk , ATRNAME_WARP_DATA_3DWD_AF ) ; 
         if ( atr_flt == NULL ){
            /* A tlrc set with no transform. No problem */
            /* fprintf(stderr,"Dude, where's my transform?\n");  */
         } else {
            STATUS("AutoTlrc Warp") ;
            if (dbg) 
               fprintf(stderr,
                        "++ Will be using %s attribute for talairach warp in"
                        " dset %s\n",
                                    ATRNAME_WARP_DATA_3DWD_AF, dset->self_name) ;
            if (!THD_WarpData_From_3dWarpDrive(dset, atr_flt)) {
               fprintf(stderr,"Error: Failed to create WarpData!\n");
            }
         }
      } else {
         /* fprintf(stderr,"Not in TLRC space, bother not.\n"); */
      }
   } /* the very end of if on warp existing */

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

   if(atr_flt = THD_find_float_atr( blk, "IJK_TO_DICOM_REAL" )){
      /* load oblique transformation matrix */
      if(atr_flt) {
        LOAD_MAT44(dset->daxes->ijk_to_dicom_real, \
            atr_flt->fl[0], atr_flt->fl[1], atr_flt->fl[2], atr_flt->fl[3], \
            atr_flt->fl[4], atr_flt->fl[5], atr_flt->fl[6], atr_flt->fl[7], \
            atr_flt->fl[8], atr_flt->fl[9], atr_flt->fl[10], atr_flt->fl[11]);
      }
   }

   /* update attributes for time axes - copied from thd_dsetdblk.c */
   /*--- read time-dependent information, if any ---*/
   atr_int = THD_find_int_atr  ( blk , ATRNAME_TAXIS_NUMS ) ;
   atr_flt = THD_find_float_atr( blk , ATRNAME_TAXIS_FLOATS ) ;
   if( atr_int != NULL && atr_flt != NULL ){
     int isfunc , nvals ;

     dset->taxis = myXtNew( THD_timeaxis ) ;

     dset->taxis->type    = TIMEAXIS_TYPE ;
     dset->taxis->ntt     = atr_int->in[0] ;
     dset->taxis->nsl     = atr_int->in[1] ;
     dset->taxis->ttorg   = atr_flt->fl[0] ;
     dset->taxis->ttdel   = atr_flt->fl[1] ;
     dset->taxis->ttdur   = atr_flt->fl[2] ;
     dset->taxis->zorg_sl = atr_flt->fl[3] ;
     dset->taxis->dz_sl   = atr_flt->fl[4] ;

     dset->taxis->units_type = atr_int->in[2] ;    /* 21 Oct 1996 */
     if( dset->taxis->units_type < 0 )             /* assign units */
       dset->taxis->units_type = UNITS_SEC_TYPE ;  /* to the time axis */

     if( dset->taxis->nsl > 0 ){
       atr_flt = THD_find_float_atr( blk , ATRNAME_TAXIS_OFFSETS ) ;
       if( atr_flt == NULL || atr_flt->nfl < dset->taxis->nsl ){
         dset->taxis->nsl     = 0 ;
         dset->taxis->toff_sl = NULL ;
         dset->taxis->zorg_sl = 0.0 ;
         dset->taxis->dz_sl   = 0.0 ;
       } else {
         int ii ;
         dset->taxis->toff_sl = (float *) XtMalloc(sizeof(float)*dset->taxis->nsl) ;
         for( ii=0 ; ii < dset->taxis->nsl ; ii++ )
           dset->taxis->toff_sl[ii] = atr_flt->fl[ii] ;
       }
     } else {
       dset->taxis->nsl     = 0 ;
       dset->taxis->toff_sl = NULL ;
       dset->taxis->zorg_sl = 0.0 ;
       dset->taxis->dz_sl   = 0.0 ;
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

/** ENTRY("THD_need_brick_factor") ; **/

   if( ! ISVALID_DSET(dset)            ) return( 0 ) ;
   if( ! ISVALID_DATABLOCK(dset->dblk) ) return( 0 ) ;
   if( dset->dblk->brick_fac == NULL   ) return( 0 ) ;

   nval = DSET_NVALS(dset) ;
   for( ii=0 ; ii < nval ; ii++ )
      if( DSET_BRICK_FACTOR(dset,ii) != 0.0 &&
          DSET_BRICK_FACTOR(dset,ii) != 1.0   ) return( 1 ) ;

   return( 0 ) ;
}
