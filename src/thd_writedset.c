/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"
#include "thd_niftiwrite.h"

static int error_count = 0 ;
int THD_get_write_error_count(void){ return error_count; }
void THD_reset_write_error_count(void){ error_count = 0; }

static int use_3D_format    = 0 ;  /* 21 Mar 2003 */
static int use_NIFTI_format = 0 ;  /* 06 Apr 2005 */
static int quiet_overwrite  = 0 ;  /* 31 Jan 2011 */
static int update_dset_atr_status = 1; /* 31 Oct 2011 */

void THD_use_3D_format   ( int uu ){ use_3D_format    = uu; }
void THD_use_NIFTI_format( int uu ){ use_NIFTI_format = uu; }
void THD_set_quiet_overwrite ( int uu ){ quiet_overwrite = uu; }
int THD_get_quiet_overwrite () { return(quiet_overwrite);}
/*----------------------------------------------------------------*/
/*! This routine writes all the header data in the struct to
    the datablock attributes, then writes the dataset to disk.

   29 Apr 1998: erase attributes that are unused, so that
                  they won't be left over from a previous life

   09 Mar 2005: attributes are now set in function
                THD_set_dataset_attributes() rather than here

   06 Apr 2005: might go all NIFTI on you
------------------------------------------------------------------*/

Boolean THD_write_3dim_dataset( char *new_sessname , char *new_prefixname ,
                                THD_3dim_dataset *dset , Boolean write_brick )
{
   THD_datablock *blk ;
   int ii, cmode ;
   int is_nsd = 0, is_gifti = 0 ;  /* is NI_SURF_DSET  03 Jul 2006 [rickr] */
   int free_1d = 0 ;               /* write 1D using prefix */
   char *ppp ;  /* 06 Apr 2005 */
   Boolean bb ;

ENTRY("THD_write_3dim_dataset") ;

   /*-- sanity checks --*/

   if( ! ISVALID_3DIM_DATASET(dset) ){
     ERROR_message("Cannot write dataset: it is invalid") ;
     error_count++ ; RETURN(False) ;
   }
   if( ! ISVALID_DATABLOCK(dset->dblk) ){
     ERROR_message("Cannot write dataset: invalid datablock") ;
     error_count++ ; RETURN(False) ;
   }
   if( ! ISVALID_DISKPTR(dset->dblk->diskptr) ){
     ERROR_message("Cannot write dataset: invalid diskptr") ;
     error_count++ ; RETURN(False) ;
   }

   if( new_prefixname ) ppp = new_prefixname;
   else                 ppp = DSET_PREFIX(dset);

   blk = dset->dblk ;
   blk->parent = (RwcPointer)dset ;  /* 05 Jul 2005 */

   /* Can only write AFNI formatted datasets */
   if( DSET_IS_MINC(dset)     ) {
      fprintf(stderr,"** cannot write '%s', dset is MINC\n",
              ppp ? ppp : "<NO PREFIX>");
      error_count++ ; RETURN(False) ;  /* 29 Oct 2001 */
   }
   if( DSET_IS_MASTERED(dset) ) {
      fprintf(stderr,"** cannot write '%s', dset is MASTERED (sub-bricks)\n",
              ppp ? ppp : "<NO PREFIX>");
      error_count++ ; RETURN(False) ;  /* 11 Jan 1999 */
   }
   if( DSET_IS_ANALYZE(dset)  ) {
      fprintf(stderr,"** cannot write '%s', dset is ANALYZE\n",
              ppp ? ppp : "<NO PREFIX>");
      error_count++ ; RETURN(False) ;  /* 27 Aug 2002 */
   }
   if( DSET_IS_CTFMRI(dset)   ) {
      fprintf(stderr,"** cannot write '%s', dset is CTFMRI\n",
              ppp ? ppp : "<NO PREFIX>");
      error_count++ ; RETURN(False) ;  /* 05 Dec 2002 */
   }
   if( DSET_IS_CTFSAM(dset)   ) {
      fprintf(stderr,"** cannot write '%s', dset is CTFSAM\n",
              ppp ? ppp : "<NO PREFIX>");
      error_count++ ; RETURN(False) ;  /* 05 Dec 2002 */
   }
   if( DSET_IS_TCAT(dset)     ) {
      fprintf(stderr,"** cannot write '%s', dset is TCAT\n",
              ppp ? ppp : "<NO PREFIX>");
      error_count++ ; RETURN(False) ;  /* 05 Aug 2004 */
   }

   if( DSET_IS_VOLUMES(dset) && write_brick ) {
      fprintf(stderr,"** cannot write '%s', dset is VOLUMES\n",
              ppp ? ppp : "<NO PREFIX>");
      error_count++ ; RETURN(False) ;  /* 20 Jun 2002 */
   }

   /* block NI_SURF_DSET from 1D write    11 Jul 2006 [rickr] */
   ppp = DSET_PREFIX(dset) ;
   is_gifti = DSET_IS_GIFTI(dset) || STRING_HAS_SUFFIX(ppp,".gii")
                                  || STRING_HAS_SUFFIX(ppp,".gii.dset") ;
   is_nsd = DSET_IS_NI_SURF_DSET(dset) || STRING_HAS_SUFFIX(ppp,".niml.dset") ||
            is_gifti ;

   /* might want to block trapping of 1D writes    20 Mar 2008 [rickr] */
   free_1d = is_nsd || AFNI_yesenv("AFNI_WRITE_1D_AS_PREFIX");

   if( DSET_IS_1D(dset) ||                 /* block NSD  03 Jul 2006 [rickr] */
       ( DSET_NY(dset)==1 && DSET_NZ(dset)==1 && !free_1d) ){ /* 04 Mar 2003 */

     THD_write_1D( new_sessname , new_prefixname , dset ) ;
     RETURN(True) ;
   }

   /*------------------------------*/
   /*-----  change filenames? -----*/

   THD_init_diskptr_names( blk->diskptr ,
                           new_sessname , NULL , new_prefixname ,
                           dset->view_type , True ) ;

   /* 15 Aug 2005 */

   if( !AFNI_yesenv("AFNI_ALLOW_MILLISECONDS") ){ DSET_UNMSEC(dset); }

   /*----- 09 Mar 2005: set attribute structs in the datablock -----*/
   if(THD_update_dset_atr_status()) {
      THD_set_dataset_attributes( dset ) ;
   }
   
   /* The TROSS logger */
   if ((ppp = my_getenv("AFNI_HISTDB_SCRIPT")) && ppp[0] != '\0') {
      THD_set_string_atr( dset->dblk , "HISTDB_SCRIPT" , ppp ) ;
   }
   
   /*----- 06 Jun 2007: deconflict dataset name? -----*/

   /* default is ERROR_exit */
   if( !THD_ok_overwrite() ){
     ppp = my_getenv("AFNI_DECONFLICT") ;
     if( ppp == NULL || toupper(*ppp) != 'O' ){
       char pfx[THD_MAX_PREFIX] ;
       MCW_strncpy( pfx , DSET_PREFIX(dset) , THD_MAX_PREFIX ) ;
       ii = THD_deconflict_prefix( dset ) ;
       if( ii ){
         if( ppp && toupper(*ppp) == 'Y' ){
           WARNING_message("changed output dataset name from '%s' to '%s'",
                           pfx , DSET_PREFIX(dset) ) ;
         } else {
           ERROR_message("output dataset name '%s' conflicts with existing file",pfx);
           ERROR_message("dataset NOT written to disk!") ;
           error_count++ ; RETURN(False) ;
         }
       }
     }
   } else if( THD_is_file(dset->dblk->diskptr->header_name) ||
              THD_is_file(dset->dblk->diskptr->brick_name)    ){
     if (!quiet_overwrite) {
      WARNING_message("Over-writing dataset %s",
                        dset->dblk->diskptr->header_name);
     }
   }

   /*------ 06 Apr 2005: write a NIFTI-1 dataset??? -----*/
   /*       11 Oct 2005: allow .hdr suffix also          */

   ppp = DSET_PREFIX(dset) ;
   if( STRING_HAS_SUFFIX(ppp,".nii")    ||
       STRING_HAS_SUFFIX(ppp,".nii.gz") ||
       STRING_HAS_SUFFIX(ppp,".hdr")    || 
       use_NIFTI_format                 ||
       dset->dblk->diskptr->storage_mode == STORAGE_BY_NIFTI){

     niftiwr_opts_t options ;

     ii = strlen(DSET_DIRNAME(dset)) + strlen(ppp) + 32 ;
     options.infile_name = calloc(1,ii) ;
     strcpy(options.infile_name,DSET_DIRNAME(dset)) ;
     strcat(options.infile_name,ppp) ;

     if( !STRING_HAS_SUFFIX(options.infile_name,".nii")    &&
         !STRING_HAS_SUFFIX(options.infile_name,".nii.gz") &&
         !STRING_HAS_SUFFIX(options.infile_name,".hdr")      ) /* 11 Oct 2005 */
       strcat(options.infile_name,".nii") ;

     /* allow user to order gzip-ed output via environment,
        OR complain if the file is ordered to be gzip-ed but can't be */


#ifdef HAVE_ZLIB                                            /* 21 Sep 2005 */
     cmode = THD_get_write_compression() ; /* check env. variable for compression*/

/*     AFNI_yesenv("AFNI_AUTOGZIP")    */ /* changed to compress base on AFNI_COMPRESSOR instead */
     if( (cmode==COMPRESS_GZIP) &&
         STRING_HAS_SUFFIX(options.infile_name,".nii")   )
       strcat(options.infile_name,".gz") ;
#else
     if( STRING_HAS_SUFFIX(options.infile_name,".nii.gz") ){
       WARNING_message("Can't write compressed file '%s'; writing '.nii' instead") ;
       ii = strlen(options.infile_name) ;
       options.infile_name[ii-3] = '\0' ;
     }
#endif

     {  /* set the nifti_io debug level       8 Apr 2005 [rickr] */
        char * ept = my_getenv("AFNI_NIFTI_DEBUG");
        if( ept != NULL ) options.debug_level = atoi(ept);
        else              options.debug_level = 0 ;
     }

     if( !write_brick ){
       ERROR_message("Can't write HEADER only for NIfTI-1 file: %s\n",
                     options.infile_name ) ;
       ii = 0 ;
     } else {
       ii = THD_write_nifti(dset,options) ;
       strcpy( dset->dblk->diskptr->brick_name , options.infile_name ) ;
     }

     free((void *)options.infile_name) ;
     if( ii==0 ) error_count++ ;
     RETURN( (Boolean)ii ) ;
   }

   /*------ 21 Mar 2003: use the .3D format? -----*/

   if( STRING_HAS_SUFFIX(ppp,".3D") || DSET_IS_3D(dset) || use_3D_format ){
     if( !write_brick ){
       fprintf(stderr,
               "** ERROR: can't write HEADER only for .3D file: %s\n",
               DSET_PREFIX(dset) ) ;
       error_count++ ; RETURN(False) ;
     }
     THD_write_3D( NULL, NULL, dset ) ; RETURN(True) ;
   }

   /*------ 12 Jun 2006: use the .niml format -----*/

   if( STRING_HAS_SUFFIX(ppp,".niml") || DSET_IS_NIML(dset) ){
     bb = THD_write_niml( dset, write_brick ) ;
     if( bb == False ) error_count++ ;
     RETURN(bb) ;
   }

   /*------ 28 Jun 2006: use the .niml.dset format -----*/

   /* if(STRING_HAS_SUFFIX(ppp,".niml.dset") || DSET_IS_NI_SURF_DSET(dset)) */
   if( is_nsd ){  /* already determined                 03 Jul 2006 [rickr] */
     if( is_gifti ) bb = THD_write_gifti( dset, write_brick, 0 ) ;
     else           bb = THD_write_niml( dset, write_brick ) ;
     if( bb == False ) error_count++ ;
     RETURN(bb) ;
   }

   /*----- write datablock to disk in AFNI .HEAD/.BRIK format -----*/

   bb = THD_write_datablock(blk,write_brick) ;
   if( bb == False ) error_count++ ;
   RETURN(bb) ;
}

/* accessor functions to control whether attributes will be reinitialized 
   from dset structure */
/* 3drefit turns this off, but all other calls continue to reinitialize
   attributes. Turning off this reset of the attribute is needed for writing
   a dataset where items that could get reset like IJK_TO_DICOM_REAL are
   rewritten using the cardinal transformation */
int
THD_update_dset_atr_status()
{
   return(update_dset_atr_status);
}

void
THD_set_dset_atr_status(int st)
{
   update_dset_atr_status = st;
}
