/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"
#include "thd_niftiwrite.h"

static int use_3D_format    = 0 ;  /* 21 Mar 2003 */
static int use_NIFTI_format = 0 ;  /* 06 Apr 2005 */

void THD_use_3D_format   ( int uu ){ use_3D_format    = uu; }
void THD_use_NIFTI_format( int uu ){ use_NIFTI_format = uu; }

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
   int ii ;
   char *ppp ;  /* 06 Apr 2005 */

ENTRY("THD_write_3dim_dataset") ;

   /*-- sanity checks --*/

   if( ! ISVALID_3DIM_DATASET(dset)    ||
       ! ISVALID_DATABLOCK(dset->dblk) ||
       ! ISVALID_DISKPTR(dset->dblk->diskptr) ) RETURN(False) ;

   blk = dset->dblk ;
   blk->parent = (XtPointer)dset ;  /* 05 Jul 2005 */

   /* Can only write AFNI formatted datasets */

   if( DSET_IS_MINC(dset)     ) RETURN(False) ;  /* 29 Oct 2001 */
   if( DSET_IS_MASTERED(dset) ) RETURN(False) ;  /* 11 Jan 1999 */
   if( DSET_IS_ANALYZE(dset)  ) RETURN(False) ;  /* 27 Aug 2002 */
   if( DSET_IS_CTFMRI(dset)   ) RETURN(False) ;  /* 05 Dec 2002 */
   if( DSET_IS_CTFSAM(dset)   ) RETURN(False) ;  /* 05 Dec 2002 */
   if( DSET_IS_TCAT(dset)     ) RETURN(False) ;  /* 05 Aug 2004 */

   if( DSET_IS_VOLUMES(dset) && write_brick ) RETURN(False) ;  /* 20 Jun 2002 */

   if( DSET_IS_1D(dset) ||
       ( DSET_NY(dset)==1 && DSET_NZ(dset)==1 ) ){             /* 04 Mar 2003 */

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

   THD_set_dataset_attributes( dset ) ;

   /*------ 06 Apr 2005: write a NIFTI-1 dataset??? -----*/
   /*       11 Oct 2005: allow .hdr suffix also          */

   ppp = DSET_PREFIX(dset) ;
   if( STRING_HAS_SUFFIX(ppp,".nii")    ||
       STRING_HAS_SUFFIX(ppp,".nii.gz") ||
       STRING_HAS_SUFFIX(ppp,".hdr")    || use_NIFTI_format ){

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
     if( AFNI_yesenv("AFNI_AUTOGZIP")                  &&
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
     RETURN( (Boolean)ii ) ;
   }

   /*------ 21 Mar 2003: use the .3D format? -----*/

   if( STRING_HAS_SUFFIX(ppp,".3D") || DSET_IS_3D(dset) || use_3D_format ){
     if( !write_brick ){
       fprintf(stderr,
               "** ERROR: can't write HEADER only for .3D file: %s\n",
               DSET_PREFIX(dset) ) ;
       RETURN(False) ;
     }
     THD_write_3D( NULL, NULL, dset ) ; RETURN(True) ;
   }

   /*------ 12 Jun 2006: use the .niml format -----*/

   if( STRING_HAS_SUFFIX(ppp,".niml") || DSET_IS_NIML(dset) ){
     THD_write_niml( dset, write_brick ) ; RETURN(True) ;
   }

   /*------ 28 Jun 2006: use the .niml.dset format -----*/

   if( STRING_HAS_SUFFIX(ppp,".niml.dset") || DSET_IS_NI_SURF_DSET(dset) ){
     THD_write_niml( dset, write_brick ) ; RETURN(True) ;
   }

   /*----- write datablock to disk in AFNI .HEAD/.BRIK format -----*/

   RETURN( THD_write_datablock(blk,write_brick) ) ;
}
