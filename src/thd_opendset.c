/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

extern THD_3dim_dataset *THD_3dim_from_ROIstring(char *shar);

#undef  CHECK_FOR_DATA     /* 06 Jan 2005: message about empty files */
#define CHECK_FOR_DATA(fn)                                                \
 do{ if( fsize <= 0 ){                                                    \
       if( fsize == 0 )                                                   \
         fprintf(stderr,"** Can't read ANY data from file '%s'\n",(fn));  \
       RETURN(NULL) ;                                                     \
     }} while(0)

/*-----------------------------------------------------------------
 * this is a list of known filename extensions
   (as found in THD_open_one_dataset())         28 Jun 2006 [rickr]
-------------------------------------------------------------------*/
static char * file_extension_list[] = {
    ".HEAD", ".BRIK", ".BRIK.gz",
    ".mnc",
    ".mri",
    ".svl",
    ".1D",
    ".3D",
    ".nii", ".nii.gz", ".nia", ".hdr", ".img",
    ".mpg", ".mpeg", ".MPG", ".MPEG",
    ".niml", ".niml.dset"
};


/*----------------------------------------------------------------
   simply given a pathname, try to open it as a dataset
   [allow for .HEAD, .BRIK, or just prefix+viewcode filenames]
------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_one_dataset( char *pathname )
{
   int ii , plen ;
   char dirname[THD_MAX_NAME] , fullname[THD_MAX_NAME] ;
   THD_3dim_dataset *dset ;
   THD_datablock    *dblk ;
   char *sub ;
   char *fname ;   /* to skip directory during HEAD/BRIK search in filename */
   int   offset ;  /*                                 - [rickr 20 Sep 2002] */
   int fsize ;     /* 06 Jan 2005 */

ENTRY("THD_open_one_dataset") ;

   /*-- sanity check --*/

   if( pathname == NULL              ||
       (plen=strlen(pathname)) == 0  ||
       pathname[plen-1]        == '/'  ) RETURN(NULL) ;

   /*-- perhaps open the new-fangled way [22 May 2000] --*/

   if( getenv("AFNI_USE_THD_open_dataset") != NULL &&
       strstr(pathname,"[")                != NULL   ){
      RETURN( THD_open_dataset(pathname) ) ;
   }

   fsize = THD_filesize(pathname) ;                         /* 06 Jan 2005 */
   if( fsize == 0 && !THD_is_file(pathname) ) fsize = -1 ;  /* 31 Mar 2005 */

   /*-- perhaps the MINC way --*/

   if( STRING_HAS_SUFFIX(pathname,".mnc") ){
     CHECK_FOR_DATA(pathname) ;               /* 06 Jan 2005 */
     RETURN( THD_open_minc(pathname) ) ;
   }

   /*-- perhaps the ANALYZE way --*/

   if( STRING_HAS_SUFFIX(pathname,".hdr") ){
     CHECK_FOR_DATA(pathname) ;               /* 06 Jan 2005 */
     RETURN( THD_open_analyze(pathname) ) ;
   }

   /*-- perhaps the CTF way [04 Dec 2002] --*/

   if( STRING_HAS_SUFFIX(pathname,".mri") ){
     CHECK_FOR_DATA(pathname) ;               /* 06 Jan 2005 */
     RETURN( THD_open_ctfmri(pathname) ) ;
   } else if( STRING_HAS_SUFFIX(pathname,".svl") ){
     CHECK_FOR_DATA(pathname) ;               /* 06 Jan 2005 */
     RETURN( THD_open_ctfsam(pathname) ) ;
   }

   /*-- 04 Mar 2003: allow input of .1D files --*/

   if( STRING_HAS_SUFFIX(pathname,".1D") ){
     CHECK_FOR_DATA(pathname) ;               /* 06 Jan 2005 */
     RETURN( THD_open_1D(pathname) ) ;
   } else if( STRING_HAS_SUFFIX(pathname,".3D") ){  /* 21 Mar 2003 */
     CHECK_FOR_DATA(pathname) ;               /* 06 Jan 2005 */
     RETURN( THD_open_3D(pathname) ) ;
   }

   /*-- 28 Aug 2003: the NIFTI way! --*/

   if( STRING_HAS_SUFFIX(pathname,".nii")    ||
       STRING_HAS_SUFFIX(pathname,".nii.gz") ||
       STRING_HAS_SUFFIX(pathname,".nia")      ){

     CHECK_FOR_DATA(pathname) ;               /* 06 Jan 2005 */
     RETURN( THD_open_nifti(pathname) ) ;
   }

   /*-- 03 Dec 2003: the MPEG way! --*/

   if( STRING_HAS_SUFFIX(pathname,".mpg")  ||
       STRING_HAS_SUFFIX(pathname,".MPG")  ||
       STRING_HAS_SUFFIX(pathname,".MPEG") ||
       STRING_HAS_SUFFIX(pathname,".mpeg")   ){

     CHECK_FOR_DATA(pathname) ;               /* 06 Jan 2005 */
     RETURN( THD_open_mpeg(pathname) ) ;
   }

   /*-- 26 May 2006 [rickr]: the NIML way! --*/

   if( STRING_HAS_SUFFIX(pathname,".niml") ){

     CHECK_FOR_DATA(pathname) ;
     RETURN( THD_open_niml(pathname) ) ;
   }

   /*-- 26 May 2006 [rickr]: the NI_SURF_DSET way! --*/

   if( STRING_HAS_SUFFIX(pathname,".niml.dset") ){

     CHECK_FOR_DATA(pathname) ;
     RETURN( THD_open_niml(pathname) ) ;
   }

   /* -- Try to read an AFNI dataset and if that fails, 
         there is one more chance                 -- */
         
   /*-- Must be an AFNI-formatted dataset! -------------*/
   /*-- find directory and last names in the pathname --*/

   for( ii=plen-1 ; ii >= 0 ; ii-- ) if( pathname[ii] == '/' ) break ;

   if( ii < 0 ){
     strcpy( dirname , "./" ) ;      /* fake directory name */
   } else {
     strcpy( dirname , pathname ) ;
     dirname[ii+1] = '\0' ;
   }
   offset = ii + 1 ;  /* offset of file within pathname - rickr [20 Sep 2002] */

   /*-- perform surgery on the name to make it a valid .HEAD --*/

   strcpy( fullname , pathname ) ;
   fname = fullname + offset ; /* trailing filename (past directory) - rickr */

   /* (REPLACE) sub = strstr( fullname , DATASET_HEADER_SUFFIX ) ;  * .HEAD ? */
   sub = strstr( fname , DATASET_HEADER_SUFFIX ) ;   /* .HEAD ?  r:fname */

   if( sub == NULL ){                                   /* no! */
      sub = strstr( fname , DATASET_BRICK_SUFFIX ) ; /* .BRIK ?  r:fname */

      if( sub == NULL ){                               /* no! */
         ii = strlen(fullname) ;
         if( fullname[ii-1] != '.' ) strcat( fullname , "." ) ; /* tack .HEAD */
         strcat( fullname , DATASET_HEADER_SUFFIX ) ;           /* onto end */

      } else {                                     /* yes! */
         strcpy( sub , DATASET_HEADER_SUFFIX ) ;   /* replace .BRIK with .HEAD */
      }
   }

   /*-- open it up? --*/

   fsize = THD_filesize(fullname) ;                         /* 06 Jan 2005 */
   if( fsize == 0 && !THD_is_file(pathname) ) fsize = -1 ;  /* 31 Mar 2005 */
   if (fsize > 0) { /* there's more to try should this fail ... ZSS Feb 06 */
      dblk = THD_init_one_datablock( dirname , fullname ) ;
      if( dblk != NULL ) {
         dset = THD_3dim_from_block( dblk ) ;
         RETURN(dset) ;
      }
   } else {
      /*-- Nothing worked, see if name is that of an atlas based ROI -- */
      dset = NULL;
      /* fprintf(stderr,"Here's your moment %s\n", pathname); */
      RETURN(THD_3dim_from_ROIstring(pathname));
   }
   /* all else failed, give them the famed message */
   CHECK_FOR_DATA(fullname) ;
   
   RETURN(dset) ; /* not destined to get here */
}

/*--------------------------------------------------------------------
   Returns -1 if no dataset, otherwise returns view_type.
   * If sname==NULL, then "./" is used.
   * pname mustn't be NULL.
   * If vt is a good view type (>= 0), then we only check that,
       otherwise we check all possible view types (smallest one
       found wins).
----------------------------------------------------------------------*/

int THD_is_dataset( char *sname , char *pname , int vt ) /* 17 Mar 2000 */
{
   THD_3dim_dataset *dset ;
   int ii , vv ;

ENTRY("THD_is_dataset") ;

   if( pname == NULL ) RETURN(-1) ;

   dset = EDIT_empty_copy(NULL) ;
   EDIT_dset_items( dset , ADN_prefix , pname , ADN_none ) ;

   if( sname != NULL )
     EDIT_dset_items( dset , ADN_directory_name , sname , ADN_none ) ;

   if( vt >= FIRST_VIEW_TYPE && vt <= LAST_VIEW_TYPE ){
     EDIT_dset_items( dset , ADN_view_type , vt , ADN_none ) ;
     ii = THD_is_file(dset->dblk->diskptr->header_name);
     THD_delete_3dim_dataset(dset,False) ;
     if( ii ) RETURN(vt) ;
     RETURN(-1) ;
   }

   for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ ){
     EDIT_dset_items( dset , ADN_view_type , vv , ADN_none ) ;
     ii = THD_is_file(dset->dblk->diskptr->header_name);
     if( ii ){ THD_delete_3dim_dataset(dset,False); RETURN(vv); }
   }

   THD_delete_3dim_dataset( dset , False ) ;
   RETURN(-1) ;
}

/*--------------------------------------------------------------------*/

char * THD_dataset_headname( char *sname , char *pname , int vt )
{
   THD_3dim_dataset *dset ;
   char *str ; int ll ;

ENTRY("THD_dataset_headname") ;

   if( pname == NULL ) RETURN(NULL) ;

   dset = EDIT_empty_copy(NULL) ;
   EDIT_dset_items( dset , ADN_prefix , pname , ADN_none ) ;

   if( sname != NULL )
      EDIT_dset_items( dset , ADN_directory_name , sname , ADN_none ) ;

   if( vt >= FIRST_VIEW_TYPE && vt <= LAST_VIEW_TYPE )
      EDIT_dset_items( dset , ADN_view_type , vt , ADN_none ) ;

   ll = strlen(dset->dblk->diskptr->header_name) + 1 ;
   str = (char *) malloc(sizeof(char)*ll ) ;
   strcpy( str , dset->dblk->diskptr->header_name ) ;

   THD_delete_3dim_dataset( dset , False ) ;
   RETURN(str) ;
}


/* ------------------------------------------------------------- */
/* given a filename, return one STORAGE_BY_* value from 3ddata.h
 *                                           20 Apr 2006 [rickr] */
int storage_mode_from_filename( char * fname )
{
ENTRY("storage_mode_from_filename");

    if( !fname || !*fname )                     RETURN(STORAGE_UNDEFINED);

    /* STORAGE_BY_SLICES was never implemented   :'( */

    if( STRING_HAS_SUFFIX(fname, ".HEAD") ||
        STRING_HAS_SUFFIX(fname, ".BRIK") ||
        STRING_HAS_SUFFIX(fname, ".BRIK.gz") )  RETURN(STORAGE_BY_BRICK);
        
    if( STRING_HAS_SUFFIX(fname, ".mnc") )      RETURN(STORAGE_BY_MINC);

    if( 0 )                                     RETURN(STORAGE_BY_VOLUMES);

    if( 0 )   /* default is NIFTI */            RETURN(STORAGE_BY_ANALYZE);
        
    if( STRING_HAS_SUFFIX(fname, ".mri") )      RETURN(STORAGE_BY_CTFMRI);

    if( STRING_HAS_SUFFIX(fname, ".svl") )      RETURN(STORAGE_BY_CTFSAM);

    if( STRING_HAS_SUFFIX(fname, ".1D") )       RETURN(STORAGE_BY_1D);

    if( STRING_HAS_SUFFIX(fname, ".3D") )       RETURN(STORAGE_BY_3D);

    if( STRING_HAS_SUFFIX(fname, ".nii")    ||
        STRING_HAS_SUFFIX(fname, ".nii.gz") ||
        STRING_HAS_SUFFIX(fname, ".nia")    ||
        STRING_HAS_SUFFIX(fname, ".hdr")    ||
        STRING_HAS_SUFFIX(fname, ".img") )      RETURN(STORAGE_BY_NIFTI);

    if( STRING_HAS_SUFFIX(fname, ".mpg")   ||
        STRING_HAS_SUFFIX(fname, ".mpeg")  ||
        STRING_HAS_SUFFIX(fname, ".MPG")   ||
        STRING_HAS_SUFFIX(fname, ".MPEG") )     RETURN(STORAGE_BY_MPEG);

    /* 26 May 2006 [rickr] */
    if( STRING_HAS_SUFFIX(fname, ".niml") )     RETURN(STORAGE_BY_NIML);

    if( STRING_HAS_SUFFIX(fname,".niml.dset") ) RETURN(STORAGE_BY_NI_SURF_DSET);

    RETURN(STORAGE_UNDEFINED);
}


/* ---------------------------------------------------- */
/* given a filename, return a pointer to the extension
 * (from file_extension_list)
 *                                  28 Jun 2006 [rickr] */
char * find_filename_extension( char * fname )
{
    char ** eptr;
    int c, flen, num_ext;

ENTRY("find_filename_extension");

    if( !fname || !*fname ) RETURN(NULL);

    num_ext = sizeof(file_extension_list)/sizeof(char *);
    flen = strlen(fname);

    for( c = 0, eptr = file_extension_list; c < num_ext; c++, eptr++ )
        if( STRING_HAS_SUFFIX(fname, *eptr) )
            RETURN(fname + (flen - strlen(*eptr)));

    RETURN(NULL);   /* not found */
}


/* ------------------------------------------------------------- */
/* given a filename, return 1 if it has a know extension that is
 * not an AFNI extension                     20 Apr 2006 [rickr] */
int has_known_non_afni_extension( char * fname )
{
    int mode;

ENTRY("has_known_non_afni_extension");

    mode = storage_mode_from_filename(fname);

    /* UNDEFINED, BRICK and VOLUMES are the unknown or AFNI cases */
    if( mode <= STORAGE_UNDEFINED   ||
        mode == STORAGE_BY_BRICK    ||
        mode == STORAGE_BY_VOLUMES  ||
        mode  > LAST_STORAGE_MODE ) RETURN(0);

    RETURN(1); /* otherwise, we recognize it as non-AFNI */
}

