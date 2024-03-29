/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

extern THD_3dim_dataset *THD_3dim_G_from_ROIstring(char *shar);

#undef  CHECK_FOR_DATA     /* 06 Jan 2005: message about empty files */
#define CHECK_FOR_DATA(fn)                                                \
 do{ if( fsize == 0 ){                                                    \
       if( isfile )                                                       \
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
    ".1D", ".1D.dset", ".1D.do", ".txt",
    ".3D",
    ".nii", ".nii.gz", ".nia", ".hdr", ".img",
    ".mpg", ".mpeg", ".MPG", ".MPEG",
    ".niml", ".niml.dset", ".niml.do",
    ".gii", ".gii.dset", ".niml.tract" , ".jpg" , ".jpeg" , ".png" , ".heic"
};

/*
   Return a plausible path to a pathless filename,
   returns NULL if no file was found.
*/
char * Add_plausible_path(char *fname)
{
   char dname[THD_MAX_NAME], ename[THD_MAX_NAME],
         *elocal=NULL, *eee=NULL, *pp=NULL, *epath=NULL;
   int epos=0, ll=0, ii=0, id = 0, kk=0;
   FILE *fp=NULL;

   ENTRY("Add_plausible_path");

   if (!fname) RETURN(NULL);

   /* If file exists as is, go back */
   if ((fp = fopen( fname , "r"))) {
      pp = (char *)malloc(sizeof(char)*(strlen(fname)+1));
      pp = strcpy(pp, fname);
      fclose(fp);
      RETURN(pp);
   }

   ii = strlen(fname);
   while (ii >=0) {
      if (fname[ii] == '/') { /* Have path in name, return whole thing,
                                 if file exists! */
         if ((fp = fopen( fname , "r"))) {
            pp = (char *)malloc(sizeof(char)*(strlen(fname)+1));
            pp = strcpy(pp, fname);
            fclose(fp);
            RETURN(pp);
         } else {
            RETURN(NULL);
         }
      }
      --ii;
   }

   /* have name, try different paths */
                       epath = getenv("AFNI_R_PATH") ;
   if( epath == NULL ) epath = getenv("AFNI_PLUGINPATH") ;
   if( epath == NULL ) epath = getenv("AFNI_PLUGIN_PATH") ;
   if( epath == NULL ) epath = getenv("PATH") ;


   /* Next block is based on one in thd_ttatlas_query.c */
   /*----- copy path list into local memory -----*/

   ll = strlen(epath) ;
   elocal = AFMALL(char, sizeof(char) * (ll+2) ) ;

   /*----- put a blank at the end -----*/

   strcpy( elocal , epath ) ; elocal[ll] = ' ' ; elocal[ll+1] = '\0' ;

   /*----- replace colons with blanks -----*/

   for( ii=0 ; ii < ll ; ii++ )
     if( elocal[ii] == ':' ) elocal[ii] = ' ' ;

   /*----- extract blank delimited strings;
           use as directory names to look for atlas -----*/

   epos = 0 ;

   do{
      ii = sscanf( elocal+epos , "%s%n" , ename , &id ); /* next substring */
      if( ii < 1 ) break ;                               /* none -> done   */

      epos += id ;                                 /* char after last scanned */

      ii = strlen(ename) ;                         /* make sure name has   */
      if( ename[ii-1] != '/' ){                    /* a trailing '/' on it */
          ename[ii]  = '/' ; ename[ii+1] = '\0' ;
      }
      strcpy(dname,ename) ;
      strcat(dname,fname) ;               /* add file name */

      if ((fp = fopen( dname , "r"))) {
         pp = (char *)malloc(sizeof(char)*(strlen(dname)+1));
         pp = strcpy(pp, dname);
         fclose(fp);
         RETURN(pp);
      }

   } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */


   RETURN(pp);
}

/*----------------------------------------------------------------
   simply given a pathname, try to open it as a dataset
   [allow for .HEAD, .BRIK, or just prefix+viewcode filenames]
------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_one_dataset( char *pathname )
{
   int ii , plen ;
   char dirname[THD_MAX_NAME] , fullname[THD_MAX_NAME] ;
   THD_3dim_dataset *dset=NULL ;  /* NULL added 23 Feb 2007, from Bernd Feige */
   THD_datablock    *dblk ;
   char *sub ;
   char *fname ;   /* to skip directory during HEAD/BRIK search in filename */
   int   offset ;  /*                                 - [rickr 20 Sep 2002] */
   long long fsize ;     /* 06 Jan 2005, to unsigned 20 Feb 2006 [rickr] */
                                      /* to long long 22 Mar 2007 [RWC] */
   int   isfile = 1;
   static char qname[THD_MAX_NAME+222] ;

ENTRY("THD_open_one_dataset") ;

   /*-- sanity check --*/

   if( pathname == NULL              ||
       (plen=strlen(pathname)) == 0  ||
       pathname[plen-1]        == '/'  ) RETURN(NULL) ;


   if( pathname[0] == '~' && pathname[1] == '/' ){  /* 13 Feb 2008 */
     char *eee = getenv("HOME") ;
     if( eee != NULL ){
       strcpy(qname,eee); strcat(qname,pathname+1);
       pathname = qname ;
     }
   }

   /*-- perhaps open the new-fangled way [22 May 2000] --*/
   if( getenv("AFNI_USE_THD_open_dataset") != NULL &&
       strstr(pathname,"[")                != NULL   ){
      dset = THD_open_dataset(pathname) ;
      THD_patch_brickim(dset) ;  /* 20 Oct 2006 */
      RETURN(dset) ;
   }

   /*-- other cases that require new-fangled way   23 Jul 2012 [rickr] --*/
   if( ! strncmp(pathname,"filelist:",9) ){
      dset = THD_open_dataset(pathname) ;
      THD_patch_brickim(dset) ;  /* 20 Oct 2006 */
      RETURN(dset) ;
   }

   fsize = THD_filesize(pathname) ;                         /* 06 Jan 2005 */

   /* replace fsize == -1 use with isfile variable   28 Feb 2007 [rickr] */
   if( fsize == 0 && !THD_is_file(pathname) ) isfile = 0;

   /*-- perhaps the MINC way --*/

   if( STRING_HAS_SUFFIX(pathname,".mnc") ){
     static int first=1 ;
     if( first ){
       ERROR_message("MINC-1 dataset open disabled: %s",pathname) ;
       first = 0 ; RETURN(NULL) ;
     }
   }

   /*-- perhaps the ANALYZE way --*/

   if( STRING_HAS_SUFFIX(pathname,".hdr") ){
     CHECK_FOR_DATA(pathname) ;               /* 06 Jan 2005 */
     dset = THD_open_analyze(pathname) ;
     THD_patch_brickim(dset) ;  /* 20 Oct 2006 */
     RETURN(dset) ;
   }

   /*-- perhaps the CTF way [04 Dec 2002] --*/

   if( STRING_HAS_SUFFIX(pathname,".mri") ){
     CHECK_FOR_DATA(pathname) ;               /* 06 Jan 2005 */
     dset = THD_open_ctfmri(pathname) ;
     THD_patch_brickim(dset) ;  /* 20 Oct 2006 */
     RETURN(dset) ;
   } else if( STRING_HAS_SUFFIX(pathname,".svl") ){
     CHECK_FOR_DATA(pathname) ;               /* 06 Jan 2005 */
     dset = THD_open_ctfsam(pathname) ;
     THD_patch_brickim(dset) ;  /* 20 Oct 2006 */
     RETURN(dset) ;
   }

   /*-- 04 Mar 2003: allow input of .1D files --*/

   if( STRING_HAS_SUFFIX(pathname,".1D") ||
       STRING_HAS_SUFFIX(pathname,".1D.dset") ){
     CHECK_FOR_DATA(pathname) ;               /* 06 Jan 2005 */
     dset = THD_open_1D(pathname) ;
     THD_patch_brickim(dset) ;  /* 20 Oct 2006 */
     RETURN(dset) ;
   } else if( STRING_HAS_SUFFIX(pathname,".3D") ){  /* 21 Mar 2003 */
     CHECK_FOR_DATA(pathname) ;               /* 06 Jan 2005 */
     dset = THD_open_3D(pathname) ;
     THD_patch_brickim(dset) ;  /* 20 Oct 2006 */
     RETURN(dset) ;
   }

   /*-- 28 Aug 2003: the NIFTI way! --*/

   if( STRING_HAS_SUFFIX(pathname,".nii")    ||
       STRING_HAS_SUFFIX(pathname,".nii.gz") ||
       STRING_HAS_SUFFIX(pathname,".nia")      ){

     /* let NIFTI decide if the dataset is there   10 Jun 2015 [ricrk] */
     /* CHECK_FOR_DATA(pathname) ; */              /* 06 Jan 2005 */
     dset = THD_open_nifti(pathname) ;
     THD_patch_brickim(dset) ;  /* 20 Oct 2006 */
     THD_report_obliquity(dset) ;  /* 20 Dec 2007 */
     RETURN(dset) ;
   }

   /*-- 03 Dec 2003: the MPEG way! --*/

   if( STRING_HAS_SUFFIX_CASE(pathname,".mpg") ||
       STRING_HAS_SUFFIX_CASE(pathname,".mpeg")  ){

     CHECK_FOR_DATA(pathname) ;               /* 06 Jan 2005 */
     dset = THD_open_mpeg(pathname) ;
     THD_patch_brickim(dset) ;  /* 20 Oct 2006 */
     RETURN(dset) ;
   }

   if( STRING_HAS_SUFFIX_CASE(pathname,".jpg")  ||
       STRING_HAS_SUFFIX_CASE(pathname,".png")  ||
       STRING_HAS_SUFFIX_CASE(pathname,".jpeg") ||
       STRING_HAS_SUFFIX_CASE(pathname,".heic")   ){  /* 06 Jul 2016 */

     CHECK_FOR_DATA(pathname) ;
     dset = THD_open_image(pathname) ;  /* will be loaded, too */
     THD_patch_brickim(dset) ;
     RETURN(dset) ;
   }

   /*-- 26 May 2006 [rickr]: the NIML way! --*/

   if( STRING_HAS_SUFFIX(pathname,".niml") ){

     CHECK_FOR_DATA(pathname) ;
     dset = THD_open_niml(pathname) ;
     THD_patch_brickim(dset) ;  /* 20 Oct 2006 */
     RETURN(dset) ;
   }

   /*-- 26 May 2006 [rickr]: the NI_SURF_DSET way! --*/

   if( STRING_HAS_SUFFIX(pathname,".niml.dset") ){

     CHECK_FOR_DATA(pathname) ;
     dset = THD_open_niml(pathname) ;
     THD_patch_brickim(dset) ;  /* 20 Oct 2006 */
     RETURN(dset) ;
   }

   /*-- 13 Feb 2008 [rickr]: the GIFTI way! --*/

   if( STRING_HAS_SUFFIX(pathname,".gii") ||
       STRING_HAS_SUFFIX(pathname,".gii.dset") ){

     CHECK_FOR_DATA(pathname) ;
     dset = THD_open_gifti(pathname) ;
     THD_patch_brickim(dset) ;  /* 20 Oct 2006 */
     RETURN(dset) ;
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

   /* note: e.g. DATASET_HEADER_SUFFIX does not start with '.' */
   /* (REPLACE) sub = strstr( fullname , DATASET_HEADER_SUFFIX ) ;  * .HEAD ? */
   sub = strstr( fname , DATASET_DOT_HEADER_SUFFIX ) ;   /* .HEAD ?  r:fname */

   if( sub == NULL ){                                   /* no! */
      sub = strstr( fname , DATASET_DOT_BRICK_SUFFIX ) ; /* .BRIK ?  r:fname */

      if( sub == NULL ){                               /* no! */
         ii = strlen(fullname) ;
         if( fullname[ii-1] != '.' ) strcat( fullname , "." ) ; /* tack .HEAD */
         strcat( fullname , DATASET_HEADER_SUFFIX ) ;           /* onto end */

      } else {                                     /* yes! */
         strcpy( sub , DATASET_DOT_HEADER_SUFFIX ) ; /* replace .BRIK with .HEAD */
      }
   }

   /*-- open it up? --*/

   fsize = THD_filesize(fullname) ;                         /* 06 Jan 2005 */
   if( fsize == 0 && !THD_is_file(fullname) ) isfile = 0;
   else                                       isfile = 1;

   /* if it is not a file, check the ROI case   28 Feb 2007 [rickr] */
   if( isfile ) {
      dblk = THD_init_one_datablock( dirname , fullname ) ;
      if( dblk != NULL ) {
         dset = THD_3dim_from_block( dblk ) ;
         THD_patch_brickim(dset) ;  /* 20 Oct 2006 */
         THD_report_obliquity(dset) ;  /* 20 Dec 2007 */
         RETURN(dset) ;
      }
   } else {
      /*-- Nothing worked, see if name is that of an atlas based ROI -- */
      /* fprintf(stderr,"Here's your moment %s\n", pathname); */
      dset = THD_3dim_G_from_ROIstring(pathname) ;
      THD_patch_brickim(dset) ;  /* 20 Oct 2006 */
      RETURN(dset) ;
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

static int THD_deconflict_nifti( char *brick_name )
{
   int lp , ls ; char suf[9] ;
   char aa,bb,cc ;

   if( !THD_is_file(brick_name) ) return 0 ;

   lp = strlen(brick_name) ;
   if( STRING_HAS_SUFFIX(brick_name,".nii") ){
     ls = lp-4 ; strcpy(suf,".nii") ;
   } else if( STRING_HAS_SUFFIX(brick_name,".nii.gz") ){
     ls = lp-7 ; strcpy(suf,".nii.gz") ;
   } else if( STRING_HAS_SUFFIX(brick_name,".hdr") ){
     ls = lp-4 ; strcpy(suf,".hdr") ;
   } else {
     ls = lp   ; strcpy(suf,"\0") ;
   }

   for( aa='A' ; aa <= 'Z' ; aa++ ){
    for( bb='A' ; bb <= 'Z' ; bb++ ){
     for( cc='1' ; cc <= '9' ; cc++ ){
       brick_name[ls  ] = '_' ;
       brick_name[ls+1] = aa  ;
       brick_name[ls+2] = bb  ;
       brick_name[ls+3] = cc  ;
       brick_name[ls+4] = '\0';
       strcat(brick_name,suf) ;
       if( ! THD_is_file(brick_name) ) return 1 ;
   }}}

   if( ls > THD_MAX_NAME-45 ) ls = THD_MAX_NAME-45 ;
   brick_name[ls++] = '_' ;
   UNIQ_idcode_fill( brick_name+ls ) ;
   strcat(brick_name,suf) ;
   return 1;
}

/*--------------------------------------------------------------------*/
/*! Modify the prefix of a dataset to make sure it doesn't conflict
    with an existing file.  Return value is 0 if no change was
    needed, 1 if a change was made.
----------------------------------------------------------------------*/

int THD_deconflict_prefix( THD_3dim_dataset *dset )
{
   char pfx[THD_MAX_PREFIX] ; int lp  ;
   char aa,bb,cc ;

ENTRY("THD_deconflict_prefix") ;

   if( !ISVALID_DSET(dset) ) RETURN(0) ;

   MCW_strncpy( pfx , DSET_PREFIX(dset) , THD_MAX_PREFIX ) ;

   if( PREFIX_IS_NIFTI(pfx) ){
     /* adjust brick_name */
     EDIT_dset_items( dset , ADN_prefix , pfx , ADN_none ) ;

     /* deconflict brick_name (to include path) */
     lp = THD_deconflict_nifti( dset->dblk->diskptr->brick_name ) ;

     /* reset names from adjusted brick_name */
     if( lp > 0 ) EDIT_dset_items( dset , ADN_prefix ,
                                   dset->dblk->diskptr->brick_name , ADN_none );
     RETURN(lp) ;
   }

   if( ! THD_is_file(dset->dblk->diskptr->header_name) ) RETURN(0) ;

   lp = strlen(pfx) ;
   if( lp > THD_MAX_PREFIX-5 ) lp = THD_MAX_PREFIX-5 ;

   for( aa='A' ; aa <= 'Z' ; aa++ ){
    for( bb='A' ; bb <= 'Z' ; bb++ ){
     for( cc='1' ; cc <= '9' ; cc++ ){
       pfx[lp  ] = '_' ;
       pfx[lp+1] = aa  ;
       pfx[lp+2] = bb  ;
       pfx[lp+3] = cc  ;
       pfx[lp+4] = '\0';
       EDIT_dset_items( dset , ADN_prefix , pfx , ADN_none ) ;
       if( ! THD_is_file(dset->dblk->diskptr->header_name) ) RETURN(1) ;
   }}}

   /** ugly brute force final solution: should never happen! **/

   if( lp > THD_MAX_PREFIX-35 ) lp = THD_MAX_PREFIX-35 ;
   pfx[lp++] = '_' ; UNIQ_idcode_fill( pfx+lp ) ;
   EDIT_dset_items( dset , ADN_prefix , pfx , ADN_none ) ;
   RETURN(1) ;
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


    if( 0 )                                     RETURN(STORAGE_BY_VOLUMES);

    if( 0 )   /* default is NIFTI */            RETURN(STORAGE_BY_ANALYZE);

    if( STRING_HAS_SUFFIX(fname, ".mri") )      RETURN(STORAGE_BY_CTFMRI);

    if( STRING_HAS_SUFFIX(fname, ".svl") )      RETURN(STORAGE_BY_CTFSAM);

    if( STRING_HAS_SUFFIX(fname, ".1D") ||
        STRING_HAS_SUFFIX(fname, ".1D.dset"))       RETURN(STORAGE_BY_1D);

    if( STRING_HAS_SUFFIX(fname, ".3D") )       RETURN(STORAGE_BY_3D);

    if( STRING_HAS_SUFFIX(fname, ".nii")    ||
        STRING_HAS_SUFFIX(fname, ".nii.gz") ||
        STRING_HAS_SUFFIX(fname, ".nia")    ||
        STRING_HAS_SUFFIX(fname, ".hdr")    ||
        STRING_HAS_SUFFIX(fname, ".img") )      RETURN(STORAGE_BY_NIFTI);

    if( STRING_HAS_SUFFIX_CASE(fname,".mpg") ||
        STRING_HAS_SUFFIX_CASE(fname,".mpeg")  )RETURN(STORAGE_BY_MPEG);

    /* 26 May 2006 [rickr] */
    if( STRING_HAS_SUFFIX(fname, ".niml") )     RETURN(STORAGE_BY_NIML);

    if( STRING_HAS_SUFFIX(fname,".niml.dset") ) RETURN(STORAGE_BY_NI_SURF_DSET);
    if( STRING_HAS_SUFFIX(fname,".tract.dset") ) RETURN(STORAGE_BY_NI_TRACT);

    if( STRING_HAS_SUFFIX(fname,".gii") ||
        STRING_HAS_SUFFIX(fname,".gii.dset") )  RETURN(STORAGE_BY_GIFTI);

    RETURN(STORAGE_UNDEFINED);
}

/* ------------------------------------------------------------- */
/* return whether the given storage mode suggests a surface type
 *
 * Note that 3D, 1D and NIML do not imply either way, life is hard.
 * For now, return as true for them.
 *
 * returns 0 or 1 as boolean                 04 Apr 2012 [rickr] */
int is_surface_storage_mode( int smode )

{
ENTRY("is_surface_storage_mode");

    if ( smode == STORAGE_BY_1D           ||
         smode == STORAGE_BY_3D           ||
         smode == STORAGE_BY_NIML         ||
         smode == STORAGE_BY_NI_SURF_DSET ||
         smode == STORAGE_BY_GIFTI
       ) RETURN(1);

    RETURN(0);
}


int storage_mode_from_prefix( char * fname )
{
   int sm=STORAGE_UNDEFINED;

ENTRY("storage_mode_from_prefix");

   if( !fname || !*fname )                     RETURN(STORAGE_UNDEFINED);
   sm = storage_mode_from_filename(fname);
   if( sm != STORAGE_UNDEFINED ) RETURN(sm);

   if (fname[strlen(fname)-1] == '.') {
      if( STRING_HAS_SUFFIX(fname, "+orig.") ||
          STRING_HAS_SUFFIX(fname, "+acpc.") ||
          STRING_HAS_SUFFIX(fname, "+tlrc.") ) sm = STORAGE_BY_BRICK;
   } else {
      if( STRING_HAS_SUFFIX(fname, "+orig") ||
          STRING_HAS_SUFFIX(fname, "+acpc") ||
          STRING_HAS_SUFFIX(fname, "+tlrc") ) sm = STORAGE_BY_BRICK;
   }

   RETURN(sm);
}

/* There is also: storage_mode_str() */
char *storage_mode_name(int mode) {
   switch (mode) {
      case STORAGE_UNDEFINED:
         return("UNDEFINED");
      case STORAGE_BY_BRICK:
         return("BRIK");
      case STORAGE_BY_VOLUMES:
         return("VOLUMES");
      case STORAGE_BY_ANALYZE:
         return("ANALYZE");
      case STORAGE_BY_CTFMRI:
         return("CTFMRI");
      case STORAGE_BY_CTFSAM:
         return("CTFSAM");
      case STORAGE_BY_1D:
         return("1D");
      case STORAGE_BY_3D:
         return("3D");
      case STORAGE_BY_NIFTI:
         return("NIFTI");
      case STORAGE_BY_MPEG:
         return("MPEG");
      case STORAGE_BY_NIML:
         return("NIML");
      case STORAGE_BY_NI_SURF_DSET:
         return("NI_SURF_DSET");
      case STORAGE_BY_NI_TRACT:
         return("NI_TRACT");
      case STORAGE_BY_GIFTI:
         return("GIFTI");
   }
   return("UNDEFINED");
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

/* --------------------------------------------------------------- */
/* return 1 if smode is for a writable format   5 Mar 2012 [rickr] */

int is_writable_storage_mode( int smode )
{
ENTRY("has_writable_extension");

   switch (smode) {
      case STORAGE_UNDEFINED:           RETURN(0);
      case STORAGE_BY_ANALYZE:          RETURN(0);  /* returns NIFTI */
      case STORAGE_BY_CTFMRI:           RETURN(0);
      case STORAGE_BY_CTFSAM:           RETURN(0);
      case STORAGE_BY_MPEG:             RETURN(0);

      case STORAGE_BY_BRICK:            RETURN(1);
      case STORAGE_BY_VOLUMES:          RETURN(1);
      case STORAGE_BY_1D:               RETURN(1);
      case STORAGE_BY_3D:               RETURN(1);
      case STORAGE_BY_NIFTI:            RETURN(1);
      case STORAGE_BY_NIML:             RETURN(1);
      case STORAGE_BY_NI_SURF_DSET:     RETURN(1);
      case STORAGE_BY_NI_TRACT:         RETURN(1);
      case STORAGE_BY_GIFTI:            RETURN(1);
   }

   RETURN(0); /* not writable */
}

/* ---------------------------------------------------- */
/* given a filename, return a string excluding known
   afni extensions (from file_extension_list)
   DO NOT FREE output.
                                   06 Feb 2012 [ZSS] */

char * without_afni_filename_extension( char * fname )
{
    char ** eptr;
    static char onames[5][THD_MAX_NAME+1];
    static int icall=0;
    int c, flen, num_ext;

    ENTRY("without_afni_filename_extension");

    if( !fname || !*fname ) RETURN(NULL);
    ++icall;
    if (icall > 4) icall = 0;
    onames[icall][0]='\0';

    if (strlen(fname) >= THD_MAX_NAME) {
      WARNING_message("Filename too long for without_afni_filename_extension()"
                      "Returning fname");
      RETURN(fname);
    }
    num_ext = sizeof(file_extension_list)/sizeof(char *);
    flen = strlen(fname);

    for( c = 0, eptr = file_extension_list; c < num_ext; c++, eptr++ ) {
        if( STRING_HAS_SUFFIX(fname, *eptr) ) {
            flen = flen - strlen(*eptr);
            strncpy(onames[icall], fname, flen);
            onames[icall][flen]='\0';
            RETURN(onames[icall]);
        }
    }
    RETURN(fname);   /* not found */
}

char * without_afni_filename_view_and_extension( char * fname )
{
    char *noext;
    static char onames[5][THD_MAX_NAME+1];
    static int icall=0;
    int flen;

ENTRY("without_afni_filename_view_and_extension");

    if( !fname || !*fname ) RETURN(NULL);
    ++icall;
    if (icall > 4) icall = 0;
    onames[icall][0]='\0';

    if ((noext = without_afni_filename_extension(fname))) {
      flen = strlen(noext);
      if (fname[strlen(noext)-1] == '.') {
         if( STRING_HAS_SUFFIX(noext, "+orig.") ||
             STRING_HAS_SUFFIX(noext, "+acpc.") ||
             STRING_HAS_SUFFIX(noext, "+tlrc.") ) {
            flen = flen - 6;
            strncpy(onames[icall], noext, flen);
            onames[icall][flen]='\0';
         }
      } else if( STRING_HAS_SUFFIX(noext, "+orig") ||
                 STRING_HAS_SUFFIX(noext, "+acpc") ||
                 STRING_HAS_SUFFIX(noext, "+tlrc") ) {
         flen = flen - 5;
         strncpy(onames[icall], noext, flen);
         onames[icall][flen]='\0';
      } else {
         strncpy(onames[icall], noext, flen);
         onames[icall][flen]='\0';
      }

      RETURN(onames[icall]);
    }

    RETURN(fname);   /* not found */
}

/*--------------------------------------------------------------*/
/* Add prefix and/or suffix to filename fname taking care
   to leave the path undisturbed and known extensions preserved */

char * modify_afni_prefix( char * fname , char *pref, char *suf)
{
    char ** eptr;
    static char onames[9][THD_MAX_NAME+1];
    static int icall=0;
    int c, isl, icp, flen, num_ext;

ENTRY("modify_afni_prefix");

    if( !fname || !*fname ) RETURN(NULL);
    ++icall;
    if (icall > 8) icall = 0;
    onames[icall][0]='\0';


    if (!suf && !pref) {
      /* nothing to do */
      RETURN(fname);
    }

    if (pref && strlen(pref)) {
      flen = strlen(fname);
      if (flen+strlen(pref) >= THD_MAX_NAME) {
         WARNING_message("Filename too long for modify_afni_prefix()"
                         "Returning fname");
         RETURN(fname);
      }

      /* find last '/' */
      isl=flen-1;
      while(isl>=0) {
         if (fname[isl] == '/') break;
         --isl;
      }
      for (icp=0; icp<=isl; ++icp) onames[icall][icp]=fname[icp];
      onames[icall][icp]='\0';
      strcat(onames[icall], pref);
      strcat(onames[icall],fname+isl+1);

      fname = onames[icall];
      ++icall; if (icall > 8) icall = 0;
      onames[icall][0]='\0';
    }

    if (suf && strlen(suf)) {
      flen = strlen(fname);
      if (flen+strlen(suf) >= THD_MAX_NAME) {
         WARNING_message("Filename too long for modify_afni_prefix()"
                         "Returning fname");
         RETURN(fname);
      }

      /* remove the extension */
      num_ext = sizeof(file_extension_list)/sizeof(char *);
      for( c = 0, eptr = file_extension_list; c < num_ext; c++, eptr++ ) {
        if( STRING_HAS_SUFFIX(fname, *eptr) ) {
            flen = flen - strlen(*eptr);
            strncpy(onames[icall], fname, flen); onames[icall][flen]='\0';
            strcat(onames[icall], suf);
            strcat(onames[icall], *eptr);
            break;
        }
      }
      if (onames[icall][0] == '\0') {
         /* no extensions, just append */
         strcat(onames[icall],fname);
         strcat(onames[icall], suf);
      }

      RETURN(onames[icall]);
   }

   /* should not get here, but be nice anyway */
   RETURN(fname);   /* not found */

}
