/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"


/*----------------------------------------------------------------
   simply given a pathname, try to open it as a dataset
   [allow for .HEAD, .BRIK, or just prefix+viewcode filenames]
------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_one_dataset( char *pathname )
{
   int ii , plen ;
   char dirname[THD_MAX_NAME] , fullname[THD_MAX_NAME] ;
   THD_3dim_dataset * dset ;
   THD_datablock    * dblk ;
   char * sub ;
   char * fname ;   /* to skip directory during HEAD/BRIK search in filename */
   int    offset ;  /*                                 - [rickr 20 Sep 2002] */

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

   /*-- perhaps the MINC way --*/

   if( STRING_HAS_SUFFIX(pathname,".mnc") ){
     RETURN( THD_open_minc(pathname) ) ;
   }

   /*-- perhaps the ANALYZE way --*/

   if( STRING_HAS_SUFFIX(pathname,".hdr") ){
     RETURN( THD_open_analyze(pathname) ) ;
   }

   /*-- perhaps the CTF way [04 Dec 2002] --*/

   if( STRING_HAS_SUFFIX(pathname,".mri") ){
     RETURN( THD_open_ctfmri(pathname) ) ;
   } else if( STRING_HAS_SUFFIX(pathname,".svl") ){
     RETURN( THD_open_ctfsam(pathname) ) ;
   }

   /*-- 04 Mar 2003: allow input of .1D files --*/

   if( STRING_HAS_SUFFIX(pathname,".1D") ){
     RETURN( THD_open_1D(pathname) ) ;
   } else if( STRING_HAS_SUFFIX(pathname,".3D") ){  /* 21 Mar 2003 */
     RETURN( THD_open_3D(pathname) ) ;
   }

   /*-- 28 Aug 2003: the NIFTI way! --*/

   if( STRING_HAS_SUFFIX(pathname,".nii") ||
       STRING_HAS_SUFFIX(pathname,".nia")   ){

     RETURN( THD_open_nifti(pathname) ) ;
   }

   /*-- 03 Dec 2003: the MPEG way! --*/

   if( STRING_HAS_SUFFIX(pathname,".mpg")  ||
       STRING_HAS_SUFFIX(pathname,".MPG")  ||
       STRING_HAS_SUFFIX(pathname,".MPEG") ||
       STRING_HAS_SUFFIX(pathname,".mpeg")   ){

     RETURN( THD_open_mpeg(pathname) ) ;
   }

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

   dblk = THD_init_one_datablock( dirname , fullname ) ;
   if( dblk == NULL ) RETURN(NULL) ;

   dset = THD_3dim_from_block( dblk ) ;
   RETURN(dset) ;
}

/*--------------------------------------------------------------------
   Returns -1 if no dataset, otherwise returns view_type.
   * If sname==NULL, then "./" is used.
   * pname mustn't be NULL.
   * If vt is a good view type (>= 0), then we only check that,
       otherwise we check all possible view types (smallest one
       found wins).
----------------------------------------------------------------------*/

int THD_is_dataset( char * sname , char * pname , int vt ) /* 17 Mar 2000 */
{
   THD_3dim_dataset * dset ;
   int ii , vv ;

   if( pname == NULL ) return -1 ;

   dset = EDIT_empty_copy(NULL) ;
   EDIT_dset_items( dset , ADN_prefix , pname , ADN_none ) ;

   if( sname != NULL )
     EDIT_dset_items( dset , ADN_directory_name , sname , ADN_none ) ;

   if( vt >= FIRST_VIEW_TYPE && vt <= LAST_VIEW_TYPE ){
     EDIT_dset_items( dset , ADN_view_type , vt , ADN_none ) ;
     ii = THD_is_file(dset->dblk->diskptr->header_name);
     THD_delete_3dim_dataset(dset,False) ;
     if( ii ) return vt ;
     return -1 ;
   }

   for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ ){
     EDIT_dset_items( dset , ADN_view_type , vv , ADN_none ) ;
     ii = THD_is_file(dset->dblk->diskptr->header_name);
     if( ii ){ THD_delete_3dim_dataset(dset,False); return vv; }
   }

   THD_delete_3dim_dataset( dset , False ) ;
   return -1 ;
}

/*--------------------------------------------------------------------*/

char * THD_dataset_headname( char * sname , char * pname , int vt )
{
   THD_3dim_dataset * dset ;
   char * str ; int ll ;

   if( pname == NULL ) return NULL ;

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
   return str ;
}
