/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*---------------------------------------------------------------*/
/*! Initialize the names inside a diskptr
   29 Feb 2001: modified to take directory from prefixname
                as well as from dirname - RWCox.
   12 May 2003: if prefixname starts with '/', then dirname
                will be ignored - RWCox.
-----------------------------------------------------------------*/

void THD_init_diskptr_names( THD_diskptr *dkptr ,
                             char *dirname , char *headname ,
                             char *prefixname , int view_type ,
                             Boolean do_datafiles )
{
   int ii ;
   Boolean redo_filecode = False ;
   char dname[THD_MAX_NAME]="\0" , pname[THD_MAX_PREFIX]="\0" ; /* 29 Feb 2001 */

ENTRY("THD_init_diskptr_names") ;

   if( ! ISVALID_DISKPTR(dkptr) ) EXRETURN ;

   /* 29 Feb 2001: put dirname and any directories in prefixname together */

   if( dirname != NULL && (ii=strlen(dirname)) > 0 ){
      MCW_strncpy(dname,dirname,THD_MAX_NAME-2) ;
      if( dname[ii-1] != '/' ){ dname[ii] = '/'; dname[ii+1] = '\0'; }
   }

   if( prefixname != NULL ){
      if( strstr(prefixname,"/") != NULL ){
         int lp = strlen(prefixname) , jj , ld ;
         for( ii=lp-1 ; ii >= 0 && prefixname[ii] != '/' ; ii-- ) ; /* find last '/' */
         if( ii >= 0 ){  /* should always be true */
            ld = strlen(dname) ; if( prefixname[0] == '/' ) ld = 0 ;
            memcpy(dname+ld,prefixname,ii+1) ; dname[ld+ii+1] = '\0' ;
            MCW_strncpy(pname,prefixname+ii+1,THD_MAX_PREFIX) ;
         } else {
            MCW_strncpy(pname,prefixname,THD_MAX_PREFIX) ; /* should never transpire */
         }
      } else {
         MCW_strncpy(pname,prefixname,THD_MAX_PREFIX) ;
      }
   }

   /*-- rewrite directory name? --*/

   if( (ii=strlen(dname)) > 0 ){
      MCW_strncpy( dkptr->directory_name , dname , THD_MAX_NAME ) ;
      if( dkptr->directory_name[ii-1] != '/' ){
         dkptr->directory_name[ii]   = '/' ;
         dkptr->directory_name[ii+1] = '\0' ;
      }
   }

   /*-- rewrite viewcode? --*/

   if( view_type >= FIRST_VIEW_TYPE && view_type <= LAST_VIEW_TYPE ){
      MCW_strncpy( dkptr->viewcode , VIEW_codestr[view_type] , THD_MAX_VIEWCODE ) ;
      redo_filecode = True ;
   }

   /*-- rewrite prefix? --*/

   if( strlen(pname) > 0 ){
      MCW_strncpy( dkptr->prefix , pname , THD_MAX_PREFIX ) ;
      redo_filecode = True ;
   }

   /*-- if either viewcode or prefix changed, alter filecode --*/

   if( redo_filecode ){
      switch (dkptr->storage_mode) { /* ZSS: Oct 2011 */
         default: /* This makes no sense, but to keep matters
                     as they were. Migrate slowly towards
                     STORAGE_BY_NIFTI treatment below */
            PREFIX_VIEW_TO_FILECODE( dkptr->prefix, dkptr->viewcode,
                                  dkptr->filecode ) ;
            break;
         case STORAGE_BY_BRICK:
            PREFIX_VIEW_TO_FILECODE( dkptr->prefix, dkptr->viewcode,
                                  dkptr->filecode ) ;
            break;
         case STORAGE_BY_1D:
         case STORAGE_BY_NI_SURF_DSET:
         case STORAGE_BY_NI_TRACT:
         case STORAGE_BY_GIFTI:
         case STORAGE_BY_NIFTI:
            strcpy(dkptr->filecode, dkptr->prefix);
            break;
      }
   }

   /*-- rewrite header_name --*/
   if( headname != NULL && strlen(headname) > 0 ){
      MCW_strncpy( dkptr->header_name , headname , THD_MAX_NAME ) ;
   } else {
      switch (dkptr->storage_mode) { /* ZSS: Oct 2011 */
         case STORAGE_BY_BRICK:
            sprintf( dkptr->header_name , "%s%s.%s" ,
               dkptr->directory_name,dkptr->filecode,DATASET_HEADER_SUFFIX ) ;
            break;
         default: /* same twisted logic as in previous switch */
            sprintf( dkptr->header_name , "%s%s.%s" ,
               dkptr->directory_name,dkptr->filecode,DATASET_HEADER_SUFFIX ) ;
            break;
         case STORAGE_BY_1D:
         case STORAGE_BY_NI_SURF_DSET:
         case STORAGE_BY_NI_TRACT:
         case STORAGE_BY_GIFTI:
         case STORAGE_BY_NIFTI:
            sprintf( dkptr->header_name , "%s%s" ,
               dkptr->directory_name,dkptr->filecode ) ;
            break;
      }
   }

   /*-- if desired, create the datafile names as well --*/
   if( do_datafiles ){
      switch (dkptr->storage_mode) { /* ZSS: Oct 2011 */
         case STORAGE_BY_BRICK:
            sprintf( dkptr->brick_name , "%s%s.%s",
                     dkptr->directory_name,dkptr->filecode,DATASET_BRICK_SUFFIX);
            break;
         default:
            sprintf( dkptr->brick_name , "%s%s.%s",
                     dkptr->directory_name,dkptr->filecode,DATASET_BRICK_SUFFIX);
            break;
         case STORAGE_BY_1D:
         case STORAGE_BY_NI_SURF_DSET:
         case STORAGE_BY_NI_TRACT:
         case STORAGE_BY_GIFTI:
         case STORAGE_BY_NIFTI:
            sprintf( dkptr->brick_name , "%s%s",
                     dkptr->directory_name,dkptr->filecode);
            break;
      }
   }

   EXRETURN ;
}
