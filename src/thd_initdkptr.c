#include "mrilib.h"
#include "thd.h"


/*---------------------------------------------------------------
   Initialize the names inside a diskptr
-----------------------------------------------------------------*/

void THD_init_diskptr_names( THD_diskptr * dkptr ,
                             char * dirname , char * headname ,
                             char * prefixname , int view_type ,
                             Boolean do_datafiles )
{
   int ii ;
   Boolean redo_filecode = False ;

   if( ! ISVALID_DISKPTR(dkptr) ) return ;

   /*-- rewrite directory name? --*/

   if( dirname != NULL && (ii=strlen(dirname)) > 0 ){
      MCW_strncpy( dkptr->directory_name , dirname , THD_MAX_NAME ) ;
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

   if( prefixname != NULL && strlen(prefixname) > 0 ){
      MCW_strncpy( dkptr->prefix , prefixname , THD_MAX_PREFIX ) ;
      redo_filecode = True ;
   }

   /*-- if either viewcode or prefix changed, alter filecode --*/

   if( redo_filecode ){
      PREFIX_VIEW_TO_FILECODE( dkptr->prefix, dkptr->viewcode, dkptr->filecode ) ;
   }

   /*-- rewrite header_name --*/

   if( headname != NULL && strlen(headname) > 0 ){
      MCW_strncpy( dkptr->header_name , headname , THD_MAX_NAME ) ;
   } else {
      sprintf( dkptr->header_name , "%s%s.%s" ,
               dkptr->directory_name,dkptr->filecode,DATASET_HEADER_SUFFIX ) ;
   }

   /*-- if desired, create the datafile names as well --*/

   if( do_datafiles ){

      sprintf( dkptr->brick_name , "%s%s.%s",
               dkptr->directory_name,dkptr->filecode,DATASET_BRICK_SUFFIX ) ;

   }
   return ;
}
