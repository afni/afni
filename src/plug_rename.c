/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Simple plugin to rename a dataset from within AFNI.

  May 1998: modified to work with compressed .BRIK names.
************************************************************************/

char * RENAME_main( PLUGIN_interface * ) ;

static char helpstring[] =
   " Purpose: Renaming a dataset from within AFNI\n"
   " Inputs:\n"
   " Dataset   = A dataset in the current session.\n"
   " Prefix    = New filename prefix.\n"
   " N.B.: All views containing this dataset's\n"
   "       children and/or parents will be affected\n"
   "       affected by this operation."
;

/***********************************************************************
   Set up the interface to the user
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   CHECK_IF_ALLOWED("DATASETRENAME","Dataset Rename") ;  /* 30 Sep 2016 */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "Dataset Rename" , "Dataset Renaming" , helpstring ,
                                 PLUGIN_CALL_VIA_MENU , RENAME_main  ) ;

   PLUTO_add_hint( plint , "Rename a Dataset" ) ;

   PLUTO_set_sequence( plint , "A:afnicontrol:dset" ) ;

   PLUTO_set_runlabels( plint , "Rename+Keep" , "Rename+Close" ) ;  /* 04 Nov 2003 */

   /*-- first line of input: Dataset --*/

   PLUTO_add_option( plint , "Input" , "Input" , TRUE ) ;
   PLUTO_add_dataset(plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    WARP_ON_DEMAND_MASK | DIMEN_ALL_MASK |
                                    SESSION_ALL_MASK    | BRICK_ALLTYPE_MASK ) ;

   /*-- second line of input: Prefix for output dataset --*/

   PLUTO_add_option( plint , "Output" , "Output" , TRUE ) ;
   PLUTO_add_string( plint , "Prefix" , 0,NULL , 19 ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

char * RENAME_main( PLUGIN_interface * plint )
{
   char * new_prefix ;
   MCW_idcode * idc ;
   THD_3dim_dataset * dset ;
   char * old_header_name , * old_brick_name ;
   THD_slist_find find ;
   THD_session * ss ;
   int iss , id , ivv , ierr , mm ;

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   if( plint == NULL )
      return "***********************\n"
             "RENAME_main: NULL input\n"
             "***********************"  ;

   PLUTO_next_option(plint) ;
   idc  = PLUTO_get_idcode(plint) ;
   dset = PLUTO_find_dset(idc) ;
   if( dset == NULL )
      return "******************************\n"
             "RENAME_main:bad input dataset\n"
             "******************************"  ;

   PLUTO_next_option(plint) ;
   new_prefix = PLUTO_get_string(plint) ;
   if( ! PLUTO_prefix_ok(new_prefix) )
      return "***********************\n"
             "RENAME_main:bad prefix\n"
             "***********************"  ;

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   /*-- find this dataset in the AFNI library --*/

   find = THD_dset_in_sessionlist( FIND_IDCODE, idc, GLOBAL_library.sslist, -1 ) ;
   iss  = find.sess_index ;
   ss   = GLOBAL_library.sslist->ssar[iss] ;
   id = find.dset_index ; 

   /*-- for each element of this row,
        change its internal names and, if needed, filenames on disk --*/

   ierr = 0 ;

   for( ivv=FIRST_VIEW_TYPE ; ivv <= LAST_VIEW_TYPE ; ivv++ ){

      dset = GET_SESSION_DSET(ss, id, ivv);
      
      if( ! ISVALID_3DIM_DATASET(dset) ) continue ;  /* skip this one */

      /*-- copy the old filenames --*/

      old_header_name = XtNewString( dset->dblk->diskptr->header_name ) ;
      old_brick_name  = XtNewString( dset->dblk->diskptr->brick_name  ) ;

      /*-- initialize the new filenames inside the dataset --*/

      EDIT_dset_items( dset , ADN_prefix , new_prefix , ADN_none ) ;

      /*-- rename the old files to the new files, if they exist on disk --*/

      if( THD_is_file(old_header_name) )
         ierr += rename( old_header_name , dset->dblk->diskptr->header_name ) ;

      /* May 1998: fix .BRIK rename to allow for compression */
#if 0
      if( THD_is_file(old_brick_name) )
         ierr += rename( old_brick_name , dset->dblk->diskptr->brick_name ) ;
#else
      mm = COMPRESS_filecode(old_brick_name) ;
      if( mm != COMPRESS_NOFILE ){
        char * old_name = COMPRESS_add_suffix(old_brick_name,mm) ;
        char * new_name = COMPRESS_add_suffix(dset->dblk->diskptr->brick_name,mm) ;
        ierr += rename( old_name , new_name ) ;
        free(old_name) ; free(new_name) ;
      }
#endif

      XtFree(old_header_name) ; XtFree(old_brick_name) ;
   }

   /*-- clean up AFNI --*/

   PLUTO_fixup_names() ;

   /*-- done --*/

   if( ierr ) return "***********************************************\n"
                     "RENAME_main: some file rename operations failed\n"
                     "***********************************************"  ;

   return NULL ;
}
