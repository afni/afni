#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Simple plugin to copy a dataset and make a new one.
************************************************************************/

char * COPY_main( PLUGIN_interface * ) ;

static char helpstring[] =
   " Purpose: Creating a copy of a dataset\n"
   " Inputs:\n"
   " Dataset = A dataset in the current session that exists in memory\n"
   "             (not warp-on-demand).\n"
   " Prefix  = Filename prefix to be used for the output dataset."
;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "Dataset Copy" , "Make a Copy of a Dataset" , helpstring ,
                                 PLUGIN_CALL_VIA_MENU , COPY_main  ) ;

   PLUTO_add_hint( plint , "Make a Copy of a Dataset" ) ;

   /*-- first line of input: Dataset --*/

   PLUTO_add_option( plint , "Input" , "Input" , TRUE ) ;
   PLUTO_add_dataset(plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_ALL_MASK | BRICK_ALLTYPE_MASK ) ;

   /*-- second line of input: Prefix for output dataset --*/

   PLUTO_add_option( plint , "Output" , "Output" , TRUE ) ;
   PLUTO_add_string( plint , "Prefix" , 0,NULL , 19 ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

char * COPY_main( PLUGIN_interface * plint )
{
   char * tag , * new_prefix ;
   MCW_idcode * idc ;
   THD_3dim_dataset * dset , * new_dset ;
   int ival ;

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   if( plint == NULL )
      return "**********************\n"
             "COPY_main:  NULL input\n"
             "**********************"  ;

   PLUTO_next_option(plint) ;
   idc  = PLUTO_get_idcode(plint) ;
   dset = PLUTO_find_dset(idc) ;
   if( dset == NULL )
      return "*****************************\n"
             "COPY_main:  bad input dataset\n"
             "*****************************"  ;

   PLUTO_next_option(plint) ;
   new_prefix = PLUTO_get_string(plint) ;
   if( ! PLUTO_prefix_ok(new_prefix) )
      return "**********************\n"
             "COPY_main:  bad prefix\n"
             "**********************"  ;

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   /*-- make a new dataset --*/

   new_dset = PLUTO_copy_dset( dset , new_prefix ) ;

   if( new_dset == NULL )
      return  "****************************************\n"
              "COPY_main:  failed to copy input dataset\n"
              "****************************************"  ;

   DSET_unload( dset ) ;  /* unload old one from memory */

   ival = PLUTO_add_dset( plint , new_dset , DSET_ACTION_MAKE_CURRENT ) ;

   if( ival ){
      THD_delete_3dim_dataset( new_dset , False ) ;
      return "**********************************************\n"
             "COPY_main:  failure to add new dataset to AFNI\n"
             "**********************************************" ;
   }

   /*-- done successfully!!! --*/

   return NULL ;
}
