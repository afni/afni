#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/*****************************************************************************
   Like 3ddup, but as a plugin.  RWCox - 06 Aug 2003.
******************************************************************************/
   
static char helpstring[] =
  " Purpose: Make a 'warp-on-demand' duplicated of a dataset.\n"
  "\n"
  " Inputs:\n"
  "  Dataset = A dataset in the current session (not warp-on-demand itself).\n"
  "  Prefix  = Name for the new dataset.\n"
  "\n"
  " Note: output dataset will be in the AFNI .HEAD format.\n"
  "\n"
  " RWCox - 06 Aug 2003 - cf. 3ddup."
;

static char * DUP_main( PLUGIN_interface *plint ) ;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "Dataset Dup" , "Warp-on-Demand duplicate" , helpstring ,
                                 PLUGIN_CALL_VIA_MENU , DUP_main  ) ;

   PLUTO_add_hint( plint , "Warp-on-Demand duplicate" ) ;

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

/*----------------------------------------------------------------------------*/

static char * DUP_main( PLUGIN_interface *plint )
{
   THD_3dim_dataset *dset_in , *dset_out ;
   THD_warp *warp , *twarp ;
   MCW_idcode *idc ;
   char *new_prefix ;

   if( plint == NULL ) return " \nDUP_main: NULL input!\n" ;

   PLUTO_next_option(plint) ;
   idc     = PLUTO_get_idcode(plint) ;
   dset_in = PLUTO_find_dset(idc) ;
   if( dset_in == NULL )
     return "****************************\n"
            "DUP_main:  bad input dataset\n"
            "****************************"  ;
   if( !DSET_ONDISK(dset_in) )
     return "********************************\n"
            "DUP_main:  illegal input dataset\n"
            "********************************"  ;

   PLUTO_next_option(plint) ;
   new_prefix = PLUTO_get_string(plint) ;
   if( ! PLUTO_prefix_ok(new_prefix) )
     return "*********************\n"
            "DUP_main:  bad prefix\n"
            "*********************"  ;

   /*** copy header info ***/

   dset_out = EDIT_empty_copy( dset_in ) ;
   if( !ISVALID_3DIM_DATASET(dset_out) )
     return "****************************\n"
            "DUP_main:  can't duplicate?!\n"
            "****************************"  ;

   EDIT_dset_items( dset_out , ADN_prefix,new_prefix , ADN_none ) ;

   tross_Copy_History( dset_in , dset_out ) ;
   tross_Append_History( dset_out , "Warp-on-Demand duplicate from plug_3ddup." ) ;

   warp = myXtNew(THD_warp) ; *warp = IDENTITY_WARP ;

   EDIT_dset_items( dset_out ,
                      ADN_warp        , warp    ,
                      ADN_warp_parent , dset_in ,
                    ADN_none ) ;

   /*** done! ***/

   THD_write_3dim_dataset( NULL , NULL , dset_out , False ) ;
   PLUTO_add_dset( plint , dset_out , DSET_ACTION_MAKE_CURRENT ) ;

   return NULL ;
}
