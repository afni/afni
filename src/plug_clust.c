#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Simple plugin to cluster data and return new dataset
************************************************************************/

char * CLUST_main( PLUGIN_interface * ) ;

static char helpstring[] =
  " Purpose: Apply the clustering algorithm to a functional dataset.\n"
  " Inputs:\n"
  " Dataset   = Functional dataset that must already be in memory\n"
  "               (not warp-on-demand) -- this is required.\n"
  " Params    = Radius controls the maximum distance between two nonzero\n"
  "               voxels for them to be considered neighbors in a cluster.\n"
  "             MinVol controls the minimum volume of a cluster that will\n"
  "               be accepted.\n"
  " Threshold = If the input dataset has a threshold sub-brick attached,\n"
  "               this option can be used to set its level.\n"
  " Output    = If this option is used, then a new dataset will be made\n"
  "               from the clusters.  In that case, a prefix for the\n"
  "               new dataset filename must be provided.  If this option\n"
  "               is not used, then the clustering results will overwrite\n"
  "               the existing dataset in memory and on disk."
;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "3D Cluster" , "Dataset Clustering" , helpstring ,
                                 PLUGIN_CALL_VIA_MENU , CLUST_main  ) ;

   PLUTO_add_hint( plint , "Dataset Clustering" ) ;

   /*-- first line of input: Dataset --*/

   PLUTO_add_option( plint , "Dataset" , "Dataset" , TRUE ) ;
   PLUTO_add_dataset(plint , "Function" ,
                                    ANAT_NONE_MASK , FUNC_ALL_MASK ,
                                    DIMEN_3D_MASK | BRICK_ALLREAL_MASK ) ;

   /*-- second line of input: Cluster Parameters --*/

   PLUTO_add_option( plint , "Params" , "Params" , TRUE ) ;
   PLUTO_add_number( plint , "Radius(mm)" , 0, 100,1 , 20,TRUE ) ;
   PLUTO_add_number( plint , "MinVol(ul)" , 0,1000,-1,100,TRUE ) ;

   /*-- third line of input: Threshold (optional) --*/

   PLUTO_add_option( plint , "Threshold" , "Threshold" , FALSE ) ;
   PLUTO_add_number( plint , "Cutoff"    , 0,1000,2 , 50,TRUE ) ;

   /*-- fourth line of input: Prefix for output dataset --*/

   PLUTO_add_option( plint , "Output" , "Output" , FALSE ) ;
   PLUTO_add_string( plint , "Prefix" , 0,NULL , 19 ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

char * CLUST_main( PLUGIN_interface * plint )
{
   char * tag , * new_prefix ;
   float rmm , vmul , thresh ;
   MCW_idcode * idc ;
   THD_3dim_dataset * dset , * new_dset ;
   int ival , ityp , nbytes , nvals ;
   EDIT_options edopt ;
   void * new_brick , * old_brick ;

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   if( plint == NULL )
      return "**********************\n"
             "CLUST_main: NULL input\n"
             "**********************"    ;

   tag = PLUTO_get_optiontag(plint) ;
   if( tag==NULL || strcmp(tag,"Dataset") != 0 )
      return "**********************************\n"
             "CLUST_main: bad Dataset option tag\n"
             "**********************************"    ;

   idc  = PLUTO_get_idcode(plint) ;
   dset = PLUTO_find_dset(idc) ;
   if( dset == NULL )
      return "*****************************\n"
             "CLUST_main: bad input dataset\n"
             "*****************************"   ;

   tag = PLUTO_get_optiontag(plint) ;
   if( tag==NULL || strcmp(tag,"Params") != 0 )
      return "*********************************\n"
             "CLUST_main: bad Params option tag\n"
             "*********************************"   ;

   rmm  = PLUTO_get_number(plint) ;
   vmul = PLUTO_get_number(plint) ;
   if( rmm <= 0 || vmul <= 0 )
      return "****************************\n"
             "CLUST_main: bad Params input\n"
             "****************************"   ;

   tag = PLUTO_peek_optiontag(plint) ;
   if( tag != NULL && strcmp(tag,"Threshold") == 0 ){
      PLUTO_next_option(plint) ;
      thresh = PLUTO_get_number(plint) ;
      if( thresh < 0.0 )
         return "*******************************\n"
                "CLUST_main: bad Threshold input\n"
                "*******************************"   ;

      if( thresh > 0.0 && DSET_THRESH_INDEX(dset) < 0 )
         return "**********************************************\n"
                "CLUST_main: Dataset has no threshold sub-brick\n"
                "**********************************************"  ;
   } else {
      thresh = 0.0 ;
   }

   tag = PLUTO_peek_optiontag(plint) ;
   if( tag != NULL && strcmp(tag,"Output") == 0 ){
      PLUTO_next_option(plint) ;
      new_prefix = PLUTO_get_string(plint) ;
      if( ! PLUTO_prefix_ok(new_prefix) )
         return "**********************\n"
                "CLUST_main: bad prefix\n"
                "**********************"   ;
   } else {
     new_prefix = NULL ;
   }

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   /*-- maybe, make a new dataset --*/

   if( new_prefix == NULL ){  /* no prefix => edit input in place */

      new_dset = dset ;
      DSET_load( dset ) ;     /* load into memory */

   } else {                   /* OK, make a copy first */
      new_dset = PLUTO_copy_dset( dset , new_prefix ) ;

      if( new_dset == NULL )
         return  "****************************************\n"
                 "CLUST_main: failed to copy input dataset\n"
                 "****************************************"  ;

      DSET_unload( dset ) ;  /* unload from memory */
   }

   /*-- set up for dataset editing --*/

   INIT_EDOPT( &edopt ) ;

   edopt.edit_clust = ECFLAG_SAME ;
   edopt.clust_rmm  = rmm ;
   edopt.clust_vmul = vmul ;

   if( thresh > 0.0 ) edopt.thresh = thresh ;

   /*-- edit the new dataset --*/

   EDIT_one_dataset( new_dset , &edopt ) ;

   /*-- if we made a completely new dataset, give it to AFNI --*/

   if( new_dset != dset ){
      ival = PLUTO_add_dset( plint , new_dset , DSET_ACTION_MAKE_CURRENT ) ;

      if( ival ){
        THD_delete_3dim_dataset( new_dset , False ) ;
        return "**********************************************\n"
               "CLUST_main: failure to add new dataset to AFNI\n"
               "**********************************************" ;
      }
   } else {
      DSET_write( new_dset ) ;    /* otherwise, re-write to disk */
      PLUTO_force_redisplay() ;  /* and force a redisplay of images */
   }

   /*-- done successfully!!! --*/

   return NULL ;
}
