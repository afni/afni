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
  Simple plugin to cluster data and return new dataset
  Author:   RW Cox

  Mod:      Added Erode/Dilate option to sever narrow connecting path 
            between clusters, by first eroding the outer layer of voxels, 
            then restoring voxels near the main body of the cluster.
	    Also, added 'Type' option to control processing of data 
	    inside clusters.
  Author:   B. Douglas Ward
  Date:     19 June 1998

************************************************************************/

static char * CLUST_main( PLUGIN_interface * ) ;

static char helpstring[] =
  " Purpose: Apply the clustering algorithm to a functional dataset.\n"
  " Inputs:\n"
  " Dataset     = Functional dataset that must already be in memory\n"
  "                 (not warp-on-demand) -- this is required.\n"
  " Params      = Type determines method for setting voxel intensities \n"
  "                 within a cluster. \n"
  "               Radius controls the maximum distance between two nonzero\n"
  "                 voxels for them to be considered neighbors in a cluster.\n"
  "               MinVol controls the minimum volume of a cluster that will\n"
  "                 be accepted.\n"
  " Erode/Dilate  = Sever narrow connecting paths between clusters. \n"
  "   % Voxels        Min. % of active 'neighbors' for a voxel to survive. \n"
  "   Dilate          Restore voxels near main body of cluster. \n"
  " Threshold     = If the input dataset has a threshold sub-brick attached,\n"
  "                   this option can be used to set its level.\n"
  " Output        = If this option is used, then a new dataset will be made\n"
  "                  from the clusters.  In that case, a prefix for the\n"
  "                  new dataset filename must be provided.  If this option\n"
  "                  is not used, then the clustering results will overwrite \n"
  "                  the existing dataset in memory and on disk.\n"
  "Author -- RW Cox"
;

/***********************************************************************
   Set up the interface to the user
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;
 
   /*----- plugin option labels -----*/                      /* 19 June 1998 */
   char * boolean_types[2] = {"False", "True"};
   char * cluster_types[7] = {"Keep", "Mean", "Max", "AMax", "SMax", "Size",
			      "Order"};                      

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "3D Cluster" , "Dataset Clustering" , helpstring ,
                                 PLUGIN_CALL_VIA_MENU , CLUST_main  ) ;

   PLUTO_add_hint( plint , "Dataset Clustering" ) ;

   PLUTO_set_sequence( plint , "A:afniinfo:dsetcluster" ) ;

   /*-- first line of input: Dataset --*/

   PLUTO_add_option( plint , "Dataset" , "Dataset" , TRUE ) ;
   PLUTO_add_dataset(plint , "Function" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_3D_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_hint( plint , "Choose input dataset" ) ;

   /*-- second line of input: Cluster Parameters --*/

   PLUTO_add_option( plint , "Params" , "Params" , TRUE ) ;
   PLUTO_add_hint( plint , "Find and reject small clusters" ) ;
   PLUTO_add_string (plint, "Type", 7, cluster_types, 0);    /* 19 June 1998 */
   PLUTO_add_hint( plint , "How to process data inside clusters" ) ;
   PLUTO_add_number( plint , "Radius(mm)" , 0, 100,1 , 20,TRUE ) ;
   PLUTO_add_hint( plint , "Max distance between 'neighbors'" ) ;
   PLUTO_add_number( plint , "MinVol(ul)" , 0,1000,-1,100,TRUE ) ;
   PLUTO_add_hint( plint , "Min size for cluster to survive" ) ;


   /*---- 3rd line of input: Erosion/Dilation option ----*/  /* 19 June 1998 */
   PLUTO_add_option (plint, "Erode/Dilate", "Erode/Dilate", FALSE);
   PLUTO_add_hint (plint , "Sever narrow connecting paths between clusters");
   PLUTO_add_number (plint, "% Voxels", 0, 100, 0, 50, TRUE);
   PLUTO_add_hint (plint , 
		   "Min % of active 'neighbors' for a voxel to survive");
   PLUTO_add_string (plint, "Dilate?",  2, boolean_types, 0);
   PLUTO_add_hint (plint , "Restore voxels near main body of cluster");


   /*-- fourth line of input: Threshold (optional) --*/

   PLUTO_add_option( plint , "Threshold" , "Threshold" , FALSE ) ;
   PLUTO_add_hint( plint , "Zero out if threshold brick too small" ) ;
   PLUTO_add_number( plint , "Cutoff"    , 0,1000,2 , 50,TRUE ) ;
   PLUTO_add_hint( plint , "Threshold values < this => 0" ) ;

   /*-- fifth line of input: Prefix for output dataset --*/

   PLUTO_add_option( plint , "Output" , "Output" , FALSE ) ;
   PLUTO_add_string( plint , "Prefix" , 0,NULL , 19 ) ;
   PLUTO_add_hint( plint , "Name output dataset" ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

static char * CLUST_main( PLUGIN_interface * plint )
{
   char * tag , * new_prefix ;
   float rmm , vmul , thresh ;
   MCW_idcode * idc ;
   THD_3dim_dataset * dset , * new_dset ;
   int ival , ityp , nbytes , nvals ;
   EDIT_options edopt ;
   void * new_brick , * old_brick ;

   int clust_type;       /* input cluster type option */     /* 19 June 1998 */
   char * str;           /* input string */
   float pv;             /* pv % voxels within rmm must be active */
   int dilate;           /* boolean for perform dilation of cluster voxels */


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

   str = PLUTO_get_string(plint);                            /* 19 June 1998 */
   if (strcmp(str,"Keep") == 0)         clust_type = ECFLAG_SAME;
   else  if (strcmp(str,"Mean") == 0)   clust_type = ECFLAG_MEAN;
   else  if (strcmp(str,"Max") == 0)    clust_type = ECFLAG_MAX;
   else  if (strcmp(str,"AMax") == 0)   clust_type = ECFLAG_AMAX;
   else  if (strcmp(str,"SMax") == 0)   clust_type = ECFLAG_SMAX;
   else	 if (strcmp(str,"Size") == 0)   clust_type = ECFLAG_SIZE;
   else  if (strcmp(str,"Order") == 0)  clust_type = ECFLAG_ORDER;
   else  if (strcmp(str,"Depth") == 0)  clust_type = ECFLAG_DEPTH;
   else
     return 
       "**********************************\n"
       "CLUST_main: Illegal Cluster option\n"
       "**********************************";

   rmm  = PLUTO_get_number(plint) ;
   vmul = PLUTO_get_number(plint) ;
   if( rmm <= 0 || vmul <= 0 )
      return "****************************\n"
             "CLUST_main: bad Params input\n"
             "****************************"   ;


                                                             /* 19 June 1998 */
   tag = PLUTO_peek_optiontag(plint) ;
   if( tag != NULL && strcmp(tag,"Erode/Dilate") == 0 )
     {
       PLUTO_next_option(plint) ;
       pv  = PLUTO_get_number(plint);
       if ((pv > 0.0) && (rmm <= 0.0))
	 return 
	   "*******************************************************\n"
	   "CLUST_main: Erode/Dilate requires use of Cluster option\n"
	   "*******************************************************";
       else
	 pv  = pv / 100.0;
       
       str = PLUTO_get_string(plint);
       if (strcmp (str, "True") == 0)
	 {
	   if (pv <= 0.0)
	     return 
	       "***********************************************\n"
	       "CLUST_main: Dilate requires use of Erode option\n"
	       "***********************************************";
	   else
	     dilate = 1;
	 }
       else
	 dilate = 0;       
     }
   else
     {
       pv = 0.0;
       dilate = 0;
     }
   
   
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

   edopt.edit_clust = clust_type;                            /* 19 June 1998 */

   edopt.clust_rmm  = rmm ;
   edopt.clust_vmul = vmul ;

   edopt.erode_pv = pv;
   edopt.dilate = dilate;

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
      DSET_overwrite( new_dset ) ;    /* otherwise, re-write to disk */
      PLUTO_force_redisplay() ;  /* and force a redisplay of images */
   }

   /*-- done successfully!!! --*/

   return NULL ;
}
