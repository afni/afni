#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Simple plugin to copy a dataset and make a new one with zeropadding
************************************************************************/

char * ZPAD_main( PLUGIN_interface * ) ;

static char helpstring[] =
   " Purpose: Creating a zero-padded copy of a dataset.\n"
   " Inputs:\n"
   " Dataset = A dataset in the current session that exists in memory\n"
   "             (not warp-on-demand).\n"
   " Prefix  = Filename prefix to be used for the output dataset.\n"
   " Type    = Lets you change the 'type' of the output dataset, for\n"
   "             example from anat to func.\n"
   " Padding = I, S, A, P, L, R = number of planes to add (or subtract)\n"
   "           at the Inferior, Superior, Anterior, Posterior, Left,\n"
   "           and Right edges, respectively.\n"
   "\n"
   "Author -- RWCox - Oct 2000"
;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "Dset Zeropad" ,
                                "Make a Zero-Padded Copy of a Dataset" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , ZPAD_main  ) ;

   PLUTO_add_hint( plint , "Copy and Zero-Pad a Dataset" ) ;

   PLUTO_set_sequence( plint , "A:newdset:copy" ) ;

   /*-- first line of input: Dataset --*/

   PLUTO_add_option( plint , "Input" , "Input" , TRUE ) ;
   PLUTO_add_dataset(plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_ALL_MASK | BRICK_ALLTYPE_MASK ) ;

   /*-- second line of input: Prefix for output dataset --*/

   PLUTO_add_option( plint , "Output" , "Output" , TRUE ) ;
   PLUTO_add_string( plint , "Prefix" , 0,NULL , 19 ) ;

   /*-- 3rd line of input: padding --*/

   PLUTO_add_option( plint , "Padding" , "Padding" , TRUE ) ;
   PLUTO_add_number( plint , "I" , -19 , 19 , 0 , 0 , FALSE ) ;
   PLUTO_add_number( plint , "S" , -19 , 19 , 0 , 0 , FALSE ) ;
   PLUTO_add_number( plint , "A" , -19 , 19 , 0 , 0 , FALSE ) ;
   PLUTO_add_number( plint , "P" , -19 , 19 , 0 , 0 , FALSE ) ;
   PLUTO_add_number( plint , "L" , -19 , 19 , 0 , 0 , FALSE ) ;
   PLUTO_add_number( plint , "R" , -19 , 19 , 0 , 0 , FALSE ) ;

   /*-- 4th line of input: Type option --*/

   PLUTO_add_option( plint , "Dataset" , "Dataset" , FALSE ) ;
   PLUTO_add_string( plint , "Type" , NUM_DSET_TYPES,DSET_prefixstr , 0 ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

char * ZPAD_main( PLUGIN_interface * plint )
{
   char * tag , * new_prefix , * cpt ;
   MCW_idcode * idc ;
   THD_3dim_dataset * dset , * new_dset ;
   int ftyp=-1 , dtyp=-1 , ival ;
   int add_I, add_S, add_A, add_P, add_L, add_R;

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   if( plint == NULL )
      return "**********************\n"
             "ZPAD_main:  NULL input\n"
             "**********************"  ;

   PLUTO_next_option(plint) ;
   idc  = PLUTO_get_idcode(plint) ;
   dset = PLUTO_find_dset(idc) ;
   if( dset == NULL )
      return "*****************************\n"
             "ZPAD_main:  bad input dataset\n"
             "*****************************"  ;

   dtyp = dset->type ;

   PLUTO_next_option(plint) ;
   new_prefix = PLUTO_get_string(plint) ;
   if( ! PLUTO_prefix_ok(new_prefix) )
      return "**********************\n"
             "ZPAD_main:  bad prefix\n"
             "**********************"  ;

   PLUTO_next_option(plint) ;
   add_I = PLUTO_get_number(plint) ;
   add_S = PLUTO_get_number(plint) ;
   add_A = PLUTO_get_number(plint) ;
   add_P = PLUTO_get_number(plint) ;
   add_L = PLUTO_get_number(plint) ;
   add_R = PLUTO_get_number(plint) ;
   if( add_I==0 && add_S==0 && add_P==0 && add_A==0 && add_L==0 && add_R==0 )
      return "***********************\n"
             "ZPAD_main: no padding?!\n"
             "***********************"  ;

   tag = PLUTO_get_optiontag(plint) ;
   while( tag != NULL ){

      if( strcmp(tag,"Dataset") == 0 ){
         cpt  = PLUTO_get_string(plint) ;
         ftyp = PLUTO_string_index( cpt , NUM_DSET_TYPES,DSET_prefixstr ) ;
         if( ftyp >= 0 ){
            if( ftyp <= LAST_FUNC_TYPE ){
               dtyp = HEAD_FUNC_TYPE ;
            } else {
               ftyp -= (LAST_FUNC_TYPE+1) ;  /* 14 Jul 1998 */
               dtyp  = HEAD_ANAT_TYPE ;
            }
         }
      }

      tag = PLUTO_get_optiontag(plint) ;
   }

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   /*-- make a new dataset --*/

   new_dset = THD_zeropad( dset ,
                           add_I, add_S, add_A, add_P, add_L, add_R,
                           new_prefix ) ;

   if( new_dset == NULL )
      return  "****************************************\n"
              "ZPAD_main:  failed to create new dataset\n"
              "****************************************"  ;

   DSET_unload( dset ) ;  /* unload old one from memory */

   /*--- modify dataset, if desired ---*/

   if( ftyp >= 0 ) EDIT_dset_items( new_dset ,
                                       ADN_type      , dtyp ,
                                       ADN_func_type , ftyp ,
                                    ADN_none ) ;

   ival = PLUTO_add_dset( plint , new_dset , DSET_ACTION_MAKE_CURRENT ) ;

   if( ival ){
      DSET_delete(new_dset) ;
      return "**********************************************\n"
             "ZPAD_main:  failure to add new dataset to AFNI\n"
             "**********************************************" ;
   }

   /*-- done successfully!!! --*/

   return NULL ;
}
