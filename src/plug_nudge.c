#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to nudge a dataset in x,y,z
************************************************************************/

/*--------------------- string to 'help' the user --------------------*/

static char helpstring[] =
  "Purpose: Nudge a dataset in the (x,y,z) directions\n"
  "\n"
  "Input:  Dataset = dataset to play with\n"
  "Delta:  Dx = mm to move in x-direction }  measured from \n"
  "        Dy = mm to move in y-direction }- when dataset was\n"
  "        Dz = mm to move in z-direction }  loaded into plugin\n"
  "Output: Write? = Yes means to write out\n"
  "                 .HEAD file with these changes\n"
  "\n"
  "Suggestions:\n"
  "-----------\n"
  "The purpose of this plugin is to help you align two volumes\n"
  "when the correct centering offsets were not entered into to3d.\n"
  "\n"
  "The directions of (x,y,z) in the current datasets can be determined\n"
  "using the 'Datamode->Misc->Info' buttons.\n"
  "\n"
  "If you have two anatomical datasets, you can make a functional\n"
  "duplicate of one (using 3ddup), so that you can overlay it\n"
  "on the other dataset.  Set the functional dataset to be the one to\n"
  "be nudged by this plugin, and turn 'See Function' on.  When you\n"
  "alter Dx, Dy, and/or Dz, then press 'Run+Keep', the functional\n"
  "overlay will be moved accordingly, relative to the offsets\n"
  "currently stored in the dataset's HEAD file.\n"
  "\n"
  "Another way to use this is to toggle between two anatomical\n"
  "datasets using the 'Switch Anatomy' control.  Note that if\n"
  "you are nudging an anatomical underlay, it will not appear\n"
  "to move when you press 'Run+Keep', since the anatomical\n"
  "dataset bounds define the image viewing window.  In this\n"
  "case, you will need to switch to the other dataset and\n"
  "back to see the effect of the change.  Or, if you have\n"
  "both datasets open in two different AFNI controller windows,\n"
  "you can use 'Jumpback' on the image window popup menu.\n"
  "\n"
  "Instead of using the 'Write?' option, I recommend that you\n"
  "simply note the Dx, Dy, and Dz values needed, and apply\n"
  "them outside of AFNI using a command like\n"
  "  3drefit -dxorigin Dx -dyorigin Dy -dzorigin Dz name+orig\n"
  "\n"
  "Author -- RW Cox - 02 Mar 2000"
;

static char * yesno[2] = {"No", "Yes"} ;

/*----------------- prototypes for internal routines -----------------*/

char * NUD_main( PLUGIN_interface * ) ;  /* the entry point */

/***********************************************************************
   Set up the interface to the user:
    1) Create a new interface using "PLUTO_new_interface";

    2) For each line of inputs, create the line with "PLUTO_add_option"
         (this line of inputs can be optional or mandatory);

    3) For each item on the line, create the item with
        "PLUTO_add_dataset" for a dataset chooser,
        "PLUTO_add_string"  for a string chooser,
        "PLUTO_add_number"  for a number chooser.
************************************************************************/

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */
   int meth ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "Nudge xyz" ,
                                "Move dataset along axes" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , NUD_main  ) ;

   PLUTO_add_hint( plint , "Move dataset along axes" ) ;

   PLUTO_set_sequence( plint , "A:olddset:nudger" ) ;

   /*--------------------*/

   PLUTO_add_option( plint , "Input" , "Input" , TRUE ) ;

   PLUTO_add_dataset( plint, "Dataset",
                      ANAT_ALL_MASK, FUNC_ALL_MASK,
                      DIMEN_ALL_MASK | BRICK_ALLREAL_MASK );

   /*--------------------*/

   PLUTO_add_option( plint , "Delta" , "Delta" , TRUE ) ;

   PLUTO_add_number (plint, "Dx", -99999, 99999, 0, 0, TRUE);
   PLUTO_add_number (plint, "Dy", -99999, 99999, 0, 0, TRUE);
   PLUTO_add_number (plint, "Dz", -99999, 99999, 0, 0, TRUE);

   /*--------------------*/

   PLUTO_add_option( plint , "Output" , "Output" , FALSE ) ;
   PLUTO_add_string( plint , "Write?" , 2 , yesno , 0 ) ;

   /*--------- done with interface setup ---------*/

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

char * NUD_main( PLUGIN_interface * plint )
{
   char * tag ;
   MCW_idcode * idc ;
   THD_3dim_dataset * dset ;
   THD_fvec3 xyzorg , txyz ;
   float dx,dy,dz ;
   char buf[256],ox[16],oy[16],oz[16],nx[16],ny[16],nz[16] ;
   int wrote=0 ;

   static THD_3dim_dataset * old_dset = NULL ;  /* saved from last time */
   static THD_fvec3 old_xyzorg ;

   /* bad input */

   if( plint == NULL )
      return "**********************\n"
             "NUD_main:  NULL input\n"
             "**********************"  ;

   /* get dataset */

   PLUTO_next_option(plint) ;
   idc  = PLUTO_get_idcode(plint) ;
   dset = PLUTO_find_dset(idc) ;
   if( dset == NULL )
      return "*****************************\n"
             "NUD_main:  bad input dataset\n"
             "*****************************"  ;

   if( dset != old_dset ){
      LOAD_FVEC3( old_xyzorg ,
                  dset->daxes->xxorg, dset->daxes->yyorg, dset->daxes->zzorg ) ;
      old_dset = dset ;
   }

   PLUTO_next_option(plint) ;
   dx = PLUTO_get_number(plint) ;
   dy = PLUTO_get_number(plint) ;
   dz = PLUTO_get_number(plint) ;

   LOAD_FVEC3( txyz , dx,dy,dz ) ;
   xyzorg = ADD_FVEC3(txyz,old_xyzorg) ;

   EDIT_dset_items( dset , ADN_xyzorg , xyzorg , ADN_none ) ;
   PLUTO_force_redisplay() ;

   tag = PLUTO_get_optiontag(plint) ;
   if( tag != NULL && strcmp(tag,"Output") == 0 ){
      tag = PLUTO_get_string(plint) ;
      if( tag != NULL && strcmp(tag,"Yes") == 0 ){

         wrote = 1 ;
         AV_fval_to_char( old_xyzorg.xyz[0] , ox ) ;
         AV_fval_to_char( old_xyzorg.xyz[1] , oy ) ;
         AV_fval_to_char( old_xyzorg.xyz[2] , oz ) ;
         AV_fval_to_char(     xyzorg.xyz[0] , nx ) ;
         AV_fval_to_char(     xyzorg.xyz[1] , ny ) ;
         AV_fval_to_char(     xyzorg.xyz[2] , nz ) ;
         sprintf(buf," \n"
                     "Old xorigin=%s  yorigin=%s  zorigin=%s\n"
                     "New xorigin=%s  yorigin=%s  zorigin=%s\n"
                     "Rewriting %s\n" ,
                 ox,oy,oz , nx,ny,nz , DSET_HEADNAME(dset) ) ;
         PLUTO_popup_message( plint , buf ) ;

         DSET_write_header(dset) ;
         LOAD_FVEC3( old_xyzorg ,
                     dset->daxes->xxorg,dset->daxes->yyorg,dset->daxes->zzorg ) ;
      }
   }

   if( !wrote ){
         AV_fval_to_char( old_xyzorg.xyz[0] , ox ) ;
         AV_fval_to_char( old_xyzorg.xyz[1] , oy ) ;
         AV_fval_to_char( old_xyzorg.xyz[2] , oz ) ;
         AV_fval_to_char(     xyzorg.xyz[0] , nx ) ;
         AV_fval_to_char(     xyzorg.xyz[1] , ny ) ;
         AV_fval_to_char(     xyzorg.xyz[2] , nz ) ;
         sprintf(buf," \n"
                     "Old xorigin=%s  yorigin=%s  zorigin=%s\n"
                     "New xorigin=%s  yorigin=%s  zorigin=%s\n"
                     "HEAD file not written\n" ,
                 ox,oy,oz , nx,ny,nz ) ;
         PLUTO_popup_transient( plint , buf ) ;
   }

   return NULL ;
}
