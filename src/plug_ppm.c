#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/************************************************************************/

char * IM_main( PLUGIN_interface * ) ;  /* the entry point */

/*---------------------------- global data ---------------------------*/

static PLUGIN_interface * global_plint = NULL ;

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

   if( ncall > 0 ) return NULL ;  /* only one interface */

   plint = PLUTO_new_interface( "Images" ,
                                "Images" ,
                                NULL ,
                                PLUGIN_CALL_VIA_MENU , IM_main  ) ;

   global_plint = plint ;  /* make global copy */

   PLUTO_add_option( plint ,
                     "Input" ,  /* label at left of input line */
                     "Input" ,  /* tag to return to plugin */
                     TRUE       /* is this mandatory? */
                   ) ;

   PLUTO_add_string( plint , "Filename" , 0 , NULL , 19 ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

char * IM_main( PLUGIN_interface * plint )
{
   char * str ;
   MRI_IMAGE * im ;

   PLUTO_next_option(plint) ;

   str  = PLUTO_get_string(plint) ;
   im   = mri_read_ppm(str) ;
   if( im == NULL ) return "Can't open\nimage file" ;

   (void) PLUTO_popup_image(NULL,im) ;
   mri_free(im) ;
   return NULL ;
}
