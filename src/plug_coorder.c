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
  Plugin to switch the coordinate display order
************************************************************************/

/*--------------------- string to 'help' the user --------------------*/

static char helpstring[] =
  "Purpose: control the AFNI display of coordinates.\n"
  "\n"
  "The input 'Order' specifies which anatomical directions are to\n"
  "be displayed in the AFNI control window as the -x, -y, and -z\n"
  "axes, respectively.\n"
  "\n"
  "For example, the 'Dicom' order is equivalent to 'RAI', which\n"
  "means that\n"
  "  -x = Right      [and so +x = Left     ]\n"
  "  -y = Anterior   [    so +y = Posterior]\n"
  "  -z = Inferior   [    so +z = Superior ]\n"
  "\n"
  "The 'flipped' order is 'LPI', which is used in many journals.\n"
  "See the output of 'afni -help' for more information.\n"
  "\n"
  "As usual, after you make your choice, you must press one of the\n"
  "'Run' buttons for the plugin to be executed.\n"
  "Author -- RW Cox"
;

/*--------------------- strings for output format --------------------*/

static char * cord_strings[] = {
 "Dicom" , "Flipped" ,
  "RAI" , "RAS" , "RPI" , "RPS" , "RIA" , "RIP" , "RSA" , "RSP" ,
  "LAI" , "LAS" , "LPI" , "LPS" , "LIA" , "LIP" , "LSA" , "LSP" ,
  "AIR" , "ASR" , "PIR" , "PSR" , "IAR" , "IPR" , "SAR" , "SPR" ,
  "AIL" , "ASL" , "PIL" , "PSL" , "IAL" , "IPL" , "SAL" , "SPL" ,
  "IRA" , "SRA" , "IRP" , "SRP" , "ARI" , "PRI" , "ARS" , "PRS" ,
  "ILA" , "SLA" , "ILP" , "SLP" , "ALI" , "PLI" , "ALS" , "PLS"
} ;

#define NUM_CORD_STRINGS (sizeof(cord_strings)/sizeof(char *))

/*----------------- prototypes for internal routines -----------------*/

static char * CORD_main( PLUGIN_interface * ) ;  /* the entry point */

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

   plint = PLUTO_new_interface( "Coord Order" ,
                                "Coordinate Order Display" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , CORD_main  ) ;

   PLUTO_add_hint( plint , "Coordinate Order Display" ) ;

   PLUTO_set_sequence( plint , "A:afnicontrol:display" ) ;

   /*---------- 2nd line: other inputs ----------*/

   PLUTO_add_option( plint ,
                     "Input" ,  /* label at left of input line */
                     "Input" ,  /* tag to return to plugin */
                     TRUE       /* is this mandatory? */
                   ) ;

   meth = PLUTO_string_index( GLOBAL_library.cord.orcode ,
                              NUM_CORD_STRINGS ,
                              cord_strings ) ;
   if( meth < 0 || meth >= NUM_CORD_STRINGS ) meth = 0 ;

   PLUTO_add_string( plint ,
                     "Order" ,          /* label next to chooser button */
                     NUM_CORD_STRINGS , /* number of strings to choose among */
                     cord_strings ,     /* list of strings to choose among */
                     meth               /* index of default string */
                   ) ;

   /*--------- done with interface setup ---------*/

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

static char * CORD_main( PLUGIN_interface * plint )
{
   char * str ;                 /* strings from user */

   /*--------- go to next input line ---------*/

   PLUTO_next_option(plint) ;

   str = PLUTO_get_string(plint) ;      /* get string item (the method) */

   MCW_strncpy(GLOBAL_argopt.orient_code,str,4) ;
   THD_coorder_fill( GLOBAL_argopt.orient_code , &GLOBAL_library.cord ) ;
   PLUTO_force_redisplay() ;
   return NULL ;
}
