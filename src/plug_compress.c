#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to switch the .BRIK compression mode
************************************************************************/

/*--------------------- string to 'help' the user --------------------*/

static char helpstring[] =
  "Purpose: control the way AFNI compresses datasets.\n"
  "\n"
  "The input 'Mode' specifies how dataset .BRIK files will be\n"
  "compress when written to disk.  The options are\n"
  "\n"
  "  NONE     = no compression\n"
  "  GZIP     = use program 'gzip'\n"
  "  BZIP2    = use program 'bzip2'    [very slow; most compression]\n"
  "  COMPRESS = use program 'compress'\n"
  "\n"
  "Compressed datasets will save disk space.  The principal cost is\n"
  "the CPU time it takes to read and write compressed files."
  "\n"
  "As usual, after you make your choice, you must press one of the\n"
  "'Run' buttons for the plugin to be executed.\n"
  "Author -- RW Cox"
;

/*--------------------- strings for output format --------------------*/

static char * comp_strings[] = {
 "NONE" , "GZIP" , "BZIP2" , "COMPRESS"
} ;

#define NUM_COMP_STRINGS (sizeof(comp_strings)/sizeof(char *))

/*----------------- prototypes for internal routines -----------------*/

char * COMP_main( PLUGIN_interface * ) ;  /* the entry point */

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

   plint = PLUTO_new_interface( "BRIK Compressor" ,
                                "Control .BRIK compression" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , COMP_main  ) ;

   PLUTO_add_hint( plint , "Control .BRIK compression" ) ;

   PLUTO_set_sequence( plint , "A:afnicontrol:dset" ) ;

   /*---------- 2nd line: other inputs ----------*/

   PLUTO_add_option( plint ,
                     "Input" ,  /* label at left of input line */
                     "Input" ,  /* tag to return to plugin */
                     TRUE       /* is this mandatory? */
                   ) ;

   meth = THD_get_write_compression()+1 ;
   if( meth < 0 || meth >= NUM_COMP_STRINGS ) meth = 0 ;

   PLUTO_add_string( plint ,
                     "Mode" ,          /* label next to chooser button */
                     NUM_COMP_STRINGS , /* number of strings to choose among */
                     comp_strings ,     /* list of strings to choose among */
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

char * COMP_main( PLUGIN_interface * plint )
{
   char * str ;
   int meth ;

   PLUTO_next_option(plint) ;

   str  = PLUTO_get_string(plint) ;
   meth = PLUTO_string_index(str,NUM_COMP_STRINGS,comp_strings) - 1 ;
   if( meth < 0 || meth > COMPRESS_LASTCODE ) meth = COMPRESS_NONE ;
   THD_set_write_compression(meth) ;
   return NULL ;
}
