#include "afni.h"

#ifndef ALLOW_PLUGINS
void ICOR_init(void){}
#else

/***********************************************************************
  Pseudo-plugin to setup InstaCorr operations
************************************************************************/

/*--------------------- string to 'help' the user --------------------*/

static char helpstring[] =
  "Purpose: control AFNI InstaCorr operations\n"
  "\n"
  "Author -- RW Cox -- Apr 2009"
;

/*----------------- prototypes for internal routines -----------------*/

static char * ICOR_main( PLUGIN_interface * ) ;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

PLUGIN_interface * ICOR_init(void)
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */
   int ii , num , ll ;
   char str[16] ;
   MCW_function_list * rlist ;

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "InstaCorr" ,
                                "Control InstaCorr" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU ,
                                (char *(*)())ICOR_main  ) ;

   PLUTO_add_hint( plint , "Control InstaCorr" ) ;

   PLUTO_set_runlabels( plint , "Set+Keep" , "Set+Close" ) ;

   /*--------- make interface lines -----------*/

   PLUTO_add_option ( plint , "Time Series" , "TimeSeries" , TRUE ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                      ANAT_ALL_MASK , FUNC_ALL_MASK , DIMEN_4D_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number ( plint , "Ignore" , 0,50,0,0,FALSE ) ;
   PLUTO_add_number ( plint , "Blur"   , 0,99,1,0,TRUE  ) ;

   PLUTO_add_option ( plint , "Mask" , "Mask" , FALSE ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                      ANAT_ALL_MASK , FUNC_ALL_MASK , DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number ( plint , "Index" , 0,99999,0,0,TRUE ) ;

   PLUTO_add_option    ( plint , "Global Ort" , "GlobalOrts" , FALSE ) ;
   PLUTO_add_timeseries( plint , "1D file" ) ;

   PLUTO_add_option    ( plint , "Slice Orts" , "SliceOrts" , FALSE ) ;
   PLUTO_add_timeseries( plint , "1D file" ) ;

   PLUTO_add_option( plint , "Bandpass(Hz)" , "Bandpass" , FALSE ) ;
   PLUTO_add_number( plint , "Lower" , 0,1000,3, 10 , TRUE ) ;
   PLUTO_add_number( plint , "Upper" , 0,1000,3,100 , TRUE ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

static char * ICOR_main( PLUGIN_interface *plint )
{

   INFO_message("ICOR_main!") ;
   return NULL ;
}
#endif
