#include "afni.h"

#ifndef ALLOW_PLUGINS
IPLUGIN_interface *  ICOR_init(char *lab)
{
  MCW_popup_message( THE_TOPSHELL ,
                     " \n"
                     " InstaCorr not available\n"
                     " since this copy of AFNI\n"
                     "  was compiled without\n"
                     "  support for plugins!\n " , MCW_USER_KILL ) ;
  return NULL ;
}
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

/*----------------- global data for doing the work -------------------*/

static ICOR_setup *iset = NULL ;

/*----------------- prototypes for internal routines -----------------*/

static char * ICOR_main( PLUGIN_interface * ) ;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

PLUGIN_interface * ICOR_init(char *lab)
{
   PLUGIN_interface *plint ;     /* will be the output of this routine */
   static char *yn[2] = { "No" , "Yes" } ;
   char sk[32] , sc[32] ;

   if( lab == NULL ) lab = "\0" ;

   /*---------------- set titles and call point ----------------*/

   sprintf(sk,"%sControl InstaCorr",lab) ;
   plint = PLUTO_new_interface( "InstaCorr" ,
                                sk ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU ,
                                (char *(*)())ICOR_main  ) ;

   sprintf(sk,"%sSetup+Keep",lab) ; sprintf(sc,"%sSetup+Close",lab) ;
   PLUTO_set_runlabels( plint , sk , sc ) ;

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
   PLUTO_add_string ( plint , "Auto"  , 2 , yn , 1 ) ;

   PLUTO_add_option    ( plint , "Global Orts" , "GlobalOrts" , FALSE ) ;
   PLUTO_add_timeseries( plint , "1D file" ) ;

#if 0
   PLUTO_add_option    ( plint , "Slice Orts" , "SliceOrts" , FALSE ) ;
   PLUTO_add_timeseries( plint , "1D file" ) ;
#endif

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
   char *tag ;
   float fbot=-1.0f , ftop=999999.9f ;
   MRI_IMAGE *gortim=NULL ;
   THD_3dim_dataset *dset=NULL , *mset=NULL ;
   int ignore=0 , mindex=0 , automask=0 ; float blur=0.0f ;

   /*--- loop over input lines ---*/

   while(1){
     tag = PLUTO_get_optiontag(plint) ;
     if( tag == NULL ) break ;

     /** TimeSeries **/

     if( strcmp(tag,"TimeSeries") == 0 ){
       MCW_idcode *idc ;
       idc  = PLUTO_get_idcode(plint) ;
       dset = PLUTO_find_dset(idc) ;
       if( dset == NULL ) ERROR_message("Can't find Time Series dataset") ;
       ignore = PLUTO_get_number(plint) ;
       blur   = PLUTO_get_number(plint) ;
       break ;
     }

     if( strcmp(tag,"Mask") == 0 ){
       MCW_idcode *idc ; char *am ;
       idc      = PLUTO_get_idcode(plint) ;
       mset     = PLUTO_find_dset(idc) ;
       mindex   = PLUTO_get_number(plint) ;
       am       = PLUTO_get_string(plint) ;
       automask = (am[0] == 'Y') ;
       if( !automask && mset == NULL )
         WARNING_message("No Masking selected?!") ;
       else if( mset != NULL && automask )
         WARNING_message("Automask disabled when Mask dataset is chosen") ;
       break ;
     }

     if( strcmp(tag,"GlobalOrts") == 0 ){
       gortim = PLUTO_get_timeseries(plint) ;
       if( gortim == NULL ) ERROR_message("Ignoring NULL 'Global Orts' time series") ;
       break ;
     }

     if( strcmp(tag,"Bandpass") == 0 ){
       fbot = PLUTO_get_number(plint) ;
       ftop = PLUTO_get_number(plint) ;
       if( fbot >= ftop ) ERROR_message("Ignoring disordered Bandpass frequencies") ;
       break ;
     }

     return "** ICOR_main: table corruption! **" ; /* should never happen! */
   }

   return NULL ;
}
#endif  /* ALLOW_PLUGINS */

/*------------------------------------------------------------------*/

void AFNI_icor_bbox_CB( Widget w, XtPointer cd, XtPointer cb)
{
   Three_D_View *im3d = (Three_D_View *) cd ;

ENTRY("AFNI_icor_bbox_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

INFO_message("AFNI_icor_bbox_CB %d",MCW_val_bbox(im3d->vwid->func->icor_bbox)) ;

   EXRETURN ;
}
