#include "afni.h"

/*******************************************************************
  Functions to drive AFNI user-interface stuff from plugouts, etc.
  These routines take as input strings, and then call the
  appropriate callbacks to simulate what happens when the user
  presses various buttons.
********************************************************************/

/*-----------------------------------------------------------------
  Drive AFNI in various (incomplete) ways.
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

typedef int dfunc(char *) ;

typedef struct { char *nam; dfunc *fun; } AFNI_driver_pair ;

static AFNI_driver_pair dpair[] = {
 { "RESCAN_THIS"      , AFNI_drive_rescan_controller } ,
 { "SET_SESSION"      , AFNI_drive_switch_session    } ,
 { "SET_ANATOMY"      , AFNI_drive_switch_anatomy    } ,
 { "SET_FUNCTION"     , AFNI_drive_switch_function   } ,
 { "SWITCH_SESSION"   , AFNI_drive_switch_session    } ,
 { "SWITCH_ANATOMY"   , AFNI_drive_switch_anatomy    } ,
 { "SWITCH_FUNCTION"  , AFNI_drive_switch_function   } ,
 { "OPEN_WINDOW"      , AFNI_drive_open_window       } ,
 { "QUIT"             , AFNI_drive_quit              } ,

 { NULL , NULL } } ;

int AFNI_driver( char *cmd )
{
   int clen , rval , ii , dd , dlen ;

ENTRY("AFNI_driver") ;

   if( cmd == NULL || *cmd == '\0' ) RETURN(-1) ;

   clen = strlen(cmd) ;

   /* skip blanks */

   for( ii=0 ; ii < clen ; ii++ )
      if( !isblank(cmd[ii]) ) break ;

   if( ii == clen ) RETURN(-1) ;

   cmd += ii ; clen = strlen(cmd) ;

   /* scan for command */

   for( dd=0 ; dpair[dd].nam != NULL ; dd++ ){

      dlen = strlen(dpair[dd].nam) ;
      if( clen >= dlen                         &&
          strncmp(cmd,dpair[dd].nam,dlen) == 0   ){  /* found it */

         for( ii=dlen ; ii < clen ; ii++ )      /* skip blanks */
            if( !isblank(cmd[ii]) ) break ;

         rval = dpair[dd].fun( cmd+ii ) ;
         RETURN(rval) ;
      }
   }

   RETURN(-1) ;
}

/*-----------------------------------------------------------------
  Convert a controller code ("A", etc.) to an index.
  * Returns -1 if the index is illegal.
  * Note that a legal index may not have an active controller.
  * Controller #i is pointed to by GLOBAL_library.controllers[i],
    for i=0..MAX_CONTROLLERS-1; cf. AFNI_rescan_controller().
-------------------------------------------------------------------*/

int AFNI_controller_code_to_index( char *code )
{
   int ic ;
ENTRY("AFNI_controller_code_to_index") ;
   if( code == NULL ) RETURN(-1) ;
   ic = *code - 'A' ;
   if( ic < 0 || ic >= MAX_CONTROLLERS ) ic = -1 ;
   RETURN(ic) ;
}

/*-----------------------------------------------------------------
  Rescan a controller's session for new datasets.
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

int AFNI_drive_rescan_controller( char *code )
{
   int ic ;
   Three_D_View *im3d ;

ENTRY("AFNI_rescan_controller") ;

   ic = AFNI_controller_code_to_index( code ) ;
   if( ic < 0 ) ic = 0 ;                       /* default = A */

   im3d = GLOBAL_library.controllers[ic] ;

   if( IM3D_OPEN(im3d) )
      AFNI_rescan_CB( NULL , (XtPointer) im3d , NULL ) ;
   else
      RETURN(-1) ;

   RETURN(0) ;
}

/*-----------------------------------------------------------------
  Rescan a controller's session for new datasets.
  Input is of the form "A.directoryname".
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

int AFNI_drive_switch_session( char *cmd )
{
   int ic , dadd=2 ;
   Three_D_View *im3d ;
   char dname[THD_MAX_NAME] ;
   MCW_choose_cbs cbs ;

ENTRY("AFNI_switch_session") ;

   if( cmd == NULL || strlen(cmd) < 3 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   /* get session name, truncate trailing blanks */

   MCW_strncpy( dname , cmd+dadd , THD_MAX_NAME ) ;
   for( ic=strlen(dname)-1 ; ic >= 0 ; ic-- )
      if( isblank(dname[ic]) || iscntrl(dname[ic]) ) dname[ic] = '\0' ;
      else break ;

   if( strlen(dname) == 0 ) RETURN(-1) ;

   /* find session name */

   for( ic=0 ; ic < GLOBAL_library.sslist->num_sess ; ic++ )
      if( strstr(GLOBAL_library.sslist->ssar[ic]->sessname,dname) != NULL ) break;

   if( ic == GLOBAL_library.sslist->num_sess ) RETURN(-1) ;

   /* do the switcheroo */

   cbs.ival = ic ;

   AFNI_finalize_dataset_CB( im3d->vwid->view->choose_sess_pb ,
                             (XtPointer) im3d ,  &cbs          ) ;

   RETURN(0) ;
}

/*----------------------------------------------------------------------
  Set the anatomical dataset.  Input is of the form "A.prefix".
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

int AFNI_drive_switch_anatomy( char *cmd )
{
   int ic , dadd=2 ;
   Three_D_View *im3d ;
   char dname[THD_MAX_NAME] ;
   MCW_choose_cbs cbs ;
   THD_slist_find slf ;

ENTRY("AFNI_switch_anatomy") ;

   if( cmd == NULL || strlen(cmd) < 3 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   /* get dataset name, truncate trailing blanks */

   MCW_strncpy( dname , cmd+dadd , THD_MAX_NAME ) ;
   for( ic=strlen(dname)-1 ; ic >= 0 ; ic-- )
      if( isblank(dname[ic]) || iscntrl(dname[ic]) ) dname[ic] = '\0' ;
      else break ;

   if( strlen(dname) == 0 ) RETURN(-1) ;

   /* find this dataset in current session of this controller */

   slf = THD_dset_in_session( FIND_PREFIX , dname , im3d->ss_now ) ;

   if( slf.anat_index < 0 ) RETURN(-1) ;

   cbs.ival = slf.anat_index ;

   AFNI_finalize_dataset_CB( im3d->vwid->view->choose_anat_pb ,
                             (XtPointer) im3d ,  &cbs          ) ;

   RETURN(0) ;
}

/*----------------------------------------------------------------------
  Set the functional dataset.  Input is of the form "A.prefix".
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

int AFNI_drive_switch_function( char *cmd )
{
   int ic , dadd=2 ;
   Three_D_View *im3d ;
   char dname[THD_MAX_NAME] ;
   MCW_choose_cbs cbs ;
   THD_slist_find slf ;

ENTRY("AFNI_switch_function") ;

   if( cmd == NULL || strlen(cmd) < 3 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   /* get dataset name, truncate trailing blanks */

   MCW_strncpy( dname , cmd+dadd , THD_MAX_NAME ) ;
   for( ic=strlen(dname)-1 ; ic >= 0 ; ic-- )
      if( isblank(dname[ic]) || iscntrl(dname[ic]) ) dname[ic] = '\0' ;
      else break ;

   if( strlen(dname) == 0 ) RETURN(-1) ;

   /* find this dataset in current session of this controller */

   slf = THD_dset_in_session( FIND_PREFIX , dname , im3d->ss_now ) ;

   if( slf.func_index < 0 ) RETURN(-1) ;

   cbs.ival = slf.func_index ;

   AFNI_finalize_dataset_CB( im3d->vwid->view->choose_func_pb ,
                             (XtPointer) im3d ,  &cbs          ) ;

   RETURN(0) ;
}

/*--------------------------------------------------------------------
  Open a window in the controller.
  Input is a string of the form A.sagittalimage, etc.
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

int AFNI_drive_open_window( char *cmd )
{
   int ic ;
   Three_D_View *im3d ;

ENTRY("AFNI_drive_open_window") ;

   /* make sure the controller itself is open */

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ) ic = 0 ;

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_VALID(im3d) ){                  /* try to create it */
      AFNI_make_controller( ic ) ;
      im3d = GLOBAL_library.controllers[ic] ;
      if( !IM3D_VALID(im3d) ) RETURN(-1) ;   /* should never happen */
   }
   if( !IM3D_OPEN(im3d) )                    /* make sure is visible */
      OPEN_CONTROLLER(im3d) ;

   if( strlen(cmd) < 3 ) RETURN(0) ;         /* no window? */

   /* open a window */

        if( strstr(cmd,"axialimage") != NULL )
          AFNI_view_xyz_CB( im3d->vwid->imag->image_xyz_pb, im3d, NULL ) ;

   else if( strstr(cmd,"sagittalimage") != NULL )
          AFNI_view_xyz_CB( im3d->vwid->imag->image_yzx_pb, im3d, NULL ) ;

   else if( strstr(cmd,"coronalimage") != NULL )
          AFNI_view_xyz_CB( im3d->vwid->imag->image_zxy_pb, im3d, NULL ) ;

   else if( strstr(cmd,"axialgraph") != NULL )
          AFNI_view_xyz_CB( im3d->vwid->imag->graph_xyz_pb, im3d, NULL ) ;

   else if( strstr(cmd,"sagittalgraph") != NULL )
          AFNI_view_xyz_CB( im3d->vwid->imag->graph_yzx_pb, im3d, NULL ) ;

   else if( strstr(cmd,"coronalgraph") != NULL )
          AFNI_view_xyz_CB( im3d->vwid->imag->graph_zxy_pb, im3d, NULL ) ;

   else
          RETURN(-1) ;

   RETURN(0) ;
}

/*---------------------------------------------------------------*/

int AFNI_drive_quit( char *cmd )
{
  fprintf(stderr,"\n*** Plugout commanded AFNI to quit! ***\n") ;
  exit(0) ;
}
