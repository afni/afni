#include "afni.h"

/*******************************************************************
  Functions to drive AFNI user-interface stuff from plugouts, etc.
  These routines take as input strings, and then call the
  appropriate callbacks to simulate what happens when the user
  presses various buttons.
********************************************************************/

static int AFNI_drive_rescan_controller( char *code ) ;
static int AFNI_drive_switch_session( char *cmd ) ;
static int AFNI_drive_switch_anatomy( char *cmd ) ;
static int AFNI_drive_switch_function( char *cmd ) ;
static int AFNI_drive_open_window( char *cmd ) ;
static int AFNI_drive_close_window( char *cmd ) ;
static int AFNI_drive_quit( char *cmd ) ;

static int AFNI_drive_system( char *cmd ) ;         /* 19 Dec 2002 */
static int AFNI_drive_chdir ( char *cmd ) ;         /* 19 Dec 2002 */

#ifdef ALLOW_PLUGINS
static int AFNI_drive_open_plugin( char *cmd ) ;    /* 13 Nov 2001 */
#endif

static int AFNI_drive_open_graph_xy ( char *cmd ) ; /* 14 Nov 2001 */
static int AFNI_drive_close_graph_xy( char *cmd ) ;
static int AFNI_drive_clear_graph_xy( char *cmd ) ;
static int AFNI_drive_addto_graph_xy( char *cmd ) ;

static int AFNI_drive_open_graph_1D ( char *cmd ) ; /* 15 Nov 2001 */
static int AFNI_drive_close_graph_1D( char *cmd ) ;
static int AFNI_drive_clear_graph_1D( char *cmd ) ;
static int AFNI_drive_addto_graph_1D( char *cmd ) ;

static int AFNI_drive_geom_graph    ( char *cmd ) ; /* 16 Nov 2001 */

static int AFNI_drive_add_overlay_color( char *cmd ) ; /* 16 Jan 2003 */
static int AFNI_drive_set_threshold    ( char *cmd ) ; /* 16 Jan 2003 */
static int AFNI_drive_set_threshnew    ( char *cmd ) ; /* 06 Feb 2004 */
static int AFNI_drive_set_pbar_number  ( char *cmd ) ; /* 16 Jan 2003 */
static int AFNI_drive_set_pbar_sign    ( char *cmd ) ; /* 17 Jan 2003 */
static int AFNI_drive_set_pbar_all     ( char *cmd ) ; /* 17 Jan 2003 */
static int AFNI_drive_pbar_rotate      ( char *cmd ) ; /* 17 Jan 2003 */
static int AFNI_set_func_autorange     ( char *cmd ) ; /* 17 Jan 2003 */
static int AFNI_set_func_range         ( char *cmd ) ; /* 21 Jan 2003 */
static int AFNI_set_func_visible       ( char *cmd ) ; /* 21 Jan 2003 */
static int AFNI_set_func_resam         ( char *cmd ) ; /* 21 Jan 2003 */
static int AFNI_sleeper                ( char *cmd ) ; /* 22 Jan 2003 */
static int AFNI_setenv                 ( char *cmd ) ; /* 22 Jan 2003 */
static int AFNI_define_colorscale      ( char *cmd ) ; /* 03 Feb 2003 */
static int AFNI_open_panel             ( char *cmd ) ; /* 05 Feb 2003 */
static int AFNI_drive_purge_memory     ( char *cmd ) ; /* 09 Dec 2004 */

/*-----------------------------------------------------------------
  Drive AFNI in various (incomplete) ways.
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

typedef int dfunc(char *) ;

  /* function label from plugout, function pointer inside AFNI */

typedef struct { char *nam; dfunc *fun; } AFNI_driver_pair ;

static AFNI_driver_pair dpair[] = {
 { "RESCAN_THIS"      , AFNI_drive_rescan_controller } ,
 { "SET_SESSION"      , AFNI_drive_switch_session    } ,
 { "SET_ANATOMY"      , AFNI_drive_switch_anatomy    } ,
 { "SET_FUNCTION"     , AFNI_drive_switch_function   } ,
 { "SWITCH_SESSION"   , AFNI_drive_switch_session    } ,
 { "SWITCH_ANATOMY"   , AFNI_drive_switch_anatomy    } ,
 { "SWITCH_FUNCTION"  , AFNI_drive_switch_function   } ,
 { "SWITCH_DIRECTORY" , AFNI_drive_switch_session    } ,
 { "SWITCH_UNDERLAY"  , AFNI_drive_switch_anatomy    } ,
 { "SWITCH_OVERLAY"   , AFNI_drive_switch_function   } ,
 { "PURGE_MEMORY"     , AFNI_drive_purge_memory      } ,

 { "OPEN_WINDOW"      , AFNI_drive_open_window       } ,
 { "ALTER_WINDOW"     , AFNI_drive_open_window       } ,
 { "CLOSE_WINDOW"     , AFNI_drive_close_window      } ,

 { "OPEN_GRAPH_XY"    , AFNI_drive_open_graph_xy     } ,
 { "CLOSE_GRAPH_XY"   , AFNI_drive_close_graph_xy    } ,
 { "CLEAR_GRAPH_XY"   , AFNI_drive_clear_graph_xy    } ,
 { "ADDTO_GRAPH_XY"   , AFNI_drive_addto_graph_xy    } ,

 { "OPEN_GRAPH_1D"    , AFNI_drive_open_graph_1D     } ,
 { "CLOSE_GRAPH_1D"   , AFNI_drive_close_graph_1D    } ,
 { "CLEAR_GRAPH_1D"   , AFNI_drive_clear_graph_1D    } ,
 { "ADDTO_GRAPH_1D"   , AFNI_drive_addto_graph_1D    } ,

 { "SET_GRAPH_GEOM"   , AFNI_drive_geom_graph        } ,

 { "QUIT"             , AFNI_drive_quit              } ,

 { "SYSTEM"           , AFNI_drive_system            } ,
 { "CHDIR"            , AFNI_drive_chdir             } ,

 { "ADD_OVERLAY_COLOR"  , AFNI_drive_add_overlay_color } ,
 { "SET_THRESHOLD"      , AFNI_drive_set_threshold     } ,
 { "SET_THRESHNEW"      , AFNI_drive_set_threshnew     } ,
 { "SET_FUNC_THRESH"    , AFNI_drive_set_threshold     } ,
 { "SET_PBAR_NUMBER"    , AFNI_drive_set_pbar_number   } ,
 { "SET_PBAR_SIGN"      , AFNI_drive_set_pbar_sign     } ,
 { "SET_PBAR_ALL"       , AFNI_drive_set_pbar_all      } ,
 { "PBAR_ROTATE"        , AFNI_drive_pbar_rotate       } ,
 { "SET_FUNC_AUTORANGE" , AFNI_set_func_autorange      } ,
 { "SET_FUNC_RANGE"     , AFNI_set_func_range          } ,
 { "SET_FUNC_VISIBLE"   , AFNI_set_func_visible        } ,
 { "SET_FUNC_RESAM"     , AFNI_set_func_resam          } ,
 { "SLEEP"              , AFNI_sleeper                 } ,
 { "SETENV"             , AFNI_setenv                  } ,
 { "DEFINE_COLORSCALE"  , AFNI_define_colorscale       } ,
 { "DEFINE_COLOR_SCALE" , AFNI_define_colorscale       } ,
 { "OPEN_PANEL"         , AFNI_open_panel              } ,

 { NULL , NULL } } ;

/*----------------------------------------------------------------------*/

int AFNI_driver( char *cmdd )
{
   int clen , rval , ii , dd , dlen ;
   char *cmd , *dmd ;

ENTRY("AFNI_driver") ;

   if( cmdd == NULL || *cmdd == '\0' ) RETURN(-1) ;  /* bad */

   dmd = cmd = strdup(cmdd) ; clen = strlen(cmd) ;

   /* skip leading blanks */

   for( ii=0 ; ii < clen ; ii++ )
      if( !isspace(cmd[ii]) ) break ;

   if( ii == clen ){ free(dmd); RETURN(-1); }  /* all blanks? */

   cmd += ii ; clen = strlen(cmd) ;

   /* 19 Dec 2002: trim trailing blanks */

   for( ii=clen-1 ; ii > 0 && isspace(cmd[ii]) ; ii-- )
     cmd[ii] = '\0' ;

   clen = strlen(cmd) ;

   /* scan dpair list for command */

   for( dd=0 ; dpair[dd].nam != NULL ; dd++ ){

      dlen = strlen(dpair[dd].nam) ;
      if( clen >= dlen                         &&
          strncmp(cmd,dpair[dd].nam,dlen) == 0   ){  /* found it */

         for( ii=dlen ; ii < clen ; ii++ )      /* skip blanks */
            if( !isspace(cmd[ii]) ) break ;     /* after command name */

         rval = dpair[dd].fun( cmd+ii ) ;       /* execute command */
         free(dmd) ; RETURN(rval) ;
      }
   }

   free(dmd) ; RETURN(-1) ;  /* not in the list */
}

/*---------------------------------------------------------------*/
/*! Purge named dataset or all datasets from memory. */

int AFNI_drive_purge_memory( char *cmd )  /* 09 Dec 2004 */
{
   char dname[THD_MAX_NAME] ;
   THD_slist_find slf ;
   int ic ;

ENTRY("AFNI_drive_purge_memory") ;

   if( *cmd == '\0' ){ AFNI_purge_dsets(1); RETURN(0); }

   /* get dataset name, truncate trailing blanks */

   MCW_strncpy( dname , cmd , THD_MAX_NAME ) ;
   for( ic=strlen(dname)-1 ; ic >= 0 ; ic-- )
     if( isspace(dname[ic]) || iscntrl(dname[ic]) ) dname[ic] = '\0' ;
     else break ;

   if( strlen(dname) == 0 ) RETURN(-1) ;

   /* find this dataset in current session of this controller */

   slf = THD_dset_in_sessionlist( FIND_PREFIX, dname, GLOBAL_library.sslist,-1 );

   if( slf.sess_index >= 0 && slf.dset_index >= 0 ){
     THD_3dim_dataset **dss =
       GLOBAL_library.sslist->ssar[slf.sess_index]->dsset[slf.dset_index] ;
     for( ic=0 ; ic <= LAST_VIEW_TYPE ; ic++ ) PURGE_DSET( dss[ic] ) ;
     RETURN(0) ;
   }

   RETURN(-1) ;
}

/*---------------------------------------------------------------*/
/*! Run a system() command. */

int AFNI_drive_system( char *cmd )  /* 19 Dec 2002 */
{
   return system( cmd ) ;
}

/*---------------------------------------------------------------*/
/*! Change working directory. */

int AFNI_drive_chdir( char *cmd )   /* 19 Dec 2002 */
{
   return chdir( cmd ) ;
}

/*-----------------------------------------------------------------
  Convert a controller code ("A", etc.) to an index.
  * Returns -1 if the index is illegal.
  * Returns -1 if the next character in the code string is NOT
     a NUL or a '.'.
  * Note that a legal index might not have an active controller.
  * Controller #i is pointed to by GLOBAL_library.controllers[i],
    for i=0..MAX_CONTROLLERS-1; cf. AFNI_rescan_controller().
-------------------------------------------------------------------*/

int AFNI_controller_code_to_index( char *code )
{
   int ic ;
ENTRY("AFNI_controller_code_to_index") ;
   if( code    == NULL || *code   == '\0' ) RETURN(-1) ;
   if( code[1] != '\0' && code[1] != '.' && code[1] != ' ' ) RETURN(-1) ;
   ic = *code - 'A' ;
   if( ic < 0 || ic >= MAX_CONTROLLERS ) ic = -1 ;
   RETURN(ic) ;
}

/*-----------------------------------------------------------------
  Rescan a controller's current session for new datasets.
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

static int AFNI_drive_rescan_controller( char *code )
{
   int ic ;
   Three_D_View *im3d ;

ENTRY("AFNI_rescan_controller") ;

   ic = AFNI_controller_code_to_index( code ) ;
   if( ic < 0 ) ic = 0 ;                       /* default = A */

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   /* same callback as Rescan This */

   AFNI_rescan_CB( NULL , (XtPointer) im3d , NULL ) ;
   RETURN(0) ;
}

/*-----------------------------------------------------------------
  Switch to a new directory in a controller.
  Input is of the form "A.directoryname".
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

static int AFNI_drive_switch_session( char *cmd )
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
      if( isspace(dname[ic]) || iscntrl(dname[ic]) ) dname[ic] = '\0' ;
      else break ;

   if( strlen(dname) == 0 ) RETURN(-1) ;

   /* find session name in list of sessions (sloppy compare) */

   for( ic=0 ; ic < GLOBAL_library.sslist->num_sess ; ic++ )
      if( strstr(GLOBAL_library.sslist->ssar[ic]->sessname,dname) != NULL ) break;

   if( ic == GLOBAL_library.sslist->num_sess ) RETURN(-1) ;

   /* do the switcheroo: same callback as Switch Session */

   cbs.ival = ic ;

   AFNI_finalize_dataset_CB( im3d->vwid->view->choose_sess_pb ,
                             (XtPointer) im3d ,  &cbs          ) ;

   RETURN(0) ;
}

/*----------------------------------------------------------------------
  Set the anatomical dataset.  Input is of the form "A.prefix".
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

static int AFNI_drive_switch_anatomy( char *cmd )
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
     if( isspace(dname[ic]) || iscntrl(dname[ic]) ) dname[ic] = '\0' ;
     else break ;

   if( strlen(dname) == 0 ) RETURN(-1) ;

   /* find this dataset in current session of this controller */

   slf = THD_dset_in_session( FIND_PREFIX , dname , im3d->ss_now ) ;

   if( slf.dset_index < 0 ) RETURN(-1) ;

   cbs.ival = slf.dset_index ;

   /* same callback as Switch Anatomy */

   AFNI_finalize_dataset_CB( im3d->vwid->view->choose_anat_pb ,
                             (XtPointer) im3d ,  &cbs          ) ;

   RETURN(0) ;
}

/*----------------------------------------------------------------------
  Set the functional dataset.  Input is of the form "A.prefix".
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

static int AFNI_drive_switch_function( char *cmd )
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
      if( isspace(dname[ic]) || iscntrl(dname[ic]) ) dname[ic] = '\0' ;
      else break ;

   if( strlen(dname) == 0 ) RETURN(-1) ;

   /* find this dataset in current session of this controller */

   slf = THD_dset_in_session( FIND_PREFIX , dname , im3d->ss_now ) ;

   if( slf.dset_index < 0 ) RETURN(-1) ;

   cbs.ival = slf.dset_index ;

   /* same callback as Switch Function */

   AFNI_finalize_dataset_CB( im3d->vwid->view->choose_func_pb ,
                             (XtPointer) im3d ,  &cbs          ) ;

   RETURN(0) ;
}

/*--------------------------------------------------------------------
  Open a window in the controller.
  Input is a string of the form A.sagittalimage, etc.
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

static int AFNI_drive_open_window( char *cmd )
{
   int ic ;
   Three_D_View *im3d ;
   char *cpt ;
   int gww=-1,ghh=-1,gxx=-1,gyy=-1 ;
   MCW_imseq   *isq=NULL ;
   MCW_grapher *gra=NULL ;

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

   if( !IM3D_OPEN(im3d) ){                   /* make sure is visible */
      OPEN_CONTROLLER(im3d) ;
      AFNI_initialize_controller( im3d ) ;   /* decide what to see */
      AFNI_initialize_view( NULL , im3d ) ;  /* set up to see it */
      AFNI_controller_clonify() ;
   }

   if( strlen(cmd) < 3 ) RETURN(0) ;         /* no commands? */

   /* 13 Nov 2001: plugins are done in a separate function */

#ifdef ALLOW_PLUGINS
   if( strstr(cmd,"plugin.") != NULL ){
      ic = AFNI_drive_open_plugin( cmd ) ;
      RETURN(ic) ;
   }
#endif

   /* open a graph or image window */

        if( strstr(cmd,"axialimage") != NULL ){
          AFNI_view_xyz_CB( im3d->vwid->imag->image_xyz_pb, im3d, NULL ) ;
          isq = im3d->s123 ;

   } else if( strstr(cmd,"sagittalimage") != NULL ){
          AFNI_view_xyz_CB( im3d->vwid->imag->image_yzx_pb, im3d, NULL ) ;
          isq = im3d->s231 ;

   } else if( strstr(cmd,"coronalimage") != NULL ){
          AFNI_view_xyz_CB( im3d->vwid->imag->image_zxy_pb, im3d, NULL ) ;
          isq = im3d->s312 ;

   } else if( strstr(cmd,"axialgraph") != NULL ){
          AFNI_view_xyz_CB( im3d->vwid->imag->graph_xyz_pb, im3d, NULL ) ;
          gra = im3d->g123 ;

   } else if( strstr(cmd,"sagittalgraph") != NULL ){
          AFNI_view_xyz_CB( im3d->vwid->imag->graph_yzx_pb, im3d, NULL ) ;
          gra = im3d->g231 ;

   } else if( strstr(cmd,"coronalgraph") != NULL ){
          AFNI_view_xyz_CB( im3d->vwid->imag->graph_zxy_pb, im3d, NULL ) ;
          gra = im3d->g312 ;
   }
   XmUpdateDisplay( im3d->vwid->top_shell ) ;

   /* find geom=..., if present */

   cpt = strstr(cmd,"geom=") ;
   if( cpt != NULL )
     AFNI_decode_geom( cpt+5 , &gww,&ghh,&gxx,&gyy ) ;

   /*--- opened an image viewer: maybe modify it ---*/

   if( isq != NULL ){

      /* geometry */

      if( gxx >= 0 && gyy >= 0 )
        XtVaSetValues( isq->wtop, XmNx, gxx, XmNy, gyy, NULL ) ;
      if( gww > 0 && ghh > 0 )
        XtVaSetValues( isq->wtop, XmNwidth, gww, XmNheight, ghh, NULL ) ;

      /* image fraction */

      cpt = strstr(cmd,"ifrac=") ;
      if( cpt != NULL ){
         float ifrac = strtod( cpt+6 , NULL ) ;
         if( ifrac >= FRAC_MIN && ifrac <= 1.0 )
           drive_MCW_imseq( isq, isqDR_setifrac, (XtPointer)(&ifrac) ) ;
      }

      /* montage */

      cpt = strstr(cmd,"mont=") ;
      if( cpt != NULL ){
         int mww=-1 , mhh=-1 , msp=-1 , mgap=-1 , nn ;
         char mcol[128] = "\0" ;

         nn = sscanf( cpt+5 , "%dx%d:%d:%d:%s" , &mww,&mhh,&msp,&mgap,mcol );

         if( nn >= 2 && mww >= 1 && mww <= MONT_NMAX && mhh >= 1 && mhh <= MONT_NMAX ){
            int mp[5] ;
            mp[0] = mww ; mp[1] = mhh ; mp[2] = msp ; mp[3] = mgap ;
            mp[4] = DC_find_overlay_color(im3d->dc,mcol);
            drive_MCW_imseq( isq , isqDR_setmontage , (XtPointer) mp ) ;
         }
      }

      /* iconify [06 Aug 2002] */

      cpt = strstr(cmd,"iconi") ;
      if( cpt != NULL ){
        XIconifyWindow( XtDisplay(isq->wtop) ,
                         XtWindow(isq->wtop)  ,
                         isq->dc->screen_num   ) ;
      }

      /* opacity [21 Jan 2003] */

      cpt = strstr(cmd,"opacity=") ;
      if( cpt != NULL ){
        int opaval = -1 ;
        sscanf( cpt+8 , "%d" , &opaval ) ;
        drive_MCW_imseq( isq , isqDR_setopacity , (XtPointer) opaval ) ;
      }

   /*--- opened a graph viewer: maybe modify it ---*/

   } else if( gra != NULL ){

      /* geometry */

      if( gxx >= 0 && gyy >= 0 )
        XtVaSetValues( gra->fdw_graph, XmNx, gxx, XmNy, gyy, NULL ) ;
      if( gww > 0 && ghh > 0 )
        XtVaSetValues( gra->fdw_graph, XmNwidth, gww, XmNheight, ghh, NULL ) ;

      /* matrix */

      cpt = strstr(cmd,"matrix=") ;
      if( cpt != NULL ){
        int mat = (int) strtod( cpt+7 , NULL ) ;
        if( mat > 0 )
          drive_MCW_grapher( gra , graDR_setmatrix , (XtPointer) mat ) ;
      }

      /* pinnum OR pintop */

      cpt = strstr(cmd,"pinnum=") ;
      if( cpt == NULL ) cpt = strstr(cmd,"pintop=") ;
      if( cpt != NULL ){
        int pn = (int) strtod( cpt+7 , NULL ) ;
        if( pn >= MIN_PIN )
          drive_MCW_grapher( gra, graDR_setpinnum, (XtPointer) pn ) ;
      }

      /* pinbot [19 Mar 2004] */

      cpt = strstr(cmd,"pinbot=") ;
      if( cpt != NULL ){
        int pn = (int) strtod( cpt+7 , NULL ) ;
        if( pn > 0 )
          drive_MCW_grapher( gra, graDR_setpinbot, (XtPointer) pn ) ;
      }

      /* iconify [06 Aug 2002] */

      cpt = strstr(cmd,"iconi") ;
      if( cpt != NULL ){
        XIconifyWindow( XtDisplay(gra->fdw_graph) ,
                         XtWindow(gra->fdw_graph)  ,
                         gra->dc->screen_num   ) ;
      }


   /*--- opened the controller itself: maybe move it ---*/

   } else {

      /* geometry */

      if( gxx >= 0 && gyy >= 0 )
        XtVaSetValues( im3d->vwid->top_shell, XmNx, gxx, XmNy, gyy, NULL ) ;

      /* iconify [06 Aug 2002] */

      cpt = strstr(cmd,"iconi") ;
      if( cpt != NULL ){
        XIconifyWindow( XtDisplay(im3d->vwid->top_shell) ,
                         XtWindow(im3d->vwid->top_shell)  ,
                         im3d->dc->screen_num   ) ;
      }

   }

   /*-- finito --*/

   XmUpdateDisplay( im3d->vwid->top_shell ) ;
   RETURN(0) ;
}

/*--------------------------------------------------------------------
  Close a window in the controller.
  Input is a string of the form A.sagittalimage, etc.
  Return value is 0 if good, -1 if bad.
  You can't close a plugin window this way.
-------------------------------------------------------------------*/

static int AFNI_drive_close_window( char *cmd )
{
   int ic ;
   Three_D_View *im3d ;

ENTRY("AFNI_drive_close_window") ;

   /* make sure the controller itself is open */

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ) ic = 0 ;

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;       /* that was easy */

   if( strlen(cmd) < 3 ){                    /* close the controller */
      if( AFNI_count_controllers() > 1 ){    /* but only if there is */
         CLOSE_CONTROLLER(im3d); RETURN(0);  /* at least 1 more open */
      }
      RETURN(-1) ;                           /* can't close last controller */
   }

   /* close a graph or image window */

        if( strstr(cmd,"axialimage") != NULL )
          drive_MCW_imseq( im3d->s123 , isqDR_destroy , NULL ) ;

   else if( strstr(cmd,"sagittalimage") != NULL )
          drive_MCW_imseq( im3d->s231 , isqDR_destroy , NULL ) ;

   else if( strstr(cmd,"coronalimage") != NULL )
          drive_MCW_imseq( im3d->s312 , isqDR_destroy , NULL ) ;

   else if( strstr(cmd,"axialgraph") != NULL )
          drive_MCW_grapher( im3d->g123 , graDR_destroy , NULL ) ;

   else if( strstr(cmd,"sagittalgraph") != NULL )
          drive_MCW_grapher( im3d->g231 , graDR_destroy , NULL ) ;

   else if( strstr(cmd,"coronalgraph") != NULL )
          drive_MCW_grapher( im3d->g312 , graDR_destroy , NULL ) ;

   else
          RETURN(-1) ;

   XmUpdateDisplay( im3d->vwid->top_shell ) ;
   RETURN(0) ;
}

/*---------------------------------------------------------------
   Open a plugin window -- 13 Nov 2001
-----------------------------------------------------------------*/

#ifdef ALLOW_PLUGINS
static int AFNI_drive_open_plugin( char *cmd )
{
   int ic , ipl , ll , pl , qq ;
   Three_D_View *im3d ;
   char *cpt ;
   int gww=-1,ghh=-1,gxx=-1,gyy=-1 ;

   int      npbut              ;       /* how many plugins */
   char **  pluglab            ;       /* their labels     */
   PLUGIN_interface ** plugint ;       /* their interfaces */

ENTRY("AFNI_drive_open_plugin") ;

   /* the controller is always good,
      since we are called from a place that just made sure of that */

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ) ic = 0 ;
   im3d = GLOBAL_library.controllers[ic] ;

   cpt = strstr(cmd,"plugin.") ;
   if( cpt == NULL || strlen(cpt) < 9 ) RETURN(-1) ;
   cpt += 7 ;  /* start of plugin name */
   pl   = strlen(cpt) ;

   /* get list of plugins */

   npbut   = im3d->vwid->nplugbut; if( npbut < 1 ) RETURN(-1) ;
   pluglab = im3d->vwid->pluglab ;
   plugint = im3d->vwid->plugint ;

   /* check name after plugin. vs. list */

   for( ipl=0 ; ipl < npbut ; ipl++ ){
      for( ll=strlen(pluglab[ipl]) ;   /* truncate trailing blanks */
           ll >= 0 && isspace(pluglab[ipl][ll]) ; ll-- ) ; /* nada */
      if( ll < 0 ) continue ;                        /* all blanks?! ERROR */
      if( pl < ll ) continue ;                       /* too short to match */
      for( qq=0 ; qq < ll ; qq++ )                   /* match each nonblank */
         if( !isspace(pluglab[ipl][qq]) && cpt[qq]!=pluglab[ipl][qq] ) break ;
      if( qq == ll ) break ;  /* found a match */
   }

   if( ipl >= npbut ) RETURN(-1) ;  /* no plugin found with this name */

   /* try to start the plugin (same callback as pressing the plugin's button) */

   PLUG_startup_plugin_CB( im3d->vwid->plugbut[ipl] , plugint[ipl] , NULL ) ;

   /* find geom=..., if present */

   cpt = strstr(cmd,"geom=") ;
   if( cpt != NULL )
      AFNI_decode_geom( cpt+5 , &gww,&ghh,&gxx,&gyy ) ;

   if( gxx >= 0                         &&
       gyy >= 0                         &&
       plugint[ipl]->wid        != NULL &&
       plugint[ipl]->wid->shell != NULL   ){

       XtVaSetValues( plugint[ipl]->wid->shell,
                      XmNx , gxx , XmNy , gyy , NULL ) ;
   }

   XmUpdateDisplay( im3d->vwid->top_shell ) ;
   RETURN(0) ;
}
#endif

/*---------------------------------------------------------------*/

static int AFNI_drive_quit( char *cmd )
{
  int ii ;
  fprintf(stderr,"\n******* Plugout commanded AFNI to quit! ") ; fflush(stderr) ;
  for( ii=0 ; ii < 7 ; ii++ ){ RWC_sleep(100); fprintf(stderr,"*"); fflush(stderr); }
  fprintf(stderr,"\n") ;
  exit(0) ;
}

/*===============================================================
  14 Nov 2001: Draw graphs from plugout-supplied data
=================================================================*/

/*-------------------------------------------------------------------------
  Define type to hold graphing information; also see coxplot/ stuff.
---------------------------------------------------------------------------*/

#include "coxplot.h"

typedef struct {
   char gname[THD_MAX_NAME] ;  /* graph name */
   int ny ;                    /* number of sub-graphs */
   float xbot,xtop ,           /* graph ranges */
         ybot,ytop  ;

   int num_pt ;                /* number points now graphed */
   int num_mpinit ;            /* number lines used when initializing graph */

   MEM_topshell_data *mp ; /* the plotting/display data structure */

   float last_x ;          /* last plotted points (if num_pt > 0) */
   float *last_y ;
} Graph_xy ;

/*-------- storage to hold active graphs controlled by plugouts --------*/

static int        num_Graph_xy  = 0 ;     /* how many graphs now have */
static Graph_xy **Graph_xy_list = NULL ;  /* array of existing graphs */

/*--------------------------------------------------------------------------
  Find a graph in the list, and return its index; if not present, return -1.
----------------------------------------------------------------------------*/

static int find_graph_xy_name( char *name )
{
   int ii ;

   if( name == NULL || name[0] == '\0' ) return -1 ;

   for( ii=0 ; ii < num_Graph_xy ; ii++ )
      if( Graph_xy_list[ii] != NULL                  &&
          strcmp(Graph_xy_list[ii]->gname,name) == 0   ) return ii ;

   return -1 ;
}

/*--------------------------------------------------------------------------
  Find an empty slot in the graph list, or make one (always succeeds).
  Return the index in the graph list.
----------------------------------------------------------------------------*/

static int find_empty_graph_xy(void)
{
   int ii ;

   /* nothing in list ==> make a new list */

   if( Graph_xy_list == NULL ){
      Graph_xy_list    = (Graph_xy **) malloc(sizeof(Graph_xy *)) ;
      Graph_xy_list[0] = NULL ;
      num_Graph_xy     = 1 ;
      return 0 ;
   }

   /* search list for an empty element */

   for( ii=0 ; ii < num_Graph_xy ; ii++ )
      if( Graph_xy_list[ii] == NULL ) return ii ;

   /* add empty element to list */

   Graph_xy_list = (Graph_xy **) realloc( Graph_xy_list ,
                                          sizeof(Graph_xy *)*(num_Graph_xy+1) ) ;
   Graph_xy_list[num_Graph_xy] = NULL ;
   num_Graph_xy++ ;
   return (num_Graph_xy-1) ;
}

/*--------------------------------------------------------------------------
  If the user kills a graph, do this (free up stuff)
----------------------------------------------------------------------------*/

static void kill_graph_xy( MEM_topshell_data * mp )
{
   int ii ;

   if( mp == NULL || Graph_xy_list == NULL ) return ;

   /* find this graph in the list */

   for( ii=0 ; ii < num_Graph_xy ; ii++ )
      if( Graph_xy_list[ii]     != NULL &&
          Graph_xy_list[ii]->mp == mp     ) break ;

   if( ii >= num_Graph_xy ) return ;  /* should not happen */

   /* free data from this graph */

   if( Graph_xy_list[ii]->last_y != NULL )
      free( Graph_xy_list[ii]->last_y ) ;

   if( mp->userdata != NULL ) free(mp->userdata) ;

   free( Graph_xy_list[ii] ) ; Graph_xy_list[ii] = NULL ; return ;
}

/*--------------------------------------------------------------------------
  SET_GRAPH_GEOM gname geom=something
----------------------------------------------------------------------------*/

static int AFNI_drive_geom_graph( char *cmd )
{
   int ntok , ig ;
   char **stok=NULL ;
   char *gname , *cpt ;
   Graph_xy *gxy ;
   int gww=-1,ghh=-1,gxx=-1,gyy=-1 ;

ENTRY("AFNI_drive_geom_graph") ;

   /* tokenize the command string */

   ntok = breakup_string( cmd , &stok ) ;
   if( ntok <= 0 || stok == NULL ) RETURN(-1) ;

   /* check if this graph name is already in use */

   ig = find_graph_xy_name( stok[0] ) ;
   freeup_strings(ntok,stok) ;
   if( ig < 0 || Graph_xy_list[ig]->mp == NULL ) RETURN(-1) ;
   gxy = Graph_xy_list[ig] ;

   cpt = strstr(cmd,"geom=") ;
   if( cpt == NULL || strlen(cpt) < 7 ) RETURN(-1) ;

   AFNI_decode_geom( cpt+5 , &gww,&ghh,&gxx,&gyy ) ;

   ig = -1 ;
   if( gxx >= 0 && gyy >= 0 ){
     XtVaSetValues( gxy->mp->top , XmNx,gxx    , XmNy,gyy     , NULL ); ig=0;
   }
   if( gww > 0 && ghh > 0 ){
     XtVaSetValues( gxy->mp->top , XmNwidth,gww, XmNheight,ghh, NULL ); ig=0;
   }

   RETURN(ig) ;
}

/*--------------------------------------------------------------------------
  OPEN_GRAPH_XY gname toplabel xbot xtop xlabel
                            ny ybot ytop ylabel yname_1 ... yname_ny
----------------------------------------------------------------------------*/

static int AFNI_drive_open_graph_xy( char *cmd )
{
   int ntok , ig ;
   char **stok=NULL ;

   char *gname , *toplabel=NULL , *xlabel=NULL , *ylabel=NULL ;
   int   ny=1 ;
   float xbot=0.0,xtop=1.0 , ybot=0.0,ytop=1.0 ;
   char **yname=NULL ;

   Graph_xy *gxy ;

ENTRY("AFNI_drive_open_graph_xy") ;

   /* tokenize the command string */

   ntok = breakup_string( cmd , &stok ) ;
   if( ntok <= 0 || stok == NULL ) RETURN(-1) ;

   /* check if this graph name is already in use */

   gname = stok[0] ;
   ig = find_graph_xy_name( gname ) ;
   if( ig >= 0 ){
      freeup_strings(ntok,stok) ; RETURN(-1) ;   /* already used = bad */
   }

   /* create a graph */

   ig = find_empty_graph_xy() ;

   Graph_xy_list[ig] = gxy = (Graph_xy *) calloc(1,sizeof(Graph_xy)) ;

   MCW_strncpy( gxy->gname , gname , THD_MAX_NAME ) ;

#if 0
{ int qq ;
  fprintf(stderr,"AFNI_drive_open_graph_xy tokens:\n") ;
  for( qq=0 ; qq < ntok ; qq++ ) fprintf(stderr," %2d:%s\n",qq,stok[qq]); }
#endif

   /* parse values out of the tokens */

   if( ntok > 1 ) toplabel = stok[1] ;

   if( ntok > 2 ) xbot = strtod(stok[2],NULL) ;
   if( ntok > 3 ) xtop = strtod(stok[3],NULL) ;
   if( xbot == xtop )    { xtop = xbot+1.0 ; }
   else if( xbot > xtop ){ float qq = xbot; xbot = xtop; xtop = qq ; }

   if( ntok > 4 ) xlabel = stok[4] ;

   if( ntok > 5 ){
      int qq = strtol(stok[5],NULL,10) ;
      if( qq > 1 ) ny = qq ;
   }

   if( ntok > 6 ) ybot = strtod(stok[6],NULL) ;
   if( ntok > 7 ) ytop = strtod(stok[7],NULL) ;
   if( ybot == ytop )    { ytop = ybot+1.0 ; }
   else if( ybot > ytop ){ float qq = ybot; ybot = ytop; ytop = qq ; }

   if( ntok > 8 ) ylabel = stok[8] ;

   if( ntok > 9 ){
      int qq ;
      yname = (char **) calloc( ny , sizeof(char *) ) ;
      for( qq=0 ; qq < ny && qq+9 < ntok ; qq++ ) yname[qq] = stok[qq+9] ;
   }

   /* put values into graph struct */

   gxy->xbot = xbot ; gxy->xtop = xtop ;
   gxy->ybot = ybot ; gxy->ytop = ytop ; gxy->ny = ny ;

   gxy->num_pt = 0 ;  /* number of points plotted thus far */

   /* create the actual graph */

   gxy->mp = plot_ts_init( GLOBAL_library.dc->display ,
                           xbot , xtop ,
                           -ny , ybot , ytop ,
                           xlabel , ylabel ,
                           toplabel , yname , kill_graph_xy ) ;

   freeup_strings(ntok,stok) ;
   if( yname != NULL ) free(yname) ;

   if( gxy->mp == NULL ){         /* should not happen */
      free(Graph_xy_list[ig]) ; Graph_xy_list[ig] = NULL ;
      RETURN(-1) ;
   }

   /* number of lines plotted so far (labels, etc.) */

   gxy->num_mpinit = MEMPLOT_NLINE(gxy->mp->mp) ;

   /* place to store last location plotted */

   gxy->last_y = (float *) calloc(ny,sizeof(float)) ;

   RETURN(0) ;
}

/*--------------------------------------------------------------------------
  CLOSE_GRAPH_XY gname
----------------------------------------------------------------------------*/

static int AFNI_drive_close_graph_xy( char *cmd )
{
   int ig , ntok ;
   char **stok = NULL ;

ENTRY("AFNI_drive_close_graph_xy") ;

   /* tokenize the command string */

   ntok = breakup_string( cmd , &stok ) ;
   if( ntok <= 0 || stok == NULL ) RETURN(-1) ;

   /* check if this graph name is already in use */

   ig = find_graph_xy_name( stok[0] ) ;
   freeup_strings(ntok,stok) ;
   if( ig < 0 || Graph_xy_list[ig]->mp == NULL ) RETURN(-1) ;

   plotkill_topshell( Graph_xy_list[ig]->mp ) ; /* will call kill_graph_xy */
   RETURN(0) ;
}

/*--------------------------------------------------------------------------
  CLEAR_GRAPH_XY gname
----------------------------------------------------------------------------*/

static int AFNI_drive_clear_graph_xy( char *cmd )
{
   int ig , ntok ;
   char **stok = NULL ;

ENTRY("AFNI_drive_clear_graph_xy") ;

   /* tokenize the command string */

   ntok = breakup_string( cmd , &stok ) ;
   if( ntok <= 0 || stok == NULL ) RETURN(-1) ;

   /* check if this graph name is already in use */

   ig = find_graph_xy_name( stok[0] ) ;
   freeup_strings(ntok,stok) ;
   if( ig < 0 || Graph_xy_list[ig]->mp == NULL ) RETURN(-1) ;

   /* cut off the graph lines since the initial stuff */

   TRUNC_MEMPLOT( Graph_xy_list[ig]->mp->mp ,
                  Graph_xy_list[ig]->num_mpinit ) ;

   /* show it again, Sam (but the data part will be gone) */

   redraw_topshell( Graph_xy_list[ig]->mp ) ;
   Graph_xy_list[ig]->num_pt = 0 ;  /* number of points plotted thus far */
   RETURN(0) ;
}

/*--------------------------------------------------------------------------
  ADDTO_GRAPH_XY gname x y_1 y_2 .. y_ny [repeat]
----------------------------------------------------------------------------*/

static int AFNI_drive_addto_graph_xy( char *cmd )
{
   int ig , ntok , nx , ny , ii,jj , tt , num_pt , ibot ;
   char **stok = NULL ;
   Graph_xy *gxy ;
   float *x , **y ;

ENTRY("AFNI_drive_addto_graph_xy") ;

   /* tokenize the command string */

   ntok = breakup_string( cmd , &stok ) ;
   if( ntok <= 0 || stok == NULL ) RETURN(-1) ;

   /* check if this graph name is already in use */

   ig = find_graph_xy_name( stok[0] ) ;
   if( ig < 0 || Graph_xy_list[ig]->mp == NULL ){
      freeup_strings(ntok,stok) ; RETURN(-1) ;
   }
   gxy = Graph_xy_list[ig] ;

   /* number of sub-graphs */

   ny = gxy->ny ;

   /* number of points to add to each sub-graph */

   nx = (ntok-1)/(ny+1) ;
   if( nx < 1 ){ freeup_strings(ntok,stok); RETURN(-1); }

   /* attach to last_x and last_y? */

   num_pt = gxy->num_pt ;
   if( num_pt > 0 ) nx++ ;  /* need 1 extra place for last_x and last_y */

   x = (float *) calloc(nx,sizeof(float)) ;
   y = (float **) malloc( sizeof(float *)*ny ) ;
   for( jj=0 ; jj < ny ; jj++ )
      y[jj] = (float *) calloc(nx,sizeof(float)) ;

   /* put last_x and last_y into graphing data, if needed */

   if( num_pt > 0 ){
      x[0] = gxy->last_x ;
      for( jj=0 ; jj < ny ; jj++ ) y[jj][0] = gxy->last_y[jj] ;
      ibot = 1 ;
   } else {
      ibot = 0 ;
   }

   /* parse tokens into numbers for graphing */

   tt = 1 ;                            /* token index */
   for( ii=ibot ; ii < nx ; ii++ ){
      x[ii] = strtod( stok[tt++] , NULL ) ;
      for( jj=0 ; jj < ny ; jj++ )
         y[jj][ii] = strtod( stok[tt++] , NULL ) ;
   }

   /* save new last_x and last_y */

   gxy->last_x = x[nx-1] ;
   for( jj=0 ; jj < ny ; jj++ ) gxy->last_y[jj] = y[jj][nx-1] ;

   /**** graphing! ****/

   plot_ts_addto( gxy->mp , nx , x , -ny , y ) ;

   /* cleanup */

   gxy->num_pt += nx ;  /* number of points in each sub-graph so far */

   for( jj=0 ; jj < ny ; jj++ ) free(y[jj]) ;
   free(y) ; free(x) ; freeup_strings(ntok,stok) ;

   RETURN(0) ;
}

/*-------------------------------------------------------------------------
  CLOSE_GRAPH_1D gname
---------------------------------------------------------------------------*/

static int AFNI_drive_close_graph_1D( char *cmd )
{
  return AFNI_drive_close_graph_xy( cmd ) ;
}

/*-------------------------------------------------------------------------
  OPEN_GRAPH_1D gname toplab nx dx xlab
                             ny ybot ytop ylab yname_1 ... yname_ny
---------------------------------------------------------------------------*/

static int AFNI_drive_open_graph_1D( char *cmd )
{
   int ntok , ig , ii ;
   char **stok=NULL ;

   char *gname , *toplabel=NULL , *xlabel=NULL , *ylabel=NULL ;
   int   ny=1 , nx=500 ;
   float dx=1.0 , ybot=0.0,ytop=1.0 ;
   char **yname=NULL ;

   Graph_xy *gxy ;

ENTRY("AFNI_drive_open_graph_1D") ;

   /* tokenize the command string */

   ntok = breakup_string( cmd , &stok ) ;
   if( ntok <= 0 || stok == NULL ) RETURN(-1) ;

   /* check if this graph name is already in use */

   gname = stok[0] ;
   ig = find_graph_xy_name( gname ) ;
   if( ig >= 0 ){
      freeup_strings(ntok,stok) ; RETURN(-1) ;   /* already used = bad */
   }

#if 0
{ int qq ;
  fprintf(stderr,"AFNI_drive_open_graph_1D tokens:\n") ;
  for( qq=0 ; qq < ntok ; qq++ ) fprintf(stderr," %2d:%s\n",qq,stok[qq]); }
#endif

   /* create a graph */

   ig = find_empty_graph_xy() ;

   Graph_xy_list[ig] = gxy = (Graph_xy *) malloc(sizeof(Graph_xy)) ;

   MCW_strncpy( gxy->gname , gname , THD_MAX_NAME ) ;

   /* parse values out of the tokens */

   if( ntok > 1 ) toplabel = stok[1] ;

   if( ntok > 2 ){
      int qq = strtol(stok[2],NULL,10) ;
      if( qq >= 10 ) nx = qq ;
   }

   if( ntok > 3 ){
      float qq = strtod(stok[3],NULL) ;
      if( qq > 0.0 ) dx = qq ;
   }

   if( ntok > 4 ) xlabel = stok[4] ;

   if( ntok > 5 ){
      int qq = strtol(stok[5],NULL,10) ;
      if( qq > 1 ) ny = qq ;
   }

   if( ntok > 6 ) ybot = strtod(stok[6],NULL) ;
   if( ntok > 7 ) ytop = strtod(stok[7],NULL) ;
   if( ybot == ytop )    { ytop = ybot+1.0 ; }
   else if( ybot > ytop ){ float qq = ybot; ybot = ytop; ytop = qq ; }

   if( ntok > 8 ) ylabel = stok[8] ;

   if( ntok > 9 ){
      int qq ;
      yname = (char **) calloc( ny , sizeof(char *) ) ;
      for( qq=0 ; qq < ny && qq+9 < ntok ; qq++ ) yname[qq] = stok[qq+9] ;
   }

   /* put values into graph struct */

   gxy->xbot = 0.0  ; gxy->xtop = nx*dx ;
   gxy->ybot = ybot ; gxy->ytop = ytop  ; gxy->ny = ny ;

   gxy->num_pt = 0.0 ;

   /* create the actual graph */

   gxy->mp = plot_strip_init( GLOBAL_library.dc->display ,
                              nx , dx ,
                              -ny , ybot , ytop ,
                              xlabel , ylabel ,
                              toplabel , yname , kill_graph_xy ) ;

   freeup_strings(ntok,stok) ;
   if( yname != NULL ) free(yname) ;

   if( gxy->mp == NULL ){         /* should not happen */
      free(Graph_xy_list[ig]) ; Graph_xy_list[ig] = NULL ;
      RETURN(-1) ;
   }

   gxy->last_y = NULL ;  /* not used for this graph type */

   RETURN(0) ;
}

/*-------------------------------------------------------------------------
  CLEAR_GRAPH_1D gname
---------------------------------------------------------------------------*/

static int AFNI_drive_clear_graph_1D( char *cmd )
{
   int ig , ntok , ii ;
   char **stok = NULL ;

ENTRY("AFNI_drive_clear_graph_1D") ;

   /* tokenize the command string */

   ntok = breakup_string( cmd , &stok ) ;
   if( ntok <= 0 || stok == NULL ) RETURN(-1) ;

   /* check if this graph name is already in use */

   ig = find_graph_xy_name( stok[0] ) ;
   freeup_strings(ntok,stok) ;
   if( ig < 0 || Graph_xy_list[ig]->mp == NULL ) RETURN(-1) ;

   /* do the clearance work */

   plot_strip_clear( Graph_xy_list[ig]->mp ) ;  /* coxplot/plot_strip.c */

   RETURN(0) ;
}

/*-------------------------------------------------------------------------
  ADDTO_GRAPH_1D gname y_1 y_2 .. y_ny [repeat]
---------------------------------------------------------------------------*/

static int AFNI_drive_addto_graph_1D( char *cmd )
{
   int ig, ntok, nx, ny, ii,jj, tt, num_pt, ibot, nadd ;
   char **stok = NULL ;
   Graph_xy *gxy ;
   float **y ;

ENTRY("AFNI_drive_addto_graph_1D") ;

   /* tokenize the command string */

   ntok = breakup_string( cmd , &stok ) ;
   if( ntok <= 0 || stok == NULL ) RETURN(-1) ;

   /* check if this graph name is already in use */

   ig = find_graph_xy_name( stok[0] ) ;
   if( ig < 0 || Graph_xy_list[ig]->mp == NULL ){
      freeup_strings(ntok,stok) ; RETURN(-1) ;
   }
   gxy = Graph_xy_list[ig] ;

   /* number of sub-graphs */

   ny = gxy->ny ;

   /* number of points to add to each sub-graph */

   nadd = (ntok-1)/ny ;
   if( nadd < 1 ){ freeup_strings(ntok,stok); RETURN(-1); }

   /* make space for incoming data */

   y = (float **) malloc( sizeof(float *)*ny ) ;
   for( jj=0 ; jj < ny ; jj++ )
      y[jj] = (float *) calloc(nadd,sizeof(float)) ;

   /* convert incoming tokens to numbers */

   tt = 1 ;                            /* token index */
   for( ii=0 ; ii < nadd ; ii++ ){
      for( jj=0 ; jj < ny ; jj++ )
         y[jj][ii] = strtod( stok[tt++] , NULL ) ;
   }

   /***** graphing! *****/

   plot_strip_addto( gxy->mp , nadd , y ) ;

   /* cleanup */

   for( jj=0 ; jj < ny ; jj++ ) free(y[jj]) ;
   free(y) ; freeup_strings(ntok,stok) ;

   RETURN(0) ;
}

/*------------------------------------------------------------------------*/
/*! ADD_OVERLAY_COLOR colordef colorlab -- 16 Jan 2003
--------------------------------------------------------------------------*/

static int AFNI_drive_add_overlay_color( char *cmd )
{
   char cdef[256]="\0" , clab[256]="\0" ;
   int ii ;

ENTRY("AFNI_drive_add_overlay_color") ;

   sscanf( cmd , "%255s %255s" , cdef,clab ) ;
   if( cdef[0] == '\0' || clab[0] == '\0' ) RETURN(-1) ;

   if( GLOBAL_library.dc == NULL ){           /* before X11 started */
     if( INIT_ncolovr < MAX_NCOLOVR ){
       ii = INIT_ncolovr++ ;
       INIT_labovr[ii] = XtNewString(cdef) ;
       INIT_colovr[ii] = XtNewString(clab) ;
     } else
       RETURN(-1) ;
   } else {                                   /* after X11 started */
     ii = DC_add_overlay_color( GLOBAL_library.dc , cdef , clab ) ;
     if( ii < 0 ) RETURN(-1) ;
     OVC_mostest( GLOBAL_library.dc->ovc ) ;
   }
   RETURN(0) ;
}

/*------------------------------------------------------------------------*/
/*! SET_THRESHOLD [c].val [dec] -- 16 Jan 2003
    as in "SET_THRESHOLD A.3327 2" or
          "SET_THRESHOLD .9932 2"
--------------------------------------------------------------------------*/

static int AFNI_drive_set_threshold( char *cmd )
{
   int ic , dadd=1 , dec=-1 , ival , smax ;
   Three_D_View *im3d ;
   float val ;
   char *cpt ;
   static float tval[9] = { 1.0 , 10.0 , 100.0 , 1000.0 , 10000.0 ,
                            100000.0 , 1000000.0 , 10000000.0 , 100000000.0 } ;

ENTRY("AFNI_drive_set_threshold") ;

   if( cmd == NULL || strlen(cmd) < 3 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   /* get val and set scale */

   if( cmd[dadd] != '.' ) RETURN(-1) ;
   val = strtod( cmd+dadd , &cpt ) ;
   ival = rint(val/THR_FACTOR) ;
   smax = (int)( pow(10.0,THR_TOP_EXPON) + 0.001 ) - 1 ;
   if( ival < 0 || ival > smax ) RETURN(-1) ;
   XmScaleSetValue( im3d->vwid->func->thr_scale , ival ) ;

   /* get dec, if present, and apply it */

   sscanf(cpt,"%d",&dec) ;

   if( dec >= 0 && dec <= THR_TOP_EXPON )
     AFNI_set_thresh_top( im3d , tval[dec] ) ;

   AFNI_thr_scale_CB( im3d->vwid->func->thr_scale, (XtPointer)im3d, NULL ) ;
   RETURN(0) ;
}

/*------------------------------------------------------------------------*/
/*! SET_THRESHNEW [c].val [**] -- 06 Feb 2004
    as in "SET_THRESHNEW A.0.372" or
          "SET_THRESHNEW 0.372 **"   (the "**" means to set the range too)
--------------------------------------------------------------------------*/

static int AFNI_drive_set_threshnew( char *cmd )
{
   int ic,dadd , olddec,newdec , ival,smax,id,stop , dopval,dostar;
   Three_D_View *im3d ;
   float val , pval ;
   char *cpt ;
   static float tval[9] = { 1.0 , 10.0 , 100.0 , 1000.0 , 10000.0 ,
                            100000.0 , 1000000.0 , 10000000.0 , 100000000.0 } ;

ENTRY("AFNI_drive_set_threshnew") ;

   if( cmd == NULL || *cmd == '\0' ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){
     ic = 0 ; dadd = 0 ;
   } else {
     if( cmd[1] == '\0' ) RETURN(-1) ;   /* no value following? */
     dadd = 2 ;                          /* skip period following controller */
   }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;   /* stupid user */

   /* get val from command line */

   val = strtod( cmd+dadd , &cpt ) ;
   if( val < 0.0 || val > THR_TOP_VALUE ) RETURN(-1) ; /* stupid user */

   /* get current scale decimal setting */

   olddec = (int)rint( log10(im3d->vinfo->func_thresh_top) ) ;
        if( olddec < 0             ) olddec = 0 ;
   else if( olddec > THR_TOP_EXPON ) olddec = THR_TOP_EXPON ;
   newdec = olddec ;

   smax  = (int)rint( pow(10.0,THR_TOP_EXPON) ) ;
   stop  = smax - 1 ;                             /* max slider value */

   dopval = (val >= 0.0) && (val <= 1.0) && (strchr(cpt,'p') != NULL) &&
            (DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) > 0) ;

   dostar = (val > 0.0) && (strchr(cpt,'*') != NULL) ;

   if( dopval ){
     pval = THD_pval_to_stat( val ,
              DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) ,
              DSET_BRICK_STATAUX (im3d->fim_now,im3d->vinfo->thr_index)  ) ;
     if( pval >= 0.0 ) val = pval ;
   }

   if( val >= im3d->vinfo->func_thresh_top || dostar ){ /* reset scale range */

     newdec = (int)( log10(val) + 1.0 ) ;
          if( newdec < 0             ) newdec = 0 ;
     else if( newdec > THR_TOP_EXPON ) newdec = THR_TOP_EXPON ;
   }

   if( newdec != olddec )
     AFNI_set_thresh_top( im3d , tval[newdec] ) ;

   ival = rint( val/(THR_FACTOR*tval[newdec]) ) ;
        if( ival < 0    ) ival = 0    ;
   else if( ival > stop ) ival = stop ;

   XmScaleSetValue( im3d->vwid->func->thr_scale , ival ) ;

   AFNI_thr_scale_CB( im3d->vwid->func->thr_scale, (XtPointer)im3d, NULL ) ;
   RETURN(0) ;
}

/*------------------------------------------------------------------------*/
/*! SET_PBAR_NUMBER [c.]num
    as in "SET_PBAR_NUMBER A.3"
    use a large number (99, say) to turn on colorscale mode
--------------------------------------------------------------------------*/

static int AFNI_drive_set_pbar_number( char *cmd )
{
   int ic , dadd=2 , num=-1 ;
   Three_D_View *im3d ;
   MCW_pbar *pbar ;

ENTRY("AFNI_drive_set_pbar_number") ;

   if( cmd == NULL || strlen(cmd) < 1 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   if( !isdigit(cmd[dadd]) ) RETURN(-1) ;

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   pbar = im3d->vwid->func->inten_pbar ;

   num = (int) strtod( cmd+dadd , NULL ) ;
   if( num < NPANE_MIN ) RETURN(-1) ;
   if( num > NPANE_MAX ) num = NPANE_MAX+1 ;
   AV_assign_ival( im3d->vwid->func->inten_av , num ) ;

   HIDE_SCALE(im3d) ;
   if( num <= NPANE_MAX ){
     pbar->bigmode = 0 ;
     alter_MCW_pbar( pbar , num , NULL ) ;
   } else {
     int npane=pbar->num_panes , jm=pbar->mode ;
     float pmax=pbar->pval_save[npane][0][jm] ,
           pmin=pbar->pval_save[npane][npane][jm] ;
     PBAR_set_bigmode( pbar , 1 , pmin,pmax ) ;
     AFNI_inten_pbar_CB( pbar , im3d , 0 ) ;
   }
   FIX_SCALE_SIZE(im3d) ;

   RETURN(0) ;
}

/*------------------------------------------------------------------------*/
/*! SET_PBAR_SIGN [c.]+ OR [c.]-
    as in "SET_PBAR_SIGN A.+"
--------------------------------------------------------------------------*/

static int AFNI_drive_set_pbar_sign( char *cmd )
{
   int ic , dadd=2 , val ;
   Three_D_View *im3d ;

ENTRY("AFNI_drive_set_pbar_sign") ;

   if( cmd == NULL || strlen(cmd) < 1 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   switch( cmd[dadd] ){
     default: RETURN(-1) ;

     case 'p':
     case 'P':
     case '+': val = 1 ; break ;

     case 's':
     case 'S':
     case '-': val = 0 ; break ;
   }

   MCW_set_bbox( im3d->vwid->func->inten_bbox , val ) ;

   AFNI_inten_bbox_CB( im3d->vwid->func->inten_bbox->wbut[PBAR_MODEBUT] ,
                       (XtPointer) im3d , NULL ) ;
   RETURN(0) ;
}

/*-------------------------------------------------------------------------*/
/*! SET_PBAR_ALL [c.]{+|-}num val=color val=color ...
   "SET_PBAR_ALL A.+5 1.0=yellow 0.5=red 0.05=none -0.05=blue -0.50=cyan"
   "SET_PBAR_ALL A.+99 topval colorscale_name FLIP ROTA=n"
---------------------------------------------------------------------------*/

static int AFNI_drive_set_pbar_all( char *cmd )
{
   int ic , dadd=2 , npan=0 , pos , nn , ii,jj ;
   float pval[NPANE_MAX+1] , val ;
   int   pcol[NPANE_MAX]   , col , flip=0 , rota=0 ;
   char  str[256] , *cpt ;
   MCW_pbar *pbar ;
   Three_D_View *im3d ;

ENTRY("AFNI_drive_set_pbar_all") ;

   if( cmd == NULL || strlen(cmd) < 4 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   /* get sign */

   switch( cmd[dadd] ){
     default: RETURN(-1) ;
     case '+': pos = 1 ; break ;
     case '-': pos = 0 ; break ;
   }
   dadd++ ;

   /* get number of panes */

   sscanf( cmd+dadd , "%d%n" , &npan , &nn ) ;
   if( npan < NPANE_MIN ) RETURN(-1) ;
   if( npan > NPANE_MAX ) npan = NPANE_MAX+1 ;
   dadd += nn ;

   pbar = im3d->vwid->func->inten_pbar ;

   if( npan <= NPANE_MAX ){ /* discrete panes: get value=colorname array */

     for( ii=0 ; ii < npan ; ii++ ){
       str[0] = '\0' ; nn = 0 ;
       sscanf( cmd+dadd , "%f=%255s%n" , &val,str,&nn ) ;
       if( str[0] == '\0' || nn == 0 ) RETURN(-1) ;  /* can't parse */

       col = DC_find_overlay_color( GLOBAL_library.dc , str ) ;
       if( col < 0 )                   RETURN(-1) ;  /* bad color name */

       for( jj=0 ; jj < ii ; jj++ )                  /* check ordering */
         if( pval[jj] <= val )         RETURN(-1) ;

       if( ii > 0 ){                                 /* check size */
         if( fabs(val) >= pval[0] )    RETURN(-1) ;
         if( pos && val <= 0.0 )       RETURN(-1) ;
       } else {
         if( val <= 0.0 )              RETURN(-1) ;
       }

       pval[ii] = val ; pcol[ii] = col ;
       dadd += nn ;
     }
     if( pos ) pval[npan] = 0.0 ;        /* set bottom level */
     else      pval[npan] = -pval[0] ;

   } else {     /* 03 Feb 2003: get topval and colorscale_name */

     str[0] = '\0' ; val = 0.0 ;
     sscanf( cmd+dadd , "%f %s" , &val, str ) ;
     if( val <= 0.0 ) RETURN(-1) ;

     flip = ( strstr(cmd+dadd,"FLIP") != NULL ) ;

     cpt = strstr(cmd+dadd,"ROTA=") ;
     if( cpt != NULL ) sscanf(cpt+5,"%d",&rota) ;
   }

   /* now set pbar values (and other widgets) */

   im3d->vinfo->use_posfunc = pbar->mode = pos ;
   MCW_set_bbox( im3d->vwid->func->inten_bbox , pos ) ;
   AV_assign_ival( im3d->vwid->func->inten_av , npan ) ;

   HIDE_SCALE(im3d) ;
   if( npan <= NPANE_MAX ){         /* set discrete panels */
     for( ii=0 ; ii < npan ; ii++ ){
       pbar->ov_index[ii] = pbar->ovin_save[npan][ii][pos] = pcol[ii] ;
       PBAR_set_panecolor( pbar , ii , pcol[ii] ) ;
     }
     pbar->bigmode = 0 ;
     alter_MCW_pbar( pbar , npan , pval ) ;
     AFNI_hintize_pbar( pbar , (im3d->vinfo->fim_range != 0.0)
                                ? im3d->vinfo->fim_range
                                : im3d->vinfo->fim_autorange ) ;
   } else {    /* set the colorscale */
     float pmax, pmin ;
     pbar->bigset = 0 ;
     pmax = val ; pmin = (pbar->mode) ? 0.0 : -pmax ;
     PBAR_set_bigmode( pbar , 1 , pmin,pmax ) ;
     PBAR_set_bigmap( pbar , str ) ;
     rotate_MCW_pbar( pbar , rota ) ;  /* 07 Feb 2004 */
     if( flip ) PBAR_flip( pbar ) ;    /* 07 Feb 2004 */
     AFNI_inten_pbar_CB( pbar , im3d , 0 ) ;
   }
   FIX_SCALE_SIZE(im3d) ;

   RETURN(0) ;
}

/*-------------------------------------------------------------------------*/
/*! PBAR_ROTATE [c.]{+|-}
   "PBAR_ROTATE A.+"
---------------------------------------------------------------------------*/

static int AFNI_drive_pbar_rotate( char *cmd )
{
   int ic , dadd=2 , nn ;
   MCW_pbar *pbar ;
   Three_D_View *im3d ;

ENTRY("AFNI_drive_pbar_rotate") ;

   if( cmd == NULL || strlen(cmd) < 1 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   /* get sign */

   switch( cmd[dadd] ){
     default: RETURN(-1) ;
     case '+': nn =  1 ; break ;
     case '-': nn = -1 ; break ;
   }

   rotate_MCW_pbar( im3d->vwid->func->inten_pbar , nn ) ;
   RETURN(0) ;
}

/*-------------------------------------------------------------------------*/
/*! SET_FUNC_AUTORANGE [c.]{+|-}
   "SET_FUNC_AUTORANGE A.+"
---------------------------------------------------------------------------*/

static int AFNI_set_func_autorange( char *cmd )
{
   int ic , dadd=2 , nn ;
   Three_D_View *im3d ;

ENTRY("AFNI_set_func_autorange") ;

   if( cmd == NULL || strlen(cmd) < 1 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   switch( cmd[dadd] ){
     default: RETURN(-1) ;
     case '+': nn = 1 ; break ;
     case '-': nn = 0 ; break ;
   }

   MCW_set_bbox( im3d->vwid->func->range_bbox , nn ) ;
   AFNI_range_bbox_CB( im3d->vwid->func->range_bbox->wbut[RANGE_AUTOBUT] ,
                       im3d , NULL ) ;
   RETURN(0) ;
}

/*-------------------------------------------------------------------------*/
/*! SET_FUNC_RANGE [c.]value
   "SET_FUNC_RANGE A.0.3333"
---------------------------------------------------------------------------*/

static int AFNI_set_func_range( char *cmd )
{
   int ic , dadd=2 ;
   Three_D_View *im3d ;
   float val ;

ENTRY("AFNI_set_func_range") ;

   if( cmd == NULL || strlen(cmd) < 1 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   val = strtod( cmd+dadd , NULL ) ;
   if( val <  0.0 ) RETURN(-1) ;

   if( val == 0.0 ){
     char clabel[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;
     char str[8] ;
     sprintf(str,"%c.+",clabel[ic]) ;
     RETURN( AFNI_set_func_autorange(str) ) ;
   }

   MCW_set_bbox( im3d->vwid->func->range_bbox , 0 ) ;   /* autoRange box off */
   im3d->vinfo->use_autorange = 0 ;

   AV_SENSITIZE( im3d->vwid->func->range_av , 1 ) ;
   AV_assign_fval( im3d->vwid->func->range_av , val ) ;
   AFNI_range_av_CB( im3d->vwid->func->range_av , im3d ) ;
   RETURN(0) ;
}

/*-------------------------------------------------------------------------*/
/*! SET_FUNC_VISIBLE [c.]{+|-}
   "SET_FUNC_VISIBLE A.+"
---------------------------------------------------------------------------*/

static int AFNI_set_func_visible( char *cmd )
{
   int ic , dadd=2 , nn ;
   Three_D_View *im3d ;

ENTRY("AFNI_set_func_visible") ;

   if( cmd == NULL || strlen(cmd) < 1 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   switch( cmd[dadd] ){
     default: RETURN(-1) ;
     case '+': nn = 1 ; break ;
     case '-': nn = 0 ; break ;
   }

   MCW_set_bbox( im3d->vwid->view->see_func_bbox , nn ) ;
   AFNI_see_func_CB( NULL , im3d , NULL ) ;
   RETURN(0) ;
}

/*-------------------------------------------------------------------------*/
/*! SET_FUNC_RESAM [c.]{NN|Li|Cu|Bk}.{NN|Li|Cu|Bk}
   "SET_FUNC_RESAM A.Li.Li"
---------------------------------------------------------------------------*/

static int AFNI_set_func_resam( char *cmd )
{
   int ic , dadd=2 , fr=-1 , tr=-1 ;
   Three_D_View *im3d ;

ENTRY("AFNI_set_func_resam") ;

   if( cmd == NULL || strlen(cmd) < 2 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

        if( cmd[dadd] == 'N' && cmd[dadd+1] == 'N' ) fr = 0 ;
   else if( cmd[dadd] == 'L' && cmd[dadd+1] == 'i' ) fr = 1 ;
   else if( cmd[dadd] == 'C' && cmd[dadd+1] == 'u' ) fr = 2 ;
   else if( cmd[dadd] == 'B' && cmd[dadd+1] == 'k' ) fr = 2 ;
   else                                              RETURN(-1);

   if( cmd[dadd+2] == '.' ){
     dadd += 3 ;
          if( cmd[dadd] == 'N' && cmd[dadd+1] == 'N' ) tr = 0 ;
     else if( cmd[dadd] == 'L' && cmd[dadd+1] == 'i' ) tr = 1 ;
     else if( cmd[dadd] == 'C' && cmd[dadd+1] == 'u' ) tr = 2 ;
     else if( cmd[dadd] == 'B' && cmd[dadd+1] == 'k' ) tr = 2 ;
     else                                              RETURN(-1);
   }

   AV_assign_ival( im3d->vwid->dmode->func_resam_av , fr ) ;
   im3d->vinfo->func_resam_mode = fr ;
   if( im3d->b123_fim != NULL )
     im3d->b123_fim->resam_code =
      im3d->b231_fim->resam_code =
       im3d->b312_fim->resam_code = im3d->vinfo->func_resam_mode ;

   if( tr >= 0 ){
     AV_assign_ival( im3d->vwid->dmode->thr_resam_av , fr ) ;
     im3d->vinfo->thr_resam_mode = tr ;
     if( im3d->b123_fim != NULL )
       im3d->b123_fim->thr_resam_code =
        im3d->b231_fim->thr_resam_code =
         im3d->b312_fim->thr_resam_code = im3d->vinfo->thr_resam_mode ;
   }

   AFNI_resam_av_CB( NULL , im3d ) ;
   RETURN(0) ;
}

/*------------------------------------------------------------------*/
/*! SLEEP msec */

static int AFNI_sleeper( char *cmd )
{
   int ms=-1 ;
   if( cmd == NULL || strlen(cmd) < 1 ) return(-1) ;
   sscanf( cmd , "%d" , &ms ) ;
   if( ms > 0 ) RWC_sleep( ms ) ;
   return(0) ;
}

/*------------------------------------------------------------------*/
/*! SETENV name value */

static int AFNI_setenv( char *cmd )
{
   char nam[256]="\0" , val[1024]="\0" , eqn[1280] , *eee ;

   if( cmd == NULL || strlen(cmd) < 3 ) return(-1) ;

   sscanf( cmd , "%255s %1023s" , nam , val ) ;
   if( nam[0] == '\0' || val[0] == '\0' ) return(-1) ;

   sprintf(eqn,"%s=%s",nam,val) ;
   eee = strdup(eqn) ; putenv(eee) ;
   return(0) ;
}

/*------------------------------------------------------------------*/
/*! REDISPLAY */

static int AFNI_redisplay( char *cmd )
{
   int cc ;
   Three_D_View *qq3d ;

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){
      qq3d = GLOBAL_library.controllers[cc] ;
      if( ! IM3D_OPEN(qq3d) ) continue ;
      AFNI_set_viewpoint( qq3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
   }
   return(0) ;
}

/*--------------------------------------------------------------------*/
/*! DEFINE_COLORSCALE name num=color num=color ... */

static int AFNI_define_colorscale( char *cmd )
{
  return PBAR_define_bigmap( cmd ) ;
}

/*---------------------------------------------------------------------*/
/*! OPEN_PANEL [c.]Define_Function, etc. */

static int AFNI_open_panel( char *cmd )
{
   int ic , dadd=2 , fr=-1 , tr=-1 ;
   Three_D_View *im3d ;

ENTRY("AFNI_open_panel") ;

   if( cmd == NULL || strlen(cmd) < 2 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   /* do the right thing (simulate a button press) */

   if( strcmp(cmd+dadd,"Define_Function") == 0 || strcmp(cmd+dadd,"Define_Overlay") == 0 ){
     if( !XtIsManaged(im3d->vwid->func->frame) )
       AFNI_define_CB( im3d->vwid->view->define_func_pb, im3d, NULL ) ;
   } else if( strcmp(cmd+dadd,"Define_Datamode") == 0 ){
     if( !XtIsManaged(im3d->vwid->dmode->frame) )
       AFNI_define_CB( im3d->vwid->view->define_dmode_pb, im3d, NULL ) ;
   } else if( strcmp(cmd+dadd,"Define_Markers")  == 0 ){
     if( !XtIsManaged(im3d->vwid->marks->frame) )
       AFNI_define_CB( im3d->vwid->view->define_marks_pb, im3d, NULL ) ;
   } else {
     RETURN(-1) ;
   }
   RETURN(0) ;
}
