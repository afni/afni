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

 { "OPEN_WINDOW"      , AFNI_drive_open_window       } ,
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

   if( slf.anat_index < 0 ) RETURN(-1) ;

   cbs.ival = slf.anat_index ;

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

   if( slf.func_index < 0 ) RETURN(-1) ;

   cbs.ival = slf.func_index ;

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

   /*--- opened a graph viewer: maybe modify it ---*/

   } else if ( gra != NULL ){

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

      /* pinnum */

      cpt = strstr(cmd,"pinnum=") ;
      if( cpt != NULL ){
         int pn = (int) strtod( cpt+7 , NULL ) ;
         if( pn > 1 )
            drive_MCW_grapher( gra , graDR_setpinnum , (XtPointer) pn ) ;
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
  for( ii=0 ; ii < 7 ; ii++ ){ iochan_sleep(100); fprintf(stderr,"*"); fflush(stderr); }
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
  int ii = AFNI_drive_close_graph_xy( cmd ) ;
  RETURN(ii) ;
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
