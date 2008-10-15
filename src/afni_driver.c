#include "afni.h"
#include <X11/keysym.h>

/******************************************************************************
 *       Functions to drive AFNI user-interface stuff from plugouts, etc.     *
 *       These routines take as input strings, and then call the              *
 *       appropriate callbacks to simulate what happens when the user         *
 *       presses various buttons.                                             *
*******************************************************************************/

/*****-------------------------------------------------------------------------
 To add a new command, you need to
 (a) put a prototype for the function to implement the command here
     - this function parses the command arguments, if any, out of
       the command string
     - then it calls the appropriate AFNI functions to carry out the command
 (b) put a {string,function} initialization into the 'dpair' array below,
     so that when the string is seen as the leading part of the command string,
     the function will be invoked with the rest of the command string as its
     argument.
--------------------------------------------------------------------------*****/

static int AFNI_drive_rescan_controller( char *code ) ;
static int AFNI_drive_switch_session( char *cmd ) ;
static int AFNI_drive_switch_anatomy( char *cmd ) ;
static int AFNI_drive_switch_function( char *cmd ) ;
static int AFNI_drive_open_window( char *cmd ) ;
static int AFNI_drive_close_window( char *cmd ) ;
static int AFNI_drive_quit( char *cmd ) ;
static int AFNI_drive_setenv( char *cmd ) ;

static int AFNI_drive_set_subbricks( char *cmd ) ;  /* 30 Nov 2005 */

static int AFNI_drive_save_jpeg( char *cmd ) ;      /* 28 Jul 2005 */
static int AFNI_drive_save_png ( char *cmd ) ;      /* 11 Dec 2006 */
static int AFNI_drive_save_raw ( char *cmd ) ;      /* 13 Nov 2007 */
static int AFNI_drive_save_rawmont( char *cmd ) ;   /* 13 Nov 2007 */
static int AFNI_drive_save_agif( char *cmd ) ;      /* 07 Dec 2006 */
static int AFNI_drive_save_mpeg( char *cmd ) ;      /* 07 Dec 2006 */
static int AFNI_drive_save_alljpeg( char *cmd ) ;   /* 07 Dec 2006 */
static int AFNI_drive_set_view( char *cmd ) ;       /* 28 Jul 2005 */
static int AFNI_drive_set_dicom_xyz( char *cmd ) ;  /* 28 Jul 2005 */
static int AFNI_drive_set_spm_xyz( char *cmd ) ;    /* 28 Jul 2005 */
static int AFNI_drive_set_ijk( char *cmd ) ;        /* 28 Jul 2005 */
static int AFNI_drive_set_xhairs( char *cmd ) ;     /* 28 Jul 2005 */
static int AFNI_drive_save_filtered( char *cmd ) ;  /* 14 Dec 2006 */
static int AFNI_drive_save_allpng( char *cmd ) ;    /* 15 Dec 2006 */

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
static int AFNI_define_colorscale      ( char *cmd ) ; /* 03 Feb 2003 */
static int AFNI_open_panel             ( char *cmd ) ; /* 05 Feb 2003 */
static int AFNI_drive_purge_memory     ( char *cmd ) ; /* 09 Dec 2004 */
static int AFNI_redisplay              ( char *cmd ) ;
static int AFNI_read_niml_file         ( char *cmd ) ; /* 01 Feb 2008 */
static int AFNI_drive_quiet_plugouts   ( char *cmd);   /* 15 Oct 2008 */
static int AFNI_drive_noisy_plugouts   ( char *cmd);   /* 15 Oct 2008 */

static int AFNI_trace                  ( char *cmd ) ; /* 04 Oct 2005 */

/*-----------------------------------------------------------------
  Set up the {string,function} pairs that choose how the
  different commands will be executed.  The way the strings are
  compared means that you can't have 2 distinct commands like
  "XXX" and "XXXYYY", since when the "XXXYYY" command was given,
  it might instead be matched to "XXX", and then the wrong function
  would be called, resulting in Galactic anarchy.
-------------------------------------------------------------------*/

typedef int dfunc(char *) ;  /* action functions */

  /* pairs of { command label , action function pointer } */

typedef struct { char *nam; dfunc *fun; } AFNI_driver_pair ;

  /* this array controls the dispatch of commands to functions */

static AFNI_driver_pair dpair[] = {
 { "RESCAN_THIS"      , AFNI_drive_rescan_controller } ,

 { "SET_SESSION"      , AFNI_drive_switch_session    } ,
 { "SWITCH_SESSION"   , AFNI_drive_switch_session    } ,
 { "SWITCH_DIRECTORY" , AFNI_drive_switch_session    } ,

 { "SET_ANATOMY"      , AFNI_drive_switch_anatomy    } ,
 { "SWITCH_ANATOMY"   , AFNI_drive_switch_anatomy    } ,
 { "SWITCH_UNDERLAY"  , AFNI_drive_switch_anatomy    } ,
 { "SET_UNDERLAY"     , AFNI_drive_switch_anatomy    } ,

 { "SET_FUNCTION"     , AFNI_drive_switch_function   } ,
 { "SWITCH_FUNCTION"  , AFNI_drive_switch_function   } ,
 { "SWITCH_OVERLAY"   , AFNI_drive_switch_function   } ,
 { "SET_OVERLAY"      , AFNI_drive_switch_function   } ,
 { "SET_SUBBRICKS"    , AFNI_drive_set_subbricks     } ,
 { "SET_SUB_BRICKS"   , AFNI_drive_set_subbricks     } ,

 { "PURGE_MEMORY"     , AFNI_drive_purge_memory      } ,

 { "OPEN_WINDOW"      , AFNI_drive_open_window       } ,
 { "ALTER_WINDOW"     , AFNI_drive_open_window       } ,
 { "CLOSE_WINDOW"     , AFNI_drive_close_window      } ,

 { "SAVE_JPEG"        , AFNI_drive_save_jpeg         } ,
 { "SAVE_PNG"         , AFNI_drive_save_png          } ,
 { "SAVE_RAWMONT"     , AFNI_drive_save_rawmont      } ,
 { "SAVE_RAW"         , AFNI_drive_save_raw          } ,
 { "SAVE_MPEG"        , AFNI_drive_save_mpeg         } ,
 { "SAVE_AGIF"        , AFNI_drive_save_agif         } ,
 { "SAVE_ALLJPEG"     , AFNI_drive_save_alljpeg      } ,
 { "SAVE_ALLPNG"      , AFNI_drive_save_allpng       } ,
 { "SAVE_FILTERED"    , AFNI_drive_save_filtered     } ,
 { "SET_VIEW"         , AFNI_drive_set_view          } ,
 { "SET_DICOM_XYZ"    , AFNI_drive_set_dicom_xyz     } ,
 { "SET_SPM_XYZ"      , AFNI_drive_set_spm_xyz       } ,
 { "SET_IJK"          , AFNI_drive_set_ijk           } ,
 { "SET_XHAIRS"       , AFNI_drive_set_xhairs        } ,
 { "SET_CROSSHAIRS"   , AFNI_drive_set_xhairs        } ,

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
 { "SEE_OVERLAY"        , AFNI_set_func_visible        } ,
 { "SET_FUNC_RESAM"     , AFNI_set_func_resam          } ,
 { "SLEEP"              , AFNI_sleeper                 } ,
 { "SETENV"             , AFNI_drive_setenv            } ,
 { "DEFINE_COLORSCALE"  , AFNI_define_colorscale       } ,
 { "DEFINE_COLOR_SCALE" , AFNI_define_colorscale       } ,
 { "OPEN_PANEL"         , AFNI_open_panel              } ,

 { "REDISPLAY"          , AFNI_redisplay               } ,
 { "REDRAW"             , AFNI_redisplay               } ,

 { "READ_NIML_FILE"     , AFNI_read_niml_file          } , /* 01 Feb 2008 */
 { "READ_NIML_DATA"     , AFNI_read_niml_file          } ,

 { "TRACE"              , AFNI_trace                   } , /* debugging */
 { "QUIET_PLUGOUTS"     , AFNI_drive_quiet_plugouts    } , /* 15 Oct 2008 */
 { "NOISY_PLUGOUTS"     , AFNI_drive_noisy_plugouts    } , /* 15 Oct 2008 */

 { NULL , NULL }  /* flag that we've reached the end times */
} ;

static int           num_epair = 0 ;      /* 04 Dec 2007 */
static AFNI_driver_pair *epair = NULL ;   /* dynamic commands */

/*----------------------------------------------------------------------*/

#if 0
static int junkfun( char *cmd )   /* 04 Dec 2007 */
{
  fprintf(stderr,"junkfun('%s')\n",cmd) ; return 0 ;
}
#endif

/*----------------------------------------------------------------------*/
/*! Accept a command, find the corresponding action function, call it.
    Return value is -1 if bad things happened, otherwise return is 0.   */

int AFNI_driver( char *cmdd )
{
   int clen , rval , ii , dd , dlen ;
   char *cmd , *dmd ;

ENTRY("AFNI_driver") ;

#if 0
   if( num_epair == 0 )
     AFNI_driver_register( "JUNK" , junkfun ) ;   /* 04 Dec 2007: testing */
#endif

   if( cmdd == NULL || *cmdd == '\0' ) RETURN(-1) ;  /* bad */

   if( strncmp(cmdd,"DRIVE_AFNI ",11) == 0 ) cmdd += 11 ;  /* 28 Dec 2006 */

   dmd = cmd = strdup(cmdd) ; clen = strlen(cmd) ;

   /*--- skip leading blanks ---*/

   for( ii=0 ; ii < clen ; ii++ )
     if( !isspace(cmd[ii]) ) break ;

   if( ii == clen ){ free(dmd); RETURN(-1); }  /* all blanks? */

   cmd += ii ; clen = strlen(cmd) ;

   /*--- 19 Dec 2002: trim trailing blanks ---*/

   for( ii=clen-1 ; ii > 0 && isspace(cmd[ii]) ; ii-- )
     cmd[ii] = '\0' ;

   clen = strlen(cmd) ;

   /*--- scan dpair list for command ---*/

   for( dd=0 ; dpair[dd].nam != NULL ; dd++ ){

     dlen = strlen(dpair[dd].nam) ;
     if( clen >= dlen                         &&
         strncmp(cmd,dpair[dd].nam,dlen) == 0   ){  /* found it */

       for( ii=dlen ; ii < clen ; ii++ )     /* skip blanks */
         if( !isspace(cmd[ii]) ) break ;     /* after command name */

       AFNI_block_rescan(1) ;                /* 10 Nov 2005 */
       rval = dpair[dd].fun( cmd+ii ) ;      /* execute command */
       if( rval < 0 )
         WARNING_message("Bad drive AFNI result from '%s'",cmd); /* 22 Feb 2007 */
       AFNI_block_rescan(0) ;
       free(dmd) ; RETURN(rval) ;
     }
   }

   /*--- scan epair list for command [04 Dec 2007] ---*/

   for( dd=0 ; dd < num_epair ; dd++ ){

     dlen = strlen(epair[dd].nam) ;
     if( clen >= dlen                         &&
         strncmp(cmd,epair[dd].nam,dlen) == 0   ){  /* found it */

       for( ii=dlen ; ii < clen ; ii++ )     /* skip blanks */
         if( !isspace(cmd[ii]) ) break ;     /* after command name */

       AFNI_block_rescan(1) ;                /* 10 Nov 2005 */
       rval = epair[dd].fun( cmd+ii ) ;      /* execute command */
       if( rval < 0 )
         WARNING_message("Bad drive AFNI result in '%s'",cmd) ;
       AFNI_block_rescan(0) ;
       free(dmd) ; RETURN(rval) ;
     }
   }

   /*--- didn't match user command to anything at all?!? ---*/

   ERROR_message("Can't drive AFNI with '%s'",cmd) ;  /* 22 Feb 2007 */

   free(dmd) ; RETURN(-1) ;  /* not in the lists */
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

   if( slf.dset_index < 0 ){   /* 18 Mar 2005 */
     MCW_idcode idcode ;
     MCW_strncpy( idcode.str , dname , MCW_IDSIZE ) ;
     slf = THD_dset_in_sessionlist( FIND_IDCODE, &idcode,
                                    GLOBAL_library.sslist,-1 );
   }

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
     a NUL or a '.' or a blank.
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
   if( !IM3D_OPEN(im3d) ){
     im3d = AFNI_find_open_controller() ;
     if( im3d == NULL ) RETURN(-1) ;
   }

   /* same callback as Rescan This */

   AFNI_rescan_CB( NULL , (XtPointer)im3d , NULL ) ;
   RETURN(0) ;
}

/* make AFNI quiet when communicating with plugouts */
static int AFNI_drive_quiet_plugouts( char *cmd)
{
   iochan_enable_perror(0) ;
   AFNI_plugout_verb(0);
}

/* make AFNI verbose when communicating with plugouts */
static int AFNI_drive_noisy_plugouts( char *cmd)
{
   iochan_enable_perror(1) ;
   AFNI_plugout_verb(1);
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

   AFNI_finalize_dataset_CB( im3d->vwid->view->choose_sess_pb,
                             (XtPointer)im3d ,  &cbs          ) ;

   RETURN(0) ;
}

/*------------------------------------------------------------------*/

void AFNI_set_anat_index( Three_D_View *im3d , int nuse )
{
   if( IM3D_OPEN(im3d)                    &&
       im3d->type == AFNI_3DDATA_VIEW     &&
       nuse >= 0                          &&
       nuse <  DSET_NVALS(im3d->anat_now) &&
       nuse != im3d->vinfo->anat_index      ){

     MCW_arrowval *tav = im3d->vwid->imag->time_index_av ;
     MCW_arrowval *aav = im3d->vwid->func->anat_buck_av ;
     if( im3d->vinfo->time_on ){
       AV_assign_ival( tav , nuse ) ;               /* set time_index */
       AFNI_time_index_CB( tav, (XtPointer) im3d ); /* will set anat_index */
     } else {
       AV_assign_ival( aav, nuse ) ;                /* set directly */
       AFNI_bucket_CB( aav, im3d ) ;
     }
   }
}

/*------------------------------------------------------------------*/

void AFNI_set_fim_index( Three_D_View *im3d , int nfun )
{
   if( IM3D_OPEN(im3d)                   &&
       im3d->type == AFNI_3DDATA_VIEW    &&
       nfun >= 0                         &&
       nfun <  DSET_NVALS(im3d->fim_now) &&
       nfun != im3d->vinfo->fim_index       ){
     MCW_arrowval *aav = im3d->vwid->func->fim_buck_av ;
     AV_assign_ival( aav, nfun ) ;
     AFNI_bucket_CB( aav, im3d ) ;
   }
}

/*------------------------------------------------------------------*/

void AFNI_set_thr_index( Three_D_View *im3d , int nthr )
{
   if( IM3D_OPEN(im3d)                   &&
       im3d->type == AFNI_3DDATA_VIEW    &&
       nthr >= 0                         &&
       nthr <  DSET_NVALS(im3d->fim_now) &&
       nthr != im3d->vinfo->thr_index       ){
     MCW_arrowval *aav = im3d->vwid->func->thr_buck_av ;
     AV_assign_ival( aav, nthr ) ;
     AFNI_bucket_CB( aav, im3d ) ;
   }
}

/*------------------------------------------------------------------*/

static int AFNI_drive_set_subbricks( char *cmd )
{
   int ic , dadd=2 , nanat=-1,nfun=-1,nthr=-1 ;
   Three_D_View *im3d ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   sscanf( cmd+dadd , "%d%d%d" , &nanat,&nfun,&nthr ) ;
   AFNI_set_anat_index( im3d , nanat ) ;
   AFNI_set_fim_index ( im3d , nfun  ) ;
   AFNI_set_thr_index ( im3d , nthr  ) ;
   RETURN(0) ;
}

/*------------------------------------------------------------------
  Set the anatomical dataset.  Input is of the form "A.prefix n".
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

static int AFNI_drive_switch_anatomy( char *cmd )
{
   int ic , dadd=2 , nuse=0 ;
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

   sscanf( cmd+dadd , "%244s%n" , dname , &nuse ) ;
   for( ic=strlen(dname)-1 ; ic >= 0 ; ic-- )
     if( isspace(dname[ic]) || iscntrl(dname[ic]) ) dname[ic] = '\0' ;
     else break ;

   if( strlen(dname) == 0 ) RETURN(-1) ;

   if( nuse > 0 ) sscanf( cmd+dadd+nuse+1 , "%d" , &nuse ) ;  /* 30 Nov 2005 */
   else           nuse = -1 ;

   /* find this dataset in current session of this controller */

   slf = THD_dset_in_session( FIND_PREFIX , dname , im3d->ss_now ) ;

   if( slf.dset_index < 0 ){   /* 18 Mar 2005 */
     MCW_idcode idcode ;
     MCW_strncpy( idcode.str , dname , MCW_IDSIZE ) ;
     slf = THD_dset_in_session( FIND_IDCODE , &idcode , im3d->ss_now ) ;
   }

   if( slf.dset_index < 0 ) RETURN(-1) ;

   cbs.ival = slf.dset_index ;

   /* same callback as Switch Anatomy */

   AFNI_finalize_dataset_CB( im3d->vwid->view->choose_anat_pb ,
                             (XtPointer)im3d ,  &cbs          ) ;

   AFNI_set_anat_index( im3d , nuse ) ;   /* 30 Nov 2005 */

   RETURN(0) ;
}

/*----------------------------------------------------------------------
  Set the functional dataset.  Input is of the form "A.prefix j k".
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

static int AFNI_drive_switch_function( char *cmd )
{
   int ic , dadd=2 , nuse=0 , nfun=-1,nthr=-1 ;
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

   sscanf( cmd+dadd , "%244s%n" , dname , &nuse ) ;
   for( ic=strlen(dname)-1 ; ic >= 0 ; ic-- )
      if( isspace(dname[ic]) || iscntrl(dname[ic]) ) dname[ic] = '\0' ;
      else break ;

   if( strlen(dname) == 0 ) RETURN(-1) ;

   if( nuse > 0 )                                    /* 30 Nov 2005 */
     sscanf( cmd+dadd+nuse+1 , "%d%d" , &nfun,&nthr ) ;

   /* find this dataset in current session of this controller */

   slf = THD_dset_in_session( FIND_PREFIX , dname , im3d->ss_now ) ;

   if( slf.dset_index < 0 ){   /* 18 Mar 2005 */
     MCW_idcode idcode ;
     MCW_strncpy( idcode.str , dname , MCW_IDSIZE ) ;
     slf = THD_dset_in_session( FIND_IDCODE , &idcode , im3d->ss_now ) ;
   }

   if( slf.dset_index < 0 ) RETURN(-1) ;

   cbs.ival = slf.dset_index ;

   /* same callback as Switch Function */

   AFNI_finalize_dataset_CB( im3d->vwid->view->choose_func_pb ,
                             (XtPointer)im3d ,  &cbs          ) ;

   AFNI_set_fim_index( im3d , nfun ) ;   /* 30 Nov 2005 */
   AFNI_set_thr_index( im3d , nthr ) ;

   RETURN(0) ;
}

/*---------------------------------------------------------------------*/
/* Macros for deciding on which window is in play -- allows for
   various mis-spellings that might naturally transpire -- 22 Feb 2007 */

#define HAS_axialimage(s)                                               \
 ( strstr((s),"axialim")!=NULL || strstr((s),"axial_im")!=NULL )

#define HAS_sagittalimage(s)                                            \
 ( strstr((s),"sagittalim")!=NULL || strstr((s),"sagittal_im")!=NULL || \
   strstr((s),"sagitalim") !=NULL || strstr((s),"sagital_im") !=NULL   )

#define HAS_coronalimage(s)                                             \
 ( strstr((s),"coronalim")!=NULL || strstr((s),"coronal_im")!=NULL )

#define HAS_axialgraph(s)                                               \
 ( strstr((s),"axialgr")!=NULL || strstr((s),"axial_gr")!=NULL )

#define HAS_sagittalgraph(s)                                            \
 ( strstr((s),"sagittalgr")!=NULL || strstr((s),"sagittal_gr")!=NULL || \
   strstr((s),"sagitalgr") !=NULL || strstr((s),"sagital_gr") !=NULL   )

#define HAS_coronalgraph(s)                                             \
 ( strstr((s),"coronalgr")!=NULL || strstr((s),"coronal_gr")!=NULL )

/*--------------------------------------------------------------------
  Open a window in the controller.
  Input is a string of the form A.sagittalimage, etc.
  Return value is 0 if good, -1 if bad.
----------------------------------------------------------------------*/

static int AFNI_drive_open_window( char *cmd )
{
   int ic ;
   Three_D_View *im3d ;
   char *cpt , *ccc ;
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

        if( HAS_axialimage(cmd) ){
          AFNI_view_xyz_CB( im3d->vwid->imag->image_xyz_pb, im3d, NULL ) ;
          isq = im3d->s123 ;

   } else if( HAS_sagittalimage(cmd) ){
          AFNI_view_xyz_CB( im3d->vwid->imag->image_yzx_pb, im3d, NULL ) ;
          isq = im3d->s231 ;

   } else if( HAS_coronalimage(cmd) ){
          AFNI_view_xyz_CB( im3d->vwid->imag->image_zxy_pb, im3d, NULL ) ;
          isq = im3d->s312 ;

   } else if( HAS_axialgraph(cmd) ){
          AFNI_view_xyz_CB( im3d->vwid->imag->graph_xyz_pb, im3d, NULL ) ;
          gra = im3d->g123 ;

   } else if( HAS_sagittalgraph(cmd) ){
          AFNI_view_xyz_CB( im3d->vwid->imag->graph_yzx_pb, im3d, NULL ) ;
          gra = im3d->g231 ;

   } else if( HAS_coronalgraph(cmd) ){
          AFNI_view_xyz_CB( im3d->vwid->imag->graph_zxy_pb, im3d, NULL ) ;
          gra = im3d->g312 ;
   }
   XmUpdateDisplay( im3d->vwid->top_shell ) ;

   /* find geom=..., if present */

   cpt = strstr(cmd,"geom=") ;
   if( cpt == NULL ) cpt = strstr(cmd,"geom:") ;
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
      if( cpt == NULL ) cpt = strstr(cmd,"ifrac:") ;
      if( cpt != NULL ){
        float ifrac = strtod( cpt+6 , NULL ) ;
        if( ifrac >= FRAC_MIN && ifrac <= 1.0 )
          drive_MCW_imseq( isq, isqDR_setifrac, (XtPointer)(&ifrac) ) ;
      }

      /* montage */

      cpt = strstr(cmd,"mont=") ;
      if( cpt == NULL ) cpt = strstr(cmd,"mont:") ;
      if( cpt != NULL ){
        int mww=-1 , mhh=-1 , msp=-1 , mgap=-1 , nn ;
        char mcol[128] = "\0" ;

        nn = sscanf( cpt+5 , "%dx%d:%d:%d:%s" , &mww,&mhh,&msp,&mgap,mcol );

        if( nn >= 2 && mww >= 1 && mww <= MONT_NMAX && mhh >= 1 && mhh <= MONT_NMAX ){
          int mp[5] ;
          mp[0] = mww ; mp[1] = mhh ; mp[2] = msp ; mp[3] = mgap ;
          mp[4] = DC_find_overlay_color(im3d->dc,mcol);
          drive_MCW_imseq( isq , isqDR_setmontage , (XtPointer)mp ) ;
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
      if( cpt == NULL ) cpt = strstr(cmd,"opacity:") ;
      if( cpt != NULL ){
        int opaval = -1 ;
        sscanf( cpt+8 , "%d" , &opaval ) ;
        drive_MCW_imseq( isq , isqDR_setopacity , (XtPointer)opaval ) ;
      }

      /* crop [03 May 2007] */

      cpt = strstr(cmd,"crop=") ;
      if( cpt == NULL ) cpt = strstr(cmd,"crop:") ;
      if( cpt != NULL ){
        int iar[4] ; char s1,s2,s3 ;
        iar[0] = iar[1] = iar[2] = iar[3] = -1 ;
        sscanf(cpt+5,"%d%c%d%c%d%c%d",
               iar+0 , &s1 , iar+1 , &s2 , iar+2 , &s3 , iar+3 ) ;
        if( iar[0] >= 0 && iar[1] >= iar[0] && iar[2] >= 0 && iar[3] >= iar[2] )
          drive_MCW_imseq( isq , isqDR_set_crop , (XtPointer)iar ) ;
      }

      /* keypress [18 Feb 2005] */

      ccc = cmd ;   /* 28 Dec 2006: allow multiple keypress= options */
      while(1){
        cpt = strstr(ccc,"keypress=") ;
        if( cpt == NULL ) cpt = strstr(ccc,"keypress:") ;
        if( cpt != NULL ){
          unsigned long key ;
          cpt += 9 ;
          if( *cpt == '\'' || *cpt == '\"' ) cpt++ ;
               if( strncmp(cpt,"XK_Left"     , 7) == 0 ) key = XK_Left     ;
          else if( strncmp(cpt,"XK_Right"    , 8) == 0 ) key = XK_Right    ;
          else if( strncmp(cpt,"XK_Down"     , 7) == 0 ) key = XK_Down     ;
          else if( strncmp(cpt,"XK_Up"       , 5) == 0 ) key = XK_Up       ;
          else if( strncmp(cpt,"XK_Page_Up"  ,10) == 0 ) key = XK_Page_Up  ;
          else if( strncmp(cpt,"XK_Page_Down",12) == 0 ) key = XK_Page_Down;
          else if( strncmp(cpt,"XK_Delete"   , 9) == 0 ) key = XK_Delete   ;
          else if( strncmp(cpt,"XK_Home"     , 7) == 0 ) key = XK_Home     ;
          else if( strncmp(cpt,"XK_F2"       , 5) == 0 ) key = XK_F2       ;
          else if( strncmp(cpt,"XK_F3"       , 5) == 0 ) key = XK_F3       ;
          else if( strncmp(cpt,"XK_F4"       , 5) == 0 ) key = XK_F4       ;
          else if( strncmp(cpt,"XK_F5"       , 5) == 0 ) key = XK_F5       ;
          else if( strncmp(cpt,"XK_F6"       , 5) == 0 ) key = XK_F6       ;
          else if( strncmp(cpt,"XK_F7"       , 5) == 0 ) key = XK_F7       ;
          else if( strncmp(cpt,"XK_F8"       , 5) == 0 ) key = XK_F8       ;
          else if( strncmp(cpt,"XK_F9"       , 5) == 0 ) key = XK_F9       ;
          else if( strncmp(cpt,"XK_F10"      , 6) == 0 ) key = XK_F10      ;
          else if( strncmp(cpt,"XK_F11"      , 6) == 0 ) key = XK_F11      ;
          else if( strncmp(cpt,"XK_F12"      , 6) == 0 ) key = XK_F12      ;
          else                                           key = *cpt        ;
          ISQ_handle_keypress( isq , key ) ;
        } else {
          break ;  /* break out of this while(1) loop */
        }
        ccc = cpt+1 ;  /* scan from here for the next keypress= option */
      } ;

   /*--- opened a graph viewer: maybe modify it ---*/

   } else if( gra != NULL ){

      /* geometry */

      if( gxx >= 0 && gyy >= 0 )
        XtVaSetValues( gra->fdw_graph, XmNx, gxx, XmNy, gyy, NULL ) ;
      if( gww > 0 && ghh > 0 )
        XtVaSetValues( gra->fdw_graph, XmNwidth, gww, XmNheight, ghh, NULL ) ;

      /* matrix */

      cpt = strstr(cmd,"matrix=") ;
      if( cpt == NULL ) cpt = strstr(cmd,"matrix:") ;
      if( cpt != NULL ){
        int mat = (int) strtod( cpt+7 , NULL ) ;
        if( mat > 0 )
          drive_MCW_grapher( gra , graDR_setmatrix , (XtPointer)mat ) ;
      }

      /* pinnum OR pintop */

      cpt = strstr(cmd,"pinnum=") ;
      if( cpt == NULL ) cpt = strstr(cmd,"pinnum:") ;
      if( cpt == NULL ) cpt = strstr(cmd,"pintop=") ;
      if( cpt == NULL ) cpt = strstr(cmd,"pintop:") ;
      if( cpt != NULL ){
        int pn = (int) strtod( cpt+7 , NULL ) ;
        if( pn >= MIN_PIN )
          drive_MCW_grapher( gra, graDR_setpinnum, (XtPointer)pn ) ;
      }

      /* pinbot [19 Mar 2004] */

      cpt = strstr(cmd,"pinbot=") ;
      if( cpt == NULL ) cpt = strstr(cmd,"pinbot:") ;
      if( cpt != NULL ){
        int pn = (int) strtod( cpt+7 , NULL ) ;
        if( pn > 0 )
          drive_MCW_grapher( gra, graDR_setpinbot, (XtPointer)pn ) ;
      }

      /* iconify [06 Aug 2002] */

      cpt = strstr(cmd,"iconi") ;
      if( cpt != NULL ){
        XIconifyWindow( XtDisplay(gra->fdw_graph) ,
                         XtWindow(gra->fdw_graph)  ,
                         gra->dc->screen_num   ) ;
      }

      /* keypress [18 Feb 2005] */

      ccc = cmd ;   /* 28 Dec 2006: allow multiple keypress= options */
      while(1){
        cpt = strstr(ccc,"keypress=") ;
        if( cpt == NULL ) cpt = strstr(ccc,"keypress:") ;
        if( cpt != NULL ){
          char buf[2] ;
          cpt += 9 ;
          if( *cpt == '\'' || *cpt == '\"' ) cpt++ ;
               if( strncmp(cpt,"XK_Left" ,7) == 0 ) buf[0] = '<'  ;
          else if( strncmp(cpt,"XK_Right",8) == 0 ) buf[0] = '>'  ;
          else if( strncmp(cpt,"XK_Down" ,7) == 0 ) buf[0] = 'Z'  ;
          else if( strncmp(cpt,"XK_Up"   ,5) == 0 ) buf[0] = 'z'  ;
          else                                      buf[0] = *cpt ;
          if( buf[0] == 'N' ) buf[0] = '\0' ;  /* bad key for this */
          buf[1] = '\0' ;
          GRA_timer_stop( gra ) ;
          GRA_handle_keypress( gra , buf , NULL ) ;
        } else {
          break ;  /* break out of this while(1) loop */
        }
        ccc = cpt+1 ;  /* scan from here for the next keypress= option */
      } ;

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

        if( HAS_axialimage(cmd) )
          drive_MCW_imseq( im3d->s123 , isqDR_destroy , NULL ) ;

   else if( HAS_sagittalimage(cmd) )
          drive_MCW_imseq( im3d->s231 , isqDR_destroy , NULL ) ;

   else if( HAS_coronalimage(cmd) )
          drive_MCW_imseq( im3d->s312 , isqDR_destroy , NULL ) ;

   else if( HAS_axialgraph(cmd) )
          drive_MCW_grapher( im3d->g123 , graDR_destroy , NULL ) ;

   else if( HAS_sagittalgraph(cmd) )
          drive_MCW_grapher( im3d->g231 , graDR_destroy , NULL ) ;

   else if( HAS_coronalgraph(cmd) )
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
   if( cpt == NULL ) cpt = strstr(cmd,"geom:") ;
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
  fprintf(stderr,"\n******* AFNI is commanded to quit! ") ; fflush(stderr) ;
  for( ii=0 ; ii < 7 ; ii++ ){ RWC_sleep(100); fprintf(stderr,"*"); fflush(stderr); }
  fprintf(stderr,"\n\n") ;
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
   if( cpt == NULL ) cpt = strstr(cmd,"geom:") ;
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
     NORMAL_cursorize( pbar->panew ) ;  /* 08 Apr 2005 */
   } else {
     int npane=pbar->num_panes , jm=pbar->mode ;
     float pmax=pbar->pval_save[npane][0][jm] ,
           pmin=pbar->pval_save[npane][npane][jm] ;
     PBAR_set_bigmode( pbar , 1 , pmin,pmax ) ;
     AFNI_inten_pbar_CB( pbar , im3d , 0 ) ;
     POPUP_cursorize( pbar->panew ) ;  /* 08 Apr 2005 */
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
                       (XtPointer)im3d , NULL ) ;
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
     if( cpt == NULL ) cpt = strstr(cmd+dadd,"ROTA:") ;
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
     NORMAL_cursorize( pbar->panew ) ;  /* 08 Apr 2005 */
   } else {    /* set the colorscale */
     float pmax, pmin ;
     pbar->bigset = 0 ;
     pmax = val ; pmin = (pbar->mode) ? 0.0 : -pmax ;
     PBAR_set_bigmode( pbar , 1 , pmin,pmax ) ;
     PBAR_set_bigmap( pbar , str ) ;
     rotate_MCW_pbar( pbar , rota ) ;  /* 07 Feb 2004 */
     if( flip ) PBAR_flip( pbar ) ;    /* 07 Feb 2004 */
     AFNI_inten_pbar_CB( pbar , im3d , 0 ) ;
     POPUP_cursorize( pbar->panew ) ;  /* 08 Apr 2005 */
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
/*! SETENV name value [restored from Limbo 08 Mar 2006] */

int AFNI_drive_setenv( char *cmd )
{
   char nam[256]="\0" , val[1024]="\0" , eqn[1280] , *eee ;

   if( cmd == NULL || strlen(cmd) < 3 ) return(-1) ;

   /*-- scan for the name and value --*/

   sscanf( cmd , "%255s %1023s" , nam , val ) ;

   /*-- if didn't get both, try "name=value" --*/

   if( nam[0] == '\0' || val[0] == '\0' && strchr(cmd,'=') != NULL ){
     char *ccc = strdup(cmd) ;
     eee = strchr(ccc,'=') ; *eee = ' ' ;
     sscanf( ccc , "%255s %1023s" , nam , val ) ;
     free((void *)ccc) ;
   }
   if( nam[0] == '\0' || val[0] == '\0' ) return(-1) ;

   /*-- set the actual environment variable; on some Unixes,
        must be in a permanent piece of memory, thus the strdup() --*/

   sprintf(eqn,"%s=%s",nam,val) ;
   eee = strdup(eqn) ; putenv(eee) ;

   /**------- special cases require special actions -------**/
   /** much of this code is directly from afni_pplug_env.c **/

   /*-- turn locking on or off --*/

   if( strcmp(nam,"AFNI_ALWAYS_LOCK") == 0 ){
          if( NOISH  (val) ) AFNI_lock_clear_CB (NULL,NULL,NULL) ;
     else if( YESSISH(val) ) AFNI_lock_setall_CB(NULL,NULL,NULL) ;
   }

   /*-- turn image global ranging on or off --*/

   else if( strcmp(nam,"AFNI_IMAGE_GLOBALRANGE") == 0 ){
     Three_D_View *im3d ; int ii,gbr=YESSISH(val) ;
     for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
       im3d = GLOBAL_library.controllers[ii] ;
       if( ! IM3D_OPEN(im3d) ) continue ;
       if( gbr ){
         AFNI_range_setter( im3d , im3d->s123 ) ;
         AFNI_range_setter( im3d , im3d->s231 ) ;
         AFNI_range_setter( im3d , im3d->s312 ) ;
         drive_MCW_imseq( im3d->s123 , isqDR_display , (XtPointer)(-1) ) ;
         drive_MCW_imseq( im3d->s231 , isqDR_display , (XtPointer)(-1) ) ;
         drive_MCW_imseq( im3d->s312 , isqDR_display , (XtPointer)(-1) ) ;
       } else {
         drive_MCW_imseq( im3d->s123 , isqDR_setrange , (XtPointer)NULL ) ;
         drive_MCW_imseq( im3d->s231 , isqDR_setrange , (XtPointer)NULL ) ;
         drive_MCW_imseq( im3d->s312 , isqDR_setrange , (XtPointer)NULL ) ;
       }
     }
   }

   /*-- display coordinate order --*/

   else if( strcmp(nam,"AFNI_ORIENT") == 0 ){
     MCW_strncpy(GLOBAL_argopt.orient_code,val,4) ;
     THD_coorder_fill( GLOBAL_argopt.orient_code , &GLOBAL_library.cord ) ;
     PLUTO_force_redisplay() ;
   }

   /*-- display edges only of overlay blobs --*/

   else if( strcmp(nam,"AFNI_EDGIZE_OVERLAY") == 0 ||
            strcmp(nam,"AFNI_OVERLAY_ZERO")   == 0   ){
      PLUTO_force_redisplay() ;
   }

   /*-- compression mode --*/

   else if( strcmp(nam,"AFNI_COMPRESSOR") == 0 ){
     int meth = PLUTO_string_index( val, NUM_COMPRESS_elist, COMPRESS_elist );
     if( meth < 0 ) meth = COMPRESS_NONE ;
     THD_set_write_compression(meth) ;
   }

   /*-- session name trail length --*/

#ifdef USE_SESSTRAIL
   else if( strcmp(nam,"AFNI_SESSTRAIL") == 0 ){
     int tt ; THD_session *sess ; char *str ;
     int ii = SESSTRAIL ; SESSTRAIL = (int)strtod(val,NULL) ;
     if( ii == SESSTRAIL ) return ;
     for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ )
      if( IM3D_OPEN(GLOBAL_library.controllers[ii]) )
       AFNI_set_window_titles( GLOBAL_library.controllers[ii] ) ;
     for( ii=0 ; ii < GLOBAL_library.sslist->num_sess ; ii++ ){
       sess = GLOBAL_library.sslist->ssar[ii] ;
       str  = THD_trailname(sess->sessname,SESSTRAIL) ;
       tt   = 1+strlen(str) - THD_MAX_NAME ; if( tt < 0 ) tt = 0 ;
       strcpy( sess->lastname , str+tt ) ;
     }
   }
#endif

   /*-- left on the left? --*/

   else if( strcmp(nam,"AFNI_LEFT_IS_LEFT") == 0 ){
     GLOBAL_argopt.left_is_left = YESSISH(val) ;
   }

   return(0) ;
}

/*------------------------------------------------------------------*/
/*! REDISPLAY */

static int AFNI_redisplay( char *cmd )
{
#if 0
   int cc ;
   Three_D_View *qq3d ;

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){
      qq3d = GLOBAL_library.controllers[cc] ;
      if( ! IM3D_OPEN(qq3d) ) continue ;
      AFNI_set_viewpoint( qq3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
   }
#else
   PLUTO_force_redisplay() ;
#endif
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

/*--------------------------------------------------------------------*/
/*! Function for saving images in various formats. */

static int AFNI_drive_save_1image( char *cmd , int mode , char *suf )
{
   int ic , dadd=2 , imm ;
   Three_D_View *im3d ;
   char junk[256] , fname[599] ;
   MCW_imseq   *isq=NULL ;
   MCW_grapher *gra=NULL ;

ENTRY("AFNI_drive_save_1image") ;

   /* make sure the controller itself is open */

   if( strlen(cmd) < 3 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ){
     ERROR_message("Image save '%s': controller not open",cmd); RETURN(-1);
   }

   /* extract the filename to save into */

   junk[0] = fname[0] = '\0' ;
   sscanf( cmd+dadd , "%255s%255s" , junk , fname ) ;
   if( junk[0] == '\0' || fname[0] == '\0' ){
     ERROR_message("Image save '%s': something is missing",cmd); RETURN(-1);
     RETURN(-1) ;
   }
   if( fname[0] == '\'' || fname[0] == '\"' ){
     char qt=fname[0] , *q1 , *q2 ;
     q1 = strchr(cmd+dadd,qt)+1 ;
     q2 = strchr(q1,qt) ; if( q2 == NULL ) q2 = cmd+strlen(cmd) ;
     if( (imm=q2-q1) > 0 && imm < 599 ){
       strncpy(fname,q1,imm) ; fname[imm] = '\0' ;
     } else {
       ERROR_message("Image save '%s': filename is bad",cmd); RETURN(-1);
     }
   }

   /* find graph or image window */

        if( HAS_axialimage   (cmd+dadd) ) isq = im3d->s123 ;
   else if( HAS_sagittalimage(cmd+dadd) ) isq = im3d->s231 ;
   else if( HAS_coronalimage (cmd+dadd) ) isq = im3d->s312 ;
   else if( HAS_axialgraph   (cmd+dadd) ) gra = im3d->g123 ;
   else if( HAS_sagittalgraph(cmd+dadd) ) gra = im3d->g231 ;
   else if( HAS_coronalgraph (cmd+dadd) ) gra = im3d->g312 ;

   XmUpdateDisplay( im3d->vwid->top_shell ) ;

   if( suf != NULL && *suf != '\0' && !STRING_HAS_SUFFIX_CASE(fname,suf) )
     strcat(fname,suf) ;

   if( isq != NULL ){
     switch( mode ){
       case RAWMONT_MODE:  imm = isqDR_save_rawmont ; break ;
       case RAW_MODE:      imm = isqDR_save_raw     ; break ;
       case PNG_MODE:      imm = isqDR_save_png     ; break ;
       case JPEG_MODE:     imm = isqDR_save_jpeg    ; break ;
       default:            imm = isqDR_save_filtered; break ;
     }
     drive_MCW_imseq( isq, imm, (XtPointer)fname ) ;
   } else if( gra != NULL ){
     if( mode == RAW_MODE || mode == RAWMONT_MODE ){
       ERROR_message("Can't save 'raw' image from a graph!") ;
     } else {
       if( mode == -1 ){  /* start with '|' means a pipe */
         memmove( fname+1, fname, strlen(fname)+1 ); fname[0] = '|';
       }
       GRA_file_pixmap( gra , fname ) ;
     }
   } else {
     ERROR_message("Image save '%s': don't understand windowname",cmd) ;
     RETURN(-1) ;
   }

   RETURN(0) ;
}

/*--------------------------------------------------------------------*/
/*! SAVE_JPEG [c.]imagewindowname fname */

static int AFNI_drive_save_jpeg( char *cmd )
{
   return AFNI_drive_save_1image( cmd , JPEG_MODE , ".jpg" ) ;
}

/*------------*/
/*! SAVE_PNG */

static int AFNI_drive_save_png( char *cmd )
{
   return AFNI_drive_save_1image( cmd , PNG_MODE , ".png" ) ;
}

/*------------*/
/*! SAVE_RAW */

static int AFNI_drive_save_raw( char *cmd )
{
   return AFNI_drive_save_1image( cmd , RAW_MODE , NULL ) ;
}

/*------------*/
/*! SAVE_RAWMONT */

static int AFNI_drive_save_rawmont( char *cmd )
{
   return AFNI_drive_save_1image( cmd , RAWMONT_MODE , NULL ) ;
}

/*-----------------*/
/*! SAVE_FILTERED */

static int AFNI_drive_save_filtered( char *cmd )
{
   return AFNI_drive_save_1image( cmd , -1 , NULL ) ;
}

/*--------------------------------------------------------------------*/
/*! Save all the images in an image viewer. */

static int AFNI_drive_save_allimages( char *cmd , int mode )
{
   int ic , dadd=2 ;
   Three_D_View *im3d ;
   char junk[256] , fname[288] ;
   MCW_imseq   *isq=NULL ;
   MCW_grapher *gra=NULL ;
   int imode ;

ENTRY("AFNI_drive_save_allimages") ;

   /* make sure the controller itself is open */

   if( strlen(cmd) < 3 ) RETURN(-1) ;

   switch( mode ){
     case AGIF_MODE: imode = isqDR_save_agif    ; break ;
     case MPEG_MODE: imode = isqDR_save_mpeg    ; break ;
     case JPEG_MODE: imode = isqDR_save_jpegall ; break ;
     case PNG_MODE : imode = isqDR_save_pngall  ; break ;
     default:
       ERROR_message("Saving All Images: illegal code=%d?",mode); RETURN(-1);
   }

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ){
     ERROR_message("Saving All Images '%s': Controller not open",cmd); RETURN(-1);
   }

   /* extract the filename to save into */

   junk[0] = fname[0] = '\0' ;
   sscanf( cmd+dadd , "%255s%255s" , junk , fname ) ;
   if( junk[0] == '\0' || fname[0] == '\0' ){
     ERROR_message("Saving All Images '%s': something is missing",cmd); RETURN(-1);
   }

   /* find graph or image window */

        if( HAS_axialimage   (cmd+dadd) ) isq = im3d->s123 ;
   else if( HAS_sagittalimage(cmd+dadd) ) isq = im3d->s231 ;
   else if( HAS_coronalimage (cmd+dadd) ) isq = im3d->s312 ;
   else if( HAS_axialgraph   (cmd+dadd) ) gra = im3d->g123 ;
   else if( HAS_sagittalgraph(cmd+dadd) ) gra = im3d->g231 ;
   else if( HAS_coronalgraph (cmd+dadd) ) gra = im3d->g312 ;

   XmUpdateDisplay( im3d->vwid->top_shell ) ;

   if( isq != NULL ){
     drive_MCW_imseq( isq, imode , (XtPointer)fname ) ;
   } else if( gra != NULL ){
     ERROR_message("Saving All Images '%s': graph windows not implemented",cmd);
     RETURN(-1) ;
   } else {
     ERROR_message("Saving All Images '%s': unknown window name",cmd) ;
     RETURN(-1) ;
   }

   RETURN(0) ;
}

/*--------------------------------------*/
/*! SAVE_AGIF [c.]imagewindowname fname */

static int AFNI_drive_save_agif( char *cmd )
{
  return AFNI_drive_save_allimages( cmd , AGIF_MODE ) ;
}

/*--------------------------------------*/
/*! SAVE_MPEG [c.]imagewindowname fname */

static int AFNI_drive_save_mpeg( char *cmd ){
  return AFNI_drive_save_allimages( cmd , MPEG_MODE ) ;
}

/*-----------------------------------------*/
/*! SAVE_ALLJPEG [c.]imagewindowname fname */

static int AFNI_drive_save_alljpeg( char *cmd ){
  return AFNI_drive_save_allimages( cmd , JPEG_MODE ) ;
}

/*-----------------------------------------*/
/*! SAVE_ALLPNG [c.]imagewindowname fname */

static int AFNI_drive_save_allpng( char *cmd ){
  return AFNI_drive_save_allimages( cmd , PNG_MODE ) ;
}

/*--------------------------------------------------------------------*/
/*! SET_VIEW [c.]viewname */

static int AFNI_drive_set_view( char *cmd )
{
   int ic , vv=-1 ;
   Three_D_View *im3d ;

   if( strlen(cmd) < 3 ) return -1;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ) ic = 0 ;
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

        if( strstr(cmd,"orig") != NULL ) vv = VIEW_ORIGINAL_TYPE ;
   else if( strstr(cmd,"acpc") != NULL ) vv = VIEW_ACPCALIGNED_TYPE ;
   else if( strstr(cmd,"tlrc") != NULL ) vv = VIEW_TALAIRACH_TYPE ;
   else                                  return -1 ;

   if( vv == im3d->vinfo->view_type ) return 0 ;  /* nothing to do */
   if( !XtIsSensitive(im3d->vwid->view->view_bbox->wbut[vv]) ) return -1 ;

   MCW_set_bbox( im3d->vwid->view->view_bbox , 1 << vv ) ;
   AFNI_switchview_CB( NULL , (XtPointer)im3d , NULL ) ;
   return 0 ;
}

/*--------------------------------------------------------------------*/
/*! SET_DICOM_XYZ [c.] x y z */

static int AFNI_drive_set_dicom_xyz( char *cmd )
{
   int ic , dadd=2 ;
   Three_D_View *im3d ;
   float x,y,z ;

   if( strlen(cmd) < 3 ) return -1;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

   ic = sscanf( cmd+dadd , "%f%f%f" , &x,&y,&z ) ;
   if( ic < 3 ) return -1 ;
   AFNI_jumpto_dicom( im3d , x,y,z ) ;
   return 0 ;
}

/*--------------------------------------------------------------------*/
/*! SET_SPM_XYZ [c.] x y z */

static int AFNI_drive_set_spm_xyz( char *cmd )
{
   int ic , dadd=2 ;
   Three_D_View *im3d ;
   float x,y,z ;

   if( strlen(cmd) < 3 ) return -1;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

   ic = sscanf( cmd+dadd , "%f%f%f" , &x,&y,&z ) ;
   if( ic < 3 ) return -1 ;
   AFNI_jumpto_dicom( im3d , -x,-y,z ) ;
   return 0 ;
}

/*--------------------------------------------------------------------*/
/*! SET_IJK [c.] i j k */

static int AFNI_drive_set_ijk( char *cmd )
{
   int ic , dadd=2 ;
   Three_D_View *im3d ;
   int i,j,k ;

   if( strlen(cmd) < 3 ) return -1;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

   ic = sscanf( cmd+dadd , "%d%d%d" , &i,&j,&k ) ;
   if( ic < 3 ) return -1 ;
   AFNI_set_viewpoint( im3d , i,j,k , REDISPLAY_ALL ) ;
   return 0 ;
}

/*--------------------------------------------------------------------*/
/*! SET_XHAIRS [c.]code */

static int AFNI_drive_set_xhairs( char *cmd )
{
   int ic , dadd=2 , hh=-1 ;
   Three_D_View *im3d ;
   int i,j,k ;

   if( strlen(cmd) < 3 ) return -1;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

        if( strstr(cmd,"OFF")    != NULL ) hh = 0 ;
   else if( strstr(cmd,"SINGLE") != NULL ) hh = 1 ;
   else if( strstr(cmd,"MULTI")  != NULL ) hh = 2 ;
   else if( strstr(cmd,"LR_AP")  != NULL ) hh = 3 ;
   else if( strstr(cmd,"LR_IS")  != NULL ) hh = 4 ;
   else if( strstr(cmd,"AP_IS")  != NULL ) hh = 5 ;
   else if( strstr(cmd,"LR")     != NULL ) hh = 6 ;
   else if( strstr(cmd,"AP")     != NULL ) hh = 7 ;
   else if( strstr(cmd,"IS")     != NULL ) hh = 8 ;
   else                                    return -1 ;

   AV_assign_ival( im3d->vwid->imag->crosshair_av, hh ) ;
   AFNI_crosshair_visible_CB( im3d->vwid->imag->crosshair_av, (XtPointer)im3d );
   return 0 ;
}

/*--------------------------------------------------------------------*/
/*! TRACE {YES | NO} */

static int AFNI_trace( char *cmd )
{
#ifdef USE_TRACING
   DBG_trace = (YESSISH(cmd)) ? 2 : 0 ;
#endif
   return 0 ;
}

/*--------------------------------------------------------------------*/

static int AFNI_read_niml_file( char *cmd )  /* 01 Feb 2008 */
{
   void *nini ;
   nini = NI_read_element_fromfile(cmd) ;
   if( nini != NULL ){
     AFNI_process_NIML_data(0,nini,-1) ;
     NI_free_element(nini) ;
     return 0 ;
   }
   return -1;
}

/*--------------------------------------------------------------------*/

void AFNI_driver_register( char *cmd , int (*cbfun)(char *) )
{
   int nn ; char *cpt ;

   if( cmd == NULL || *cmd == '\0' || cbfun == NULL ) return ;
   for( cpt=cmd ; *cpt != '\0' ; cpt++ ){
     if( isspace(*cpt) ){
       ERROR_message("Illegal driver registration '%s' (spaces not allowed)",cmd); return;
     }
   }
   for( nn=0 ; dpair[nn].nam != NULL ; nn++ ){
     if( strcmp(cmd,dpair[nn].nam) == 0 ){
       ERROR_message("Illegal driver built-in duplication '%s'",cmd); return;
     }
   }
   for( nn=0 ; nn < num_epair; nn++ ){
     if( strcmp(cmd,epair[nn].nam) == 0 ){
       ERROR_message("Illegal driver custom duplication '%s'",cmd); return;
     }
   }

   nn = num_epair + 1 ;
   epair = (AFNI_driver_pair *)realloc( (void *)epair , sizeof(AFNI_driver_pair)*nn) ;
   epair[num_epair].nam = strdup(cmd) ;
   epair[num_epair].fun = cbfun ;
   num_epair            = nn ;
   return ;
}
