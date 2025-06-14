#include "afni.h"
#include "afni_plugout.h"
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
static int AFNI_drive_quitt( char *cmd ) ;
static int AFNI_drive_setenv( char *cmd ) ;
static int AFNI_drive_getenv( char *cmd );          /* 20 Oct 2008 */

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
static int AFNI_drive_get_dicom_xyz( char *cmd ) ;  /* 07 Oct 2010 */
static int AFNI_drive_get_olay_val( char *cmd ) ;   /* 11 Sep 2020 */
static int AFNI_drive_get_ulay_val( char *cmd ) ;   /* 11 Sep 2020 */
static int AFNI_drive_get_thr_val( char *cmd ) ;    /* 11 Sep 2020 */
static int AFNI_drive_get_olay_name( char *cmd ) ;  /* 11 Sep 2020 */
static int AFNI_drive_get_ulay_name( char *cmd ) ;  /* 11 Sep 2020 */
static int AFNI_drive_set_spm_xyz( char *cmd ) ;    /* 28 Jul 2005 */
static int AFNI_drive_set_ijk( char *cmd ) ;        /* 28 Jul 2005 */
static int AFNI_drive_get_ijk( char *cmd ) ;        /* 07 Oct 2010 */
static int AFNI_drive_set_ijk_index( char *cmd ) ;  /* 29 Jul 2010 */
static int AFNI_drive_set_xhairs( char *cmd ) ;     /* 28 Jul 2005 */
static int AFNI_drive_xhairs_gap( char *cmd ) ;     /* 27 Feb 2021 */
static int AFNI_drive_save_filtered( char *cmd ) ;  /* 14 Dec 2006 */
static int AFNI_drive_save_allpng( char *cmd ) ;    /* 15 Dec 2006 */
static int AFNI_drive_write_underlay( char *cmd ) ; /* 16 Jun 2014 */
static int AFNI_drive_write_overlay( char *cmd ) ;  /* 16 Jun 2014 */
static int AFNI_drive_write_cont_spxhelp(char *cmd);/* 08 Apr 2015 */
static int AFNI_drive_snap_cont( char *cmd );       /* 08 Apr 2015 */
static int AFNI_drive_snap_viewer( char *cmd );     /* 23 May 2018 */
static int AFNI_drive_underlay_range( char *cmd );  /* 23 Jul 2018 */
static int AFNI_drive_nothing( char *cmd );         /* 23 Jul 2018 */

static FILE * AFNI_drive_get_outstream(void);       /* 02 Jun 2015 */
static int AFNI_drive_set_outstream(char *outfile); /* 02 Jun 2015 */

static int AFNI_drive_system( char *cmd ) ;         /* 19 Dec 2002 */
static int AFNI_drive_chdir ( char *cmd ) ;         /* 19 Dec 2002 */

static int AFNI_drive_instacorr( char *cmd ) ;      /* 20 Oct 2010 */

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
static int AFNI_drive_pbar_saveim      ( char *cmd ) ; /* 19 Oct 2018 */
static int AFNI_set_func_autorange     ( char *cmd ) ; /* 17 Jan 2003 */
static int AFNI_set_func_range         ( char *cmd ) ; /* 21 Jan 2003 */
static int AFNI_set_func_visible       ( char *cmd ) ; /* 21 Jan 2003 */
static int AFNI_set_func_resam         ( char *cmd ) ; /* 21 Jan 2003 */
static int AFNI_sleeper                ( char *cmd ) ; /* 22 Jan 2003 */
static int AFNI_define_colorscale      ( char *cmd ) ; /* 03 Feb 2003 */
static int AFNI_open_panel             ( char *cmd ) ; /* 05 Feb 2003 */
static int AFNI_close_panel            ( char *cmd ) ; /* 14 Apr 2017 */
static int AFNI_drive_purge_memory     ( char *cmd ) ; /* 09 Dec 2004 */
static int AFNI_redisplay              ( char *cmd ) ;
static int AFNI_read_niml_file         ( char *cmd ) ; /* 01 Feb 2008 */
static int AFNI_drive_quiet_plugouts   ( char *cmd ) ; /* 15 Oct 2008 */
static int AFNI_drive_noisy_plugouts   ( char *cmd ) ; /* 15 Oct 2008 */
static int AFNI_set_func_percentile    ( char *cmd ) ; /* 27 Apr 2012 */
static int AFNI_set_func_alpha         ( char *cmd ) ; /* 10 Dec 2014 */
static int AFNI_set_func_boxed         ( char *cmd ) ; /* 05 Nov 2018 */

static int AFNI_trace                  ( char *cmd ) ; /* 04 Oct 2005 */

/*-----------------------------------------------------------------
  Set up the {string,function} pairs that choose how the
  different commands will be executed.  The way the strings are
  compared means that you can't have 2 distinct commands like
  "XXX" and "XXXYYY", since when the "XXXYYY" command was given,
  it might instead be matched to "XXX", and then the wrong function
  would be called, resulting in Galactic anarchy (at best).
-------------------------------------------------------------------*/

typedef int dfunc(char *) ;  /* action functions */

  /* pairs of { command label , action function pointer } */

typedef struct { char *nam; dfunc *fun; } AFNI_driver_pair ;

  /* this array controls the dispatch of commands to functions */
     /* note the string compare below checks only the beginning of string
        matches command - so SET_XHAIRS_GAP can match SET_XHAIRS_GAP
        and SET_XHAIRS. The order of comparison then matters.
        Either change here or set the order in command list or
        make sure name is unique */

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

 { "OPEN_WINDOW"      , AFNI_drive_open_window       } ,  /* synonyms */
 { "ALTER_WINDOW"     , AFNI_drive_open_window       } ,
 { "SET_WINDOW"       , AFNI_drive_open_window       } ,

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
 { "GET_DICOM_XYZ"    , AFNI_drive_get_dicom_xyz     } ,
 { "GET_OLAY_VAL"     , AFNI_drive_get_olay_val      } ,
 { "GET_ULAY_VAL"     , AFNI_drive_get_ulay_val      } ,
 { "GET_THR_VAL"      , AFNI_drive_get_thr_val       } ,
 { "GET_ULAY_NAME"    , AFNI_drive_get_ulay_name     } ,
 { "GET_OLAY_NAME"    , AFNI_drive_get_olay_name     } ,
 { "SET_SPM_XYZ"      , AFNI_drive_set_spm_xyz       } ,
 { "SET_IJK"          , AFNI_drive_set_ijk           } ,
 { "GET_IJK"          , AFNI_drive_get_ijk           } ,
 { "SET_INDEX"        , AFNI_drive_set_ijk_index     } ,
 { "SET_XHAIRS_GAP"   , AFNI_drive_xhairs_gap        } ,
 { "SET_XHAIRS"       , AFNI_drive_set_xhairs        } ,
 { "SET_CROSSHAIRS"   , AFNI_drive_set_xhairs        } ,
 { "SET_XHAIR_GAP"    , AFNI_drive_xhairs_gap        } ,  /* repeat -s */
 { "SET_OUTPLUG"      , AFNI_drive_set_outstream     } ,
 { "OPEN_GRAPH_XY"    , AFNI_drive_open_graph_xy     } ,
 { "CLOSE_GRAPH_XY"   , AFNI_drive_close_graph_xy    } ,
 { "CLEAR_GRAPH_XY"   , AFNI_drive_clear_graph_xy    } ,
 { "ADDTO_GRAPH_XY"   , AFNI_drive_addto_graph_xy    } ,

 { "OPEN_GRAPH_1D"    , AFNI_drive_open_graph_1D     } ,
 { "CLOSE_GRAPH_1D"   , AFNI_drive_close_graph_1D    } ,
 { "CLEAR_GRAPH_1D"   , AFNI_drive_clear_graph_1D    } ,
 { "ADDTO_GRAPH_1D"   , AFNI_drive_addto_graph_1D    } ,

 { "SET_GRAPH_GEOM"   , AFNI_drive_geom_graph        } ,

 { "QUITT"            , AFNI_drive_quitt             } ,
 { "QQUIT"            , AFNI_drive_quitt             } ,
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
 { "PBAR_SAVEIM"        , AFNI_drive_pbar_saveim       } ,
 { "SET_FUNC_AUTORANGE" , AFNI_set_func_autorange      } ,
 { "SET_FUNC_RANGE"     , AFNI_set_func_range          } ,
 { "SET_FUNC_VISIBLE"   , AFNI_set_func_visible        } ,
 { "SET_FUNC_ALPHA"     , AFNI_set_func_alpha          } ,
 { "SET_FUNC_BOXED"     , AFNI_set_func_boxed          } ,
 { "SEE_OVERLAY"        , AFNI_set_func_visible        } ,
 { "SET_FUNC_RESAM"     , AFNI_set_func_resam          } ,
 { "SLEEP"              , AFNI_sleeper                 } ,
 { "SETENV"             , AFNI_drive_setenv            } ,
 { "GETENV"             , AFNI_drive_getenv            } , /* 20 Oct 2008,rcr */
 { "DEFINE_COLORSCALE"  , AFNI_define_colorscale       } ,
 { "DEFINE_COLOR_SCALE" , AFNI_define_colorscale       } ,
 { "OPEN_PANEL"         , AFNI_open_panel              } ,
 { "CLOSE_PANEL"        , AFNI_close_panel             } , /* 14 Apr 2017 */

 { "INSTACORR"          , AFNI_drive_instacorr         } , /* 20 Oct 2010 */

 { "REDISPLAY"          , AFNI_redisplay               } ,
 { "REDRAW"             , AFNI_redisplay               } ,

 { "READ_NIML_FILE"     , AFNI_read_niml_file          } , /* 01 Feb 2008 */
 { "READ_NIML_DATA"     , AFNI_read_niml_file          } ,

 { "TRACE"              , AFNI_trace                   } , /* debugging */
 { "QUIET_PLUGOUTS"     , AFNI_drive_quiet_plugouts    } , /* 15 Oct 2008 */
 { "NOISY_PLUGOUTS"     , AFNI_drive_noisy_plugouts    } , /* 15 Oct 2008 */
 { "SET_FUNC_PERCENTILE", AFNI_set_func_percentile     } , /* 27 Apr 2012,zss */

 { "SAVE_OVERLAY"       , AFNI_drive_write_overlay     } , /* 16 Jun 2014 */
 { "WRITE_OVERLAY"      , AFNI_drive_write_overlay     } ,
 { "SAVE_UNDERLAY"      , AFNI_drive_write_underlay    } ,
 { "WRITE_UNDERLAY"     , AFNI_drive_write_underlay    } ,
 { "WRITE_UNDERLAY"     , AFNI_drive_write_underlay    } ,
 { "WRITE_CONT_SPX_HELP", AFNI_drive_write_cont_spxhelp} ,
 { "SNAP_CONT"          , AFNI_drive_snap_cont         } ,
 { "SNAP_VIEWER"        , AFNI_drive_snap_viewer       } ,

 { "SET_ULAY_RANGE"     , AFNI_drive_underlay_range    } , /* 23 Jul 2018 */
 { "DO_NOTHING"         , AFNI_drive_nothing           } ,

 { NULL , NULL }  /* flag that we've reached the end times */
} ;

static int           num_epair = 0 ;      /* 04 Dec 2007 */
static AFNI_driver_pair *epair = NULL ;   /* dynamic commands */
static FILE *afniout = NULL;             /* no default output stream */

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

   /* change output from stdout to something else maybe */
   AFNI_drive_get_outstream();                             /* 02 Jun 2015 DRG */

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

   /*--- 24 Jun 2011: patch obvious stupidities ---*/

        if( strncmp(cmd,"INSTACORR_SET" ,13) == 0 ) cmd[9] = ' ' ;
   else if( strncmp(cmd,"INSTACORR_INIT",14) == 0 ) cmd[9] = ' ' ;

   /*--- scan dpair list for command ---*/

   for( dd=0 ; dpair[dd].nam != NULL ; dd++ ){

     dlen = strlen(dpair[dd].nam) ;
     /* note the string compare below checks only the beginning of string
        matches command - so SET_XHAIRS_GAP can match SET_XHAIRS_GAP
        and SET_XHAIRS. The order of comparison then matters.
        Either change here or set the order in command list or
        make sure name is unique */
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

   ERROR_message( "Can't drive AFNI with '%s'\n"
         "  For command options see README.driver or try:\n"
         "       apsearch -view_readme driv \n",cmd) ;  /* 22 Feb 2007 */

   free(dmd) ; RETURN(-1) ;  /* not in the lists */
}

/*---------------------------------------------------------------*/

int AFNI_drive_nothing( char *cmd ) /* not the hardest code to write */
{
  RETURN(0) ;
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
/*     THD_3dim_dataset **dss =
       GLOBAL_library.sslist->ssar[slf.sess_index]->dsset_xform_table[slf.dset_index] ;*/
     THD_3dim_dataset *dss;
     for( ic=0 ; ic <= LAST_VIEW_TYPE ; ic++ ) {
        dss = GET_SESSION_DSET(GLOBAL_library.sslist->ssar[slf.sess_index], slf.dset_index,ic) ;
        if (dss != NULL)
           PURGE_DSET( dss ) ;
     }
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
   RETURN(0) ;
}

/* make AFNI verbose when communicating with plugouts */
static int AFNI_drive_noisy_plugouts( char *cmd)
{
   iochan_enable_perror(1) ;
   AFNI_plugout_verb(1);
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

   AFNI_finalize_dataset_CB( im3d->vwid->view->choose_sess_pb,
                             (XtPointer)im3d ,  &cbs          ) ;

   RETURN(0) ;
}

/*------------------------------------------------------------------*/

void AFNI_set_anat_index( Three_D_View *im3d , int nuse )
{
   if( IM3D_OPEN(im3d)                    &&
       im3d->type == AFNI_3DDATA_VIEW     &&
       ISVALID_DSET(im3d->anat_now)       &&
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
       ISVALID_DSET(im3d->fim_now)       &&
       nfun >= 0                         &&
       nfun <  DSET_NVALS(im3d->fim_now) &&
       nfun != im3d->vinfo->fim_index       ){
     MCW_arrowval *aav = im3d->vwid->func->fim_buck_av ;
     AV_assign_ival( aav, nfun ) ;
     IM3D_CLEAR_TMASK(im3d) ;
     IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
     AFNI_bucket_CB( aav, im3d ) ;
   }
}

/*------------------------------------------------------------------*/

void AFNI_set_thr_index( Three_D_View *im3d , int nthr )
{
   if( IM3D_OPEN(im3d)                   &&
       im3d->type == AFNI_3DDATA_VIEW    &&
       ISVALID_DSET(im3d->fim_now)       &&
       nthr >= 0                         &&
       nthr <  DSET_NVALS(im3d->fim_now) &&
       nthr != im3d->vinfo->thr_index       ){
     MCW_arrowval *aav = im3d->vwid->func->thr_buck_av ;
     AV_assign_ival( aav, nthr ) ;
     IM3D_CLEAR_TMASK(im3d) ;
     IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
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
   if( nanat >= 0 ) AFNI_set_anat_index( im3d , nanat ) ;
   if( nfun  >= 0 ) AFNI_set_fim_index ( im3d , nfun  ) ;
   if( nthr  >= 0 ) AFNI_set_thr_index ( im3d , nthr  ) ;
   IM3D_CLEAR_TMASK(im3d) ;
   IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
   RETURN(0) ;
}

/*------------------------------------------------------------------
  Set the anatomical dataset.  Input is of the form "A.prefix n".
  Return value is 0 if good, -1 if bad.
-------------------------------------------------------------------*/

static int AFNI_drive_switch_anatomy( char *cmd )
{
   int ic , dadd=2 , nuse=0, sb = -1 ;
   Three_D_View *im3d ;
   char dname[THD_MAX_NAME]={""} ;
   MCW_choose_cbs cbs ;
   THD_slist_find slf ;

ENTRY("AFNI_switch_anatomy") ;

   if( cmd == NULL ) RETURN(-1) ; /* No need to die if strlen(cmd) < 3!
                                     I get in trouble for short names and
                                     I get in trouble for long names!
                                     Sheesh.    ZSS Dec 2011 */

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   /* get dataset name, truncate trailing blanks */
   if (*(cmd+dadd) != '\0') sscanf( cmd+dadd , "%244s%n" , dname , &nuse ) ;
   else RETURN(-1) ;
   for( ic=strlen(dname)-1 ; ic >= 0 ; ic-- )
     if( isspace(dname[ic]) || iscntrl(dname[ic]) ) dname[ic] = '\0' ;
     else break ;

   if( strlen(dname) == 0 ) RETURN(-1) ;

   if( nuse > 0                   &&
       *(cmd+dadd+nuse)   != '\0' &&
       *(cmd+dadd+nuse+1) != '\0'    )
      sscanf( cmd+dadd+nuse+1 , "%d" , &sb ) ;  /* 30 Nov 2005 */
               /* not checking for early string termination
                  before sscanf was causing corruption in some cases.
                  ZSS Nov 2009 */

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

   if( sb >= 0 ) AFNI_set_anat_index( im3d , sb ) ;   /* 30 Nov 2005 */

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
   char dname[THD_MAX_NAME]={""};
   MCW_choose_cbs cbs ;
   THD_slist_find slf ;

ENTRY("AFNI_switch_function") ;

   if( cmd == NULL ) RETURN(-1) ;/* No need to die if strlen(cmd) < 3!
                                                         ZSS Dec 2011 */

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   /* get dataset name, truncate trailing blanks */

   if (*(cmd+dadd) != '\0') sscanf( cmd+dadd , "%244s%n" , dname , &nuse ) ;
   else RETURN(-1) ;
   for( ic=strlen(dname)-1 ; ic >= 0 ; ic-- )
      if( isspace(dname[ic]) || iscntrl(dname[ic]) ) dname[ic] = '\0' ;
      else break ;

   if( strlen(dname) == 0 ) RETURN(-1) ;

   if( nuse > 0                   &&     /* 30 Nov 2005 */
       *(cmd+dadd+nuse)   != '\0' &&    /* See equivalent condition in */
       *(cmd+dadd+nuse+1) != '\0'   )  /* AFNI_drive_switch_anatomy   */
     sscanf( cmd+dadd+nuse+1 , "%d%d" , &nfun,&nthr ) ;

   /* find this dataset in current session of this controller */

   slf = THD_dset_in_session( FIND_PREFIX , dname , im3d->ss_now ) ;

   if( slf.dset_index < 0 ){   /* 18 Mar 2005 */
     MCW_idcode idcode ;
     MCW_strncpy( idcode.str , dname , MCW_IDSIZE ) ;
     slf = THD_dset_in_session( FIND_IDCODE , &idcode , im3d->ss_now ) ;
   }

   if( slf.dset_index < 0 ) RETURN(-1) ;  /* not found */

   cbs.ival = slf.dset_index ;

   /* same callback as Switch Function */

   IM3D_CLEAR_TMASK(im3d) ;
   IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
   AFNI_finalize_dataset_CB( im3d->vwid->view->choose_func_pb ,
                             (XtPointer)im3d ,  &cbs          ) ;

   if( nfun >= 0 ) AFNI_set_fim_index( im3d , nfun ) ;   /* 30 Nov 2005 */
   if( nthr >= 0 ) AFNI_set_thr_index( im3d , nthr ) ;

   RETURN(0) ;
}

/*---------------------------------------------------------------------*/
/* Lifted from NIML code [19 May 2017] */

#define IS_STRING_CHAR(c) ( isgraph(c) && !isspace(c) &&  \
                            (c) != '>' && (c) != '/'  &&  \
                            (c) != '=' && (c) != '<'    )

#define IS_QUOTE_CHAR(c)  ( (c) == '"' || (c) == '\'' )

static int_pair find_string( int nst, int nch, char *ch )
{
   int_pair ans = {-1,-1} ;  /* default answer ==> nothing found */
   int ii,jj ; char quot ;

   if( nst >= nch || nch < 2 || ch == NULL ) return ans;        /* bad input */
   for( ii=nst; ii<nch && !IS_STRING_CHAR(ch[ii]); ii++ ) ; /* skip to start */
   if( ii >= nch ) return ans ;                                 /* bad input */
   if( IS_QUOTE_CHAR(ch[ii]) ){                             /* quoted string */
      if( ii == nch-1 ) return ans ;                            /* bad input */
      quot = ch[ii] ; ii++ ;
      for( jj=ii ; jj<nch && ch[jj]!=quot && ch[jj]!= '\0' ; jj++ ); /* skip */
   } else {
      for( jj=ii+1 ; jj<nch && IS_STRING_CHAR(ch[jj]) ; jj++ ) ; /* to blank */
   }
   ans.i = ii ; ans.j = jj ; /* answer starts at ch[ii] and goes to ch[jj-1] */
   return ans ;
}

/*---------------------------------------------------------------------*/
/* Macros for deciding on which window is in play -- allows for
   various mis-spellings that might naturally transpire -- 22 Feb 2007 */

#define HAS_allimage(s)                                                 \
 ( strcasestr((s),"allim")!=NULL || strcasestr((s),"all_im")!=NULL )

#define HAS_axialimage(s)                                               \
 ( strcasestr((s),"axialim")!=NULL || strcasestr((s),"axial_im")!=NULL )

#define HAS_sagittalimage(s)                                            \
 ( strcasestr((s),"sagittalim")!=NULL || strcasestr((s),"sagittal_im")!=NULL || \
   strcasestr((s),"sagitalim") !=NULL || strcasestr((s),"sagital_im") !=NULL   )

#define HAS_coronalimage(s)                                             \
 ( strcasestr((s),"coronalim")!=NULL || strcasestr((s),"coronal_im")!=NULL )

#define HAS_axialgraph(s)                                               \
 ( strcasestr((s),"axialgr")!=NULL || strcasestr((s),"axial_gr")!=NULL )

#define HAS_sagittalgraph(s)                                            \
 ( strcasestr((s),"sagittalgr")!=NULL || strcasestr((s),"sagittal_gr")!=NULL || \
   strcasestr((s),"sagitalgr") !=NULL || strcasestr((s),"sagital_gr") !=NULL   )

#define HAS_coronalgraph(s)                                             \
 ( strcasestr((s),"coronalgr")!=NULL || strcasestr((s),"coronal_gr")!=NULL )

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
   if( strcasestr(cmd,"plugin.") != NULL ){
     ic = AFNI_drive_open_plugin( cmd ) ;
     RETURN(ic) ;
   }
#endif

   /* special case: open all image windows and return immediately */

   if( HAS_allimage(cmd) ){
     AFNI_view_xyz_CB( im3d->vwid->imag->image_xyz_pb, im3d, NULL ) ;
     NI_sleep(3) ;
     AFNI_view_xyz_CB( im3d->vwid->imag->image_yzx_pb, im3d, NULL ) ;
     NI_sleep(3) ;
     AFNI_view_xyz_CB( im3d->vwid->imag->image_zxy_pb, im3d, NULL ) ;
     NI_sleep(3) ;
     XmUpdateDisplay( im3d->vwid->top_shell ) ;
   }

   /* open a single graph or image window */

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

   cpt = strcasestr(cmd,"geom=") ;
   if( cpt == NULL ) cpt = strcasestr(cmd,"geom:") ;
   if( cpt != NULL )
     AFNI_decode_geom( cpt+5 , &gww,&ghh,&gxx,&gyy ) ;

   /*--- opened an image viewer: maybe modify it ---*/

   if( isq != NULL ){
      int ms = 0 ;

      /* geometry */

      if( gxx >= 0 && gyy >= 0 ){
        XtVaSetValues( isq->wtop, XmNx, gxx, XmNy, gyy, NULL ) ; ms += 400 ;
      }
      if( gww > 0 && ghh > 0 ){
        XtVaSetValues( isq->wtop, XmNwidth, gww, XmNheight, ghh, NULL ) ; ms += 400 ;
      }
      if( ms > 0 ){ NI_sleep(ms) ; ms = 0 ; }

      /* image fraction */

      cpt = strcasestr(cmd,"ifrac=") ;
      if( cpt == NULL ) cpt = strcasestr(cmd,"ifrac:") ;
      if( cpt != NULL ){
        float ifrac = strtod( cpt+6 , NULL ) ;
        if( ifrac >= FRAC_MIN && ifrac <= 1.0 )
          drive_MCW_imseq( isq, isqDR_setifrac, (XtPointer)(&ifrac) ) ;
        ms += 100 ;
      }

      /* montage */

      cpt = strcasestr(cmd,"mont=") ;
      if( cpt == NULL ) cpt = strcasestr(cmd,"mont:") ;
      if( cpt != NULL ){
        int mww=-1 , mhh=-1 , msp=-1 , mgap=-1 , nn ;
        char mcol[128] = "\0" ;

        nn = sscanf( cpt+5 , "%dx%d:%d:%d:%s" , &mww,&mhh,&msp,&mgap,mcol );

        if( nn >= 2 && mww >= 1 && mww <= MONT_NMAX && mhh >= 1 && mhh <= MONT_NMAX ){
          int mp[5] ;
          mp[0] = mww ; mp[1] = mhh ; mp[2] = msp ; mp[3] = mgap ;
          mp[4] = DC_find_closest_overlay_color(im3d->dc,mcol);
          drive_MCW_imseq( isq , isqDR_setmontage , (XtPointer)mp ) ;
        }
        ms += mww*mhh ;
      }

      /* iconify [06 Aug 2002] */

      cpt = strcasestr(cmd,"iconi") ;
      if( cpt != NULL ){
        XIconifyWindow( XtDisplay(isq->wtop) ,
                         XtWindow(isq->wtop)  ,
                         isq->dc->screen_num   ) ;
      }

      /* opacity [21 Jan 2003] */

      cpt = strcasestr(cmd,"opacity=") ;
      if( cpt == NULL ) cpt = strcasestr(cmd,"opacity:") ;
      if( cpt != NULL ){
        int opaval = -1 ;
        sscanf( cpt+8 , "%d" , &opaval ) ;
        drive_MCW_imseq( isq , isqDR_setopacity , (XtPointer)ITOP(opaval) ) ;
        ms += 100 ;
      }

      /* crop [03 May 2007] */

      cpt = strcasestr(cmd,"crop=") ;
      if( cpt == NULL ) cpt = strcasestr(cmd,"crop:") ;
      if( cpt != NULL ){
        int iar[4] ; char s1,s2,s3 ;
        iar[0] = iar[1] = iar[2] = iar[3] = -1 ;
        sscanf(cpt+5,"%d%c%d%c%d%c%d",
               iar+0 , &s1 , iar+1 , &s2 , iar+2 , &s3 , iar+3 ) ;
        if( iar[0] >= 0 && iar[1] >= iar[0] && iar[2] >= 0 && iar[3] >= iar[2] )
          drive_MCW_imseq( isq , isqDR_set_crop , (XtPointer)iar ) ;
        ms += 100 ;
      }

      /* zoom [10 Dec 2019] */

      cpt = strcasestr(cmd,"zoom=") ;
      if( cpt == NULL ) cpt = strcasestr(cmd,"zoom:") ;
      if( cpt != NULL ){
        int val=-666 ;  /* a beastly thing to do */
        sscanf(cpt+5,"%d",&val) ;
        drive_MCW_imseq( isq , isqDR_set_zoom , &val ) ;
        ms += 100 ;
      }

      /* range [15 Oct 2012] */

      cpt = strcasestr(cmd,"range=") ;
      if( cpt == NULL ) cpt = strcasestr(cmd,"range:") ;
      if( cpt != NULL ){
        float rrr[3] = {0.0f,0.0f,0.f} ; char s1 ;
        sscanf( cpt+6 , "%f%c%f" , rrr+0 , &s1 , rrr+1 ) ;
        if( rrr[0] >= rrr[1] ) rrr[0] = rrr[1] = 0.0f ;
        drive_MCW_imseq( isq , isqDR_setrange , (XtPointer)rrr ) ;
        ms += 100 ;
      }

      /* overlay_label [19 May 2017] */

      cpt = strcasestr(cmd,"overlay_label=") ;
      if( cpt == NULL ) cpt = strcasestr(cmd,"overlay_label:") ;
      if( cpt != NULL ){
        int_pair ans ;
        ans = find_string( 14 , strlen(cpt) , cpt ) ;
        if( ans.i > 0 && ans.j > ans.i ){
          MCW_choose_cbs cbs ; int qq , nqq=ans.j-ans.i ;
          cbs.reason = mcwCR_string ;
          cbs.cval   = malloc(sizeof(char)*(nqq+8)) ;
          for( qq=0 ; qq < nqq ; qq++ ) cbs.cval[qq] = cpt[ans.i+qq] ;
          cbs.cval[qq] = '\0' ;
          ISQ_overlay_label_CB( NULL , (XtPointer)isq , &cbs ) ;
          free(cbs.cval) ;
        }
        ms += 100 ;
      }

      /* keypress [18 Feb 2005] */

      ccc = cmd ;   /* 28 Dec 2006: allow multiple keypress= options */
      while(1){
        cpt = strcasestr(ccc,"keypress=") ;
        if( cpt == NULL ) cpt = strcasestr(ccc,"keypress:") ;
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
          ISQ_handle_keypress( isq , key , 0 ) ;
        } else {
          break ;  /* break out of this while(1) loop */
        }
        ccc = cpt+1 ;  /* scan from here for the next keypress= option */
        ms += 100 ;
      }

      /* butpress [25 Oct 2019] */

      ccc = cmd ;   /* allow multiple butpress= options */
      while(1){
#define NBPRESS 3
        static char *bpress_name[NBPRESS] = { "Colr" , "Swap" , "Norm" } ;
        static int bpress_code[NBPRESS]   = { isqDR_pressbut_Colr ,
                                              isqDR_pressbut_Swap ,
                                              isqDR_pressbut_Norm  } ;

        cpt = strcasestr(ccc,"butpress=") ;
        if( cpt == NULL ) cpt = strcasestr(ccc,"butpress:") ;
        if( cpt != NULL ){
          int qq ;

          cpt += 9 ;                           /* skip "butpress=" */
          if( *cpt == '\'' || *cpt == '\"' ) cpt++ ; /* skip quote */

          for( qq=0 ; qq < NBPRESS ; qq++ ){   /* find button name */
            if( strncasecmp(cpt,bpress_name[qq],strlen(bpress_name[qq])) == 0 ){
              drive_MCW_imseq( im3d->s123 , bpress_code[qq] , NULL ) ;
              break ;
            }
          }
          if( qq == NBPRESS )
            WARNING_message("unknown image viewer button: '%s'",cpt-9) ;
        } else {
          break ;  /* break out of this while(1) loop */
        }
        ccc = cpt+1 ;  /* scan from here for the next butpress= option */
        ms += 100 ;
      } ;

      if( ms > 0 ){ NI_sleep(ms) ; ms = 0 ; }

   /*--- opened a graph viewer: maybe modify it ---*/

   } else if( gra != NULL ){
      int ms = 0 ;

      /* geometry */

      if( gxx >= 0 && gyy >= 0 ){
        XtVaSetValues( gra->fdw_graph, XmNx, gxx, XmNy, gyy, NULL ) ; ms += 400 ;
      }
      if( gww > 0 && ghh > 0 ){
        XtVaSetValues( gra->fdw_graph, XmNwidth, gww, XmNheight, ghh, NULL ) ; ms += 400 ;
      }
      if( ms > 0 ){ NI_sleep(ms) ; ms = 0 ; }

      /* matrix */

      cpt = strcasestr(cmd,"matrix=") ;
      if( cpt == NULL ) cpt = strcasestr(cmd,"matrix:") ;
      if( cpt != NULL ){
        int mat = (int) strtod( cpt+7 , NULL ) ;
        if( mat > 0 )
          drive_MCW_grapher( gra , graDR_setmatrix , (XtPointer)ITOP(mat) ) ;
        ms += 100 ;
      }

      /* pinnum OR pintop */

      cpt = strcasestr(cmd,"pinnum=") ;
      if( cpt == NULL ) cpt = strcasestr(cmd,"pinnum:") ;
      if( cpt == NULL ) cpt = strcasestr(cmd,"pintop=") ;
      if( cpt == NULL ) cpt = strcasestr(cmd,"pintop:") ;
      if( cpt != NULL ){
        int pn = (int) strtod( cpt+7 , NULL ) ;
        if( pn >= MIN_PIN )
          drive_MCW_grapher( gra, graDR_setpinnum, (XtPointer)ITOP(pn) ) ;
        ms += 100 ;
      }

      /* pinbot [19 Mar 2004] */

      cpt = strcasestr(cmd,"pinbot=") ;
      if( cpt == NULL ) cpt = strcasestr(cmd,"pinbot:") ;
      if( cpt != NULL ){
        int pn = (int) strtod( cpt+7 , NULL ) ;
        if( pn > 0 )
          drive_MCW_grapher( gra, graDR_setpinbot, (XtPointer)ITOP(pn) ) ;
        ms += 100 ;
      }

      /* iconify [06 Aug 2002] */

      cpt = strcasestr(cmd,"iconi") ;
      if( cpt != NULL ){
        XIconifyWindow( XtDisplay(gra->fdw_graph) ,
                         XtWindow(gra->fdw_graph)  ,
                         gra->dc->screen_num   ) ;
      }

      /* keypress [18 Feb 2005] */

      ccc = cmd ;   /* 28 Dec 2006: allow multiple keypress= options */
      while(1){
        cpt = strcasestr(ccc,"keypress=") ;
        if( cpt == NULL ) cpt = strcasestr(ccc,"keypress:") ;
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
          ms += 50 ;
        } else {
          break ;  /* break out of this while(1) loop */
        }
        ccc = cpt+1 ;  /* scan from here for the next keypress= option */
      } ;

      if( ms > 0 ){ NI_sleep(ms) ; ms = 0 ; }

   /*--- opened the controller itself: maybe move it ---*/

   } else {
      int ms = 0 ;

      /* geometry */

      if( gxx >= 0 && gyy >= 0 ){
        XtVaSetValues( im3d->vwid->top_shell, XmNx, gxx, XmNy, gyy, NULL ) ;
        ms += 400 ;
      }

      /* iconify [06 Aug 2002] */

      cpt = strcasestr(cmd,"iconi") ;
      if( cpt != NULL ){
        XIconifyWindow( XtDisplay(im3d->vwid->top_shell) ,
                         XtWindow(im3d->vwid->top_shell)  ,
                         im3d->dc->screen_num   ) ;
      }

      if( ms > 0 ){ NI_sleep(ms) ; ms = 0 ; }

   }

   /*-- finito --*/

   NI_sleep(16) ;
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

   cpt = strcasestr(cmd,"plugin.") ;
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

   cpt = strcasestr(cmd,"geom=") ;
   if( cpt == NULL ) cpt = strcasestr(cmd,"geom:") ;
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
  for( ii=0 ; ii < 7 ; ii++ ){ RWC_sleep(123); fprintf(stderr,"*"); fflush(stderr); }
  fprintf(stderr,"\n\n") ;
  AFexit(0) ; return 0 ;
}

/*---------------------------------------------------------------*/

static int AFNI_drive_quitt( char *cmd )
{
  fprintf(stderr,"\nAFNI QUITTs!\n"); exit(0);
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

   cpt = strcasestr(cmd,"geom=") ;
   if( cpt == NULL ) cpt = strcasestr(cmd,"geom:") ;
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
                           toplabel , yname , (void_func *)kill_graph_xy ) ;

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
   memset(gxy, 0 , sizeof(Graph_xy)) ;  /* 12 Feb 2009 [lesstif patrol] */

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
                              toplabel , yname , (void_func *)kill_graph_xy ) ;

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
   ival = rint(val/THR_factor) ;
   smax = (int)( pow(10.0,THR_top_expon) + 0.001 ) - 1 ;
   if( ival < 0 || ival > smax ) RETURN(-1) ;
   XmScaleSetValue( im3d->vwid->func->thr_scale , ival ) ;

   /* get dec, if present, and apply it */

   sscanf(cpt,"%d",&dec) ;

   IM3D_CLEAR_TMASK(im3d) ;
   IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
   if( dec >= 0 && dec <= THR_top_expon )
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
   int ic,dadd , olddec,newdec , ival,smax,id,stop , dopval, doqval, dostar;
   Three_D_View *im3d ;
   float val , pval , qval;
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
   if( val < 0.0 || val > THR_top_value ) RETURN(-1) ; /* stupid user */

   /* get current scale decimal setting */

   olddec = (int)rint( log10(im3d->vinfo->func_thresh_top) ) ;
        if( olddec < 0             ) olddec = 0 ;
   else if( olddec > THR_top_expon ) olddec = THR_top_expon ;
   newdec = olddec ;

   smax  = (int)rint( pow(10.0,THR_top_expon) ) ;
   stop  = smax - 1 ;                             /* max slider value */

   dopval = (val >= 0.0) && (val <= 1.0) && (strchr(cpt,'p') != NULL) &&
            (DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) > 0) ;
   doqval = (val >= 0.0) && (val <= 1.0) && (strchr(cpt,'q') != NULL) &&
            (DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) > 0) ;

   dostar = (val > 0.0) && (strchr(cpt,'*') != NULL) ;

   if( dopval ){
     pval = THD_pval_to_stat( val ,
              DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) ,
              DSET_BRICK_STATAUX (im3d->fim_now,im3d->vinfo->thr_index)  ) ;
     if( pval >= 0.0 ) val = pval ;
   }

   if (doqval) {
      qval   = qginv(0.5*val) ;
      qval = THD_fdrcurve_zqtot( im3d->fim_now,im3d->vinfo->thr_index,qval) ;
      if( qval >= 0.0 ) val = qval;
   }

   if( val >= im3d->vinfo->func_thresh_top || dostar ){ /* reset scale range */

     newdec = (int)( log10(val) + 1.0 ) ;
          if( newdec < 0             ) newdec = 0 ;
     else if( newdec > THR_top_expon ) newdec = THR_top_expon ;
   }

   IM3D_CLEAR_TMASK(im3d) ;
   IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
   if( newdec != olddec )
     AFNI_set_thresh_top( im3d , tval[newdec] ) ;

   ival = rint( val/(THR_factor*tval[newdec]) ) ;
        if( ival < 0    ) ival = 0    ;
   else if( ival > stop ) ival = stop ;

   XmScaleSetValue( im3d->vwid->func->thr_scale , ival ) ;

   AFNI_thr_scale_CB( im3d->vwid->func->thr_scale, (XtPointer)im3d, NULL ) ;
   RETURN(0) ;
}

/*------------------------------------------------------------------------*/
/*! PBAR_SAVEIM [c] filename [dim=WxH[f]]   [19 Oct 2018] */

static int AFNI_drive_pbar_saveim( char *cmd )
{
   int ic , dadd=2 ;
   Three_D_View *im3d ;
   char fname[256] , *cpt ;
   MCW_choose_cbs cbs ;

ENTRY("AFNI_drive_pbar_saveim") ;

   if( cmd == NULL || strlen(cmd) < 1 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   fname[0] = '\0' ;
   sscanf( cmd+dadd , "%254s" , fname ) ;
   if( fname[0] == '\0' ) RETURN(-1) ;

   cbs.reason = mcwCR_string ;
   cbs.cval   = fname ;

   cpt = strcasestr(cmd+dadd+strlen(fname),"dim=") ;
   if( cpt != NULL && strlen(cpt+4) > 0 ){
     char dname[356] ;
     strcpy(dname,"AFNI_PBAR_IMXY ") ;
     sscanf( cpt+4 , "%254s" , dname+strlen(dname) ) ;
     AFNI_drive_setenv( dname ) ;
   }

   AFNI_finalize_saveim_CB( NULL , (XtPointer)im3d , &cbs ) ;

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

   IM3D_CLEAR_TMASK(im3d) ;
   IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
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
     POPUP_cursorize( pbar->panew ) ;  /* 08 Apr 2005 */
   }
   AFNI_inten_pbar_CB( pbar , im3d , 0 ) ;
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

   IM3D_CLEAR_TMASK(im3d) ;
   IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
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
   float pval[NPANE_MAX+1] , val,wal ;
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

   for( ii=0 ; ii <= NPANE_MAX ; ii++ ) pval[ii] = 0.0f ; /* 19 Dec 2018 */

   if( npan <= NPANE_MAX ){ /* discrete panes: get value=colorname array */

     for( ii=0 ; ii < npan ; ii++ ){
       str[0] = '\0' ; nn = 0 ;
       sscanf( cmd+dadd , "%f=%255s%n" , &val,str,&nn ) ;
       if( str[0] == '\0' || nn == 0 ) RETURN(-1) ;  /* can't parse */

       col = DC_find_closest_overlay_color( GLOBAL_library.dc , str ) ;
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

     str[0] = '\0' ; val = wal = 0.0f ;
     cpt = strcasestr(cmd+dadd,"::") ;
     if( cpt == NULL ){
       sscanf( cmd+dadd , "%f %s" , &val, str ) ;
       if( val <= 0.0 ) RETURN(-1) ;
       if( pos ) pbar->mode = 1 ;
       wal = (pbar->mode) ? 0.0f : -val ;
     } else {
       sscanf( cmd+dadd , "%f::%f %s" , &wal,&val , str ) ;
       if( wal >= val ) RETURN(-1) ;
     }

     flip = ( strcasestr(cmd+dadd,"FLIP") != NULL ) ;

     cpt = strcasestr(cmd+dadd,"ROTA=") ;
     if( cpt == NULL ) cpt = strcasestr(cmd+dadd,"ROTA:") ;
     if( cpt != NULL ) sscanf(cpt+5,"%d",&rota) ;
   }

   /* now set pbar values (and other widgets) */

   IM3D_CLEAR_TMASK(im3d) ;
   IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
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
     AFNI_hintize_pbar( pbar , FIM_RANGE(im3d) ) ;
     NORMAL_cursorize( pbar->panew ) ;  /* 08 Apr 2005 */
   } else {    /* set the colorscale */
     float pmax, pmin ;
     pbar->bigset = 0 ;
     pmax = val ; pmin = wal ;
     PBAR_set_bigmode( pbar , 1 , pmin,pmax ) ;
     PBAR_set_bigmap( pbar , str , 0) ;
     rotate_MCW_pbar( pbar , rota ) ;  /* 07 Feb 2004 */
     if( flip ) PBAR_flip( pbar ) ;    /* 07 Feb 2004 */
     POPUP_cursorize( pbar->panew ) ;  /* 08 Apr 2005 */
   }
   AFNI_inten_pbar_CB( pbar , im3d , 0 ) ;
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

   IM3D_CLEAR_TMASK(im3d) ;
   IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
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
/*! SET_FUNC_PERCENTILE [c.]{+|-}
   "SET_FUNC_PERCENTILE A.+"
---------------------------------------------------------------------------*/

static int AFNI_set_func_percentile( char *cmd )
{
   int ic , dadd=2 , nn ;
   Three_D_View *im3d ;

ENTRY("AFNI_set_func_percentile") ;

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

   MCW_set_bbox( im3d->vwid->func->perc_bbox , nn ) ;
   AFNI_perc_bbox_CB( im3d->vwid->func->perc_bbox->wbut[PERC_AUTOBUT] ,
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

   val = (float)strtod( cmd+dadd , NULL ) ;
   if( val < 0.0f ) RETURN(-1) ;

   if( val == 0.0f ){
     static char clabel[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;
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

   IM3D_CLEAR_TMASK(im3d) ;
   IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
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

/*-------------------------------------------------------------------------*/
/*! SET_FUNC_ALPHA [c.]mode
   "SET_FUNC_ALPHA A.Yes"
   "SET_FUNC_ALPHA A.No"
   "SET_FUNC_ALPHA A.Linear"     [Added 28 Jun 2021]
   "SET_FUNC_ALPHA A.Quadratic"
---------------------------------------------------------------------------*/

static int AFNI_set_func_alpha( char *cmd )  /* 10 Dec 2014 */
{
   int ic , dadd=2 , mode_onoff=0 , mode_type=1 ; char *cpt ;
   int               old_onoff    , old_type ;
   Three_D_View *im3d ;

ENTRY("AFNI_set_func_alpha") ;

   if( cmd == NULL || strlen(cmd) < 1 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   for( cpt=cmd+dadd ; isspace(*cpt) ; cpt++ ) ; /*skip whitespace*/
   if( *cpt == '\0' ) RETURN(-1) ;

   /* Turn alpha on or off */

   old_onoff  = im3d->vinfo->thr_use_alpha ;
   mode_onoff = !( toupper(*cpt) == 'N' || strcasestr(cpt,"off") != NULL ) ;

   /* Linear (0) or Quadratic (0) fading mode */

   old_type   = im3d->vwid->func->thr_alpha_av->ival ;
   mode_type  =  ( toupper(*cpt) == 'L' ) ? 0
                :( toupper(*cpt) == 'Q' ) ? 1
                :                          old_type ;

   if( mode_type != old_type )
     AV_assign_ival( im3d->vwid->func->thr_alpha_av , mode_type ) ;

   /* if something changed, do something */

   if( old_onoff != mode_onoff ){      /*--- simulate pressing 'A' button ---*/
     AFNI_func_thrtop_CB( im3d->vwid->func->thrtop_alpha_pb, im3d, NULL ) ;
   } else if( old_type != mode_type ){ /*--- simulate changing arrowval   ---*/
     AFNI_func_alpha_CB( im3d->vwid->func->thr_alpha_av , (XtPointer)im3d ) ;
   }

   RETURN(0) ;
}

/*-------------------------------------------------------------------------*/
/*! SET_FUNC_BOXED [c.]mode
   "SET_FUNC_BOXED A.Yes"
---------------------------------------------------------------------------*/

static int AFNI_set_func_boxed( char *cmd )  /* 10 Dec 2014 */
{
   int ic , dadd=2 , mode=0 ; char *cpt ;
   Three_D_View *im3d ;

ENTRY("AFNI_set_func_boxed") ;

   if( cmd == NULL || strlen(cmd) < 1 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   for( cpt=cmd+dadd ; isspace(*cpt) ; cpt++ ) ; /*skip whitespace*/
   if( *cpt == '\0' ) RETURN(-1) ;

   mode = !( toupper(*cpt) == 'N' || strcasestr(cpt,"off") != NULL ) ;

   if( im3d->vinfo->thr_use_boxed != mode ){
     AFNI_func_thrtop_CB( im3d->vwid->func->thrtop_boxed_pb, im3d, NULL ) ;
   }

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

   if( (nam[0] == '\0' || val[0] == '\0') && strchr(cmd,'=') != NULL ){
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
      ENV_globalrange_view(val);  /*same function is used for interactive environment */
#if 0
     Three_D_View *im3d ; int ii,gbr ;
   /* reset image_globalrange */
     THD_set_image_globalrange(-1);

     gbr = THD_get_image_globalrange(); /* resets from environment variable setting */

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
#endif
   }

   /*-- display coordinate order --*/

   else if( strcmp(nam,"AFNI_ORIENT") == 0 ){
     MCW_strncpy(GLOBAL_argopt.orient_code,val,4) ;
     THD_coorder_fill( GLOBAL_argopt.orient_code , &GLOBAL_library.cord ) ;
     PLUTO_force_redisplay() ;
   }

   /*-- display edges only of overlay blobs --*/

   else if( strcmp(nam,"AFNI_OVERLAY_ZERO") == 0 ){
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

   else if( strcmp(nam,"AFNI_LEFT_IS_POSTERIOR") == 0 ){
     GLOBAL_argopt.left_is_posterior = YESSISH(val) ;
   }

   /*-- reset the Colr scale in image viewers? --*/

   else if( strcmp(nam,"AFNI_IMAGE_COLORSCALE") == 0 ){  /* 25 Oct 2019 */
     DC_init_im_col(NULL) ;     /* probably does nothing useful */
     DC_palette_restore( GLOBAL_library.dc , 0.0 ) ;
     PLUTO_force_redisplay() ;
   }

   return(0) ;
}

/*------------------------------------------------------------------*/
/*! GETENV name         - print env value       20 Oct 2008 [rickr] */

int AFNI_drive_getenv( char *cmd )
{
   char nam[256]="\0" , *eee ;

   if( cmd == NULL || strlen(cmd) < 3 ) return(-1) ;

   /*-- scan for the name --*/

   sscanf( cmd , "%255s" , nam ) ;

   if( nam[0] == '\0' ) return(-1) ;

   /*-- get and printf the actual environment variable --*/

   eee = my_getenv(nam);
   fprintf(afniout,"%s = %s\n", nam, eee ? eee : "<UNSET>");
   fflush(afniout);

   return 0;
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

   if( strcasecmp(cmd+dadd,"Define_Function") == 0 || strcasecmp(cmd+dadd,"Define_Overlay") == 0 ){
     if( !XtIsManaged(im3d->vwid->func->frame) )
       AFNI_define_CB( im3d->vwid->view->define_func_pb, im3d, NULL ) ;
   } else if( strcasecmp(cmd+dadd,"Define_Datamode") == 0 ){
     if( !XtIsManaged(im3d->vwid->dmode->frame) )
       AFNI_define_CB( im3d->vwid->view->define_dmode_pb, im3d, NULL ) ;
   } else if( strcasecmp(cmd+dadd,"Define_Markers")  == 0 ){
     if( !XtIsManaged(im3d->vwid->marks->frame) )
       AFNI_define_CB( im3d->vwid->view->define_marks_pb, im3d, NULL ) ;
   } else if( strncasecmp(cmd+dadd,"Etc",3) == 0 ){  /* 14 Apr 2017 */
     if( !XtIsManaged(im3d->vwid->view->frame) ){
       AFNI_controller_panel_CB( NULL , im3d , NULL ) ;
     }
   } else {
     RETURN(-1) ;
   }
   RETURN(0) ;
}

/*---------------------------------------------------------------------*/
/*! CLOSE_PANEL [c.]Define_Function, etc. [14 Apr 2017] */

static int AFNI_close_panel( char *cmd )
{
   int ic , dadd=2 , fr=-1 , tr=-1 ;
   Three_D_View *im3d ;

ENTRY("AFNI_close_panel") ;

   if( cmd == NULL || strlen(cmd) < 2 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }

   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   /* do the right thing (simulate a button press) */

   if( strcasecmp(cmd+dadd,"Define_Function") == 0 || strcasecmp(cmd+dadd,"Define_Overlay") == 0 ){
     if( XtIsManaged(im3d->vwid->func->frame) )
       AFNI_define_CB( im3d->vwid->view->define_func_pb, im3d, NULL ) ;
   } else if( strcasecmp(cmd+dadd,"Define_Datamode") == 0 ){
     if( XtIsManaged(im3d->vwid->dmode->frame) )
       AFNI_define_CB( im3d->vwid->view->define_dmode_pb, im3d, NULL ) ;
   } else if( strcasecmp(cmd+dadd,"Define_Markers")  == 0 ){
     if( XtIsManaged(im3d->vwid->marks->frame) )
       AFNI_define_CB( im3d->vwid->view->define_marks_pb, im3d, NULL ) ;
   } else if( strncasecmp(cmd+dadd,"Etc",3) == 0 ){
     if( XtIsManaged(im3d->vwid->view->frame) ){
       AFNI_controller_panel_CB( NULL , im3d , NULL ) ;
     }
   } else {
     RETURN(-1) ;
   }
   RETURN(0) ;
}

/*--------------------------------------------------------------------*/
/*! Function for saving images in various formats. */

static int AFNI_drive_save_1image( char *cmd , int mode , char *suf )
{
   int ic , dadd=2 , imm , blowup=1 ;
   Three_D_View *im3d ;
   char junk[256] , fname[599] , *cpt ;
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
   sscanf( cmd+dadd , "%255s %255s" , junk , fname ) ;
   if( junk[0] == '\0' || fname[0] == '\0' ){
     ERROR_message("Image save '%s': something is missing",cmd) ;
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

   /* 30 Oct 2013: find blowup factor */

   cpt = strcasestr(cmd,"blowup=") ;
   if( cpt != NULL ){
     blowup = (int)strtod(cpt+7,NULL) ;
     if( blowup < 1 ) blowup = 1 ; else if( blowup > 8 ) blowup = 8 ;
   }
/* fprintf(stderr,"blowup set to %d\n",blowup) ; */

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
     isq->saver_blowup = blowup ;
     drive_MCW_imseq( isq, imm, (XtPointer)fname ) ;
     isq->saver_blowup = 1 ;
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
   int ic , dadd=2 , blowup=1 ;
   Three_D_View *im3d ;
   char junk[256] , fname[288] , *cpt ;
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
   sscanf( cmd+dadd , "%255s %255s" , junk , fname ) ;
   if( junk[0] == '\0' || fname[0] == '\0' ){
     ERROR_message("Saving All Images '%s': something is missing",cmd); RETURN(-1);
   }

   /* 30 Oct 2013: find blowup factor */

   cpt = strcasestr(cmd,"blowup=") ;
   if( cpt != NULL ){
     blowup = (int)strtod(cpt+8,NULL) ;
     if( blowup < 1 ) blowup = 1 ; else if( blowup > 8 ) blowup = 8 ;
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
     isq->saver_blowup = blowup ;
     drive_MCW_imseq( isq, imode , (XtPointer)fname ) ;
     isq->saver_blowup = 1 ;
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

        if( strcasestr(cmd,"orig") != NULL ) vv = VIEW_ORIGINAL_TYPE ;
   else if( strcasestr(cmd,"acpc") != NULL ) vv = VIEW_ACPCALIGNED_TYPE ;
   else if( strcasestr(cmd,"tlrc") != NULL ) vv = VIEW_TALAIRACH_TYPE ;
   else                                  return -1 ;

   if( vv == im3d->vinfo->view_type ) return 0 ;  /* nothing to do */
   if( !XtIsSensitive(im3d->vwid->view->view_bbox->wbut[vv]) ) return -1 ;

   IM3D_CLEAR_TMASK(im3d) ;
   IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
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
/*! GET_DICOM_XYZ [c.] */

static int AFNI_drive_get_dicom_xyz( char *cmd )
{
   int ic , dadd=2 ;
   Three_D_View *im3d ;
   float x,y,z ;

/*   if( strlen(cmd) < 1 ) return -1;*/

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

   x = im3d->vinfo->xi ;  /* current RAI coordinates */
   y = im3d->vinfo->yj ;
   z = im3d->vinfo->zk ;

   fprintf(afniout, "RAI xyz: %f %f %f\n", x, y, z);
/*   fprintf(stdout, "RAI xyz: %f %f %f\n", x, y, z);*/
   fflush(afniout);

   return 0 ;
}


/*--------------------------------------------------------------------*/
/*! GET_OLAY_VAL [c.] */
/* get the value of the overlay sub-brick at the current crosshairs */
static int AFNI_drive_get_olay_val( char *cmd )
{
   int ic ;
   Three_D_View *im3d ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

   fprintf(afniout, "OLay value: %s \n", im3d->vinfo->func_val);
   fflush(afniout);

   return 0 ;
}

/*--------------------------------------------------------------------*/
/*! GET_ULAY_VAL [c.] */
/* get the value of the underlay sub-brick at the current crosshairs */
static int AFNI_drive_get_ulay_val( char *cmd )
{
   int ic ;
   Three_D_View *im3d ;


   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

   fprintf(afniout, "ULay value: %s \n", im3d->vinfo->anat_val);
   fflush(afniout);

   return 0 ;
}

/*--------------------------------------------------------------------*/
/*! GET_THR_VAL [c.] */
/* get the value of the threshold sub-brick at the current crosshairs */
static int AFNI_drive_get_thr_val( char *cmd )
{
   int ic ;
   Three_D_View *im3d ;


   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

   fprintf(afniout, "Thr value: %s \n", im3d->vinfo->thr_val);
   fflush(afniout);

   return 0 ;
}

/*--------------------------------------------------------------------*/
/*! GET_ULAY_NAME [c.] */
/* get the underlay dataset name */
static int AFNI_drive_get_ulay_name( char *cmd )
{
   int ic ;
   Three_D_View *im3d ;


   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

   fprintf(afniout, "ULay dset: %s \n", DSET_PREFIX(im3d->anat_now));
   fflush(afniout);

   return 0 ;
}

/*--------------------------------------------------------------------*/
/*! GET_OLAY_NAME [c.] */
/* get the overlay dataset name */
static int AFNI_drive_get_olay_name( char *cmd )
{
   int ic ;
   Three_D_View *im3d ;


   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

   fprintf(afniout, "OLay dset: %s \n", DSET_PREFIX(im3d->fim_now));
   fflush(afniout);

   return 0 ;
}

/* get the output file for plugout info */
static FILE *
AFNI_drive_get_outstream()
{
   char *afni_outfile;

   /* first time in set the output stream to stdout or environment variable */
   if(afniout==NULL){
      /* get from environment variable if set */
      afni_outfile = my_getenv("AFNI_OUTPLUG");
      if(afni_outfile!=NULL) {
         AFNI_drive_set_outstream(afni_outfile);
      }
   }

   /* if still NULL output stream, set to default of stdout */
   if(afniout==NULL) afniout = stdout;

   return(afniout);
}

/* set the output stream to a file rather than stdout*/
static int
AFNI_drive_set_outstream(char *outfile)
{
   /* if just passed a NULL, reset output to stdout */
   if(outfile==NULL){
      afniout = stdout;
      return(-1);
   }

   /* check if resetting to stdout by string from plugout command */
   if(strcmp(outfile, "stdout")==0) {
      afniout = stdout;
      return 0;
   }

    /* make sure this file name is a good one, and open it for append */
   if( THD_filename_ok(outfile) )
      afniout = fopen(outfile, "a");

   /* something went wrong, so tell user and reset to stdout */
   if(afniout==NULL){
      fprintf(stderr, "**** couldn't open outfile, resetting to stdout\n");
      afniout = stdout;
      return(-1);
   }
   else {
    return 0;
   }
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
/*! GET_IJK [c.] */

static int AFNI_drive_get_ijk( char *cmd )
{
   int ic , dadd=2 ;
   Three_D_View *im3d ;
   int i,j,k;

/*   if( strlen(cmd) < 1 ) return -1;*/

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

   i = im3d->vinfo->i1 ;  /* current ijk coordinates */
   j = im3d->vinfo->j2 ;
   k = im3d->vinfo->k3 ;

   fprintf(afniout, "ijk: %d %d %d\n", i,j,k);
   fflush(afniout);

   return 0 ;
}

/*--------------------------------------------------------------------*/
/*! SET_INDEX [c.] ijk */

static int AFNI_drive_set_ijk_index( char *cmd )
{
   int ic , dadd=2 ;
   Three_D_View *im3d ;
   int ijk ;

   if( strlen(cmd) < 1 ) return -1;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

   ic = sscanf( cmd+dadd , "%d" , &ijk ) ;
   if( ic < 1 ) return -1 ;
   AFNI_set_index_viewpoint( im3d , ijk , REDISPLAY_ALL ) ;
   return 0 ;
}

/*--------------------------------------------------------------------*/
/*! SET_XHAIRS [c.]code */

static int AFNI_drive_set_xhairs( char *cmd )
{
   int ic , dadd=2 , hh=-1 ;
   Three_D_View *im3d ;
   int i,j,k ;

   if( strlen(cmd) < 1 ) return -1;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

        if( strcasestr(cmd+dadd,"OFF")    != NULL ) hh = 0 ;
   else if( strcasestr(cmd+dadd,"SINGLE") != NULL ) hh = 1 ;
   else if( strcasestr(cmd+dadd,"MULTI")  != NULL ) hh = 2 ;
   else if( strcasestr(cmd+dadd,"LR_AP")  != NULL ) hh = 3 ;
   else if( strcasestr(cmd+dadd,"LR_IS")  != NULL ) hh = 4 ;
   else if( strcasestr(cmd+dadd,"AP_IS")  != NULL ) hh = 5 ;
   else if( strcasestr(cmd+dadd,"LR")     != NULL ) hh = 6 ;
   else if( strcasestr(cmd+dadd,"AP")     != NULL ) hh = 7 ;
   else if( strcasestr(cmd+dadd,"IS")     != NULL ) hh = 8 ;
   else                                         return -1 ;

   AV_assign_ival( im3d->vwid->imag->crosshair_av, hh ) ;
   AFNI_crosshair_visible_CB( im3d->vwid->imag->crosshair_av, (XtPointer)im3d );
   return 0 ;
}

static int AFNI_drive_xhairs_gap( char *cmd )
{
   int ic , dadd=2 , hh=-1 ;
   Three_D_View *im3d ;
   int gapsize ;

   if( strlen(cmd) < 1 ) return -1;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

   ic = sscanf( cmd+dadd , "%d" , &gapsize ) ;
   if( ic < 1 ) return -1;
   AV_assign_ival( im3d->vwid->imag->crosshair_gap_av, gapsize);
   AFNI_crosshair_gap_CB( im3d->vwid->imag->crosshair_gap_av,
                         (XtPointer) im3d ) ;

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
/*! INSTACORR [c] SET [x y z] or
                  INIT name=value [name=value ...]  */

static int AFNI_drive_instacorr( char *cmd )
{
   int ic, dadd=2 ; Three_D_View *im3d ;

   if( strlen(cmd) < 3 ) return -1;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

   /** SET the seed location and do the work **/

   if( strncasecmp(cmd+dadd,"SET",3) == 0 ){
     float x,y,z ; int good=0 ; char cj='N' ;

     IM3D_CLEAR_TMASK(im3d) ;
     IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
     if( cmd[dadd+3] != '\0' ) good = sscanf(cmd+dadd+3,"%f %f %f %c",&x,&y,&z,&cj) ;
     if( good < 3 ){
        AFNI_icor_setref(im3d) ;  /* no x,y,z ==> use current xhair point */
     } else {
        good = AFNI_icor_setref_xyz(im3d,x,y,z) ; /* have x,y,z */
        if( good > 0 ){
          cj = toupper(cj) ;
          if( cj == 'J' || cj == 'Y' ) AFNI_jumpto_dicom( im3d , x,y,z ) ;
          AFNI_icor_setref_locked(im3d) ;
        }
     }
     return 0 ;

   /** APSET (for 3dGroupInCorr) */

   } else if( strncasecmp(cmd+dadd,"APSET",5) == 0 ){  /* Apr 2013 */
     float x,y,z ; int good=0 ; char cj='N' ;

     if( im3d->giset == NULL                ||
        !im3d->giset->ready                 ||
        !GICOR_apair_allow_bit(im3d->giset) ||
         GICOR_apair_mirror_bit(im3d->giset)  ) return -1 ;  /* bad choice */

     IM3D_CLEAR_TMASK(im3d) ;
     IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
     if( cmd[dadd+5] != '\0' ) good = sscanf(cmd+dadd+5,"%f %f %f %c",&x,&y,&z,&cj) ;
     if( good < 3 ){
        AFNI_gicor_setapair_xyz(im3d,
                                im3d->vinfo->xi, im3d->vinfo->yj, im3d->vinfo->zk) ;
     } else {
        AFNI_gicor_setapair_xyz(im3d,x,y,z) ; /* have x,y,z */
        cj = toupper(cj) ;
        if( cj == 'J' || cj == 'Y' ) AFNI_jumpto_dicom( im3d , x,y,z ) ;
     }
     return 0 ;

   /** INITialize **/

   } else if( strncasecmp(cmd+dadd,"INIT",4) == 0 ){

     NI_str_array *sar; ICOR_setup *iset; int ii,mm=0; char *cpt,*dpt;
     int start=0,end=0 , cnum=0,clen=0,cstep=0 ;

     /* skip ahead until we find a blank */

     for( ii=dadd+4 ; cmd[ii] != '\0' && !isspace(cmd[ii]) ; ii++ ) ; /*nada*/
     if( cmd[ii] == '\0' ) return -1 ;

     /*  break up rest of command into set of 'name=value' strings */

     sar = NI_decode_string_list(cmd+ii,";") ;
     if( sar == NULL ) return -1 ;

     INIT_ICOR_setup(iset) ;              /* zeroes out everything */
     if( im3d->iset != NULL ){              /* copy existing setup */
       iset->dset     = im3d->iset->dset ;
       iset->automask = im3d->iset->automask ;
       iset->mset     = (iset->automask) ? NULL : im3d->iset->mset ;
       iset->start    = im3d->iset->start ;
       iset->end      = im3d->iset->end ;
       iset->clen     = im3d->iset->clen ;
       iset->cnum     = im3d->iset->cnum ;
       iset->cstep    = im3d->iset->cstep ;
       iset->mindex   = im3d->iset->mindex ;
       iset->fbot     = im3d->iset->fbot ;
       iset->ftop     = im3d->iset->ftop ;
       iset->blur     = im3d->iset->blur ;
       iset->sblur    = im3d->iset->sblur ;
       iset->polort   = im3d->iset->polort ;
       iset->cmeth    = im3d->iset->cmeth ;
       iset->despike  = im3d->iset->despike ;
       if( im3d->iset->prefix != NULL ) iset->prefix = strdup  (im3d->iset->prefix) ;
       if( im3d->iset->gortim != NULL ) iset->gortim = mri_copy(im3d->iset->gortim) ;
       iset->iter_count  = im3d->iset->iter_count ;  /* 05 Feb 2015 */
       iset->iter_thresh = im3d->iset->iter_thresh ;

     } else {                              /* set some default params */
       iset->automask = 1 ;
       iset->fbot     = 0.01f ;
       iset->ftop     = 0.10f ;
       iset->polort   = 2 ;
       iset->cmeth    = NBISTAT_PEARSON_CORR ;
       mm             = 1 ;
       iset->iter_count  = 1 ;     /* 05 Feb 2015 */
       iset->iter_thresh = 0.01f ;
     }

     if( iset->prefix == NULL ){
       iset->prefix = (char *)malloc(sizeof(char)*16) ;
       cpt = AFNI_controller_label(im3d) ;
       sprintf(iset->prefix,"%c_ICOR",cpt[1]) ;
     }

     /* scan to set params [mm = number of 'major' changes] */

     for( ii=0 ; ii < sar->num ; ii++ ){
       cpt = sar->str[ii]    ; if( *cpt == '\0' ) continue ;           /* bad */
       dpt = strchr(cpt,'=') ; if(  dpt == NULL ) continue ;        /* badder */
       *dpt = '\0' ; dpt++   ; if( *dpt == '\0' ) continue ;       /* baddest */

       if( strcasecmp(cpt,"dset")       == 0 ||
           strcasecmp(cpt,"dataset")    == 0 ||
           strcasecmp(cpt,"timeseries") == 0   ){                  /* dataset */
         THD_slist_find ff ; char *ppt ;
         ppt = strchr(dpt,'+') ; if( ppt != NULL ) *ppt = '\0' ;
         ff = PLUTO_dset_finder(dpt) ; iset->dset = ff.dset ; mm++ ;
         if( iset->dset == NULL )
           ERROR_message("INSTACORR INIT: failed to find Dataset %s",dpt) ;

       } else if( strcasecmp(cpt,"mask") == 0 ){        /* mask [07 Feb 2013] */
         if( strcasecmp(dpt,"AUTO") == 0 || strcasecmp(dpt,"AUTOMASK") == 0 ){
           iset->automask = 1 ; iset->mset = NULL ; mm++ ;
         } else if( strcasecmp(dpt,"NULL") == 0 || strcasecmp(dpt,"NONE") == 0 ){
           iset->automask = 0 ; iset->mset = NULL ; mm++ ;
         } else {
           THD_slist_find ff ; char *ppt ;
           iset->automask = 0 ;
           ppt = strchr(dpt,'+') ; if( ppt != NULL ) *ppt = '\0' ;
           ff = PLUTO_dset_finder(dpt) ; iset->mset = ff.dset ; mm++ ;
           if( iset->mset == NULL )
             ERROR_message("INSTACORR INIT: failed to find Mask %s",dpt) ;
         }

       } else if( strcasecmp(cpt,"Count") == 0 ){     /* 05 Feb 2015 */
         iset->iter_count = (int)strtod(dpt,NULL) ; mm++ ;
              if( iset->iter_count < 1 ) iset->iter_count = 1 ;
         else if( iset->iter_count > 6 ) iset->iter_count = 6 ;

       } else if( strcasecmp(cpt,"Thresh") == 0 ){    /* 05 Feb 2015 */
         iset->iter_thresh = strtod(dpt,NULL) ; mm++ ;
         if( iset->iter_thresh < 0.0f ) iset->iter_thresh = 0.0f ;

       } else if( strcasecmp(cpt,"eset")     == 0 ||
                  strcasecmp(cpt,"extraset") == 0   ){
         THD_slist_find ff ; char *ppt ;
         ppt = strchr(dpt,'+') ; if( ppt != NULL ) *ppt = '\0' ;
         ff = PLUTO_dset_finder(dpt) ; iset->eset = ff.dset ; mm++ ;
         if( iset->eset == NULL )
           ERROR_message("INSTACORR INIT: failed to find Extraset %s",dpt) ;

       } else if( strcasecmp(cpt,"ignore") == 0 ){                  /* ignore */
         iset->start = strtod(dpt,NULL) ; mm++ ;
         if( iset->start < 0 ) iset->start = 0 ;
         iset->end = 0 ;

       } else if( strcasecmp(cpt,"startend")  == 0 ||
                  strcasecmp(cpt,"start,end") == 0   ){        /* 21 Nov 2012 */
         char *qpt ;
         start = (int)strtod(dpt,&qpt) ; mm++ ;
         if( start < 0 ) start = 0 ;
         while( isspace(*qpt) ) qpt++ ;
         if( *qpt == ',' || *qpt == '+' ){
           char qc = *qpt ;
           if( !isdigit(*qpt) ) qpt++ ;
           end = (int)strtod(qpt,NULL) ;
           if( qc == '+' && end > 4 ) end = start + end-1 ;
         } else if( *qpt == '@' ){          /* '@' stuff from 07 Oct 2014 */
           clen = (int)strtod(qpt+1,&qpt) ;
           if( clen < 5 ){
             WARNING_message("Bad 'Start,End' @ string '%s'",cpt) ; clen = 0 ;
           } else if( *qpt == ',' ){
             cnum = (int)strtod(qpt+1,&qpt) ;
             if( *qpt == ',' )
               cstep = (int)strtod(qpt+1,&qpt) ;
           }
         } else if( *qpt != '\0' ){
           WARNING_message("Don't understand 'Start,End' string '%s'",cpt) ;
         }
         iset->start = start ; iset->end = end ;

       } else if( strcasecmp(cpt,"blur") == 0 ){                      /* blur */
         iset->blur = strtod(dpt,NULL) ; mm++ ;
         if( iset->blur <= 0.0f ) iset->blur = 0.0f ;

       } else if( strcasecmp(cpt,"automask") == 0 ){              /* automask */
         iset->automask = YESSISH(dpt) ; mm++ ;

       } else if( strcasecmp(cpt,"despike") == 0 ){                /* despike */
         iset->despike = YESSISH(dpt) ; mm++ ;

       } else if( strcasecmp(cpt,"bandpass") == 0 ){              /* bandpass */
         sscanf(dpt,"%f,%f",&(iset->fbot),&(iset->ftop)) ;

       } else if( strcasecmp(cpt,"seedrad") == 0 ||
                  strcasecmp(cpt,"sblur")   == 0 ||
                  strcasecmp(cpt,"seedblur")== 0   ){              /* seedrad */
         iset->sblur = strtod(dpt,NULL) ;
         if( iset->sblur <= 0.0f ) iset->sblur = 0.0f ;

       } else if( strcasecmp(cpt,"method") == 0 ||
                  strcasecmp(cpt,"meth")   == 0   ){                /* method */
         switch( toupper(*dpt) ){
           default:  iset->cmeth = NBISTAT_PEARSON_CORR  ; break ;
           case 'S': iset->cmeth = NBISTAT_SPEARMAN_CORR ; break ;
           case 'Q': iset->cmeth = NBISTAT_QUADRANT_CORR ; break ;
           case 'K': iset->cmeth = NBISTAT_KENDALL_TAUB  ; break ;
           case 'B': iset->cmeth = NBISTAT_BC_PEARSON_M  ; break ;
           case 'V': iset->cmeth = NBISTAT_BC_PEARSON_V  ; break ;
           case 'T': iset->cmeth = NBISTAT_TICTACTOE_CORR; break ;
           case 'E': iset->cmeth = NBISTAT_EUCLIDIAN_DIST; break ; /* ZSS */
           case 'C': iset->cmeth = NBISTAT_CITYBLOCK_DIST; break ; /* ZSS */
         }

       } else if( strcasecmp(cpt,"polort") == 0 ){                  /* polort */
         iset->polort = strtod(dpt,NULL) ; mm++ ;
              if( iset->polort < -1 ) iset->polort = -1 ;
         else if( iset->polort >  2 ) iset->polort =  2 ;

       } else {      /* Extraordinary crimes against the People, or the State */
         WARNING_message("Ignoring unknown INSTACORR INIT: '%s=%s'",cpt,dpt) ;
       }

     } /* end of loop over 'name=value' strings */

     if( !ISVALID_DSET(iset->dset) ){
       ERROR_message("INSTACORR INIT dataset not set -- cannot continue :-(") ;
       return -1 ;
     }
     if( iset->end <= 0 || iset->end <= iset->start || iset->end >= DSET_NVALS(iset->dset) )
       iset->end = DSET_NVALS(iset->dset)-1 ;

     if( clen > 0 ){
       int ss , nn , nv=DSET_NVALS(iset->dset) ;
       if( start+clen >= nv ){
         ERROR_message("INSTACORR_INIT 'start@' length is too long") ; return -1 ;
       }
       ss = (cstep <= 0 || cstep > clen) ? clen : cstep ;
       nn = 1 + (nv-start-clen) / ss ;
       if( cnum > nn || cnum == 0 ) cnum = nn ;
       INFO_message("INSTACORR INIT section length=%d number=%d step=%d",clen,cnum,cstep) ;
       iset->cmeth = NBISTAT_PEARSON_CORR  ;
       iset->clen  = clen ;
       iset->cnum  = cnum ;
       iset->cstep = cstep ;
     }

     NI_delete_str_array(sar) ;  /* send it to the graveyard of unwanted data */

     if( mm == 0 ){                                /* only minor changes made */
       im3d->iset->sblur = iset->sblur ;
       im3d->iset->cmeth = (iset->clen == 0) ? iset->cmeth : NBISTAT_PEARSON_CORR ;
       DESTROY_ICOR_setup(iset) ;
       ININFO_message("INSTACORR INIT minor change made :-)") ;
       return 0 ;
     }

     /* major changes made ==> re-do complete initialization */

     SHOW_AFNI_PAUSE ;
     ii = THD_instacorr_prepare( iset ) ;      /* all the real work is herein */
     SHOW_AFNI_READY ;
     if( ii == 0 ){
       DESTROY_ICOR_setup(iset) ;
       ERROR_message("Bad INSTACORR%s INIT? :-(",AFNI_controller_label(im3d)) ;
       return -1 ;
     }
     DESTROY_ICOR_setup(im3d->iset) ; im3d->iset = iset ;     /* ready to go! */
     SENSITIZE_INSTACORR(im3d,True) ;
     ININFO_message("INSTACORR INIT completed successfully :-)") ;
     return 0 ;
   }

   /* unknown order gets here ==> an elephant sits on the user */

   return -1 ;
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
/* WRITE_OVERLAY [c] prefix */

static int AFNI_drive_write_overlay( char *cmd )  /* 16 Jun 2014 */
{
   int ic, dadd=2 , ii ; Three_D_View *im3d ; char *prefix ;

   if( strlen(cmd) < 3 ) return -1 ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) || !ISVALID_DSET(im3d->fim_now) ) return -1 ;

   /* skip blanks */

   for( ii=dadd ; cmd[ii] != '\0' && isspace(cmd[ii]) ; ii++ ) ; /*nada*/
   prefix = cmd+ii ;
   if( !THD_filename_ok(prefix) ) return -1 ;

   AFNI_writeout_dataset( im3d->fim_now , prefix ) ;
   return 0 ;
}

/*--------------------------------------------------------------------*/
/* WRITE_UNDERLAY [c] prefix */

static int AFNI_drive_write_underlay( char *cmd )  /* 16 Jun 2014 */
{
   int ic, dadd=2 , ii ; Three_D_View *im3d ; char *prefix ;

   if( strlen(cmd) < 3 ) return -1 ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) || !ISVALID_DSET(im3d->anat_now) ) return -1 ;

   /* skip blanks */

   for( ii=dadd ; cmd[ii] != '\0' && isspace(cmd[ii]) ; ii++ ) ; /*nada*/
   prefix = cmd+ii ; if( !THD_filename_ok(prefix) ) return -1 ;

   AFNI_writeout_dataset( im3d->anat_now , prefix ) ;
   return 0 ;
}

/*--------------------------------------------------------------------*/
/* WRITE_CONT_SPX_HELP prefix */
extern char * AFNI_Help_AllMainCont (TFORM targ);
static int AFNI_drive_write_cont_spxhelp( char *cmd )
{
   int ic, dadd=2 , ii ;
   Three_D_View *im3d ;
   char *prefix, *s=NULL;
   FILE *fout = NULL;

   if( strlen(cmd) < 3 ) return -1 ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ) return -1 ;

   /* skip blanks */

   for( ii=dadd ; cmd[ii] != '\0' && isspace(cmd[ii]) ; ii++ ) ; /*nada*/
   prefix = cmd+ii ; if( !THD_filename_ok(prefix) ) return -1 ;

   fout = fopen(prefix,"w");
   if (!fout) {
      fprintf(stderr,"Failed to open %s for writing", prefix);
   } else {
      if ((s = AFNI_Help_AllMainCont(SPX))) {
         fprintf(fout,"%s", s);
      } else {
         fprintf(stderr,"Failed to get help string");
      }
      fclose(fout); fout = NULL;
   }

   return 0 ;
}

/*--------------------------------------------------------------------*/
/* SNAP_CONT prefix
   Take a selfie of a main AFNI controller. */
/*--------------------------------------------------------------------*/

static int AFNI_drive_snap_cont( char *cmd )
{
   int ic, dadd=2 , ii ;
   Three_D_View *im3d ;
   char *prefix;

ENTRY("AFNI_drive_snap_cont") ;

   if( strlen(cmd) < 3 ) RETURN(-1) ;

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_OPEN(im3d) ||  !im3d->vwid->top_form) RETURN(-1) ;
   if ( ! XtIsManaged(im3d->vwid->top_form) ||
        ! XtIsRealized(im3d->vwid->top_form) )  {
      fprintf(stderr,"im3d->vwid->top_form ot realized (%d) or managed (%d)\n",
         XtIsRealized(im3d->vwid->top_form), XtIsManaged(im3d->vwid->top_form));
      RETURN(-1) ;
   }

   /* skip blanks */

   for( ii=dadd ; cmd[ii] != '\0' && isspace(cmd[ii]) ; ii++ ) ; /*nada*/
   prefix = cmd+ii ; if( !THD_filename_ok(prefix) ) RETURN(-1) ;

   ii = ISQ_snapfile2 ( im3d->vwid->top_form,  prefix);

   RETURN(ii) ;
}

/*--------------------------------------------------------------------*/
/* SNAP_VIEWER A.viewer prefix
   Take a selfie of a viewer window. */
/*--------------------------------------------------------------------*/

static int AFNI_drive_snap_viewer( char *cmd )
{
   int ic, dadd=2 , ii ;
   Three_D_View *im3d ;
   char *prefix;
   MCW_imseq   *isq=NULL ;
   MCW_grapher *gra=NULL ;
   char junk[256] , fname[599] , *cpt ;
   Widget wsnap=(Widget)NULL ;

ENTRY("AFNI_drive_snap_viewer") ;

   if( strlen(cmd) < 3 ) RETURN(-1) ;

   /* make sure the controller itself is open */

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_VALID(im3d) ){
     ERROR_message("Viewer snapshot '%s': controller not open",cmd) ;
     RETURN(-1) ;
   }

   /* find graph or image window */

        if( HAS_axialimage   (cmd+dadd) ) isq = im3d->s123 ;
   else if( HAS_sagittalimage(cmd+dadd) ) isq = im3d->s231 ;
   else if( HAS_coronalimage (cmd+dadd) ) isq = im3d->s312 ;
   else if( HAS_axialgraph   (cmd+dadd) ) gra = im3d->g123 ;
   else if( HAS_sagittalgraph(cmd+dadd) ) gra = im3d->g231 ;
   else if( HAS_coronalgraph (cmd+dadd) ) gra = im3d->g312 ;

   if( isq == NULL && gra == NULL ){
     ERROR_message("Viewer snapshot '%s': no viewer window specified",cmd) ;
     RETURN(-1) ;
   }

   /* extract the filename to save into */

   junk[0] = fname[0] = '\0' ;
   sscanf( cmd+dadd , "%255s %255s" , junk , fname ) ;
   if( junk[0] == '\0' || fname[0] == '\0' ){
     ERROR_message("Viewer snapshot '%s': filename is missing",cmd) ;
     RETURN(-1) ;
   }
   if( fname[0] == '\'' || fname[0] == '\"' ){
     char qt=fname[0] , *q1 , *q2 ; int imm ;
     q1 = strchr(cmd+dadd,qt)+1 ;
     q2 = strchr(q1,qt) ; if( q2 == NULL ) q2 = cmd+strlen(cmd) ;
     if( (imm=q2-q1) > 0 && imm < 599 ){
       strncpy(fname,q1,imm) ; fname[imm] = '\0' ;
     } else {
       ERROR_message("Viewer snapshot '%s': filename is bad",cmd);
       RETURN(-1);
     }
   }

        if( isq != NULL ) wsnap = isq->wform ;
   else if( gra != NULL ) wsnap = gra->top_form ;
   else {
     ERROR_message("Viewer snapshot '%s': no valid viewer specified",cmd);
     RETURN(-1);
   }

   if( wsnap != (Widget)NULL )
     ii = ISQ_snapfile2(wsnap,fname) ;
   else {
     ERROR_message("Viewer snapshot '%s': viewer window doesn't exist",cmd);
     RETURN(-1);
   }

   RETURN(ii) ;
}

/*--------------------------------------------------------------------*/
/* SET_ULAY_RANGE A.viewer bot top [ztop] -- 23 Jul 2018 (RWC) */
/*--------------------------------------------------------------------*/

/*- macro to redisplay image with new range -*/

#define RNGIM(iq)                                     \
 do{ if( (iq) != NULL ){                              \
       (iq)->rng_extern = 0  ; (iq)->rng_ztop = ztop; \
       (iq)->rng_bot    = bot; (iq)->rng_top  = top ; \
       ISQ_redisplay( (iq) , -1 , isqDR_reimage ) ;   \
     } } while(0)

static int AFNI_drive_underlay_range( char *cmd )
{
   int ic, dadd=2 ;
   Three_D_View *im3d=NULL ;
   MCW_imseq    *isq=NULL ;
   char junk[256] ;
   float bot,top,ztop ;

ENTRY("AFNI_drive_underlay_range") ;

   if( strlen(cmd) < 3 ) RETURN(-1) ;

   /* make sure the controller itself is open */

   ic = AFNI_controller_code_to_index( cmd ) ;
   if( ic < 0 ){ ic = 0 ; dadd = 0 ; }
   im3d = GLOBAL_library.controllers[ic] ;
   if( !IM3D_VALID(im3d) ){
     ERROR_message("Command '%s': controller not open",cmd) ;
     RETURN(-1) ;
   }

   /* check if doing all image windows for this controller */
   /* (and get the ranging numbers at the same time) */

   junk[0] = '\0' ; bot = top = ztop = 0.0f ;
   sscanf( cmd+dadd , "%255s %f%f%f" , junk , &bot , &top , &ztop ) ;
   if( strncasecmp(junk,"all",3) == 0 ){
     RNGIM(im3d->s123) ;
     RNGIM(im3d->s231) ;
     RNGIM(im3d->s312) ;
     RETURN(0) ;
   }

   /* find single image window */

        if( HAS_axialimage   (cmd+dadd) ){ RNGIM(im3d->s123); RETURN(0); }
   else if( HAS_sagittalimage(cmd+dadd) ){ RNGIM(im3d->s231); RETURN(0); }
   else if( HAS_coronalimage (cmd+dadd) ){ RNGIM(im3d->s312); RETURN(0); }

   ERROR_message("Command '%s': no image window specified",cmd) ;
   RETURN(1) ;
}
#undef RNGIM

/*--------------------------------------------------------------------*/
/* See README.driver for details */

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
