/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "to3d.h"

#define LABEL_ARG(str) \
  XtVaTypedArg , XmNlabelString , XmRString , (str) , strlen(str)+1

/*------------------ global variables: so shoot me ---------------------*/

static to3d_widget_set wset ;
static to3d_data       user_inputs ;

static THD_3dim_dataset * dset    = NULL ;
static THD_datablock    * dblk    = NULL ;
static THD_dataxes      * daxes   = NULL ;
static THD_diskptr      * dkptr   = NULL ;
static THD_marker_set   * markers = NULL ;
static THD_string_array * imnames = NULL ;  /* name for each slice */

static int     Argc , First_Image_Arg = 1 ;
static char ** Argv ;
static char *  dbrick = NULL ;  /* global image data array */
static int     geomparent_loaded = 0 ;
static int     geometry_loaded   = 0 ;

static int     negative_shorts   = 0 ;  /* 19 Jan 2000 */
static int     nfloat_err        = 0 ;  /* 14 Sep 1999 */

#ifdef AFNI_DEBUG
#  define QQQ(str) (printf("to3d: %s\n",str),fflush(stdout))
#else
#  define QQQ(str)
#endif

/*** additions of Mar 2, 1995 ***/

#define FatalError(str) \
   ( fprintf(stderr,"\n*** %s\n\n try 'to3d -help'\n",(str)) , exit(1) )

static struct {
   int ncolor ;       /* -ncolor # */
   float gamma ;      /* -gamma #  */
   Boolean xtwarns ;  /* -xtwarns  */
   float gsfac ;      /* -gsfac    */
   int   datum_all ;  /* -datum    */

#ifdef USE_MRI_DELAY
   int delay_input ;  /* -in:1     */
#endif

   int editing ;      /* -edit     */

   int swap_two , swap_four ;  /* 14 Sep 1998 */

   int nofloatscan ;           /* 14 Sep 1999 */

} argopt  ;

/*----------------------------------------------------------------------*/
static char * FALLback[] =
  {   "*fontList:        9x15bold=charset1" ,
      "*background:      gray40"            ,
      "*menu*background: gray40"            ,
      "*borderColor:     gray40"            ,
      "*foreground:      yellow"            ,
      "*borderWidth:     0"                 ,
      "*troughColor:     green"             ,
      "*XmLabel.translations: #override<Btn2Down>:" ,
      "*help*background:      black"                ,
      "*help*foreground:      yellow"               ,
      "*cluefont:             9x15bold"             ,
      "*help*waitPeriod:      1066"                 ,
      "*help*cancelWaitPeriod: 50"                  ,
   NULL } ;
/*-----------------------------------------------------------------------*/

/*-----------------------------------------------------------------------
   This routine is used to hide the Xt warnings.
   It simply does nothing -- it replaces the default Xt warning handler.
-------------------------------------------------------------------------*/

void AFNI_handler(char * msg){}

void AFNI_startup_timeout_CB( XtPointer client_data , XtIntervalId * id )
{
   char msg[512] ;

   MCW_help_CB(NULL,NULL,NULL) ;
   MCW_alter_widget_cursor( wset.topshell , -XC_left_ptr , "yellow","blue" ) ;

   if( negative_shorts && !AFNI_yesenv("AFNI_NO_NEGATIVES_WARNING") ){ /* 19 Jan 2000 */
      sprintf(msg , " \n"
                    " WARNING: %d negative voxels were\n"
                    "          read in images of shorts.\n"
                    "          It is possible the input\n"
                    "          images need byte-swapping.\n \n"
                    " ** I recommend that you View Images. **\n" ,
              negative_shorts ) ;

      (void) MCW_popup_message( wset.anatomy_parent_label , msg ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;
   }

   if( nfloat_err ){ /* 20 Jan 2000 */
      sprintf(msg , " \n"
                    " WARNING: %d errors in floating point images\n"
                    "          were detected.  It is possible that\n"
                    "          the inputs need to be 4swap-ed, or\n"
                    "          otherwise repaired.\n \n"
                    " ** Erroneous values have been replaced by\n"
                    " ** zeros. I recommend that you View Images. **\n" ,
              nfloat_err ) ;

      (void) MCW_popup_message( wset.geometry_parent_label , msg ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;
   }
}

/*-----------------------------------------------------------------------*/

static char * commandline = NULL ;  /* for History */

int main( int argc , char * argv[] )
{
   XtAppContext   app ;
   XtErrorHandler old_handler ;
   Boolean        all_good ;

   /* read the user data from the command line, if any */

   wset.topshell = NULL ;  /* flag that X has yet to start */

   /* initialize defaults for command line arguments
      that aren't related to dataset construction    */

   INIT_ngray       = NGRAY ;
   INIT_gamma       = GAMMA ;
   INIT_fov         = 240.0 ;
   argopt.ncolor    = -1 ;
   argopt.gamma     = -1.0 ;
   argopt.xtwarns   = False ;         /* don't show Xt warnings */
   argopt.gsfac     = 0.0 ;           /* no global scaling factor */
   argopt.datum_all = ILLEGAL_TYPE ;  /* use first image type */
#ifdef USE_MRI_DELAY
   argopt.delay_input = FALSE ;
#endif
   argopt.editing     = FALSE ;

   argopt.swap_two = argopt.swap_four = 0 ;  /* 14 Sep 1998 */

   argopt.nofloatscan = 0 ;                  /* 14 Sep 1999 */

   /* read the inputs */

   printf("to3d: 2D slices into 3D datasets for AFNI, by RW Cox\n") ;

   Argc = argc ;
   Argv = argv ;

   T3D_initialize_user_data() ;
   T3D_read_images() ;

   if( negative_shorts )
      printf("WARNING: %d negative voxels were read in images of shorts.\n"
             "         It is possible the input images need byte-swapping.\n",
             negative_shorts ) ;

QQQ("main1");

   if( strlen(user_inputs.geometry_parent_filename) > 0 )
      T3D_geometry_parent_CB( NULL , NULL , NULL ) ;

   if( strlen(user_inputs.anatomy_parent_filename) > 0 )
      T3D_anatomy_parent_CB( NULL , NULL , NULL ) ;

   all_good = T3D_check_data( False ) ;

   /** Mar 1997: if any FOV or SLAB command line inputs are used,
                 then require that they all be given.            **/

   if( all_good ){
      int iii =  (user_inputs.xincode > 0)
               + (user_inputs.yincode > 0) + (user_inputs.zincode > 0) ;

      if( iii > 0 && iii < 3 ) all_good = False ;
   }

   if( all_good && !user_inputs.nosave ){      /* done! */
      T3D_save_file_CB( NULL , NULL , NULL ) ;
      printf("3D dataset written to disk\n") ;
      exit(0) ;
   }

   /* Otherwise, initialize X11 and Xt */

   printf("Making widgets") ; fflush(stdout) ;

   wset.topshell = XtVaAppInitialize( &app , "AFNI" , NULL , 0 ,
                                      &argc , argv , FALLback , NULL ) ;

   AFNI_load_defaults( wset.topshell ) ;
   if( argopt.ncolor <= 2   ) argopt.ncolor = INIT_ngray ;
   if( argopt.gamma  <= 0.0 ) argopt.gamma  = INIT_gamma ;

   if( ! (geomparent_loaded || geometry_loaded) ) user_inputs.fov = INIT_fov ;

   if( argopt.xtwarns == False )
      old_handler = XtAppSetWarningHandler(app,AFNI_handler) ;  /* turn off */

   if( MCW_isitmwm(wset.topshell) ){
      XtVaSetValues( wset.topshell ,
                        XmNmwmDecorations ,
                        MWM_DECOR_ALL | MWM_DECOR_RESIZEH | MWM_DECOR_MAXIMIZE ,
                     NULL ) ;

      /* turn off Drag-n-Drop (courtesy the Motif FAQ) */

      XtVaSetValues( XmGetXmDisplay(XtDisplay(wset.topshell)) ,
                        XmNdragInitiatorProtocolStyle , XmDRAG_NONE ,
                        XmNdragReceiverProtocolStyle  , XmDRAG_NONE ,
                     NULL ) ;
   }

   /* make the rest of the widgets */

   T3D_create_widgets() ;
   MCW_help_CB(wset.topshell,NULL,NULL) ;  /* initialize help widget */

   printf("\n") ; fflush(stdout) ;

QQQ("main2");

   if( AFNI_noenv("AFNI_HINTS") ) MCW_hint_toggle() ;

   /* let the user do the rest */

   (void) XtAppAddTimeOut( app , 1000 , AFNI_startup_timeout_CB , NULL ) ;
   XtAppMainLoop(app) ;
   exit(0) ;
}

/*---------------------------------------------------------------------*/

void T3D_create_widgets(void)
{
   wset.dc  = MCW_new_DC( wset.topshell, argopt.ncolor,
                          NCOLOVR,FD_colovr,FD_colovr, argopt.gamma , 0 );
   wset.seq = NULL ;  /* no viewing open now */

   /*---- form to hold all widgets ----*/

   wset.topform = XtVaCreateWidget(
                     "dialog" , xmFormWidgetClass , wset.topshell ,
                         XmNborderWidth , 0 ,
#if 1
                         XmNtraversalOn , False ,
#endif
                     NULL ) ;

   MCW_register_help( wset.topform ,
                      "I weep for Adonais--he is dead!\n"
                      "Oh, weep for Adonais! though our tears\n"
                      "Thaw not the frost which binds so dear a head!\n"
                      "And thou, sad Hour, selected from all years\n"
                      "To mourn our loss, rouse thy obscure compeers,\n"
                      "And teach them thine own sorrow, say: 'With me\n"
                      "Died Adonais; till the Future dares\n"
                      "Forget the Past, his fate and fame shall be\n"
                      "An echo and a light unto eternity!'"
                    ) ;

   /*---- 3 arrowvals for orientation ----*/

   wset.xorient_av = new_MCW_arrowval(
                      wset.topform ,                     /* parent */
                      "x orientation\n(across screen)" , /* label */
                      MCW_AV_downup ,                    /* arrows */
                      FIRST_ORIENT_TYPE ,                /* min */
                      LAST_ORIENT_TYPE ,                 /* max */
                      user_inputs.xorient ,              /* init */
                      MCW_AV_readtext ,                  /* text */
                      0 ,                                /* decimals */
                      T3D_orient_av_CB , NULL ,          /* callback */
                      T3D_text_display ,                 /* text maker */
                      ORIENT_typestr                     /* and data */
                     ) ;

   XtVaSetValues( wset.xorient_av->wrowcol ,
                     XmNleftAttachment , XmATTACH_FORM ,
                     XmNleftOffset     , T3D_FORM_SPACING ,
                     XmNtopAttachment  , XmATTACH_FORM ,
                     XmNtopOffset      , T3D_FORM_SPACING ,
                  NULL ) ;

   wset.yorient_av = new_MCW_arrowval(
                      wset.topform ,                     /* parent */
                      "y orientation\n (down screen) " , /* label */
                      MCW_AV_downup ,                    /* arrows */
                      FIRST_ORIENT_TYPE ,                /* min */
                      LAST_ORIENT_TYPE ,                 /* max */
                      user_inputs.yorient ,              /* init */
                      MCW_AV_readtext ,                  /* text */
                      0 ,                                /* decimals */
                      T3D_orient_av_CB , NULL ,          /* callback */
                      T3D_text_display ,                 /* text maker */
                      ORIENT_typestr                     /* and data */
                     ) ;

   XtVaSetValues( wset.yorient_av->wrowcol ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopWidget      , wset.xorient_av->wrowcol ,
                     XmNtopOffset      , T3D_FORM_SPACING ,
                     XmNleftAttachment , XmATTACH_FORM ,
                     XmNleftOffset     , T3D_FORM_SPACING ,
                  NULL ) ;

   wset.zorient_av = new_MCW_arrowval(
                      wset.topform ,                     /* parent */
                      "z orientation\n(slices 0,1,..)" , /* label */
                      MCW_AV_downup ,                    /* arrows */
                      FIRST_ORIENT_TYPE ,                /* min */
                      LAST_ORIENT_TYPE ,                 /* max */
                      user_inputs.zorient ,              /* init */
                      MCW_AV_readtext ,                  /* text */
                      0 ,                                /* decimals */
                      T3D_orient_av_CB , NULL ,          /* callback */
                      T3D_text_display ,                 /* text maker */
                      ORIENT_typestr                     /* and data */
                     ) ;

   XtVaSetValues( wset.zorient_av->wrowcol ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopWidget      , wset.yorient_av->wrowcol ,
                     XmNtopOffset      , T3D_FORM_SPACING ,
                     XmNleftAttachment , XmATTACH_FORM ,
                     XmNleftOffset     , T3D_FORM_SPACING ,
                  NULL ) ;

   XtVaSetValues( wset.xorient_av->wtext ,
                     XmNcolumns   , LONGEST_ORIENT_TYPESTR ,
                     XmNmaxLength , LONGEST_ORIENT_TYPESTR ,
                  NULL ) ;

   XtVaSetValues( wset.yorient_av->wtext ,
                     XmNcolumns   , LONGEST_ORIENT_TYPESTR ,
                     XmNmaxLength , LONGEST_ORIENT_TYPESTR ,
                  NULL ) ;

   XtVaSetValues( wset.zorient_av->wtext ,
                     XmNcolumns   , LONGEST_ORIENT_TYPESTR ,
                     XmNmaxLength , LONGEST_ORIENT_TYPESTR ,
                  NULL ) ;

   wset.xorient_av->allow_wrap = 1 ;
   wset.yorient_av->allow_wrap = 1 ;
   wset.zorient_av->allow_wrap = 1 ;

   MCW_reghelp_children( wset.xorient_av->wrowcol ,
      "Choose the anatomical orientation\n"
      "which fits the images when displayed\n"
      "in their ORIGINAL orientation:\n"
      "left->right, across the screen" ) ;
   MCW_reghint_children( wset.xorient_av->wrowcol ,
                         "Anatomical orientation across screen" ) ;

   MCW_reghelp_children( wset.yorient_av->wrowcol ,
      "Choose the anatomical orientation\n"
      "which fits the images when displayed\n"
      "in their ORIGINAL orientation:\n"
      "top->bottom, down the screen" ) ;
   MCW_reghint_children( wset.yorient_av->wrowcol ,
                         "Anatomical orientation down screen" ) ;

   MCW_reghelp_children( wset.zorient_av->wrowcol ,
      "Choose the anatomical orientation\n"
      "which fits the images when displayed\n"
      "in their ORIGINAL orientation:\n"
      "slice order, `thru' the screen" ) ;
   MCW_reghint_children( wset.zorient_av->wrowcol ,
                         "Anatomical orientation in slice direction" ) ;

QQQ("orientations setup") ;
   printf(".");fflush(stdout);

   /*---- bbox and arrowvals for voxel sizes ----*/

   wset.xsize_av = new_MCW_arrowval(
                      wset.topform ,                /* parent */
                      "x voxel\nsize (mm)" ,        /* label */
                      MCW_AV_downup ,               /* arrows */
                      1 ,                           /* min */
                      1000 ,                        /* max */
                      (int)(100*user_inputs.xsize), /* init */
                      MCW_AV_editext ,              /* text */
                      2 ,                           /* decimals */
                      T3D_size_av_CB , NULL ,       /* callback */
                      NULL , NULL                   /* text maker */
                     ) ;

   XtVaSetValues( wset.xsize_av->wrowcol ,
                     XmNtopAttachment  , XmATTACH_FORM ,
                     XmNtopOffset      , T3D_FORM_SPACING ,
                     XmNleftAttachment , XmATTACH_WIDGET ,
                     XmNleftWidget     , wset.xorient_av->wrowcol ,
                     XmNleftOffset     , T3D_FORM_SPACING ,
                  NULL ) ;

/*
   AV_assign_fval( wset.xsize_av , user_inputs.xsize ) ;
*/

   wset.ysize_av = new_MCW_arrowval(
                      wset.topform ,                /* parent */
                      "y voxel\nsize (mm)" ,        /* label */
                      MCW_AV_downup ,               /* arrows */
                      1 ,                           /* min */
                      4000 ,                        /* max */
                      (int)(100*user_inputs.ysize), /* init */
                      MCW_AV_editext ,              /* text */
                      2 ,                           /* decimals */
                      T3D_size_av_CB , NULL ,       /* callback */
                      NULL , NULL                   /* text maker */
                     ) ;

   XtVaSetValues( wset.ysize_av->wrowcol ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopWidget      , wset.xsize_av->wrowcol ,
                     XmNtopOffset      , T3D_FORM_SPACING ,
                     XmNleftAttachment , XmATTACH_WIDGET ,
                     XmNleftWidget     , wset.xorient_av->wrowcol ,
                     XmNleftOffset     , T3D_FORM_SPACING ,
                  NULL ) ;

   wset.zsize_av = new_MCW_arrowval(
                      wset.topform ,                /* parent */
                      "z voxel\nsize (mm)" ,        /* label */
                      MCW_AV_downup ,               /* arrows */
                      1 ,                           /* min */
                      4000 ,                        /* max */
                      (int)(100*user_inputs.zsize), /* init */
                      MCW_AV_editext ,              /* text */
                      2 ,                           /* decimals */
                      T3D_size_av_CB , NULL ,       /* callback */
                      NULL , NULL                   /* text maker */
                     ) ;

   XtVaSetValues( wset.zsize_av->wrowcol ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopWidget      , wset.ysize_av->wrowcol ,
                     XmNtopOffset      , T3D_FORM_SPACING ,
                     XmNleftAttachment , XmATTACH_WIDGET ,
                     XmNleftWidget     , wset.xorient_av->wrowcol ,
                     XmNleftOffset     , T3D_FORM_SPACING ,
                  NULL ) ;

#ifdef ALLOW_NONCONTIG
   wset.zspacing_av = new_MCW_arrowval(
                        wset.topform ,                    /* parent */
                        "z voxel\nspacing  " ,            /* label */
                        MCW_AV_downup ,                   /* arrows */
                        1 ,                               /* min */
                        4000 ,                            /* max */
                        (int)(100*user_inputs.zspacing),  /* init */
                        MCW_AV_editext ,                  /* text */
                        2 ,                               /* decimals */
                        T3D_size_av_CB , NULL ,           /* callback */
                        NULL , NULL                       /* text maker */
                      ) ;

   XtVaSetValues( wset.zspacing_av->wrowcol ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopWidget      , wset.zsize_av->wrowcol ,
                     XmNtopOffset      , T3D_FORM_SPACING ,
                     XmNleftAttachment , XmATTACH_WIDGET ,
                     XmNleftWidget     , wset.xorient_av->wrowcol ,
                     XmNleftOffset     , T3D_FORM_SPACING ,
                  NULL ) ;
#endif /* ALLOW_NONCONTIG */

   wset.fov_av = new_MCW_arrowval(
                      wset.topform ,             /* parent */
                      "Field of\nview (mm)" ,    /* label */
                      MCW_AV_downup ,            /* arrows */
                      1 ,                        /* min */
                      10000 ,                    /* max */
                      (int)(10*user_inputs.fov), /* init */
                      MCW_AV_editext ,           /* text */
                      1 ,                        /* decimals */
                      T3D_fov_av_CB , NULL ,     /* callback */
                      NULL , NULL                /* text maker */
                     ) ;

   XtVaSetValues( wset.fov_av->wrowcol ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
#ifdef ALLOW_NONCONTIG
                     XmNtopWidget      , wset.zspacing_av->wrowcol ,
#else
                     XmNtopWidget      , wset.zsize_av->wrowcol ,
#endif
                     XmNtopOffset      , T3D_FORM_SPACING ,
                     XmNleftAttachment , XmATTACH_WIDGET ,
                     XmNleftWidget     , wset.xorient_av->wrowcol ,
                     XmNleftOffset     , T3D_FORM_SPACING ,
                  NULL ) ;

   wset.voxshape_bbox = new_MCW_bbox(
                          wset.topform ,        /* parent */
                          3 ,                   /* num buttons */
                          T3D_voxshape_label ,  /* labels */
                          MCW_BB_radio_one ,    /* bbox type */
                          MCW_BB_frame ,        /* use a frame */
                          T3D_voxshape_CB ,     /* callback */
                          NULL                  /* callback data */
                        ) ;

   XtVaSetValues( wset.voxshape_bbox->wtop ,
                     XmNrightAttachment  , XmATTACH_WIDGET ,
                     XmNrightWidget      , wset.fov_av->wrowcol ,
                     XmNrightOffset      , T3D_FORM_SPACING ,
                     XmNtopAttachment    , XmATTACH_OPPOSITE_WIDGET ,
                     XmNtopWidget        , wset.fov_av->wrowcol ,
                     XmNtopOffset        , 0 ,
                  NULL ) ;

#ifdef ALLOW_NONCONTIG
   wset.voxcontig_bbox = new_MCW_bbox(
                          wset.topform ,        /* parent */
                          3 ,                   /* num buttons */
                          T3D_voxcontig_label , /* labels */
                          MCW_BB_radio_one ,    /* bbox type */
                          MCW_BB_frame ,        /* use a frame */
                          T3D_voxcontig_CB ,    /* callback */
                          NULL                  /* callback data */
                        ) ;

   XtSetSensitive( wset.voxcontig_bbox->wbut[1] , False ) ;
   XtSetSensitive( wset.voxcontig_bbox->wbut[2] , False ) ;

   XtVaSetValues( wset.voxcontig_bbox->wtop ,
                     XmNrightAttachment  , XmATTACH_WIDGET ,
                     XmNrightWidget      , wset.voxshape_bbox->wtop ,
                     XmNrightOffset      , T3D_FORM_SPACING ,
                     XmNtopAttachment    , XmATTACH_OPPOSITE_WIDGET ,
                     XmNtopWidget        , wset.voxshape_bbox->wtop ,
                     XmNtopOffset        , 0 ,
                  NULL ) ;
#endif /* ALLOW_NONCONTIG */

   MCW_reghelp_children( wset.xsize_av->wrowcol ,
    "If in-slice voxel dimensions are\n"
    "not square, use this to set the\n"
    "x axis (across screen) dimension" ) ;
   MCW_reghint_children( wset.xsize_av->wrowcol ,
                         "Across screen voxel size" ) ;

   MCW_reghelp_children( wset.ysize_av->wrowcol ,
    "If in-slice voxel dimensions are\n"
    "not square, use this to set the\n"
    "y axis (down screen) dimension" ) ;
   MCW_reghint_children( wset.ysize_av->wrowcol ,
                         "Down screen voxel size" ) ;

   MCW_reghelp_children( wset.zsize_av->wrowcol ,
    "If voxels are not cubical,\n"
    "use this to set the z axis\n"
    "(slice direction) thickness" ) ;
   MCW_reghint_children( wset.ysize_av->wrowcol ,
                         "Slice direction voxel size" ) ;

#ifdef ALLOW_NONCONTIG
   MCW_reghelp_children( wset.zspacing_av->wrowcol ,
    "If the slices are not\n"
    "contiguous, use this to\n"
    "set the center-to-center\n"
    "slice spacing" ) ;
#endif /* ALLOW_NONCONTIG */

   MCW_reghelp_children( wset.fov_av->wrowcol ,
    "If the voxels are cubical,\n"
    "or at least square, use this\n"
    "to specify the sizes by giving\n"
    "the Field-Of-View dimension\n"
    "(the width across screen of images)" ) ;
   MCW_reghint_children( wset.fov_av->wrowcol ,
                         "In-slice width of entire image" ) ;

   MCW_reghelp_children( wset.voxshape_bbox->wrowcol ,
                         "Choose one button to\n"
                         "specify the voxel 'shape':\n"
                         " cubical  = all dimensions equal\n"
                         " square   = x & y dimensions\n"
                         "            equal, z different\n"
                         " irregular= all dimensions unequal" ) ;
   MCW_reghint_children( wset.voxshape_bbox->wrowcol ,
                         "Specify voxel shape" ) ;

#ifdef ALLOW_NONCONTIG
   MCW_reghelp_children( wset.voxcontig_bbox->wrowcol ,
    "Pressed IN:  slices are contiguous\n"
    "Pressed OUT: slices are not contiguous\n"
    "(i.e., z size and z spacing differ)" ) ;
#endif /* ALLOW_NONCONTIG */

   /*-- set the vox*_bbox values depending on initial sizes --*/

   if( user_inputs.xsize != user_inputs.ysize ){
      user_inputs.voxshape = VOXSHAPE_IRREGULAR ;
   } else {
      if( user_inputs.xsize != user_inputs.zsize ){
         user_inputs.voxshape = VOXSHAPE_SQUARE ;
      } else {
         user_inputs.voxshape = VOXSHAPE_CUBICAL ;
      }
   }

QQQ("voxshapes setup") ;
   printf(".");fflush(stdout);

   /*---- Label to show the TR (for 3D+time datasets) ----*/

   { char buf[96] ;
     XmString xstr ;

     sprintf( buf,"TR = %.3f (%s)\nNR = %d Nz = %d",
              user_inputs.TR, UNITS_TYPE_LABEL(user_inputs.tunits) ,
              user_inputs.ntt,user_inputs.nzz ) ;
     xstr = XmStringCreateLtoR( buf , XmFONTLIST_DEFAULT_TAG ) ;

     wset.TR_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , wset.topform ,
            XmNlabelString      , xstr ,
            XmNrecomputeSize    , False ,
            XmNalignment        , XmALIGNMENT_BEGINNING ,
            XmNtopAttachment    , XmATTACH_WIDGET ,
            XmNtopWidget        , wset.fov_av->wrowcol ,
            XmNtopOffset        , T3D_FORM_SPACING ,
            XmNleftAttachment   , XmATTACH_WIDGET ,
            XmNleftWidget       , wset.voxshape_bbox->wtop ,
            XmNleftOffset       , T3D_FORM_SPACING ,
         NULL ) ;

      XmStringFree( xstr ) ;

      MCW_register_help( wset.TR_label ,
         "Shows the TR (inter-brick time), number\n"
         "of bricks NR, and number of slizes Nz,\n"
         "for time-dependent datasets.\n"
         "These CANNOT be changed interactively.\n"
         "They can only be set on the command line." ) ;

      MCW_register_hint( wset.TR_label , "3D+time parameters" ) ;

     if( user_inputs.ntt < 2 )
        XtUnmanageChild( wset.TR_label ) ;
   }

   /*---- arrowvals for origin ----*/

   wset.xorigin_av = new_MCW_arrowval(
                        wset.topform ,                  /* parent */
                        "x origin (mm)\n[left edge]" ,  /* label */
                        MCW_AV_downup ,                 /* arrows */
                       -50000 ,                         /* min */
                        50000 ,                         /* max */
                        (int)(100*user_inputs.xorigin), /* init */
                        MCW_AV_editext ,                /* text */
                        2 ,                             /* decimals */
                        T3D_origin_av_CB , NULL ,       /* callback */
                        NULL , NULL                     /* text maker */
                     ) ;

   XtVaSetValues( wset.xorigin_av->wrowcol ,
                     XmNleftAttachment , XmATTACH_WIDGET ,
                     XmNleftWidget     , wset.xsize_av->wrowcol ,
                     XmNleftOffset     , T3D_FORM_SPACING ,
                     XmNtopAttachment  , XmATTACH_FORM ,
                     XmNtopOffset      , T3D_FORM_SPACING ,
                  NULL ) ;

   wset.xorigin_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , wset.topform ,
            LABEL_ARG("X") ,
            XmNrecomputeSize  , False ,
            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftWidget     , wset.xorigin_av->wrowcol ,
            XmNleftOffset     , 1 ,
            XmNtopAttachment  , XmATTACH_OPPOSITE_WIDGET ,
            XmNtopWidget      , wset.xorigin_av->wrowcol ,
            XmNtopOffset      , 2 ,
         NULL ) ;

   wset.yorigin_av = new_MCW_arrowval(
                        wset.topform ,                  /* parent */
                        "y origin (mm)\n[top edge]" ,   /* label */
                        MCW_AV_downup ,                 /* arrows */
                       -50000 ,                         /* min */
                        50000 ,                         /* max */
                        (int)(100*user_inputs.yorigin), /* init */
                        MCW_AV_editext ,                /* text */
                        2 ,                             /* decimals */
                        T3D_origin_av_CB , NULL ,       /* callback */
                        NULL , NULL                     /* text maker */
                     ) ;

   XtVaSetValues( wset.yorigin_av->wrowcol ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopWidget      , wset.xorigin_av->wrowcol ,
                     XmNtopOffset      , T3D_FORM_SPACING ,
                     XmNleftAttachment , XmATTACH_WIDGET ,
                     XmNleftWidget     , wset.xsize_av->wrowcol ,
                     XmNleftOffset     , T3D_FORM_SPACING ,
                  NULL ) ;

   wset.yorigin_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , wset.topform ,
            LABEL_ARG("X") ,
            XmNrecomputeSize  , False ,
            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftWidget     , wset.yorigin_av->wrowcol ,
            XmNleftOffset     , 1 ,
            XmNtopAttachment  , XmATTACH_OPPOSITE_WIDGET ,
            XmNtopWidget      , wset.yorigin_av->wrowcol ,
            XmNtopOffset      , 2 ,
         NULL ) ;

   wset.zorigin_av = new_MCW_arrowval(
                        wset.topform ,                  /* parent */
                        "z origin (mm)\n[slice 0]" ,    /* label */
                        MCW_AV_downup ,                 /* arrows */
                       -50000 ,                         /* min */
                        50000 ,                         /* max */
                        (int)(100*user_inputs.zorigin), /* init */
                        MCW_AV_editext ,                /* text */
                        2 ,                             /* decimals */
                        T3D_origin_av_CB , NULL ,       /* callback */
                        NULL , NULL                     /* text maker */
                     ) ;

   XtVaSetValues( wset.zorigin_av->wrowcol ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopWidget      , wset.yorigin_av->wrowcol ,
                     XmNtopOffset      , T3D_FORM_SPACING ,
                     XmNleftAttachment , XmATTACH_WIDGET ,
                     XmNleftWidget     , wset.xsize_av->wrowcol ,
                     XmNleftOffset     , T3D_FORM_SPACING ,
                  NULL ) ;

   wset.zorigin_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , wset.topform ,
            LABEL_ARG("X") ,
            XmNrecomputeSize  , False ,
            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftWidget     , wset.zorigin_av->wrowcol ,
            XmNleftOffset     , 1 ,
            XmNtopAttachment  , XmATTACH_OPPOSITE_WIDGET ,
            XmNtopWidget      , wset.zorigin_av->wrowcol ,
            XmNtopOffset      , 2 ,
         NULL ) ;

   wset.centered_bbox = new_MCW_bbox(
                          wset.topform ,       /* parent */
                          3 ,                  /* num buttons */
                          T3D_centered_label , /* labels */
                          MCW_BB_check ,       /* bbox type */
                          MCW_BB_frame ,       /* use a frame */
                          T3D_centered_CB ,    /* callback */
                          NULL                 /* callback data */
                        ) ;

   XtVaSetValues( wset.centered_bbox->wtop ,
#if 0
                     XmNleftAttachment   , XmATTACH_WIDGET ,
                     XmNleftWidget       , wset.fov_av->wrowcol ,
                     XmNleftOffset       , T3D_FORM_SPACING ,
#else
                     XmNrightAttachment  , XmATTACH_OPPOSITE_WIDGET ,
                     XmNrightWidget      , wset.zorigin_av->wrowcol ,
                     XmNrightOffset      , 0 ,
#endif
                     XmNtopAttachment    , XmATTACH_OPPOSITE_WIDGET ,
                     XmNtopWidget        , wset.fov_av->wrowcol ,
                     XmNtopOffset        , 0 ,
                  NULL ) ;

   MCW_reghelp_children( wset.centered_bbox->wrowcol ,
    "Choose any combination of\n"
    "buttons to indicate whether\n"
    "each axis is centered\n"
    "in the gradient coil fields:\n"
    "Pressed IN means centered\n"
    "Pressed OUT means not centered" ) ;
   MCW_reghint_children( wset.centered_bbox->wrowcol ,
                         "Is data domain centered around 0?" ) ;

   MCW_reghelp_children( wset.xorigin_av->wrowcol ,
      "Use the arrows (or type) to enter\n"
      "the x-axis distance from the center of\n"
      "the first voxel to the\n"
      "gradient coil (0,0,0) point" ) ;
   MCW_reghint_children( wset.xorigin_av->wrowcol ,
                         "X-axis distance to 1st voxel center" ) ;

   MCW_reghelp_children( wset.yorigin_av->wrowcol ,
      "Use the arrows (or type) to enter\n"
      "the y-axis distance from the center of\n"
      "the first voxel to the\n"
      "gradient coil (0,0,0) point" ) ;
   MCW_reghint_children( wset.yorigin_av->wrowcol ,
                         "Y-axis distance to 1st voxel center" ) ;

   MCW_reghelp_children( wset.zorigin_av->wrowcol ,
      "Use the arrows (or type) to enter\n"
      "the z-axis distance from the center of\n"
      "the first voxel to the\n"
      "gradient coil (0,0,0) point" ) ;
   MCW_reghint_children( wset.zorigin_av->wrowcol ,
                         "Z-axis distance to 1st voxel center" ) ;

   MCW_register_help( wset.xorigin_label ,
                      "Shows the direction that the\n"
                      "x origin distance applies" ) ;
   MCW_register_hint( wset.xorigin_label ,
                      "Direction of x origin distance" ) ;

   MCW_register_help( wset.yorigin_label ,
                      "Shows the direction that the\n"
                      "y origin distance applies" ) ;
   MCW_register_hint( wset.yorigin_label ,
                      "Direction of y origin distance" ) ;

   MCW_register_help( wset.zorigin_label ,
                      "Shows the direction that the\n"
                      "z origin distance applies" ) ;
   MCW_register_hint( wset.zorigin_label ,
                      "Direction of z origin distance" ) ;

QQQ("origins setup") ;
   printf(".");fflush(stdout);

   /*----- a separator to keep the geometry stuff separate -----*/

   wset.region_separator =
      XtVaCreateManagedWidget(
         "dialog" , xmSeparatorGadgetClass , wset.topform ,
            XmNseparatorType   , XmDOUBLE_LINE ,
            XmNmargin          , 3             ,
            XmNleftAttachment  , XmATTACH_FORM ,
            XmNrightAttachment , XmATTACH_FORM ,
            XmNtopAttachment   , XmATTACH_WIDGET ,
            XmNtopOffset       , T3D_FORM_SPACING ,
            XmNtopWidget       , wset.voxshape_bbox->wtop ,
         NULL ) ;

   MCW_register_help( wset.region_separator ,
                        "Thou art indeed just, Lord, if I contend\n"
                        "With thee; but, sir, so what I plead is just.\n"
                        "Why do sinners' ways prosper? and why must\n"
                        "Disappointment all I endeavour end?\n"
                        "Wert thou my enemy, O thou my friend,\n"
                        "How wouldst thou worse, I wonder, than thou dost\n"
                        "Defeat, thwart me? Oh, the sots and thralls of lust\n"
                        "Do in spare hours more thrive than I that spend,\n"
                        "Sir, life upon thy cause. See, banks and brakes\n"
                        "Now, leav`ed how thick! lac`ed they are again\n"
                        "With fretty chervil, look, and fresh wind shakes\n"
                        "Them; birds build -- but not I build; no, but strain,\n"
                        "Time's eunuch, and not breed one work that wakes.\n"
                        "Mine, O thou lord of life, send my roots rain."
                    ) ;

   /*---- April 1996: arrowval for view_type ----*/

   wset.view_type_av = new_MCW_arrowval(
                         wset.topform ,                   /* parent */
                         "View:  " ,                      /* label */
                         MCW_AV_downup ,                  /* arrows */
                         FIRST_VIEW_TYPE ,                /* min */
                         LAST_VIEW_TYPE ,                 /* max */
                         user_inputs.view_type ,          /* init */
                         MCW_AV_readtext ,                /* text */
                         0 ,                              /* decimals */
                         T3D_orient_av_CB , NULL ,        /* callback */
                         T3D_text_display ,               /* text maker */
                         VIEW_typestr                     /* and data */
                       ) ;

   XtVaSetValues( wset.view_type_av->wrowcol ,
                     XmNbottomAttachment , XmATTACH_WIDGET ,
                     XmNbottomWidget     , wset.region_separator ,
                     XmNbottomOffset     , T3D_FORM_SPACING ,
                     XmNleftAttachment   , XmATTACH_FORM ,
                     XmNleftOffset       , T3D_FORM_SPACING ,
                  NULL ) ;

   XtVaSetValues( wset.view_type_av->wtext ,
                     XmNcolumns   , LONGEST_VIEW_TYPESTR ,
                     XmNmaxLength , LONGEST_VIEW_TYPESTR ,
                  NULL ) ;

   MCW_reghelp_children( wset.view_type_av->wrowcol ,
      "Sets the view type for the\n"
      "dataset being created.  Except\n"
      "under unusual circumstances,\n"
      "this should be " VIEW_ORIGINAL_STR ) ;

   /*---- Label to show what kind of data is stored ----*/

   { char buf[32] ;
     sprintf( buf , "Datum: %s\n" , MRI_TYPE_name[argopt.datum_all] ) ;
     wset.datum_label =
        XtVaCreateManagedWidget(
           "dialog" , xmLabelWidgetClass , wset.topform ,
              LABEL_ARG(buf)   ,
              XmNbottomAttachment , XmATTACH_WIDGET ,
              XmNbottomWidget     , wset.view_type_av->wrowcol ,
              XmNbottomOffset     , 1 ,
              XmNleftAttachment   , XmATTACH_FORM ,
              XmNleftOffset       , T3D_FORM_SPACING ,
              XmNmarginHeight     , 0 ,
              XmNmarginWidth      , 0 ,
           NULL ) ;

   }
   MCW_register_help( wset.datum_label ,
     "This shows the type of data stored\n"
     "in the images as read into to3d.\n"
     "You can only alter this by exiting\n"
     "the program and using the -datum\n"
     "option when you run to3d again." ) ;
   MCW_register_hint( wset.datum_label , "Type of data stored in images" ) ;

   /*----- textfield widgets below the bar -----*/

   /** name widgets not being used no more **/

#ifndef NO_NAMES
   wset.dataset_name_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , wset.topform ,
            LABEL_ARG("Dataset name:  ") ,
            XmNtopAttachment   , XmATTACH_WIDGET ,
            XmNtopWidget       , wset.region_separator ,
            XmNtopOffset       , T3D_FORM_SPACING ,
            XmNleftAttachment  , XmATTACH_FORM ,
            XmNleftOffset      , T3D_FORM_SPACING ,
         NULL ) ;

   wset.dataset_name_textfield =
      XtVaCreateManagedWidget(
         "dialog" , xmTextFieldWidgetClass , wset.topform ,
            XmNcolumns         , 80 ,
            XmNeditable        , True ,
            XmNmaxLength       , THD_MAX_NAME-1 ,
            XmNresizeWidth     , False ,
            XmNcursorPositionVisible , True ,
            XmNblinkRate , 0 ,
/*
            XmNmarginHeight    , 1 ,
            XmNmarginWidth     , 1 ,
*/
            XmNvalue           , user_inputs.dataset_name ,

            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftWidget     , wset.dataset_name_label ,
            XmNleftOffset     , T3D_FORM_SPACING ,
            XmNtopAttachment  , XmATTACH_OPPOSITE_WIDGET ,
            XmNtopWidget      , wset.dataset_name_label ,
            XmNtopOffset      , 0 ,
         NULL ) ;

   MCW_register_help( wset.dataset_name_label ,
    "Every 3D dataset needs a name;\n"
    "Type in what you need to remind\n"
    "you of the subject/experiment" ) ;

   MCW_register_help( wset.dataset_name_textfield ,
    "Every 3D dataset needs a label;\n"
    "Type in what you need to remind\n"
    "you of the subject/experiment" ) ;

   wset.short_label1_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , wset.topform ,
            LABEL_ARG("Short label:   ") ,
            XmNleftAttachment  , XmATTACH_FORM ,
            XmNleftOffset      , T3D_FORM_SPACING ,
            XmNtopAttachment   , XmATTACH_WIDGET ,
            XmNtopWidget       , wset.dataset_name_textfield ,
            XmNtopOffset       , T3D_FORM_SPACING ,
         NULL ) ;

   wset.short_label1_textfield =
      XtVaCreateManagedWidget(
         "dialog" , xmTextFieldWidgetClass , wset.topform ,
            XmNcolumns         , T3D_NAME_WIDTH ,
            XmNeditable        , True ,
            XmNmaxLength       , THD_MAX_LABEL-1 ,
            XmNresizeWidth     , False ,
            XmNcursorPositionVisible , True ,
            XmNblinkRate , 0 ,
/*
            XmNmarginHeight    , 1 ,
            XmNmarginWidth     , 1 ,
*/
            XmNvalue           , user_inputs.short_label1 ,

            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftWidget     , wset.short_label1_label ,
            XmNleftOffset     , T3D_FORM_SPACING ,
            XmNtopAttachment  , XmATTACH_OPPOSITE_WIDGET ,
            XmNtopWidget      , wset.short_label1_label ,
            XmNtopOffset      , 0 ,
         NULL ) ;

   MCW_register_help( wset.short_label1_label ,
    "The short label is used\n"
    "for a compact reminder of\n"
    "the dataset contents;\n"
    "type your choice in" ) ;

   MCW_register_help( wset.short_label1_textfield ,
    "The short label is used\n"
    "for a compact reminder of\n"
    "the dataset contents;\n"
    "type your choice in" ) ;
#endif  /* NO_NAMES */

   wset.geometry_parent_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , wset.topform ,
            LABEL_ARG("Copy geometry  \nof this dataset") ,
            XmNleftAttachment  , XmATTACH_FORM ,
            XmNleftOffset      , T3D_FORM_SPACING ,
            XmNtopAttachment   , XmATTACH_WIDGET ,
#ifdef NO_NAMES
            XmNtopWidget       , wset.region_separator ,
#else
            XmNtopWidget       , wset.short_label1_textfield ,
#endif
            XmNtopOffset       , T3D_FORM_SPACING ,
         NULL ) ;

   wset.geometry_parent_textfield =
      XtVaCreateManagedWidget(
         "dialog" , xmTextFieldWidgetClass , wset.topform ,
            XmNcolumns         , T3D_NAME_WIDTH ,
            XmNeditable        , True ,
            XmNmaxLength       , THD_MAX_NAME-1 ,
            XmNresizeWidth     , False ,
            XmNcursorPositionVisible , True ,
            XmNblinkRate , 0 ,
/*
            XmNmarginHeight    , 1 ,
            XmNmarginWidth     , 1 ,
*/
            XmNvalue           , user_inputs.geometry_parent_filename ,

            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftWidget     , wset.geometry_parent_label ,
            XmNleftOffset     , T3D_FORM_SPACING ,
            XmNtopAttachment  , XmATTACH_OPPOSITE_WIDGET ,
            XmNtopWidget      , wset.geometry_parent_label ,
            XmNtopOffset      , 0 ,
         NULL ) ;

   XtAddCallback( wset.geometry_parent_textfield ,
                  XmNactivateCallback ,
                  T3D_geometry_parent_CB , NULL ) ;    /* return key */

   XtAddCallback( wset.geometry_parent_textfield ,
                  XmNlosingFocusCallback ,
                  T3D_geometry_parent_CB , NULL ) ;    /* Tab key */

   XtInsertEventHandler( wset.geometry_parent_textfield ,
                         LeaveWindowMask ,             /* pointer leaves */
                         FALSE ,
                         T3D_pointer_leave_EV , NULL ,
                         XtListTail ) ;                /* last in queue */

   MCW_register_help( wset.geometry_parent_label ,
    "If a previously created 3D dataset\n"
    "has the same geometry (voxel sizes,\n"
    "etc.), enter its header filename\n"
    "and press the Enter key" ) ;
   MCW_register_hint( wset.geometry_parent_label ,
                      "Use the geometry of another dataset" ) ;

   MCW_register_help( wset.geometry_parent_textfield ,
    "If a previously created 3D dataset\n"
    "has the same geometry (voxel sizes,\n"
    "etc.), enter its header filename name\n"
    "here and press the Enter key" ) ;
   MCW_register_hint( wset.geometry_parent_textfield ,
                      "Use the geometry of another dataset" ) ;

   wset.anatomy_parent_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , wset.topform ,
            LABEL_ARG("Anatomy parent \nis this dataset") ,
            XmNleftAttachment  , XmATTACH_WIDGET ,
            XmNleftWidget      , wset.geometry_parent_textfield ,
            XmNleftOffset      , T3D_FORM_SPACING ,
            XmNtopAttachment   , XmATTACH_WIDGET ,
#ifdef NO_NAMES
            XmNtopWidget       , wset.region_separator ,
#else
            XmNtopWidget       , wset.short_label1_textfield ,
#endif
            XmNtopOffset       , T3D_FORM_SPACING ,
         NULL ) ;

   wset.anatomy_parent_textfield =
      XtVaCreateManagedWidget(
         "dialog" , xmTextFieldWidgetClass , wset.topform ,
            XmNcolumns         , T3D_NAME_WIDTH ,
            XmNeditable        , True ,
            XmNmaxLength       , THD_MAX_NAME-1 ,
            XmNresizeWidth     , False ,
            XmNcursorPositionVisible , True ,
            XmNblinkRate , 0 ,
/*
            XmNmarginHeight    , 1 ,
            XmNmarginWidth     , 1 ,
*/
            XmNvalue           , user_inputs.anatomy_parent_filename ,

            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftOffset     , T3D_FORM_SPACING ,
            XmNleftWidget     , wset.anatomy_parent_label ,
            XmNtopAttachment  , XmATTACH_OPPOSITE_WIDGET ,
            XmNtopWidget      , wset.anatomy_parent_label ,
            XmNtopOffset      , 0 ,
         NULL ) ;

   XtAddCallback( wset.anatomy_parent_textfield ,
                  XmNactivateCallback ,
                  T3D_anatomy_parent_CB , NULL ) ;    /* return key */

   XtAddCallback( wset.anatomy_parent_textfield ,
                  XmNlosingFocusCallback ,
                  T3D_anatomy_parent_CB , NULL ) ;    /* Tab key */

   XtInsertEventHandler( wset.anatomy_parent_textfield ,
                         LeaveWindowMask ,             /* pointer leaves */
                         FALSE ,
                         T3D_pointer_leave_EV , NULL ,
                         XtListTail ) ;                /* last in queue */

   MCW_register_help( wset.anatomy_parent_label ,
    "A dataset may optionally be attached\n"
    "to an 'anatomy parent' -- that is, an\n"
    "anatomical dataset which is aligned\n"
    "with the current dataset; usually, the\n"
    "anatomy parent should have been acquired\n"
    "during the same imaging session as the\n"
    "current dataset, so that this requisite\n"
    "alignment is assured.  If no anatomy parent\n"
    "is given, then at the time one is needed,\n"
    "an appropriate dataset will be chosen from\n"
    "the same session directory.\n"
    "\n"
#ifdef REQUIRE_ANAT_PARENT
    "N.B.: Functional datasets require anatomy parents;\n"
    "      for anatomical datasets, anatomy parents are\n"
    "      optional."
#else
    "N.B.: You only need specify an anatomy parent\n"
    "      here if a specific dataset is required."
#endif
   ) ;

   MCW_register_help( wset.anatomy_parent_textfield ,
    "A dataset may optionally be attached\n"
    "to an 'anatomy parent' -- that is, an\n"
    "anatomical dataset which is aligned\n"
    "with the current dataset; usually, the\n"
    "anatomy parent should have been acquired\n"
    "during the same imaging session as the\n"
    "current dataset, so that this requisite\n"
    "alignment is assured.  If no anatomy parent\n"
    "is given, then at the time one is needed,\n"
    "an appropriate dataset will be chosen from\n"
    "the same session directory.\n"
    "\n"
#ifdef REQUIRE_ANAT_PARENT
    "N.B.: functional datasets require anatomy parents;\n"
    "      for anatomical datasets, anatomy parents are\n"
    "      optional."
#else
    "N.B.: You only need specify an anatomy parent\n"
    "      here if a specific dataset is required."
#endif
   ) ;

#ifndef NO_NAMES
   wset.geometry_dataname_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , wset.topform ,
            LABEL_ARG("Geometry parent\nDataset name   ") ,
            XmNleftAttachment  , XmATTACH_FORM ,
            XmNleftOffset      , T3D_FORM_SPACING ,
            XmNtopAttachment   , XmATTACH_WIDGET ,
            XmNtopWidget       , wset.geometry_parent_textfield ,
            XmNtopOffset       , T3D_FORM_SPACING ,
            XmNsensitive       , False ,
         NULL ) ;

   wset.geometry_dataname_textfield =
      XtVaCreateManagedWidget(
         "dialog" , xmTextFieldWidgetClass , wset.topform ,
            XmNcolumns         , T3D_NAME_WIDTH ,
            XmNeditable        , False ,
            XmNmaxLength       , THD_MAX_NAME-1 ,
            XmNresizeWidth     , False ,

            XmNcursorPositionVisible , False ,
            XmNsensitive       , False ,
/*
            XmNmarginHeight    , 1 ,
            XmNmarginWidth     , 1 ,
*/
            XmNvalue           , " " ,

            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftWidget     , wset.geometry_dataname_label ,
            XmNleftOffset     , T3D_FORM_SPACING ,
            XmNtopAttachment  , XmATTACH_OPPOSITE_WIDGET ,
            XmNtopWidget      , wset.geometry_dataname_label ,
            XmNtopOffset      , 0 ,
         NULL ) ;

   MCW_register_help( wset.geometry_dataname_label ,
    "This item just shows the\n"
    "dataset name of the 'geometry\n"
    "parent' you selected (e.g., to\n"
    "let you decide if you've got\n"
    "the right one)." ) ;

   MCW_register_help( wset.geometry_dataname_textfield ,
    "This item just shows the\n"
    "dataset name of the 'geometry\n"
    "parent' you selected (e.g., to\n"
    "let you decide if you've got\n"
    "the right one)." ) ;

   wset.anatomy_dataname_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , wset.topform ,
            LABEL_ARG("Anatomy parent \nDataset name   ") ,
            XmNleftAttachment  , XmATTACH_WIDGET ,
            XmNleftWidget      , wset.geometry_dataname_textfield ,
            XmNleftOffset      , T3D_FORM_SPACING ,
            XmNtopAttachment   , XmATTACH_WIDGET ,
            XmNtopWidget       , wset.geometry_parent_textfield ,
            XmNtopOffset       , T3D_FORM_SPACING ,
            XmNsensitive       , False ,
         NULL ) ;

   wset.anatomy_dataname_textfield =
      XtVaCreateManagedWidget(
         "dialog" , xmTextFieldWidgetClass , wset.topform ,
            XmNcolumns         , T3D_NAME_WIDTH ,
            XmNeditable        , False ,
            XmNmaxLength       , THD_MAX_NAME-1 ,
            XmNresizeWidth     , False ,

            XmNcursorPositionVisible , False ,
            XmNsensitive       , False ,
/*
            XmNmarginHeight    , 1 ,
            XmNmarginWidth     , 1 ,
*/
            XmNvalue           , " " ,

            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftWidget     , wset.anatomy_dataname_label ,
            XmNleftOffset     , T3D_FORM_SPACING ,
            XmNtopAttachment  , XmATTACH_OPPOSITE_WIDGET ,
            XmNtopWidget      , wset.anatomy_dataname_label ,
            XmNtopOffset      , 0 ,
         NULL ) ;

   MCW_register_help( wset.anatomy_dataname_label ,
    "This item just shows the\n"
    "dataset name of the 'anatomy\n"
    "parent' you selected" ) ;

   MCW_register_help( wset.anatomy_dataname_textfield ,
    "This item just shows the\n"
    "dataset name of the 'anatomy\n"
    "parent' you selected" ) ;

QQQ("name fields setup") ;
   printf(".");fflush(stdout);
#endif  /* NO_NAMES */

   /*----- arrowvals for dataset and function types -----*/

   wset.dataset_type_av =
      new_MCW_arrowval(
             wset.topform ,                     /* parent */
             "Type of data\nin the images    ", /* label */
             MCW_AV_downup ,                    /* arrows */
             FIRST_3DIM_TYPE ,                  /* min */
             LAST_3DIM_TYPE ,                   /* max */
             user_inputs.dataset_type ,         /* init */
             MCW_AV_readtext ,                  /* text type */
             0 ,                                /* decimals */
             T3D_type_av_CB , NULL ,            /* callback */
             T3D_text_display ,                 /* text maker */
             DATASET_typestr                    /* and data */
       ) ;

   wset.dataset_type_av->allow_wrap = 1 ;

   XtVaSetValues( wset.dataset_type_av->wtext ,
                     XmNcolumns   , LONGEST_3DIM_TYPESTR ,
                     XmNmaxLength , LONGEST_3DIM_TYPESTR ,
                  NULL ) ;

   XtVaSetValues( wset.dataset_type_av->wrowcol ,
                     XmNleftAttachment , XmATTACH_FORM ,
                     XmNleftOffset     , T3D_FORM_SPACING ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
#ifndef NO_NAMES
                     XmNtopWidget      , wset.geometry_dataname_label ,
#else
                     XmNtopWidget      , wset.geometry_parent_label ,
#endif
                     XmNtopOffset      , T3D_FORM_SPACING ,
                  NULL ) ;

   MCW_reghelp_children( wset.dataset_type_av->wrowcol ,
                          "Use the arrows to specify\n"
                          "the type of data stored\n"
                          "in the image files.\n"
                          "N.B.: 3D+time datasets\n"
                          " must be anatomical."     ) ;
   MCW_reghint_children( wset.dataset_type_av->wrowcol ,
                         "Type of data you acquired" ) ;

   if( user_inputs.ntt > 0 )
      AV_SENSITIZE( wset.dataset_type_av , False ) ;

   wset.function_type_av =
      new_MCW_arrowval(
             wset.topform ,                      /* parent */
             "Type of function \nin the images", /* label */
             MCW_AV_downup ,                     /* arrows */
             FIRST_FUNC_TYPE ,                   /* min */
             LAST_FUNC_TYPE ,                    /* max */
             user_inputs.function_type ,         /* init */
             MCW_AV_readtext ,                   /* text type */
             0 ,                                 /* decimals */
             T3D_type_av_CB , NULL ,             /* callback */
             T3D_text_display ,                  /* text maker */
             FUNC_typestr                        /* and data */
       ) ;

   wset.function_type_av->allow_wrap = 1 ;

   XtVaSetValues( wset.function_type_av->wtext ,
                     XmNcolumns   , LONGEST_FUNC_TYPESTR ,
                     XmNmaxLength , LONGEST_FUNC_TYPESTR ,
                  NULL ) ;

   XtVaSetValues( wset.function_type_av->wrowcol ,
                     XmNleftAttachment , XmATTACH_WIDGET ,
                     XmNleftOffset     , T3D_FORM_SPACING ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopOffset      , T3D_FORM_SPACING ,
                     XmNleftWidget     , wset.geometry_parent_textfield ,
#ifndef NO_NAMES
                     XmNtopWidget      , wset.geometry_dataname_textfield ,
#else
                     XmNtopWidget      , wset.geometry_parent_textfield ,
#endif
                     XtNmappedWhenManaged , False ,
                  NULL ) ;

   MCW_reghelp_children( wset.function_type_av->wrowcol ,
    "Use the arrows to specify\n"
    "the type of functional data\n"
    "stored in the image files" ) ;
   MCW_reghint_children( wset.function_type_av->wrowcol ,
                         "Type of functional data" ) ;

   wset.anatomy_type_av =
      new_MCW_arrowval(
             wset.topform ,                      /* parent */
             "Type of anatomy  \nin the images", /* label */
             MCW_AV_downup ,                     /* arrows */
             FIRST_ANAT_TYPE ,                   /* min */
             LAST_ANAT_TYPE ,                    /* max */
             user_inputs.anatomy_type ,          /* init */
             MCW_AV_readtext ,                   /* text type */
             0 ,                                 /* decimals */
             T3D_type_av_CB , NULL ,             /* callback */
             T3D_text_display ,                  /* text maker */
             ANAT_typestr                        /* and data */
       ) ;

   wset.anatomy_type_av->allow_wrap = 1 ;

   XtVaSetValues( wset.anatomy_type_av->wtext ,
                     XmNcolumns   , LONGEST_ANAT_TYPESTR ,
                     XmNmaxLength , LONGEST_ANAT_TYPESTR ,
                  NULL ) ;

   XtVaSetValues( wset.anatomy_type_av->wrowcol ,
                     XmNleftAttachment , XmATTACH_WIDGET ,
                     XmNleftWidget     , wset.geometry_parent_textfield ,
#ifndef NO_NAMES
                     XmNtopWidget      , wset.geometry_dataname_textfield ,
#else
                     XmNtopWidget      , wset.geometry_parent_textfield ,
#endif
                     XmNleftOffset     , T3D_FORM_SPACING ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopOffset      , T3D_FORM_SPACING ,
                     XtNmappedWhenManaged , False ,
                  NULL ) ;

   MCW_reghelp_children( wset.anatomy_type_av->wrowcol ,
    "Use the arrows to specify\n"
    "the type of anatomical data\n"
    "stored in the image files" ) ;
   MCW_reghint_children( wset.anatomy_type_av->wrowcol ,
                         "Type of anatomical data" ) ;

   /*----- stat_aux data fields -----*/

   wset.stat_aux_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , wset.topform ,
            XmNleftAttachment , XmATTACH_FORM ,
            XmNleftOffset     , T3D_FORM_SPACING ,
            XmNtopAttachment  , XmATTACH_WIDGET ,
            XmNtopWidget      , wset.dataset_type_av->wrowcol ,
            XmNtopOffset      , T3D_FORM_SPACING ,
         NULL ) ;

   wset.stat_aux_textfield =
      XtVaCreateManagedWidget(
         "dialog" , xmTextFieldWidgetClass , wset.topform ,
            XmNcolumns        , 80 ,
            XmNeditable       , True ,
            XmNmaxLength      , THD_MAX_NAME-1 ,
            XmNresizeWidth    , False ,
            XmNcursorPositionVisible , True ,
            XmNblinkRate , 0 ,
            XmNleftAttachment , XmATTACH_FORM ,
            XmNleftOffset     , T3D_FORM_SPACING ,
            XmNtopAttachment  , XmATTACH_WIDGET ,
            XmNtopWidget      , wset.stat_aux_label ,
            XmNtopOffset      , 1 ,
         NULL ) ;

    MCW_register_help( wset.stat_aux_label ,
      "Some types of functional data require input\n"
      "of extra statistical parameters.  These\n"
      "values should be entered in the text field\n"
      "below, separated by spaces and/or commas.\n"
      "\n"
      " Inten+Cor requires the number of\n"
      "   samples (images) used, the number of\n"
      "   fitting parameters, and the number of\n"
      "   orthogonalization parameters.\n"
      "\n"
      " Inten+Ttest requires the number of\n"
      "   degrees-of-freedom in the t-test."
    ) ;

    MCW_register_help( wset.stat_aux_textfield ,
      "Some types of functional data require input\n"
      "of extra statistical parameters.  These\n"
      "values should be entered in this text field,\n"
      "separated by spaces and/or commas.\n"
      "\n"
      " Inten+Cor requires the number of\n"
      "   samples (images) used, the number of\n"
      "   fitting parameters, and the number of\n"
      "   orthogonalization parameters.\n"
      "\n"
      " Inten+Ttest requires the number of\n"
      "   degrees-of-freedom in the t-test."
    ) ;

    MCW_register_hint( wset.stat_aux_label , "Extra statistical parameters" ) ;
    MCW_register_hint( wset.stat_aux_textfield , "Extra statistical parameters" ) ;

   /** The stuff below is commented out,
       since the textfield is scanned only at the write-out time **/

#if 0
   XtAddCallback( wset.stat_aux_textfield ,
                  XmNactivateCallback ,
                  T3D_stat_aux_CB , NULL ) ;    /* return key */

   XtAddCallback( wset.stat_aux_textfield ,
                  XmNlosingFocusCallback ,
                  T3D_stat_aux_CB , NULL ) ;    /* Tab key */

   XtInsertEventHandler( wset.stat_aux_textfield ,
                         LeaveWindowMask ,             /* pointer leaves */
                         FALSE ,
                         T3D_pointer_leave_EV , NULL ,
                         XtListTail ) ;                /* last in queue */
#endif

   /*--- output locations ---*/

   wset.output_file_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , wset.topform ,
            LABEL_ARG("Prefix for 3D  \nDataset file   ") ,

            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftWidget     , wset.geometry_parent_textfield ,
            XmNleftOffset     , T3D_FORM_SPACING ,
            XmNtopAttachment  , XmATTACH_WIDGET ,
            XmNtopWidget      , wset.stat_aux_textfield ,
            XmNtopOffset      , T3D_FORM_SPACING ,
            XmNbottomAttachment , XmATTACH_FORM ,
            XmNbottomOffset     , T3D_FORM_SPACING ,
         NULL ) ;

   wset.output_file_textfield =
      XtVaCreateManagedWidget(
         "dialog" , xmTextFieldWidgetClass , wset.topform ,
            XmNcolumns         , T3D_NAME_WIDTH ,
            XmNeditable        , True ,
            XmNmaxLength       , THD_MAX_PREFIX-1 ,
            XmNresizeWidth     , False ,
            XmNcursorPositionVisible , True ,
            XmNblinkRate , 0 ,
/*
            XmNmarginHeight    , 1 ,
            XmNmarginWidth     , 1 ,
*/
            XmNvalue           , user_inputs.output_filename ,

            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftOffset     , T3D_FORM_SPACING ,
            XmNleftWidget     , wset.output_file_label ,
            XmNtopAttachment  , XmATTACH_OPPOSITE_WIDGET ,
            XmNtopWidget      , wset.output_file_label ,
            XmNtopOffset      , 0 ,

         NULL ) ;

   MCW_register_help( wset.output_file_label ,
    "Type in the 'prefix' for the filenames\n"
    "that the 3D dataset will be saved in.\n"
    "[The actual filenames will be of the\n"
    " form prefix+orig.suff, where suff =\n"
    " " DATASET_HEADER_SUFFIX " and " DATASET_BRICK_SUFFIX "]" ) ;

   MCW_register_help( wset.output_file_textfield ,
    "Type in the 'prefix' for the filenames\n"
    "that the 3D dataset will be saved in.\n"
    "[The actual filenames will be of the\n"
    " form prefix+orig.suff, where suff =\n"
    " " DATASET_HEADER_SUFFIX " and " DATASET_BRICK_SUFFIX "]" ) ;

   MCW_register_hint( wset.output_file_label ,
                      "New dataset's 'prefix' for filenames" ) ;
   MCW_register_hint( wset.output_file_textfield ,
                      "New dataset's 'prefix' for filenames" ) ;

   wset.session_file_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , wset.topform ,
            LABEL_ARG("Session direct.\nfor 3D Datasets") ,

            XmNleftAttachment , XmATTACH_FORM ,
            XmNleftOffset     , T3D_FORM_SPACING ,
            XmNtopAttachment  , XmATTACH_WIDGET ,
            XmNtopWidget      , wset.stat_aux_textfield ,
            XmNtopOffset      , T3D_FORM_SPACING ,
         NULL ) ;

   wset.session_file_textfield =
      XtVaCreateManagedWidget(
         "dialog" , xmTextFieldWidgetClass , wset.topform ,
            XmNcolumns         , T3D_NAME_WIDTH ,
            XmNeditable        , True ,
            XmNmaxLength       , THD_MAX_NAME-1 ,
            XmNresizeWidth     , False ,
            XmNcursorPositionVisible , True ,
            XmNblinkRate , 0 ,
/*
            XmNmarginHeight    , 1 ,
            XmNmarginWidth     , 1 ,
*/
            XmNvalue           , user_inputs.session_filename ,

            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftOffset     , T3D_FORM_SPACING ,
            XmNleftWidget     , wset.session_file_label ,
            XmNtopAttachment  , XmATTACH_OPPOSITE_WIDGET ,
            XmNtopWidget      , wset.session_file_label ,
            XmNtopOffset      , 0 ,

         NULL ) ;

   MCW_register_help( wset.session_file_label ,
    "Type in the name of the session\n"
    "directory in which to save this\n"
    "3D dataset, and in which to look\n"
    "for the 'parent' datasets" ) ;

   MCW_register_help( wset.session_file_textfield ,
    "Type in the name of the session\n"
    "directory in which to save this\n"
    "3D dataset, and in which to look\n"
    "for the 'parent' datasets" ) ;

   MCW_register_hint( wset.session_file_label , "New dataset's directory" ) ;
   MCW_register_hint( wset.session_file_textfield , "New dataset's directory" ) ;

QQQ("types and files setup") ;
   printf(".");fflush(stdout);

   /*----- action controls at lower right corner -----*/

   wset.action_frame =
      XtVaCreateManagedWidget(
         "dialog" , xmFrameWidgetClass , wset.topform ,
            XmNshadowType       , XmSHADOW_ETCHED_IN ,
            XmNleftAttachment   , XmATTACH_WIDGET ,
            XmNleftWidget       , wset.anatomy_parent_textfield ,
            XmNleftOffset       , T3D_FORM_SPACING ,
            XmNbottomAttachment , XmATTACH_FORM ,
            XmNbottomOffset     , T3D_FORM_SPACING ,
         NULL ) ;

   wset.action_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , wset.action_frame ,
            XmNorientation , XmVERTICAL ,
            XmNpacking     , XmPACK_TIGHT ,
         NULL ) ;

   /* 14 Sep 1998: add a button to swap bytes in the images */

   { int    dd = mri_datum_size((MRI_TYPE)argopt.datum_all) ;
     char * tt = "Byte Swap[2]" ;
     char * ff = "Byte Swap[4]" ;
     if( dd == 2 || dd == 4 ){
        if( dd == 2 )
           wset.swap_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , wset.action_rowcol ,
                  LABEL_ARG(tt) , NULL ) ;
         else
           wset.swap_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , wset.action_rowcol ,
                  LABEL_ARG(ff) , NULL ) ;

         XtAddCallback( wset.swap_pb , XmNactivateCallback ,
                        T3D_swap_CB , NULL ) ;
         MCW_register_hint( wset.swap_pb , "Use if images need this" ) ;
     } else {
        wset.swap_pb = NULL ;
     }
   }

   wset.button_help_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , wset.action_rowcol ,
            LABEL_ARG("button help") ,
         NULL ) ;
   XtAddCallback( wset.button_help_pb , XmNactivateCallback ,
                  MCW_click_help_CB , NULL ) ;
   MCW_register_help( wset.button_help_pb ,
     "Click the hand\non any button to get\na little help" ) ;
   MCW_register_hint( wset.button_help_pb , "Get more help" ) ;

   wset.open_view_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , wset.action_rowcol ,
            LABEL_ARG("View Images") ,
         NULL ) ;
   XtAddCallback( wset.open_view_pb , XmNactivateCallback ,
                  T3D_open_view_CB , NULL ) ;
   MCW_register_help( wset.open_view_pb ,
    "Use this to open a window to see the images;\n"
    "N.B.: If you use the viewing controls to change the\n"
    "      orientation of the images, then you must be\n"
    "      aware that 'x' and 'y' in the geometry controls\n"
    "      above refer to the images in their ORIGINAL\n"
    "      orientation on the screen, NOT in the rotated\n"
    "      and/or mirrored orientation you will be viewing." ) ;
   MCW_register_hint( wset.open_view_pb , "See the input images" ) ;

   wset.save_file_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , wset.action_rowcol ,
            LABEL_ARG("Save Dataset") ,
         NULL ) ;
   XtAddCallback( wset.save_file_pb , XmNactivateCallback ,
                  T3D_save_file_CB , NULL ) ;
   MCW_register_help( wset.save_file_pb ,
    "Use this to save the volume data\n"
    "into 3D dataset files when you\n"
    "have set all the control data." ) ;
   MCW_register_hint( wset.save_file_pb , "Write new dataset to disk" ) ;

   wset.quit_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , wset.action_rowcol ,
            XmNrecomputeSize , False ,
            LABEL_ARG("quit") ,
         NULL ) ;
   XtAddCallback( wset.quit_pb , XmNactivateCallback ,
                  T3D_quit_CB , NULL ) ;
   MCW_register_help( wset.quit_pb ,
    "Press this button TWICE\nto exit the program" ) ;
   MCW_register_hint( wset.quit_pb , "Press twice to exit program" ) ;

   MCW_set_widget_bg( wset.quit_pb , MCW_hotcolor(wset.quit_pb) , 0 ) ;

   XtManageChild( wset.action_rowcol ) ;

QQQ("controls setup") ;
   printf(".");fflush(stdout);

   /*----- all done -----*/

   XtManageChild( wset.topform ) ;
QQQ("XtManageChild( wset.topform ) done") ;

   XtRealizeWidget( wset.topshell ) ;
QQQ("XtRealizeWidget( wset.topshell ) done") ;

   T3D_data_to_widgets() ;
   T3D_set_dependent_geometries() ;
   return ;
}

/*---------------------------------------------------------------------
   Read any command line arguments and interpret them into
   the user_inputs data structure.
-----------------------------------------------------------------------*/

static to3d_data default_user_inputs = {
   ORI_R2L_TYPE , ORI_R2L_TYPE , ORI_R2L_TYPE ,      /* orientations */
   VOXSHAPE_IRREGULAR ,                              /* not really used */
   VOXCONTIG_YES ,                                   /* really used */
   (XCENTERED | YCENTERED | ZCENTERED) ,             /* axes centered ? */
   VIEW_ORIGINAL_TYPE ,                              /* original view */
   HEAD_ANAT_TYPE , ANAT_SPGR_TYPE , FUNC_FIM_TYPE , /* data types */
   0,0,0 ,                                           /* images sizes */

   240.0 , 0.9375 , 0.9375 , 0.9375 , 0.9375 ,  /* FOV; x,y,z sizes; z spacing */

   120.0 , 120.0 , 120.0   /* x,y,z origins */
} ;

#define EMPTY_STRING(str) ((str)[0] = '\0')

/*.....................................................................*/

#define WarningError(str) \
  { fprintf(stderr,"\n***Warning: %s\n",(str)) ; nopt++ ; continue ; }

void T3D_initialize_user_data(void)
{
   int nopt , ii ;

   user_inputs = default_user_inputs ;  /* copy defaults */
   user_inputs.nosave = 0 ;

   /*-- initialize strings --*/

   EMPTY_STRING( user_inputs.dataset_name ) ;
   EMPTY_STRING( user_inputs.short_label1 ) ;
   EMPTY_STRING( user_inputs.short_label2 ) ;
   EMPTY_STRING( user_inputs.geometry_parent_filename ) ;
   EMPTY_STRING( user_inputs.anatomy_parent_filename ) ;
   EMPTY_STRING( user_inputs.anatomy_dataname ) ;
   EMPTY_STRING( user_inputs.geometry_dataname ) ;
   EMPTY_STRING( user_inputs.output_filename ) ;
#ifndef OMIT_DATASET_IDCODES
   ZERO_IDCODE( user_inputs.anatomy_parent_idcode ) ;
#endif

   strcpy( user_inputs.session_filename , "./" ) ;

   strcpy( user_inputs.dataset_type_string ,
           DATASET_typestr[user_inputs.dataset_type] ) ;

   strcpy( user_inputs.function_type_string ,
           FUNC_typestr[user_inputs.function_type] ) ;

   user_inputs.view_type = VIEW_ORIGINAL_TYPE ;

   for( ii=0 ; ii < MAX_STAT_AUX ; ii++ )
      user_inputs.stat_aux[ii] = 0.0 ;

   user_inputs.xincode = user_inputs.yincode = user_inputs.zincode = INCODE_NONE ;

   user_inputs.ntt      = 0 ;
   user_inputs.TR       = 1.0 ;
   user_inputs.nzz      = 0 ;
   user_inputs.t_then_z = 0 ;
   user_inputs.tpattern = NULL ;
   user_inputs.tunits   = UNITS_MSEC_TYPE ;

   /*-- scan options --*/

   nopt = 1 ;
   while( nopt < Argc && Argv[nopt][0] == '-' ){

      if( strncmp(Argv[nopt],"-help",4) == 0 ){
         Syntax() ;
      }

      /* -type from the anatomy prefixes */

      for( ii=FIRST_ANAT_TYPE ; ii <= LAST_ANAT_TYPE ; ii++ )
         if( strncmp( &(Argv[nopt][1]) ,
                      ANAT_prefixstr[ii] , THD_MAX_PREFIX ) == 0 ) break ;

      if( ii <= LAST_ANAT_TYPE ){
         user_inputs.anatomy_type = ii ;
         user_inputs.dataset_type = HEAD_ANAT_TYPE ;
         strcpy( user_inputs.dataset_type_string ,
                 DATASET_typestr[user_inputs.dataset_type] ) ;
         strcpy( user_inputs.anatomy_type_string ,
                 ANAT_typestr[user_inputs.anatomy_type] ) ;

         nopt++ ; continue ;
      }

      /* -type from the function prefixes */

      for( ii=FIRST_FUNC_TYPE ; ii <= LAST_FUNC_TYPE ; ii++ )
         if( strncmp( &(Argv[nopt][1]) ,
                      FUNC_prefixstr[ii] , THD_MAX_PREFIX ) == 0 ) break ;

      if( ii <= LAST_FUNC_TYPE ){
         user_inputs.function_type = ii ;
         user_inputs.dataset_type = HEAD_FUNC_TYPE ;
         strcpy( user_inputs.dataset_type_string ,
                 DATASET_typestr[user_inputs.dataset_type] ) ;
         strcpy( user_inputs.function_type_string ,
                 FUNC_typestr[user_inputs.function_type] ) ;

         nopt++ ; continue ;
      }

#ifdef USE_MRI_DELAY
      /*--- -in:1 ---*/

      if( strncmp(Argv[nopt],"-in:1",5) == 0 ){
         argopt.delay_input = TRUE ;
         nopt++ ; continue ;
      }
#endif

      /*--- -view type ---*/

      if( strncmp(Argv[nopt],"-view",4) == 0 ){
         char * str ;

         if( ++nopt >= Argc ) FatalError("-view needs a type") ;

         str = Argv[nopt] ; if( str[0] == '+' ) str++ ;

         for( ii=FIRST_VIEW_TYPE ; ii <= LAST_VIEW_TYPE ; ii++ )
            if( strcmp(str,VIEW_codestr[ii]) == 0 ) break ;

         if( ii <= LAST_VIEW_TYPE ){
            user_inputs.view_type = ii ;
         } else {
            WarningError("Unknown view type after -view!") ;
         }
         nopt++ ; continue ;
      }

      /*--- -session name ---*/

      if( strncmp(Argv[nopt],"-session",4) == 0 ){
         if( ++nopt >= Argc ) FatalError("-session needs a name") ;
         MCW_strncpy( user_inputs.session_filename, Argv[nopt], THD_MAX_NAME ) ;
         nopt++ ; continue ;
      }

      /*--- -prefix name ---*/

      if( strncmp(Argv[nopt],"-prefix",4) == 0 ){
         if( ++nopt >= Argc ) FatalError("-prefix needs a name") ;
         MCW_strncpy( user_inputs.output_filename , Argv[nopt] , THD_MAX_NAME ) ;
         nopt++ ; continue ;
      }

#ifndef NO_NAMES
      /*--- -dname name ---*/

      if( strncmp(Argv[nopt],"-dname",4) == 0 ){
         if( ++nopt >= Argc ) FatalError("-dname needs a name") ;
         MCW_strncpy( user_inputs.dataset_name , Argv[nopt] , THD_MAX_NAME ) ;
         nopt++ ; continue ;
      }

      /*--- -dlabel name ---*/

      if( strncmp(Argv[nopt],"-dlabel",4) == 0 ){
         if( ++nopt >= Argc ) FatalError("-dlabel needs a name") ;
         MCW_strncpy( user_inputs.short_label1 , Argv[nopt] , THD_MAX_LABEL ) ;
         nopt++ ; continue ;
      }
#endif

      /*--- -geomparent headerfile ---*/

      if( strncmp(Argv[nopt],"-geomparent",6) == 0 ){
         if( ++nopt >= Argc ) FatalError("-geomparent needs a headerfile" ) ;
         MCW_strncpy( user_inputs.geometry_parent_filename ,
                      Argv[nopt] , THD_MAX_NAME ) ;
         nopt++ ; continue ;
      }

      /*--- -anatparent headerfile ---*/

      if( strncmp(Argv[nopt],"-anatparent",6) == 0 ){
         if( ++nopt >= Argc ) FatalError("-anatparent needs a headerfile" ) ;
         MCW_strncpy( user_inputs.anatomy_parent_filename ,
                      Argv[nopt] , THD_MAX_NAME ) ;
         nopt++ ; continue ;
      }

      /*--- -nosave ---*/

      if( strncmp(Argv[nopt],"-nosave",4) == 0 ){
         user_inputs.nosave = 1 ;
         nopt++ ; continue ;
      }

      /*--- July 1997: -orient code ---*/

      if( strncmp(Argv[nopt],"-orient",4) == 0 ){
         char acod ;

         if( ++nopt >= Argc ) FatalError("-orient needs a code") ;
         if( strlen(Argv[nopt]) != 3 ) FatalError("Illegal -orient code") ;
         acod = toupper(Argv[nopt][0]) ; user_inputs.xorient = ORCODE(acod) ;
         acod = toupper(Argv[nopt][1]) ; user_inputs.yorient = ORCODE(acod) ;
         acod = toupper(Argv[nopt][2]) ; user_inputs.zorient = ORCODE(acod) ;
         nopt++ ; continue ;
      }

      /********** April 1996: new options for setting dimensions **********/

/** 21 Nov 1997: alter the way dimensions are decoded **/
#undef USE_OLD_DCODE

      /*--- -xFOV or -xSLAB ---*/

      if( strncmp(Argv[nopt],"-xFOV",5)==0 || strncmp(Argv[nopt],"-xSLAB",5)==0 ||
          strncmp(Argv[nopt],"-xfov",5)==0 || strncmp(Argv[nopt],"-xslab",5)==0   ){
         float val , xin_bot , xin_top ;
         char * ptr , * ptr2 , acod ;
         int dcod1 , dcod2 , xincode ;

         xincode = ( strncmp(Argv[nopt],"-xFOV",5)==0 ||
                     strncmp(Argv[nopt],"-xfov",5)==0   ) ? INCODE_FOV : INCODE_SLAB ;

         if( ++nopt >= Argc ) WarningError("need an argument after -xFOV/-xSLAB!") ;

         /* should now have something of one of the forms
               <number> <dircode> : <number> <dircode>
               <number> <dircode> - <number> <dircode>
               <number> <dircode> : <dircode>
               <number> <dircode> - <dircode>

            Step 1: get the first <number>   */

#ifdef USE_OLD_DCODE
         val = strtod( Argv[nopt] , &ptr ) ;
         if( val < 0.0 || (val == 0.0 && ptr == Argv[nopt]) )
            WarningError("a nonegative number should follow -xFOV/-xSLAB!") ;

         xin_bot = val ;

         /* Step 2: get the first <dircode> */

         acod = toupper( *ptr ) ; dcod1 = ORCODE(acod) ; ptr++ ;
         if( dcod1 < 0 )
            WarningError("orientation code should follow first dimension in -xFOV/-xSLAB!") ;
#else
         { int nused ;
           nused = decode_location( Argv[nopt] , &xin_bot , &dcod1 ) ;
           if( xin_bot < 0.0 || xin_bot == WAY_BIG )
             WarningError("a nonegative number should follow -xFOV/-xSLAB!") ;
           if( dcod1 < 0 )
             WarningError("1st orientation code illegal after -xFOV/-xSLAB!") ;
           ptr = Argv[nopt] + nused ;
         }
#endif


         /* Step 3: get the separator */

         if( *ptr != ':' && *ptr != '-' )
#ifdef USE_OLD_DCODE
            WarningError(": or - should follow orientation code in -xFOV/-xSLAB!") ;
#else
            WarningError(": or - should follow 1st dimension in -xFOV/-xSLAB!") ;
#endif
         ptr++ ;
         if( *ptr == '\0' )
            WarningError("orientation code or dimension should follow : or - in -xFOV/-xSLAB!") ;

         /* Step 4: try to get the second <dircode>.  If that fails, then get
                    the second <number> first, then the second <dircode>.
                    If the second <dircode> works, however, then the second
                    <number> is defined to be equal to the first <number>.   */

#ifdef USE_OLD_DCODE
         acod = toupper( *ptr ) ; dcod2 = ORCODE(acod) ;
         if( dcod2 < 0 ){
            val = strtod( ptr , &ptr2 ) ;
            if( val < 0.0 || (val == 0.0 && ptr2 == ptr) )
              WarningError("orientation code or dimension should follow : or - in -xFOV/-xSLAB!") ;
            xin_top = val ;
            ptr = ptr2 ;
            acod = toupper( *ptr ) ; dcod2 = ORCODE(acod) ;
            if( dcod2 < 0 )
               WarningError("orientation code should follow second dimension in -xFOV/-xSLAB!") ;
         } else {
            xin_top = xin_bot ;
         }
#else
         { int nused ;
           nused = decode_location( ptr , &xin_top , &dcod2 ) ;
           if( dcod2 < 0 )
             WarningError("need 2nd orientation code in -xFOV/-xSLAB!") ;
           if( xin_top < 0.0 )
             WarningError("need nonegative 2nd dimension in -xFOV/-xSLAB!") ;
           if( xin_top == WAY_BIG ) xin_top = xin_bot ;
         }
#endif

         /* Now, check for consistency:
              Direction codes should be in the same or opposite directions;
              however, must be opposite if the two <number>s are identical.
              For example 120A-120A makes no sense,
              but 120A-120P is reasonable, and 10A-30A is also reasonable.  */

         if( xin_top == xin_bot && dcod1 != ORIENT_OPPOSITE(dcod2) )
            WarningError("zero thickness slab specified in -xFOV/-xSLAB!") ;

         if( dcod1 != dcod2 && dcod1 != ORIENT_OPPOSITE(dcod2) )
            WarningError("inconsistent directions given in -xFOV/-xSLAB!") ;

         /* If the direction codes are the same, then if the first <number>
            is smaller, reverse the first direction code and negate the
            first number.  For example, 10A:30A becomes -10P:30A, which is
            equivalent.  If the first number is larger, then reverse the
            second code and number.  For example, 30A:10A becomes 30A:-10P.
            Why do this?  Then the first direction code correctly expresses
            the sense of the input direction. The sum of numbers becomes
            the cross-slab thickness, needed to compute various stuff later. */

         if( dcod1 == dcod2 ){
            if( xin_bot < xin_top ){
               dcod1   = ORIENT_OPPOSITE(dcod1) ;
               xin_bot = -xin_bot ;
            } else {
               dcod2   = ORIENT_OPPOSITE(dcod2) ;
               xin_top = -xin_top ;
            }
         }

         /* All is OK, so store the results */

         user_inputs.xincode = xincode + dcod1 ;
         user_inputs.xin_bot = xin_bot ;
         user_inputs.xin_top = xin_top ;

printf("decoded %s to give xincode=%d bot=%f top=%f\n",Argv[nopt],
       user_inputs.xincode, user_inputs.xin_bot, user_inputs.xin_top ) ;

         nopt++ ; continue ;
      }

      /*--- -yFOV or -ySLAB ---*/

      if( strncmp(Argv[nopt],"-yFOV",5)==0 || strncmp(Argv[nopt],"-ySLAB",5)==0 ||
          strncmp(Argv[nopt],"-yfov",5)==0 || strncmp(Argv[nopt],"-yslab",5)==0   ){

         float val , yin_bot , yin_top ;
         char * ptr , * ptr2 , acod ;
         int dcod1 , dcod2 , yincode ;

         yincode = ( strncmp(Argv[nopt],"-yFOV",5)==0 ||
                     strncmp(Argv[nopt],"-yfov",5)==0   ) ? INCODE_FOV : INCODE_SLAB ;

         if( ++nopt >= Argc ) WarningError("need an argument after -yFOV/-ySLAB!") ;

         /* should now have something of one of the forms
               <number> <dircode> : <number> <dircode>
               <number> <dircode> - <number> <dircode>
               <number> <dircode> : <dircode>
               <number> <dircode> - <dircode>

            Step 1: get the first <number>   */

#ifdef USE_OLD_DCODE
         val = strtod( Argv[nopt] , &ptr ) ;
         if( val < 0.0 || (val == 0.0 && ptr == Argv[nopt]) )
            WarningError("a nonegative number should follow -yFOV/-ySLAB!") ;

         yin_bot = val ;

         /* Step 2: get the first <dircode> */

         acod = toupper( *ptr ) ; dcod1 = ORCODE(acod) ; ptr++ ;
         if( dcod1 < 0 )
            WarningError("orientation code should follow first dimension in -yFOV/-ySLAB!") ;
#else
         { int nused ;
           nused = decode_location( Argv[nopt] , &yin_bot , &dcod1 ) ;
           if( yin_bot < 0.0 || yin_bot == WAY_BIG )
             WarningError("a nonegative number should follow -yFOV/-ySLAB!") ;
           if( dcod1 < 0 )
             WarningError("1st orientation code illegal after -yFOV/-ySLAB!") ;
           ptr = Argv[nopt] + nused ;
         }
#endif

         /* Step 3: get the separator */

         if( *ptr != ':' && *ptr != '-' )
#ifdef USE_OLD_DCODE
            WarningError(": or - should follow orientation code in -yFOV/-ySLAB!") ;
#else
            WarningError(": or - should follow 1st dimension in -yFOV/-ySLAB!") ;
#endif

         ptr++ ;
         if( *ptr == '\0' )
            WarningError("orientation code or dimension should follow : or - in -yFOV/-ySLAB!") ;

         /* Step 4: try to get the second <dircode>.  If that fails, then get
                    the second <number> first, then the second <dircode>.
                    If the second <dircode> works, however, then the second
                    <number> is defined to be equal to the first <number>.   */

#ifdef USE_OLD_DCODE
         acod = toupper( *ptr ) ; dcod2 = ORCODE(acod) ;
         if( dcod2 < 0 ){
            val = strtod( ptr , &ptr2 ) ;
            if( val < 0.0 || (val == 0.0 && ptr2 == ptr) )
              WarningError("orientation code or dimension should follow : or - in -yFOV/-ySLAB!") ;
            yin_top = val ;
            ptr = ptr2 ;
            acod = toupper( *ptr ) ; dcod2 = ORCODE(acod) ;
            if( dcod2 < 0 )
               WarningError("orientation code should follow second dimension in -yFOV/-ySLAB!") ;
         } else {
            yin_top = yin_bot ;
         }
#else
         { int nused ;
           nused = decode_location( ptr , &yin_top , &dcod2 ) ;
           if( dcod2 < 0 )
             WarningError("need 2nd orientation code in -yFOV/-ySLAB!") ;
           if( yin_top < 0.0 )
             WarningError("need nonegative 2nd dimension in -yFOV/-ySLAB!") ;
           if( yin_top == WAY_BIG ) yin_top = yin_bot ;
         }
#endif

         /* Now, check for consistency:
              Direction codes should be in the same or opposite directions;
              however, must be opposite if the two <number>s are identical.
              For example 120A-120A makes no sense,
              but 120A-120P is reasonable, and 10A-30A is also reasonable.  */

         if( yin_top == yin_bot && dcod1 != ORIENT_OPPOSITE(dcod2) )
            WarningError("zero thickness slab specified in -yFOV/-ySLAB!") ;

         if( dcod1 != dcod2 && dcod1 != ORIENT_OPPOSITE(dcod2) )
            WarningError("inconsistent directions given in -yFOV/-ySLAB!") ;

         /* If the direction codes are the same, then if the first <number>
            is smaller, reverse the first direction code and negate the
            first number.  For example, 10A:30A becomes -10P:30A, which is
            equivalent.  If the first number is larger, then reverse the
            second code and number.  For example, 30A:10A becomes 30A:-10P.
            Why do this?  Then the first direction code correctly expresses
            the sense of the input direction. The sum of numbers becomes
            the cross-slab thickness, needed to compute various stuff later. */

         if( dcod1 == dcod2 ){
            if( yin_bot < yin_top ){
               dcod1   = ORIENT_OPPOSITE(dcod1) ;
               yin_bot = -yin_bot ;
            } else {
               dcod2   = ORIENT_OPPOSITE(dcod2) ;
               yin_top = -yin_top ;
            }
         }

         /* All is OK, so store the results */

         user_inputs.yincode = yincode + dcod1 ;
         user_inputs.yin_bot = yin_bot ;
         user_inputs.yin_top = yin_top ;

printf("decoded %s to give yincode=%d bot=%f top=%f\n",Argv[nopt],
       user_inputs.yincode, user_inputs.yin_bot, user_inputs.yin_top ) ;

         nopt++ ; continue ;
      }

      /*--- -zFOV or -zSLAB ---*/

      if( strncmp(Argv[nopt],"-zFOV",5)==0 || strncmp(Argv[nopt],"-zSLAB",5)==0 ||
          strncmp(Argv[nopt],"-zfov",5)==0 || strncmp(Argv[nopt],"-zslab",5)==0   ){

         float val , zin_bot , zin_top ;
         char * ptr , * ptr2 , acod ;
         int dcod1 , dcod2 , zincode ;

         zincode = ( strncmp(Argv[nopt],"-zFOV",5)==0 ||
                     strncmp(Argv[nopt],"-zfov",5)==0   ) ? INCODE_FOV : INCODE_SLAB ;

         if( ++nopt >= Argc ) WarningError("need an argument after -zFOV/-zSLAB!") ;

         /* should now have something of one of the forms
               <number> <dircode> : <number> <dircode>
               <number> <dircode> - <number> <dircode>
               <number> <dircode> : <dircode>
               <number> <dircode> - <dircode>

            Step 1: get the first <number>   */

#ifdef USE_OLD_DCODE
         val = strtod( Argv[nopt] , &ptr ) ;
         if( val < 0.0 || (val == 0.0 && ptr == Argv[nopt]) )
            WarningError("a nonegative number should follow -zFOV/-zSLAB!") ;

         zin_bot = val ;

         /* Step 2: get the first <dircode> */

         acod = toupper( *ptr ) ; dcod1 = ORCODE(acod) ; ptr++ ;
         if( dcod1 < 0 )
            WarningError("orientation code should follow first dimension in -zFOV/-zSLAB!") ;
#else
         { int nused ;
           nused = decode_location( Argv[nopt] , &zin_bot , &dcod1 ) ;
           if( zin_bot < 0.0 || zin_bot == WAY_BIG )
             WarningError("a nonegative number should follow -yFOV/-ySLAB!") ;
           if( dcod1 < 0 )
             WarningError("1st orientation code illegal after -yFOV/-ySLAB!") ;
           ptr = Argv[nopt] + nused ;
         }
#endif

         /* Step 3: get the separator */

         if( *ptr != ':' && *ptr != '-' )
#ifdef USE_OLD_DCODE
            WarningError(": or - should follow orientation code in -zFOV/-zSLAB!") ;
#else
            WarningError(": or - should follow 1st dimension in -zFOV/-zSLAB!") ;
#endif

         ptr++ ;
         if( *ptr == '\0' )
            WarningError("orientation code or dimension should follow : or - in -zFOV/-zSLAB!") ;

         /* Step 4: try to get the second <dircode>.  If that fails, then get
                    the second <number> first, then the second <dircode>.
                    If the second <dircode> works, however, then the second
                    <number> is defined to be equal to the first <number>.   */

#ifdef USE_OLD_DCODE
         acod = toupper( *ptr ) ; dcod2 = ORCODE(acod) ;
         if( dcod2 < 0 ){
            val = strtod( ptr , &ptr2 ) ;
            if( val < 0.0 || (val == 0.0 && ptr2 == ptr) )
              WarningError("orientation code or dimension should follow : or - in -zFOV/-zSLAB!") ;
            zin_top = val ;
            ptr = ptr2 ;
            acod = toupper( *ptr ) ; dcod2 = ORCODE(acod) ;
            if( dcod2 < 0 )
               WarningError("orientation code should follow second dimension in -zFOV/-zSLAB!") ;
         } else {
            zin_top = zin_bot ;
         }
#else
         { int nused ;
           nused = decode_location( ptr , &zin_top , &dcod2 ) ;
           if( dcod2 < 0 )
             WarningError("need 2nd orientation code in -zFOV/-zSLAB!") ;
           if( zin_top < 0.0 )
             WarningError("need nonegative 2nd dimension in -zFOV/-zSLAB!") ;
           if( zin_top == WAY_BIG ) zin_top = zin_bot ;
         }
#endif

         /* Now, check for consistency:
              Direction codes should be in the same or opposite directions;
              however, must be opposite if the two <number>s are identical.
              For example 120A-120A makes no sense,
              but 120A-120P is reasonable, and 10A-30A is also reasonable.  */

         if( zin_top == zin_bot && dcod1 != ORIENT_OPPOSITE(dcod2) )
            WarningError("zero thickness slab specified in -zFOV/-zSLAB!") ;

         if( dcod1 != dcod2 && dcod1 != ORIENT_OPPOSITE(dcod2) )
            WarningError("inconsistent directions given in -zFOV/-zSLAB!") ;

         /* If the direction codes are the same, then if the first <number>
            is smaller, reverse the first direction code and negate the
            first number.  For example, 10A:30A becomes -10P:30A, which is
            equivalent.  If the first number is larger, then reverse the
            second code and number.  For example, 30A:10A becomes 30A:-10P.
            Why do this?  Then the first direction code correctly expresses
            the sense of the input direction. The sum of numbers becomes
            the cross-slab thickness, needed to compute various stuff later. */

         if( dcod1 == dcod2 ){
            if( zin_bot < zin_top ){
               dcod1   = ORIENT_OPPOSITE(dcod1) ;
               zin_bot = -zin_bot ;
            } else {
               dcod2   = ORIENT_OPPOSITE(dcod2) ;
               zin_top = -zin_top ;
            }
         }

         /* All is OK, so store the results */

         user_inputs.zincode = zincode + dcod1 ;
         user_inputs.zin_bot = zin_bot ;
         user_inputs.zin_top = zin_top ;

printf("decoded %s to give zincode=%d bot=%f top=%f\n",Argv[nopt],
       user_inputs.zincode, user_inputs.zin_bot, user_inputs.zin_top ) ;

         nopt++ ; continue ;
      }

      /*---- -statpar x x x ----*/

      if( strncmp(Argv[nopt],"-statpar",5) == 0 ){
         float val ;
         char * ptr ;

         if( ++nopt >= Argc ) FatalError("need an argument after -statpar!") ;

         ii = 0 ;
         do{
            val = strtod( Argv[nopt] , &ptr ) ;
            if( *ptr != '\0' ) break ;
            user_inputs.stat_aux[ii++] = val ;
            nopt++ ;
         } while( nopt < Argc ) ;

         if( ii == 0 )
            WarningError("No numbers given after -statpar?") ;
         continue ;
      }

      /***** 21 Oct 1996: options for time units *****/

      if( strcmp(Argv[nopt],"-t=ms")==0 || strcmp(Argv[nopt],"-t=msec")==0 ){
         user_inputs.tunits = UNITS_MSEC_TYPE ;
         nopt++ ; continue ;
      }

      if( strcmp(Argv[nopt],"-t=s")==0 || strcmp(Argv[nopt],"-t=sec")==0 ){
         user_inputs.tunits = UNITS_SEC_TYPE ;
         nopt++ ; continue ;
      }

      if( strcmp(Argv[nopt],"-t=Hz")==0 || strcmp(Argv[nopt],"-t=Hertz")==0 ){
         user_inputs.tunits = UNITS_HZ_TYPE ;
         nopt++ ; continue ;
      }

      /************* Aprille 1996: New options for specifying time ***********/

      if( strncmp(Argv[nopt],"-time:zt",8)==0 || strncmp(Argv[nopt],"-time:tz",8)==0 ){
         int   t_then_z , ntt , nzz , nerr ;
         float TR , tframe , tsl ;
         char * tpattern , * eptr ;

         if( nopt+4 >= Argc ) FatalError("need 4 arguments after -time: options") ;

         t_then_z = ( strncmp(Argv[nopt],"-time:tz",8)==0 ) ;

         ntt = strtol( Argv[++nopt] , NULL , 10 ) ;
         nzz = strtol( Argv[++nopt] , NULL , 10 ) ;
         if( ! t_then_z ){ ii = ntt ; ntt = nzz ; nzz = ii ; }

         TR       = strtod( Argv[++nopt] , &eptr ) ;
         tpattern = Argv[++nopt] ;

         /** 03 Nov 1996: allow units to be written after TR **/

         if( strcmp(eptr,"ms")==0 || strcmp(eptr,"msec")==0 ){
            user_inputs.tunits = UNITS_MSEC_TYPE ;
         } else if( strcmp(eptr,"s")==0 || strcmp(eptr,"sec")==0 ){
            user_inputs.tunits = UNITS_SEC_TYPE ;
         } else if( strcmp(eptr,"Hz")==0 || strcmp(eptr,"Hertz")==0 ){
            user_inputs.tunits = UNITS_HZ_TYPE ;
         }

         /** 31 July 1996: be more specific about errors **/

         nerr = 0 ;
         if( ntt < 2 ){
            fprintf(stderr,"Illegal value of nt after -time: option\n") ; nerr++ ;
         }
         if( nzz < 2 ){
            fprintf(stderr,"Illegal value of nz after -time: option\n") ; nerr++ ;
         }
         if( TR <= 0.0 ){
            fprintf(stderr,"Illegal value of TR after -time: option\n") ; nerr++ ;
         }
         if( nerr > 0 ){
            nopt++ ; continue ;  /* skip on to next option, this one is bad! */
         }

         user_inputs.tpattern = (float *) malloc( sizeof(float) * nzz ) ;
         for( ii=0 ; ii < nzz ; ii++ ) user_inputs.tpattern[ii] = 0.0 ;

         tframe = TR / nzz ;  /* time per slice */

         user_inputs.ntt      = ntt ;
         user_inputs.nzz      = nzz ;
         user_inputs.t_then_z = t_then_z ;
         user_inputs.TR       = TR ;

         if( nzz > 1 && tpattern[0] == '@' ){
            FILE * fp ;

            /*--- read pattern file (ignore EOFs!) ---*/

            fp = fopen( tpattern+1 , "r" ) ;
            if( fp == NULL ){
               fprintf(stderr,"Cannot open tpattern file %s\n",tpattern+1) ;
            } else {
               for( ii=0 ; ii < nzz ; ii++ )
                  fscanf( fp , "%f" , user_inputs.tpattern + ii ) ;
               fclose( fp ) ;
            }
         } else if( nzz > 1 &&
                   (strcmp(tpattern,"alt+z")==0 || strcmp(tpattern,"altplus")==0) ){

            /*--- set up alternating in the +z direction ---*/

            tsl = 0.0 ;
            for( ii=0 ; ii < nzz ; ii+=2 ){
               user_inputs.tpattern[ii] = tsl ; tsl += tframe ;
            }
            for( ii=1 ; ii < nzz ; ii+=2 ){
               user_inputs.tpattern[ii] = tsl ; tsl += tframe ;
            }
         } else if( nzz > 1 &&
                   (strcmp(tpattern,"alt-z")==0 || strcmp(tpattern,"altminus")==0) ){

            /*--- set up alternating in the -z direction ---*/

            tsl = 0.0 ;
            for( ii=nzz-1 ; ii >=0 ; ii-=2 ){
               user_inputs.tpattern[ii] = tsl ; tsl += tframe ;
            }
            for( ii=nzz-2 ; ii >=0 ; ii-=2 ){
               user_inputs.tpattern[ii] = tsl ; tsl += tframe ;
            }
         } else if( nzz > 1 &&
                   (strcmp(tpattern,"seq+z")==0 || strcmp(tpattern,"seqplus")==0) ){

            /*--- set up sequential in the +z direction ---*/

            tsl = 0.0 ;
            for( ii=0 ; ii < nzz ; ii++ ){
               user_inputs.tpattern[ii] = tsl ; tsl += tframe ;
            }
         } else if( nzz > 1 &&
                   (strcmp(tpattern,"seq-z")==0 || strcmp(tpattern,"seqminus")==0) ){

            /*--- set up sequential in the -z direction ---*/

            tsl = 0.0 ;
            for( ii=nzz-1 ; ii >=0 ; ii-- ){
               user_inputs.tpattern[ii] = tsl ; tsl += tframe ;
            }
         } else if( nzz == 1 ||
                   (strcmp(tpattern,"zero")==0 || strcmp(tpattern,"simult")==0) ){

            /*--- delete the tpattern ---*/

            free(user_inputs.tpattern) ;
            user_inputs.tpattern = NULL ;

         } else {
            fprintf(stderr,"Unknown tpattern = %s\n",tpattern) ;
         }

         nopt++ ; continue ;
      }

      /********** non-dataset arguments ************/

      /*----- -gamma # option -----*/

      if( strncmp(Argv[nopt],"-gamma",4) == 0 ){
         float val ;
         if( nopt+1 >= Argc ) FatalError("need an argument after -gamma!");

         val = strtod( Argv[++nopt] , NULL ) ;
         if( val > 0 ) argopt.gamma = val ;
         else fprintf(stderr,
                "\n*** warning: -gamma value %s illegal\n", Argv[nopt]);

         nopt++ ; continue ;  /* go to next arg */
      }

      /*----- -gsfac value option -----*/

      if( strncmp(Argv[nopt],"-gsfac",4) == 0 ){
         float val ;
         if( nopt+1 >= Argc ) FatalError("need an argument after -gsfac!");

         val = strtod( Argv[++nopt] , NULL ) ;
         if( val != 0.0 ) argopt.gsfac = val ;
         else fprintf(stderr,
                "\n*** warning: -gsfac value %s illegal\n", Argv[nopt]);

         nopt++ ; continue ;  /* go to next arg */
      }

      /*----- -datum type option -----*/

      if( strncmp(Argv[nopt],"-datum",6) == 0 ){
         if( ++nopt >= Argc ) FatalError("need an argument after -datum!") ;

         if( strcmp(Argv[nopt],"short") == 0 ){
            argopt.datum_all = MRI_short ;
         } else if( strcmp(Argv[nopt],"float") == 0 ){
            argopt.datum_all = MRI_float ;
         } else if( strcmp(Argv[nopt],"complex") == 0 ){
            argopt.datum_all = MRI_complex ;
         } else if( strcmp(Argv[nopt],"byte") == 0 ){
            argopt.datum_all = MRI_byte ;
         } else {
            char buf[256] ;
            sprintf(buf,"-datum of type '%s' is not supported in AFNI!",
                    Argv[nopt] ) ;
            FatalError(buf) ;
         }

         nopt++ ; continue ;  /* go to next arg */
      }

      /*----- -ncolor # option -----*/

      if( strncmp(Argv[nopt],"-ncolor",4) == 0 ){
         float val ;
         if( nopt+1 >= Argc ) FatalError("need an argument after -ncolor!");

         val = strtod( Argv[++nopt] , NULL ) ;
         if( val > 4 ) argopt.ncolor = val ;
         else fprintf(stderr,
                "\n*** warning: -ncolor value %s illegal\n", Argv[nopt]);

         nopt++ ; continue ;  /* go to next arg */
      }

      /*----- -xtwarns option -----*/

      if( strncmp(Argv[nopt],"-xtwarns",6) == 0 ){
         argopt.xtwarns = True ;
         nopt++ ; continue ;  /* go to next arg */
      }

      /*----- -2swap and -4swap options -----*/

      if( strncmp(Argv[nopt],"-2swap",4) == 0 ){
         argopt.swap_two = 1 ;
         nopt++ ; continue ;  /* go to next arg */
      }

      if( strncmp(Argv[nopt],"-4swap",4) == 0 ){
         argopt.swap_four = 1 ;
         nopt++ ; continue ;  /* go to next arg */
      }

      /*----- -nofloatscan -----*/

      if( strncmp(Argv[nopt],"-nofloatscan",6) == 0 ){
         argopt.nofloatscan = 1 ;
         nopt++ ; continue ;  /* go to next arg */
      }

      /*--- illegal option ---*/

      printf("*** ILLEGAL OPTION: %s\n\n",Argv[nopt]) ;
      FatalError("cannot continue") ;

   }

   First_Image_Arg = nopt ;

   /* 14 Sep 1999: manufacture a command line for History,
                   but with (possibly) fewer image inputs */

   { int nim=Argc-First_Image_Arg , ii ;
     char ** qargv ;

     if( nim < 9 ){     /* 8 or fewer images ==> copy them all */

        qargv = Argv ;

     } else {           /* im0 im1 im2 ... im<last-1> im<last> */

        qargv = (char **) malloc( sizeof(char *) * Argc ) ;   /* copy all */
        for( ii=0 ; ii < Argc ; ii++ ) qargv[ii] = Argv[ii] ; /* argv's   */

        qargv[First_Image_Arg+3] = "..." ;   /* notice of omission */

        for( ii=First_Image_Arg+4 ; ii < Argc-2 ; ii++ ) /* these will */
           qargv[ii] = NULL ;                            /* be omitted */
     }

     commandline = tross_commandline( "to3d" , Argc , qargv ) ;

     if( qargv != Argv ) free(qargv) ;
   }

   return ;
}

/*--------------------------------------------------------------------*/

void Syntax()
{
   int ii ;

   printf(
    "Usage: to3d [options] image_files ...\n"
    "       Creates 3D datasets for use with AFNI from 2D image files\n"
    "\n"
    "The available options are\n"
    "  -help   show this message\n"
    "  -'type' declare images to contain data of a given type\n"
    "          where 'type' is chosen from the following options:\n" ) ;

   printf("       ANATOMICAL TYPES\n") ;
   for( ii=FIRST_ANAT_TYPE ; ii <= LAST_ANAT_TYPE ; ii++ )
      printf("     %8s == %s\n",ANAT_prefixstr[ii],ANAT_typestr[ii] ) ;

   printf("       FUNCTIONAL TYPES\n") ;
   for( ii=FIRST_FUNC_TYPE ; ii <= LAST_FUNC_TYPE ; ii++ )
      printf("     %8s == %s\n",FUNC_prefixstr[ii],FUNC_typestr[ii] ) ;

   printf(
    "                 [for paired (+) types above, images are fim first,]\n"
    "                 [then followed by the threshold (etc.) image files]\n" ) ;

   printf(
    "\n"
    "  -statpar value value ... value [* NEW IN 1996 *]\n"
    "     This option is used to supply the auxiliary statistical parameters\n"
    "     needed for certain dataset types (e.g., 'fico' and 'fitt').  For\n"
    "     example, a correlation coefficient computed using program 'fim2'\n"
    "     from 64 images, with 1 ideal, and with 2 orts could be specified with\n"
    "       -statpar 64 1 2\n"
   ) ;

   printf(
    "\n"
#ifndef NO_NAMES
    "  -dname   name      will make 3D dataset's name = 'name'\n"
    "  -dlabel  name      will make 3D dataset's short label = 'name'\n"
#endif
    "  -prefix  name      will write 3D dataset using prefix 'name'\n"
    "  -session name      will write 3D dataset into session directory 'name'\n"
    "  -geomparent fname  will read geometry data from dataset file 'fname'\n"
    "                       N.B.: geometry data does NOT include time-dependence\n"
    "  -anatparent fname  will take anatomy parent from dataset file 'fname'\n"
    "\n"
    "  -nosave  will suppress autosave of 3D dataset, which normally occurs\n"
    "           when the command line options supply all needed data correctly\n"
    "\n"
    "  -view type [* NEW IN 1996 *]\n"
    "    Will set the dataset's viewing coordinates to 'type', which\n"
    "    must be one of these strings:  "
            VIEW_ORIGINAL_CODE    " "
            VIEW_ACPCALIGNED_CODE " "
            VIEW_TALAIRACH_CODE   "\n"
   ) ;

   printf(
    "\n"
    "TIME DEPENDENT DATASETS [* NEW IN 1996 *]\n"
    "  -time:zt nz nt TR tpattern  OR  -time:tz nt nz TR tpattern\n"
    "\n"
    "    These options are used to specify a time dependent dataset.\n"
    "    '-time:zt' is used when the slices are input in the order\n"
    "               z-axis first, then t-axis.\n"
    "    '-time:tz' is used when the slices are input in the order\n"
    "               t-axis first, then z-axis.\n"
    "\n"
    "    nz  =  number of points in the z-direction\n"
    "    nt  =  number of points in the t-direction\n"
    "            (thus exactly nt * nz slices must be read in)\n"
    "    TR  =  repetition interval between acquisitions of the\n"
    "            same slice, in milliseconds (or other units, as given below)\n"
    "\n"
    "    tpattern = Code word that identifies how the slices (z-direction)\n"
    "               were gathered in time.  The values that can be used:\n"
    "\n"
    "       alt+z = altplus   = alternating in the plus direction\n"
    "       alt-z = altminus  = alternating in the minus direction\n"
    "       seq+z = seqplus   = sequential in the plus direction\n"
    "       seq-z = seqminus  = sequential in the minus direction\n"
    "       zero  = simult    = simultaneous acquisition\n"
    "               @filename = read temporal offsets from 'filename'\n"\
    "\n"
    "    For example if nz = 5 and TR = 1000, then the inter-slice\n"
    "    time is taken to be dt = TR/nz = 200.  In this case, the\n"
    "    slices are offset in time by the following amounts:\n"
    "\n"
    "                    S L I C E   N U M B E R\n"
    "      tpattern        0    1    2    3    4  Comment\n"
    "      ----------   ---- ---- ---- ---- ----  -------------------------------\n"
    "      altplus         0  600  200  800  400  Alternating in the +z direction\n"
    "      altminus      400  800  200  600    0  Alternating in the -z direction\n"
    "      seqplus         0  200  400  600  800  Sequential  in the -z direction\n"
    "      seqplus       800  600  400  200    0  Sequential  in the -z direction\n"
    "      simult          0    0    0    0    0  All slices acquired at once\n"
    "\n"
    "    If @filename is used for tpattern, then nz ASCII-formatted numbers are\n"
    "    read from the file.  These are used to indicate the time offsets (in ms)\n"
    "    for each slice. For example, if 'filename' contains\n"
    "       0 600 200 800 400\n"
    "    then this is equivalent to 'altplus' in the above example.\n"
    "\n"
    "    Notes:\n"
    "      * Time-dependent functional datasets are not yet supported by\n"
    "          to3d or any other GPL AFNI package software.  For many users,\n"
    "          the proper dataset type for these datasets is '-epan'.\n"
    "      * Time-dependent datasets with more than one value per time point\n"
    "          (e.g., 'fith', 'fico', 'fitt') are also not allowed by to3d.\n"
    "      * If you use 'abut' to fill in gaps in the data and/or to\n"
    "          subdivide the data slices, you will have to use the @filename\n"
    "          form for tpattern, unless 'simult' or 'zero' is acceptable.\n"
    "      * At this time, the value of 'tpattern' is not actually used in\n"
    "          any GPL AFNI program.  The values are stored in the dataset\n"
    "          .HEAD files, and will be used in the future.\n"
    "      * The values set on the command line can't be altered interactively.\n"
    "      * The units of TR can be specified by the command line options below:\n"
    "            -t=ms or -t=msec  -->  milliseconds (the default)\n"
    "            -t=s  or -t=sec   -->  seconds\n"
    "            -t=Hz or -t=Hertz -->  Hertz (for chemical shift images?)\n"
    "          Alternatively, the units symbol ('ms', 'msec', 's', 'sec',\n"
    "            'Hz', or 'Hertz') may be attached to TR in the '-time:' option,\n"
    "            as in '-time:zt 16 64 4.0sec alt+z'\n"
   ) ;

   printf(
     "\n"
     "COMMAND LINE GEOMETRY SPECIFICATION [* NEW IN 1996 *]\n"
     "   -xFOV   <dimen1><direc1>-<dimen2><direc2>\n"
     "     or       or\n"
     "   -xSLAB  <dimen1><direc1>-<direc2>\n"
     "\n"
     "   (Similar -yFOV, -ySLAB, -zFOV and -zSLAB option are also present.)\n"
     "\n"
     " These options specify the size and orientation of the x-axis extent\n"
     " of the dataset.  <dimen#> means a dimension (in mm); <direc> is\n"
     " an anatomical direction code, chosen from\n"
     "      A (Anterior)    P (Posterior)    L (Left)\n"
     "      I (Inferior)    S (Superior)     R (Right)\n"
     " Thus, 20A-30P means that the x-axis of the input images runs from\n"
     " 20 mm Anterior to 30 mm Posterior.  For convenience, 20A-20P can be\n"
     " abbreviated as 20A-P.\n"
     "\n"
     " -xFOV  is used to mean that the distances are from edge-to-edge of\n"
     "          the outermost voxels in the x-direction.\n"
     " -xSLAB is used to mean that the distances are from center-to-center\n"
     "          of the outermost voxels in the x-direction.\n"
     "\n"
     " Under most circumstance, -xFOV , -yFOV , and -zSLAB would be the\n"
     " correct combination of geometry specifiers to use.  For example,\n"
     " a common type of run at MCW would be entered as\n"
     "    -xFOV 120I-S -yFOV 120A-P -zSLAB 60S-50I\n"
   ) ;

   printf(
    "\n"
    "INPUT IMAGE FORMATS [* SIGNIFICANTLY CHANGED IN 1996 *]\n"
    "  Image files may be single images of unsigned bytes or signed shorts\n"
    "  (64x64, 128x128, 256x256, 512x512, or 1024x1024) or may be grouped\n"
    "  images (that is, 3- or 4-dimensional blocks of data).\n"
    "  In the grouped case, the string for the command line file spec is like\n"
    "\n"
    "    3D:hglobal:himage:nx:ny:nz:fname   [16 bit input]\n"
    "    3Ds:hglobal:himage:nx:ny:nz:fname  [16 bit input, swapped bytes]\n"
    "    3Db:hglobal:himage:nx:ny:nz:fname  [ 8 bit input]\n"
    "    3Di:hglobal:himage:nx:ny:nz:fname  [32 bit input]\n"
    "    3Df:hglobal:himage:nx:ny:nz:fname  [floating point input]\n"
    "    3Dc:hglobal:himage:nx:ny:nz:fname  [complex input]\n"
    "\n"
    "  where '3D:' or '3Ds': signals this is a 3D input file of signed shorts\n"
    "        '3Db:'          signals this is a 3D input file of unsigned bytes\n"
    "        '3Di:'          signals this is a 3D input file of signed ints\n"
    "        '3Df:'          signals this is a 3D input file of floats\n"
    "        '3Dc:'          signals this is a 3D input file of complex numbers\n"
    "                         (real and imaginary pairs of floats)\n"
    "        hglobal = number of bytes to skip at start of whole file\n"
    "        himage  = number of bytes to skip at start of each 2D image\n"
    "        nx      = x dimension of each 2D image in the file\n"
    "        ny      = y dimension of each 2D image in the file\n"
    "        nz      = number of 2D images in the file\n"
    "        fname   = actual filename on disk to read\n"
    "\n"
    "  * The ':' separators are required.  The k-th image starts at\n"
    "      BYTE offset hglobal+(k+1)*himage+vs*k*nx*ny in file 'fname'\n"
    "      for k=0,1,...,nz-1.\n"
    "  * Here, vs=voxel length=1 for bytes, 2 for shorts, 4 for ints and floats,\n"
    "      and 8 for complex numbers.\n"
    "  * As a special case, hglobal = -1 means read data starting at\n"
    "      offset len-nz*(vs*nx*ny+himage), where len=file size in bytes.\n"
    "      (That is, to read the needed data from the END of the file.)\n"
    "  * Note that there is no provision for skips between data rows inside\n"
    "      a 2D slice, only for skips between 2D slice images.\n"
    "  * The int, float, and complex formats presume that the data in\n"
    "      the image file are in the 'native' format for this CPU; that is,\n"
    "      there is no provision for data conversion (unlike the 3Ds: format).\n"
    "  * Whether the 2D image data is interpreted as a 3D block or a 3D+time\n"
    "      block depends on the rest of the command line parameters.  The\n"
    "      various 3D: input formats are just ways of inputting multiple 2D\n"
    "      slices from a single file.\n"
    "\n"
    "The 'raw pgm' image format is also supported; it reads data into 'byte' images.\n"
    "\n"
    "  Notes:\n"
    "   * Not all GPL AFNI programs support all datum types.  Shorts and\n"
    "       floats are safest. (See the '-datum' option below.)\n"
    "   * If '-datum short' is used or implied, then int, float, and complex\n"
    "       data will be scaled to fit into a 16 bit integer.  If the '-gsfac'\n"
    "       option below is NOT used, then each slice will be SEPARATELY\n"
    "       scaled according to the following choice:\n"
    "       (a) If the slice values all fall in the range -32767 .. 32767,\n"
    "           then no scaling is performed.\n"
    "       (b) Otherwise, the image values are scaled to lie in the range\n"
    "           0 .. 10000 (original slice min -> 0, original max -> 10000).\n"
    "       This latter option is almost surely not what you want!  Therefore,\n"
    "       if you use the 3Di:, 3Df:, or 3Dc: input methods and store the\n"
    "       data as shorts, I suggest you supply a global scaling factor.\n"
    "       Similar remarks apply to '-datum byte' scaling, with even more force.\n"
    "   * To3d now incoporates POSIX filename 'globbing', which means that\n"
    "       you can input filenames using 'escaped wildcards', and then to3d\n"
    "       will internally do the expansion to the list of files.  This is\n"
    "       only desirable because some systems limit the number of command-line\n"
    "       arguments to a program.  It is possible that you would wish to input\n"
    "       more slice files than your computer supports.  For example,\n"
    "           to3d exp.?.*\n"
    "       might overflow the system command line limitations.  The way to do\n"
    "       this using internal globbing would be\n"
    "           to3d exp.\\?.\\*\n"
    "       where the \\ characters indicate to pass the wildcards ? and *\n"
    "       through to the program, rather than expand them in the shell.\n"
    "       (a) Note that if you choose to use this feature, ALL wildcards in\n"
    "           a filename must be escaped with \\ or NONE must be escaped.\n"
    "       (b) Using the C shell, it is possible to turn off shell globbing\n"
    "           by using the command 'set noglob' -- if you do this, then you\n"
    "           do not need to use the \\ character to escape the wildcards.\n"
    "       (c) Internal globbing of 3D: file specifiers is supported in to3d.\n"
    "           For example, '3D:0:0:64:64:100:sl.\\*' could be used to input\n"
    "           a series of 64x64x100 files with names 'sl.01', 'sl.02' ....\n"
    "           This type of expansion is specific to to3d; the shell will not\n"
    "           properly expand such 3D: file specifications.\n"
    "       (d) In the C shell (csh or tcsh), you can use forward single 'quotes'\n"
    "           to prevent shell expansion of the wildcards, as in the command\n"
    "               to3d '3D:0:0:64:64:100:sl.*'\n"
    "     The globbing code is adapted from software developed by the\n"
    "     University of California, Berkeley, and is copyrighted by the\n"
    "     Regents of the University of California (see file mcw_glob.c).\n"
    "\n"
    "  -2swap\n"
    "     This option will force all input 2 byte images to be byte-swapped\n"
    "     after they are read in.\n"
    "  -4swap\n"
    "     This option will force all input 4 byte images to be byte-swapped\n"
    "     after they are read in.\n"
    "\n"
    "  -gsfac value\n"
    "     will scale each input slice by 'value'.  For example,\n"
    "     '-gsfac 0.31830989' will scale by 1/Pi (approximately).\n"
    "     This option only has meaning if one of '-datum short' or\n"
    "     '-datum byte' is used or implied.  Otherwise, it is ignored.\n"
    "\n"
    "  -datum type\n"
    "     will set the voxel data to be stored as 'type', which is currently\n"
    "     allowed to be short, float, byte, or complex.\n"
    "     If -datum is not used, then the datum type of the first input image\n"
    "     will determine what is used.  In that case, the first input image will\n"
    "     determine the type as follows:\n"
    "        byte       --> byte\n"
    "        short      --> short\n"
    "        int, float --> float\n"
    "        complex    --> complex\n"
    "     If -datum IS specified, then all input images will be converted\n"
    "     to the desired type.  Note that the list of allowed types may\n"
    "     grow in the future, so you should not rely on the automatic\n"
    "     conversion scheme.  Also note that floating point datasets may\n"
    "     not be portable between CPU architectures.\n"
    "\n"
    "  -nofloatscan\n"
    "     tells to3d NOT to scan input float and complex data files for\n"
    "     illegal values - the default is to scan and replace illegal\n"
    "     floating point values with zeros (cf. program float_scan).\n"
#ifdef USE_MRI_DELAY
    "\n"
    "  -in:1\n"
    "     Input of huge 3D: files (with all the data from a 3D+time run, say)\n"
    "     can cause to3d to fail from lack of memory.  The reason is that\n"
    "     the images are from a file are all read into RAM at once, and then\n"
    "     are scaled, converted, etc., as needed, then put into the final\n"
    "     dataset brick.  This switch will cause the images from a 3D: file\n"
    "     to be read and processed one slice at a time, which will lower the\n"
    "     amount of memory needed.  The penalty is somewhat more I/O overhead.\n"
#endif
   ) ;

   printf(
    "\n"
    "NEW IN 1997:\n"
    "  -orient code\n"
    "     Tells the orientation of the 3D volumes.  The code must be 3 letters,\n"
    "     one each from the pairs {R,L} {A,P} {I,S}.  The first letter gives\n"
    "     the orientation of the x-axis, the second the orientation of the\n"
    "     y-axis, the third the z-axis:\n"
    "        R = right-to-left         L = left-to-right\n"
    "        A = anterior-to-posterior P = posterior-to-anterior\n"
    "        I = inferior-to-superior  S = superior-to-inferior\n"
    "     Note that the -xFOV, -zSLAB constructions can convey this information.\n"
   ) ;

   printf(
    "\n"
    "OPTIONS THAT AFFECT THE X11 IMAGE DISPLAY\n"
    "   -gamma gg    the gamma correction factor for the\n"
    "                  monitor is 'gg' (default gg is 1.0; greater than\n"
    "                  1.0 makes the image contrast larger -- this may\n"
    "                  also be adjusted interactively)\n"
    "   -ncolors nn  use 'nn' gray levels for the image\n"
    "                  displays (default is %d)\n"
    "   -xtwarns     turn on display of Xt warning messages\n" ,
    INIT_ngray
   ) ;

   exit(0) ;
}

/*---------------------------------------------------------------------
  read the centered button box and set arrowvals on or off
-----------------------------------------------------------------------*/

void T3D_centered_CB( Widget w ,
                      XtPointer client_data , XtPointer call_data )
{
   int val ;
   Boolean sens ;

   user_inputs.xyz_centered = val = MCW_val_bbox( wset.centered_bbox ) ;

   sens = (val & XCENTERED) == 0 ;
      AV_SENSITIZE( wset.xorigin_av , sens ) ;
      XtSetSensitive( wset.xorigin_label , sens ) ;

   sens = (val & YCENTERED) == 0 ;
      AV_SENSITIZE( wset.yorigin_av , sens ) ;
      XtSetSensitive( wset.yorigin_label , sens ) ;

   sens = (val & ZCENTERED) == 0 ;
      AV_SENSITIZE( wset.zorigin_av , sens ) ;
      XtSetSensitive( wset.zorigin_label , sens ) ;

   T3D_set_dependent_geometries() ;
   RESET_QUIT ;
}

#ifdef ALLOW_NONCONTIG
/*---------------------------------------------------------------------
   read the voxcontig button box and set the appropriate arrowvals
-----------------------------------------------------------------------*/

void T3D_voxcontig_CB( Widget w ,
                       XtPointer client_data , XtPointer call_data )
{
   int val ;
   Boolean sens ;

   user_inputs.voxcontig = val = MCW_val_bbox( wset.voxcontig_bbox ) ;

   sens = (val != VOXCONTIG_YES) ; AV_SENSITIZE( wset.zspacing_av , sens ) ;
   T3D_set_dependent_geometries() ;
   RESET_QUIT ;
}
#endif /* ALLOW_NONCONTIG */

/*---------------------------------------------------------------------
   Set the dependent geometry values, depending on various flags
-----------------------------------------------------------------------*/

void T3D_set_dependent_geometries(void)
{
   float size ;

QQQ("T3D_set_dependent_geometries: entry") ;

   if( ! XtIsRealized( wset.topshell ) ) return ;

   /* Voxel shapes not irregular?  Then set them appropriately. */

   if( user_inputs.voxshape != VOXSHAPE_IRREGULAR && user_inputs.nx > 0 ){

      size = user_inputs.fov / user_inputs.nx ;

      if( user_inputs.xsize != size ){
         AV_assign_fval( wset.xsize_av , size ) ;
         user_inputs.xsize = size ;
      }

      if( user_inputs.ysize != size ){
         AV_assign_fval( wset.ysize_av , size ) ;
         user_inputs.ysize = size ;
      }

      if( user_inputs.voxshape == VOXSHAPE_CUBICAL &&
          user_inputs.zsize    != size ){
         AV_assign_fval( wset.zsize_av , size ) ;
         user_inputs.zsize = size ;
      }
   }

QQQ(" -- voxshapes done") ;

#ifdef ALLOW_NONCONTIG
   /* contiguous voxels turned on? Then set zspacing to zsize. */

   if( user_inputs.voxcontig == VOXCONTIG_YES &&
       user_inputs.zspacing  != user_inputs.zsize ){
      AV_assign_fval( wset.zspacing_av , user_inputs.zsize ) ;
      user_inputs.zspacing = user_inputs.zsize ;
   }
#endif /* ALLOW_NONCONTIG */

   /* centered axes?  Then set origins from sizes. */

   if( (user_inputs.xyz_centered & XCENTERED) != 0 && user_inputs.nx != 0 ){
      size = 0.5 * (user_inputs.nx-1) * user_inputs.xsize ;
      if( size != user_inputs.xorigin ){
         AV_assign_fval( wset.xorigin_av , size ) ;
         user_inputs.xorigin = size ;
      }
   }

   if( (user_inputs.xyz_centered & YCENTERED) != 0 && user_inputs.ny != 0 ){
      size = 0.5 * (user_inputs.ny-1) * user_inputs.ysize ;
      if( size != user_inputs.yorigin ){
         AV_assign_fval( wset.yorigin_av , size ) ;
         user_inputs.yorigin = size ;
      }
   }

   if( (user_inputs.xyz_centered & ZCENTERED) != 0 && user_inputs.nz != 0 ){
#ifdef ALLOW_NONCONTIG
      size = 0.5 * (user_inputs.nz-1) * user_inputs.zspacing ;
#else
      size = 0.5 * (user_inputs.nz-1) * user_inputs.zsize ;
#endif
      if( size != user_inputs.zorigin ){
         AV_assign_fval( wset.zorigin_av , size ) ;
         user_inputs.zorigin = size ;
      }
   }

QQQ(" -- centers done") ;

}

/*---------------------------------------------------------------------
   read the voxshape button box and set the appropriate
   arrowvals to sensitive or insensitive
-----------------------------------------------------------------------*/

void T3D_voxshape_CB( Widget w ,
                      XtPointer client_data , XtPointer call_data )
{
   int val ;
   Boolean fov_sens , xsize_sens , ysize_sens , zsize_sens ;

   user_inputs.voxshape = val = MCW_val_bbox( wset.voxshape_bbox ) ;

   switch( val ){

      default:  XBell( XtDisplay(wset.topshell) , 100 ) ; return ;

      case VOXSHAPE_CUBICAL:
         fov_sens   = True ;
         xsize_sens = ysize_sens = zsize_sens = False ;
      break ;

      case VOXSHAPE_SQUARE:
         fov_sens   = True ;
         xsize_sens = ysize_sens = False ;
         zsize_sens = True ;
      break ;

      case VOXSHAPE_IRREGULAR:
         fov_sens   = False ;
         xsize_sens = ysize_sens = zsize_sens = True ;
      break ;

   }
   AV_SENSITIZE( wset.fov_av   , fov_sens   ) ;
   AV_SENSITIZE( wset.xsize_av , xsize_sens ) ;
   AV_SENSITIZE( wset.ysize_av , ysize_sens ) ;
   AV_SENSITIZE( wset.zsize_av , zsize_sens ) ;

   T3D_set_dependent_geometries() ;
   RESET_QUIT ;
}

/*-----------------------------------------------------------------------
   Text displaying routine (for arrowvals)
-------------------------------------------------------------------------*/

char * T3D_text_display( MCW_arrowval * av , XtPointer cd )
{
   char ** tar = (char **) cd ;
   int  ii = av->ival ;

   return tar[ii] ;
}

/*----------------------------------------------------------------------*/

void T3D_quit_timeout_CB( XtPointer client_data , XtIntervalId * id )
{
   RESET_QUIT ;
}

void T3D_quit_CB( Widget wcall ,
                  XtPointer client_data , XtPointer call_data )
{
   static Boolean first = True ;
   static Widget wquit  = NULL ;

  if( wcall == NULL ){
    if( wquit == NULL ) return ;
    MCW_set_widget_label( wquit , "quit" ) ;
    first = True ;
    return ;
  }

  if( first ){
     if( wquit == NULL ) wquit = wcall ;
     first = False ;
     MCW_set_widget_label( wquit , "QUIT" ) ;

     (void) XtAppAddTimeOut( XtWidgetToApplicationContext(wcall) ,
                             5000 , T3D_quit_timeout_CB , NULL ) ;

     return ;
  }
  exit(0) ;
}

/*---------------------------------------------------------------------*/

void T3D_swap_CB( Widget w , XtPointer cd , XtPointer call_data )
{
   int dd = mri_datum_size((MRI_TYPE)argopt.datum_all) ;
   int nx , ny , nz , nv , nvox ;

   nx = dset->daxes->nxx ; ny = dset->daxes->nyy ;
   nz = dset->daxes->nzz ; nv = dblk->nvals      ; nvox = nx*ny*nz*nv ;

   switch( dd ){
      case 2: swap_twobytes ( nvox , dbrick ) ; break ;
      case 4: swap_fourbytes( nvox , dbrick ) ; break ;
   }

   if( ISQ_REALZ(wset.seq) ){
      drive_MCW_imseq( wset.seq , isqDR_clearstat , NULL ) ;
      drive_MCW_imseq( wset.seq , isqDR_display   , (XtPointer)-1 ) ;
   }

   return ;
}

/*---------------------------------------------------------------------*/

void T3D_fov_av_CB( MCW_arrowval * av , XtPointer cd )
{
   user_inputs.fov = wset.fov_av->fval ;
   T3D_set_dependent_geometries() ;
   RESET_QUIT ;
}

/*---------------------------------------------------------------------*/

void T3D_orient_av_CB( MCW_arrowval * av , XtPointer cd )
{
   int ior = av->ival ;

   if( av == wset.xorient_av ){
      user_inputs.xorient = ior ;
      SET_ORIGIN_LABEL(wset.xorigin_label,user_inputs.xorient) ;
   } else if( av == wset.yorient_av ){
      user_inputs.yorient = ior ;
      SET_ORIGIN_LABEL(wset.yorigin_label,user_inputs.yorient) ;
   } else if( av == wset.zorient_av ){
      user_inputs.zorient = ior ;
      SET_ORIGIN_LABEL(wset.zorigin_label,user_inputs.zorient) ;
   } else if( av == wset.view_type_av ){
      user_inputs.view_type = ior ;
   } else {
      XBell( XtDisplay(wset.topshell) , 100 ) ;
      fprintf(stderr,"\n*** illegal call to T3D_orient_av_CB!\n") ;
   }
   RESET_QUIT ;
}

/*---------------------------------------------------------------------*/

void T3D_origin_av_CB( MCW_arrowval * av , XtPointer cd )
{
   float size = av->fval ;

   if( av == wset.xorigin_av ){
      user_inputs.xorigin = size ;
   } else if( av == wset.yorigin_av ){
      user_inputs.yorigin = size ;
   } else if( av == wset.zorigin_av ){
      user_inputs.zorigin = size ;
   } else {
      XBell( XtDisplay(wset.topshell) , 100 ) ;
      fprintf(stderr,"\n*** illegal call to T3D_origin_av_CB!\n") ;
   }
   RESET_QUIT ;
}

/*---------------------------------------------------------------------*/

void T3D_size_av_CB( MCW_arrowval * av , XtPointer cd )
{
   float size = av->fval ;

   if( av == wset.xsize_av ){
      user_inputs.xsize = size ;
   } else if( av == wset.ysize_av ){
      user_inputs.ysize = size ;
   } else if( av == wset.zsize_av ){
      user_inputs.zsize = size ;
#ifdef ALLOW_NONCONTIG
   } else if( av == wset.zspacing_av ){
      user_inputs.zspacing = size ;
#endif
   } else {
      XBell( XtDisplay(wset.topshell) , 100 ) ;
      fprintf(stderr,"\n*** illegal call to T3D_size_av_CB!\n") ;
   }
   T3D_set_dependent_geometries() ;
   RESET_QUIT ;
}

/*---------------------------------------------------------------------*/

void T3D_type_av_CB( MCW_arrowval * av , XtPointer cd )
{
   int itype = av->ival ;
   Boolean isfunc ;
   int nvals_old , nvals_new ;

   isfunc    = ISFUNCTYPE(user_inputs.dataset_type) ;
   nvals_old = (isfunc) ? FUNC_nvals[user_inputs.function_type]
                        : ANAT_nvals[user_inputs.anatomy_type]  ;

   if( av == wset.dataset_type_av ){
      user_inputs.dataset_type = itype ;

      isfunc = ISFUNCTYPE(itype) ;

#ifdef FUNCTION_ONLY_ANAT_PARENT
      SENSITIZE( wset.anatomy_parent_label     , isfunc ) ;
      SENSITIZE( wset.anatomy_parent_textfield , isfunc ) ;
#endif

      if( isfunc ){
         XtUnmapWidget( wset.anatomy_type_av->wrowcol ) ;
         XtMapWidget  ( wset.function_type_av->wrowcol ) ;
      } else {
         XtUnmapWidget( wset.function_type_av->wrowcol ) ;
         XtMapWidget  ( wset.anatomy_type_av->wrowcol ) ;
      }

   } else if( av == wset.function_type_av ){
      user_inputs.function_type = itype ;
   } else if( av == wset.anatomy_type_av ){
      user_inputs.anatomy_type = itype ;
   } else {
      XBell( XtDisplay(wset.topshell) , 100 ) ;
      fprintf(stderr,"\n*** illegal call to T3D_type_av_CB!\n") ;
   }

   /*--- check if # of values/pixel has altered ---*/

   isfunc    = ISFUNCTYPE(user_inputs.dataset_type) ;
   nvals_new = (isfunc) ? FUNC_nvals[user_inputs.function_type]
                        : ANAT_nvals[user_inputs.anatomy_type]  ;

   /*-- time-dependent data can have only 1 value per time point! --*/

   if( user_inputs.ntt > 0 && nvals_new != 1 ){
      T3D_poperr("***** DATA TYPE WARNING *****\n",
                 "New data type is not allowed\n"
                 "with time-dependent datatset!" ) ;
      return ;
   }

   if( nvals_new != nvals_old ){
      int nz = user_inputs.nimage / nvals_new ;
      if( nz * nvals_new != user_inputs.nimage ){
         T3D_poperr("**** DATA TYPE WARNING *****\n",
                    "Number of images not an even\n"
                    "multiple of # of data values" ) ;
      }

#ifdef AFNI_DEBUG
printf("T3D_type_av_CB: new nvals=%d  nz=%d\n",nvals_new,nz) ; fflush(stdout) ;
#endif

      user_inputs.nz    = nz ;
      user_inputs.nvals = nvals_new ;
      T3D_set_dependent_geometries() ;
      T3D_fix_dataset_dimen() ;
   }

   T3D_setup_stat_aux() ;

   RESET_QUIT ;
}

/*----------------------------------------------------------------------
  patch the dataset dimensions for altered nz/nvals
------------------------------------------------------------------------*/

void T3D_fix_dataset_dimen(void)
{
   int nx , ny , nz , nv , ibr , bsize , nvold ;

QQQ("T3D_fix_dataset_dimen: entry") ;

   nvold = dblk->nvals ;

   nv = dblk->nvals = dkptr->nvals  = user_inputs.nvals ;
   nx = user_inputs.nx ;
   ny = user_inputs.ny ;
   nz = daxes->nzz = dkptr->dimsizes[2]  = user_inputs.nz ;

QQQ("T3D_fix_dataset_dimen: about to clear bricks") ;

   for( ibr=0 ; ibr < nvold ; ibr++ )
      mri_clear_data_pointer( DBLK_BRICK(dblk,ibr) ) ;

   myXtFree(dblk->brick_bytes) ; dblk->brick_bytes = NULL ;
   myXtFree(dblk->brick_fac  ) ; dblk->brick_fac   = NULL ;

QQQ("T3D_fix_dataset_dimen: about to recreate bricks") ;

   THD_init_datablock_brick( dblk , argopt.datum_all , NULL ) ;

QQQ("T3D_fix_dataset_dimen: about to attach bricks") ;

   bsize = nx*ny*nz * mri_datum_size( argopt.datum_all ) ;
   for( ibr=0 ; ibr < nv ; ibr++ ){
      mri_fix_data_pointer( dbrick + ibr*bsize , DBLK_BRICK(dblk,ibr) ) ;
   }

QQQ("T3D_fix_dataset_dimen: exit") ;
  return ;
}

/*------------------------------------------------------------------------
   read the images from the remaining command line arguments,
   and start the setup of the 3dim_dataset
--------------------------------------------------------------------------*/

void T3D_read_images(void)
{
   MRI_IMAGE * im , * shim ;
   char * bar ;
   int npix , ii , bb , dsize ;
   int nx , ny , nz , nim , lf , isfunc , nvals , kz,kim , bsize,ibr ;
   MRI_IMARR * arr ;
   char iname[THD_MAX_NAME] ;
   float nonshort_min=1.E38 , nonshort_max=-1.E38 ;
   float nonbyte_min =1.E38 , nonbyte_max =-1.E38 ;
   int   nonshort_num=0 , nonfloat_num=0 , noncomplex_num=0 , nonbyte_num=0 ;
   int     gnim ;
   char ** gname ;
   int time_dep , ltt,kzz , ntt,nzz , nvoxt ;

#ifdef AFNI_DEBUG
printf("T3D_read_images: entry\n") ;
#endif

   nim = Argc - First_Image_Arg ;  /* = number of files, not images! */

   INIT_SARR( imnames ) ;  /* image name for each slice */

   MCW_file_expand( nim , Argv+First_Image_Arg , &gnim , &gname ) ;
#ifdef AFNI_DEBUG
printf("T3D_read_images: input file count = %d; expanded = %d\n",nim,gnim) ;
#endif

   if( gnim < 1 ){ FatalError("NO INPUT IMAGE FILES?") ; }

   /**--- count up the actual number of images into nz ---**/

   nz = 0 ;
   for( lf=0 ; lf < gnim ; lf++ ){
      ii = mri_imcount( gname[lf] ) ;
      if( ii == 0 ){
         fprintf(stderr,"*** bad file specifier %s\n",gname[lf]) ;
         exit(1) ;
      }
      nz += ii ;
   }
#ifdef AFNI_DEBUG
printf("T3D_read_images: mri_imcount totals nz=%d\n",nz) ;
#endif

   if( nz < 2 ){
      fprintf(stderr,"*** Must have at least 2 input images! ***\n") ;
      exit(1) ;
   }

   /**------ Perform various sanity checks if the user is
             trying to create a time dependent dataset.  ------**/

   time_dep = (user_inputs.ntt > 0) ;
   if( time_dep ){ ntt = user_inputs.ntt ; nzz = user_inputs.nzz ; }

   if( time_dep && nz != ntt * nzz ){
      fprintf(stderr,"*** Number of slices on command line   = %d\n"
                     "*** Number of slices needed for -time: = %d\n"
                     "*** Something is wrong with your command line!\n" ,
              nz , ntt * nzz ) ;
      exit(1) ;
   }

   if( time_dep ){
      isfunc = ISFUNCTYPE(user_inputs.dataset_type) ;
      nvals  = (isfunc) ? FUNC_nvals[user_inputs.function_type]
                        : ANAT_nvals[user_inputs.anatomy_type]  ;

      if( nvals != 1 ){
         fprintf(stderr,"*** Sorry: time dependent datasets with more than one\n"
                        "***        value per time point are not yet supported!\n" ) ;
         exit(1) ;
      }

#if 0
      if( isfunc ){
         fprintf(stderr,"*** Sorry: time dependent functional\n"
                        "***        datasets are not yet supported!\n" ) ;
         exit(1) ;
      }
#endif
   }

   /*--- read 1st file to get sizes ---*/

#ifdef USE_MRI_DELAY
   if( argopt.delay_input )
      arr = mri_read_file_delay( gname[0] ) ;
   else
#endif
      arr = mri_read_file( gname[0] ) ;

   if( arr == NULL || arr->num == 0 ){
      fprintf(stderr,"*** cannot read first file! ***\n") ; exit(1) ;
   }
   im = arr->imarr[0] ;

   nx = im->nx ;
   ny = im->ny ; npix = nx * ny ;

   /**--- use 1st file to set default datum type, if not set already ---**/

   if( argopt.datum_all < 0 ){
      switch( im->kind ){
         case MRI_byte:     argopt.datum_all = MRI_byte  ; break ;

         default:
         case MRI_short:    argopt.datum_all = MRI_short ; break ;

         case MRI_int:
         case MRI_double:
         case MRI_float:    argopt.datum_all = MRI_float ; break ;

         case MRI_complex:  argopt.datum_all = MRI_complex ; break ;
      }
   }

   /**--- allocate storage for all slices to be input ---**/

   dsize  = mri_datum_size( (MRI_TYPE) argopt.datum_all ) ;
   dbrick = bar = XtMalloc( dsize * nx * ny * nz ) ;
   nvoxt  = nx * ny * nz ;

#ifdef AFNI_DEBUG
printf("T3D_read_images: first file (%s) has nx=%d ny=%d #im=%d\n",
       gname[0],nx,ny,arr->num) ;
#endif

   /*--- read all files, convert to desired type if needed, put in the brick ---*/

#ifndef AFNI_DEBUG
   printf("Reading images: ");fflush(stdout);
#endif

   kz = 0 ; if( time_dep ){ ltt = kzz = 0 ; }

   for( lf=0 ; lf < gnim ; lf++ ){  /** loop over files **/

      /*--- open this file, if not the first (which we read in a minute ago) ---*/

      if( lf != 0 ){
#ifdef USE_MRI_DELAY
         if( argopt.delay_input )
            arr = mri_read_file_delay( gname[0] ) ;
         else
#endif
            arr = mri_read_file( gname[lf] ) ;

         if( arr == NULL || arr->num == 0 ){
            fprintf(stderr,"*** cannot read file %s\n",gname[lf]) ;
            exit(1) ;
         }
#ifdef AFNI_DEBUG
printf("T3D_read_images: file %d (%s) has #im=%d\n",lf,gname[lf],arr->num) ;
#endif
      }

      for( kim=0 ; kim < arr->num ; kim++ ){  /** loop over 2D images in file **/

         /*--- set string for this slice for display in image viewing window ---*/

         if( arr->num == 1 ){
            strcpy( iname , gname[lf] ) ;
         } else {
            sprintf( iname , "%s#%d" , gname[lf],kim) ;
         }
         ADDTO_SARR(imnames,iname) ;

         /*--- get the image and check if it fits the first image dimensions ---*/

         im = arr->imarr[kim] ;
         if( im->nx != nx || im->ny != ny ){
            fprintf(stderr,"*** file %s has nonconforming images!\n",
                   gname[lf] ) ;
            exit(1) ;
         }

#ifdef USE_MRI_DELAY
         if( argopt.delay_input )
            (void) mri_data_pointer( im ) ;  /* force load of image from disk */
#endif

         /* 14 Sep 1998: swap bytes if ordered */

         if( im->pixel_size == 2 && argopt.swap_two ){
            swap_twobytes( im->nvox , mri_data_pointer(im) ) ;
         } else if( im->pixel_size == 4 && argopt.swap_two ){
            swap_fourbytes( im->nvox , mri_data_pointer(im) ) ;
         }

         /* 14 Sep 1999: check float inputs for errors */

         if( !argopt.nofloatscan && im->kind == MRI_float )
            nfloat_err += thd_floatscan( im->nvox , MRI_FLOAT_PTR(im) ) ;
         else if( !argopt.nofloatscan && im->kind == MRI_complex )
            nfloat_err += thd_complexscan( im->nvox , MRI_COMPLEX_PTR(im) ) ;

         /*--- convert input image to desired type:  im --> shim ---*/

         if( im->kind == argopt.datum_all ){    /* data is desired type */
            shim = im ;

         } else {                               /* must convert data */
            switch( argopt.datum_all ){

               default: FatalError("Illegal argopt.datum_all!") ;

               case MRI_short:{                 /** convert to shorts **/
                  short * shar ;

                  if( argopt.gsfac == 0.0 ){      /* scale each slice by itself */
                     float immin , immax ;
                     immin = mri_min( im ) ; immax = mri_max( im ) ;
                     if( immin >= -32767.0 && immax <= 32767.0 )
                        shim = mri_to_short( 1.0 , im ) ;   /* no need to scale */
                     else
                        shim = mri_to_short_scl( 0.0 , 10000.0 , im ) ;  /* scale */

                     nonshort_min = MIN(immin,nonshort_min) ;
                     nonshort_max = MAX(immax,nonshort_max) ; nonshort_num++ ;
                     KILL_1MRI(im) ;
                  } else {                         /* use global scaling factor */
                     float immin , immax ;
                     shim = mri_to_short( argopt.gsfac , im ) ;
                     immin = mri_min( im ) ; immax = mri_max( im ) ;
                     nonshort_min = MIN(immin,nonshort_min) ;
                     nonshort_max = MAX(immax,nonshort_max) ; nonshort_num++ ;
                     KILL_1MRI(im) ;
                  }

                  /* correct for oddities of FIM */

                  shar = MRI_SHORT_PTR( shim ) ;  /* image of shorts */
                  if( shar[1] == -10000 && shar[2] == 10000 ) shar[1] = shar[2] = 0 ;
               }
               break ;  /* end of conversion to shorts */

               case MRI_byte:{                 /** convert to bytes **/

                  if( argopt.gsfac == 0.0 ){      /* scale each slice by itself */
                     float immin , immax ;
                     immin = mri_min( im ) ; immax = mri_max( im ) ;
                     if( immin >= 0 && immax <= 255.0 )
                        shim = mri_to_byte_scl( 1.0 , 0.0 , im ) ; /* no scale */
                     else
                        shim = mri_to_byte_scl( 0.0 , 255.0 , im ) ;  /* scale */

                     nonbyte_min = MIN(immin,nonbyte_min) ;
                     nonbyte_max = MAX(immax,nonbyte_max) ; nonbyte_num++ ;
                     KILL_1MRI(im) ;
                  } else {                         /* use global scaling factor */
                     float immin , immax ;
                     shim = mri_to_byte_scl( argopt.gsfac , 0.0 , im ) ;
                     immin = mri_min( im ) ; immax = mri_max( im ) ;
                     nonbyte_min = MIN(immin,nonbyte_min) ;
                     nonbyte_max = MAX(immax,nonbyte_max) ; nonbyte_num++ ;
                     KILL_1MRI(im) ;
                  }

               }
               break ;  /* end of conversion to bytes */

               case MRI_float:{            /** convert to floats **/
                  float * shar ;

                  shim = mri_to_float( im ) ;  nonfloat_num++ ;
                  KILL_1MRI(im) ;

                  shar = MRI_FLOAT_PTR( shim ) ;  /* image of floats */
                  if( shar[1]==-10000.0 && shar[2]==10000.0 ) shar[1]=shar[2]=0.0 ;
               }
               break ;  /* end of conversion to floats */

               case MRI_complex:{         /** convert to complexes **/
                  complex * shar ;

                  shim = mri_to_complex( im ) ;  noncomplex_num++ ;
                  KILL_1MRI(im) ;

                  shar = MRI_COMPLEX_PTR( shim ) ;  /* image of complexes */
                  if( shar[1].r == -10000.0 && shar[2].r == 10000.0 &&
                      shar[1].i ==      0.0 && shar[2].i ==     0.0   ){

                     shar[1].r=shar[2].r=0.0 ;
                  }
               }
               break ;  /* end of conversion to complexes */

            }
         }  /**-- end of conversion: desired image is in shim --**/

#ifdef AFNI_DEBUG
printf("T3D_read_images: putting data into slice %d\n",kz) ;
#endif

         if( ! time_dep ){

            /**-- copy data from shim into the kz-th slice in bar --**/

            bb = npix * dsize * kz ;
            memcpy( bar+bb , mri_data_pointer(shim) , npix*dsize ) ;
         } else {

            /**-- copy data from shim into the (kzz,ltt)-th slice in bar --**/

            bb = npix * dsize * ( kzz + ltt * nzz ) ;
            memcpy( bar+bb , mri_data_pointer(shim) , npix*dsize ) ;

            /*-- step the kzz,ltt indices forward, depending on the
                 order in which the slices are coming in (-time:zt or :tz) --*/

            if( user_inputs.t_then_z ){
              ltt++ ; if( ltt == ntt ){ ltt = 0 ; kzz++ ; }
            } else {
              kzz++ ; if( kzz == nzz ){ kzz = 0 ; ltt++ ; }
            }
         }
         kz++ ;

         KILL_1MRI(shim) ;
#ifndef AFNI_DEBUG
         if( kz%10 == 0 ){ printf("%d",(kz/10)%10) ; fflush(stdout); }
#endif
      }  /** end of loop over images from 1 file **/

      FREE_IMARR(arr) ;
   }  /** end of loop over files **/
#ifndef AFNI_DEBUG
   printf("\n");fflush(stdout);
#endif

   if( nfloat_err > 0 )  /* 14 Sep 1999 */
      printf("*** Found %d float errors in inputs - see program float_scan!\n",
             nfloat_err) ;

   MCW_free_expand( gnim , gname ) ;

   /**-- 19 Jan 2000: check inputs shorts for negativity --**/

   if( argopt.datum_all == MRI_short ){
      short * sar = (short *) dbrick ;
      for( ii=0 ; ii < nvoxt ; ii++ )
         if( sar[ii] < 0 ) negative_shorts++ ;
   }

   /**--- print conversion information ---**/

   if( nonshort_num > 0 )
      printf( "Number of non-short slices converted to shorts = %d\n"
              "Smallest value in them                         = %f\n"
              "Largest value in them                          = %f\n" ,
             nonshort_num , nonshort_min , nonshort_max ) ;

   if( nonbyte_num > 0 )
      printf( "Number of non-byte slices converted to bytes = %d\n"
              "Smallest value in them                       = %f\n"
              "Largest value in them                        = %f\n" ,
             nonbyte_num , nonbyte_min , nonbyte_max ) ;

   if( nonfloat_num > 0 )
      printf( "Number of non-float slices converted to floats = %d\n",
             nonfloat_num ) ;

   if( noncomplex_num > 0 )
      printf( "Number of non-complex slices converted to complexes = %d\n",
             noncomplex_num ) ;

   /*--- now create the rest of the data structures, as far as we can ---*/

   isfunc = ISFUNCTYPE(user_inputs.dataset_type) ;
   nvals  = (isfunc) ? FUNC_nvals[user_inputs.function_type]
                     : ANAT_nvals[user_inputs.anatomy_type]  ;

   nim    = kz ;  /* number of images actually processed above */

   if( time_dep ){
      nvals = ntt ;
      if( nim != ntt * nzz ){
         fprintf(stderr,
                  "\n"
                  "***** TIME-DEPENDENCE ERROR *****\n"
                  " Number of images input does not\n"
                  " match number specified in -time:\n"
                  " option on command line!\n" ) ;
         exit(1) ;
      }
   }

#ifdef AFNI_DEBUG
printf("T3D_read_images: nvals set to %d\n",nvals) ;
#endif

   nz = nim / nvals ;
   if( nz * nvals != nim ){
      fprintf(stderr,
               "\n"
               "***** DATA TYPE ERROR *****\n"
               " Number of images not an even\n"
               " multiple of number of values\n"
               " required for chosen data type\n" ) ;
      exit(1) ;
   }

   dset    =                 myXtNew( THD_3dim_dataset ) ;  /* these are */
   dblk    = dset->dblk    = myXtNew( THD_datablock ) ;     /* globals */
   daxes   = dset->daxes   = myXtNew( THD_dataxes ) ;
   markers = dset->markers = NULL ;                       /* later, dude */
   dkptr   = dblk->diskptr = myXtNew( THD_diskptr ) ;

   dset->tagset = NULL ;  /* Oct 1998 */

   INIT_KILL(dset->kl) ; INIT_KILL(dblk->kl) ;

   dkptr->type         = DISKPTR_TYPE ;
   dkptr->rank         = 3 ;
   dkptr->nvals        = nvals ;
   dkptr->dimsizes[0]  = nx ;
   dkptr->dimsizes[1]  = ny ;
   dkptr->dimsizes[2]  = nz ;
   dkptr->storage_mode = STORAGE_BY_BRICK ;
   dkptr->byte_order   = THD_get_write_order() ;  /* 25 April 1998 */
   EMPTY_STRING(dkptr->prefix) ;
   EMPTY_STRING(dkptr->viewcode) ;
   EMPTY_STRING(dkptr->filecode) ;
   EMPTY_STRING(dkptr->directory_name) ;
   EMPTY_STRING(dkptr->header_name) ;
   EMPTY_STRING(dkptr->brick_name) ;

   dblk->type        = DATABLOCK_TYPE ;
   dblk->nvals       = dkptr->nvals ;
   dblk->malloc_type = DATABLOCK_MEM_MALLOC ;
   dblk->natr        = 0 ;
   dblk->natr_alloc  = 0 ;
   dblk->atr         = NULL ;
   dblk->brick       = NULL ;
   dblk->brick_fac   = NULL ;
   dblk->brick_bytes = NULL ;

   THD_init_datablock_brick( dblk , argopt.datum_all , NULL ) ;
   THD_null_datablock_auxdata( dblk ) ;

   dblk->master_nvals = 0 ;     /* 11 Jan 1999 */
   dblk->master_ival  = NULL ;
   dblk->master_bytes = NULL ;

   bsize = nx*ny*nz * mri_datum_size( argopt.datum_all ) ;
   for( ibr=0 ; ibr < nvals ; ibr++ ){
      mri_fix_data_pointer( dbrick + ibr*bsize , DBLK_BRICK(dblk,ibr) ) ;
   }

   daxes->type     = DATAXES_TYPE ;
   daxes->nxx      = nx ;
   daxes->nyy      = ny ;
   daxes->nzz      = nz ;
   daxes->xxorg    = - user_inputs.xorigin ;
   daxes->yyorg    = - user_inputs.yorigin ;
   daxes->zzorg    = - user_inputs.zorigin ;
   daxes->xxdel    =   user_inputs.xsize ;
   daxes->yydel    =   user_inputs.ysize ;
#ifdef ALLOW_NONCONTIG
   daxes->zzdel    =   user_inputs.zspacing ;
#else
   daxes->zzdel    =   user_inputs.zsize ;
#endif
   daxes->xxorient = user_inputs.xorient ;
   daxes->yyorient = user_inputs.yorient ;
   daxes->zzorient = user_inputs.zorient ;

   dset->type      = user_inputs.dataset_type ;
   dset->view_type = user_inputs.view_type ;
   dset->func_type = ISANAT(dset) ? (user_inputs.anatomy_type)
                                  : (user_inputs.function_type) ;

   dset->wod_daxes   = NULL ;
   dset->wod_flag    = 0 ;
   dset->stats       = NULL ;
   dset->pts         = NULL ;
   dset->death_mark  = 0 ;

   ZERO_STAT_AUX( dset ) ;

   user_inputs.nx     = nx ;
   user_inputs.ny     = ny ;
   user_inputs.nz     = nz ;
   user_inputs.nimage = nim ;
   user_inputs.nvals  = nvals ;

   /**--- fix dimensions if user input axes stuff ---**/

   if( user_inputs.xincode > 0 ){
      int dcode , fov ;
      float dx , xorg , size ;

      fov   = 1 ;
      dcode = user_inputs.xincode - INCODE_FOV ;

      if( dcode < FIRST_ORIENT_TYPE || dcode > LAST_ORIENT_TYPE ){
         dcode = user_inputs.xincode - INCODE_SLAB ;
         fov   = 0 ;
      }

      if( fov ){
         dx   = (user_inputs.xin_bot + user_inputs.xin_top) / nx ;
         xorg =  user_inputs.xin_bot - 0.5*dx ;
      } else {
         dx   = (user_inputs.xin_bot + user_inputs.xin_top) / (nx-1) ;
         xorg =  user_inputs.xin_bot ;
      }
      user_inputs.xorient = dcode ;
      user_inputs.xsize   = dx ;
      user_inputs.xorigin = xorg ;
      user_inputs.fov     = nx * dx ; geometry_loaded = 1 ;

      size = 0.5 * (nx-1) * dx ;
      if( fabs(size-xorg) < 0.01 )
         user_inputs.xyz_centered |= XCENTERED ;
      else
         user_inputs.xyz_centered &= ~XCENTERED ;
   }

   if( user_inputs.yincode > 0 ){
      int dcode , fov ;
      float dy , yorg , size ;

      fov   = 1 ;
      dcode = user_inputs.yincode - INCODE_FOV ;

      if( dcode < FIRST_ORIENT_TYPE || dcode > LAST_ORIENT_TYPE ){
         dcode = user_inputs.yincode - INCODE_SLAB ;
         fov   = 0 ;
      }

      if( fov ){
         dy   = (user_inputs.yin_bot + user_inputs.yin_top) / ny ;
         yorg =  user_inputs.yin_bot - 0.5*dy ;
      } else {
         dy   = (user_inputs.yin_bot + user_inputs.yin_top) / (ny-1) ;
         yorg =  user_inputs.yin_bot ;
      }
      user_inputs.yorient = dcode ;
      user_inputs.ysize   = dy ;
      user_inputs.yorigin = yorg ;
      user_inputs.fov     = ny * dy ; geometry_loaded = 1 ;

      size = 0.5 * (ny-1) * dy ;
      if( fabs(size-yorg) < 0.01 )
         user_inputs.xyz_centered |= YCENTERED ;
      else
         user_inputs.xyz_centered &= ~YCENTERED ;
   }

   if( user_inputs.zincode > 0 ){
      int dcode , fov ;
      float dz , zorg , size ;

      fov   = 1 ;
      dcode = user_inputs.zincode - INCODE_FOV ;

      if( dcode < FIRST_ORIENT_TYPE || dcode > LAST_ORIENT_TYPE ){
         dcode = user_inputs.zincode - INCODE_SLAB ;
         fov   = 0 ;
      }

      if( fov ){
         dz   = (user_inputs.zin_bot + user_inputs.zin_top) / nz ;
         zorg =  user_inputs.zin_bot - 0.5*dz ;
      } else {
         if( nz == 1 ){
            fprintf(stderr,"*** -zSLAB illegal with only 1 slice! ***\n") ;
            exit(1) ;
         }
         dz   = (user_inputs.zin_bot + user_inputs.zin_top) / (nz-1) ;
         zorg =  user_inputs.zin_bot ;
      }
      user_inputs.zorient = dcode ;
      user_inputs.zsize   = dz ;
      user_inputs.zorigin = zorg ;

      size = 0.5 * (nz-1) * dz ;
      if( fabs(size-zorg) < 0.01 )
         user_inputs.xyz_centered |= ZCENTERED ;
      else
         user_inputs.xyz_centered &= ~ZCENTERED ;
   }

   dset->taxis = NULL ;  /* will be patched later, if necessary */

   if( commandline != NULL ) tross_Append_History( dset , commandline ) ;

   /*********** DONE **********/

   return ;
}

/*-------------------------------------------------------------*/

void T3D_open_view_CB( Widget w ,
                       XtPointer client_data , XtPointer call_data )
{
   FD_brick * br ;
   char * title = "to3d" ;
   int nim ;

   if( wset.seq != NULL ){
      if( ISQ_REALZ(wset.seq) )
         XMapRaised( XtDisplay(wset.topshell) , XtWindow(wset.seq->wtop) ) ;
      else
         XBell( XtDisplay(wset.topshell) , 100 ) ;
      return ;
   }

   daxes->xxdel = user_inputs.xsize ;  /* these are here to make */
   daxes->yydel = user_inputs.ysize ;  /* the aspect ratio work  */
#ifdef ALLOW_NONCONTIG
   daxes->zzdel = user_inputs.zspacing ;
#else
   daxes->zzdel = user_inputs.zsize ;
#endif

   br = THD_3dim_dataset_to_brick( dset , 1,2,3 ) ;

   if( br == NULL ){
      XBell( XtDisplay(wset.topshell) , 100 ) ;
      fprintf(stderr,"\n*** bad data in THD_3dim_dataset_to_brick!\n");
      return ;
   }

   wset.seq = open_MCW_imseq( wset.dc , T3D_getim , br ) ;
   drive_MCW_imseq( wset.seq , isqDR_realize , NULL ) ;

   drive_MCW_imseq( wset.seq , isqDR_getimnr , (XtPointer) &nim ) ;
   drive_MCW_imseq( wset.seq , isqDR_title   , (XtPointer) imnames->ar[nim] ) ;

   /* 01 Dec 1999: add "sides" markers for image viewer */

   { static char * ws[4] = { "-x" , "-y" , "+x" , "+y" } ;
     drive_MCW_imseq( wset.seq, isqDR_winfosides, (XtPointer)ws ) ;
   }

   MCW_invert_widget( wset.open_view_pb ) ;

QQQ("open_view");

   RESET_QUIT ;
}

/*-------------------------------------------------------------------*/

XtPointer T3D_getim( int n , int type , FD_brick * br )
{
   MCW_imseq_status * stat ;

   if( n < 0 || n >= br->n3 || type == isqCR_getoverlay ) return NULL ;

   if( type == isqCR_getstatus ){
      stat = myXtNew( MCW_imseq_status ) ;

      stat->num_total  = br->n3 ;
      stat->num_series = br->n3 ;
      stat->send_CB    = T3D_imseq_CB ;
      stat->parent     = (XtPointer) br ;
      stat->aux        = NULL ;

      stat->transforms0D = NULL ;
      stat->transforms2D = NULL ;

      return (XtPointer) stat ;
   }

   if( type == isqCR_getimage || type == isqCR_getqimage ){
      return (XtPointer) FD_brick_to_mri( n , 0 , br ) ;
   }

   return NULL ; /* should never be reached */
}

/*-------------------------------------------------------------------*/

void T3D_imseq_CB( MCW_imseq * seq , FD_brick * br , ISQ_cbs * cbs )
{
   switch( cbs->reason ){

      case isqCR_destroy:
         myXtFree( seq ) ; wset.seq = NULL ;
         myXtFree( br ) ;
         MCW_invert_widget( wset.open_view_pb ) ;
      break ;

      case isqCR_newimage:
         drive_MCW_imseq( seq, isqDR_title, (XtPointer) imnames->ar[cbs->nim] ) ;
      break ;

      case isqCR_force_redisplay:{  /* 22 Aug 1998 */
         drive_MCW_imseq( seq , isqDR_display , (XtPointer) seq->im_nr ) ;
         drive_MCW_imseq( seq , isqDR_rebar   , (XtPointer) seq->im_nr ) ;
      }

   }
   return ;
}

/*-----------------------------------------------------------------------
   Save the stuff
-------------------------------------------------------------------------*/

void T3D_save_file_CB( Widget w ,
                       XtPointer client_data , XtPointer call_data )
{
   Boolean good , isfunc ;
   int ii , jj , bigfile ;
   Widget wmsg = NULL ;

QQQ("save_file1");

   /*-- store all control data in the user_inputs data struct --*/

   if( wset.topshell != NULL ) T3D_widgets_to_data() ;

   /*-- check for legal values --*/

   good = T3D_check_data( True ) ;
   if( !good ) return ;

   /*-- store values in dataset --*/

   dset->type      = user_inputs.dataset_type ;
   dset->view_type = user_inputs.view_type ;

   isfunc = ISFUNC(dset) ;

   dset->func_type = (isfunc) ? (user_inputs.function_type)
                              : (user_inputs.anatomy_type) ;

   if( isfunc && FUNC_HAVE_THR(dset->func_type) ){
      int iv = FUNC_ival_thr[dset->func_type] ;
      switch( DBLK_BRICK_TYPE(dblk,iv) ){
         default:
            dblk->brick_fac[iv] = 0.0 ;
         break ;

         case MRI_short:
            dblk->brick_fac[iv] = 1.0 / FUNC_scale_short[dset->func_type] ;
         break ;

         case MRI_byte:
            dblk->brick_fac[iv] = 1.0 / FUNC_scale_byte[dset->func_type] ;
         break ;
      }
   }

   if( user_inputs.need_stat_aux ){
      INIT_STAT_AUX( dset ,
                     FUNC_need_stat_aux[user_inputs.function_type] ,
                     user_inputs.stat_aux ) ;
   }

   dset->warp = NULL ;

   MCW_strncpy( dset->label1 , user_inputs.short_label1 , THD_MAX_LABEL ) ;
   MCW_strncpy( dset->label2 , user_inputs.short_label2 , THD_MAX_LABEL ) ;
   dset->keywords = NULL ;

   if( strlen(dset->label1) == 0 ){
      MCW_strncpy( dset->label1 , user_inputs.output_filename , THD_MAX_LABEL ) ;
   }

   EMPTY_STRING( dset->warp_parent_name ) ;
#ifndef OMIT_DATASET_IDCODES
   ZERO_IDCODE(dset->warp_parent_idcode) ;
#endif

   if( strlen(user_inputs.anatomy_dataname) > 0 ){
      MCW_strncpy( dset->anat_parent_name ,
                   user_inputs.anatomy_dataname , THD_MAX_NAME ) ;
#ifndef OMIT_DATASET_IDCODES
      dset->anat_parent_idcode = user_inputs.anatomy_parent_idcode ;
#endif
   } else {
      EMPTY_STRING( dset->anat_parent_name ) ;
#ifndef OMIT_DATASET_IDCODES
      ZERO_IDCODE(dset->anat_parent_idcode) ;
#endif
   }

   MCW_strncpy( dset->self_name, user_inputs.dataset_name, THD_MAX_NAME ) ;

   daxes->xxorient = user_inputs.xorient ;
   daxes->yyorient = user_inputs.yorient ;
   daxes->zzorient = user_inputs.zorient ;

   daxes->xxorg = (ORIENT_sign[user_inputs.xorient] == '+')
                  ? (-user_inputs.xorigin) : ( user_inputs.xorigin) ;

   daxes->yyorg = (ORIENT_sign[user_inputs.yorient] == '+')
                  ? (-user_inputs.yorigin) : ( user_inputs.yorigin) ;

   daxes->zzorg = (ORIENT_sign[user_inputs.zorient] == '+')
                  ? (-user_inputs.zorigin) : ( user_inputs.zorigin) ;

   daxes->xxdel = (ORIENT_sign[user_inputs.xorient] == '+')
                  ? ( user_inputs.xsize) : (-user_inputs.xsize) ;

   daxes->yydel = (ORIENT_sign[user_inputs.yorient] == '+')
                  ? ( user_inputs.ysize) : (-user_inputs.ysize) ;

#ifdef ALLOW_NONCONTIG
   daxes->zzdel = (ORIENT_sign[user_inputs.zorient] == '+')
                  ? ( user_inputs.zspacing) : (-user_inputs.zspacing) ;
#else
   daxes->zzdel = (ORIENT_sign[user_inputs.zorient] == '+')
                  ? ( user_inputs.zsize) : (-user_inputs.zsize) ;
#endif

   /*-- this stuff is no longer used --*/

#ifdef ALLOW_NONCONTIG
   daxes->xxskip = 0.0 ;
   daxes->yyskip = 0.0 ;
   daxes->zzskip = user_inputs.zspacing - user_inputs.zsize ;
#endif

   dset->taxis = NULL ;

   if( user_inputs.ntt > 0 ){
      dset->taxis = myXtNew( THD_timeaxis ) ;

      dset->taxis->type       = TIMEAXIS_TYPE ;
      dset->taxis->ntt        = user_inputs.ntt ;
      dset->taxis->ttorg      = 0.0 ;
      dset->taxis->ttdel      = user_inputs.TR ;
      dset->taxis->ttdur      = 0.0 ;
      dset->taxis->units_type = user_inputs.tunits ;  /* 21 Oct 1996 */

      if( user_inputs.tpattern != NULL ){
         dset->taxis->nsl     = daxes->nzz ;
         dset->taxis->zorg_sl = daxes->zzorg ;
         dset->taxis->dz_sl   = daxes->zzdel ;
         dset->taxis->toff_sl = user_inputs.tpattern ;
      } else {
         dset->taxis->nsl     = 0 ;
         dset->taxis->zorg_sl = 0.0 ;
         dset->taxis->dz_sl   = 0.0 ;
         dset->taxis->toff_sl = NULL ;
      }
   }

   /*-- get the dataset statistics --*/

   if( dset->taxis == NULL )
      bigfile = (daxes->nxx * daxes->nyy * daxes->nzz > 999999) ;
   else
      bigfile = (daxes->nxx * daxes->nyy * daxes->nzz * dset->taxis->ntt > 999999) ;

   if( wset.topshell != NULL && bigfile ){
      wmsg = MCW_popup_message( wset.save_file_pb ,
                                   "***************\n"
                                   "*  Computing  *\n"
                                   "*  dataset    *\n"
                                   "*  statistics *\n"
                                   "***************" ,
                                MCW_CALLER_KILL ) ;
      MCW_set_widget_cursor( wmsg , -XC_watch ) ;
      XFlush( XtDisplay(wmsg) ) ;
   } else {
      wmsg = NULL ;
   }

   THD_load_statistics( dset ) ;

   if( wmsg != NULL ) XtDestroyWidget( wmsg ) ;

   /*-- maybe some Talairach markers, too --*/

   if( dset->type      == HEAD_ANAT_TYPE     &&
       dset->view_type == VIEW_ORIGINAL_TYPE &&
       DSET_NUM_TIMES(dset) == 1                ){  /* no markers on 3D+t datasets! */

      markers = dset->markers = myXtNew( THD_marker_set ) ;
      markers->numdef = 0 ;

      for( ii=0 ; ii < MARKS_MAXNUM ; ii++ ){       /* null all data out */
         markers->valid[ii] = 0 ;
         for( jj=0 ; jj < MARKS_MAXLAB  ; jj++ )
            markers->label[ii][jj] = '\0';
         for( jj=0 ; jj < MARKS_MAXHELP ; jj++ )
            markers->help[ii][jj]  = '\0';
      }

      for( ii=0 ; ii < NMARK_ALIGN ; ii++ ){       /* copy strings in */
         MCW_strncpy( &(markers->label[ii][0]) ,
                      THD_align_label[ii] , MARKS_MAXLAB ) ;
         MCW_strncpy( &(markers->help[ii][0]) ,
                      THD_align_help[ii] , MARKS_MAXHELP ) ;
      }

      for( ii=0 ; ii < MARKS_MAXFLAG ; ii++ )     /* copy flags in */
         markers->aflags[ii] = THD_align_aflags[ii] ;

   } /* end of markers (for HEAD_ANAT_TYPE) */

QQQ("save_file9");

   /*----- actually write the dataset out! -----*/

   if( wset.topshell != NULL && bigfile ){
      wmsg = MCW_popup_message( wset.save_file_pb ,
                                   "********************\n"
                                   "*  Please wait for *\n"
                                   "*  disk operation  *\n"
                                   "********************" ,
                                MCW_CALLER_KILL ) ;
      MCW_set_widget_cursor( wmsg , -XC_watch ) ;
      XFlush( XtDisplay(wmsg) ) ;
   } else {
      wmsg = NULL ;
   }

#ifndef OMIT_DATASET_IDCODES
   /** make up a new idcode for this new output dataset **/

   dset->idcode = MCW_new_idcode() ;
#endif

   good = THD_write_3dim_dataset( user_inputs.session_filename ,
                                  user_inputs.output_filename , dset , True ) ;

   if( wmsg != NULL ) XtDestroyWidget( wmsg ) ;

   if( !good ) T3D_poperr("*******************\n\n" ,
                          "Some error occurred\n"
                          "while trying to write file") ;

   else if( wset.topshell != NULL )
      wmsg = MCW_popup_message( wset.save_file_pb ,
                                 "*********************\n"
                                 "*  Dataset written  *\n"
                                 "*  out to disk.     *\n"
                                 "*********************" ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;

   return ;
}

/*-------------------------------------------------------------------
   Just to be safe, load the data structure from the widgets
---------------------------------------------------------------------*/

void T3D_widgets_to_data(void)
{
   char * str ;

   if( wset.topshell == NULL ) return ;

   user_inputs.xorient = wset.xorient_av->ival ;
   user_inputs.yorient = wset.yorient_av->ival ;
   user_inputs.zorient = wset.zorient_av->ival ;

   user_inputs.voxshape     = MCW_val_bbox( wset.voxshape_bbox ) ;
#ifdef ALLOW_NONCONTIG
   user_inputs.voxcontig    = MCW_val_bbox( wset.voxcontig_bbox ) ;
#endif
   user_inputs.xyz_centered = MCW_val_bbox( wset.centered_bbox ) ;

   user_inputs.dataset_type  = wset.dataset_type_av->ival ;
   user_inputs.function_type = wset.function_type_av->ival ;
   user_inputs.anatomy_type  = wset.anatomy_type_av->ival ;

   MCW_strncpy( user_inputs.dataset_type_string ,
                DATASET_typestr[user_inputs.dataset_type] ,
                THD_MAX_NAME ) ;

   MCW_strncpy( user_inputs.function_type_string ,
                FUNC_typestr[user_inputs.function_type] ,
                THD_MAX_NAME ) ;

   MCW_strncpy( user_inputs.anatomy_type_string ,
                ANAT_typestr[user_inputs.anatomy_type] ,
                THD_MAX_NAME ) ;

   user_inputs.fov      = wset.fov_av->fval ;
   user_inputs.xsize    = wset.xsize_av->fval ;
   user_inputs.ysize    = wset.ysize_av->fval ;
   user_inputs.zsize    = wset.zsize_av->fval ;
#ifdef ALLOW_NONCONTIG
   user_inputs.zspacing = wset.zspacing_av->fval ;
#endif
   user_inputs.xorigin  = wset.xorigin_av->fval ;
   user_inputs.yorigin  = wset.yorigin_av->fval ;
   user_inputs.zorigin  = wset.zorigin_av->fval ;

#ifndef NO_NAMES
   str = XmTextFieldGetString( wset.dataset_name_textfield ) ;
   MCW_strncpy( user_inputs.dataset_name, str, THD_MAX_NAME ) ;
   myXtFree(str) ;

   str = XmTextFieldGetString( wset.short_label1_textfield ) ;
   MCW_strncpy( user_inputs.short_label1, str, THD_MAX_LABEL ) ;
   myXtFree(str) ;
#endif

   str = XmTextFieldGetString( wset.geometry_parent_textfield ) ;
   MCW_strncpy( user_inputs.geometry_parent_filename, str, THD_MAX_NAME ) ;
   myXtFree(str) ;

   str = XmTextFieldGetString( wset.anatomy_parent_textfield ) ;
   MCW_strncpy( user_inputs.anatomy_parent_filename, str, THD_MAX_NAME ) ;
   myXtFree(str) ;

   str = XmTextFieldGetString( wset.output_file_textfield ) ;
   MCW_strncpy( user_inputs.output_filename, str, THD_MAX_NAME ) ;
   myXtFree(str) ;

   str = XmTextFieldGetString( wset.session_file_textfield ) ;
   MCW_strncpy( user_inputs.session_filename, str, THD_MAX_NAME ) ;
   myXtFree(str) ;

#ifndef NO_NAMES
   str = XmTextFieldGetString( wset.geometry_dataname_textfield ) ;
   MCW_strncpy( user_inputs.geometry_dataname , str, THD_MAX_NAME ) ;
   myXtFree(str) ;

   str = XmTextFieldGetString( wset.anatomy_dataname_textfield ) ;
   MCW_strncpy( user_inputs.anatomy_dataname , str, THD_MAX_NAME ) ;
   myXtFree(str) ;
#endif

   if( user_inputs.need_stat_aux ){
      int ii ;
      char * endptr , * sstr ;

      for( ii=0 ; ii < MAX_STAT_AUX ; ii++ )
         user_inputs.stat_aux[ii] = 0.0 ;

      sstr = XmTextFieldGetString( wset.stat_aux_textfield ) ;
      if( sstr != NULL ){

         /** scan thru string, converting to floats **/

         str = sstr ;
         ii  = 0 ;
         do{
            /* skip ahead over whitespace or commas */

            while( *str != '\0' && ( isspace(*str) || *str == ',' ) ) str++ ;
            if( *str == '\0' ) break ;  /* end of scanning */

            user_inputs.stat_aux[ii++] = strtod(str,&endptr) ;

            if( endptr == str || *endptr == '\0' ) break ; /* end of scanning */

            str = endptr ; /* otherwise, move to next location */
         } while ( ii < MAX_STAT_AUX ) ;
         myXtFree(sstr) ;
      }
   }

   return ;
}

/*-------------------------------------------------------------------
   Load the widgets from the data structure
---------------------------------------------------------------------*/

void T3D_data_to_widgets(void)
{
   if( wset.topshell == NULL ) return ;  /* no widgets! */

QQQ("T3D_data_to_widgets: entry") ;

   AV_assign_ival( wset.xorient_av , user_inputs.xorient ) ;
   AV_assign_ival( wset.yorient_av , user_inputs.yorient ) ;
   AV_assign_ival( wset.zorient_av , user_inputs.zorient ) ;

QQQ("  -- orient done") ;

   MCW_set_bbox( wset.voxshape_bbox  , user_inputs.voxshape     ) ;
#ifdef ALLOW_NONCONTIG
   MCW_set_bbox( wset.voxcontig_bbox , user_inputs.voxcontig    ) ;
#endif
   MCW_set_bbox( wset.centered_bbox  , user_inputs.xyz_centered ) ;

QQQ("  -- bbox done") ;

   AV_assign_ival( wset.dataset_type_av  , user_inputs.dataset_type ) ;
   AV_assign_ival( wset.function_type_av , user_inputs.function_type ) ;
   AV_assign_ival( wset.anatomy_type_av , user_inputs.anatomy_type ) ;

   AV_assign_fval( wset.fov_av       , user_inputs.fov ) ;
   AV_assign_fval( wset.xsize_av     , user_inputs.xsize ) ;
   AV_assign_fval( wset.ysize_av     , user_inputs.ysize ) ;
   AV_assign_fval( wset.zsize_av     , user_inputs.zsize ) ;
#ifdef ALLOW_NONCONTIG
   AV_assign_fval( wset.zspacing_av  , user_inputs.zspacing ) ;
#endif
   AV_assign_fval( wset.xorigin_av   , user_inputs.xorigin ) ;
   AV_assign_fval( wset.yorigin_av   , user_inputs.yorigin ) ;
   AV_assign_fval( wset.zorigin_av   , user_inputs.zorigin ) ;

   SET_ORIGIN_LABEL(wset.xorigin_label,user_inputs.xorient) ;
   SET_ORIGIN_LABEL(wset.yorigin_label,user_inputs.yorient) ;
   SET_ORIGIN_LABEL(wset.zorigin_label,user_inputs.zorient) ;

QQQ("  -- aval done") ;

   T3D_voxshape_CB (NULL,NULL,NULL) ;  /* take actions based on */
#ifdef ALLOW_NONCONTIG
   T3D_voxcontig_CB(NULL,NULL,NULL) ;  /* settings of these toggles */
#endif
   T3D_centered_CB (NULL,NULL,NULL) ;

   T3D_type_av_CB( wset.dataset_type_av , NULL ) ;  /* set dataset type */

QQQ("  -- CB done") ;

#ifndef NO_NAMES
   XmTextFieldSetString( wset.dataset_name_textfield ,
                         user_inputs.dataset_name ) ;

   XmTextFieldSetString( wset.short_label1_textfield ,
                         user_inputs.short_label1 ) ;
#endif

   XmTextFieldSetString( wset.geometry_parent_textfield ,
                         user_inputs.geometry_parent_filename ) ;

   XmTextFieldSetString( wset.anatomy_parent_textfield ,
                         user_inputs.anatomy_parent_filename ) ;

   XmTextFieldSetString( wset.output_file_textfield ,
                         user_inputs.output_filename ) ;

   XmTextFieldSetString( wset.session_file_textfield ,
                         user_inputs.session_filename ) ;

#ifndef NO_NAMES
   XmTextFieldSetString( wset.geometry_dataname_textfield ,
                         user_inputs.geometry_dataname ) ;

   XmTextFieldSetString( wset.anatomy_dataname_textfield ,
                         user_inputs.anatomy_dataname ) ;
#endif

   /* April 1996: stat_aux data fields */

   T3D_setup_stat_aux() ;

   { int ii,num,bl ; char buf[256] ;

     for( ii=MAX_STAT_AUX-1 ; ii>=0 && user_inputs.stat_aux[ii]==0.0 ; ii-- ) ; /* nada */
     num = ii+1 ;

     if( num > 0 ){
       buf[0] = '\0' ;
       for( ii=0 ; ii < num ; ii++ ){
          bl = strlen(buf) ; if( bl+12 > 256 ) break ;
          sprintf( buf+bl , "%g " , user_inputs.stat_aux[ii] ) ;
       }
       XmTextFieldSetString( wset.stat_aux_textfield , buf ) ;
     }
   }

QQQ("  -- textfield done") ;
   return ;
}

/*-----------------------------------------------------------
  check data in user_inputs for legality
-------------------------------------------------------------*/

#define OUTERR "*** ILLEGAL INPUTS (cannot save) ***\n\n"

Boolean T3D_check_data( Boolean perr )
{
   char xlab,ylab,zlab ;
   int ii , ll , ll_out , ll_sess ;
   Boolean good = True , isfunc ;
   char new_name[THD_MAX_NAME] ;

   /*-- check that orientations are legal --*/

   xlab = ORIENT_xyz[user_inputs.xorient] ;
   ylab = ORIENT_xyz[user_inputs.yorient] ;
   zlab = ORIENT_xyz[user_inputs.zorient] ;

   if( xlab == ylab || xlab == zlab || ylab == zlab ){
      if(perr)T3D_poperr( OUTERR , "Axes orientations are not consistent!" ) ;
      good = False ;
   }

   /*-- check that the output filename is acceptable --*/

   ll_out = ll = strlen( user_inputs.output_filename ) ;
   if( ll == 0 ){
      if(perr)T3D_poperr( OUTERR , "No output filename provided!" ) ;
      good = False ;
   } else {
      for( ii=0 ; ii < ll ; ii++ )
         if( iscntrl(user_inputs.output_filename[ii]) ||
             isspace(user_inputs.output_filename[ii]) ||
             user_inputs.output_filename[ii] == '/'     ) break ;

      if( ii < ll ){
         if(perr)T3D_poperr( OUTERR ,
                             "Output filename contains illegal character!" ) ;
         good = False ;
      }
   }

   /*-- check that the session filename is acceptable --*/

   ll_sess = ll = strlen( user_inputs.session_filename ) ;
   if( ll == 0 ){
      if(perr)T3D_poperr( OUTERR , "No session directory name provided!" ) ;
      good = False ;
   } else {
      for( ii=0 ; ii < ll ; ii++ )
         if( iscntrl(user_inputs.session_filename[ii]) ||
             isspace(user_inputs.session_filename[ii])   ) break ;

      if( ii < ll ){
         if(perr)T3D_poperr( OUTERR ,
                             "Session filename contains illegal character!" ) ;
         good = False ;
      }
   }

   /* Check if the file already exists */

   if( ll_sess > 0 && ll_out > 0 ){
      PATH_CONCAT( new_name , user_inputs.session_filename ,
                              user_inputs.output_filename   ) ;
      strcat(new_name , "+orig." DATASET_HEADER_SUFFIX ) ;
      ll = THD_is_file( new_name ) || THD_is_directory( new_name ) ;
      if( ll ){
       if(perr) T3D_poperr( OUTERR , "Output file already exists!" ) ;
       good = False ;
      }
   }

   /*-- if the image type is functional, check
        that the anatomy parent dataset name has been set --*/

   isfunc = ISFUNCTYPE(user_inputs.dataset_type) ;

#ifdef REQUIRE_ANAT_PARENT
   if( isfunc && strlen(user_inputs.anatomy_dataname) == 0 ){
      if(perr)T3D_poperr( OUTERR , "Anatomy parent not properly set!" ) ;
      good = False ;
   }
#endif

   /*-- check for good data types in the bricks --*/

   if( isfunc ){
      if( ! AFNI_GOOD_FUNC_DTYPE(argopt.datum_all) ){
         if(perr)T3D_poperr(OUTERR , "Illegal functional datum type!" ) ;
         good = False ;
      }
   } else {
      if( ! AFNI_GOOD_DTYPE(argopt.datum_all) ){
         if(perr)T3D_poperr(OUTERR , "Illegal anatomical datum type!" ) ;
         good = False ;
      }
   }

   /*-- check if a dataset name has been defined --*/

#ifndef NO_NAMES
   if( strlen(user_inputs.dataset_name) == 0 ){
      if(perr)T3D_poperr( OUTERR,"You **MUST** supply a name for the dataset!" ) ;
      good = False ;
   }
#endif

   /*-- check if the stat_aux parameters are good --*/

   T3D_setup_stat_aux() ;
   if( user_inputs.need_stat_aux ){

      for( ii=0 ; ii < FUNC_need_stat_aux[user_inputs.function_type] ; ii++ )
         if( user_inputs.stat_aux[ii] <= 0.0 ) break ;

      if( ii < FUNC_need_stat_aux[user_inputs.function_type] ){
         if(perr)T3D_poperr(OUTERR , "Invalid statistical parameters!" ) ;
         good = False ;
      }
   }

   /*-- return the status we found --*/

   return good ;
}

/*----------------------------------------------------------------*/

void T3D_poperr( char * prefix_msg , char * msg )
{
   static char * total_msg = NULL ;
   static int    len_total = 0 ;
   int len_needed ;

   len_needed = strlen(prefix_msg) + strlen(msg) + 2 ;
   if( len_needed > len_total ){
      total_msg = XtRealloc( total_msg , len_needed ) ;
      len_total = len_needed ;
   }
   strcpy( total_msg , prefix_msg ) ;
   strcat( total_msg , msg ) ;

   if( wset.topshell != NULL ){
      (void) MCW_popup_message( wset.action_frame,
                                total_msg, MCW_USER_KILL | MCW_TIMER_KILL ) ;
      XFlush( XtDisplay(wset.topshell) ) ;
      sleep(1) ;
   } else {
      fprintf(stderr,"%s\n",total_msg) ;
   }
   return ;
}

/*-----------------------------------------------------------------
   Simulate an activate callback when the pointer leaves a widget
-------------------------------------------------------------------*/

void T3D_pointer_leave_EV( Widget w , XtPointer client_data ,
                           XEvent * ev , Boolean * continue_to_dispatch )
{
   XLeaveWindowEvent * lev = (XLeaveWindowEvent *) ev ;
   XmAnyCallbackStruct cbs ;

   if( lev->type != LeaveNotify || w == NULL ) return ;

   cbs.reason = XmCR_ACTIVATE ;  /* simulate a return press */
   cbs.event  = ev ;
   XtCallCallbacks( w , XmNactivateCallback , &cbs ) ;
}

/*---------------------------------------------------------------
   Geometry parent name has been specified, so check it out
-----------------------------------------------------------------*/

#define DEBLANK(str) \
   { int iq ;        \
     for( iq=strlen(str)-1; iq >= 0 && str[iq]==' '; iq-- )str[iq]='\0'; \
   }

#define INERR "*** DATASET READ ERROR ***\n\n"

void T3D_geometry_parent_CB( Widget w ,
                             XtPointer client_data , XtPointer call_data )
{
   static char * old_name = NULL ;
   char * new_name ;
   float size ;
   char new_path[THD_MAX_NAME] , new_pref[THD_MAX_NAME] ;
   THD_3dim_dataset * geom_dset ;
   THD_dataxes      * geom_daxes ;

   if( old_name == NULL ) old_name = XtNewString("Elvis Lives!!!") ;

#ifdef AFNI_DEBUG
printf("T3D_geometry_parent_CB: entry\n") ; fflush(stdout) ;
#endif

   if( w != NULL ){
      new_name = XmTextFieldGetString( w ) ;  /* get the new text */
   } else {
      new_name = XtNewString( user_inputs.geometry_parent_filename ) ;
   }
   DEBLANK(new_name) ;

#ifdef AFNI_DEBUG
printf("T3D_geometry_parent_CB: got string\n") ; fflush(stdout) ;
#endif

   if( strlen(new_name) == 0 )         { old_name = new_name ; geomparent_loaded = 0 ; return ; }
   if( strcmp(new_name,old_name) == 0 ){ myXtFree(new_name) ; geomparent_loaded = 0 ; return ; }

   /* have a new filename --> try to read dataset from it */

   myXtFree(old_name) ; old_name = new_name ;

   /* make a pathname to the geometry dataset */

   if( strstr(new_name,"/") == NULL ){
      char * sess ;
      if( w != NULL ) sess = XmTextFieldGetString(wset.session_file_textfield);
      else            sess = XtNewString( user_inputs.session_filename ) ;
      DEBLANK(sess) ;
      if( strlen(sess) > 0 ){
         PATH_CONCAT( new_path , sess , new_name ) ;
      } else {
         strcpy( new_path , new_name ) ;
      }
      myXtFree(sess) ;
   } else {
      strcpy( new_path , new_name ) ;
   }

   FILENAME_TO_PREFIX(new_path,new_pref) ;
   if( strlen(new_pref) == 0 ) strcat(new_path,"+orig") ;

   /* read dataset from this path */

   geom_dset = THD_open_one_dataset( new_path ) ;
   if( geom_dset == NULL ){
      T3D_poperr( INERR ,
                  "Cannot read 3D dataset\nin geometry parent file" ) ;
      geomparent_loaded = 0 ; return ;
   }

#ifdef AFNI_DEBUG
printf("T3D_geometry_parent_CB: got dataset\n") ; fflush(stdout) ;
#endif

   geom_daxes = geom_dset->daxes ;

   if( geom_daxes->nxx != user_inputs.nx ||
       geom_daxes->nyy != user_inputs.ny ||
       geom_daxes->nzz != user_inputs.nz   ){

       char msg[256] ;

       sprintf(msg,
                 "*** Shape mismatch!! ***\n"
                 "file   nx=%d ny=%d nz=%d\n"
                 "images nx=%d ny=%d nz=%d",
               geom_daxes->nxx,geom_daxes->nyy,geom_daxes->nzz,
               user_inputs.nx ,user_inputs.ny ,user_inputs.nz  ) ;

       T3D_poperr( INERR , msg ) ;
       THD_delete_3dim_dataset( geom_dset , False ) ;
       geomparent_loaded = 0 ; return ;
   }

#ifdef AFNI_DEBUG
printf("T3D_geometry_parent_CB: checked shape\n") ; fflush(stdout) ;
#endif

#ifdef ALLOW_NONCONTIG
   if( geom_daxes->xxskip != 0.0 || geom_daxes->yyskip != 0.0 ){
      T3D_poperr( INERR , "Nonzero skip factors for x and y!" ) ;
      THD_delete_3dim_dataset( geom_dset , False ) ;
      geomparent_loaded = 0 ; return ;
   }
#ifdef AFNI_DEBUG
printf("T3D_geometry_parent_CB: checked skips\n") ; fflush(stdout) ;
#endif
#endif

   /* at this point, the geom dataset seems OK, so copy its axes data */

   /* but first, load all data from the widgets into the user_inputs
      (this will mean that the T3D_data_to_widgets call later won't
       clobber any un-altered but currently un-saved data fields    */

   T3D_widgets_to_data() ;

#ifdef AFNI_DEBUG
printf("T3D_geometry_parent_CB: did T3D_widgets_to_data\n") ; fflush(stdout) ;
#endif

#ifdef REQUIRE_ANAT_PARENT
   if( strlen(geom_dset->anat_parent_name) > 0 ){
      MCW_strncpy( user_inputs.anatomy_dataname ,
                   geom_dset->anat_parent_name, THD_MAX_NAME ) ;
#ifndef OMIT_DATASET_IDCODES
      user_inputs.anatomy_parent_idcode = geom_dset->anat_parent_idcode ;
#endif
   }
#endif

   MCW_strncpy( user_inputs.geometry_dataname ,
                geom_dset->self_name , THD_MAX_NAME ) ;

   MCW_strncpy( user_inputs.geometry_parent_filename ,
                new_name , THD_MAX_NAME ) ;

#ifdef AFNI_DEBUG
printf("T3D_geometry_parent_CB: copied strings\n") ; fflush(stdout) ;
#endif

   user_inputs.xorient = geom_daxes->xxorient ;
   user_inputs.yorient = geom_daxes->yyorient ;
   user_inputs.zorient = geom_daxes->zzorient ;

#if 0  /* Bad stuff, replaced June 20, 1995 */
   user_inputs.xorigin = fabs(geom_daxes->xxorg) ;
   user_inputs.yorigin = fabs(geom_daxes->yyorg) ;
   user_inputs.zorigin = fabs(geom_daxes->zzorg) ;
#else
   user_inputs.xorigin = (ORIENT_sign[user_inputs.xorient] == '+')
                         ? (-geom_daxes->xxorg) : (geom_daxes->xxorg) ;
   user_inputs.yorigin = (ORIENT_sign[user_inputs.yorient] == '+')
                         ? (-geom_daxes->yyorg) : (geom_daxes->yyorg) ;
   user_inputs.zorigin = (ORIENT_sign[user_inputs.zorient] == '+')
                         ? (-geom_daxes->zzorg) : (geom_daxes->zzorg) ;
#endif

   user_inputs.xsize    = fabs(geom_daxes->xxdel) ;
   user_inputs.ysize    = fabs(geom_daxes->yydel) ;
#ifdef ALLOW_NONCONTIG
   user_inputs.zspacing = fabs(geom_daxes->zzdel) ;
   user_inputs.zsize    = user_inputs.zspacing - geom_daxes->zzskip ;
#else
   user_inputs.zsize    = fabs(geom_daxes->zzdel) ;
#endif

   /*-- set the vox*_bbox values depending on initial sizes --*/

   if( user_inputs.xsize != user_inputs.ysize ){
      user_inputs.voxshape = VOXSHAPE_IRREGULAR ;
   } else {
      if( user_inputs.xsize != user_inputs.zsize ){
         user_inputs.voxshape = VOXSHAPE_SQUARE ;
      } else {
         user_inputs.voxshape = VOXSHAPE_CUBICAL ;
      }
   }
   user_inputs.fov = user_inputs.xsize * user_inputs.nx ;

#ifdef ALLOW_NONCONTIG
   if( geom_daxes->zzskip == 0 ) user_inputs.voxcontig = VOXCONTIG_YES ;
   else                          user_inputs.voxcontig = VOXCONTIG_UNIF ;
#endif

   user_inputs.xyz_centered = 0 ;

   size = 0.5 * (user_inputs.nx-1) * user_inputs.xsize ;
   if( fabs(size - user_inputs.xorigin) < 0.01 )
      user_inputs.xyz_centered |= XCENTERED ;

   size = 0.5 * (user_inputs.ny-1) * user_inputs.ysize ;
   if( fabs(size - user_inputs.yorigin) < 0.01 )
      user_inputs.xyz_centered |= YCENTERED ;

   size = 0.5 * (user_inputs.nz-1) * user_inputs.zsize ;
   if( fabs(size - user_inputs.zorigin) < 0.01 )
      user_inputs.xyz_centered |= ZCENTERED ;

#ifdef AFNI_DEBUG
printf("T3D_geometry_parent_CB: copied geometry\n") ;
printf("  ?orient = %d %d %d\n",
       user_inputs.xorient,user_inputs.yorient,user_inputs.zorient) ;
printf("  ?origin = %f %f %f\n",
       user_inputs.xorigin,user_inputs.yorigin,user_inputs.zorigin) ;
printf("  ?size = %f %f %f\n",
       user_inputs.xsize,user_inputs.ysize,user_inputs.zsize) ;
printf("  voxshape = %d  fov = %g\n",user_inputs.voxshape,user_inputs.fov) ;
fflush(stdout) ;
#endif

   T3D_data_to_widgets() ;

#ifdef AFNI_DEBUG
printf("T3D_geometry_parent_CB: did T3D_data_to_widgets\n") ; fflush(stdout) ;
#endif

   THD_delete_3dim_dataset( geom_dset , False ) ;

   geometry_loaded   = 1 ;
   geomparent_loaded = 1 ;  return ;/* flag that load was OK */
}

/*---------------------------------------------------------------
   Anatomy parent name has been specified, so check it out
-----------------------------------------------------------------*/

void T3D_anatomy_parent_CB( Widget w ,
                            XtPointer client_data , XtPointer call_data )
{
   static char * old_name = NULL ;
   char * new_name ;
   char new_path[THD_MAX_NAME] , new_pref[THD_MAX_NAME] ;
   Boolean isfunc ;
   THD_3dim_dataset * anat_dset ;

   if( old_name == NULL ) old_name = XtNewString("Elvis Lives!!!") ;

   /* see if the new text is any different from the old one */

#ifdef AFNI_DEBUG
printf("T3D_anatomy_parent_CB: entry\n") ; fflush(stdout) ;
#endif

   if( w != NULL ){
      new_name = XmTextFieldGetString( w ) ;  /* get the new text */
   } else {
      new_name = XtNewString( user_inputs.anatomy_parent_filename ) ;
   }
   DEBLANK(new_name) ;

   if( strlen(new_name) == 0 )         { old_name = new_name ; return ; }
   if( strcmp(new_name,old_name) == 0 ){ myXtFree(new_name) ; return ; }

   /* have a new filename --> try to read dataset from it */

   myXtFree(old_name) ; old_name = new_name ;

   /* make a pathname to the anatomy dataset */

   if( strstr(new_name,"/") == NULL ){
      char * sess ;
      if( w != NULL ) sess = XmTextFieldGetString(wset.session_file_textfield);
      else            sess = XtNewString( user_inputs.session_filename ) ;
      DEBLANK(sess) ;
      if( strlen(sess) > 0 ){
         PATH_CONCAT( new_path , sess , new_name ) ;
      } else {
         strcpy( new_path , new_name ) ;
      }
      myXtFree(sess) ;
   } else {
      strcpy( new_path , new_name ) ;
   }

   FILENAME_TO_PREFIX(new_path,new_pref) ;
   if( strlen(new_pref) == 0 ) strcat(new_path,"+orig") ;

   anat_dset = THD_open_one_dataset( new_path ) ;
   if( anat_dset == NULL ){
      T3D_poperr( INERR ,
                  "Cannot read 3D dataset\nin anatomy parent file" ) ;
      return ;
   }

#ifdef AFNI_DEBUG
printf("T3D_anatomy_parent_CB: got dataset\n") ; fflush(stdout) ;
#endif

   isfunc = ISFUNC(anat_dset) ;
   if( isfunc ){
      T3D_poperr( INERR ,
                  "Anatomy parent dataset\nis actually Function data!" ) ;
      THD_delete_3dim_dataset( anat_dset , False ) ;
      return ;
   }

   /* at this point, the anat dataset seems OK, so use it */

   MCW_strncpy( user_inputs.anatomy_dataname ,
                anat_dset->self_name , THD_MAX_NAME ) ;
#ifndef OMIT_DATASET_IDCODES
   user_inputs.anatomy_parent_idcode = anat_dset->idcode ;
#endif

#ifndef NO_NAMES
   if( w != NULL ) XmTextFieldSetString( wset.anatomy_dataname_textfield ,
                                         user_inputs.anatomy_dataname ) ;
#endif

   MCW_strncpy( user_inputs.anatomy_parent_filename ,
                new_name , THD_MAX_NAME ) ;

#ifdef AFNI_DEBUG
printf("T3D_anatomy_parent_CB: set strings\n") ; fflush(stdout) ;
#endif

   THD_delete_3dim_dataset( anat_dset , False ) ;
}

/****************************************************************/
/***** June 1995: routine to load constants from X defaults *****/

#if 0
# define NAME2INT(nnn,iii,bot,top)           \
  { xdef = XGetDefault(display,"AFNI",nnn) ; \
    if( xdef != NULL ){                      \
       ival = strtol( xdef , &cpt , 10 ) ;   \
       if( *cpt == '\0' && ival >= (bot) && ival <= (top) ) (iii) = ival ; } }

# define NAME2FLOAT(nnn,fff,bot,top)         \
  { xdef = XGetDefault(display,"AFNI",nnn) ; \
    if( xdef != NULL ){                      \
       fval = strtod( xdef , &cpt ) ;        \
       if( *cpt == '\0' && fval >= (bot) && fval <= (top) ) (fff) = fval ; } }

# define NAME2STRING(nnn,sss)                \
  { xdef = XGetDefault(display,"AFNI",nnn) ; \
    if( xdef != NULL ) sss  = XtNewString(xdef) ; }
#else
# define NAME2INT(nnn,iii,bot,top)           \
  { xdef = RWC_getname(display,nnn) ;        \
    if( xdef != NULL ){                      \
       ival = strtol( xdef , &cpt , 10 ) ;   \
       if( *cpt == '\0' && ival >= (bot) && ival <= (top) ) (iii) = ival ; } }

# define NAME2FLOAT(nnn,fff,bot,top)         \
  { xdef = RWC_getname(display,nnn) ;        \
    if( xdef != NULL ){                      \
       fval = strtod( xdef , &cpt ) ;        \
       if( *cpt == '\0' && fval >= (bot) && fval <= (top) ) (fff) = fval ; } }

# define NAME2STRING(nnn,sss)                \
  { xdef = RWC_getname(display,nnn) ;        \
    if( xdef != NULL ) sss  = XtNewString(xdef) ; }
#endif

#define BAD -999

void AFNI_load_defaults( Widget w )
{
   char    * xdef ;
   Display * display ;
   int       ival ;
   float     fval ;
   char *    cpt ;

   if( w == NULL ) return ;
   display = XtDisplay( w ) ;

   /** initialize display and overlay colors **/

   NAME2INT("ncolors",INIT_ngray,3,MAX_COLORS) ;

   NAME2FLOAT("gamma",INIT_gamma,0.1,9.9) ;

   NAME2FLOAT("init_fov",INIT_fov,1.0,1000.0) ;
   INIT_fov = 0.1 * ( (int)(10*INIT_fov) ) ;

   return ;
}

/*-------------------------------------------------------------*/

void T3D_stat_aux_CB( Widget w ,
                      XtPointer client_data , XtPointer call_data )
{
   return ;
}

void T3D_setup_stat_aux(void)
{
   char lbuf[THD_MAX_NAME] ;
   Boolean needit ;

   user_inputs.need_stat_aux =
       ( ISFUNCTYPE(user_inputs.dataset_type) &&
         FUNC_need_stat_aux[user_inputs.function_type] > 0 ) ;

   if( wset.topshell != NULL ){
      needit = (Boolean) user_inputs.need_stat_aux ;

      if( needit ){
         sprintf( lbuf , "%s: statistical parameters for %s" ,
                  FUNC_label_stat_aux[user_inputs.function_type] ,
                  FUNC_typestr[user_inputs.function_type] ) ;
      } else {
         sprintf( lbuf , "Field below not applicable" ) ;
      }
      XtSetSensitive( wset.stat_aux_label , needit ) ;
      XtSetSensitive( wset.stat_aux_textfield , needit ) ;
      MCW_set_widget_label( wset.stat_aux_label , lbuf ) ;
   }

   return ;
}

#ifndef USE_OLD_DCODE
/***----------------------------------------------------------
 21 Nov 1997 [RWCox]:
   Decode a location string in one of the forms
     <number><dircode>
     <dircode><number>
     <dircode>
     <number>
   val will be WAY_BIG if the number isn't given, otherwise
     it will be the number;
   dcode will be < 0 if the dircode isn't given, otherwise
     it will be the direction code.
   The return value is the number of characters used from
     the input string.
--------------------------------------------------------------***/

int decode_location( char * str , float * val , int * dcode )
{
   char acod , * ptr , * sstr = str ;

   *val   = WAY_BIG ;
   *dcode = ILLEGAL_TYPE ;
   if( sstr == NULL || sstr[0] == '\0' ) return 0 ;

   /** see if we get a legal direction code here **/

   acod = toupper(sstr[0]) ; *dcode = ORCODE(acod) ;
   if( *dcode >= 0 ) sstr++ ;

   if( sstr[0] == '\0' ) return ((*dcode < 0) ? 0 : 1) ;

   /** get number here **/

   *val = strtod( sstr , &ptr ) ;
   if( *val == 0.0 && ptr == sstr ) return ((*dcode < 0) ? 0 : 1) ;
   sstr = ptr ;

   if( *dcode >= 0 || sstr[0] == '\0' ) return (sstr-str) ;

   acod = toupper(sstr[0]) ; *dcode = ORCODE(acod) ;
   if( *dcode >= 0 ) sstr++ ;

   return (sstr-str) ;
}
#endif /* USE_OLD_DCODE */
