#undef MAIN
#include "afni_graph.h"
#include "afni.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#ifdef AFNI_DEBUG
#  define USE_TRACING
#endif
#include "dbtrace.h"

static int show_grapher_pixmap = 1 ;

MCW_grapher * new_MCW_grapher( MCW_DC * dc , get_ptr getser , XtPointer aux )
{
   int ii ;
   MCW_grapher * grapher ;
   static int new_xsize = -1 , new_ysize = -1 ;
   char * buf , * cpt ;

ENTRY("new_MCW_grapher") ;

   grapher = myXtNew( MCW_grapher ) ;

   grapher->type   = MCW_GRAPHER_TYPE ;
   grapher->dc     = dc ;
   grapher->getser = getser ;
   grapher->getaux = aux ;
   grapher->parent = NULL ;
   grapher->valid  = 1 ;

   grapher->never_drawn = 1 ;
   grapher->button2_enabled = 0 ;  /* Feb 1998 */

   grapher->gx_max = 0 ;
   grapher->gy_max = 0 ;
   grapher->fWIDE  = 0 ;
   grapher->fHIGH  = 0 ;

   grapher->logo_pixmap = XmUNSPECIFIED_PIXMAP ;
   grapher->logo_width  = grapher->logo_height = 0 ;

   grapher->status = (MCW_grapher_status *) getser(0,graCR_getstatus,aux) ;

   if( grapher->status->num_series < 2 ){
      fprintf(stderr,"*** Attempt to create grapher with < 2 time points! ***\a\n") ;
      myXtFree(grapher) ;
      RETURN(NULL) ;
   }

   GRA_NULL_tuser(grapher) ;  /* 22 Apr 1997 */

   /** make shell that holds all **/

   grapher->fdw_graph =
      XtVaAppCreateShell(
         "AFNI" , "AFNI" , topLevelShellWidgetClass , dc->display ,
           XmNminHeight , MIN_XSIZE + GL_DLX + GR_DLX ,
           XmNmaxHeight , dc->height ,
           XmNminWidth  , MIN_YSIZE + GT_DLY + GB_DLY ,
           XmNmaxWidth  , dc->width ,
           XmNdeleteResponse   , XmDO_NOTHING ,   /* deletion handled below */
           XmNallowShellResize , False ,          /* let code resize shell */
           XmNinitialResourcesPersistent , False ,
      NULL ) ;

   DC_yokify( grapher->fdw_graph , dc ) ;  /* 14 Sep 1998 */

   /** find initial size of new graphs **/

   if( new_xsize < 0 ){
      new_xsize = GX_MAX ;
#if 0
      buf = XGetDefault(dc->display,"AFNI","graph_width") ;
#else
      buf = RWC_getname(dc->display,"graph_width") ;
#endif
      if( buf != NULL ){
         ii = strtol( buf , &cpt , 10 ) ;
         if( *cpt == '\0' || new_xsize >= MIN_XSIZE ||
                             new_xsize <= dc->width - GL_DLX - GR_DLX )
            new_xsize = ii ;
      }

      new_ysize = GY_MAX ;
#if 0
      buf = XGetDefault(dc->display,"AFNI","graph_height") ;
#else
      buf = RWC_getname(dc->display,"graph_height") ;
#endif
      if( buf != NULL ){
         ii = strtol( buf , &cpt , 10 ) ;
         if( *cpt == '\0' || new_ysize >= MIN_YSIZE ||
                             new_ysize <= dc->width - GT_DLY - GB_DLY )
            new_ysize = ii ;
      }
   }

   /** make a drawing area to get everything **/

   grapher->draw_fd =
       XtVaCreateManagedWidget(
         "dialog" , xmDrawingAreaWidgetClass , grapher->fdw_graph ,

          XmNwidth  , new_xsize + GL_DLX + GR_DLX ,
          XmNheight , new_ysize + GT_DLY + GB_DLY ,

          XmNmarginWidth  , 1 ,
          XmNmarginHeight , 1 ,

          XmNtraversalOn , False ,
          XmNinitialResourcesPersistent , False ,
       NULL ) ;

   XtInsertEventHandler( grapher->draw_fd ,     /* handle events in graphs */

                            0
                          | KeyPressMask        /* get keystrokes */
                          | ButtonPressMask     /* button presses */
                          | ExposureMask        /* exposures */
                          | StructureNotifyMask /* resizes */
                         ,
                         FALSE ,                /* nonmaskable events? */
                         GRA_drawing_EV ,       /* super-handler! */
                         (XtPointer) grapher ,  /* client data */
                         XtListTail ) ;         /* last in queue */

   MCW_register_help( grapher->draw_fd ,
                       "Button 1 in a sub-graph --> move it to center\n"
                       "Button 1 in the central --> move time index\n"
                       "              sub-graph     to closest point\n"
                       "                            on the graph\n"
                       "Shift or Ctrl keys with --> single-step time\n"
                       "Button 1 in the central     index up or down\n"
                       "              sub-graph\n"
                       "\n"
                       "The red dot in the central sub-graph shows\n"
                       "the location of the current time index.\n"
                       "\n"
                       "Button 3 in a sub-graph --> show statistics\n"
                       "                            of time series\n"
                       "\n"
                       "To turn off the MCW AFNI logo, click Button 1\n"
                       "inside the logo.\n"
                       "\n"
                       "See the 'Opt' menu for keypress actions and\n"
                       "for other options to control graph display."
                    ) ;

   /*---------------------------*/
   /*--- Button 3 popup menu ---*/
   /*---------------------------*/

   grapher->but3_menu =
      XmCreatePopupMenu( grapher->draw_fd , "menu" , NULL , 0 ) ;

   grapher->but3_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , grapher->but3_menu ,
            XmNalignment , XmALIGNMENT_BEGINNING ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*-------------------------------------*/
   /*--- RowColumn to hold all buttons ---*/
   /*-------------------------------------*/

   grapher->option_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , grapher->draw_fd ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmHORIZONTAL ,
            XmNmarginWidth , 0 ,
            XmNmarginHeight, 0 ,
            XmNspacing     , 3 ,
            XmNbackground  , grapher->dc->ovc->pixov_brightest ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*-----------------------------------*/
   /*--- Menu bar of Cascade buttons ---*/
   /*-----------------------------------*/

   grapher->option_mbar =
         XmCreateMenuBar( grapher->option_rowcol, "dialog" , NULL,0 ) ;
   XtVaSetValues( grapher->option_mbar ,
                     XmNmarginWidth  , 0 ,
                     XmNmarginHeight , 0 ,
                     XmNbackground   , grapher->dc->ovc->pixov_brightest ,
                     XmNspacing      , 3 ,
                     XmNborderWidth  , 0 ,
                     XmNtraversalOn  , False ,
                  NULL ) ;
   XtManageChild( grapher->option_mbar ) ;

   /*------------------------*/
   /*--- FIM Menu Buttons ---*/
   /*------------------------*/

   grapher->fmenu = AFNI_new_fim_menu( grapher->option_mbar , GRA_fim_CB , 1 ) ;
   grapher->fmenu->parent = (XtPointer) grapher ;

   grapher->polort = 1 ;  /* 27 May 1999 */

   /* macros to put double and single separator lines in a menu */

#define MENU_DLINE(wmenu)                                          \
   (void) XtVaCreateManagedWidget(                                 \
            "dialog" , xmSeparatorWidgetClass , grapher -> wmenu , \
             XmNseparatorType , XmDOUBLE_LINE , NULL )

#define MENU_SLINE(wmenu)                                          \
   (void) XtVaCreateManagedWidget(                                 \
            "dialog" , xmSeparatorWidgetClass , grapher -> wmenu , \
             XmNseparatorType , XmSINGLE_LINE , NULL )

   /*------------------------*/
   /*--- Opt Menu Buttons ---*/
   /*------------------------*/

   grapher->opt_menu =
         XmCreatePulldownMenu( grapher->option_mbar , "menu" , NULL,0 ) ;

   grapher->opt_cbut =
         XtVaCreateManagedWidget(
            "dialog" , xmCascadeButtonWidgetClass , grapher->option_mbar ,
               LABEL_ARG("Opt") ,
               XmNsubMenuId , grapher->opt_menu ,
               XmNmarginWidth  , 0 ,
               XmNmarginHeight , 0 ,
               XmNmarginBottom , 0 ,
               XmNmarginTop    , 0 ,
               XmNmarginRight  , 0 ,
               XmNmarginLeft   , 0 ,
               XmNtraversalOn  , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

   MCW_register_hint( grapher->opt_cbut , "Graphing options menu" ) ;

   MCW_register_help( grapher->opt_cbut ,
                      "********  Graph Display Options:  ********\n"
                      "\n"
                      "Scale       --> Change vertical scaling\n"
                      "Matrix      --> Change number of sub-graphs\n"
                      "Grid        --> Change number of grid lines;\n"
                      "                 also can Pin the number of\n"
                      "                 time points displayed.\n"
                      "Slice       --> Change slice number\n"
                      "Colors, Etc --> Change colors of various\n"
                      "                 parts of the graph window\n"
                      "Baseline    --> Display each sub-graph with\n"
                      "                 its minimum at the bottom of\n"
                      "                 its window (the default), OR\n"
                      "                 with the minimum of all sub-\n"
                      "                 graphs as the common baseline\n"
                      "Save PNM    --> Save the graph window as an\n"
                      "                 image to a PNM format file\n"
                      "Write Center--> Central voxel timeseries will\n"
                      "                 be written to a file with a\n"
                      "                 name like 'X_Y_Z.suffix.1D'\n"
                      "                 where X,Y,Z are voxel indexes\n"
                      "Tran 0D     --> Choose a function to apply to\n"
                      "                 each point in each timeseries\n"
                      "Tran 1D     --> Choose a function to apply to\n"
                      "                 the timeseries as a whole\n"
                      "Double Plot --> If 'Tran 1D' is active, then\n"
                      "                 plot the data timeseries AND\n"
                      "                 the transformed timeseries\n"
                      "Done        --> Close this graphing window\n"
                      "\n"
                      "The keystrokes indicated in the menus will\n"
                      "carry out the same functions, if pressed\n"
                      "when the cursor focus is in the graph window.\n"
                      "\n"
                      "N.B.: keystrokes without menu items are:\n"
                      " <  -->  move time index down by 1\n"
                      " >  -->  move time index up by 1\n"
                      " 1  -->  move to first image (time index 0)\n"
                      " l  -->  move to last image in time series\n"
                      " L  -->  turn off the AFNI logo in the corner"
                    ) ;

   /** macro to create a new opt menu button **/

#define OPT_MENU_BUT(wname,label,hhh)                                \
   grapher -> wname =                                                \
         XtVaCreateManagedWidget(                                    \
            "dialog" , xmPushButtonWidgetClass , grapher->opt_menu , \
               LABEL_ARG( label ) ,                                  \
               XmNmarginHeight , 0 ,                                 \
               XmNtraversalOn , False ,                              \
               XmNinitialResourcesPersistent , False ,               \
            NULL ) ;                                                 \
      XtAddCallback( grapher -> wname , XmNactivateCallback ,        \
                     GRA_opt_CB , (XtPointer) grapher ) ;            \
      MCW_register_hint( grapher -> wname , hhh ) ;

   /** macro to create a new opt pullright menu **/
   /** 07 Jan 1999: added the mapCallback to fix position **/

#define OPT_MENU_PULLRIGHT(wmenu,wcbut,label,hhh)                      \
   grapher -> wmenu =                                                  \
     XmCreatePulldownMenu( grapher->opt_menu , "menu" , NULL , 0 ) ;   \
   grapher -> wcbut =                                                  \
     XtVaCreateManagedWidget(                                          \
       "dialog" , xmCascadeButtonWidgetClass , grapher->opt_menu ,     \
          LABEL_ARG( label ) ,                                         \
          XmNsubMenuId , grapher -> wmenu ,                            \
          XmNtraversalOn , False ,                                     \
          XmNinitialResourcesPersistent , False ,                      \
       NULL ) ;                                                        \
   MCW_register_hint( grapher -> wcbut , hhh ) ;                       \
   XtAddCallback( grapher -> wmenu, XmNmapCallback, GRA_mapmenu_CB, NULL ) ;

   /** macro to create a new button on a pullright menu **/

#define OPT_MENU_PULL_BUT(wmenu,wname,label,hhh)                    \
   grapher -> wname =                                               \
         XtVaCreateManagedWidget(                                   \
            "dialog" , xmPushButtonWidgetClass , grapher -> wmenu , \
               LABEL_ARG( label ) ,                                 \
               XmNmarginHeight , 0 ,                                \
               XmNtraversalOn , False ,                             \
               XmNinitialResourcesPersistent , False ,              \
            NULL ) ;                                                \
      XtAddCallback( grapher -> wname , XmNactivateCallback ,       \
                     GRA_opt_CB , (XtPointer) grapher ) ;           \
   MCW_register_hint( grapher -> wname , hhh ) ;

#ifdef USE_OPTMENUS
   /** macro to create an option menu on a pullright menu
       (this menu must be fixed up later in GRA_fix_optmenus) **/

#define OPT_MENU_OPTMENU(wmenu,wname,label,cb,hhh)                  \
   grapher -> wname =                                               \
      new_MCW_optmenu( grapher -> wmenu , label , 0,1,0,0 ,         \
                       cb , (XtPointer) grapher , NULL , NULL ) ;   \
   MCW_reghint_children( grapher -> wname -> wrowcol , hhh ) ;

#endif /* USE_OPTMENUS */

   /*** top of menu = a label to click on that does nothing at all ***/

   (void) XtVaCreateManagedWidget(
            "dialog" , xmLabelWidgetClass , grapher->opt_menu ,
               LABEL_ARG("--- Cancel ---") ,
               XmNrecomputeSize , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

   MENU_SLINE(opt_menu) ;

   OPT_MENU_PULLRIGHT(opt_scale_menu,opt_scale_cbut     ,"Scale"   ,"Change vertical scale" );
   OPT_MENU_PULL_BUT(opt_scale_menu,opt_scale_down_pb  ,"Down [-]","Shrink graph heights"  );
   OPT_MENU_PULL_BUT(opt_scale_menu,opt_scale_up_pb    ,"Up   [+]","Increase graph heights");
   OPT_MENU_PULL_BUT(opt_scale_menu,opt_scale_choose_pb,"Choose"  ,"Set vertical scale"    );
   OPT_MENU_PULL_BUT(opt_scale_menu,opt_scale_auto_pb  ,"Auto [a]","Scale automatically"   );

   OPT_MENU_PULLRIGHT(opt_mat_menu,opt_mat_cbut      ,"Matrix"  , "Change number of graphs"   ) ;
   OPT_MENU_PULL_BUT( opt_mat_menu,opt_mat_down_pb   ,"Down [m]", "Reduce number of graphs"   ) ;
   OPT_MENU_PULL_BUT( opt_mat_menu,opt_mat_up_pb     ,"Up   [M]", "Increase number of graphs" ) ;
#ifdef USE_OPTMENUS
   OPT_MENU_OPTMENU( opt_mat_menu,opt_mat_choose_av , "# " , GRA_mat_choose_CB , "Set number of graphs" ) ;
#else
   OPT_MENU_PULL_BUT( opt_mat_menu,opt_mat_choose_pb ,"Choose" , "Set number of graphs" ) ;
#endif

   OPT_MENU_PULLRIGHT(opt_grid_menu,opt_grid_cbut     ,"Grid"    , "Change vertical grid spacing" ) ;
   OPT_MENU_PULL_BUT( opt_grid_menu,opt_grid_down_pb  ,"Down [g]", "Reduce vertical grid spacing" ) ;
   OPT_MENU_PULL_BUT( opt_grid_menu,opt_grid_up_pb    ,"Up   [G]", "Increase vertical grid spacing" ) ;
   OPT_MENU_PULL_BUT( opt_grid_menu,opt_grid_choose_pb,"Choose"  , "Set vertical grid spacing" ) ;
   OPT_MENU_PULL_BUT( opt_grid_menu,opt_pin_choose_pb ,"Pin Num" , "Fix length of graph window" ) ;  /* 27 Apr 1997 */
   OPT_MENU_PULL_BUT( opt_grid_menu,opt_grid_HorZ_pb  ,"HorZ [h]", "Horizontal line at Zero" ) ; /* 05 Jan 1999 */

   OPT_MENU_PULLRIGHT(opt_slice_menu,opt_slice_cbut      ,"Slice"   , "Change slice"  ) ;
   OPT_MENU_PULL_BUT( opt_slice_menu,opt_slice_down_pb   ,"Down [z]", "Decrement slice" ) ;
   OPT_MENU_PULL_BUT( opt_slice_menu,opt_slice_up_pb     ,"Up   [Z]", "Increment slice" ) ;
#ifdef USE_OPTMENUS
   OPT_MENU_OPTMENU( opt_slice_menu,opt_slice_choose_av , "# " , GRA_slice_choose_CB , "Set slice" ) ;
#else
   OPT_MENU_PULL_BUT( opt_slice_menu,opt_slice_choose_pb ,"Choose" , "Set slice" ) ;
#endif

   /***** 16 June 1997: Colors submenu *****/

   { static char * bbox_label[1] = { "Use Thick Lines" } ;
     static char * pts_label[2]  = { "Graph Points" , "Points+Lines" } ;
     char     toplabel[64] ;
     XmString xstr ;

     OPT_MENU_PULLRIGHT(opt_colors_menu,opt_colors_cbut,"Colors, Etc.","Change graph appearance");

     if( strlen(grapher->status->namecode) > 0 ){

        sprintf( toplabel , "--- %s ---" , grapher->status->namecode ) ;
        xstr = XmStringCreateLtoR( toplabel , XmFONTLIST_DEFAULT_TAG ) ;

        (void) XtVaCreateManagedWidget(
                 "dialog" , xmLabelWidgetClass , grapher->opt_colors_menu ,
                    XmNlabelString , xstr ,
                    XmNrecomputeSize , False ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;

        XmStringFree( xstr ) ;

        MENU_DLINE(opt_colors_menu) ;
     }

     if( gr_setup_default ){
        gr_color_default[0] = INIT_GR_boxes_color  ;
        gr_color_default[1] = INIT_GR_backg_color  ;
        gr_color_default[2] = INIT_GR_grid_color   ;
        gr_color_default[3] = INIT_GR_text_color   ;
        gr_color_default[4] = INIT_GR_data_color   ;
        gr_color_default[5] = INIT_GR_ideal_color  ;
        gr_color_default[6] = INIT_GR_ort_color    ;
        gr_color_default[7] = INIT_GR_ignore_color ;
        gr_color_default[8] = INIT_GR_dplot_color  ;

        gr_thick_default[0] = INIT_GR_boxes_thick  ;
        gr_thick_default[1] = -1  ;
        gr_thick_default[2] = INIT_GR_grid_thick   ;
        gr_thick_default[3] = -1   ;
        gr_thick_default[4] = INIT_GR_data_thick   ;
        gr_thick_default[5] = INIT_GR_ideal_thick  ;
        gr_thick_default[6] = INIT_GR_ort_thick    ;
        gr_thick_default[7] = -1 ;
        gr_thick_default[8] = INIT_GR_dplot_thick  ;

        gr_setup_default = 0 ;
     }

     for( ii=0 ; ii < NUM_COLOR_ITEMS ; ii++ ){

        grapher->color_index[ii] = GRA_COLOR(gr_color_default[ii]) ;
        grapher->thick_index[ii] = gr_thick_default[ii] ;
        grapher->points_index[ii]= gr_points_default[ii] ;  /* 09 Jan 1998 */

        grapher->opt_color_av[ii] =
           new_MCW_colormenu( grapher->opt_colors_menu ,
                              gr_color_label[ii] ,
                              grapher->dc ,
                              gr_color_start[ii] , grapher->dc->ovc->ncol_ov - 1 ,
                              grapher->color_index[ii] ,
                              GRA_color_CB , (XtPointer) grapher ) ;

        if( grapher->thick_index[ii] >= 0 ){
           grapher->opt_thick_bbox[ii] =
              new_MCW_bbox( grapher->opt_colors_menu ,
                            1 , bbox_label , MCW_BB_check , MCW_BB_noframe ,
                            GRA_thick_CB , (XtPointer) grapher ) ;

           if( grapher->thick_index[ii] )
              MCW_set_bbox( grapher->opt_thick_bbox[ii] , 1 ) ;
        } else {
           grapher->opt_thick_bbox[ii] = NULL ;
        }

        /* 09 Jan 1998: add option to draw only points in graphs */
        /* 01 Aug 1998: allow points+lines to be drawn as well   */

        if( grapher->points_index[ii] >= 0 ){
           grapher->opt_points_bbox[ii] =
              new_MCW_bbox( grapher->opt_colors_menu ,
                            2 , pts_label , MCW_BB_radio_zero , MCW_BB_noframe ,
                            GRA_thick_CB , (XtPointer) grapher ) ;

           if( grapher->points_index[ii] )
              MCW_set_bbox( grapher->opt_points_bbox[ii] ,
                            1 << (grapher->points_index[ii]-1) ) ;
        } else {
           grapher->opt_points_bbox[ii] = NULL ;
        }

        MENU_DLINE( opt_colors_menu ) ;
     }

     /* 12 Jan 1998: control gap between graphs */

     grapher->opt_ggap_av =
        new_MCW_optmenu( grapher->opt_colors_menu , "Graph Gap" ,
                         0 , 19 , INIT_GR_ggap , 0 ,
                         GRA_ggap_CB , (XtPointer) grapher , NULL , NULL ) ;
     AVOPT_columnize( grapher->opt_ggap_av , 4 ) ;

   }
   /***** end colors submenu creation *****/

#if 0
   OPT_MENU_BUT(opt_color_up_pb     ,"Grid Color   [r]" , "Rotate grid color" ) ;
#endif

   OPT_MENU_BUT(opt_baseline_pb     ,"Baseline     [b]" , "Common graph baseline?" ) ;

   MENU_SLINE(opt_menu) ;
   OPT_MENU_BUT(opt_save_pb         ,"Save PNM     [S]" , "Save graph as an image" ) ;

   MENU_SLINE(opt_menu) ;
   OPT_MENU_BUT(opt_write_center_pb ,"Write Center [w]" , "Write central graph as a *.1D file" ) ;
   OPT_MENU_BUT(opt_write_suffix_pb ,"Set 'w' Suffix"   , "Set suffix for graph writing" ) ;

   /*-------------------------------------------*/
   /*--- Arrowval to list 0D transformations ---*/
   /*-------------------------------------------*/

#define COLSIZE 20

   if( grapher->status->transforms0D != NULL &&
       grapher->status->transforms0D->num > 0  ){  /* 22 Oct 1996 */

      MENU_DLINE(opt_menu) ;

      grapher->transform0D_av =
         new_MCW_optmenu( grapher->opt_menu ,
                          "Tran 0D" ,
                          0 , grapher->status->transforms0D->num , 0 , 0 ,
                          GRA_transform_CB , (XtPointer) grapher ,
                          GRA_transform_label , (XtPointer) grapher->status->transforms0D ) ;

      if( grapher->status->transforms0D->num >= COLSIZE )
         AVOPT_columnize( grapher->transform0D_av ,
                          (grapher->status->transforms0D->num/COLSIZE)+1 ) ;

      MCW_reghint_children( grapher->transform0D_av->wrowcol ,
                            "Pointwise data transformations" ) ;

   } else {
      grapher->transform0D_av = NULL ;
   }
   grapher->transform0D_func  = NULL ;  /* no function to start with */
   grapher->transform0D_index = 0 ;

   /*-------------------------------------------*/
   /*--- Arrowval to list 1D transformations ---*/
   /*-------------------------------------------*/

   if( grapher->status->transforms1D != NULL &&
       grapher->status->transforms1D->num > 0  ){  /* 03 Nov 1996 */

      static char * bbox_label[1] = { "Double Plot" } ;

      MENU_DLINE(opt_menu) ;

      grapher->transform1D_av =
         new_MCW_optmenu( grapher->opt_menu ,
                          "Tran 1D" ,
                          0 , grapher->status->transforms1D->num , 0 , 0 ,
                          GRA_transform_CB , (XtPointer) grapher ,
                          GRA_transform_label , (XtPointer) grapher->status->transforms1D ) ;

      if( grapher->status->transforms1D->num >= COLSIZE )
         AVOPT_columnize( grapher->transform1D_av ,
                          (grapher->status->transforms1D->num/COLSIZE)+1 ) ;

      MCW_reghint_children( grapher->transform1D_av->wrowcol ,
                            "Time series transformations" ) ;

      /* 08 Nov 1996: dplot = double plot */

      grapher->transform1D_dplot_bbox =
         new_MCW_bbox( grapher->opt_menu ,
                       1 , bbox_label , MCW_BB_check , MCW_BB_noframe ,
                       GRA_dplot_change_CB , (XtPointer)grapher ) ;

      MCW_reghint_children( grapher->transform1D_dplot_bbox->wrowcol ,
                            "Plot data AND transform?" ) ;

   } else {
      grapher->transform1D_av = NULL ;
      grapher->transform1D_dplot_bbox = NULL ;
   }
   grapher->transform1D_func  = NULL ;  /* no function to start with */
   grapher->transform1D_index = 0 ;

   /*------ menu to control the x-axis drawing (09 Jan 1998) ------*/

   MENU_DLINE(opt_menu) ;

   OPT_MENU_PULLRIGHT( opt_xaxis_menu , opt_xaxis_cbut    , "X-axis" , "Alter x-axis" ) ;
   OPT_MENU_PULL_BUT(  opt_xaxis_menu , opt_xaxis_pick_pb ,
                       "Pick x-axis" , "Set timeseries for x-axis" ) ;
   OPT_MENU_PULL_BUT(  opt_xaxis_menu , opt_xaxis_center_pb ,
                       "X-axis=center" , "X-axis = center voxel" ) ;
   OPT_MENU_PULL_BUT(  opt_xaxis_menu , opt_xaxis_clear_pb ,
                       "Clear x-axis" , "Clear x-axis timeseries" ) ;

   /*------ last button on this menu ------*/

   MENU_DLINE(opt_menu) ;
   OPT_MENU_BUT(opt_quit_pb         ,"Done         [q]" , "Close window" ) ;
   MCW_set_widget_bg( grapher->opt_quit_pb ,
                      MCW_hotcolor(grapher->opt_quit_pb) , 0 ) ;

   /** done with option buttons -- manage the manager widget **/

   XtManageChild( grapher->option_rowcol ) ;

   /** initialize the internal parameters **/

if(PRINT_TRACING)
{ char str[128] ;
  sprintf(str,"STATUS: num_series=%d nx=%d ny=%d",
          grapher->status->num_series,grapher->status->nx,grapher->status->ny ) ;
  STATUS(str) ; }

   grapher->fscale      =  0 ;
   grapher->mat         =  0 ;
   grapher->xpoint      = -1 ;
   grapher->ypoint      = -1 ;
   grapher->zpoint      = -1 ;
#if 0
   grapher->grid_color  = GRID_COLOR(grapher) ;
#endif
   grapher->grid_index  = -1 ;
   grapher->key_Nlock   =  0 ;
   grapher->xFD         =  0 ;
   grapher->yFD         =  0 ;
   grapher->common_base =  0 ;
   grapher->time_index  =  0 ;
   grapher->pin_num     =  0 ;  /* 27 Apr 1997 */
   grapher->ggap        =  INIT_GR_ggap ;  /* 12 Jan 1998 + 27 May 1999 */

   grapher->cen_line    =  NULL ;  /* coords of central graph plot */
   grapher->ncen_line   =  0 ;
   grapher->nncen       =  0 ;
   grapher->cen_tsim    =  NULL ;
   grapher->xax_tsim    =  NULL ;  /* 09 Jan 1998 */

   grapher->xx_text_1 = grapher->xx_text_2 = 1 ;

   grapher->ref_ts = NULL ;
   grapher->ort_ts = NULL ;

   grapher->ref_ts_plotall = grapher->ort_ts_plotall = 1 ;

   init_const( grapher ) ;

   grapher->setshift_inc_av   = NULL ;
   grapher->setshift_left_av  = NULL ;
   grapher->setshift_right_av = NULL ;
   grapher->dialog            = NULL ;
   grapher->setshift_inc      = 0.5 ;
   grapher->setshift_left     = 0 ;
   grapher->setshift_right    = 0 ;

   /** for the present, don't realize widgets (make the user do it later) **/

#if 0
STATUS("realizing widgets") ;
   XtRealizeWidget( grapher->fdw_graph ) ;

   while( XtWindow(grapher->draw_fd) == (Window) NULL ) ; /* wait */

   MCW_alter_widget_cursor( grapher->fdw_graph , -XC_left_ptr ,"yellow","blue" ) ;

   grapher->valid = 2 ;
#ifdef USE_OPTMENUS
   GRA_fix_optmenus( grapher ) ;
#endif
#endif

   grapher->fd_pxWind = (Pixmap) 0 ;

   /*** add callback for the WM_DELETE_WINDOW protocol ***/

   XmAddWMProtocolCallback(
        grapher->fdw_graph , XmInternAtom(dc->display,"WM_DELETE_WINDOW",False) ,
        end_fd_graph_CB , (XtPointer) grapher ) ;

   RETURN(grapher) ;
}

/*----------------------------------
    Exit button action
------------------------------------*/

void end_fd_graph_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_grapher * grapher = (MCW_grapher *) client_data ;
   int ii ;

ENTRY("end_fd_graph_CB") ;

   if( ! GRA_VALID(grapher) ) EXRETURN ;

   grapher->valid = 0 ;  /* can't do anything with this anymore */

   if( grapher->fd_pxWind != (Pixmap) 0 )
      XFreePixmap( grapher->dc->display , grapher->fd_pxWind ) ;

#ifdef USE_OPTMENUS
STATUS("destroying optmenus") ;
   FREE_AV(grapher->opt_mat_choose_av) ;
   FREE_AV(grapher->opt_slice_choose_av) ;
   FREE_AV(grapher->fmenu->fim_ignore_choose_av) ;
   FREE_AV(grapher->fmenu->fim_polort_choose_av) ;
#endif

STATUS("destroying arrowvals") ;
   FREE_AV( grapher->setshift_right_av)    ;
   FREE_AV( grapher->setshift_left_av)     ;
   FREE_AV( grapher->setshift_inc_av)      ;
   myXtFree( grapher->fmenu->fim_opt_bbox ) ;  /* Jan 1998 */
   myXtFree( grapher->fmenu->fimp_opt_bbox );  /* Jan 1998 */
   myXtFree( grapher->fmenu )               ;
   myXtFree( grapher->cen_line )            ;
   FREE_AV( grapher->transform0D_av )      ;  /* 22 Oct 1996 */
   FREE_AV( grapher->transform1D_av )      ;  /* 03 Nov 1996 */
   FREE_AV( grapher->opt_ggap_av )         ;  /* 28 Sep 1998: via Purify */

   for( ii=0 ; ii < NUM_COLOR_ITEMS ; ii++ )   /* 16 Jun 1997 */
      FREE_AV( grapher->opt_color_av[ii] ) ;

STATUS("destroying bboxes") ;
   myXtFree( grapher->transform1D_dplot_bbox ) ;  /* 08 Nov 1996 */

   for( ii=0 ; ii < NUM_COLOR_ITEMS ; ii++ ){
      myXtFree( grapher->opt_thick_bbox[ii] ) ;   /* 16 Jun 1997 */
      myXtFree( grapher->opt_points_bbox[ii] ) ;  /* 09 Jan 1998 */
  }

STATUS("freeing cen_tsim") ;
   mri_free( grapher->cen_tsim ) ;
   mri_free( grapher->xax_tsim ) ;  /* 09 Jan 1998 */

STATUS("freeing tuser") ;
   GRA_CLEAR_tuser( grapher ) ;  /* 22 Apr 1997 */

STATUS("destroying widgets") ;
   XtDestroyWidget( grapher->fdw_graph ) ;

   /** if AFNI has a notify callback, it will free the data **/

   if( grapher->status->send_CB != NULL ){
      GRA_cbs cbs ;
      cbs.reason = graCR_destroy ;
STATUS("calling AFNI") ;
      grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
   } else {
STATUS("freeing grapher") ;
      myXtFree( grapher ) ;    /* otherwise, we will free the data */
   }

   EXRETURN ;
}

/*----------------------------------
   Erase pixmap to background color
------------------------------------*/

void erase_fdw( MCW_grapher * grapher )
{
ENTRY("erase_fdw") ;

   DC_fg_color ( grapher->dc , BG_COLOR(grapher) ) ;
   DC_linewidth( grapher->dc , 0 ) ;

   XFillRectangle( grapher->dc->display ,
                   grapher->fd_pxWind , grapher->dc->myGC ,
                   0 , 0 , grapher->fWIDE , grapher->fHIGH ) ;

   if( show_grapher_pixmap &&
       grapher->logo_pixmap != XmUNSPECIFIED_PIXMAP &&
       grapher->logo_height > 0 && grapher->logo_width > 0 ){

      XCopyArea( grapher->dc->display ,
                 grapher->logo_pixmap , grapher->fd_pxWind , grapher->dc->myGC ,
                 0,0 , grapher->logo_width,grapher->logo_height ,
                 0,grapher->fHIGH - grapher->logo_height + 1 ) ;
   }

   EXRETURN ;
}

/*-----------------------------------------------------*/
   /* It plots line to point (x,y) for mod = 1 */
   /* or moves to this point for mod = 0.      */
   /* All into the fd_pxWind.                  */
/*-----------------------------------------------------*/

void plot_fdX( MCW_grapher * grapher , int x , int y , int mod )
{
   int iy = grapher->fHIGH - y;

   if( mod > 0 )
     XDrawLine( grapher->dc->display ,
                grapher->fd_pxWind , grapher->dc->myGC ,
                grapher->xFD , grapher->yFD , x , iy ) ;

   grapher->xFD = x ; grapher->yFD = iy ;
   return ;
}

/* ----------------------------------- */
/* Reload pixmap fd_pxWind to FDWindow */
/* ----------------------------------- */

void fd_px_store( MCW_grapher * grapher )
{
ENTRY("fd_px_store") ;

   if( ! MCW_widget_visible(grapher->draw_fd) ) EXRETURN ;  /* 03 Jan 1999 */

   XtVaSetValues( grapher->draw_fd ,
                     XmNbackgroundPixmap , grapher->fd_pxWind ,
                  NULL ) ;

   XClearWindow( grapher->dc->display , XtWindow(grapher->draw_fd) ) ;
   XFlush( grapher->dc->display ) ;
   EXRETURN ;
}

/*--------------------------------------------------------------
  draw a small circle somewhere:
    xwin,ywin = Window coordinates of center
    filled    = 1 or 0, if you want the circle solid or not
                14 Jan 1998: 2 if for points on a filled circle
----------------------------------------------------------------*/

#define NCIR 12  /* hollow circle */
#define NBAL 21  /* filled circle */
#define NBAX 25  /* with points   */

#define NBTOP NBAX  /* max # of points */

static XPoint xball[] = {
      {-1,-2},{ 0,-2},{ 1,-2},
      { 2,-1},{ 2, 0},{ 2, 1},
      { 1, 2},{ 0, 2},{-1, 2},
      {-2, 1},{-2, 0},{-2,-1},  /* NCIR ends here */
      {-1,-1},{-1, 0},{-1, 1},
      { 0,-1},{ 0, 0},{ 0, 1},
      { 1,-1},{ 1, 0},{ 1, 1},  /* NBAL ends here */
      { 0,-3},{ 0, 3},{ 3, 0},
      {-3, 0}                   /* NBAX ends here */
 } ;

/*--- draw into Pixmap (the graph itself) ---*/

void GRA_small_circle( MCW_grapher * grapher , int xwin , int ywin , int filled )
{
   int  i , ncirc ;
   XPoint a[NBTOP] ;

   switch( filled ){
      default: ncirc = NCIR ; break ;
      case 1:  ncirc = NBAL ; break ;
      case 2:  ncirc = NBAX ; break ;
   }

   for( i=0 ; i < ncirc ; i++ ){
      a[i].x = xball[i].x + xwin ;
      a[i].y = xball[i].y + ywin ;
   }

   XDrawPoints( grapher->dc->display , grapher->fd_pxWind ,
                grapher->dc->myGC , a , ncirc , CoordModeOrigin ) ;
   return ;
}

/*--- draw into window (the graph overlay) ---*/

void GRA_overlay_circle( MCW_grapher * grapher , int xwin , int ywin , int filled )
{
   int  i , ncirc ;
   XPoint a[NBTOP] ;

   switch( filled ){
      default: ncirc = NCIR ; break ;
      case 1:  ncirc = NBAL ; break ;
      case 2:  ncirc = NBAX ; break ;
   }

   for( i=0 ; i < ncirc ; i++ ){
      a[i].x = xball[i].x + xwin ;
      a[i].y = xball[i].y + ywin ;
   }

   DC_linewidth( grapher->dc , 0 ) ;

   XDrawPoints( grapher->dc->display , XtWindow(grapher->draw_fd) ,
                grapher->dc->myGC , a , ncirc , CoordModeOrigin ) ;
   return ;
}

/*-----------------------------------------------
   redraw stuff that overlays the pixmap
-------------------------------------------------*/

#define SHORT_NAME_WIDTH 384

static char * long_index_name  = "index="  ;
static char * short_index_name = "#"       ;
static char * long_value_name  = " value=" ;
static char * short_value_name = "="       ;
static char * long_time_name   = " at "    ;
static char * short_time_name  = "@"       ;

void GRA_redraw_overlay( MCW_grapher * grapher )
{
   Window    win ;
   Display * dis ;
   int       ii , xxx ;
   float     val ;
   char buf[16] , strp[128] ;
   char * vbuf , *iname , *vname ;

ENTRY("GRA_redraw_overlay") ;

   if( ! GRA_REALZ(grapher) ){ STATUS("ILLEGAL CALL") ; EXRETURN ; }

   if( ! MCW_widget_visible(grapher->draw_fd) ) EXRETURN ;  /* 03 Jan 1999 */

   /* erase contents of window (that aren't in the pixmap) */

   dis = grapher->dc->display ;
   win = XtWindow(grapher->draw_fd) ;
   XClearWindow( dis , win ) ;

   /* 22 July 1996:
      draw a ball on the graph at the currently display time_index */

   ii = grapher->time_index ;
   if( ii >= 0 && ii < NPTS(grapher) && ii < grapher->nncen ){
      DC_fg_color( grapher->dc , IDEAL_COLOR(grapher) ) ;
      GRA_overlay_circle( grapher ,
                          grapher->cen_line[ii].x , grapher->cen_line[ii].y , 2 ) ;
   }

   /* draw text showing value at currently displayed time_index */

   if( ii >= 0 && grapher->cen_tsim != NULL && ii < grapher->cen_tsim->nx ){
      val = MRI_FLOAT_PTR(grapher->cen_tsim)[ii] ;
      AV_fval_to_char( val , buf ) ;
      vbuf = (buf[0]==' ') ? buf+1 : buf ;

      if( grapher->fWIDE < SHORT_NAME_WIDTH ){
         iname = short_index_name ; vname = short_value_name ;
      } else {
         iname = long_index_name ; vname = long_value_name ;
      }

      sprintf( strp , "%s%d%s%s" , iname,ii , vname,vbuf ) ;

      if( grapher->cen_tsim->dx != 0.0 ){
         val = grapher->cen_tsim->xo + ii * grapher->cen_tsim->dx ;
         AV_fval_to_char( val , buf ) ;
         vbuf = (buf[0]==' ') ? buf+1 : buf ;
         ii = strlen(strp) ;
         sprintf( strp+ii , "%s%s" ,
                  (grapher->fWIDE < SHORT_NAME_WIDTH) ? short_time_name
                                                      : long_time_name, vbuf ) ;
      }

      xxx = MAX( grapher->xx_text_2 ,
                 grapher->xorigin[grapher->xc][grapher->yc] ) ;

      DC_fg_color( grapher->dc , IDEAL_COLOR(grapher) ) ;
      overlay_txt( grapher, xxx , GB_DLY-15 , strp ) ;
   }

   /* no more to do now */

   XFlush( dis ) ;
   EXRETURN ;
}

/*-----------------------------------------------
   redraw entire graph
------------------------------------------------*/

void redraw_graph( MCW_grapher * grapher , int code )
{
   int x, y , npoints , www,xxx ;
   int xc = grapher->xc , yc = grapher->yc ;
   char strp[256] , buf[64] ;
   int xd,yd,zd ;

ENTRY("redraw_graph") ;

   if( ! GRA_REALZ(grapher) ){ STATUS("ILLEGAL ENTRY") ; EXRETURN; }
   if( grapher->fd_pxWind == (Pixmap) 0 ){ STATUS("ILLEGAL ENTRY") ; EXRETURN; }

   /*---- draw the graphs ----*/

   erase_fdw  ( grapher ) ;
   draw_grids ( grapher ) ;
   plot_graphs( grapher , code ) ;

   /*---- draw some strings for informative purposes ----*/

   DC_fg_color( grapher->dc , TEXT_COLOR(grapher) ) ;

   /*** y axis labels ***/

   AV_fval_to_char( grapher->pmax[xc][yc] , strp ) ;
   www = DC_text_width(grapher->dc,strp) ;
   xxx = GL_DLX - www - 2 ;
   xxx = MAX(0,xxx) ;
   fd_txt( grapher , xxx , GB_DLY + grapher->gy_max - MYTXT, strp) ;

   AV_fval_to_char( grapher->pmax[xc][yc] - grapher->pmin[xc][yc] , buf ) ;
   if( buf[0] == ' ' ) buf[0] = '+' ;
   sprintf( strp , "[%s]" , buf ) ;
   www = DC_text_width(grapher->dc,strp) ;
   xxx = GL_DLX - www + 2 ;
   xxx = MAX(0,xxx) ;
   fd_txt( grapher , xxx , GB_DLY + grapher->gy_max - MYTXT - 14 , strp) ;

   AV_fval_to_char( grapher->pmin[xc][yc] , strp ) ;
   www = DC_text_width(grapher->dc,strp) ;
   xxx = GL_DLX - www - 2 ;
   xxx = MAX(0,xxx) ;
   fd_txt( grapher , xxx , GB_DLY + 5, strp) ;

   /*** bottom of the page coordinates stuff ***/

   /* first column */

   grapher->xx_text_1 = GL_DLX+5 ;

   xd = grapher->xpoint ; yd = grapher->ypoint ; zd = grapher->zpoint ;
#ifndef DONT_MANGLE_XYZ
   { THD_ivec3 id ;
     id = THD_fdind_to_3dind( grapher->getaux , TEMP_IVEC3(xd,yd,zd) ) ;
     xd = id.ijk[0] ; yd = id.ijk[1] ; zd = id.ijk[2] ; }
#endif

   sprintf(strp,"X: %d", xd) ;
   fd_txt( grapher , GL_DLX+5 , 35, strp) ;
   xxx = DC_text_width(grapher->dc,strp) ;

   sprintf(strp,"Y: %d", yd) ;
   fd_txt( grapher , GL_DLX+5 , 21, strp) ;
   www = DC_text_width(grapher->dc,strp) ; xxx = MAX(xxx,www) ;

   if( grapher->status->nz > 1 ){
      sprintf(strp,"Z: %d", zd) ;
      fd_txt( grapher , GL_DLX+5 ,  7, strp) ;
      www = DC_text_width(grapher->dc,strp) ; xxx = MAX(xxx,www) ;
   }

   /* second column */

   grapher->xx_text_2 = xxx = xxx + GL_DLX + 15 ;

   sprintf(strp,"Grid:%5d", grapher->grid_spacing ) ;

   if( grapher->fscale > 0 ){                        /* 04 Feb 1998: */
      AV_fval_to_char( grapher->fscale , buf ) ;     /* put scale on graph, too */
      www = strlen(strp) ;
      sprintf(strp+www," Scale:%s pix/datum",buf) ;
   } else if( grapher->fscale < 0 ){
      AV_fval_to_char( -grapher->fscale , buf ) ;
      www = strlen(strp) ;
      sprintf(strp+www," Scale:%s datum/pix",buf) ;
   }

   fd_txt( grapher , xxx , 21, strp) ;

   npoints = grapher->status->num_series ;

   if( grapher->pin_num < MIN_PIN )          /* 27 Apr 1997 */
      sprintf(strp, "Num: %5d", npoints) ;
   else
      sprintf(strp, "Num: %5d [%d]" , npoints,grapher->pin_num) ;

   if( grapher->common_base )            /* 08 Jan 1998 */
      strcat(strp,"  Base: common") ;
   else
      strcat(strp,"  Base: separate") ;

   fd_txt( grapher , xxx ,  7, strp ) ;

   /*** flush the pixmap to the screen ***/

   fd_px_store( grapher ) ;

   /*** draw any overlay stuff ***/

   GRA_redraw_overlay( grapher ) ;

#ifdef USE_OPTMENUS
   GRA_fix_optmenus( grapher ) ;
#endif

   grapher->never_drawn = 0 ;
   EXRETURN ;
}

/*------------------------------------------------
   Plot text in fd_pxWind at x,y position
   relative to lower left corner (!).
--------------------------------------------------*/

void fd_txt( MCW_grapher * grapher , int x , int y , char * str )
{
   XDrawString( grapher->dc->display, grapher->fd_pxWind,
                grapher->dc->myGC , x , grapher->fHIGH-y ,
                str , strlen(str) ) ;
   return ;
}

void overlay_txt( MCW_grapher * grapher , int x , int y , char * str )
{
   XDrawString( grapher->dc->display, XtWindow(grapher->draw_fd) ,
                grapher->dc->myGC , x , grapher->fHIGH-y ,
                str , strlen(str) ) ;
   return ;
}

/*-----------------------------------------------*/

void init_const( MCW_grapher * grapher )
{
   int ii , npoints ;

ENTRY("init_const") ;

   if( !GRA_VALID(grapher) ) EXRETURN ;

   if( grapher->fscale == 0 ) grapher->fscale = 1 ;

   grapher->mat_max = MAT_MAX ;
   grapher->mat_max = MIN( grapher->mat_max , grapher->status->nx ) ;
   grapher->mat_max = MIN( grapher->mat_max , grapher->status->ny ) ;

   if( grapher->mat <= 0 ) grapher->mat = 3 ;
   grapher->mat = MIN( grapher->mat , grapher->mat_max ) ;

   if( grapher->xpoint < 0 || grapher->xpoint >= grapher->status->nx )
      grapher->xpoint = grapher->status->nx / 2 ;

   if( grapher->ypoint < 0 || grapher->ypoint >= grapher->status->ny )
      grapher->ypoint = grapher->status->ny / 2 ;

   if( grapher->zpoint < 0 || grapher->zpoint >= grapher->status->nz )
      grapher->zpoint = grapher->status->nz / 2 ;

   npoints = NPTS(grapher) ;  /* 27 Apr 1997 */

   if( grapher->grid_index < 0 ){
      for( ii=GRID_MAX-1 ; ii > 0 ; ii-- ) if( grid_ar[ii] <= npoints / 3 ) break ;
      grapher->grid_index = ii ;
      grapher->grid_spacing = grid_ar[ii] ;
   }

#if 0
   if( grapher->grid_color < 0 )
      grapher->grid_color = 1 ;  /* first overlay color */
#endif

   if( grapher->time_index < 0 )
      grapher->time_index = 0 ;
   else if( grapher->time_index >= grapher->status->num_series )
      grapher->time_index = grapher->status->num_series - 1 ;

   init_mat(grapher) ;
   EXRETURN ;
}

/*-----------------------------------------------------------
    Plot all graphs to pixmap
-------------------------------------------------------------*/

void plot_graphs( MCW_grapher * grapher , int code )
{
   MRI_IMAGE * tsim ;
   MRI_IMARR * tsimar ;
   float     * tsar ;
   float       tsbot=0.0 , ftemp , tstop ;
   int i, m, index, ix, iy, xtemp,ytemp,ztemp , xoff,yoff ,npoints,its,ibot,itop;

   static int      * plot = NULL ;  /* arrays to hold plotting coordinates */
   static XPoint * a_line = NULL ;
   static int       nplot = 0 ;

   MRI_IMARR * dplot_imar = NULL ;  /* 08 Nov 1996 */
   int         dplot = 0 ;

   MRI_IMARR * eximar ;
   int         iex ;

   float nd_bot , nd_top , nd_dif ;                        /* 03 Feb 1998 */
   int   set_scale = ( (code & PLOTCODE_AUTOSCALE) != 0 ||
                       grapher->never_drawn ) ;

ENTRY("plot_graphs") ;

   GRA_fixup_xaxis( grapher ) ;   /* 09 Jan 1998 */

   DC_fg_color ( grapher->dc , DATA_COLOR(grapher) ) ;
   DC_linewidth( grapher->dc , DATA_THICK(grapher) ) ;

#define NPLOT_INIT 9999  /* 29 Apr 1997 */
   itop = NPTS(grapher) ; itop = MAX( NPLOT_INIT , itop ) ;
   if( nplot == 0 || nplot < itop ){
      myXtFree(a_line) ; myXtFree(plot) ;
      nplot  = itop ;
      plot   = (int *)    XtMalloc( sizeof(int)    * itop ) ;
      a_line = (XPoint *) XtMalloc( sizeof(XPoint) * itop ) ;
   }
   if( grapher->ncen_line < itop ){
      myXtFree(grapher->cen_line) ;
      grapher->cen_line  = (XPoint *) XtMalloc( sizeof(XPoint) * itop ) ;
      grapher->ncen_line = itop ;
   }

   ibot = grapher->init_ignore ;
   if( ibot >= NPTS(grapher) ) ibot = 0 ;  /* 03 May 1997 */

   /** loop over matrix of graphs and get all the time series for later **/

   INIT_IMARR(tsimar) ;

   /** 08 Nov 1996: initialize second array for double plotting **/

   if( grapher->transform1D_func != NULL &&
       MCW_val_bbox(grapher->transform1D_dplot_bbox) ){

      INIT_IMARR(dplot_imar) ;
      dplot = 1 ;
   }

   GRA_CLEAR_tuser( grapher ) ;  /* 22 Apr 1997 */

   /* offset to correct slice number */
   ztemp = grapher->zpoint * grapher->status->ny * grapher->status->nx ;

   for( ix=0 ; ix < grapher->mat ; ix++ ){

      /** compute the 3D index of the desired time series **/

      xtemp  = grapher->xpoint + ix - grapher->xc ;
           if( xtemp <  0                   ) xtemp += grapher->status->nx ;  /* wrap */
      else if( xtemp >= grapher->status->nx ) xtemp -= grapher->status->nx ;

      for( iy=0 ; iy < grapher->mat ; iy++ ){
         ytemp = grapher->ypoint - iy + grapher->yc ;
              if( ytemp <  0                   ) ytemp += grapher->status->ny ;  /* wrap */
         else if( ytemp >= grapher->status->ny ) ytemp -= grapher->status->ny ;

         index = ztemp + ytemp * grapher->status->nx + xtemp ;

         /** get the desired time series, using the provided routine **/

         tsim  = (MRI_IMAGE *) grapher->getser( index , graCR_getseries ,
                                                        grapher->getaux ) ;

         /* 08 Nov 1996: allow for return of NULL timeseries */

         if( tsim == NULL ){
            ADDTO_IMARR(tsimar,NULL) ;
            if( dplot) ADDTO_IMARR(dplot_imar,NULL) ;
            continue ;
         }

         /** convert it to floats, if need be **/

         if( tsim->kind != MRI_float ){
            MRI_IMAGE * qim = mri_to_float(tsim) ;
            mri_free(tsim) ; tsim = qim ;
         }

         /* 22 Oct 1996: transform each point, if ordered */

         if( grapher->transform0D_func != NULL ){
STATUS("about to perform 0D transformation") ;
            grapher->transform0D_func( tsim->nx , MRI_FLOAT_PTR(tsim) ) ;
         }

         /* 03 Nov 1996: 1D transformations, too */
         /* 08 Nov 1996: double plotting, too */

         if( grapher->transform1D_func != NULL ){
            MRI_IMAGE * qim ;

            if( dplot ){
               qim = mri_to_float(tsim) ;
               ADDTO_IMARR(dplot_imar,qim) ;
            }
            else
               qim = tsim ;

STATUS("about to perform 1D transformation") ;

            if( ! (grapher->transform1D_flags & RETURNS_STRING) ){
               grapher->transform1D_func( qim->nx , qim->xo , qim->dx ,
                                          MRI_FLOAT_PTR(qim) ) ;
            } else {
               char * quser = NULL ;
               grapher->transform1D_func( qim->nx , qim->xo , qim->dx ,
                                          MRI_FLOAT_PTR(qim) , &quser ) ;
STATUS("about to assign tuser string") ;
               if( quser != NULL )
                 grapher->tuser[ix][iy] = XtNewString(quser) ;
            }
         }

         /* put this (possibly transformed) image on the list of those to plot */

         ADDTO_IMARR(tsimar,tsim) ;
      }
   }

   /** find some statistics of each time series **/

STATUS("finding statistics of time series") ;

   if( set_scale ){
      nd_bot = WAY_BIG ; nd_top = nd_dif = - WAY_BIG ;  /* 03 Feb 1998 */
   }

   for( ix=0,its=0 ; ix < grapher->mat ; ix++ ){
      for( iy=0 ; iy < grapher->mat ; iy++,its++ ){
         float qbot,qtop ;
         double qsum , qsumq ;

         tsim = IMARR_SUBIMAGE(tsimar,its) ;
         if( tsim == NULL || tsim->nx < 2 ){
            grapher->tmean[ix][iy] = grapher->tbot[ix][iy] =
              grapher->ttop[ix][iy] = grapher->tstd[ix][iy] = 0.0 ;
            continue ;
         }

         tsar = MRI_FLOAT_PTR(tsim) ;
         itop = NPTS(grapher) ;
         itop = npoints = MIN( itop , tsim->nx ) ;

         if( itop-ibot < 2 ){
            grapher->tmean[ix][iy] = grapher->tbot[ix][iy] =
              grapher->ttop[ix][iy] = grapher->tstd[ix][iy] = 0.0 ;
            continue ;
         }

         qbot = qtop  = tsar[ibot] ;
         qsum = qsumq = 0.0 ;
         for( i=ibot ; i < itop ; i++ ){
            qbot   = MIN( qbot , tsar[i] ) ;
            qtop   = MAX( qtop , tsar[i] ) ;
            qsum  += tsar[i] ;
            qsumq += tsar[i] * tsar[i] ;
         }
         grapher->tbot[ix][iy] = qbot ; grapher->ttop[ix][iy] = qtop ;
         qsum  = qsum / (itop-ibot) ; grapher->tmean[ix][iy] = qsum ;
         qsumq = (qsumq - (itop-ibot) * qsum * qsum) / (itop-ibot-1) ;
         grapher->tstd[ix][iy] = (qsumq > 0.0) ? sqrt(qsumq) : 0.0 ;

         if( set_scale ){        /* 03 Feb 1998 */
            nd_bot = MIN( nd_bot , qbot ) ;
            nd_top = MAX( nd_top , qtop ) ;
            nd_dif = MAX( nd_dif , (qtop-qbot) ) ;
         }
      }
   }

   /* 03 Feb 1998: set the initial scale factor */

   if( set_scale && nd_bot < nd_top && nd_dif > 0.0 ){

      if( grapher->common_base ){
         grapher->fscale = 0.9 * grapher->gy / (nd_top-nd_bot) ; /* global range */
      } else {
         grapher->fscale = 0.9 * grapher->gy / nd_dif ;          /* biggest range */
      }

      if( grapher->fscale > 0.0 && grapher->fscale < 1.0 )       /* switcheroo */
         grapher->fscale = -1.0 / grapher->fscale ;

           if( grapher->fscale > 4.0 )                           /* even value */
                  grapher->fscale = (int) grapher->fscale ;

      else if( grapher->fscale > 1.0 )
                  grapher->fscale = 0.5 * ((int)(2.0*grapher->fscale)) ;

      else if( grapher->fscale < -4.0 )
                  grapher->fscale = -((int)(1.0-grapher->fscale)) ;

      else if( grapher->fscale < -1.0 )
                  grapher->fscale = -0.5 * ((int)(1.0-2.0*grapher->fscale)) ;
   }

   /** find the smallest element in all the time series, if needed **/

   if( grapher->common_base ){
      int first = 1 ;

STATUS("finding common base") ;

      for( ix=0 ; ix < tsimar->num ; ix++ ){
         tsim = IMARR_SUBIMAGE(tsimar,ix) ;
         if( tsim == NULL || tsim->nx < 2 ) continue ;   /* skip */
         tsar = MRI_FLOAT_PTR(tsim) ;
         itop = NPTS(grapher) ;
         itop = npoints = MIN( itop , tsim->nx ) ;
         if( first && ibot < itop ){ tsbot = tsar[ibot] ; first = 0 ; }
         for( i=ibot ; i < itop ; i++ ) tsbot = MIN( tsbot , tsar[i] ) ;
      }
   }

   /** loop over matrix of graphs and plot them all to the pixmap **/

STATUS("starting time series graph loop") ;
   for( ix=0,its=0 ; ix < grapher->mat ; ix++ ){

      for( iy=0 ; iy < grapher->mat ; iy++,its++ ){

         tsim = IMARR_SUBIMAGE(tsimar,its) ;
         if( tsim == NULL || tsim->nx < 2 ) continue ; /* skip this graph */
         tsar = MRI_FLOAT_PTR(tsim) ;
         itop = NPTS(grapher) ;
         itop = npoints = MIN( itop , tsim->nx ) ;

         /** find bottom of this graph, if needed **/

         if( ! grapher->common_base && ibot < itop ){
            tsbot = tsar[ibot] ;
            for( i=ibot+1 ; i < itop ; i++ ) tsbot = MIN( tsbot , tsar[i] ) ;
         }
         grapher->pmin[ix][iy] = tsbot ;  /* value at graph bottom */

#if 0  /* 29 Apr 1997 */
         /** if need be, allocate more space for graphing **/

         if( npoints > nplot ){
            myXtFree(a_line) ; myXtFree(plot) ;
            plot   = (int *)    XtMalloc( sizeof(int)    * npoints ) ;
            a_line = (XPoint *) XtMalloc( sizeof(XPoint) * npoints ) ;
            nplot  = npoints ;
         }
#endif

         /** scale graph vertically:
               fscale > 0 ==> this many pixels per unit of tsar
               fscale < 0 ==> this many units of tsar per pixel **/

         ftemp = grapher->fscale ;
              if( ftemp == 0.0 ) ftemp =  1.0 ;
         else if( ftemp <  0.0 ) ftemp = -1.0 / ftemp ;

         for( i=0 ; i < MIN(ibot,itop) ; i++ )
            plot[i] = (tsar[ibot] - tsbot) * ftemp ;

         for( i=ibot ; i < itop ; i++ )
            plot[i] = (tsar[i] - tsbot) * ftemp ;

         grapher->pmax[ix][iy] = tsbot + grapher->gy / ftemp ; /* value at graph top */

         /** Compute X11 line coords from pixel heights in plot[].
             N.B.: X11 y is DOWN the screen, but plot[] is UP the screen **/

         ftemp = grapher->gx / (float) (NPTS(grapher) - 1) ;
         xoff  = grapher->xorigin[ix][iy] ;
         yoff  = grapher->fHIGH - grapher->yorigin[ix][iy] ;

         /* 09 Jan 1998: allow x-axis to be chosen by a
                         timeseries that ranges between 0 and 1 */

#define XPIX(ii)                                                    \
   ( (grapher->xax_tsim != NULL && ii < grapher->xax_tsim->nx)       \
     ? (MRI_FLOAT_PTR(grapher->xax_tsim)[MAX(ii,ibot)] * grapher->gx) \
     : (ii * ftemp) )

         for( i=0 ; i < itop ; i++ ){
            a_line[i].x = xoff + XPIX(i) ;   /* 09 Jan 1998 */
            a_line[i].y = yoff - plot[i] ;
         }

         if( DATA_POINTS(grapher) ){         /* 09 Jan 1998 */
            for( i=0 ; i < itop ; i++ )
               GRA_small_circle( grapher,a_line[i].x,a_line[i].y,DATA_IS_THICK(grapher) ) ;
         }
         if( DATA_LINES(grapher) ){          /* 01 Aug 1998 */
            XDrawLines( grapher->dc->display ,
                        grapher->fd_pxWind , grapher->dc->myGC ,
                        a_line , npoints ,  CoordModeOrigin ) ;
         }

         /* 22 July 1996: save central graph data for later use */

         if( ix == grapher->xc && iy == grapher->yc ){
#if 0
            if( npoints > grapher->ncen_line ){
               myXtFree(grapher->cen_line) ;
               grapher->cen_line  = (XPoint *) XtMalloc( sizeof(XPoint) * npoints ) ;
               grapher->ncen_line = npoints ;
            }
#endif
            for( i=0 ; i < npoints ; i++ ) grapher->cen_line[i] = a_line[i] ;
            grapher->nncen = npoints ;

            mri_free( grapher->cen_tsim ) ;             /* copy time series too */
            grapher->cen_tsim = mri_to_float( tsim ) ;
         }

         /* 08 Nov 1996: double plot?  Duplicate the above drawing code! */

         if( dplot ){
            tsim = IMARR_SUBIMAGE(dplot_imar,its) ;
            if( tsim == NULL || tsim->nx < 2 ) continue ;  /* skip */
            tsar = MRI_FLOAT_PTR(tsim) ;
            itop = NPTS(grapher) ;
            itop = npoints = MIN( itop , tsim->nx ) ;

            ftemp = grapher->fscale ;
                 if( ftemp == 0.0 ) ftemp =  1.0 ;
            else if( ftemp <  0.0 ) ftemp = -1.0 / ftemp ;

            for( i=0 ; i < MIN(ibot,itop) ; i++ )
               plot[i] = (tsar[ibot] - tsbot) * ftemp ;
            for( i=ibot ; i < itop ; i++ )
               plot[i] = (tsar[i] - tsbot) * ftemp ;

            ftemp = grapher->gx / (float) (NPTS(grapher) - 1) ;
            xoff  = grapher->xorigin[ix][iy] ;
            yoff  = grapher->fHIGH - grapher->yorigin[ix][iy] ;

            for( i=0 ; i < itop ; i++ ){
               a_line[i].x = xoff + XPIX(i) ;  /* 09 Jan 1998 */
               a_line[i].y = yoff - plot[i] ;
            }

            DC_fg_color ( grapher->dc , DPLOT_COLOR(grapher) ) ;

            if( DPLOT_POINTS(grapher) ){        /* 09 Jan 1998 */
               for( i=0 ; i < itop ; i++ )
                  GRA_small_circle( grapher,a_line[i].x,a_line[i].y,DPLOT_IS_THICK(grapher) ) ;
            }
            if( DPLOT_LINES(grapher) ) {        /* 01 Aug 1998 */
               DC_linewidth( grapher->dc , DPLOT_THICK(grapher) ) ;
               XDrawLines( grapher->dc->display ,
                           grapher->fd_pxWind , grapher->dc->myGC ,
                           a_line , npoints ,  CoordModeOrigin ) ;
            }

            DC_fg_color ( grapher->dc , DATA_COLOR(grapher) ) ;
            DC_linewidth( grapher->dc , DATA_THICK(grapher) ) ;
         }

         /* 05 Jan 1999: plot horizontal line through zero, if desired and needed */

         if( grapher->HorZ && grapher->pmin[ix][iy] < 0.0 && grapher->pmax[ix][iy] > 0.0 ){
            DC_fg_color ( grapher->dc , GRID_COLOR(grapher) ) ;
            DC_linewidth( grapher->dc , GRID_THICK(grapher) ) ;
            DC_dashed_line( grapher->dc ) ;

            ftemp = grapher->fscale ;
                 if( ftemp == 0.0 ) ftemp =  1.0 ;
            else if( ftemp <  0.0 ) ftemp = -1.0 / ftemp ;

            XDrawLine( grapher->dc->display , grapher->fd_pxWind , grapher->dc->myGC ,
                       (int) xoff                , (int)(yoff + tsbot * ftemp) ,
                       (int)(xoff + grapher->gx) , (int)(yoff + tsbot * ftemp)  ) ;

            DC_fg_color ( grapher->dc , DATA_COLOR(grapher) ) ;
            DC_linewidth( grapher->dc , DATA_THICK(grapher) ) ;
            DC_solid_line( grapher->dc ) ;
         }

      } /* end of loop over y */
   } /* end of loop over x */

   /** cast away the data timeseries! */

   DESTROY_IMARR(tsimar) ;
   if( dplot_imar != NULL ) DESTROY_IMARR(dplot_imar) ;  /* 08 Nov 1996 */

   /*----- Now do extra plots in center frame, if any -----*/

#define REFTS_FRAC 0.38  /* fraction of one graph that this takes up */
#define REFTS_TOP  0.98  /* top of reference graph in frame */

#define ORTTS_FRAC 0.38
#define ORTTS_TOP  0.78

   /* 12 Nov 1996: include graphs of orts by looping "iex" */

   for( iex=0 ; iex <= 1 ; iex++ ){

      eximar = (iex==0) ? grapher->ref_ts : grapher->ort_ts ;

      if( eximar != NULL && IMARR_COUNT(eximar) > 0 ){
         float yscal , val , xscal , exfrac , extop ;
         int   nover , nvec , nx , ivec ;
         int   excolor , exthick ;

STATUS("plotting extra graphs") ;

         exfrac  = (iex==0) ? REFTS_FRAC : ORTTS_FRAC ;
         extop   = (iex==0) ? REFTS_TOP  : ORTTS_TOP  ;
         excolor = (iex==0) ? IDEAL_COLOR(grapher) : ORT_COLOR(grapher) ;
         exthick = (iex==0) ? IDEAL_THICK(grapher) : ORT_THICK(grapher) ;

         for( its=0 ; its < IMARR_COUNT(eximar) ; its++ ){

            tsim = IMARR_SUBIMAGE(eximar,its) ;

#ifdef AFNI_DEBUG
            if( tsim == NULL )           { STATUS("can't graph NULL timeseries!")      ; continue ; }
            if( tsim->kind != MRI_float ){ STATUS("can't graph non-float timeseries!") ; continue ; }
            if( tsim->nx < 2 )           { STATUS("can't graph 1 point timeseries!")   ; continue ; }
#else
            if( tsim == NULL || tsim->kind != MRI_float || tsim->nx < 2 ) continue ;
#endif
            nx   = tsim->nx ;
            itop = NPTS(grapher) ;
            itop = npoints = MIN( itop , nx ) ;
            nvec = (grapher->ref_ts_plotall) ? (tsim->ny) : 1 ;

            for( ivec=0 ; ivec < nvec ; ivec++ ){  /* plot each sub-vector */
               tsar  = MRI_FLOAT_PTR(tsim) + (ivec*nx) ;
               tsbot = 99999999.0 ; tstop = -99999999.0 ;
               nover = ibot ;
               for( i=ibot ; i < itop ; i++ ){
                  val = tsar[i] ;
                  if( val < WAY_BIG ){
                     tstop = MAX(tstop,val) ;
                     tsbot = MIN(tsbot,val) ;
                  } else {
                     nover++ ;
                  }
               }
               if( tstop >= WAY_BIG || tstop <= tsbot ){
                  STATUS("can't graph unpleasant extra timeseries!") ;
                  continue ;
               }

               /*** scale into a_line and draw it***/

               yscal = exfrac * grapher->gy / (tstop-tsbot) ;
               xscal = ftemp = grapher->gx / (float) (NPTS(grapher) - 1) ;

               xoff  = grapher->xorigin[grapher->xc][grapher->yc] ;
               yoff  = grapher->fHIGH - grapher->yorigin[grapher->xc][grapher->yc]
                                      - (extop - exfrac) * grapher->gy ;

               for( i=0 ; i < npoints; i++ ){
                  val = (i >= ibot &&  tsar[i] < WAY_BIG) ? tsar[i] : tsbot ;

                  a_line[i].x = xoff + XPIX(i) ;           /* 09 Jan 1998 */
                  a_line[i].y = yoff - yscal*(val-tsbot) ;
               }

               /* if none are over the limit, draw in one operation;
                  otherwise, must plot each line separately in its needed color */

               if( nover == 0 ){
                  DC_fg_color ( grapher->dc , excolor ) ;
                  DC_linewidth( grapher->dc , exthick ) ;
                  XDrawLines( grapher->dc->display ,
                              grapher->fd_pxWind , grapher->dc->myGC ,
                              a_line , npoints ,  CoordModeOrigin ) ;
               } else {
                  for( i=0 ; i < npoints-1 ; i++ ){
                     if( i >= ibot && tsar[i] < WAY_BIG && tsar[i+1] < WAY_BIG ){
                        DC_fg_color ( grapher->dc , excolor ) ;
                        DC_linewidth( grapher->dc , exthick ) ;
                     } else {
                        DC_fg_color( grapher->dc , IGNORE_COLOR(grapher) ) ;
                        if( grapher->mat < 4 )
                           GRA_small_circle( grapher,a_line[i].x,a_line[i].y,0 ) ;
                     }

                     XDrawLines( grapher->dc->display ,
                                 grapher->fd_pxWind , grapher->dc->myGC ,
                                 a_line + i , 2 ,  CoordModeOrigin ) ;
                  }
               }
            } /* end of loop over sub-vectors */
         } /* end of loop over refs */
      } /* end of if refs exist */
   } /* end of loop over refs and orts */

   /*---- 09 Jan 1998: plot graph showing x-axis as well ----*/

   if( grapher->xax_tsim != NULL ){
      float yscal , ftemp , xscal , yoff ;
      int   npt ;

      xscal = GL_DLX / (float) grapher->gx ;
      yscal = grapher->gy / (float) (NPTS(grapher) - 1) ;
      yoff  = grapher->fHIGH - grapher->yorigin[grapher->xc][grapher->yc] ;
      ftemp = 1.0 ;
      npt   = NPTS(grapher) ;
      if( npt > grapher->xax_tsim->nx ) npt = grapher->xax_tsim->nx ;

      for( i=0 ; i < npt; i++ ){
         a_line[i].x = XPIX(i) * xscal ;
         a_line[i].y = yoff - yscal*i ;
      }

      DC_fg_color ( grapher->dc , IDEAL_COLOR(grapher) ) ;
      DC_linewidth( grapher->dc , IDEAL_THICK(grapher) ) ;
      XDrawLines( grapher->dc->display , grapher->fd_pxWind , grapher->dc->myGC ,
                  a_line , npt ,  CoordModeOrigin ) ;
   }

   /***** Done!!! *****/

   EXRETURN ;
}

/*------------------------------------------------------------------------
   Draw frames around each sub-graph and grids inside them
   12 Jan 1998: modified to allow for gaps between graphs
--------------------------------------------------------------------------*/
void draw_grids( MCW_grapher * grapher )
{
   int i , mat=grapher->mat , gx=grapher->gx , gy=grapher->gy ;
   int j, k, g, xo, yo, npoints , m ;
   int xc = grapher->xc , yc = grapher->yc ;
   float ftemp ;

ENTRY("draw_grids") ;

   /* draw grid lines in the chosen color */

   if( GRID_COLOR(grapher) > 0 ){
      DC_fg_color ( grapher->dc , GRID_COLOR(grapher) ) ;
      DC_linewidth( grapher->dc , GRID_THICK(grapher) ) ;

      g       = grapher->grid_spacing ;
      npoints = NPTS(grapher) ;
      ftemp   = gx / (float) (npoints-1) ;

      for( i=0 ; i < mat ; i++ ){
         for( m=0 ; m < mat ; m++ ){
            xo = grapher->xorigin[i][m] ; yo = grapher->yorigin[i][m] ;
            for( j=1 ; j <= (npoints-1)/g ; j++ ){
               k = xo + j * g * ftemp ;
               plot_fdX( grapher , k , yo    , 0 ) ;
               plot_fdX( grapher , k , yo+gy , 1 ) ;
            }
         }
      }

      /* draw an interior framing box at the central square */

      xo = grapher->xorigin[xc][yc] ; yo = grapher->yorigin[xc][yc] ;
      g  = MIN( grapher->gy/3 , grapher->gx/3 ) ; g  = MIN( g , 4 ) ;
      for( j=1 ; j <= g ; j++ ){
         plot_fdX( grapher , xo+j    , yo+j    , 0 ) ;
         plot_fdX( grapher , xo+j    , yo+gy-j , 1 ) ;
         plot_fdX( grapher , xo+gx-j , yo+gy-j , 1 ) ;
         plot_fdX( grapher , xo+gx-j , yo+j    , 1 ) ;
         plot_fdX( grapher , xo+j    , yo+j    , 1 ) ;
      }
   }

   /* draw exterior frames */

   DC_fg_color ( grapher->dc , FG_COLOR(grapher) ) ;
   DC_linewidth( grapher->dc , FG_THICK(grapher) ) ;

   for( i=0 ; i < mat ; i++ ){
      for( j=0 ; j < mat ; j++ ){
         xo = grapher->xorigin[i][j] ; yo = grapher->yorigin[i][j] ;
         plot_fdX( grapher , xo    , yo    , 0 ) ;
         plot_fdX( grapher , xo+gx , yo    , 1 ) ;
         plot_fdX( grapher , xo+gx , yo+gy , 1 ) ;
         plot_fdX( grapher , xo    , yo+gy , 1 ) ;
         plot_fdX( grapher , xo    , yo    , 1 ) ;
      }
   }

   EXRETURN ;
}

/*------------------------------------------
  Send the caller info about the new graph
--------------------------------------------*/
void send_newinfo( MCW_grapher * grapher )
{
ENTRY("send_newinfo") ;

   if( GRA_VALID(grapher) && grapher->status->send_CB != NULL ){
      GRA_cbs cbs ;

      cbs.reason = graCR_newxyzm   ;
      cbs.xcen   = grapher->xpoint ;
      cbs.ycen   = grapher->ypoint ;
      cbs.zcen   = grapher->zpoint ;
      cbs.mat    = grapher->mat ;
      grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
   }

   EXRETURN ;
}

/*---------------------------
   initialize matrix stuff
-----------------------------*/
void init_mat( MCW_grapher * grapher )
{
   int i, j ;
   int gg ;

ENTRY("init_mat") ;
   if( !GRA_VALID(grapher) ) EXRETURN ;


   grapher->gx = grapher->gx_max / grapher->mat;
   grapher->gy = grapher->gy_max / grapher->mat;

   for (i=0;i<grapher->mat;i++) {
     for (j=0;j<grapher->mat;j++) {
           grapher->xorigin[i][j] = MDX1 + i * grapher->gx;
           grapher->yorigin[i][j] = MDY1 + j * grapher->gy;
         }
   }
   grapher->xc = grapher->mat/2;
   grapher->yc = (grapher->mat-1)/2;

   gg = grapher->ggap ;                /* 12 Jan 1998 */
   if( gg > 0 ){
      gg = MIN( gg , grapher->gx / 2 ) ;  /* shrink sizes of graphs */
      gg = MIN( gg , grapher->gy / 2 ) ;
      grapher->gx -= gg ;
      grapher->gy -= gg ;
   }

   EXRETURN ;
}

/* ----------------------------- */   /* scale plot up and redraw  */
void scale_up( MCW_grapher * grapher )
/* ----------------------------- */
{
   if( !GRA_VALID(grapher) ) return ;
   if (grapher->fscale > 0) grapher->fscale *= 2;
   else if (grapher->fscale < -2) grapher->fscale /= 2;
   else grapher->fscale = 1;
   return ;
}

/* ----------------------------- */   /* scale plot up and redraw  */
void scale_down( MCW_grapher * grapher )
/* ----------------------------- */
{
   if( !GRA_VALID(grapher) ) return ;
   if (grapher->fscale > 1) grapher->fscale /= 2;
   else if (grapher->fscale < 0) grapher->fscale *= 2;
   else grapher->fscale = -2;
   return ;
}

/* ----------------------------- */   /* decrease matrix and redraw  */
void mat_down( MCW_grapher * grapher )
/* ----------------------------- */
{
   int old;

   if( !GRA_VALID(grapher) ) return ;
   old = grapher->mat;
   grapher->mat--;
   if (grapher->mat < 1) grapher->mat = 1;
   else if (grapher->mat > grapher->mat_max) grapher->mat = grapher->mat_max;
   if (grapher->mat!= old) {
      init_mat( grapher ) ;
      redraw_graph( grapher , 0 ) ;
   }
   return ;
}

/* ----------------------------- */   /* increase matrix and redraw  */
void mat_up( MCW_grapher * grapher )
/* ----------------------------- */
{
   int old;

   if( !GRA_VALID(grapher) ) return ;
   old = grapher->mat;
   grapher->mat++;
   if (grapher->mat < 1) grapher->mat = 1;
   else if (grapher->mat > grapher->mat_max) grapher->mat = grapher->mat_max;
   if (grapher->mat!= old) {
      init_mat(grapher) ;
      redraw_graph(grapher,0) ;
   }
   return ;
}

/* ----------------------------- */   /* decrease grid spacing and redraw  */
void grid_down( MCW_grapher * grapher )
/* ----------------------------- */
{
   int old;

   if( !GRA_VALID(grapher) ) return ;
   old = grapher->grid_index;
   grapher->grid_index--;
   if (grapher->grid_index < 0) grapher->grid_index = 0;
   grapher->grid_spacing = grid_ar[grapher->grid_index] ;
   redraw_graph(grapher,0) ;
   return ;
}

/* ----------------------------- */   /* increase grid spacing and redraw  */
void grid_up( MCW_grapher * grapher )
/* ----------------------------- */
{
   int old;

   if( !GRA_VALID(grapher) ) return ;
   old = grapher->grid_index;
   grapher->grid_index++;
   if (grapher->grid_index >= GRID_MAX) grapher->grid_index = GRID_MAX - 1;
   grapher->grid_spacing = grid_ar[grapher->grid_index] ;
   redraw_graph(grapher,0) ;
   return ;
}

/*-----------------------------------------------------------------------
   Handle all events in an grapher drawing area widget
-------------------------------------------------------------------------*/

void GRA_drawing_EV( Widget w , XtPointer client_data ,
                     XEvent * ev , Boolean * continue_to_dispatch )
{
   MCW_grapher * grapher = (MCW_grapher *) client_data ;

ENTRY("GRA_drawing_EV") ;

   if( ! GRA_REALZ(grapher) ){
if(PRINT_TRACING){
char str[256] ;
sprintf(str,"unrealized grapher! Event type = %d",(int)ev->type) ;
STATUS(str) ; }
      EXRETURN ;
   }

   if( grapher->valid == 666 ){  /* 06 Jan 1999 */
if(PRINT_TRACING){
char str[256] ;
sprintf(str,"dying grapher! Event type = %d",(int)ev->type) ;
STATUS(str) ; }
      EXRETURN ;
   }

   switch( ev->type ){

      /*----- redraw -----*/

      case Expose:{
         XExposeEvent * event = (XExposeEvent *) ev ;

if(PRINT_TRACING){
char str[256] ;
sprintf(str,"Expose event with count = %d",event->count) ;
STATUS(str) ; }

         /**
             With the first expose, create the new pixmap.
             For subsequent ones, just redraw the non-pixmap (overlay) stuff.

             06 Jan 1999: check event count
         **/

         XSync( XtDisplay(w) , False ) ;  /* 05 Feb 1999 */

         if( event->count == 0 ){
            if( grapher->fd_pxWind == (Pixmap) 0 ){
               int width , height ;
               MCW_widget_geom( grapher->draw_fd , &width , &height , NULL,NULL ) ;
               GRA_new_pixmap( grapher , width , height , 1 ) ;
            } else {
               GRA_redraw_overlay( grapher ) ;
            }
         }
      }
      break ;

      /*----- take key press -----*/

      case KeyPress:{
         XKeyEvent * event = (XKeyEvent *) ev ;
         char           buf[32] ;
         KeySym         ks ;
         int            nbuf ;

STATUS("KeyPress event") ;

         if( grapher->fd_pxWind != (Pixmap) 0 ){
            buf[0] = '\0' ;
            nbuf = XLookupString( event , buf , 32 , &ks , NULL ) ;
            if( buf[0] != '\0' ) GRA_handle_keypress( grapher , buf , ev ) ;
            else if(PRINT_TRACING){
               char str[256] ;
               sprintf(str,"*** KeyPress was empty!?  nbuf=%d",nbuf) ;
               STATUS(str) ; }
         }
      }
      break ;

      /*----- take button press -----*/

      case ButtonPress:{
         XButtonEvent * event = (XButtonEvent *) ev ;
         int bx,by , width,height , but ;
         int i, j, gx , gy , mat , xloc,yloc ;
         unsigned int but_state ;

STATUS("button press") ;

         bx  = event->x ;
         by  = event->y ;
         but = event->button ; but_state = event->state ;
         MCW_discard_events( w , ButtonPressMask ) ;

         /* compute which (x,y) point the button press was at */

         gx = grapher->gx ; gy = grapher->gy ; mat = grapher->mat ;

         i  = (bx - GL_DLX + gx) * mat / grapher->gx_max ;
         j  = (by - GT_DLY + gy) * mat / grapher->gy_max ;

         xloc = grapher->xpoint + i - (mat + 2)/2 ;
         yloc = grapher->ypoint + j - (mat + 2)/2 ;

         if (xloc < 0)                    xloc += grapher->status->nx ;
         if (xloc >= grapher->status->nx) xloc -= grapher->status->nx ;
         if (yloc < 0)                    yloc += grapher->status->ny ;
         if (yloc >= grapher->status->ny) yloc -= grapher->status->ny ;

         /* Feb 1998: button 2 --> send message back to AFNI, maybe */

         if( but == Button2 ){
            if( grapher->button2_enabled && (bx > GL_DLX) ){
               GRA_cbs cbs ;
               cbs.reason = graCR_button2_points ;
               cbs.xcen   = xloc ;
               cbs.ycen   = yloc ;
               cbs.zcen   = grapher->zpoint ;
               grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
            } else {
               XBell(XtDisplay(w),100) ;
            }
         }

         /* button 1 --> move to new square as center of matrix,
                         if it is actually a new center, that is,
                         and if graphing is enabled, and if not off the left edge */

         if( grapher->fd_pxWind != (Pixmap) 0 &&
             but == Button1                   && (bx > GL_DLX) &&
             ( (xloc != grapher->xpoint) || (yloc != grapher->ypoint) ) ){

               grapher->xpoint = xloc ;
               grapher->ypoint = yloc ;
               redraw_graph( grapher , 0 ) ;
               send_newinfo( grapher ) ;
         }

         /* 22 July 1996:
            button 1 in central graph of matrix
               causes jump to time_index;
            Shift or Ctrl button 1
               causes increment of time_index in indicated direction */

         else if( grapher->fd_pxWind != (Pixmap) 0 &&
                  (but==Button1)                   && (bx > GL_DLX) &&
                  (xloc == grapher->xpoint)        && (yloc == grapher->ypoint) ){

            /* conversion factor from time index to pixels,
               the same as is used in graphing the time series data */

            float ftemp = grapher->gx / (float) (NPTS(grapher) - 1) ;

            /*-- this is the linear time index --*/

            i = 0.49 + (bx - grapher->xorigin[grapher->xc][grapher->yc]) / ftemp ;

            /*-- step forward or backward if Shift or Ctrl is pressed --*/

            if( but_state & (ShiftMask|ControlMask) ){

                    if( i > grapher->time_index ) i = grapher->time_index + 1 ;
               else if( i < grapher->time_index ) i = grapher->time_index - 1 ;

            } else { /*-- 09 Jan 1998: find closest pixel in central graph --*/

               float dist , dmin=99999.9 ;
               int imin = 0 ;

               for( i=0 ; i < grapher->nncen ; i++ ){
                  dist =  abs( bx - grapher->cen_line[i].x )   /* L1 distance */
                        + abs( by - grapher->cen_line[i].y ) ;
                  if( dist < dmin ){ dmin = dist ; imin = i ; if( dmin == 0 ) break ; }
               }
               i = imin ;
            }

            if( i >= 0 && i < NPTS(grapher) ){
               if( grapher->status->send_CB != NULL ){
                  GRA_cbs cbs ;

                  cbs.reason = graCR_setindex ;
                  cbs.key    = i ;
                  cbs.event  = ev ;
                  grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
               } else {
                  (void) drive_MCW_grapher( grapher, graDR_setindex, (XtPointer) i ) ;
               }
            }
         }

         /* Button 1 in pixmap logo = toggle on or off */

         if( but == Button1 && grapher->logo_pixmap != XmUNSPECIFIED_PIXMAP &&
             bx < grapher->logo_width && grapher->fHIGH - by < grapher->logo_height ){

            show_grapher_pixmap = ! show_grapher_pixmap ;

            if( XtIsManaged(grapher->option_rowcol) )     /* 04 Nov 1996 */
               XtUnmanageChild(grapher->option_rowcol) ;
            else
               XtManageChild(grapher->option_rowcol) ;

            redraw_graph( grapher , 0 )  ;
         }

         /* Button 3 --> popup statistics of this graph */

         if( but == Button3 ){
            int ix , iy ;

            ix = xloc - grapher->xpoint + grapher->xc ;
                 if( ix <  0            ) ix += grapher->status->nx ;
            else if( ix >= grapher->mat ) ix -= grapher->status->nx ;

            iy = grapher->ypoint - yloc + grapher->yc ;
                 if( iy <  0            ) iy += grapher->status->ny ;
            else if( iy >= grapher->mat ) iy -= grapher->status->ny ;

            if( ix >= 0 && ix < grapher->mat && iy >= 0 && iy < grapher->mat ){
               XmString xstr ;
               char str[256] , bmin[16],bmax[16],bmean[16],bstd[16] ;

               AV_fval_to_char( grapher->tbot[ix][iy]  , bmin ) ;
               AV_fval_to_char( grapher->ttop[ix][iy]  , bmax ) ;
               AV_fval_to_char( grapher->tmean[ix][iy] , bmean ) ;
               AV_fval_to_char( grapher->tstd[ix][iy]  , bstd ) ;

               sprintf( str , "Ignore  = %d\n"
                              "x voxel = %d\n"
                              "y voxel = %d\n"
                              "z voxel = %d\n"
                              "min     =%s\n"
                              "max     =%s\n"
                              "mean    =%s\n"
                              "sigma   =%s"   ,
                        grapher->init_ignore ,
                        xloc , yloc , grapher->zpoint , bmin , bmax , bmean , bstd ) ;

                /** 22 Apr 1997: incorporate user string for this voxel **/

                if( grapher->tuser[ix][iy] == NULL ){
                   xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
                } else {
                   char * qstr ; int ll ;
                   ll   = strlen(str) + strlen(grapher->tuser[ix][iy]) + 8 ;
                   qstr = (char *) malloc( sizeof(char) * ll ) ;
                   strcpy( qstr , str ) ;
                   strcat( qstr , "\n\n" ) ;
                   strcat( qstr , grapher->tuser[ix][iy] ) ;
                   xstr = XmStringCreateLtoR( qstr , XmFONTLIST_DEFAULT_TAG ) ;
                   free(qstr) ;
                }

                XtVaSetValues( grapher->but3_label , XmNlabelString , xstr , NULL ) ;
                XmStringFree( xstr ) ;

                XmMenuPosition( grapher->but3_menu , event ) ; /* where */
                XtManageChild ( grapher->but3_menu ) ;         /* popup */
            } else {
               redraw_graph(grapher,0) ;  /* 11 Nov 1996 */
            }
         }
      }
      break ;

      /*----- window changed size -----*/

      case ConfigureNotify:{
         XConfigureEvent * event = (XConfigureEvent *) ev ;
         int new_width , new_height ;

STATUS("ConfigureNotify event") ;

         XSync( XtDisplay(w) , False ) ;

         new_width  = event->width ;
         new_height = event->height ;

         if( new_width != grapher->fWIDE || new_height != grapher->fHIGH ){
            GRA_new_pixmap( grapher , new_width , new_height , 1 ) ;
         }
      }
      break ;

      /*----- ignore all other events -----*/

      default:
#ifdef AFNI_DEBUG
{char str[256]; sprintf(str,"Event code = %d\n",(int)ev->type); STATUS(str);}
#endif
      break ;

   } /* end of switch ev->type */

   EXRETURN ;
}

/*------------------------------------------------------------------
   Get a new pixmap for drawing purposes
--------------------------------------------------------------------*/

void GRA_new_pixmap( MCW_grapher * grapher ,
                     int new_width , int new_height , int redraw )
{
   int ww,hh ;

ENTRY("GRA_new_pixmap") ;

   if( ! GRA_REALZ(grapher) ) EXRETURN ;

   grapher->fWIDE  = new_width ;
   grapher->fHIGH  = new_height ;
   grapher->gx_max = new_width  - (GL_DLX + GR_DLX) ;
   grapher->gy_max = new_height - (GT_DLY + GB_DLY) ;

   if( grapher->fd_pxWind != (Pixmap) 0 ){
STATUS("freeing old Pixmap") ;
      XFreePixmap( grapher->dc->display , grapher->fd_pxWind ) ;
   }

STATUS("allocating new Pixmap") ;
   grapher->fd_pxWind = XCreatePixmap( grapher->dc->display ,
                                       XtWindow(grapher->draw_fd) ,
                                       grapher->fWIDE , grapher->fHIGH,
                                       grapher->dc->planes ) ;

   MCW_widget_geom( grapher->option_rowcol , &ww , &hh , NULL,NULL ) ;
   XtVaSetValues( grapher->option_rowcol ,
#ifdef WANT_AFNI_BITMAP
                     XmNx , grapher->fWIDE - ww - 2 ,
#else
                     XmNx , 2 ,
#endif
                     XmNy , grapher->fHIGH - hh - 2 ,
                  NULL ) ;

   if( redraw ){
      init_mat( grapher ) ;
      redraw_graph( grapher , 0 ) ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------
  Deal with keypresses in a graph window
------------------------------------------------------------*/

void GRA_handle_keypress( MCW_grapher * grapher , char * buf , XEvent * ev )
{
   int ii ;

ENTRY("GRA_handle_keypress") ;

   if( buf[0] == '\0' ) EXRETURN ;

if(PRINT_TRACING){
char str[256] ;
sprintf(str,"buf[0]=%c (%x)",(int)buf[0],(int)buf[0]) ;
STATUS(str); }

   /*** deal with the key sequence 'N <digits> <Enter>' ***/

   /* first 'N' */

   if( grapher->key_Nlock==0 && buf[0]=='N' ){
      grapher->key_Nlock = 1 ;
      MCW_alter_widget_cursor( grapher->fdw_graph , -XC_hand2 ,"yellow","blue" ) ;
      grapher->key_lock_sum = 0 ;
      EXRETURN ;
   }

   /* last <Enter> */

   if( grapher->key_Nlock && buf[0] == 13 ){

      /* if have a number, set the graph matrix size */

      if( grapher->key_lock_sum > 0 )
         grapher->mat = MIN( grapher->mat_max , grapher->key_lock_sum ) ;

      MCW_alter_widget_cursor( grapher->fdw_graph , -XC_left_ptr ,"yellow","blue" ) ;

      init_mat    ( grapher ) ;
      redraw_graph( grapher , 0 ) ;
      send_newinfo( grapher ) ;
      grapher->key_Nlock = grapher->key_lock_sum = 0 ;
      EXRETURN ;
   }

   /* intermediate <digit> */

   if( grapher->key_Nlock ){
      if( isdigit(buf[0]) ){
         ii = buf[0] - 48;
         grapher->key_lock_sum = MIN( 10000, 10*grapher->key_lock_sum + ii ) ;
      }
      EXRETURN ;
   }

   /*-- other keys are single stroke commands --*/

   MCW_discard_events( grapher->draw_fd , KeyPressMask ) ;

   switch (buf[0]) {

      case '-':
      case '+':
         if( buf[0] == '-' ) scale_down( grapher ) ;
         else                scale_up  ( grapher ) ;
         redraw_graph( grapher , 0 ) ;
      break;

      case 'a':
         redraw_graph( grapher , PLOTCODE_AUTOSCALE ) ;  /* 03 Feb 1998 */
      break ;

      case 'm':
      case 'M':
         if( buf[0] == 'm' ) mat_down( grapher ) ;
         else                mat_up  ( grapher ) ;
         send_newinfo( grapher ) ;
      break;

      case 'g':
         grid_down( grapher ) ;
      break;

      case 'G':
         grid_up( grapher ) ;
      break;

      case 'h':   /* 05 Jan 1999 */
         grapher->HorZ = ! grapher->HorZ ;
         redraw_graph( grapher , 0 ) ;
      break ;

#if 0
      case 'r':
         grapher->grid_color = (grapher->grid_color + 1 ) % grapher->dc->ovc->ncol_ov ;
         redraw_graph( grapher , 0 ) ;
      break;
#endif

      case 'q':
      case 'Q':
         end_fd_graph_CB( NULL , (XtPointer) grapher , NULL ) ;
      break ;

      case 'b':
         grapher->common_base = ! grapher->common_base ;
         redraw_graph( grapher , 0 ) ;
      break ;

      case 'S':
         MCW_choose_string( grapher->option_mbar ,
                            "Save PNM Prefix:" , NULL ,
                            GRA_saver_CB , (XtPointer) grapher ) ;
      break ;

      case 'L':
         show_grapher_pixmap = ! show_grapher_pixmap ;
         if( grapher->logo_pixmap != XmUNSPECIFIED_PIXMAP )
            redraw_graph( grapher , 0 ) ;
      break ;

      case '<':
      case '>':
      case '1':
      case 'l':
              if( buf[0] == '<' ) ii = grapher->time_index - 1 ;
         else if( buf[0] == '>' ) ii = grapher->time_index + 1 ;
         else if( buf[0] == '1' ) ii = 1 ;
         else if( buf[0] == 'l' ) ii = grapher->status->num_series - 1 ;

         if( ii >= 0 && ii < grapher->status->num_series ){
            if( grapher->status->send_CB != NULL ){
               GRA_cbs cbs ;

               cbs.reason = graCR_setindex ;
               cbs.key    = ii;
               cbs.event  = NULL ;
               grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
            } else {
               (void) drive_MCW_grapher( grapher , graDR_setindex , (XtPointer) ii) ;
            }
         }
      break ;

      case 'z':
      case 'Z':
         if( buf[0] == 'z' ){
            grapher->zpoint -- ;
            if( grapher->zpoint < 0 ) grapher->zpoint = grapher->status->nz - 1 ;
         } else {
            grapher->zpoint ++ ;
            if( grapher->zpoint >= grapher->status->nz ) grapher->zpoint = 0 ;
         }
         redraw_graph( grapher , 0 ) ;
         send_newinfo( grapher ) ;
      break ;

      case 'w':{
         char * wcfname ;
         int ndig , ll ;
         MRI_IMAGE * tsim ;
         int xd,yd,zd ;     /* 24 Sep 1999 */

         ll   = MAX( grapher->status->nx , grapher->status->ny ) ;
         ll   = MAX( grapher->status->nz , ll ) ;
         ndig = (ll < 1000) ? 3 : 4 ;

         ll   = 3*ndig + 16 ;
         if( Grapher_Stuff.wcsuffix != NULL )
            ll += strlen(Grapher_Stuff.wcsuffix) ;
         wcfname = (char *) XtMalloc(ll) ;

         /* 24 Sep 1999: mangle the name for the output */

         xd = grapher->xpoint; yd = grapher->ypoint; zd = grapher->zpoint;
#ifndef DONT_MANGLE_XYZ
         { THD_ivec3 id;
           id = THD_fdind_to_3dind( grapher->getaux, TEMP_IVEC3(xd,yd,zd) );
           xd = id.ijk[0]; yd = id.ijk[1]; zd = id.ijk[2]; }
#endif

         if( Grapher_Stuff.wcsuffix != NULL )
            sprintf(wcfname,"%0*d_%0*d_%0*d.%s.1D" ,
                    ndig,xd , ndig,yd , ndig,zd ,
                              Grapher_Stuff.wcsuffix ) ;
         else
            sprintf(wcfname,"%0*d_%0*d_%0*d.1D" ,
                    ndig,xd , ndig,yd , ndig,zd  ) ;

         ll = grapher->xpoint +
              grapher->ypoint * grapher->status->nx +
              grapher->zpoint * grapher->status->nx * grapher->status->ny ;

         tsim  = (MRI_IMAGE *) grapher->getser( ll , graCR_getseries ,
                                                     grapher->getaux ) ;

         if( tsim != NULL ){
             mri_write_1D( wcfname , tsim ) ;  /* 16 Nov 1999: replaces mri_write_ascii */
             mri_free( tsim ) ;
         }
         myXtFree(wcfname) ;
      }
      break ;

      /*--- At this point, have a key not handled here.
            Call the creator to see if it wishes to deal with it. ---*/

      default:
         if( grapher->status->send_CB != NULL ){
            GRA_cbs cbs ;

            cbs.reason = graCR_keypress ;
            cbs.key    = buf[0] ;
            cbs.event  = ev ;
            grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
         }
      break ;
   }

   EXRETURN ;
}

/*--------------------------------------------------------------------------
   06 Jan 1999: handle death after a timeout.
----------------------------------------------------------------------------*/

void GRA_quit_timeout_CB( XtPointer client_data , XtIntervalId * id )
{
   MCW_grapher * grapher = (MCW_grapher *) client_data ;

ENTRY("GRA_quit_timeout_CB") ;
   GRA_handle_keypress( grapher , "q" , NULL ) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------------
   Handle buttons from the opt menu
----------------------------------------------------------------------------*/

void GRA_opt_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_grapher * grapher = (MCW_grapher *) client_data ;

ENTRY("GRA_opt_CB") ;

   if( w == grapher->opt_scale_down_pb ){
      GRA_handle_keypress( grapher , "-" , NULL ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_scale_up_pb ){
      GRA_handle_keypress( grapher , "+" , NULL ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_scale_auto_pb ){
      GRA_handle_keypress( grapher , "a" , NULL ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_grid_down_pb ){
      GRA_handle_keypress( grapher , "g" , NULL ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_grid_up_pb ){
      GRA_handle_keypress( grapher , "G" , NULL ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_grid_HorZ_pb ){     /* 05 Jan 1999 */
      GRA_handle_keypress( grapher , "h" , NULL ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_slice_down_pb ){
      GRA_handle_keypress( grapher , "z" , NULL ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_slice_up_pb ){
      GRA_handle_keypress( grapher , "Z" , NULL ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_mat_down_pb ){
      GRA_handle_keypress( grapher , "m" , NULL ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_mat_up_pb ){
      GRA_handle_keypress( grapher , "M" , NULL ) ;
      EXRETURN ;
   }

#if 0
   if( w == grapher->opt_color_up_pb ){
      GRA_handle_keypress( grapher , "r" , NULL ) ;
      EXRETURN ;
   }
#endif

   if( w == grapher->opt_quit_pb ){
#if 0
      GRA_handle_keypress( grapher , "q" , NULL ) ;
#else
STATUS("User pressed Done button: starting timeout") ;
      grapher->valid = 666 ;
      (void) XtAppAddTimeOut( XtWidgetToApplicationContext(w) ,
                              50 , GRA_quit_timeout_CB , grapher ) ;
#endif
      EXRETURN ;
   }

   if( w == grapher->opt_baseline_pb ){
      GRA_handle_keypress( grapher , "b" , NULL ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_save_pb ){
      GRA_handle_keypress( grapher , "S" , NULL ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_write_center_pb ){
      GRA_handle_keypress( grapher , "w" , NULL ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_write_suffix_pb ){
      MCW_choose_string( grapher->option_mbar ,
                         "'Write Center' Suffix:" , Grapher_Stuff.wcsuffix ,
                         GRA_wcsuffix_choose_CB , NULL ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_scale_choose_pb ){
      MCW_choose_integer( grapher->option_mbar , "Scale" ,
                          -9999 , 9999 , (int)(grapher->fscale) ,
                          GRA_scale_choose_CB , (XtPointer) grapher ) ;
      EXRETURN ;
   }

#ifndef USE_OPTMENUS
   if( w == grapher->opt_mat_choose_pb ){
      MCW_choose_integer( grapher->option_mbar , "Matrix" ,
                          1 , grapher->mat_max , grapher->mat ,
                          GRA_mat_choose_CB , (XtPointer) grapher ) ;
      EXRETURN ;
   }
#endif

   if( w == grapher->opt_grid_choose_pb ){
      MCW_choose_integer( grapher->option_mbar , "Grid" ,
                          grid_ar[0] , grid_ar[GRID_MAX-1] , grapher->grid_spacing ,
                          GRA_grid_choose_CB , (XtPointer) grapher ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_pin_choose_pb ){   /* 27 Apr 1997 */
      MCW_choose_integer( grapher->option_mbar , "Pin Num" ,
                          0 , MAX_PIN , grapher->pin_num ,
                          GRA_pin_choose_CB , (XtPointer) grapher ) ;
      EXRETURN ;
   }

#ifndef USE_OPTMENUS
   if( w == grapher->opt_slice_choose_pb && grapher->status->nz > 1 ){
      MCW_choose_integer( grapher->option_mbar , "Slice" ,
                          0 , grapher->status->nz - 1 , grapher->zpoint ,
                          GRA_slice_choose_CB , (XtPointer) grapher ) ;
      EXRETURN ;
   }
#endif

   /*** 09 Jan 1998: x-axis stuff ***/

   if( w == grapher->opt_xaxis_clear_pb ){
      mri_free( grapher->xax_tsim ) ;
      grapher->xax_tsim = NULL ;
      redraw_graph( grapher , 0 ) ;
      EXRETURN ;
   }

   if( w == grapher->opt_xaxis_pick_pb ){
      if( IMARR_COUNT(GLOBAL_library.timeseries) > 0 ){
        MCW_choose_timeseries( grapher->fdw_graph , "Graph x-axis" ,
                               GLOBAL_library.timeseries , -1 ,
                               GRA_pick_xaxis_CB , (XtPointer) grapher ) ;
      } else {
        (void) MCW_popup_message(
                  grapher->option_mbar ,
                  "No timeseries library\nexists to pick from!" ,
                  MCW_USER_KILL | MCW_TIMER_KILL ) ;
      }
      EXRETURN ;
   }

   if( w == grapher->opt_xaxis_center_pb ){
      if( grapher->cen_tsim != NULL ){
         mri_free( grapher->xax_tsim ) ;
         grapher->xax_tsim = mri_to_float( grapher->cen_tsim ) ;
         redraw_graph(grapher,0) ;
      } else {
         XBell(XtDisplay(w),100) ;
      }
      EXRETURN ;
   }

   /** shouldn't get to here, but who knows? **/

   EXRETURN ;
}

/*----------------------------------------------------------------------*/

void GRA_fixup_xaxis( MCW_grapher * grapher )  /* 09 Jan 1998 */
{
   int ii , npt , nx , ibot , nover=0 ;
   float top,bot , fac ;
   float * xxx ;

ENTRY("GRA_fixup_xaxis") ;

   if( !GRA_VALID(grapher) || grapher->xax_tsim == NULL ) EXRETURN ;

   npt = NPTS(grapher) ; nx = grapher->xax_tsim->nx ; npt = MIN(npt,nx) ;
   xxx = MRI_FLOAT_PTR(grapher->xax_tsim) ;

   ibot = grapher->init_ignore ;
   if( ibot >= npt ) ibot = 0 ;

   /* find range over plotting interval (ibot..npt-1) */

   top = -WAY_BIG ; bot = WAY_BIG ;
   for( ii=ibot ; ii < npt ; ii++ ){
      if( xxx[ii] < WAY_BIG ){
         top = MAX(top,xxx[ii]) ; bot = MIN(bot,xxx[ii]) ;
      } else {
         nover++ ;
      }
   }
   if( bot >= top ){
      mri_free(grapher->xax_tsim) ;
      grapher->xax_tsim = NULL ;
      EXRETURN ;
   }
   if( nover == 0 && fabs(top-1.0) < 0.001 && fabs(bot) < 0.001 ) EXRETURN ;

   /* scale all of the timeseries */

   fac = 1.0 / (top-bot) ;
   for( ii=0 ; ii < nx ; ii++ )
      if( xxx[ii] < WAY_BIG ) xxx[ii] = fac * (xxx[ii]-bot) ;
      else                    xxx[ii] = 0.0 ;

   EXRETURN ;
}

/*----------------------------------------------------------------------*/

void GRA_pick_xaxis_CB( Widget wcall , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;
   int its ;
   MRI_IMAGE * tsim ;

ENTRY("GRA_pick_xaxis_CB") ;

   if( !GRA_VALID(grapher) || cbs->reason != mcwCR_timeseries ) EXRETURN ;

   its = cbs->ival ;
   if( its >= 0 && its < IMARR_COUNT(GLOBAL_library.timeseries) ){
      tsim = IMARR_SUBIMAGE(GLOBAL_library.timeseries,its) ;
      mri_free( grapher->xax_tsim ) ;
      grapher->xax_tsim = mri_to_float(tsim) ;
      redraw_graph( grapher , 0 ) ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------
   Callbacks from popup choosers
------------------------------------------------------------------------*/

void GRA_wcsuffix_choose_CB( Widget wcaller , XtPointer cd , MCW_choose_cbs * cbs )
{
   int ll , ii ;

ENTRY("GRA_wcsuffix_choose_CB") ;

   if( cbs->reason != mcwCR_string ||
       cbs->cval   == NULL         || (ll=strlen(cbs->cval)) == 0 ){

      XBell( XtDisplay(wcaller) , 100 ) ; EXRETURN ;
   }

   for( ii=0 ; ii < ll ; ii++ ){
      if( iscntrl(cbs->cval[ii]) ||
          isspace(cbs->cval[ii]) ||
          cbs->cval[ii] == '/'     ){

         XBell( XtDisplay(wcaller) , 100 ) ; EXRETURN ;
      }
   }

   if( Grapher_Stuff.wcsuffix != NULL ) myXtFree(Grapher_Stuff.wcsuffix) ;

   Grapher_Stuff.wcsuffix = XtNewString(cbs->cval) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

#ifdef USE_OPTMENUS
void GRA_mat_choose_CB( MCW_arrowval * cbs , XtPointer cd )
#else
void GRA_mat_choose_CB( Widget wcaller , XtPointer cd , MCW_choose_cbs * cbs )
#endif
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;

ENTRY("GRA_mat_choose_CB") ;

   if( ! GRA_VALID(grapher) ) EXRETURN ;

   grapher->mat = MIN( grapher->mat_max , cbs->ival ) ;
   init_mat    ( grapher ) ;
   redraw_graph( grapher , 0 ) ;
   send_newinfo( grapher ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

void GRA_scale_choose_CB( Widget wcaller , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;

ENTRY("GRA_scale_choose_CB") ;

   if( ! GRA_VALID(grapher) ) EXRETURN ;

   grapher->fscale = cbs->fval ;
   redraw_graph( grapher , 0 ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

void GRA_grid_choose_CB( Widget wcaller , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;

ENTRY("GRA_grid_choose_CB") ;

   if( ! GRA_VALID(grapher) ) EXRETURN ;
   grapher->grid_spacing = cbs->ival ;
   redraw_graph(grapher,0) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

void GRA_pin_choose_CB( Widget wcaller , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;

ENTRY("GRA_pin_choose_CB") ;

   if( ! GRA_VALID(grapher) ) EXRETURN ;
   grapher->pin_num = cbs->ival ;
   redraw_graph(grapher,0) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

void GRA_ggap_CB( MCW_arrowval * cbs , XtPointer cd )  /* 12 Jan 1998 */
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;
   int gg ;

ENTRY("GRA_ggap_CB") ;

   if( ! GRA_VALID(grapher) ) EXRETURN ;

   gg = grapher->ggap ; grapher->ggap = cbs->ival ;
   if( gg != grapher->ggap ){
      init_mat( grapher ) ; redraw_graph( grapher , 0 ) ;
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

#ifdef USE_OPTMENUS
void GRA_slice_choose_CB( MCW_arrowval * cbs , XtPointer cd )
#else
void GRA_slice_choose_CB( Widget wcaller , XtPointer cd , MCW_choose_cbs * cbs )
#endif
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;

ENTRY("GRA_slice_choose_CB") ;

   if( ! GRA_VALID(grapher) ) EXRETURN ;

   grapher->zpoint = cbs->ival ;
   if( grapher->zpoint >= grapher->status->nz )
      grapher->zpoint = grapher->status->nz - 1 ;
   redraw_graph( grapher , 0 ) ;
   send_newinfo( grapher ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

#ifdef USE_OPTMENUS
void GRA_ignore_choose_CB( MCW_arrowval * cbs , XtPointer cd )
#else
void GRA_ignore_choose_CB( Widget wcaller , XtPointer cd , MCW_choose_cbs * cbs )
#endif
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;

ENTRY("GRA_ignore_choose_CB") ;

   if( ! GRA_VALID(grapher) || grapher->status->send_CB == NULL ) EXRETURN ;

   if( cbs->ival >= 0 && cbs->ival < grapher->status->num_series-1 ){
      GRA_cbs gbs ;

      gbs.reason = graCR_setignore ;
      gbs.key    = cbs->ival ;
      grapher->status->send_CB( grapher , grapher->getaux , &gbs ) ;
   }
   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

#ifdef USE_OPTMENUS
void GRA_polort_choose_CB( MCW_arrowval * cbs , XtPointer cd )
#else
void GRA_polort_choose_CB( Widget wcaller , XtPointer cd , MCW_choose_cbs * cbs )
#endif
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;

ENTRY("GRA_polort_choose_CB") ;

   if( ! GRA_VALID(grapher) || grapher->status->send_CB == NULL ) EXRETURN ;

   if( cbs->ival >= 0 && cbs->ival <= MAX_POLORT ){
      GRA_cbs gbs ;

      gbs.reason = graCR_polort ;
      gbs.key    = cbs->ival ;
      grapher->status->send_CB( grapher , grapher->getaux , &gbs ) ;
   }
   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

void GRA_bkthr_choose_CB( Widget wcaller , XtPointer cd , MCW_choose_cbs * cbs )
{
ENTRY("GRA_bkthr_choose_CB") ;
   SET_FIM_bkthr( cbs->fval ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

void GRA_refread_choose_CB( Widget wcaller , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;
   MRI_IMAGE * flim ;
   float * far ;
   int ii ;
   GRA_cbs gbs ;

ENTRY("GRA_refread_choose_CB") ;

   if( ! GRA_VALID(grapher)             ||
       grapher->status->send_CB == NULL ||
       cbs->reason != mcwCR_string      ||
       cbs->cval == NULL                || strlen(cbs->cval) == 0 ) EXRETURN ;

   flim = mri_read_1D( cbs->cval ) ;     /* 16 Nov 1999: replaces mri_read_ascii */
   if( flim == NULL || flim->nx < 2 ){
      XBell(grapher->dc->display,100) ; mri_free(flim) ; EXRETURN ;
   }

   far = MRI_FLOAT_PTR(flim) ;
   for( ii=0 ; ii < flim->nvox ; ii++ )
      if( fabs(far[ii]) >= 33333.0 ) far[ii] = WAY_BIG ;

   gbs.reason   = graCR_refequals ;
   gbs.userdata = (XtPointer) flim ;
   grapher->status->send_CB( grapher , grapher->getaux , &gbs ) ;
   mri_free(flim) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

void GRA_refstore_choose_CB( Widget wcaller , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;

ENTRY("GRA_refstore_choose_CB") ;

   if( ! GRA_VALID(grapher)             ||
       grapher->ref_ts == NULL          ||
       IMARR_COUNT(grapher->ref_ts) < 1 ||
       cbs->reason != mcwCR_string      ||
       cbs->cval == NULL                || strlen(cbs->cval) == 0 ) EXRETURN ;

   PLUTO_register_timeseries( cbs->cval , IMARR_SUBIMAGE(grapher->ref_ts,0) ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

void GRA_refwrite_choose_CB( Widget wcaller , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;
   MRI_IMAGE * tsim ;
   int ii , ll ;
   GRA_cbs gbs ;

ENTRY("GRA_refwrite_choose_CB") ;

   if( ! GRA_VALID(grapher)             ||
       grapher->ref_ts == NULL          ||
       IMARR_COUNT(grapher->ref_ts) < 1 ||
       cbs->reason != mcwCR_string      ||
       cbs->cval == NULL                || (ll=strlen(cbs->cval)) == 0 ) EXRETURN ;

   for( ii=0 ; ii < ll ; ii++ ){
      if( iscntrl(cbs->cval[ii]) || isspace(cbs->cval[ii]) ||
          cbs->cval[ii] == '/'   || cbs->cval[ii] == ';'   ||
          cbs->cval[ii] == '*'   || cbs->cval[ii] == '?'   ||
          cbs->cval[ii] == '&'   || cbs->cval[ii] == '|'   ||
          cbs->cval[ii] == '"'   || cbs->cval[ii] == '>'   ||
          cbs->cval[ii] == '<'   || cbs->cval[ii] == '\''  ||
          cbs->cval[ii] == '['   || cbs->cval[ii] == ']'     ){

         XBell( XtDisplay(wcaller) , 100 ) ; EXRETURN ;
      }
   }

#if 0
   tsim = mri_transpose( IMARR_SUBIMAGE(grapher->ref_ts,0) ) ;
   mri_write_ascii( cbs->cval , tsim ) ;
   mri_free( tsim ) ;
#else
   mri_write_1D( cbs->cval , IMARR_SUBIMAGE(grapher->ref_ts,0) ) ; /* 16 Nov 1999 */
#endif

   /* 12 Nov 1996: put this in AFNI's list of library files */

   if( grapher->status->send_CB != NULL ){
      mri_add_name( cbs->cval , IMARR_SUBIMAGE(grapher->ref_ts,0) ) ;
      gbs.reason   = graCR_timeseries_library ;
      gbs.userdata = (XtPointer) IMARR_SUBIMAGE(grapher->ref_ts,0) ;
      grapher->status->send_CB( grapher , grapher->getaux , &gbs ) ;
   }
   EXRETURN ;
}

/*-----------------------------------------------------------------------
   External interface to drive an MCW_grapher:
     grapher    = pointer to structure returned by new_MCW_grapher
     drive_code = integer indicating which action to take
     drive_data = data or pointer to data controlling action
                  (you will probably have to cast this to
                   XtPointer to avoid ugly warnings from the compiler)

OK   drive_code       drive_data should be
--   ----------       --------------------
*    graDR_cursor     (int) with new cursor id for the image window;
                       (if negative, means from cursorfont)

*    graDR_realize    (ignored) an unrealized viewer is re-realized

*    graDR_redraw     (int *) array of 4 ints: x,y,z,m
                        redraw the graph centered at x,y,z with matrix m

*    graDR_destroy    (ignored) destroy this MCW_grapher, and delete its
                        own XtMalloc-ed internal data structures;
                        after this call, you must myXtFree(grapher) to finish
                        the job.

*    graDR_newdata    (XtPointer) contains new auxiliary data for getser;
                        this call switches the data sequence to a
                        new one entirely.  As with imseq, this should
                        be followed by a call with graDR_redraw.

*    graDR_title      (char *) contains new string for window title bar

*    graDR_icon       (Pixmap) sets the icon for this window

*    graDR_addref_ts  (MRI_IMAGE *) adds a reference timeseries to the list
                        input == NULL --> delete all references
                  ** N.B.: The input timeseries will not be copied, just
                           will keep a pointer to it.  Thus, the input
                           timeseries should not be destroyed.

*    graDR_addort_ts  (MRI_IMAGE *) adds an ort timeseries to the list
                        input == NULL --> delete all orts

*    graDR_setignore  (int) set the initial ignore level for graphs

*    graDR_polort     (int) set the polort level for fimmery

*    graDR_setindex   (int) contains the new time_index;
                        All this does is draw some stuff.

*    graDR_newlength  (int) set the expected length of time series
                        to be some new value.

*    graDR_button2_enable  (ignored) Turn button2 reporting on
*    graDR_button2_disable (ignored) and off.

*    graDR_fim_disable     (ignored) Turn all FIM stuff off (for good)

The Boolean return value is True for success, False for failure.
-------------------------------------------------------------------------*/

Boolean drive_MCW_grapher( MCW_grapher * grapher ,
                           int drive_code , XtPointer drive_data )
{

ENTRY("drive_MCW_grapher") ;

   if( ! GRA_VALID(grapher) ) RETURN(False) ;

   switch( drive_code ){

      /*------- error! -------*/

      default:{
         fprintf(stderr,"\a\n*** drive_MCW_grapher: code=%d illegal!\n",
                 drive_code) ;
         XBell( grapher->dc->display , 100 ) ;
         RETURN( False ) ;
      }
      break ;

      /*------ fim disabling -----*/

      case graDR_fim_disable:{
         int ii ;
         XtUnmanageChild( grapher->fmenu->fim_cbut ) ;
         XtUnmanageChild( grapher->opt_xaxis_cbut ) ;
         for( ii=0 ; ii < NUM_COLOR_ITEMS ; ii++ ){
            if( gr_unfim[ii] ){
               if( grapher->opt_color_av[ii] != NULL )
                  XtUnmanageChild( grapher->opt_color_av[ii]->wrowcol ) ;
               if( grapher->opt_thick_bbox[ii] != NULL )
                  XtUnmanageChild( grapher->opt_thick_bbox[ii]->wtop ) ;
               if( grapher->opt_points_bbox[ii] != NULL )
                  XtUnmanageChild( grapher->opt_points_bbox[ii]->wtop ) ;
            }
         }
      }

      /*------ button2 stuff -----*/

      case graDR_button2_enable:{
         grapher->button2_enabled = 1 ;
         RETURN( True ) ;
      }

      case graDR_button2_disable:{
         grapher->button2_enabled = 0 ;
         RETURN( True ) ;
      }

      /*------ set time index -----*/

      case graDR_setindex:{
         int new_index = (int) drive_data ;

         if( new_index < 0 || new_index >= grapher->status->num_series )
            RETURN( False ) ;

         if( new_index != grapher->time_index ){
            grapher->time_index = new_index ;
            GRA_redraw_overlay( grapher ) ;
         }
         RETURN( True ) ;
      }
      break ;

      /*------ reset length of time series -----*/
      /*------ (all else remains the same) -----*/

      case graDR_newlength:{
         int new_length = (int) drive_data ;

         if( new_length < 2 || new_length > MAX_PIN ) RETURN( False ) ;

         grapher->pin_num = new_length ;   /* 27 Apr 1997 */

#ifdef USE_OPTMENUS
         GRA_fix_optmenus( grapher ) ;
#endif
         redraw_graph( grapher , 0 ) ;
         RETURN( True ) ;
      }
      break ;


      /*------ set ignore count -----*/

      case graDR_setignore:{
         int new_ignore = (int) drive_data ;

/**
         if( new_ignore >= 0 && new_ignore < grapher->status->num_series-1 ){}
**/
         if( new_ignore >= 0 ){
            grapher->init_ignore = new_ignore ;
            redraw_graph( grapher , 0 ) ;
            RETURN( True ) ;
         } else {
            RETURN( False ) ;
         }
      }
      break ;

      /*------- set polort [27 May 1999] --------*/

      case graDR_polort:{
         int new_polort = (int) drive_data ;

         if( new_polort >= 0 ){
            grapher->polort = new_polort ;
#ifdef USE_OPTMENUS
            GRA_fix_optmenus( grapher ) ;
#endif
         }
         RETURN( True ) ;
      }
      break ;

      /*----- set reference time series (currently limited to one) -----*/

      case graDR_addref_ts:{
         MRI_IMAGE * im = (MRI_IMAGE *) drive_data ;

         if( im == NULL ){                /* no input --> kill kill kill */
STATUS("freeing reference timeseries") ;
            FREE_IMARR(grapher->ref_ts) ;
            grapher->ref_ts = NULL ;
         } else{
            if( grapher->ref_ts == NULL ) INIT_IMARR( grapher->ref_ts ) ;

            if( IMARR_COUNT(grapher->ref_ts) == 0 ){
STATUS("adding reference timeseries") ;
               ADDTO_IMARR( grapher->ref_ts , im ) ;     /* create first one */
            } else {
STATUS("replacing reference timeseries") ;
               IMARR_SUBIMAGE(grapher->ref_ts,0) = im ;  /* replace first one */
            }
         }

         redraw_graph( grapher , 0 ) ;
         RETURN( True ) ;
      }

      /*----- set ort time series (currently limited to one) -----*/

      case graDR_addort_ts:{
         MRI_IMAGE * im = (MRI_IMAGE *) drive_data ;

         if( im == NULL ){                /* no input --> kill kill kill */
STATUS("freeing ort timeseries") ;
            FREE_IMARR(grapher->ort_ts) ;
            grapher->ort_ts = NULL ;
         } else{
            if( grapher->ort_ts == NULL ) INIT_IMARR( grapher->ort_ts ) ;

            if( IMARR_COUNT(grapher->ort_ts) == 0 ){
STATUS("adding ort timeseries") ;
               ADDTO_IMARR( grapher->ort_ts , im ) ;     /* create first one */
            } else {
STATUS("replacing ort timeseries") ;
               IMARR_SUBIMAGE(grapher->ort_ts,0) = im ;  /* replace first one */
            }
         }

         redraw_graph( grapher , 0 ) ;
         RETURN( True ) ;
      }

      /*------ set icon pixmap -----*/

      case graDR_icon:{
         int xret , yret ;
         unsigned int wret,hret,bret,dret ;
         Window rret ;

         XtVaSetValues( grapher->fdw_graph , XmNiconPixmap , (Pixmap) drive_data , NULL ) ;
         grapher->logo_pixmap = (Pixmap) drive_data ;

         /* get geometry for later use */

         if( grapher->logo_pixmap != XmUNSPECIFIED_PIXMAP ){
            (void) XGetGeometry( grapher->dc->display , grapher->logo_pixmap ,
                                 &rret , &xret , &yret , &wret , &hret , &bret , &dret ) ;

            grapher->logo_width  = wret ;
            grapher->logo_height = hret ;
         } else {
            grapher->logo_width  = 0 ;
            grapher->logo_height = 0 ;
         }
         RETURN( True ) ;
      }
      break ;

      /*------- title --------*/

      case graDR_title:{
         char * title = (char *) drive_data ;

         if( title == NULL || strlen(title) == 0 ) RETURN( False ) ;

         XtVaSetValues( grapher->fdw_graph , XmNtitle , title , NULL ) ;
         RETURN( True ) ;
      }
      break ;

      /*------- death! -------*/

      case graDR_destroy:{
         end_fd_graph_CB( NULL , (XtPointer) grapher , NULL ) ;
         RETURN( True ) ;
      }
      break ;

      /*------- unrealize! -------*/

      case graDR_unrealize:{
         if( GRA_REALZ(grapher) ) XtUnrealizeWidget( grapher->fdw_graph ) ;
         grapher->valid = 1 ;

         if( grapher->fd_pxWind != (Pixmap) 0 )
            XFreePixmap( grapher->dc->display , grapher->fd_pxWind ) ;
         RETURN( True ) ;
      }
      break ;

      /*------- realize! -------*/

      case graDR_realize:{
         if( ! GRA_REALZ(grapher) ){
            int width , height ;

            grapher->valid = 2 ;

            XtRealizeWidget( grapher->fdw_graph ) ;
            while( XtWindow(grapher->fdw_graph) == (Window) NULL ) ; /* wait */

            MCW_alter_widget_cursor(grapher->fdw_graph,-XC_left_ptr,"yellow","blue") ;

            MCW_widget_geom( grapher->draw_fd , &width , &height , NULL,NULL ) ;
            GRA_new_pixmap( grapher , width , height , 0 ) ;
#ifdef USE_OPTMENUS
            GRA_fix_optmenus( grapher ) ;
#endif
         }
         RETURN( True ) ;
      }
      break ;

      /*------- new cursor for image -------*/

      case graDR_cursor:{
         int cur = (int) drive_data ;

         MCW_alter_widget_cursor( grapher->fdw_graph , cur , "yellow" , "blue" ) ;
         RETURN( True ) ;
      }
      break ;

      /*------- new data sequence!!! -------*/

      case graDR_newdata:{
         grapher->status = (MCW_grapher_status *)
                              grapher->getser(0,graCR_getstatus,drive_data) ;
         grapher->getaux = drive_data ;
         init_const( grapher ) ;
#ifdef USE_OPTMENUS
         GRA_fix_optmenus( grapher ) ;
#endif
         RETURN( True ) ;
      }
      break ;

      /*------- redraw -------*/

      case graDR_redraw:{
         int * xym = (int *) drive_data ;

STATUS("graDR_redraw") ;

         if( xym != NULL ){
            if( xym[0] >= 0 ) grapher->xpoint = xym[0] ;
            if( xym[1] >= 0 ) grapher->ypoint = xym[1] ;
            if( xym[2] >= 0 ) grapher->zpoint = xym[2] ;
            if( xym[3] >  0 ) grapher->mat    = xym[3] ;
            init_mat(grapher) ;
         }
         redraw_graph( grapher , 0 ) ;
         RETURN( True ) ;
      }

   }  /* end of switch on drive_code */

   RETURN( False ) ;  /* should never be reached! */
}

/*------------------------------------------------------------------
   Callback for all FIM menu buttons
--------------------------------------------------------------------*/

void GRA_fim_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   FIM_menu * fm = (FIM_menu *) client_data ;
   MCW_grapher * grapher = (MCW_grapher *) fm->parent ;
   GRA_cbs cbs ;

ENTRY("GRA_fim_CB") ;

   if( ! GRA_VALID(grapher) || grapher->status->send_CB == NULL ) EXRETURN ;

   /*--- carry out action, depending on which widget called me ---*/

   /*** Pick reference ***/

   if( w == grapher->fmenu->fim_pickref_pb ){
      cbs.reason = graCR_pickref ;
      grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
   }

   /*** Pick ort ***/

   else if( w == grapher->fmenu->fim_pickort_pb ){
      cbs.reason = graCR_pickort ;
      grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
   }

   /*** Clear FIM ***/

   else if( w == grapher->fmenu->fim_editref_clear_pb ){
      cbs.reason = graCR_clearfim ;
      grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
   }

   /*** Clear Ort ***/

   else if( w == grapher->fmenu->fim_editort_clear_pb ){
      cbs.reason = graCR_clearort ;
      grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
   }

   /*** set ref to central time series ***/

   else if( w == grapher->fmenu->fim_editref_equals_pb ||
            w == grapher->fmenu->fim_editref_add_pb      ){
      int ll ;
      MRI_IMAGE * tsim ;

      ll = grapher->xpoint +
           grapher->ypoint * grapher->status->nx +
           grapher->zpoint * grapher->status->nx * grapher->status->ny ;

      tsim  = (MRI_IMAGE *) grapher->getser( ll , graCR_getseries ,
                                                  grapher->getaux ) ;

      if( tsim != NULL ){
         cbs.reason   = (w == grapher->fmenu->fim_editref_equals_pb)
                        ? graCR_refequals : graCR_refadd ;
         cbs.userdata = (XtPointer) tsim ;
         grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
         mri_free( tsim ) ;
      }
   }

   /*** read or write or smooth ***/

   else if( w == grapher->fmenu->fim_editref_read_pb ){
      MCW_choose_string( grapher->option_mbar ,
                         "Ideal Input Filename:" , NULL ,
                         GRA_refread_choose_CB , (XtPointer) grapher ) ;
   }

   else if( w == grapher->fmenu->fim_editref_write_pb ){
      if( grapher->ref_ts != NULL && IMARR_COUNT(grapher->ref_ts) > 0 ){
         MCW_choose_string( grapher->option_mbar ,
                            "Ideal Output Filename:" , NULL ,
                            GRA_refwrite_choose_CB , (XtPointer) grapher ) ;
      } else {
         XBell( grapher->dc->display , 100 ) ;
      }
   }

   else if( w == grapher->fmenu->fim_editref_store_pb ){
      if( grapher->ref_ts != NULL && IMARR_COUNT(grapher->ref_ts) > 0 ){
         MCW_choose_string( grapher->option_mbar ,
                            "Label to Store Ideal:" , NULL ,
                            GRA_refstore_choose_CB , (XtPointer) grapher ) ;
      } else {
         XBell( grapher->dc->display , 100 ) ;
      }
   }

   else if( w == grapher->fmenu->fim_editref_smooth_pb ){
      cbs.reason = graCR_refsmooth ;
      if( grapher->ref_ts != NULL && IMARR_COUNT(grapher->ref_ts) > 0 ){
         grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
      } else {
         XBell( grapher->dc->display , 100 ) ;
      }
   }

   /*** Set shifts ***/

   else if( w == grapher->fmenu->fim_editref_setshift_pb ){
      GRA_setshift_startup( grapher ) ;
   }

   /*** Execute FIM ***/

   else if( w == grapher->fmenu->fim_execute_pb ){
      int val = MCW_val_bbox(grapher->fmenu->fim_opt_bbox) ;
      cbs.reason = graCR_dofim ;
      switch(val){
         default:  cbs.key = FIM_ALPHA_MASK | FIM_CORR_MASK ; break ;
         case 2:   cbs.key = FIM_PERC_MASK  | FIM_CORR_MASK ; break ;
         case 4:   cbs.key = FIM_PAVE_MASK  | FIM_CORR_MASK ; break ;
      }
      grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
   }

   else if( w == grapher->fmenu->fim_execfimp_pb ){
      cbs.reason = graCR_dofim ;
      cbs.key    = MCW_val_bbox(grapher->fmenu->fimp_opt_bbox) ;
      if( cbs.key > 0 )
         grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
      else
         XBell( grapher->dc->display , 100 ) ;
   }

   /*** FIM plotting buttons ***/

   else if( w == grapher->fmenu->fim_plot_firstref_pb ){
      if( grapher->ref_ts_plotall != 0 ){
         grapher->ref_ts_plotall = 0 ;
         redraw_graph( grapher , 0 ) ;
      }
   }

   else if( w == grapher->fmenu->fim_plot_allrefs_pb ){
      if( grapher->ref_ts_plotall == 0 ){
         grapher->ref_ts_plotall = 1 ;
         redraw_graph( grapher , 0 ) ;
      }
   }

   /*** Ignore stuff ***/

   else if( w == grapher->fmenu->fim_ignore_down_pb && grapher->status->send_CB != NULL ){
      GRA_cbs cbs ;

      cbs.reason = graCR_setignore ;
      cbs.key    = grapher->init_ignore - 1 ;
      grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
   }

   else if( w == grapher->fmenu->fim_ignore_up_pb && grapher->status->send_CB != NULL ){
      GRA_cbs cbs ;

      cbs.reason = graCR_setignore ;
      cbs.key    = grapher->init_ignore + 1 ;
      grapher->status->send_CB( grapher , grapher->getaux , &cbs ) ;
   }

   else if( w == grapher->fmenu->fim_ignore_choose_pb && grapher->status->send_CB != NULL ){
#ifdef USE_OPTMENUS
      GRA_ignore_choose_CB( grapher->fmenu->fim_ignore_choose_av , grapher ) ;
#else
      MCW_choose_integer( grapher->option_mbar , "Initial Ignore" ,
                          0 , grapher->status->num_series-1 , grapher->init_ignore ,
                          GRA_ignore_choose_CB , (XtPointer) grapher ) ;
#endif
   }

   /* 27 May 1999: set polort */

   else if( w == grapher->fmenu->fim_polort_choose_pb && grapher->status->send_CB != NULL ){
#ifdef USE_OPTMENUS
      GRA_polort_choose_CB( grapher->fmenu->fim_polort_choose_av , grapher ) ;
#else
      MCW_choose_integer( grapher->option_mbar , "Polort Order" ,
                          0 , MAX_POLORT , grapher->polort ,
                          GRA_polort_choose_CB , (XtPointer) grapher ) ;
#endif
   }

   /* 02 Jun 1999: set FIM bkg threshold */

   else if( w == grapher->fmenu->fim_bkthr_choose_pb ){
      MCW_choose_integer( grapher->option_mbar , "Bkg Thresh %" ,
                          0 , 99 , (int)(100*FIM_THR) ,
                          GRA_bkthr_choose_CB , (XtPointer) grapher ) ;
   }

   /*** Unimplemented Button ***/

   else {
      XBell( grapher->dc->display , 100 ) ;
   }

   /*--- Done!!! ---*/

   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

#define SETSHIFT_quit_label  "Quit"
#define SETSHIFT_apply_label "Apply"
#define SETSHIFT_done_label  "Set"

#define SETSHIFT_quit_help   "Press to close\nthis control box"
#define SETSHIFT_apply_help  "Press to apply this choice\nand keep this control box"
#define SETSHIFT_done_help   "Press to apply this choice\nand close this control box"

#define NUM_SETSHIFT_ACT 3

static MCW_action_item SETSHIFT_act[NUM_SETSHIFT_ACT] = {
 { SETSHIFT_quit_label  , GRA_setshift_action_CB, NULL, SETSHIFT_quit_help ,"Close window"                 , 0 },
 { SETSHIFT_apply_label , GRA_setshift_action_CB, NULL, SETSHIFT_apply_help,"Apply choice and keep window" , 0 },
 { SETSHIFT_done_label  , GRA_setshift_action_CB, NULL, SETSHIFT_done_help ,"Apply choice and close window", 1 }
} ;

#define SETSHIFT_QUIT  0
#define SETSHIFT_APPLY 1
#define SETSHIFT_DONE  2

void GRA_setshift_startup( MCW_grapher * grapher )
{
   int ib , xx,yy ;
   Widget wrc ;

ENTRY("GRA_setshift_startup") ;

   if( ! GRA_REALZ(grapher) || grapher->dialog != NULL ) EXRETURN ;

   MCW_widget_geom( grapher->fdw_graph , NULL,NULL,&xx,&yy ) ;  /* geometry of shell */

   grapher->dialog = XtVaCreatePopupShell(
                      "dialog" , xmDialogShellWidgetClass , grapher->fdw_graph ,
                         XmNx , xx+15 ,
                         XmNy , yy+15 ,
                         XmNtitle , "Shifts" ,
                         XmNdeleteResponse , XmDO_NOTHING ,
                         XmNinitialResourcesPersistent , False ,

                         XmNvisual   , grapher->dc->visual ,         /* 14 Sep 1998 */
                         XmNcolormap , grapher->dc->colormap ,
                         XmNdepth    , grapher->dc->depth ,
                         XmNscreen   , grapher->dc->screen ,
                         XmNbackground  , 0 ,
                         XmNborderColor , 0 ,

                      NULL ) ;

   if( MCW_isitmwm(grapher->fdw_graph) ){
      XtVaSetValues( grapher->dialog ,
                       XmNmwmDecorations , MWM_DECOR_BORDER ,
                       XmNmwmFunctions ,   MWM_FUNC_MOVE
                                         | MWM_FUNC_CLOSE ,
                     NULL ) ;
   }

   XmAddWMProtocolCallback(           /* make "Close" window menu work */
           grapher->dialog ,
           XmInternAtom( grapher->dc->display , "WM_DELETE_WINDOW" , False ) ,
           GRA_setshift_action_CB , grapher ) ;

   wrc = XtVaCreateWidget(                    /* RowColumn to hold all */
             "dialog" , xmRowColumnWidgetClass , grapher->dialog ,
                XmNpacking     , XmPACK_TIGHT ,
                XmNorientation , XmVERTICAL ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   (void) XtVaCreateManagedWidget(
            "dialog" , xmLabelWidgetClass , wrc ,
               LABEL_ARG("-- Shift Controls --") ,
               XmNalignment  , XmALIGNMENT_CENTER ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , wrc ,
               XmNseparatorType , XmSHADOW_ETCHED_IN ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

   grapher->setshift_inc_av = new_MCW_arrowval(
                                wrc ,   "Increment  " , MCW_AV_downup ,
                                1 , 1000 , (int)(100.0*grapher->setshift_inc) ,
                                MCW_AV_edittext , 2 ,
                                NULL , NULL , NULL , NULL ) ;

   grapher->setshift_left_av = new_MCW_arrowval(
                                  wrc , "Steps Left " , MCW_AV_downup ,
                                  0 , 29 , grapher->setshift_left ,
                                  MCW_AV_edittext , 0 ,
                                  NULL , NULL , NULL , NULL ) ;

   grapher->setshift_right_av = new_MCW_arrowval(
                                  wrc , "Steps Right" , MCW_AV_downup ,
                                  0 , 29 , grapher->setshift_right ,
                                  MCW_AV_edittext , 0 ,
                                  NULL , NULL , NULL , NULL ) ;

   grapher->setshift_inc_av->allow_wrap   = 1 ;   /* allow wrap at limits of values */
   grapher->setshift_left_av->allow_wrap  = 1 ;
   grapher->setshift_right_av->allow_wrap = 1 ;

   grapher->setshift_inc_av->fastdelay    = 250 ; /* slow down arrow repeat action */
   grapher->setshift_left_av->fastdelay   = 250 ;
   grapher->setshift_right_av->fastdelay  = 250 ;

   MCW_reghelp_children( grapher->setshift_inc_av->wrowcol ,
       "This controls the step size\n"
       "used to create the shifted\n"
       "time series -- for example,\n"
       "0.2 means a shift of 1/5\n"
       "of a time series stepsize."
   ) ;
   MCW_reghint_children( grapher->setshift_inc_av->wrowcol ,
                         "Size of shift" ) ;

   MCW_reghelp_children( grapher->setshift_left_av->wrowcol ,
      "This controls the number\n"
      "of left shifts used to\n"
      "create the desired multi-\n"
      "vector time series.\n\n"
      "N.B.: Steps Left plus\n"
      "      Steps Right should\n"
      "      be positive!"
   ) ;
   MCW_reghint_children( grapher->setshift_left_av->wrowcol ,
                         "Number of steps left" ) ;

   MCW_reghelp_children( grapher->setshift_right_av->wrowcol ,
      "This controls the number\n"
      "of right shifts used to\n"
      "create the desired multi-\n"
      "vector time series.\n\n"
      "N.B.: Steps Left plus\n"
      "      Steps Right should\n"
      "      be positive!"
   ) ;
   MCW_reghint_children( grapher->setshift_right_av->wrowcol ,
                         "Number of steps right" ) ;

   for( ib=0 ; ib < NUM_SETSHIFT_ACT ; ib++ )
      SETSHIFT_act[ib].data = (XtPointer) grapher ;

   (void) MCW_action_area( wrc , SETSHIFT_act , NUM_SETSHIFT_ACT ) ;

   XtManageChild( wrc ) ;
   XtPopup( grapher->dialog , XtGrabNone ) ;
   RWC_visibilize_widget( grapher->dialog ) ; /* 09 Nov 1999 */
   MCW_alter_widget_cursor( grapher->dialog , -XC_left_ptr ,"yellow","blue" ) ;
   EXRETURN ;
}

void GRA_setshift_action_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_grapher * grapher = (MCW_grapher *) client_data ;
   XmAnyCallbackStruct * cbs = (XmAnyCallbackStruct *) call_data ;
   char * wname ;
   int ib , close_window ;

ENTRY("GRA_setshift_action_CB") ;

   if( !GRA_REALZ(grapher) || grapher->dialog==NULL ) EXRETURN ;

   wname = XtName(w) ;

   for( ib=0 ; ib < NUM_SETSHIFT_ACT ; ib++ )           /* button index, if any */
      if( strcmp(wname,SETSHIFT_act[ib].label) == 0 ) break ;

   close_window = (ib == SETSHIFT_DONE ||
                   ib == SETSHIFT_QUIT || ib == NUM_SETSHIFT_ACT) ;

   if( close_window ){
      XtPopdown( grapher->dialog ) ;
      XSync( XtDisplay(w) , False ) ;
      XmUpdateDisplay( w ) ;
   }

   switch( ib ){

      case SETSHIFT_APPLY:
      case SETSHIFT_DONE:{
         grapher->setshift_inc   = grapher->setshift_inc_av->fval ;
         grapher->setshift_left  = grapher->setshift_left_av->ival ;
         grapher->setshift_right = grapher->setshift_right_av->ival ;

         GRA_doshift( grapher ) ;
      }
      break ;
   }

   if( close_window ){                          /* close the window */
      XtDestroyWidget( grapher->dialog ) ;
      grapher->dialog = NULL ;
      FREE_AV( grapher->setshift_right_av ) ;
      FREE_AV( grapher->setshift_left_av )  ;
      FREE_AV( grapher->setshift_inc_av )   ;
   }

   EXRETURN ;
}

void GRA_doshift( MCW_grapher * grapher )
{
   MRI_IMAGE * tsim , * newim , * tim ;
   float * tsar , * nar , * tar ;
   int ii,ivec , nx , newny , nleft,nright ;
   float shinc ;
   GRA_cbs gbs ;

ENTRY("GRA_doshift") ;

   if( !GRA_VALID(grapher) ) EXRETURN ;

   if( grapher->status->send_CB == NULL                       ||
       grapher->setshift_inc <= 0.0                           ||
       grapher->setshift_left + grapher->setshift_right <= 0  ||
       grapher->ref_ts == NULL                                ||
       IMARR_COUNT(grapher->ref_ts) == 0                        ){

      XBell( grapher->dc->display , 100 ) ; EXRETURN ;
   }

   tsim = IMARR_SUBIMAGE(grapher->ref_ts,0) ; /* current ref */
   tsar = MRI_FLOAT_PTR(tsim) ;
   nx   = tsim->nx ;

   shinc  = grapher->setshift_inc ;
   nleft  = grapher->setshift_left ;
   nright = grapher->setshift_right ;
   newny  = 1 + nleft + nright ;
   newim  = mri_new( nx , newny , MRI_float ) ;
   nar    = MRI_FLOAT_PTR(newim) ;

   for( ii=0 ; ii < nx ; ii++ )
      nar[ii] = (ii < grapher->init_ignore) ? WAY_BIG : tsar[ii] ;

   for( ivec=1 ; ivec <= nleft ; ivec++ ){
      tim = mri_shift_1D( newim , -ivec * shinc ) ;
      tar = MRI_FLOAT_PTR(tim) ;
      for( ii=0 ; ii < nx ; ii++ ) nar[ii+ivec*nx] = tar[ii] ;
      mri_free(tim) ;
   }

   for( ivec=1 ; ivec <= nright ; ivec++ ){
      tim = mri_shift_1D( newim , ivec * shinc ) ;
      tar = MRI_FLOAT_PTR(tim) ;
      for( ii=0 ; ii < nx ; ii++ ) nar[ii+(ivec+nleft)*nx] = tar[ii] ;
      mri_free(tim) ;
   }

   gbs.reason   = graCR_refequals ;
   gbs.userdata = (XtPointer) newim ;
   grapher->status->send_CB( grapher , grapher->getaux , &gbs ) ;
   mri_free( newim ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------
  Returns a bunch of widgets in a structure.
  The parent widget is intended to be a menu bar.
-----------------------------------------------------------------------*/

FIM_menu * AFNI_new_fim_menu( Widget parent , XtCallbackProc cbfunc , int graphable )
{
   FIM_menu * fmenu ;
   Widget qbut_menu = NULL ;

ENTRY("AFNI_new_fim_menu") ;

   fmenu = myXtNew(FIM_menu) ;
   fmenu->cbfunc = cbfunc ;

   /*------------------------*/
   /*--- FIM Menu Buttons ---*/
   /*------------------------*/

   fmenu->fim_menu =
         XmCreatePulldownMenu( parent , "menu" , NULL,0 ) ;

   fmenu->fim_cbut =
         XtVaCreateManagedWidget(
            "dialog" , xmCascadeButtonWidgetClass , parent ,
               LABEL_ARG("FIM") ,
               XmNsubMenuId , fmenu->fim_menu ,
               XmNmarginWidth  , 0 ,
               XmNmarginHeight , 0 ,
               XmNmarginBottom , 0 ,
               XmNmarginTop    , 0 ,
               XmNmarginRight  , 0 ,
               XmNmarginLeft   , 0 ,
               XmNtraversalOn  , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

   if( graphable ){
      MCW_register_hint( fmenu->fim_cbut , "Functional Imaging menu" ) ;
      MCW_register_help( fmenu->fim_cbut ,
                      "*******  Functional Imaging Controls:  *******\n"
                      "\n"
                      "Ideal Vector Operations:\n"
                      " Pick Ideal --> Choose from a list\n"
                      " Pick Ort   --> Choose from a list\n"
                      " Edit: = Center --> Use voxel timeseries\n"
                      "      += Center --> Average with voxel\n"
                      "      Smooth    --> A 3 point filter\n"
                      "      Shift     --> Time-shifted copies\n"
                      "      Clear     --> Turn ideal off\n"
                      "      Read      --> Input from external file\n"
                      "      Write     --> Output to external file\n"
                      "      Store     --> Save in internal list\n"
                      " Ignore: Set how many points to ignore\n"
                      "         at beginning of time series\n"
                      "         [ Applies both to graphing ]\n"
                      "         [ and to FIM computations. ]\n"
                      "\n"
                      "FIM Plots:\n"
                      "  Can choose to graph only first vector in\n"
                      "  ideal family, or all of them superimposed.\n"
                      "\n"
                      "Refresh Freq --> Choose number of time steps\n"
                      "   between redisplay of the functional overlay\n"
                      "   during FIM computations (0 == no redisplay).\n"
                      "Compute FIM  --> Use the recursive method to\n"
                      "   compute the correlation of each voxel time\n"
                      "   series with each ideal vector;  uses the best\n"
                      "   correlation as the 'correct' waveform for\n"
                      "   each voxel, individually.  Time points that\n"
                      "   have an ideal vector value >= 33333 will be\n"
                      "   ignored in the computations, as will those\n"
                      "   at the beginning specified in the Opt menu\n"
                      "   'Ignore' function."
                    ) ;
   } else {
      MCW_register_hint( fmenu->fim_cbut , "Functional Imaging menu" ) ;
      MCW_register_help( fmenu->fim_cbut ,
                      "*******  Functional Imaging Controls:  *******\n"
                      "\n"
                      "Pick Dataset --> Choose time-dependent dataset\n"
                      "   from a list.  If there is only one possible\n"
                      "   choice, it will be selected for you without\n"
                      "   displaying the list.\n"
                      "\n"
                      "Pick Ideal --> Choose time series from a list.\n"
                      "Pick Ort   --> Choose time series from a list.\n"
                      "\n"
                      "Ignore --> Set how many points to ignore at the\n"
                      "   beginning of time series.\n"
                      "   [ Applies both to graphing and FIM-ing. ]\n"
                      "\n"
                      "Refresh Freq --> Choose number of time steps\n"
                      "   between redisplay of the functional overlay\n"
                      "   during FIM computations (0 == no redisplay).\n"
                      "\n"
                      "Compute FIM --> Use the recursive method to\n"
                      "   compute the correlation of each voxel time\n"
                      "   series with each ideal vector;  uses the best\n"
                      "   correlation as the 'correct' waveform for\n"
                      "   each voxel, individually.  Time points that\n"
                      "   have an ideal vector value >= 33333 will be\n"
                      "   ignored in the computations, as will those\n"
                      "   at the beginning specified in the Opt menu\n"
                      "   'Ignore' function."
                    ) ;
   }

   /* macros to put double and single separator lines in a menu */

#undef MENU_DLINE
#define MENU_DLINE(wmenu)                                        \
   (void) XtVaCreateManagedWidget(                               \
            "dialog" , xmSeparatorWidgetClass , fmenu -> wmenu , \
             XmNseparatorType , XmDOUBLE_LINE , NULL )

#undef MENU_SLINE
#define MENU_SLINE(wmenu)                                        \
   (void) XtVaCreateManagedWidget(                               \
            "dialog" , xmSeparatorWidgetClass , fmenu -> wmenu , \
             XmNseparatorType , XmSINGLE_LINE , NULL )

   /* macro to create a new FIM menu button */

#define FIM_MENU_BUT(wname,label)                                  \
   fmenu -> wname =                                                \
         XtVaCreateManagedWidget(                                  \
            "dialog" , xmPushButtonWidgetClass , fmenu->fim_menu , \
               LABEL_ARG( label ) ,                                \
               XmNmarginHeight , 0 ,                               \
               XmNtraversalOn , False ,                            \
               XmNinitialResourcesPersistent , False ,             \
            NULL ) ;                                               \
      XtAddCallback( fmenu -> wname , XmNactivateCallback ,        \
                     cbfunc , (XtPointer) fmenu ) ;

   /** macro to create a new fim pullright menu **/
   /** 07 Jan 1999: added the mapCallback to fix position **/

#define FIM_MENU_PULLRIGHT(wmenu,wcbut,label)                      \
   fmenu -> wmenu =                                                \
     XmCreatePulldownMenu( fmenu->fim_menu , "menu" , NULL , 0 ) ; \
   fmenu -> wcbut =                                                \
     XtVaCreateManagedWidget(                                      \
       "dialog" , xmCascadeButtonWidgetClass , fmenu->fim_menu ,   \
          LABEL_ARG( label ) ,                                     \
          XmNsubMenuId , fmenu -> wmenu ,                          \
          XmNtraversalOn , False ,                                 \
          XmNinitialResourcesPersistent , False ,                  \
       NULL ) ;                                                    \
   XtAddCallback( fmenu -> wmenu, XmNmapCallback, GRA_mapmenu_CB, NULL ) ;

   /** macro to create a new button on a pullright menu **/

#define FIM_MENU_PULL_BUT(wmenu,wname,label) \
   fmenu -> wname =                                               \
         XtVaCreateManagedWidget(                                 \
            "dialog" , xmPushButtonWidgetClass , fmenu -> wmenu , \
               LABEL_ARG( label ) ,                               \
               XmNmarginHeight , 0 ,                              \
               XmNtraversalOn , False ,                           \
               XmNinitialResourcesPersistent , False ,            \
            NULL ) ;                                              \
      XtAddCallback( fmenu -> wname , XmNactivateCallback ,       \
                     cbfunc , (XtPointer) fmenu ) ;

#define EMPTY_BUT(wname) fmenu -> wname = NULL

   /** 15 Dec 1997: a pullright menu with a single button **/

#define FIM_MENU_QBUT(wname,label,qlab)                                   \
 do { Widget ccc ;                                                        \
      qbut_menu = XmCreatePulldownMenu(fmenu->fim_menu,"menu",NULL,0);    \
            ccc = XtVaCreateManagedWidget( "dialog" ,                     \
                     xmCascadeButtonWidgetClass , fmenu->fim_menu ,       \
                     LABEL_ARG( label ) ,                                 \
                     XmNsubMenuId , qbut_menu ,                           \
                     XmNtraversalOn , False ,                             \
                     XmNinitialResourcesPersistent , False , NULL ) ;     \
      fmenu -> wname = XtVaCreateManagedWidget( "dialog" ,                \
                         xmPushButtonWidgetClass , qbut_menu ,            \
                         LABEL_ARG( qlab ) ,                              \
                         XmNmarginHeight , 0 ,                            \
                         XmNtraversalOn , False ,                         \
                         XmNinitialResourcesPersistent , False , NULL ) ; \
      XtAddCallback( fmenu -> wname , XmNactivateCallback ,               \
                     cbfunc , (XtPointer) fmenu ) ;                       \
      XtAddCallback( qbut_menu, XmNmapCallback, GRA_mapmenu_CB, NULL ) ;  \
 } while(0)

   /*** top of menu = a label to click on that does nothing at all ***/

   (void) XtVaCreateManagedWidget(
            "dialog" , xmLabelWidgetClass , fmenu->fim_menu ,
               LABEL_ARG("--- Cancel ---") ,
               XmNrecomputeSize , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

   MENU_SLINE(fim_menu) ;

   if( graphable ){
      EMPTY_BUT(fim_pickdset_pb) ;
   } else {
      FIM_MENU_BUT( fim_pickdset_pb , "Pick Dataset" ) ;
   }

   FIM_MENU_BUT( fim_pickref_pb , "Pick Ideal") ;
   FIM_MENU_BUT( fim_pickort_pb , "Pick Ort"  ) ;

   if( graphable ){
      MENU_SLINE(fim_menu) ;
      FIM_MENU_PULLRIGHT(fim_editref_menu,fim_editref_cbut       ,"Edit Ideal"    ) ;
      FIM_MENU_PULL_BUT (fim_editref_menu,fim_editref_equals_pb  ,"Ideal = Center") ;
      FIM_MENU_PULL_BUT (fim_editref_menu,fim_editref_add_pb     ,"Ideal+= Center") ;
      FIM_MENU_PULL_BUT (fim_editref_menu,fim_editref_smooth_pb  ,"Smooth Ideal"  ) ;
      FIM_MENU_PULL_BUT (fim_editref_menu,fim_editref_setshift_pb,"Shift Ideal"   ) ;
      FIM_MENU_PULL_BUT (fim_editref_menu,fim_editref_clear_pb   ,"Clear Ideal"   ) ;
      FIM_MENU_PULL_BUT (fim_editref_menu,fim_editort_clear_pb   ,"Clear Ort"     ) ;
#ifdef USE_OPTMENUS
      fmenu->fim_polort_choose_av =
         new_MCW_optmenu( fmenu->fim_editref_menu , "Polort " , 0,MAX_POLORT,1,0 ,
                          GRA_fmenu_av_CB , (XtPointer) fmenu , NULL , NULL ) ;
      fmenu->fim_polort_choose_pb = fmenu->fim_polort_choose_av->wrowcol ;
#else
      FIM_MENU_PULL_BUT( fim_editref_menu,fim_polort_choose_pb ,"Polort?") ;
#endif
      FIM_MENU_PULL_BUT( fim_editref_menu,fim_bkthr_choose_pb  ,"Bkg Thresh") ;
      MENU_SLINE        (fim_editref_menu) ;
      FIM_MENU_PULL_BUT (fim_editref_menu,fim_editref_read_pb    ,"Read Ideal"    ) ;
      FIM_MENU_PULL_BUT (fim_editref_menu,fim_editref_write_pb   ,"Write Ideal"   ) ;
      FIM_MENU_PULL_BUT (fim_editref_menu,fim_editref_store_pb   ,"Store Ideal"   ) ;
   } else {
      EMPTY_BUT(fim_editref_cbut) ;
      EMPTY_BUT(fim_editref_equals_pb) ;
      EMPTY_BUT(fim_editref_add_pb) ;
      EMPTY_BUT(fim_editref_smooth_pb) ;
      EMPTY_BUT(fim_editref_setshift_pb) ;
      EMPTY_BUT(fim_editref_clear_pb) ;
      EMPTY_BUT(fim_editref_read_pb) ;
      EMPTY_BUT(fim_editref_write_pb) ;
      EMPTY_BUT(fim_editref_store_pb) ;
      EMPTY_BUT(fim_editort_clear_pb) ;
      EMPTY_BUT(fim_polort_choose_pb) ;
   }

   FIM_MENU_PULLRIGHT(fim_ignore_menu,fim_ignore_cbut      ,"Ignore") ;
   FIM_MENU_PULL_BUT( fim_ignore_menu,fim_ignore_down_pb   ,"Down"  ) ;
   FIM_MENU_PULL_BUT( fim_ignore_menu,fim_ignore_up_pb     ,"Up"    ) ;
#ifdef USE_OPTMENUS
   fmenu->fim_ignore_choose_av =
      new_MCW_optmenu( fmenu->fim_ignore_menu , "# " , 0,2,0,0 ,
                       GRA_fmenu_av_CB , (XtPointer) fmenu , NULL , NULL ) ;
   fmenu->fim_ignore_choose_pb = fmenu->fim_ignore_choose_av->wrowcol ;
#else
   FIM_MENU_PULL_BUT( fim_ignore_menu,fim_ignore_choose_pb ,"Choose") ;
#endif

   if( graphable ){
      FIM_MENU_PULLRIGHT(fim_plot_menu,fim_plot_cbut        ,"FIM Plots" ) ;
      FIM_MENU_PULL_BUT( fim_plot_menu,fim_plot_firstref_pb ,"First Ideal" ) ;
      FIM_MENU_PULL_BUT( fim_plot_menu,fim_plot_allrefs_pb  ,"All Ideals"  ) ;
   } else {
      EMPTY_BUT(fim_plot_cbut) ;
      EMPTY_BUT(fim_plot_firstref_pb) ;
      EMPTY_BUT(fim_plot_allrefs_pb) ;
   }

   MENU_DLINE(fim_menu) ;
   FIM_MENU_QBUT( fim_execute_pb   , "Compute FIM" , "-> fico") ;
   MCW_set_widget_bg( fmenu->fim_execute_pb ,
                      MCW_hotcolor(fmenu->fim_execute_pb) , 0 ) ;

   { static char * blab[] = { "Fit Coef" , "% Change" , "% From Ave" } ;
     (void) XtVaCreateManagedWidget(
             "dialog" , xmSeparatorWidgetClass , qbut_menu ,
              XmNseparatorType , XmSINGLE_LINE , NULL ) ;

     fmenu->fim_opt_bbox = new_MCW_bbox( qbut_menu , 3 , blab ,
                                         MCW_BB_radio_one , MCW_BB_noframe ,
                                         NULL , NULL ) ;
   }

   MENU_DLINE(fim_menu) ;
   FIM_MENU_QBUT( fim_execfimp_pb  , "Compute FIM+" , "-> fbuc") ;
   MCW_set_widget_bg( fmenu->fim_execfimp_pb ,
                      MCW_hotcolor(fmenu->fim_execfimp_pb) , 0 ) ;

   (void) XtVaCreateManagedWidget(
           "dialog" , xmSeparatorWidgetClass , qbut_menu ,
            XmNseparatorType , XmSINGLE_LINE , NULL ) ;

   fmenu->fimp_opt_bbox = new_MCW_bbox( qbut_menu, FIM_NUM_OPTS, fim_opt_labels,
                                        MCW_BB_check , MCW_BB_noframe ,
                                        NULL , NULL ) ;

   { char * ff = my_getenv( "AFNI_FIM_MASK" ) ; int mm=0 ;
     if( ff != NULL ) mm = strtol(ff,NULL,10) ;
     if( mm <= 0 ) mm = FIM_DEFAULT_MASK ;
     MCW_set_bbox( fmenu->fimp_opt_bbox , mm ) ;
   }

   RETURN(fmenu) ;
}

/*-----------------------------------------------------------------------------
   Routines to handle point transformations of a timeseries (22 Oct 1996)
-------------------------------------------------------------------------------*/

char * GRA_transform_label( MCW_arrowval * av , XtPointer cd )
{
   MCW_function_list * xforms = (MCW_function_list *) cd ;

   if( av == NULL    || xforms == NULL        ||
       av->ival <= 0 || av->ival > xforms->num  ) return "-none-" ;

   return xforms->labels[av->ival - 1] ;  /* label for each function */
}

void GRA_transform_CB( MCW_arrowval * av , XtPointer cd )
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;

ENTRY("GRA_transform_CB") ;

   if( ! GRA_VALID(grapher) ) EXRETURN ;

   /** set the 0D transform function pointer **/

   if( av == grapher->transform0D_av && av != NULL ){
      if( grapher->status->transforms0D == NULL || av->ival <= 0 ||
          av->ival > grapher->status->transforms0D->num            ){

         grapher->transform0D_func  = NULL ;  /* no transform */
         grapher->transform0D_index = 0 ;
      } else {
         grapher->transform0D_func  = grapher->status->transforms0D->funcs[av->ival-1];
         grapher->transform0D_index = av->ival ;
         grapher->transform0D_flags = grapher->status->transforms0D->flags[av->ival-1];
      }
   }

   /** set the 1D transform function pointer **/

   if( av == grapher->transform1D_av && av != NULL ){
      if( grapher->status->transforms1D == NULL || av->ival <= 0 ||
          av->ival > grapher->status->transforms1D->num            ){

         grapher->transform1D_func  = NULL ;  /* no transform */
         grapher->transform1D_index = 0 ;
      } else {
         grapher->transform1D_func  = grapher->status->transforms1D->funcs[av->ival-1];
         grapher->transform1D_index = av->ival ;
         grapher->transform1D_flags = grapher->status->transforms1D->flags[av->ival-1];
      }
   }

   redraw_graph( grapher , 0 ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------
   08 Nov 1996: for "double plots" -- just redraw everything
------------------------------------------------------------------------------*/

void GRA_dplot_change_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_grapher * grapher = (MCW_grapher *) client_data ;

ENTRY("GRA_dplot_change_CB") ;

   if( ! GRA_VALID(grapher) ) EXRETURN ;
   redraw_graph( grapher , 0 ) ;
   EXRETURN ;
}

#ifdef USE_OPTMENUS
/*---------------------------------------------------------------------------
   Fix the optmenus for the grapher; used when the dataset changes
   and so the upper limits on the selectors must change too
-----------------------------------------------------------------------------*/

void GRA_fix_optmenus( MCW_grapher * grapher )
{
   int igtop ;

ENTRY("GRA_fix_optmenus") ;

   if( ! GRA_REALZ(grapher) ) EXRETURN ;

   /** matrix selection **/

   if( grapher->opt_mat_choose_av->imax != grapher->mat_max )
      refit_MCW_optmenu( grapher->opt_mat_choose_av ,
                         1 , grapher->mat_max , grapher->mat , 0 ,
                         NULL , NULL ) ;

   else
      AV_assign_ival( grapher->opt_mat_choose_av , grapher->mat ) ;

   /** slice selection **/

   if( grapher->opt_slice_choose_av->imax != grapher->status->nz-1 )
      refit_MCW_optmenu( grapher->opt_slice_choose_av ,
                         0, grapher->status->nz - 1, grapher->zpoint, 0,
                         NULL , NULL ) ;

   else
      AV_assign_ival( grapher->opt_slice_choose_av , grapher->zpoint ) ;

   /** fim ignoration **/

   igtop = MIN( grapher->status->num_series-2 , 99 ) ;
   igtop = MAX( igtop , 1 ) ;

   if( grapher->fmenu->fim_ignore_choose_av->imax != igtop )
      refit_MCW_optmenu( grapher->fmenu->fim_ignore_choose_av ,
                         0 , igtop , grapher->init_ignore, 0,
                         NULL , NULL ) ;
   else
      AV_assign_ival( grapher->fmenu->fim_ignore_choose_av , grapher->init_ignore ) ;

   /** 27 May 1999: fim polort **/

   AV_assign_ival( grapher->fmenu->fim_polort_choose_av , grapher->polort ) ;

   EXRETURN ;
}

void GRA_fmenu_av_CB( MCW_arrowval * av , XtPointer cd )
{
   FIM_menu * fmenu = (FIM_menu *) cd ;

ENTRY("GRA_fmenu_av_CB") ;
   fmenu->cbfunc( av->wrowcol , cd , NULL ) ;
   EXRETURN ;
}
#endif /* USE_OPTMENUS */

/*--------------------------------------------------------------------------
   Selection of a color submenu item
----------------------------------------------------------------------------*/

void GRA_color_CB( MCW_arrowval * av , XtPointer cd )
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;
   int ii , jj ;

ENTRY("GRA_color_CB") ;

   if( ! GRA_VALID(grapher) ) EXRETURN ;

   for( ii=0 ; ii < NUM_COLOR_ITEMS ; ii++ )
      if( av == grapher->opt_color_av[ii] ) break ;

   if( ii < NUM_COLOR_ITEMS ){
      jj = grapher->color_index[ii] ;
      grapher->color_index[ii] = av->ival ;
      if( jj != grapher->color_index[ii] ) redraw_graph( grapher , 0 ) ;
   }
   EXRETURN ;
}

void GRA_thick_CB( Widget w , XtPointer cd , XtPointer call_data )
{
   MCW_grapher * grapher = (MCW_grapher *) cd ;
   int ii , jj ;

ENTRY("GRA_thick_CB") ;

   if( ! GRA_VALID(grapher) ) EXRETURN ;

   for( ii=0 ; ii < NUM_COLOR_ITEMS ; ii++ )
      if( grapher->opt_thick_bbox[ii] != NULL &&
          w == grapher->opt_thick_bbox[ii]->wbut[0] ) break ;

   if( ii < NUM_COLOR_ITEMS ){
      jj = grapher->thick_index[ii] ;
      grapher->thick_index[ii] = MCW_val_bbox( grapher->opt_thick_bbox[ii] ) ;
      if( jj != grapher->thick_index[ii] ) redraw_graph( grapher , 0 ) ;
      EXRETURN ;
   }

   /* 09 Jan 1998 */

   for( ii=0 ; ii < NUM_COLOR_ITEMS ; ii++ )
      if( grapher->opt_points_bbox[ii] != NULL &&
          ( w == grapher->opt_points_bbox[ii]->wbut[0] ||
            w == grapher->opt_points_bbox[ii]->wbut[1]   ) ) break ;

   if( ii < NUM_COLOR_ITEMS ){
      jj = grapher->points_index[ii] ;
      grapher->points_index[ii] = MCW_val_bbox( grapher->opt_points_bbox[ii] ) ;
      if( jj != grapher->points_index[ii] ) redraw_graph( grapher , 0 ) ;
      EXRETURN ;
   }

   EXRETURN ;  /* should not be reached */
}

/*--------------------------------------------------------------------------
   Save the background pixmap to a PNM file
----------------------------------------------------------------------------*/

void GRA_saver_CB( Widget wcaller , XtPointer cd , MCW_choose_cbs * cbs )
{
   int ll , ii ;
   MCW_grapher * grapher = (MCW_grapher *) cd ;
   char * fname , * ppnm ;

ENTRY("GRA_saver_CB") ;

   if( ! GRA_REALZ(grapher) ) EXRETURN ;

   if( cbs->reason != mcwCR_string ||
       cbs->cval   == NULL         || (ll=strlen(cbs->cval)) == 0 ){

      XBell( XtDisplay(wcaller) , 100 ) ; EXRETURN ;
   }

   fname = (char *) malloc( sizeof(char) * (ll+8) ) ;
   strcpy( fname , cbs->cval ) ;

   if( fname[ll-1] != '.' ){  /* add a . at the end? */
       fname[ll++] = '.' ;
       fname[ll]   = '\0' ;
   }

   for( ii=0 ; ii < ll ; ii++ )
      if( iscntrl(fname[ii]) || isspace(fname[ii]) ) break ;

   if( ii < ll || ll < 2 || ll > 240 ){
      XBell( XtDisplay(wcaller) , 100 ) ;
      free( fname ) ; EXRETURN ;
   }

   ppnm = strstr( fname , ".pnm." ) ;
   if( ppnm == fname + (ll-5) ) fname[ll-1] = '\0' ;
   else                         strcat(fname,"pnm") ;

   GRA_file_pixmap( grapher , fname ) ;
   POPDOWN_string_chooser ;
   free(fname) ; EXRETURN ;
}

void GRA_file_pixmap( MCW_grapher * grapher , char * fname )
{
   XImage * xim ;
   XGCValues gcv ;
   MRI_IMAGE * tim ;
   int ii ;

ENTRY("GRA_file_pixmap") ;

   if( ! GRA_REALZ(grapher) ) EXRETURN ;
   if( grapher->fd_pxWind == (Pixmap) 0 ) EXRETURN ;

   ii = XGetGCValues( grapher->dc->display ,
                      grapher->dc->myGC , GCPlaneMask , &gcv ) ;
   if( ii == 0 ) EXRETURN ;

   xim = XGetImage( grapher->dc->display , grapher->fd_pxWind ,
                    0 , 0 , grapher->fWIDE , grapher->fHIGH ,
                    gcv.plane_mask , ZPixmap ) ;
   if( xim == NULL ) EXRETURN ;

   tim = XImage_to_mri( grapher->dc , xim , 0 ) ;
   if( tim == NULL ){ MCW_kill_XImage( xim ) ; EXRETURN ; }

   mri_write_pnm( fname , tim ) ;
   fprintf(stderr,"Writing one PNM image to file %s\n",fname) ;
   mri_free( tim ) ;
   MCW_kill_XImage( xim ) ;  /* 10 Mar 1999 */
   EXRETURN ;
}

/*-----------------------------------------------------------------------------
   07 Jan 1999: change location of newly popped up menu
-------------------------------------------------------------------------------*/

void GRA_mapmenu_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   int ww,hh,xx,yy ;
   int pw,ph,px,py ;

ENTRY("GRA_mapmenu_CB") ;

 MCW_widget_geom( w                     , &ww,&hh , &xx,&yy ) ;
 MCW_widget_geom( XtParent(XtParent(w)) , &pw,&ph , &px,&py ) ;

if(PRINT_TRACING){
 char str[256] ;
 sprintf(str,"menu:   width=%d height=%d x=%d y=%d",ww,hh,xx,yy) ; STATUS(str) ;
 sprintf(str,"parent: width=%d height=%d x=%d y=%d",pw,ph,px,py) ; STATUS(str) ; }

   pw = pw >> 3 ;
   if( ! ( xx > px+7*pw || xx+ww < px+pw ) ){
      STATUS("moving menu") ;
      xx = px - ww ;  if( xx < 0 ) xx = 0 ;
      XtVaSetValues( w , XmNx , xx , NULL ) ;
   }
   EXRETURN ;
}
