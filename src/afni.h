/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _AFNI_HEADER_
#define _AFNI_HEADER_

#include "mrilib.h"
#include "imseq.h"
#include "xutil.h"
#include "pbar.h"
#include "afni_graph.h"
#include "afni_pcor.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <Xm/Separator.h>
#include <Xm/Display.h>
#include <Xm/CascadeB.h>

#include "logo.h"

#define WARP_4D

#ifndef DONT_USE_OPTMENUS
#ifndef USE_OPTMENUS
#define USE_OPTMENUS
#endif
#endif

/*------------------------------------------------------------
  Global data holding command line arguments information
--------------------------------------------------------------*/

typedef struct {
      float dy , dz , gamma , gsfac ;
      Boolean read_images , read_sessions , auto_purge ,
              resize_images , keep_logo , pos_func , xtwarns ,
              destruct , tlrc_big , warp_4D , unique_dcs ;
      int ncolor , datum , ignore , allow_rt , skip_afnirc ;
      char orient_code[4] ;
      char title_name[32] ;
#ifdef ALLOW_PLUGINS
      int noplugins , noplugouts ;
      int plugout_code ;
#endif

      int first_file_arg , recurse ;
      int elide_quality ;
      int no_frivolities ;
      int install_cmap ;
      int left_is_left ;  /* 09 Oct 1998 */
      int read_tim ;      /* 19 Oct 1999 */
      int read_1D ;       /* 27 Jan 2000 */

      Boolean read_dsets ;    /* 17 Mar 2000 */

      char * layout_fname ;   /* 23 Sep 2000 */

      int enable_suma ;       /* 29 Aug 2001 */

      int quiet ;             /* 25 Aug 2001 */

      int yes_niml ;          /* 28 Feb 2002 */
} AF_options ;

#ifdef MAIN
   AF_options GLOBAL_argopt ;
#else
   extern AF_options GLOBAL_argopt ;
#endif

/*-----------------------------------------------------------*/

/* define this to put "chooser" controls on the popup menu */
#undef POPUP_CHOOSERS

/*-----------------------------------------------------------*/

#define UNDERLAY_ANAT      0
#define UNDERLAY_ALLFUNC   1
#define UNDERLAY_THRFUNC   2

#define UNDERLAY_ANAT_BVAL     (1 << UNDERLAY_ANAT   )
#define UNDERLAY_ALLFUNC_BVAL  (1 << UNDERLAY_ALLFUNC)
#define UNDERLAY_THRFUNC_BVAL  (1 << UNDERLAY_THRFUNC)

#define LAST_UNDERLAY_TYPE 2
#define ISFUNC_UNDERLAY(uu) \
   ((uu)==UNDERLAY_ALLFUNC||(uu)==UNDERLAY_THRFUNC)

static char * UNDERLAY_typestr[] =
   { "Anat underlay" , "Func underlay" , "Func @Thr underlay" } ;

#define SHOWFUNC_FIM  0
#define SHOWFUNC_THR  1

#define SHOWFUNC_FIM_BVAL  (1 << SHOWFUNC_FIM)
#define SHOWFUNC_THR_BVAL  (1 << SHOWFUNC_THR)

#define LAST_SHOWFUNC_TYPE 1

static char * SHOWFUNC_typestr[] = { "Func=Intensity" , "Func=Threshold" } ;

#define DEFAULT_FIM_SCALE 10000   /* change this and bad things will happen! */

#define DMODE_BRICK      0
#define DMODE_WOD        1

#define DMODE_BRICK_BVAL (1 << DMODE_BRICK)
#define DMODE_WOD_BVAL   (1 << DMODE_WOD  )

/*------------------------------------------------------------------*/
#define USE_SONNETS
#define WANT_RWCOX_IMAGE
#define USE_SKIT

#ifdef NO_FRIVOLITIES
# undef USE_SONNETS
# undef WANT_RWCOX_IMAGE
# undef USE_SKIT
#endif

#define TOPSIZE 2048

/** this should always be exactly 5 characters! **/
/**             "12345" **/

#define VERSION "2.49e"

/** this should always be exactly 17 characters! **/
/**             "12345678901234567" **/

#define RELEASE "17 Jun 2002      "

#ifdef MAIN
#define AFNI_about \
     "************************************************\n"  \
     "* GPL AFNI: Analysis of Functional NeuroImages *\n"  \
     "*           Version " VERSION " -- " RELEASE " *\n"  \
     "*                                              *\n"  \
     "* Major portions are Copyright 1994-2000,      *\n"  \
     "*   Medical College of Wisconsin               *\n"  \
     "*   Milwaukee, WI 53226-0509                   *\n"  \
     "* See file README.copyright for information,   *\n"  \
     "* or the Datamode->Misc->License menu item.    *\n"  \
     "*                                              *\n"  \
     "* Released to the public by MCW under the GNU  *\n"  \
     "* General Public License (version 2), Dec 2000 *\n"  \
     "*                                              *\n"  \
     "* Author:  Robert W Cox, PhD                   *\n"  \
     "************************************************"

char AFNI_tophelp[TOPSIZE] = AFNI_about ;
char AFNI_abohelp[1024]    = AFNI_about ;
#else
extern char AFNI_tophelp[TOPSIZE] ;
extern char AFNI_abohelp[1024] ;
#endif

#ifdef USE_SONNETS

    extern void AFNI_popup_sonnet( Widget,int ) ; /* 12 Dec 2001 */

#   ifdef MAIN
#     include "sonnets.h"
      static int sonnet_index = 0 ;
      void RESET_sonnet(void)
      { int ll ;
        sprintf( AFNI_tophelp , "                    * %d *\n" ,
                 sonnet_index+1 ) ; ll = strlen(AFNI_tophelp) ;
        strcpy( &(AFNI_tophelp[ll]) , sonnets[sonnet_index] ) ;
        sonnet_index = (sonnet_index+lrand48()) % NUM_SONNETS ;
      }
#   else
      extern void RESET_sonnet(void) ; /* prototype */
#   endif  /* MAIN */

#   define RESET_AFNI_QUIT(iqqq) \
     { AFNI_quit_CB(NULL,(XtPointer)(iqqq),NULL) ; RESET_sonnet() ; }

#else  /* don't USE_SONNETS */

#   define RESET_AFNI_QUIT(iqqq) AFNI_quit_CB(NULL,(XtPointer)(iqqq),NULL)

#endif /* USE_SONNETS */
/*------------------------------------------------------------------*/

/** macro to clear the montage information in the
    z-direction of a particular brick, in a particular controller **/

#define CLEAR_MONTAGE(iq,bb)               \
  do{ int bz = abs((bb)->a123.ijk[2])-1 ;  \
      (iq)->vinfo->xhairs_ndown.ijk[bz] =  \
       (iq)->vinfo->xhairs_nup.ijk[bz]   = \
        (iq)->vinfo->xhairs_nskip.ijk[bz] = 0 ; } while(0)

/* 31 Dec 1998: mask for xhairs orientations */

#define ORIMASK_LR ((1<<ORI_R2L_TYPE) | (1<<ORI_L2R_TYPE))
#define ORIMASK_AP ((1<<ORI_P2A_TYPE) | (1<<ORI_A2P_TYPE))
#define ORIMASK_IS ((1<<ORI_I2S_TYPE) | (1<<ORI_S2I_TYPE))

#define ORIMASK_LR_AP (ORIMASK_LR | ORIMASK_AP)
#define ORIMASK_LR_IS (ORIMASK_LR | ORIMASK_IS)
#define ORIMASK_AP_IS (ORIMASK_AP | ORIMASK_IS)
#define ORIMASK_ALL   (ORIMASK_LR | ORIMASK_AP | ORIMASK_IS)

typedef struct {                           /* 29 Mar 1999 */
      gen_func * receiver_func ;
      void *     receiver_data ;
      int        receiver_mask ;
} AFNI_receiver ;

typedef struct {
      int   i1 , j2 , k3 ;  /* integer coordinates of current point */
      float xi , yj , zk ;  /* float (mm) coordinates (take priority) */

      int   i1_old , j2_old , k3_old ;  /* for jumpback */

      Boolean   xhairs_show_montage , xhairs_periodic , xhairs_all ;
      THD_ivec3 xhairs_ndown , xhairs_nup , xhairs_nskip ; /* montage crosshairs */
      int       time_index , top_index ;

      int xhairs_orimask ;    /* 31 Dec 1998 */

      int       anat_index , fim_index , thr_index ; /* 30 Nov 1997 */

      Boolean crosshair_visible , inverted_pause ;
      int     crosshair_gap , crosshair_ovcolor , crosshair_gap_old ;

      int view_type     ,  /* one of the VIEW_ constants in 3ddata.h */
          underlay_type ,  /* one of the UNDERLAY_ constants above */
          showfunc_type ;  /* one of the SHOWFUNC_ constants above */

      int sess_num , anat_num , func_num ;  /* codes pointing to datasets */

      XmString old_crosshair_label ;

      Boolean    func_visible , force_anat_wod , force_func_wod ,
                 pts_visible , show_voxind ;
      float      func_threshold , resam_vox ;
      float      func_thresh_top ;              /* 23 Jul 1997 */
      int        func_resam_mode , anat_resam_mode , pts_color ;
      int        thr_resam_mode ;               /* 09 Dec 1997 */

      /* 3/24/95: range data for conversion of pbar
                  values to thresholding values in the data */

      Boolean  use_autorange , use_posfunc ;
      float    fim_autorange , fim_range ;
      XmString old_range_label , autorange_label ;

      char     anat_val[32] , func_val[32] , thr_val[32] ;

      /** Feb 1998: stuff for the "receive" modules **/
      /** Mar 1999: modified to allow for more than one receiver **/

      AFNI_receiver ** receiver ;
      int              num_receiver ;
      int              drawing_enabled , drawing_mode ;
      Pixel            drawing_pixel ;

      int writeownsize ; /* 01 Aug 1999 */

      int tempflag ;     /* 15 Mar 2000: for quick communication of state */

      int see_ttatlas ;  /* 25 Jul 2001 */

} AFNI_view_info ;

#undef USE_WRITEOWNSIZE  /* 01 Aug 1999 */

#define SAVE_VPT(iqq)                           \
   ( (iqq)->vinfo->i1_old = (iqq)->vinfo->i1 ,  \
     (iqq)->vinfo->j2_old = (iqq)->vinfo->j2 ,  \
     (iqq)->vinfo->k3_old = (iqq)->vinfo->k3  )

#define WARPED_VIEW(vvv)  ((vvv)+1)
#define ISVALID_VIEW(vvv) ((vvv) >= FIRST_VIEW_TYPE && (vvv) <= LAST_VIEW_TYPE)

/*-----------------------------------------------------------*/

#define MAXOVSIZE 19
#define MAXOVPIX  (MAXOVSIZE*(MAXOVSIZE-1))
typedef struct {
      int numpix ;
      short dx[MAXOVPIX] , dy[MAXOVPIX] ;
} AFNI_ovtemplate ;

/*-------------------------------------------------------------------*/
/*--------------- define display control widgets --------------------*/

#define BKGD_COUNT 3
#define INIT_BKGD_LAB(iq) \
   do{ int qq = ((iq)->s123!=NULL) + ((iq)->s231!=NULL) + ((iq)->s312!=NULL); \
       if( qq >= BKGD_COUNT || (qq > 0 && AFNI_yesenv("AFNI_VALUE_LABEL")) ){ \
          (iq)->vwid->imag->do_bkgd_lab = True ;                              \
       } else {                                                               \
          (iq)->vwid->imag->do_bkgd_lab = False ;                             \
          XtUnmanageChild(im3d->vwid->imag->pop_bkgd_lab) ;                   \
          XtUnmanageChild(im3d->vwid->func->bkgd_lab) ;                       \
          FIX_SCALE_SIZE(im3d) ;                                              \
       } break ; } while(0)

#define AFNI_XHAIRS_OFF    0
#define AFNI_XHAIRS_SINGLE 1
#define AFNI_XHAIRS_MULTI  2

#define AFNI_XHAIRS_LR_AP  3  /* 31 Dec 1998 */
#define AFNI_XHAIRS_LR_IS  4
#define AFNI_XHAIRS_AP_IS  5
#define AFNI_XHAIRS_LR     6
#define AFNI_XHAIRS_AP     7
#define AFNI_XHAIRS_IS     8

#define AFNI_XHAIRS_LASTOPTION 8

typedef struct {
      Widget frame , rowcol ;
      Widget topper , popmenu , pop_bkgd_lab ,
             pop_jumpback_pb , pop_imageonly_pb , pop_jumpto_pb , pop_talto_pb ;
      Widget pop_jumpto_ijk_pb ;

      Widget crosshair_frame , crosshair_rowcol , crosshair_label ;

      Widget         xhair_rowcol ;
      MCW_arrowval * crosshair_av ;
      MCW_bbox     * xhall_bbox ;

      MCW_arrowval * crosshair_color_av ;
      MCW_arrowval * time_index_av ;

      Widget         gap_wrap_rowcol ;
      MCW_arrowval * crosshair_gap_av ;
      MCW_bbox     * wrap_bbox ;

      Widget view_frame , view_rowcol ,
             xyz_rowcol   , yzx_rowcol   , zxy_rowcol   ,
             name_xyz_lab , name_yzx_lab , name_zxy_lab ,   /* xyz = Axial    */
             image_xyz_pb , image_yzx_pb , image_zxy_pb ,   /* yzx = Sagittal */
             graph_xyz_pb , graph_yzx_pb , graph_zxy_pb  ;  /* zxy = Coronal  */

      Boolean do_bkgd_lab ;

      Widget pop_whereami_pb , pop_ttren_pb ;
      MCW_textwin * pop_whereami_twin ;

      Widget pop_sumato_pb ;
      Widget pop_mnito_pb ;  /* 01 May 2002 */
} AFNI_imaging_widgets ;

/*---*/

typedef struct {
      Widget     frame , rowcol ;
      MCW_bbox * view_bbox ;

      Widget     marks_frame , marks_rowcol ;
      Widget     define_marks_pb ;
      MCW_bbox * see_marks_bbox ;

      Widget     func_frame , func_rowcol ;
      Widget     define_func_pb ;
      MCW_bbox * see_func_bbox ;

      Widget     define_dmode_pb ;

      Widget dataset_frame , dataset_rowcol ,
             choose_sess_pb    , choose_anat_pb    , choose_func_pb   ,
             popchoose_sess_pb , popchoose_anat_pb , popchoose_func_pb ;

      Boolean marks_pb_inverted , func_pb_inverted , dmode_pb_inverted ;
} AFNI_viewing_widgets ;

#define OPEN_PANEL(iq,panel)                                             \
   {  XtManageChild( (iq)->vwid->  panel  ->frame ) ;                    \
      if( ! (iq)->vwid->view->  panel ## _pb_inverted ){                  \
         MCW_invert_widget( (iq)->vwid->view->define_ ## panel ## _pb ) ;  \
         (iq)->vwid->view->  panel ## _pb_inverted = True ; }             \
      XMapRaised( XtDisplay( (iq)->vwid->  panel  ->frame ) ,            \
                   XtWindow( (iq)->vwid->  panel  ->frame )  ) ; }

#define CLOSE_PANEL(iq,panel)                                            \
   {  XtUnmanageChild( (iq)->vwid->  panel  ->frame ) ;                  \
      if( (iq)->vwid->view->  panel ## _pb_inverted ){                    \
         MCW_invert_widget( (iq)->vwid->view->define_ ## panel ## _pb ) ;  \
         (iq)->vwid->view->  panel ## _pb_inverted = False ; } }

/*---*/

#define MARKS_MAXPOP (MARKS_MAXNUM+10)

typedef struct {
      Widget  frame , rowcol ;

      Widget  tog_frame , tog_rowcol , tog_topper ;
      Widget  tog[MARKS_MAXNUM] ;
      Widget  poptog[MARKS_MAXNUM] ;
      Widget  always_popup[MARKS_MAXPOP] ,
              sometimes_popup[MARKS_MAXPOP] ;
      int     num_always_popup , num_sometimes_popup ;
      Boolean inverted[MARKS_MAXNUM] , editable ;

      Widget         control_frame , control_rowcol ;
      MCW_bbox *     edits_bbox ;
      Widget         disp_frame     , disp_rowcol ;
      MCW_arrowval * disp_pcolor_av , * disp_scolor_av ,
                   * disp_size_av   , * disp_gap_av ;
      Widget         action_rowcol  ,
                     action_set_pb  , action_clear_pb ,
                     action_quality_pb ,
                     pop_set_pb     , pop_clear_pb ;

      Widget         transform_pb ;

      MCW_bbox *     tlrc_big_bbox ;
      AFNI_ovtemplate ov_mask ;

      int     ov_pcolor , ov_scolor , ov_size , ov_gap ;
      Boolean ov_visible , old_visible ;

      char tog_help[MARKS_MAXNUM][MARKS_MAXHELP] ;

      Boolean isprimary[MARKS_MAXNUM] ;
      Boolean changed ;

      Boolean tag_visible ;   /* 23 Oct 1998 */
} AFNI_marks_widgets ;

/*---*/

#define THR_PVAL_LABEL_NONE "[N/A]"

#define THR_TOP_EXPON  4         /* 30 Nov 1997 */
#define THR_FACTOR     0.0001    /* pow(10,-THR_TOP_EXPON) */

#undef USE_FUNC_FIM              /* 09 Dec 1997 */

typedef struct {
      Widget frame , rowcol ;

      Widget thr_rowcol , thr_label , thr_scale , thr_pval_label ;
      MCW_arrowval * thr_top_av ;

      Widget inten_rowcol , inten_label ;
      MCW_pbar     * inten_pbar ;
      MCW_arrowval * inten_av ;
      MCW_bbox     * inten_bbox ;

      Widget pbar_menu , pbar_equalize_pb , pbar_settop_pb ,
             pbar_readin_pb , pbar_writeout_pb ;
      MCW_arrowval * pbar_palette_av ;
      Widget pbar_showtable_pb ;

      Widget pbar_saveim_pb  ;                  /* 15 Jun 2000 */
      MCW_arrowval * pbar_transform0D_av ;
      generic_func * pbar_transform0D_func ;
      int            pbar_transform0D_index ;
      MCW_arrowval * pbar_transform2D_av ;
      generic_func * pbar_transform2D_func ;
      int            pbar_transform2D_index ;

      Widget options_rowcol , options_label ;
      MCW_bbox     * underlay_bbox ;
      MCW_bbox     * functype_bbox ;

      Widget         buck_frame , buck_rowcol ;
      MCW_arrowval * anat_buck_av , * fim_buck_av , * thr_buck_av ;  /* 30 Nov 1997 */

      Widget range_frame , range_rowcol , range_label ;
      MCW_bbox     * range_bbox ;
      MCW_arrowval * range_av ;

#ifdef USE_FUNC_FIM
      Widget fim_frame , fim_rowcol , fim_dset_label , fim_mbar ;
      FIM_menu * fim_menu ;
#endif

      Widget bkgd_lab ;

      MCW_arrowval * range_rotate_av ;  /* 30 Mar 2001 */

      MCW_bbox * see_ttatlas_bbox ;     /* 25 Jul 2001 */
} AFNI_function_widgets ;

#define PBAR_MODEBUT  0
#define PBAR_MODEPOS  (1 << PBAR_MODEBUT)

#define RANGE_AUTOBUT 0
#define RANGE_AUTOVAL (1 << RANGE_AUTOBUT)

/** On Motif 2.0 on Linux, resized pbar pieces causes the
    threshold scale to behave bizarrely.  This macro is a fixup **/

#ifdef FIX_SCALE_SIZE_PROBLEM
#  define FIX_SCALE_SIZE(iqqq)                                \
     do{ int sel_height ;  XtPointer sel_ptr ;                \
         XtVaGetValues( (iqqq)->vwid->func->thr_scale ,       \
                           XmNuserData , &sel_ptr , NULL ) ;  \
         sel_height = (int) sel_ptr ;                         \
         XtVaSetValues( (iqqq)->vwid->func->thr_scale ,       \
                           XmNheight , sel_height , NULL ) ;  \
         XtManageChild((iqqq)->vwid->func->thr_scale) ;       \
       } while(0)
#  define HIDE_SCALE(iqqq) XtUnmanageChild((iqqq)->vwid->func->thr_scale)
#else
#  define FIX_SCALE_SIZE(iqqq) /* nada */
#  define HIDE_SCALE(iqqq)   /* nada */
#endif

#ifdef FIX_SCALE_VALUE_PROBLEM
#  define BOXUP_SCALE
#  define FIX_SCALE_VALUE(iqqq)                                                    \
   do { char buf[16] ; float th = (iqqq)->vinfo->func_threshold ;                  \
        if( ISVALID_3DIM_DATASET((iqqq)->fim_now) &&                               \
            FUNC_HAVE_THR((iqqq)->fim_now->func_type) ){                           \
                                                                                   \
          th *= (iqqq)->vinfo->func_thresh_top ;                                   \
             if( th <   1.0 ) sprintf(buf,"%4.2f",th) ;                            \
        else if( th <  10.0 ) sprintf(buf,"%4.1f",th) ;                            \
        else                  sprintf(buf,"%4.0f",th) ;                            \
        XtVaSetValues( (iqqq)->vwid->func->thr_label , LABEL_ARG(buf+1) , NULL ) ; \
        MCW_expose_widget( (iqqq)->vwid->func->thr_label ) ; } } while(0)
#else
#  define FIX_SCALE_VALUE(iqqq) /* nada */
#endif

typedef struct {
      Widget frame , rowcol ;

      MCW_bbox     * anatmode_bbox , * funcmode_bbox ;
      MCW_arrowval * anat_resam_av , * resam_vox_av ;
      MCW_arrowval * func_resam_av ;                   /* moved here 03 Nov 1996 */
      MCW_arrowval * thr_resam_av ;                    /* 09 Dec 1997 */
      Widget         write_rowcol , write_anat_pb , write_func_pb , write_many_pb ;
      Widget         rescan_rowcol , rescan_pb , rescan_all_pb , rescan_timeseries_pb ;
      Widget         read_rowcol , read_sess_pb , read_1D_pb , read_Web_pb ;

      Widget         mbar_rowcol ;
      MCW_bbox     * lock_bbox ;
      Widget         lock_enforce_pb , lock_clear_pb , lock_setall_pb ;

      Widget         misc_voxind_pb ;
      Widget         misc_hints_pb ;
      Widget         misc_anat_info_pb , misc_func_info_pb ;
      Widget         misc_vcheck_pb, misc_purge_pb, misc_tracing_pb,
                     misc_showmalloc_pb , misc_dumpmalloc_pb ;

      MCW_bbox     * time_lock_bbox ;  /* 03 Nov 1998 */

      Widget         misc_writeownsize_pb ;   /* 01 Aug 1999 */
      MCW_bbox     * misc_writeownsize_bbox ;

      MCW_bbox     * misc_voxind_bbox , * misc_hints_bbox ; /* 01 Aug 1999 */

#ifdef ALLOW_PLUGINS
      Widget         misc_environ_pb ;  /* 20 Jun 2000 */
      Widget         misc_2dchain_pb ;  /* 03 Jul 2000 */
      Widget         misc_1dchain_pb ;  /* 07 Aug 2001 */
#endif

      MCW_bbox     * ijk_lock_bbox ;    /* 11 Sep 2000 */

      Widget         misc_savelayout_pb ; /* 23 Sep 2000 */
      Widget         misc_license_pb ;    /* 03 Dec 2000 */
      Widget         misc_plugout_pb ;    /* 07 Nov 2001 */
      Widget         misc_niml_pb    ;    /* 02 Mar 2002 */

} AFNI_datamode_widgets ;

/*---*/

#define USE_HIDDEN

typedef struct {
   Widget frame , rowcol ;

   Widget  rc_top , clone_pb , panel_pb ;
   Widget  rc_bot , button_help_pb , quit_pb ;
   Boolean quit_first , panel_pb_inverted ;

#ifdef USE_HIDDEN

   /** May 1995: hidden popup widgets **/

   Widget hidden_menu ,
            hidden_pts_cbut , hidden_pts_menu ,
            hidden_readpts_ijk_pb , hidden_readpts_xyz_pb ,
            hidden_writepts_ijk_pb , hidden_writepts_xyz_pb ,
            hidden_colorpts_pb ;

   Widget hidden_sonnet_pb ;

#define PTS_READ_IJK   7
#define PTS_READ_XYZ   8
#define PTS_WRITE_IJK  9
#define PTS_WRITE_XYZ 10
#define PTS_SET_COLOR 11

   int    hidden_code ;

   Widget hidden_mission_pb ;  /* 06 Jun 2001 */

#endif  /* USE_HIDDEN */

} AFNI_program_widgets ;

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
#ifdef USE_HIDDEN

extern void AFNI_hidden_CB    ( Widget , XtPointer , XtPointer );
extern void AFNI_hidden_EV    ( Widget , XtPointer , XEvent * , Boolean * ) ;
extern void AFNI_hidden_pts_CB( Widget , XtPointer , MCW_choose_cbs * ) ;

#ifdef USE_SONNETS
extern void AFNI_sonnet_CB    ( Widget , XtPointer , XtPointer );
#endif

#endif  /* USE_HIDDEN*/
/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/

/*---*/

struct PLUGIN_interface ; /* incomplete definition */

typedef struct {
      Widget top_shell , top_form ;

      AFNI_imaging_widgets  * imag ;
      AFNI_viewing_widgets  * view ;
      AFNI_marks_widgets    * marks ;
      AFNI_function_widgets * func ;
      AFNI_program_widgets  * prog ;
      AFNI_datamode_widgets * dmode ;

      Widget picture ;
      int    picture_index ;

      Widget file_dialog , file_sbox ;
      XtCallbackProc file_cb ;
      XtPointer      file_cd ;

#ifdef ALLOW_PLUGINS
      int nplugbut ;                      /* 23 Sep 2000 */
      Widget * plugbut ;
      char ** pluglab ;
      struct PLUGIN_interface ** plugint ;
#endif

      /*--- Pointers to other data ---*/

      XtPointer parent ;
} AFNI_widget_set ;

/** picture controls **/

#undef OLD_PICTURE
#ifdef OLD_PICTURE
#  define PICTURE_ON(im)  XtSetSensitive( (im)->vwid->picture , False )
#  define PICTURE_OFF(im) XtSetSensitive( (im)->vwid->picture , True )
#else
#  define PICTURE_ON(im)                                                      \
     do{ if( (im)->type == AFNI_3DDATA_VIEW )                                 \
          XtVaSetValues((im)->vwid->picture,XmNlabelPixmap,logo_pixmap,NULL); \
       } while(0)

#  define PICTURE_OFF(im)                                                               \
     do{ if( (im)->type == AFNI_3DDATA_VIEW )                                           \
          XtVaSetValues((im)->vwid->picture,XmNlabelPixmap,XmUNSPECIFIED_PIXMAP,NULL ); \
       } while(0)
#endif

/*-----------------------------*/
/*----- Data for FIM-age ------*/

#define FIM_ALPHA_MASK  1
#define FIM_BEST_MASK   2
#define FIM_PERC_MASK   4
#define FIM_BASE_MASK   8
#define FIM_CORR_MASK  16
#define FIM_PAVE_MASK  32
#define FIM_AVER_MASK  64

#define FIM_PTOP_MASK 128  /* these 3 added on 03 Jan 2000 */
#define FIM_TOPL_MASK 256
#define FIM_SIGM_MASK 512

#define FIM_DEFAULT_MASK (1 | 2 | 4 | 16)

#define FIM_NUM_OPTS   10

#ifdef MAIN
   char * fim_opt_labels[FIM_NUM_OPTS] = {
     "Fit Coef" , "Best Index" , "% Change"    , "Baseline" ,
                                 "Correlation" ,
                                 "% From Ave"  , "Average"  ,
                                 "% From Top"  , "Topline"  ,
                                 "Sigma Resid"
   } ;
#else
   extern char * fim_opt_labels[] ;
#endif

typedef struct {
   MRI_IMAGE *        fimref ;
   MRI_IMAGE *        fimort ;   /* 12 Nov 1996 */
   THD_3dim_dataset * fimdset ;
   int                refadd_count ;
   int                init_ignore ;
   int                polort ;   /* 27 May 1999 */
} AFNI_fimmer_type ;

#define MAX_POLORT 3

#define CLEAR_FIMDATA(iq)                            \
   ( (iq)->fimdata->fimref       = NULL ,            \
     (iq)->fimdata->fimort       = NULL ,            \
     (iq)->fimdata->fimdset      = NULL ,            \
     (iq)->fimdata->refadd_count = 0 ,               \
     (iq)->fimdata->polort       = INIT_fim_polort , \
     (iq)->fimdata->init_ignore  = GLOBAL_argopt.ignore )

#define USABLE_FIMDATA(iq)                                         \
  ( IM3D_OPEN(iq) && (iq)->type == AFNI_3DDATA_VIEW              && \
    DSET_GRAPHABLE((iq)->fimdata->fimdset)                        && \
    (iq)->fimdata->fimref != NULL                                  && \
    (iq)->fimdata->fimref->kind == MRI_float                        && \
    (iq)->fimdata->fimref->nx >= DSET_NUM_TIMES((iq)->fimdata->fimdset) )

#ifdef USE_FUNC_FIM
#  define ALLOW_COMPUTE_FIM(iq)                                           \
     do{ XmString xstr ; THD_3dim_dataset * ds = (iq)->fimdata->fimdset ; \
         int fim_ok = USABLE_FIMDATA(iq) ,                                \
             ds_ok  = DSET_GRAPHABLE((iq)->fimdata->fimdset) ;            \
         STATUS("** Setting FIM controls") ;                              \
         if( ISVALID_3DIM_DATASET(ds) ){                                  \
            STATUS("setting fim string to dataset") ;                     \
            xstr = XmStringCreateLtoR(ds->dblk->diskptr->filecode,        \
                                    XmFONTLIST_DEFAULT_TAG);              \
         } else {                                                         \
            STATUS("setting fim string to nothing") ;                     \
            xstr = XmStringCreateLtoR("[FIM not set up]",                 \
                                    XmFONTLIST_DEFAULT_TAG);              \
         }                                                                \
         STATUS("setting fim label to fim string") ;                      \
         XtVaSetValues( (iq)->vwid->func->fim_dset_label ,                \
                           XmNlabelString , xstr , NULL ) ;               \
         XmStringFree(xstr) ;                                             \
         SENSITIZE((iq)->vwid->func->fim_menu->fim_execute_pb,fim_ok);    \
         if( ds_ok ) AFNI_fimmer_fix_optmenu(iq) ;                        \
         STATUS("** Done setting FIM controls") ;                         \
      } while(0)
#else
#  define ALLOW_COMPUTE_FIM(iq) /* nada */
#endif

/*-----------------------------------------------------------*/
/*------------- define the central data structure -----------*/

#define AFNI_NODATA_VIEW 100
#define AFNI_IMAGES_VIEW 101
#define AFNI_3DDATA_VIEW 102

#define IM0D_VALID(ii) ((ii)!=NULL && (ii)->type==AFNI_NODATA_VIEW)
#define IM3D_VALID(ii) ((ii)!=NULL && ((ii)->type==AFNI_IMAGES_VIEW || \
                                       (ii)->type==AFNI_3DDATA_VIEW ) )

#define IM3D_OPEN(ii)  (IM3D_VALID(ii) && (ii)->opened)

#define ISVALID_IM3D(ii) IM3D_VALID(ii)

#define AFNI_IGNORE_NOTHING    0
#define AFNI_IGNORE_REDRAWS    1
#define AFNI_IGNORE_EVERYTHING 2

typedef struct {
      int type , opened ;
      MCW_DC * dc ;

      THD_session      * ss_now ;   /* session now being viewed */
      THD_3dim_dataset * anat_dset[LAST_VIEW_TYPE+1] ,   /* datasets now */
                       * fim_dset [LAST_VIEW_TYPE+1]  ;  /* being viewed */
      THD_3dim_dataset * anat_now , * fim_now ;  /* REALLY now being viewed */

      AFNI_view_info   * vinfo ;  /* information about what's being viewed */

      AFNI_fimmer_type * fimdata ; /* information about fimming */

      FD_brick  * b123_anat , * b231_anat , * b312_anat ; /* anat */
      FD_brick  * b123_fim  , * b231_fim  , * b312_fim  ; /* funcs */
      FD_brick  * b123_ulay , * b231_ulay , * b312_ulay ; /* underlays */

      MCW_imseq   * s123    , * s231      , * s312 ;      /* viewers */
      MCW_grapher * g123    , * g231      , * g312 ;      /* graphs */

      AFNI_widget_set  * vwid ;
      char window_title[THD_MAX_NAME] ;
      int ignore_seq_callbacks ;

      THD_dataxes * wod_daxes ;                 /* 02 Nov 1996 */
      THD_warp * anat_voxwarp , * fim_voxwarp ;
      int anat_wod_flag , fim_wod_flag ;

      KILL_list kl ;
      XtPointer parent ;

      int brand_new ;                           /* 07 Dec 2001 */
} Three_D_View ;

/* 02 Nov 1996: macro to load current viewing data into current datasets */

#define LOAD_ANAT_VIEW(iq)                               \
  do{ (iq)->anat_now->wod_daxes = (iq)->wod_daxes ;      \
      (iq)->anat_now->wod_flag  = (iq)->anat_wod_flag ;  \
      (iq)->anat_now->vox_warp  = (iq)->anat_voxwarp ;   \
    } while(0)

#define LOAD_FUNC_VIEW(iq)                               \
  do{ if( ISVALID_3DIM_DATASET((iq)->fim_now) ){         \
         (iq)->fim_now->wod_daxes = (iq)->wod_daxes ;    \
         (iq)->fim_now->wod_flag  = (iq)->fim_wod_flag ; \
         (iq)->fim_now->vox_warp  = (iq)->fim_voxwarp ;  \
      } } while(0)

#define LOAD_DSET_VIEWS(iq) \
  do{ LOAD_ANAT_VIEW(iq) ; LOAD_FUNC_VIEW(iq) ; } while(0)

extern int AFNI_count_controllers(void) ;
extern void AFNI_controller_clonify(void) ;
extern Three_D_View * new_AFNI_controller( Widget , MCW_DC * , int ) ;
extern void AFNI_initialize_controller( Three_D_View * ) ;
extern void AFNI_purge_dsets(int) ;
extern void AFNI_purge_unused_dsets(void) ;
extern int AFNI_controller_index( Three_D_View * ) ;

extern Three_D_View * AFNI_find_open_controller(void) ; /* 05 Mar 2002 */
extern void AFNI_popup_message( char * ) ;

extern char * AFNI_get_friend(void) ;  /* 26 Feb 2001 */

#define OPEN_CONTROLLER(iq)                                  \
 do{ XtRealizeWidget((iq)->vwid->top_shell) ;                \
     while(XtWindow((iq)->vwid->top_shell)==(Window)NULL) ;  \
     AFNI_startup_3dview(iq); (iq)->opened = 1;              \
 } while(0)

#define CLOSE_CONTROLLER(iq) \
 ( AFNI_closedown_3dview(iq), XtUnrealizeWidget((iq)->vwid->top_shell), (iq)->opened = 0 )

#define PARENTIZE(ds,par) \
   if( ISVALID_3DIM_DATASET((ds)) ) (ds)->parent = (XtPointer) (par)

/* macros to return the analogous grapher given a viewer, and vice-versa */

#define VIEWER_TO_GRAPHER(iq,ss) (((ss)==(iq)->s123) ? (iq)->g123 :  \
                                  ((ss)==(iq)->s231) ? (iq)->g231 :  \
                                  ((ss)==(iq)->s312) ? (iq)->g312 : NULL)

#define GRAPHER_TO_VIEWER(iq,gg)  (((gg)==(iq)->g123) ? (iq)->s123 :  \
                                   ((gg)==(iq)->g231) ? (iq)->s231 :  \
                                   ((gg)==(iq)->g312) ? (iq)->s312 : NULL)

#define UNDERLAY_TO_VIEWER(iq,bb) (((bb)==(iq)->b123_ulay) ? (iq)->s123 :  \
                                   ((bb)==(iq)->b231_ulay) ? (iq)->s231 :  \
                                   ((bb)==(iq)->b312_ulay) ? (iq)->s312 : NULL)

#define UNDERLAY_TO_GRAPHER(iq,bb) (((bb)==(iq)->b123_ulay) ? (iq)->g123 :  \
                                    ((bb)==(iq)->b231_ulay) ? (iq)->g231 :  \
                                    ((bb)==(iq)->b312_ulay) ? (iq)->g312 : NULL)

#define UNDERLAY_TO_OVERLAY(iq,bb) (((bb)==(iq)->b123_ulay) ? (iq)->b123_fim :  \
                                    ((bb)==(iq)->b231_ulay) ? (iq)->b231_fim :  \
                                    ((bb)==(iq)->b312_ulay) ? (iq)->b312_fim : NULL)

/*-----------------------------------------------------------------------------*/
/*---------------------------- Global library data ----------------------------*/

#include "afni_plugin.h"
#ifdef ALLOW_PLUGINS

   /*-- pseudo-plugin functions --*/

   extern PLUGIN_interface * ENV_init(void) ;            /* 20 Jun 2000 */
   extern void ENV_add_numeric( char * , char * ,
                                int , int , int , int , generic_func * ) ;
   extern void ENV_add_string( char * , char * ,
                               int , char ** , generic_func * ) ;
   extern void ENV_add_yesno( char * , char * ) ;        /* 08 Aug 2001 */

   extern PLUGIN_interface * F2D_init(void) ;            /* 03 Jul 2000 */
   extern PLUGIN_interface * F1D_init(void) ;            /* 08 Aug 2001 */
#endif

typedef struct {                 /* windows and widgets */
   XtPointer_array * windows ;   /* allowed to interrupt */
   XtPointer_array * widgets ;   /* 'real-time' functions */
} MCW_interruptables ;

#define MAX_CONTROLLERS 5

/*-------------- Here there be global variables.  So shoot me. --------------*/

#include "afni_setup.h"  /* 19 Dec 1997 */

typedef struct {
   MCW_DC * dc ;                                  /* display context for everyone */
   THD_sessionlist * sslist ;                     /* all sessions viewable */
   MRI_IMARR * timeseries ;                       /* all timeseries available */
   Three_D_View * controllers[MAX_CONTROLLERS] ;  /* all controllers available */
   MCW_interruptables interruptables ;            /* windows and widgets */

   MCW_function_list registered_0D ;              /* registered functions */
   MCW_function_list registered_1D ;
   MCW_function_list registered_2D ;

   int controller_lock , ignore_lock ;
   int have_dummy_dataset ;
   int sesstrail ;                                /* 23 Oct 1998 */

   THD_coorder cord ;

#ifdef ALLOW_PLUGINS
   struct AFNI_plugin_array * plugins ;           /* plugins */
#endif

   PBAR_palette_table * gpt ;

   int time_lock ;                                /* 03 Nov 1998 */

   int hints_on ;                                 /* 01 Aug 1999 */

   float fim_bkthr_perc ;                         /* 02 Jun 1999 */

   MCW_function_list registered_fim ;             /* 30 Jan 2000 */

   int ijk_lock ;                                 /* 11 Sep 2000 */

   THD_session *session ;                         /* 20 Dec 2001 */

   MCW_function_list registered_slice_proj ;      /* 31 Jan 2002 */

} AFNI_library_type ;

#ifdef MAIN
   AFNI_library_type GLOBAL_library ;
#else
   extern AFNI_library_type GLOBAL_library ;
#endif

#define FIM_THR          (0.01*GLOBAL_library.fim_bkthr_perc)  /* 02 Jun 1999 */
#define SET_FIM_bkthr(v) (GLOBAL_library.fim_bkthr_perc = (v))

#define DISABLE_LOCK    (GLOBAL_library.ignore_lock=1)
#define ENABLE_LOCK     (GLOBAL_library.ignore_lock=0)
#define BEEPIT          XBell(GLOBAL_library.dc->display,100)
#define ALLOW_real_time GLOBAL_argopt.allow_rt
#define ELIDE_quality   GLOBAL_argopt.elide_quality
#define GPT             GLOBAL_library.gpt
#define NO_frivolities  GLOBAL_argopt.no_frivolities
#define SESSTRAIL       GLOBAL_library.sesstrail
#define AFNI_VERBOSE    (!GLOBAL_argopt.quiet)  /* 25 Oct 2001 */

# define SUMA_ENABLED   GLOBAL_argopt.enable_suma

#define DOING_REALTIME_WORK (GLOBAL_library.interruptables.windows != NULL)

#define UNDUMMYIZE                                                              \
 do { GLOBAL_library.have_dummy_dataset = 0 ;                                   \
      XtSetSensitive(GLOBAL_library.controllers[0]->vwid->prog->clone_pb,True); \
    } while(0)

/*-----------------------------------------------------------*/
/*------------------------ prototypes -----------------------*/

extern int AFNI_vnlist_func_overlay( Three_D_View *, SUMA_irgba **, int * ) ;

extern void AFNI_parse_args( int argc , char * argv[] );
extern void FatalError(char * str);

extern void AFNI_splashup   (void) ;  /* 02 Aug 1999 */
extern void AFNI_splashdown (void) ;
extern void AFNI_splashraise(void) ;  /* 25 Sep 2000 */

extern void AFNI_quit_CB           ( Widget wcall , XtPointer cd , XtPointer cbs );
extern void AFNI_quit_timeout_CB   ( XtPointer , XtIntervalId * ) ;
extern void AFNI_startup_timeout_CB( XtPointer , XtIntervalId * ) ;

extern void AFNI_startup_layout_CB  ( XtPointer, XtIntervalId * ) ; /* 23 Sep 2000 */
extern void AFNI_save_layout_CB     ( Widget, XtPointer, XtPointer ) ;
extern void AFNI_finalsave_layout_CB( Widget, XtPointer, MCW_choose_cbs * ) ;

extern void AFNI_decode_geom( char * , int *, int *, int *, int * ) ;

extern void AFNI_clone_controller_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_controller_panel_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_make_controller( int ) ;  /* 23 Sep 2000 */

/* "locks" 04 Nov 1996 */
extern void AFNI_lock_enforce_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_lock_change_CB ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_lock_clear_CB  ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_lock_setall_CB ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_lock_carryout  ( Three_D_View * ) ;

extern void AFNI_time_lock_carryout( Three_D_View * ) ;  /* 03 Nov 1998 */
extern void AFNI_time_lock_change_CB( Widget , XtPointer , XtPointer ) ;

extern void AFNI_ijk_lock_change_CB( Widget , XtPointer , XtPointer ) ;

extern XtPointer AFNI_brick_to_mri( int n , int type , FD_brick * br );

extern THD_3dim_dataset * AFNI_read_images( int nf , char * fname[] );

extern void AFNI_seq_send_CB(MCW_imseq   * seq    ,FD_brick * br,ISQ_cbs * cbs);
extern void AFNI_gra_send_CB(MCW_grapher * grapher,FD_brick * br,GRA_cbs * cbs);

extern void AFNI_read_inputs   ( int argc, char * argv[] );
extern void AFNI_make_widgets  ( Three_D_View * im3d );
extern void AFNI_closedown_3dview( Three_D_View * im3d );
extern void AFNI_startup_3dview  ( Three_D_View * im3d ); /* 15 Jun 2000 */
extern MRI_IMAGE * AFNI_overlay( int n , FD_brick * br );

extern char * AFNI_controller_label( Three_D_View * im3d ); /* 01 Apr 1999 */
extern void AFNI_set_window_titles( Three_D_View * im3d );

extern void AFNI_crosshair_visible_CB( MCW_arrowval * , XtPointer ) ;
extern void AFNI_view_xyz_CB         ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_marktog_CB          ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_marks_action_CB     ( Widget , XtPointer , XtPointer ) ;

#define AFNI_SEE_FUNC_ON(iq) ( MCW_set_bbox( (iq)->vwid->view->see_func_bbox, 1 ), \
                               AFNI_see_func_CB( NULL , (XtPointer)(iq) , NULL )  )

#define AFNI_SEE_FUNC_OFF(iq) ( MCW_set_bbox( (iq)->vwid->view->see_func_bbox, 0), \
                                AFNI_see_func_CB( NULL , (XtPointer)(iq) , NULL ) )

#define AFNI_SETUP_FUNC_ON(iq)                                \
  do{ if( ! (iq)->vinfo->func_visible ){                      \
         MCW_set_bbox( (iq)->vwid->view->see_func_bbox, 1 ) ; \
         (iq)->vinfo->func_visible = True ;                   \
      } } while(0)

#define AFNI_SWITCH_VIEW(iq,vv)                                    \
   do{ if( (iq)->vinfo->view_type != (vv) ){                       \
         MCW_set_bbox( (iq)->vwid->view->view_bbox , 1 << (vv) ) ; \
         AFNI_switchview_CB( NULL , (XtPointer)(iq) , NULL ) ;     \
       } } while(0) ;

#define AFNI_SETUP_VIEW(iq,vv)                                     \
   do{ if( (iq)->vinfo->view_type != (vv) ){                       \
         MCW_set_bbox( (iq)->vwid->view->view_bbox , 1 << (vv) ) ; \
         (iq)->vinfo->view_type = (vv) ;                           \
       } } while(0) ;

extern void AFNI_switchview_CB        ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_see_marks_CB         ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_functype_CB          ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_see_func_CB          ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_marks_edits_CB       ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_marks_transform_CB   ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_imag_pop_CB          ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_define_CB            ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_underlay_CB          ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_choose_dataset_CB    ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_write_dataset_CB     ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_write_many_dataset_CB( Widget , XtPointer , XtPointer ) ; /* 23 Nov 1996 */
extern void AFNI_anatmode_CB          ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_funcmode_CB          ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_raiseup_CB           ( Widget , XtPointer , XtPointer ) ;

extern void AFNI_do_many_writes      ( Widget , XtPointer , MCW_choose_cbs * ) ; /* 23 Nov 1996 */
extern void AFNI_finalize_dataset_CB ( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void AFNI_jumpto_CB           ( Widget , XtPointer , MCW_choose_cbs * ) ;
extern int  AFNI_jumpto_dicom        ( Three_D_View * , float, float, float  ) ;
extern int  AFNI_jumpto_ijk          ( Three_D_View * , int, int, int  ) ;
extern void AFNI_jumpto_ijk_CB       ( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void AFNI_sumato_CB           ( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void AFNI_mnito_CB            ( Widget , XtPointer , MCW_choose_cbs * ) ;

extern void AFNI_fimmer_pickref_CB   ( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void AFNI_fimmer_pickort_CB   ( Widget , XtPointer , MCW_choose_cbs * ) ;

#ifdef USE_FUNC_FIM
extern void AFNI_fimmer_fix_optmenu( Three_D_View * ) ;
extern void AFNI_fimmer_menu_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_fimmer_dset_choose_CB( Widget, XtPointer, MCW_choose_cbs * ) ;
# ifdef USE_OPTMENUS
  extern void AFNI_fimmer_ignore_choose_CB( MCW_arrowval *, XtPointer ) ;
# else
  extern void AFNI_fimmer_ignore_choose_CB( Widget, XtPointer, MCW_choose_cbs * ) ;
# endif
#endif

/*------------------------------------------------------------------
   31 Jan 2000 - this stuff for user-defined fimfuncs
--------------------------------------------------------------------*/

typedef struct {
   MRI_IMAGE * ref_ts , * ort_ts ;
   int nvox , ignore , polort ;
} FIMdata ;

extern void AFNI_register_fimfunc( char *, int, generic_func *, void * );
extern void spearman_fimfunc( int, float *, void *, int, void * );
extern void quadrant_fimfunc( int, float *, void *, int, void * );

/*-------------------------------------------------------------------*/

extern void AFNI_fimmer_setref( Three_D_View * , MRI_IMAGE * ) ;
extern void AFNI_fimmer_setort( Three_D_View * , MRI_IMAGE * ) ;
extern void AFNI_fimmer_setignore( Three_D_View * , int ) ;
extern void AFNI_fimmer_setpolort( Three_D_View * , int ) ;
extern void AFNI_rescan_session( int ) ;
extern void AFNI_rescan_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_rescan_all_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_rescan_timeseries_CB( Widget , XtPointer , XtPointer ) ;

extern void AFNI_read_sess_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_finalize_read_sess_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_make_file_dialog( Three_D_View * ) ;
extern void AFNI_close_file_dialog_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_read_1D_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_finalize_read_1D_CB( Widget , XtPointer , XtPointer ) ;

extern int  DSET_in_global_session( THD_3dim_dataset * ) ;       /* 20 Dec 2001 */
extern void AFNI_append_sessions( THD_session *, THD_session *); /* 20 Dec 2001 */

extern void AFNI_read_Web_CB( Widget, XtPointer, XtPointer );    /* 26 Mar 2001 */
extern void AFNI_finalize_read_Web_CB( Widget, XtPointer, MCW_choose_cbs * );

extern void AFNI_fimmer_execute( Three_D_View * , int,int ) ;

extern void AFNI_process_interrupts( Widget ) ;
extern void AFNI_add_interruptable( Widget ) ;

extern int AFNI_ts_in_library( MRI_IMAGE * tsim ) ;

extern THD_3dim_dataset * AFNI_fimmer_compute( Three_D_View * ,
                                               THD_3dim_dataset * , MRI_IMAGE *,
                                               MRI_IMAGE *, THD_session *,
                                               int,int ) ;

extern void AFNI_fimmer_redisplay( int , Three_D_View * , THD_3dim_dataset * ) ;

extern void AFNI_crosshair_color_CB( MCW_arrowval * , XtPointer ) ;
extern void AFNI_crosshair_gap_CB  ( MCW_arrowval * , XtPointer ) ;
extern void AFNI_time_index_CB     ( MCW_arrowval * , XtPointer ) ;
extern void AFNI_marks_disp_av_CB  ( MCW_arrowval * , XtPointer ) ;
extern void AFNI_resam_vox_av_CB   ( MCW_arrowval * , XtPointer ) ;
extern char * AFNI_resam_texter    ( MCW_arrowval * , XtPointer ) ;
extern void   AFNI_resam_av_CB     ( MCW_arrowval * , XtPointer ) ;

extern void   AFNI_bucket_CB      ( MCW_arrowval * , XtPointer ) ; /* 30 Nov 1997 */
extern char * AFNI_bucket_label_CB( MCW_arrowval * , XtPointer ) ;

extern Boolean AFNI_refashion_dataset( Three_D_View * ,
                                       THD_3dim_dataset *, THD_dataxes * , int ) ;

#define REDISPLAY_OPTIONAL 0
#define REDISPLAY_OVERLAY  1
#define REDISPLAY_ALL      2

extern void AFNI_set_viewpoint( Three_D_View * , int,int,int , int ) ;
extern void AFNI_redisplay_func( Three_D_View * ) ; /* 05 Mar 2002 */

extern XmString AFNI_crosshair_label( Three_D_View * ) ;
extern XmString AFNI_range_label( Three_D_View * ) ;
extern XmString AFNI_autorange_label( Three_D_View * ) ;

extern void AFNI_range_bbox_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_range_av_CB  ( MCW_arrowval * , XtPointer ) ;
extern void AFNI_inten_bbox_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_wrap_bbox_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_xhall_bbox_CB( Widget , XtPointer , XtPointer ) ;

extern void AFNI_see_ttatlas_CB( Widget, XtPointer, XtPointer ) ; /* 25 Jul 2001 */

extern void AFNI_range_rotate_av_CB( MCW_arrowval *, XtPointer ); /* 30 Mar 2001 */
extern void AFNI_hintize_pbar( MCW_pbar * ,  float ) ;            /* 30 Mar 2001 */

#define HINTIZE_pbar(iq)                            \
  AFNI_hintize_pbar( (iq)->vwid->func->inten_pbar , \
                    ((iq)->vinfo->fim_range != 0.0) \
                     ? (iq)->vinfo->fim_range       \
                     : (iq)->vinfo->fim_autorange )    /* 15 Aug 2001 */

extern void AFNI_reset_func_range( Three_D_View * ) ;

extern int AFNI_first_tog( int , Widget * ) ;
extern int AFNI_all_tog  ( int , Widget * ) ;
extern void AFNI_set_tog ( int , int , Widget * ) ;

extern void AFNI_make_ptmask( int , int , AFNI_ovtemplate * ) ;
extern void AFNI_make_tagmask( int , int , AFNI_ovtemplate * ) ; /* Oct 1998 */

extern void AFNI_initialize_view( THD_3dim_dataset * , Three_D_View * ) ;

extern void AFNI_setup_viewing(  Three_D_View * , Boolean ) ;
extern void AFNI_modify_viewing( Three_D_View * , Boolean ) ;

extern int AFNI_can_transform_vector( THD_3dim_dataset *, THD_3dim_dataset * );

extern THD_fvec3 AFNI_transform_vector( THD_3dim_dataset * ,
                                        THD_fvec3 , THD_3dim_dataset * ) ;
extern THD_fvec3 AFNI_backward_warp_vector( THD_warp * , THD_fvec3 ) ;
extern THD_fvec3 AFNI_forward_warp_vector ( THD_warp * , THD_fvec3 ) ;

extern THD_warp * AFNI_make_warp( Three_D_View * ) ;

extern Boolean AFNI_marks_quality_check( Boolean , Three_D_View * ) ;

extern THD_3dim_dataset * AFNI_init_warp( Three_D_View * ,
                                          THD_3dim_dataset * ,
                                          THD_warp * , float  ) ;

extern AFNI_arrowpad_CB( MCW_arrowpad * , XtPointer ) ;
extern void AFNI_handler( char * ) ;

extern void AFNI_thr_scale_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_set_thr_pval( Three_D_View * im3d ) ;
extern void AFNI_thr_scale_drag_CB( Widget , XtPointer , XtPointer ) ;

extern void AFNI_inten_pbar_CB( MCW_pbar * , XtPointer , int ) ;
extern void AFNI_inten_av_CB( MCW_arrowval * , XtPointer ) ;

extern void   AFNI_set_thresh_top( Three_D_View * , float ) ;
extern char * AFNI_thresh_tlabel_CB( MCW_arrowval * , XtPointer ) ;
extern void   AFNI_thresh_top_CB( MCW_arrowval * , XtPointer ) ;

extern void AFNI_set_valabel( FD_brick *, int, MRI_IMAGE *, char * ) ;

extern void AFNI_init_niml( void ) ; /* 28 Feb 2002 */

/*-------------------------------------------------------------------
  Include prototypes for actual data warping and slicing here.
--------------------------------------------------------------------*/

#include "afni_warp.h"

/*------------------------------------------------------------------*/

extern THD_3dim_dataset * AFNI_follower_dataset( THD_3dim_dataset * ,
                                                 THD_3dim_dataset *  ) ;

extern void AFNI_make_descendants( THD_sessionlist * ) ;
extern void AFNI_mark_for_death  ( THD_sessionlist * ) ;
extern void AFNI_andersonville   ( THD_sessionlist * , Boolean ) ;
extern void AFNI_force_adoption  ( THD_session * , Boolean ) ;

extern MRI_IMAGE * AFNI_func_overlay( int , FD_brick * ) ;

extern void AFNI_syntax(void) ;

#define AFNI_DEFAULT_CURSOR 888
#define AFNI_WAITING_CURSOR 999

#define SHOW_AFNI_PAUSE AFNI_set_cursor( AFNI_WAITING_CURSOR )
#define SHOW_AFNI_READY AFNI_set_cursor( AFNI_DEFAULT_CURSOR )

extern void AFNI_set_cursor( int ) ;
extern void AFNI_imseq_clearstat( Three_D_View * ) ;

extern void AFNI_copy_statistics( THD_3dim_dataset * , THD_3dim_dataset * ) ;

extern void AFNI_lock_button( Three_D_View * ) ;
extern void AFNI_misc_button( Three_D_View * ) ;
extern void AFNI_misc_CB    ( Widget , XtPointer , XtPointer );

extern void AFNI_add_timeseries( MRI_IMAGE * ) ;

/*----------------------------------------------------------------*/
/*----- stuff for dataset drawing, etc. (see afni_receive.c) -----*/

/* coordinate converters */

extern void AFNI_ijk_to_xyz( THD_3dim_dataset * ,
                             int,int,int, float *,float *,float *) ;

extern void AFNI_xyz_to_ijk( THD_3dim_dataset * ,
                             float,float,float , int *,int *,int *) ;

extern void AFNI_xyz_to_dicomm( THD_3dim_dataset * ,
                                float,float,float , float *,float *,float *) ;

extern void AFNI_dicomm_to_xyz( THD_3dim_dataset * ,
                                float,float,float , float *,float *,float *) ;


/* masks for input to AFNI_receive_init */

#define RECEIVE_DRAWING_MASK     1
#define RECEIVE_VIEWPOINT_MASK   2
#define RECEIVE_OVERLAY_MASK     4    /* not implemented yet */
#define RECEIVE_DRAWNOTICE_MASK  8    /* 30 Mar 1999 */
#define RECEIVE_DSETCHANGE_MASK  16   /* 31 Mar 1999 */
#define RECEIVE_TTATLAS_MASK     32   /* 12 Jul 2001 */
#define RECEIVE_REDISPLAY_MASK   64   /* 04 Mar 2002 */
#define RECEIVE_FUNCDISPLAY_MASK 128  /* 05 Mar 2002 */

#define RECEIVE_ALL_MASK       ( 1 | 2 | 4 | 8 | 16 | 32 | 64 | 128 )

/* codes for input to AFNI_receive_control */

#define DRAWING_LINES           BUTTON2_OPENPOLY
#define DRAWING_FILL            BUTTON2_CLOSEDPOLY
#define DRAWING_POINTS          BUTTON2_POINTS
#define DRAWING_NODRAW          BUTTON2_NODRAW

#ifdef MAIN
  char * DRAWING_strings[] = { "Lines" , "Filled" , "Points" , "No Draw" } ;
#else
  extern char * DRAWING_strings[] ;
#endif

#define DRAWING_OVCINDEX        11
#define DRAWING_X11PIXEL        12
#define DRAWING_STARTUP         18
#define DRAWING_SHUTDOWN        19

#define VIEWPOINT_STARTUP       28
#define VIEWPOINT_SHUTDOWN      29

#define REDISPLAY_STARTUP       78   /* 04 Mar 2002 */
#define REDISPLAY_SHUTDOWN      79

#define FUNCDISPLAY_STARTUP     88   /* 05 Mar 2002 */
#define FUNCDISPLAY_SHUTDOWN    89

#define OVERLAY_STARTUP         38
#define OVERLAY_SHUTDOWN        39

#define DRAWNOTICE_STARTUP      48   /* 30 Mar 1999 */
#define DRAWNOTICE_SHUTDOWN     49

#define DSETCHANGE_STARTUP      58   /* 31 Mar 1999 */
#define DSETCHANGE_SHUTDOWN     59

#define TTATLAS_STARTUP         68   /* 11 Jul 2001 */
#define TTATLAS_SHUTDOWN        69

#define EVERYTHING_SHUTDOWN    666

/* whys for input to the receiver routine */

#define RECEIVE_POINTS         101
#define RECEIVE_VIEWPOINT      102
#define RECEIVE_OVERLAY        103
#define RECEIVE_CLOSURE        104
#define RECEIVE_ALTERATION     105
#define RECEIVE_DRAWNOTICE     106  /* 30 Mar 1999 */
#define RECEIVE_DSETCHANGE     107  /* 31 Mar 1999 */
#define RECEIVE_TTATLAS        108  /* 12 Jul 2001 */
#define RECEIVE_REDISPLAY      109  /* 04 Mar 2002 */
#define RECEIVE_FUNCDISPLAY    110  /* 04 Mar 2002 */

/* modes for the process_drawing routine */

#define SINGLE_MODE           1000
#define PLANAR_MODE           2000
#define THREED_MODE           3000
#define SPECIAL_MODE        100000

extern void AFNI_toggle_drawing ( Three_D_View * ) ;
extern int AFNI_receive_init    ( Three_D_View *, int, gen_func * , void * ) ;
extern void AFNI_receive_destroy( Three_D_View * ) ;
extern int AFNI_receive_control ( Three_D_View *, int,int, void * ) ;

extern void AFNI_process_viewpoint  ( Three_D_View * ) ;
extern void AFNI_process_drawnotice ( Three_D_View * ) ;
extern void AFNI_process_dsetchange ( Three_D_View * ) ;
extern void AFNI_process_alteration ( Three_D_View * ) ;
extern void AFNI_process_drawing    ( Three_D_View *, int,int, int *,int *,int * );
extern void AFNI_process_ttatlas    ( Three_D_View * ) ;
extern void AFNI_process_redisplay  ( Three_D_View * ) ; /* 04 Mar 2002 */
extern void AFNI_process_funcdisplay( Three_D_View * ) ; /* 05 Mar 2002 */

extern void AFNI_do_bkgd_lab( Three_D_View * ) ;         /* 08 Mar 2002 */

extern MRI_IMAGE * AFNI_ttatlas_overlay(Three_D_View *, int,int,int,int, MRI_IMAGE *) ;

extern void AFNI_3d_linefill( int  ,int * ,int * ,int * ,
                              int *,int **,int **,int ** ) ;

/*-----------------------------------------------------------*/
/*----------------- data for Talairach To -------------------*/

#define TTO_CMAX    48
#define TTO_LMAX    (TTO_CMAX+16)
#define TTO_FORMAT  "%s [%3d,%3d,%3d]"

typedef struct {
   short xx,yy,zz,tdlev,tdval ;
   char name[TTO_CMAX] ;
} TTO_point ;

#define TTO_COUNT 241

#define TTO_COUNT_BROD    209
#define TTO_COUNT_NONBROD 125

#ifdef MAIN
   TTO_point TTO_list[TTO_COUNT] = {
      {  0, -1, -1,0,  0,"Anterior Commissure....................."} ,
      {  0, 23,  0,0,  0,"Posterior Commissure...................."} ,
      {  0,  7, 21,0,  0,"Corpus Callosum........................."} ,
      { 30, 24, -9,4, 68,"Left  Hippocampus......................."} ,
      {-30, 24, -9,4, 68,"Right Hippocampus......................."} ,
      { 23,  5,-15,4, 71,"Left  Amygdala.........................."} ,
      {-23,  5,-15,4, 71,"Right Amygdala.........................."} ,
      { 10, 54, 14,2, 20,"Left  Posterior Cingulate..............."} ,
      {-10, 54, 14,2, 20,"Right Posterior Cingulate..............."} ,
      {  8,-32,  7,2, 21,"Left  Anterior Cingulate................"} ,
      { -8,-32,  7,2, 21,"Right Anterior Cingulate................"} ,
      { 11,-11,-12,2, 22,"Left  Subcallosal Gyrus................."} ,
      {-11,-11,-12,2, 22,"Right Subcallosal Gyrus................."} ,
      { 50, 22, 12,2, 24,"Left  Transverse Temporal Gyrus........."} ,
      {-50, 22, 12,2, 24,"Right Transverse Temporal Gyrus........."} ,
      { 25,  2,-28,2, 25,"Left  Uncus............................."} ,
      {-25,  2,-28,2, 25,"Right Uncus............................."} ,
      {  7,-30,-23,2, 26,"Left  Rectal Gyrus......................"} ,
      { -7,-30,-23,2, 26,"Right Rectal Gyrus......................"} ,
      { 40, 48,-16,2, 27,"Left  Fusiform Gyrus...................."} ,
      {-40, 48,-16,2, 27,"Right Fusiform Gyrus...................."} ,
      { 35, 86, -7,2, 28,"Left  Inferior Occipital Gyrus.........."} ,
      {-35, 86, -7,2, 28,"Right Inferior Occipital Gyrus.........."} ,
      { 56, 39,-13,2, 29,"Left  Inferior Temporal Gyrus..........."} ,
      {-56, 39,-13,2, 29,"Right Inferior Temporal Gyrus..........."} ,
      { 39,  7,  9,2, 30,"Left  Insula............................"} ,
      {-39,  7,  9,2, 30,"Right Insula............................"} ,
      { 25, 25,-12,2, 31,"Left  Parahippocampal Gyrus............."} ,
      {-25, 25,-12,2, 31,"Right Parahippocampal Gyrus............."} ,
      { 14, 78, -3,2, 32,"Left  Lingual Gyrus....................."} ,
      {-14, 78, -3,2, 32,"Right Lingual Gyrus....................."} ,
      { 35, 83,  9,2, 33,"Left  Middle Occipital Gyrus............"} ,
      {-35, 83,  9,2, 33,"Right Middle Occipital Gyrus............"} ,
      { 11,-39,-25,2, 34,"Left  Orbital Gyrus....................."} ,
      {-11,-39,-25,2, 34,"Right Orbital Gyrus....................."} ,
      { 52, 39,  0,2, 35,"Left  Middle Temporal Gyrus............."} ,
      {-52, 39,  0,2, 35,"Right Middle Temporal Gyrus............."} ,
      { 51, 17,  0,2, 36,"Left  Superior Temporal Gyrus..........."} ,
      {-51, 17,  0,2, 36,"Right Superior Temporal Gyrus..........."} ,
      { 37, 82, 27,2, 37,"Left  Superior Occipital Gyrus.........."} ,
      {-37, 82, 27,2, 37,"Right Superior Occipital Gyrus.........."} ,
      { 44,-24,  2,2, 39,"Left  Inferior Frontal Gyrus............"} ,
      {-44,-24,  2,2, 39,"Right Inferior Frontal Gyrus............"} ,
      { 13, 83, 18,2, 40,"Left  Cuneus............................"} ,
      {-13, 83, 18,2, 40,"Right Cuneus............................"} ,
      { 45, 64, 33,2, 41,"Left  Angular Gyrus....................."} ,
      {-45, 64, 33,2, 41,"Right Angular Gyrus....................."} ,
      { 51, 48, 31,2, 42,"Left  Supramarginal Gyrus..............."} ,
      {-51, 48, 31,2, 42,"Right Supramarginal Gyrus..............."} ,
      { 10, 11, 34,2, 43,"Left  Cingulate Gyrus..................."} ,
      {-10, 11, 34,2, 43,"Right Cingulate Gyrus..................."} ,
      { 48, 41, 39,2, 44,"Left  Inferior Parietal Lobule.........."} ,
      {-48, 41, 39,2, 44,"Right Inferior Parietal Lobule.........."} ,
      { 14, 61, 41,2, 45,"Left  Precuneus........................."} ,
      {-14, 61, 41,2, 45,"Right Precuneus........................."} ,
      { 27, 59, 53,2, 46,"Left  Superior Parietal Lobule.........."} ,
      {-27, 59, 53,2, 46,"Right Superior Parietal Lobule.........."} ,
      { 37,-29, 26,2, 47,"Left  Middle Frontal Gyrus.............."} ,
      {-37,-29, 26,2, 47,"Right Middle Frontal Gyrus.............."} ,
      {  7, 32, 53,2, 48,"Left  Paracentral Lobule................"} ,
      { -7, 32, 53,2, 48,"Right Paracentral Lobule................"} ,
      { 43, 25, 43,2, 49,"Left  Postcentral Gyrus................."} ,
      {-43, 25, 43,2, 49,"Right Postcentral Gyrus................."} ,
      { 44,  8, 38,2, 50,"Left  Precentral Gyrus.................."} ,
      {-44,  8, 38,2, 50,"Right Precentral Gyrus.................."} ,
      { 19,-40, 27,2, 51,"Left  Superior Frontal Gyrus............"} ,
      {-19,-40, 27,2, 51,"Right Superior Frontal Gyrus............"} ,
      {  9,-24, 35,2, 52,"Left  Medial Frontal Gyrus.............."} ,
      { -9,-24, 35,2, 52,"Right Medial Frontal Gyrus.............."} ,
      { 22,  1,  2,2, 70,"Left  Lentiform Nucleus................."} ,
      {-22,  1,  2,2, 70,"Right Lentiform Nucleus................."} ,
      {  4,  3, -9,4, 72,"Left  Hypothalamus......................"} ,
      { -4,  3, -9,4, 72,"Right Hypothalamus......................"} ,
      {  5, 19, -4,4, 73,"Left  Red Nucleus......................."} ,
      { -5, 19, -4,4, 73,"Right Red Nucleus......................."} ,
      { 11, 18, -7,4, 74,"Left  Substantia Nigra.................."} ,
      {-11, 18, -7,4, 74,"Right Substantia Nigra.................."} ,
      { 32,  1,  5,2, 75,"Left  Claustrum........................."} ,
      {-32,  1,  5,2, 75,"Right Claustrum........................."} ,
      { 12, 19,  8,2, 76,"Left  Thalamus.........................."} ,
      {-12, 19,  8,2, 76,"Right Thalamus.........................."} ,
      { 11, -7,  9,2, 77,"Left  Caudate..........................."} ,
      {-11, -7,  9,2, 77,"Right Caudate..........................."} ,
      { 27, 35,  9,4,124,"Left  Caudate Tail......................"} ,
      {-27, 35,  9,4,124,"Right Caudate Tail......................"} ,
      { 12, -6, 14,4,125,"Left  Caudate Body......................"} ,
      {-12, -6, 14,4,125,"Right Caudate Body......................"} ,
      {  9,-13,  0,4,126,"Left  Caudate Head......................"} ,
      { -9,-13,  0,4,126,"Right Caudate Head......................"} ,
      { 11,  6,  9,4,128,"Left  Ventral Anterior Nucleus.........."} ,
      {-11,  6,  9,4,128,"Right Ventral Anterior Nucleus.........."} ,
      { 15, 20,  4,4,129,"Left  Ventral Posterior Medial Nucleus.."} ,
      {-15, 20,  4,4,129,"Right Ventral Posterior Medial Nucleus.."} ,
      { 18, 19,  5,4,130,"Left  Ventral Posterior Lateral Nucleus."} ,
      {-18, 19,  5,4,130,"Right Ventral Posterior Lateral Nucleus."} ,
      {  6, 16,  8,4,131,"Left  Medial Dorsal Nucleus............."} ,
      { -6, 16,  8,4,131,"Right Medial Dorsal Nucleus............."} ,
      { 12, 20, 16,4,132,"Left  Lateral Dorsal Nucleus............"} ,
      {-12, 20, 16,4,132,"Right Lateral Dorsal Nucleus............"} ,
      { 16, 27,  8,4,133,"Left  Pulvinar.........................."} ,
      {-16, 27,  8,4,133,"Right Pulvinar.........................."} ,
      { 17, 20, 14,4,134,"Left  Lateral Posterior Nucleus........."} ,
      {-17, 20, 14,4,134,"Right Lateral Posterior Nucleus........."} ,
      { 14, 12,  9,4,135,"Left  Ventral Lateral Nucleus..........."} ,
      {-14, 12,  9,4,135,"Right Ventral Lateral Nucleus..........."} ,
      {  7, 18, 16,4,136,"Left  Midline Nucleus..................."} ,
      { -7, 18, 16,4,136,"Right Midline Nucleus..................."} ,
      {  8,  8, 12,4,137,"Left  Anterior Nucleus.................."} ,   /* 04 Mar 2002 */
      { -8,  8, 12,4,137,"Right Anterior Nucleus.................."} ,
      { 11, 20,  2,4,138,"Left  Mammillary Body..................."} ,
      {-11, 20,  2,4,138,"Right Mammillary Body..................."} ,
      { 15,  4, -2,4,144,"Left  Medial Globus Pallidus............"} ,
      {-15,  4, -2,4,144,"Right Medial Globus Pallidus............"} ,
      { 20,  5,  0,4,145,"Left  Lateral Globus Pallidus..........."} ,
      {-20,  5,  0,4,145,"Right Lateral Globus Pallidus..........."} ,
      { 24,  0,  3,4,151,"Left  Putamen..........................."} ,
      {-24,  0,  3,4,151,"Right Putamen..........................."} ,
      { 12, -8, -8,4,146,"Left  Nucleus Accumbens................."} , /* 20 Aug */
      {-12, -8, -8,4,146,"Right Nucleus Accumbens................."} , /* 2001 */
      { 17, 24, -2,4,147,"Left  Medial Geniculum Body............."} ,
      {-17, 24, -2,4,147,"Right Medial Geniculum Body............."} ,
      { 22, 24, -1,4,148,"Left  Lateral Geniculum Body............"} ,
      {-22, 24, -1,4,148,"Right Lateral Geniculum Body............"} ,
      { 10, 13, -3,4,149,"Left  Subthalamic Nucleus..............."} ,
      {-10, 13, -3,4,149,"Right Subthalamic Nucleus..............."} ,
      { 53, 19, 50,4, 81,"Left  Brodmann area 1..................."} ,
      {-53, 19, 50,4, 81,"Right Brodmann area 1..................."} ,
      { 49, 26, 43,4, 82,"Left  Brodmann area 2..................."} ,
      {-49, 26, 43,4, 82,"Right Brodmann area 2..................."} ,
      { 39, 23, 50,4, 83,"Left  Brodmann area 3..................."} ,
      {-39, 23, 50,4, 83,"Right Brodmann area 3..................."} ,
      { 39, 18, 49,4, 84,"Left  Brodmann area 4..................."} ,
      {-39, 18, 49,4, 84,"Right Brodmann area 4..................."} ,
      { 16, 40, 57,4, 85,"Left  Brodmann area 5..................."} ,
      {-16, 40, 57,4, 85,"Right Brodmann area 5..................."} ,
      { 29,  0, 50,4, 86,"Left  Brodmann area 6..................."} ,
      {-29,  0, 50,4, 86,"Right Brodmann area 6..................."} ,
      { 16, 60, 48,4, 87,"Left  Brodmann area 7..................."} ,
      {-16, 60, 48,4, 87,"Right Brodmann area 7..................."} ,
      { 24,-30, 44,4, 88,"Left  Brodmann area 8..................."} ,
      {-24,-30, 44,4, 88,"Right Brodmann area 8..................."} ,
      { 32,-33, 30,4, 89,"Left  Brodmann area 9..................."} ,
      {-32,-33, 30,4, 89,"Right Brodmann area 9..................."} ,
      { 24,-56,  6,4, 90,"Left  Brodmann area 10.................."} ,
      {-24,-56,  6,4, 90,"Right Brodmann area 10.................."} ,
      { 17,-43,-18,4, 91,"Left  Brodmann area 11.................."} ,
      {-17,-43,-18,4, 91,"Right Brodmann area 11.................."} ,
      { 39,  4,  8,4, 93,"Left  Brodmann area 13.................."} ,
      {-39,  4,  8,4, 93,"Right Brodmann area 13.................."} ,
      { 10, 88,  5,4, 94,"Left  Brodmann area 17.................."} ,
      {-10, 88,  5,4, 94,"Right Brodmann area 17.................."} ,
      { 19, 85,  4,4, 95,"Left  Brodmann area 18.................."} ,
      {-19, 85,  4,4, 95,"Right Brodmann area 18.................."} ,
      { 34, 80, 18,4, 96,"Left  Brodmann area 19.................."} ,
      {-34, 80, 18,4, 96,"Right Brodmann area 19.................."} ,
      { 47, 21,-23,4, 97,"Left  Brodmann area 20.................."} ,
      {-47, 21,-23,4, 97,"Right Brodmann area 20.................."} ,
      { 58, 18,-10,4, 98,"Left  Brodmann area 21.................."} ,
      {-58, 18,-10,4, 98,"Right Brodmann area 21.................."} ,
      { 57, 23,  5,4, 99,"Left  Brodmann area 22.................."} ,
      {-57, 23,  5,4, 99,"Right Brodmann area 22.................."} ,
      {  4, 37, 24,4,100,"Left  Brodmann area 23.................."} ,
      { -4, 37, 24,4,100,"Right Brodmann area 23.................."} ,
      {  6, -6, 30,4,101,"Left  Brodmann area 24.................."} ,
      { -6, -6, 30,4,101,"Right Brodmann area 24.................."} ,
      {  6,-15,-13,4,102,"Left  Brodmann area 25.................."} ,
      { -6,-15,-13,4,102,"Right Brodmann area 25.................."} ,
      { 15, 35,  0,4,103,"Left  Brodmann area 27.................."} ,
      {-15, 35,  0,4,103,"Right Brodmann area 27.................."} ,
      { 22, -2,-24,4,104,"Left  Brodmann area 28.................."} ,
      {-22, -2,-24,4,104,"Right Brodmann area 28.................."} ,
      {  6, 48, 11,4,105,"Left  Brodmann area 29.................."} ,
      { -6, 48, 11,4,105,"Right Brodmann area 29.................."} ,
      { 13, 62, 10,4,106,"Left  Brodmann area 30.................."} ,
      {-13, 62, 10,4,106,"Right Brodmann area 30.................."} ,
      {  9, 47, 32,4,107,"Left  Brodmann area 31.................."} ,
      { -9, 47, 32,4,107,"Right Brodmann area 31.................."} ,
      {  8,-24, 30,4,108,"Left  Brodmann area 32.................."} ,
      { -8,-24, 30,4,108,"Right Brodmann area 32.................."} ,
      {  5,-12, 24,4,109,"Left  Brodmann area 33.................."} ,
      { -5,-12, 24,4,109,"Right Brodmann area 33.................."} ,
      { 18,  0,-16,4,110,"Left  Brodmann area 34.................."} ,
      {-18,  0,-16,4,110,"Right Brodmann area 34.................."} ,
      { 23, 25,-15,4,111,"Left  Brodmann area 35.................."} ,
      {-23, 25,-15,4,111,"Right Brodmann area 35.................."} ,
      { 33, 33,-15,4,112,"Left  Brodmann area 36.................."} ,
      {-33, 33,-15,4,112,"Right Brodmann area 36.................."} ,
      { 48, 55, -7,4,113,"Left  Brodmann area 37.................."} ,
      {-48, 55, -7,4,113,"Right Brodmann area 37.................."} ,
      { 41,-12,-23,4,114,"Left  Brodmann area 38.................."} ,
      {-41,-12,-23,4,114,"Right Brodmann area 38.................."} ,
      { 48, 64, 28,4,115,"Left  Brodmann area 39.................."} ,
      {-48, 64, 28,4,115,"Right Brodmann area 39.................."} ,
      { 51, 40, 38,4,116,"Left  Brodmann area 40.................."} ,
      {-51, 40, 38,4,116,"Right Brodmann area 40.................."} ,
      { 47, 26, 11,4,117,"Left  Brodmann area 41.................."} ,
      {-47, 26, 11,4,117,"Right Brodmann area 41.................."} ,
      { 63, 22, 12,4,118,"Left  Brodmann area 42.................."} ,
      {-63, 22, 12,4,118,"Right Brodmann area 42.................."} ,
      { 58, 10, 16,4,119,"Left  Brodmann area 43.................."} ,
      {-58, 10, 16,4,119,"Right Brodmann area 43.................."} ,
      { 53,-11, 12,4,120,"Left  Brodmann area 44.................."} ,
      {-53,-11, 12,4,120,"Right Brodmann area 44.................."} ,
      { 54,-23, 10,4,121,"Left  Brodmann area 45.................."} ,
      {-54,-23, 10,4,121,"Right Brodmann area 45.................."} ,
      { 50,-38, 16,4,122,"Left  Brodmann area 46.................."} ,
      {-50,-38, 16,4,122,"Right Brodmann area 46.................."} ,
      { 38,-24,-11,4,123,"Left  Brodmann area 47.................."} ,
      {-38,-24,-11,4,123,"Right Brodmann area 47.................."} ,
      {  2, 65,-32,2, 53,"Left  Uvula of Vermis..................."} ,
      { -2, 65,-32,2, 53,"Right Uvula of Vermis..................."} ,
      {  2, 73,-28,2, 54,"Left  Pyramis of Vermis................."} ,
      { -2, 73,-28,2, 54,"Right Pyramis of Vermis................."} ,
      {  2, 71,-24,2, 55,"Left  Tuber of Vermis..................."} ,
      { -2, 71,-24,2, 55,"Right Tuber of Vermis..................."} ,
      {  2, 72,-17,2, 56,"Left  Declive of Vermis................."} ,
      { -2, 72,-17,2, 56,"Right Declive of Vermis................."} ,
      {  3, 63, -3,2, 57,"Left  Culmen of Vermis.................."} ,
      { -3, 63, -3,2, 57,"Right Culmen of Vermis.................."} ,
      { 28, 51,-36,2, 58,"Left  Cerebellar Tonsil................."} ,
      {-28, 51,-36,2, 58,"Right Cerebellar Tonsil................."} ,
      { 29, 71,-38,2, 59,"Left  Inferior Semi-Lunar Lobule........"} ,
      {-29, 71,-38,2, 59,"Right Inferior Semi-Lunar Lobule........"} ,
      {  7, 54,-20,2, 60,"Left  Fastigium........................."} ,
      { -7, 54,-20,2, 60,"Right Fastigium........................."} ,
      {  7, 55,-27,2, 61,"Left  Nodule............................"} ,
      { -7, 55,-27,2, 61,"Right Nodule............................"} ,
      { 21, 76,-26,2, 62,"Left  Uvula............................."} ,
      {-21, 76,-26,2, 62,"Right Uvula............................."} ,
      { 27, 74,-30,2, 63,"Left  Pyramis..........................."} ,
      {-27, 74,-30,2, 63,"Right Pyramis..........................."} ,
      { 20, 46,-16,2, 66,"Left  Culmen............................"} ,
      {-20, 46,-16,2, 66,"Right Culmen............................"} ,
      { 26, 69,-17,2, 65,"Left  Declive..........................."} ,
      {-26, 69,-17,2, 65,"Right Declive..........................."} ,
      { 14, 54,-23,4,127,"Left  Dentate..........................."} ,
      {-14, 54,-23,4,127,"Right Dentate..........................."} ,
      { 44, 71,-27,2, 64,"Left  Tuber............................."} ,
      {-44, 71,-27,2, 64,"Right Tuber............................."} ,
      {  4, 45,-13,2, 67,"Left  Cerebellar Lingual................"} ,
      { -4, 45,-13,2, 67,"Right Cerebellar Lingual................"}
   } ;

   char * TTO_labels[TTO_COUNT] ;

   int TTO_labeled = 0 ;  /* flag that labels not yet computed */
   int TTO_current = 0 ;  /* last chosen TTO */
#else
   extern TTO_point TTO_list[TTO_COUNT] ;
   extern char * TTO_labels[TTO_COUNT] ;
   extern int TTO_labeled ;
   extern int TTO_current ;
#endif

extern void AFNI_talto_CB( Widget, XtPointer, MCW_choose_cbs * ) ;

#define CAN_TALTO(q3d)                                           \
 ( (q3d)->vinfo->view_type == VIEW_TALAIRACH_TYPE ||             \
   AFNI_can_transform_vector(                                    \
      (q3d)->anat_dset[VIEW_TALAIRACH_TYPE] , (q3d)->anat_now ) )

extern char * AFNI_ttatlas_query( Three_D_View * ) ; /* 10 Jul 2001 */
extern void AFNI_pop_whereami_kill( Three_D_View * ) ;

extern void TTRR_popup( Three_D_View * ) ;  /* 12 Jul 2001 */

typedef struct {
   int num , meth , hemi ;
   byte *ttbrik ;
   byte *ttval  ;
   byte *ttovc  ;
} TTRR_params ;

#define TTRR_METH_OFF 0
#define TTRR_METH_GAF 1
#define TTRR_METH_AGF 2
#define TTRR_METH_FGA 3
#define TTRR_METH_FAG 4

#define TTRR_HEMI_LEFT  0
#define TTRR_HEMI_RIGHT 1
#define TTRR_HEMI_BOTH  2

extern TTRR_params * TTRR_get_params(void) ;

/*-------------------------------------*/
/*--- driving AFNI programmatically ---*/

extern int AFNI_driver( char *cmd ) ;                    /* 07 Nov 2001 */
extern int AFNI_controller_code_to_index( char *code ) ;

/*-------------------------------------------------------*/
/*--------------  registration of functions -------------*/

/* sample 0D transform functions */

extern void log10_func( int, float * ) ;
extern void ssqrt_func( int, float * ) ;

/* sample 1D transform functions */

extern void osfilt3_func( int, double,double, float * ) ;
extern void median3_func( int, double,double, float * ) ;
extern void absfft_func ( int, double,double, float * ) ;

/* 31 Jan 2002: sample slice_proj transform functions */

extern float max_proj ( int, float * ) ;
extern float min_proj ( int, float * ) ;
extern float mean_proj( int, float * ) ;

extern float extreme_proj( int, float * ) ;  /* 02 Feb 2002 */

/* sample 2D transform functions */

extern void median9_box_func ( int, int, double,double, float * ) ;
extern void winsor9_box_func ( int, int, double,double, float * ) ;
extern void osfilt9_box_func ( int, int, double,double, float * ) ;
extern void fft2D_func       ( int, int, double,double, float * ) ;
extern void median21_box_func( int, int, double,double, float * ) ;
extern void winsor21_box_func( int, int, double,double, float * ) ;

extern void AFNI_register_nD_function( int, char *, generic_func *, int ) ;

#define AFNI_register_0D_function(cc,ff) AFNI_register_nD_function(0,(cc),(ff),0)
#define AFNI_register_1D_function(cc,ff) AFNI_register_nD_function(1,(cc),(ff),0)
#define AFNI_register_2D_function(cc,ff) AFNI_register_nD_function(2,(cc),(ff),0)

#define AFNI_register_slice_proj(cc,ff)  \
   AFNI_register_nD_function(-1,(cc),(generic_func *)(ff),0)   /* 31 Jan 2002 */

#define AFNI_register_1D_funcstr(cc,ff)  \
   AFNI_register_nD_function(1,(cc),(ff),RETURNS_STRING)

extern void AFNI_store_dset_index(int,int) ;  /* 18 May 2000 */
extern int  AFNI_needs_dset_ijk(void) ;
extern int  AFNI_needs_dset_tin(void) ;

/*-----------------------------------------------------------*/
/*-----------------  initializations  -----------------------*/

/*** June 1995: modified to allow input via XGetDefault ***/

#define DEFAULT_NGRAY   80
#define DEFAULT_GAMMA   1.0

#define DEFAULT_NCOLOVR 40
#define MAX_NCOLOVR     199

/** color definitions and their labels (for 'choosers') **/

#define DEFAULT_CROSSHAIR_COLOR  13   /* note indices start at 1! */
#define DEFAULT_PRIMARY_COLOR    17   /* (0 index is "no color")  */
#define DEFAULT_SECONDARY_COLOR  14

#define DEFAULT_MARK_SIZE     8
#define DEFAULT_MARK_GAP      3
#define DEFAULT_CROSSHAIR_GAP 5

/** initialization values (static values are used in afni.c only) **/

void AFNI_load_defaults( Widget w ) ;

#ifdef MAIN

/** default colors **/

static char * INIT_def_colovr[DEFAULT_NCOLOVR] = {
   "#ffff00" , "#ffcc00"   , "#ff9900"  , "#ff6900" , "#ff4400" , "#ff0000" ,
   "#0000ff" , "#0044ff"   , "#0069ff"  , "#0099ff" , "#00ccff" , "#00ffff" ,
   "green"   , "limegreen" , "violet"   , "hotpink" ,
   "white"   , "#dddddd"   , "#bbbbbb"  , "black"   ,

   "#cc1033" , "#992066"   , "#663199"  , "#3341cc" ,  /* RGB cycle */
   "#0051ff" , "#0074cc"   , "#009799"  , "#00b966" ,  /* 10 Jun 2002 */
   "#00dc33" , "#00ff00"   , "#33ff00"  , "#66ff00" ,
   "#99ff00" , "#ccff00"   , "#ffff00"  , "#ffcc00" ,
   "#ff9900" , "#ff6600"   , "#ff3300"  , "#ff0000"
} ;

#define RGBCYC_COUNT  20  /* 10 Jun 2002: number in RGB cycle */
#define RGBCYC_FIRST  20  /*              index of first one */

static char * INIT_def_labovr[DEFAULT_NCOLOVR] = {
   "yellow" , "yell-oran" , "oran-yell" , "orange"   , "oran-red" , "red"   ,
   "dk-blue", "blue"      , "lt-blue1"  , "lt-blue2" , "blue-cyan", "cyan"  ,
   "green"  , "limegreen" , "violet"    , "hotpink"  ,
   "white"  , "gry-dd"    , "gry-bb"    , "black"    ,

   "rbgyr20_01" , "rbgyr20_02" , "rbgyr20_03" , "rbgyr20_04" , /* RBG cycle */
   "rbgyr20_05" , "rbgyr20_06" , "rbgyr20_07" , "rbgyr20_08" , /* 10 Jun 2002 */
   "rbgyr20_09" , "rbgyr20_10" , "rbgyr20_11" , "rbgyr20_12" ,
   "rbgyr20_13" , "rbgyr20_14" , "rbgyr20_15" , "rbgyr20_16" ,
   "rbgyr20_17" , "rbgyr20_18" , "rbgyr20_19" , "rbgyr20_20"
} ;

/** actual colors (from defaults above, or from X11 resources) **/

char * INIT_colovr[MAX_NCOLOVR] ;
char * INIT_labovr[MAX_NCOLOVR] ;

/** misc constants **/

int INIT_ngray           = DEFAULT_NGRAY ,
    INIT_ncolovr         = DEFAULT_NCOLOVR ,
    INIT_crosshair_color = DEFAULT_CROSSHAIR_COLOR ,
    INIT_marks1_color    = DEFAULT_PRIMARY_COLOR ,
    INIT_marks2_color    = DEFAULT_SECONDARY_COLOR ,
    INIT_marks_size      = DEFAULT_MARK_SIZE ,
    INIT_marks_gap       = DEFAULT_MARK_GAP ,
    INIT_crosshair_gap   = DEFAULT_CROSSHAIR_GAP ,
    INIT_purge           = 0 ,
    INIT_posfunc         = 0 ,
    INIT_bigscroll       = 5 ,
    INIT_resam_anat      = RESAM_LINEAR_TYPE ,
    INIT_resam_func      = RESAM_NN_TYPE ,
    INIT_resam_thr       = RESAM_NN_TYPE   ;

float INIT_gamma         = DEFAULT_GAMMA ,
      INIT_resam_vox     = DEFAULT_RESAMPLE_VOX ;

int INIT_ignore           = 0 ;
int INIT_tlrc_big         = 1 ;
int INIT_montage_periodic = 1 ;
int INIT_fim_polort       = 1 ; /* 30 May 1999 */

#else

extern int INIT_ngray           ,
           INIT_ncolovr         ,
           INIT_crosshair_color ,
           INIT_marks1_color    ,
           INIT_marks2_color    ,
           INIT_marks_size      ,
           INIT_marks_gap       ,
           INIT_crosshair_gap   ,
           INIT_purge           ,
           INIT_posfunc         ,
           INIT_bigscroll       ,
           INIT_resam_anat      ,
           INIT_resam_func      ,
           INIT_resam_thr        ;

extern float INIT_gamma         ,
             INIT_resam_vox      ;

extern int INIT_ignore ;
extern int INIT_tlrc_big ;
extern int INIT_montage_periodic ;

extern int INIT_fim_polort ;

extern char * INIT_colovr[] ;
extern char * INIT_labovr[] ;

#endif /* MAIN */

/**********************************************/
/***** Setup constants for the color pbar *****/

extern void AFNI_setup_inten_pbar( Three_D_View * ) ;

#define DEFAULT_PANES_POS  8
#define DEFAULT_PANES_SGN  9

#ifdef MAIN
int INIT_panes_pos  = DEFAULT_PANES_POS ,
    INIT_panes_sgn  = DEFAULT_PANES_SGN ,
    INIT_panes_hide = 0 ;

#define NPANE_INIT 10

float INIT_pval_pos[NPANE_MAX+1][NPANE_MAX+1] = {
  { 0 },                                                                        /* 0 panes */
  { 1.00, 0.00 },                                                               /* 1 */
  { 1.00, 0.50,  0.00 },                                                        /* 2 */
  { 1.00, 0.67,  0.33,  0.00 },                                                 /* 3 */
  { 1.00, 0.75,  0.50,  0.25,  0.00 },                                          /* 4 */
  { 1.00, 0.80,  0.60,  0.40,  0.20,  0.00 },                                   /* 5 */
  { 1.00, 0.84,  0.67,  0.50,  0.33,  0.16,  0.00 },                            /* 6 */
  { 1.00, 0.90,  0.75,  0.60,  0.45,  0.30,  0.15,  0.00 },                     /* 7 */
  { 1.00, 0.80,  0.70,  0.60,  0.50,  0.40,  0.30,  0.15,  0.00 },              /* 8 */
  { 1.00, 0.90,  0.80,  0.70,  0.60,  0.50,  0.25,  0.15,  0.05,  0.00 },       /* 9 */
  { 1.00, 0.90,  0.80,  0.70,  0.60,  0.50,  0.40,  0.30,  0.20,  0.10,  0.00 } /*10 */
} ;

int INIT_ovin_pos[NPANE_MAX+1][NPANE_MAX+1] = {
  { 0 } ,                                    /* 0 panes */
  { 1 } ,                                    /* 1 */
  { 1 , 0 } ,                                /* 2 */
  { 1 , 6 , 0 } ,                            /* 3 */
  { 1 , 4 , 6 , 0 } ,                        /* 4 */
  { 1 , 3 , 5 , 6 , 0 } ,                    /* 5 */
  { 1 , 2 , 3 , 5 , 6 , 0 } ,                /* 6 */
  { 1 , 2 , 3 , 4 , 5 , 6 , 0 } ,            /* 7 */
  { 1 , 2 , 3 , 4 , 5 , 6 ,16 , 0 } ,        /* 8 */
  { 1 , 2 , 3 , 4 , 5 , 6 ,16 ,15 , 0 } ,    /* 9 */
  { 1 , 2 , 3 , 5 , 5 , 6 ,16 ,15 , 7 , 0 }  /*10 */
} ;

float INIT_pval_sgn[NPANE_MAX+1][NPANE_MAX+1] = {
  { 0 },                                                                        /* 0 panes */
  { 1.00,-1.00 },                                                               /* 1 */
  { 1.00, 0.00, -1.00 },                                                        /* 2 */
  { 1.00, 0.05, -0.05, -1.00 },                                                 /* 3 */
  { 1.00, 0.50,  0.00, -0.50, -1.00 },                                          /* 4 */
  { 1.00, 0.50,  0.05, -0.05, -0.50, -1.00 },                                   /* 5 */
  { 1.00, 0.66,  0.33,  0.00, -0.33, -0.66, -1.00 },                            /* 6 */
  { 1.00, 0.66,  0.33,  0.05, -0.05, -0.33, -0.66, -1.00 },                     /* 7 */
  { 1.00, 0.75,  0.50,  0.25,  0.00, -0.25, -0.50, -0.75, -1.00 },              /* 8 */
  { 1.00, 0.75,  0.50,  0.25,  0.05, -0.05, -0.25, -0.50, -0.75, -1.00 },       /* 9 */
  { 1.00, 0.80,  0.60,  0.40,  0.20,  0.00, -0.20, -0.40, -0.60, -0.80, -1.00 } /*10 */
} ;

int INIT_ovin_sgn[NPANE_MAX+1][NPANE_MAX+1] = {
  { 0 } ,
  { 1 } ,
  { 1 , 11 } ,
  { 1 , 0 , 11 } ,
  { 1 , 4 ,  8 , 11 } ,
  { 1 , 4 ,  0 ,  8 , 11 } ,
  { 1 , 3 ,  5 ,  7 ,  9 , 11 } ,
  { 1 , 3 ,  5 ,  0 ,  7 ,  9 , 11 } ,
  { 1 , 2 ,  4 ,  5 ,  8 ,  9 , 10 , 11 } ,
  { 1 , 2 ,  4 ,  5 ,  0 ,  8 ,  9 , 10 , 11 } ,
  { 1 , 2 ,  3 ,  4 ,  5 ,  7 ,  8 ,  9 , 10 , 11 }
} ;
#else
extern int INIT_panes_pos , INIT_panes_sgn , INIT_panes_hide ;

extern float INIT_pval_pos[NPANE_MAX+1][NPANE_MAX+1] ;
extern int   INIT_ovin_pos[NPANE_MAX+1][NPANE_MAX+1] ;

extern float INIT_pval_sgn[NPANE_MAX+1][NPANE_MAX+1] ;
extern int   INIT_ovin_sgn[NPANE_MAX+1][NPANE_MAX+1] ;
#endif

#endif /* _AFNI_HEADER_ */
