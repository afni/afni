#ifndef _AFNI_HEADER_
#define _AFNI_HEADER_

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/


#include "imseq.h"
#include "xutil.h"
#include "pbar.h"
#include "afni_graph.h"
#include "afni_pcor.h"
#include "mrilib.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <Xm/Separator.h>
#include <Xm/Display.h>
#include <Xm/CascadeB.h>

#include "mcw.h"

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

#define VERSION "2.20b"

/** this should always be exactly 17 characters! **/
/*              "12345678901234567" **/

#define RELEASE "14 July 1998     "

#ifdef MAIN
#define AFNI_about \
     "************************************************\n"  \
     "* MCW AFNI: Analysis of Functional NeuroImages *\n"  \
     "*           Version " VERSION " -- " RELEASE " *\n"  \
     "*                                              *\n"  \
     "* Copyright:     Medical College of Wisconsin  *\n"  \
     "*   1994-8       Milwaukee, WI 53226-0509      *\n"  \
     "*                                              *\n"  \
     "* Author:  Robert W. Cox, Ph.D.                *\n"  \
     "* E-mail:  rwcox@mcw.edu                       *\n"  \
     "************************************************"

char AFNI_tophelp[TOPSIZE] = AFNI_about ;
char AFNI_abohelp[1024]    = AFNI_about ;
#else
extern char AFNI_tophelp[TOPSIZE] ;
extern char AFNI_abohelp[1024] ;
#endif

#ifdef USE_SONNETS

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

typedef struct {
      int   i1 , j2 , k3 ;  /* integer coordinates of current point */
      float xi , yj , zk ;  /* float (mm) coordinates (take priority) */

      int   i1_old , j2_old , k3_old ;  /* for jumpback */

      Boolean   xhairs_show_montage , xhairs_periodic , xhairs_all ;
      THD_ivec3 xhairs_ndown , xhairs_nup , xhairs_nskip ; /* montage crosshairs */
      int       time_index , top_index ;

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

      gen_func * receiver ;
      void *     receiver_data ;
      int        receiver_mask ;
      int        drawing_enabled , drawing_mode ;
      Pixel      drawing_pixel ;
} AFNI_view_info ;

#define SAVE_VPT(iqq)                           \
   ( (iqq)->vinfo->i1_old = (iqq)->vinfo->i1 ,  \
     (iqq)->vinfo->j2_old = (iqq)->vinfo->j2 ,  \
     (iqq)->vinfo->k3_old = (iqq)->vinfo->k3  )

#define WARPED_VIEW(vvv)  ((vvv)+1)
#define ISVALID_VIEW(vvv) ((vvv) >= FIRST_VIEW_TYPE && (vvv) <= LAST_VIEW_TYPE)

/*-----------------------------------------------------------*/

#define MAXOVSIZE 19
#define MAXOVPIX  (12*(MAXOVSIZE+2)+1)
typedef struct {
      int numpix ;
      int dx[MAXOVPIX] , dy[MAXOVPIX] ;
} AFNI_ovtemplate ;

/*-------------------------------------------------------------------*/
/*--------------- define display control widgets --------------------*/

#define ALLOW_BKGD_LAB

#ifdef ALLOW_BKGD_LAB
#define BKGD_COUNT 3
#define INIT_BKGD_LAB(iq) \
   do{ int qq = ((iq)->s123!=NULL) + ((iq)->s231!=NULL) + ((iq)->s312!=NULL); \
       if( qq >= BKGD_COUNT ){                              \
          (iq)->vwid->imag->do_bkgd_lab = True ;            \
       } else {                                             \
          (iq)->vwid->imag->do_bkgd_lab = False ;           \
          XtUnmanageChild(im3d->vwid->imag->pop_bkgd_lab) ; \
          XtUnmanageChild(im3d->vwid->func->bkgd_lab) ;     \
          FIX_SCALE_SIZE(im3d) ;                            \
       } break ; } while(0)
#else
#define INIT_BKGD_LAB(iq)
#endif

#define AFNI_XHAIRS_OFF    0
#define AFNI_XHAIRS_SINGLE 1
#define AFNI_XHAIRS_MULTI  2

typedef struct {
      Widget frame , rowcol ;
      Widget topper , popmenu , pop_bkgd_lab ,
             pop_jumpback_pb , pop_imageonly_pb , pop_jumpto_pb , pop_talto_pb ;

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
             name_xyz_lab , name_yzx_lab , name_zxy_lab ,
             image_xyz_pb , image_yzx_pb , image_zxy_pb ,
             graph_xyz_pb , graph_yzx_pb , graph_zxy_pb  ;

      Boolean do_bkgd_lab ;

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
      Widget         read_rowcol , read_sess_pb , read_1D_pb ;

      Widget         mbar_rowcol ;
      MCW_bbox     * lock_bbox ;
      Widget         lock_enforce_pb , lock_clear_pb ;

      Widget         misc_voxind_pb ;
      Widget         misc_hints_pb ;
      Widget         misc_anat_info_pb , misc_func_info_pb ;
      Widget         misc_newstuff_pb , misc_purge_pb , misc_tracing_pb ;
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

      /*--- Pointers to other data ---*/

      XtPointer parent ;
} AFNI_widget_set ;

/** picture controls **/

#undef OLD_PICTURE
#ifdef OLD_PICTURE
#  define PICTURE_ON(im)  XtSetSensitive( (im)->vwid->picture , False )
#  define PICTURE_OFF(im) XtSetSensitive( (im)->vwid->picture , True )
#else
#  define PICTURE_ON(im)  \
       XtVaSetValues( (im)->vwid->picture, XmNlabelPixmap,mcw_pixmap, NULL )
#  define PICTURE_OFF(im) \
       XtVaSetValues( (im)->vwid->picture, XmNlabelPixmap,XmUNSPECIFIED_PIXMAP, NULL )
#endif

/*-----------------------------*/
/*----- Data for FIM-age ------*/

#define FIM_ALPHA_MASK  1
#define FIM_BEST_MASK   2
#define FIM_PERC_MASK   4
#define FIM_BASE_MASK   8
#define FIM_CORR_MASK  16

#define FIM_DOALL_MASK   (1 | 2 | 4 | 8 | 16)
#define FIM_DEFAULT_MASK (1 | 2 | 4 |     16)

#define FIM_NUM_OPTS    5

#ifdef MAIN
   char * fim_opt_labels[FIM_NUM_OPTS] = {
     "Fit Coef" , "Best Index" , "% Change" , "Baseline" , "Correlation"
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
} AFNI_fimmer_type ;

#define CLEAR_FIMDATA(iq)                  \
   ( (iq)->fimdata->fimref       = NULL ,  \
     (iq)->fimdata->fimort       = NULL ,  \
     (iq)->fimdata->fimdset      = NULL ,  \
     (iq)->fimdata->refadd_count = 0 ,     \
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

#define OPEN_CONTROLLER(iq) \
 ( XtRealizeWidget((iq)->vwid->top_shell) , (iq)->opened = 1 )

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

#ifdef ALLOW_PLUGINS
#  include "afni_plugin.h"
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

   THD_coorder cord ;

#ifdef ALLOW_PLUGINS
   struct AFNI_plugin_array * plugins ;           /* plugins */
#endif

   PBAR_palette_table * gpt ;

} AFNI_library_type ;

#ifdef MAIN
   AFNI_library_type GLOBAL_library ;
#else
   extern AFNI_library_type GLOBAL_library ;
#endif

#define DISABLE_LOCK    (GLOBAL_library.ignore_lock=1)
#define ENABLE_LOCK     (GLOBAL_library.ignore_lock=0)
#define BEEPIT          XBell(GLOBAL_library.dc->display,100)
#define ALLOW_real_time GLOBAL_argopt.allow_rt
#define ELIDE_quality   GLOBAL_argopt.elide_quality
#define GPT             GLOBAL_library.gpt
#define NO_frivolities  GLOBAL_argopt.no_frivolities

#define DOING_REALTIME_WORK (GLOBAL_library.interruptables.windows != NULL)

#define UNDUMMYIZE                                                              \
 do { GLOBAL_library.have_dummy_dataset = 0 ;                                   \
      XtSetSensitive(GLOBAL_library.controllers[0]->vwid->prog->clone_pb,True); \
    } while(0)

/*-----------------------------------------------------------*/
/*------------------------ prototypes -----------------------*/

extern void AFNI_parse_args( int argc , char * argv[] );
extern void FatalError(char * str);

extern void AFNI_quit_CB           ( Widget wcall , XtPointer cd , XtPointer cbs );
extern void AFNI_quit_timeout_CB   ( XtPointer , XtIntervalId * ) ;
extern void AFNI_startup_timeout_CB( XtPointer , XtIntervalId * ) ;

extern void AFNI_clone_controller_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_controller_panel_CB( Widget , XtPointer , XtPointer ) ;

/* "locks" 04 Nov 1996 */
extern void AFNI_lock_enforce_CB    ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_lock_change_CB     ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_lock_clear_CB      ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_lock_carryout      ( Three_D_View * ) ;

extern XtPointer AFNI_brick_to_mri( int n , int type , FD_brick * br );

extern THD_3dim_dataset * AFNI_read_images( int nf , char * fname[] );

extern void AFNI_seq_send_CB(MCW_imseq   * seq    ,FD_brick * br,ISQ_cbs * cbs);
extern void AFNI_gra_send_CB(MCW_grapher * grapher,FD_brick * br,GRA_cbs * cbs);

extern void AFNI_read_inputs   ( int argc, char * argv[] );
extern void AFNI_make_widgets  ( Three_D_View * im3d );
extern void AFNI_closedown_3dview( Three_D_View * im3d );
extern MRI_IMAGE * AFNI_overlay( int n , FD_brick * br );

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

extern void AFNI_fimmer_setref( Three_D_View * , MRI_IMAGE * ) ;
extern void AFNI_fimmer_setort( Three_D_View * , MRI_IMAGE * ) ;
extern void AFNI_fimmer_setignore( Three_D_View * , int ) ;
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

extern void AFNI_fimmer_execute( Three_D_View * , int ) ;

extern void AFNI_process_interrupts( Widget ) ;
extern void AFNI_add_interruptable( Widget ) ;

extern int AFNI_ts_in_library( MRI_IMAGE * tsim ) ;

extern THD_3dim_dataset * AFNI_fimmer_compute( Three_D_View * ,
                                               THD_3dim_dataset * , MRI_IMAGE *,
                                               MRI_IMAGE *, THD_session *, int ) ;

extern void AFNI_fimmer_redisplay( int , Three_D_View * , THD_3dim_dataset * ) ;

#ifdef USE_TALAIRACH_TO
extern void AFNI_talto_CB               ( Widget, XtPointer, MCW_choose_cbs * ) ;
#endif

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

extern XmString AFNI_crosshair_label( Three_D_View * ) ;
extern XmString AFNI_range_label( Three_D_View * ) ;
extern XmString AFNI_autorange_label( Three_D_View * ) ;

extern void AFNI_range_bbox_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_range_av_CB  ( MCW_arrowval * , XtPointer ) ;
extern void AFNI_inten_bbox_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_wrap_bbox_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_xhall_bbox_CB( Widget , XtPointer , XtPointer ) ;

extern void AFNI_reset_func_range( Three_D_View * ) ;

extern int AFNI_first_tog( int , Widget * ) ;
extern int AFNI_all_tog  ( int , Widget * ) ;
extern void AFNI_set_tog ( int , int , Widget * ) ;

extern void AFNI_make_ptmask( int , int , AFNI_ovtemplate * ) ;

extern void AFNI_initialize_view( THD_3dim_dataset * , Three_D_View * ) ;

extern void AFNI_setup_viewing(  Three_D_View * , Boolean ) ;
extern void AFNI_modify_viewing( Three_D_View * , Boolean ) ;

THD_fvec3 AFNI_transform_vector( THD_3dim_dataset * ,
                                 THD_fvec3 , THD_3dim_dataset * ) ;
THD_fvec3 AFNI_backward_warp_vector( THD_warp * , THD_fvec3 ) ;
THD_fvec3 AFNI_forward_warp_vector ( THD_warp * , THD_fvec3 ) ;

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

/*-------------------------------------------------------------------
  Include prototypes for actual data warping and slicing here.
--------------------------------------------------------------------*/

#ifdef THD_FREEUP
#undef THD_FREEUP
#endif
#define THD_FREEUP AFNI_purge_unused_dsets

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

#define RECEIVE_DRAWING_MASK    1
#define RECEIVE_VIEWPOINT_MASK  2
#define RECEIVE_OVERLAY_MASK    4
#define RECEIVE_ALL_MASK       ( 1 | 2 | 4 )

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

#define OVERLAY_STARTUP         38
#define OVERLAY_SHUTDOWN        39

#define EVERYTHING_SHUTDOWN    666

/* whys for input to the receiver routine */

#define RECEIVE_POINTS         101
#define RECEIVE_VIEWPOINT      102
#define RECEIVE_OVERLAY        103
#define RECEIVE_CLOSURE        104
#define RECEIVE_ALTERATION     105

/* modes for the process_drawing routine */

#define SINGLE_MODE           1000
#define PLANAR_MODE           2000
#define THREED_MODE           3000
#define SPECIAL_MODE        100000

extern void AFNI_toggle_drawing ( Three_D_View *, int ) ;
extern void AFNI_process_drawing( Three_D_View *, int,int, int *,int *,int * ) ;
extern int AFNI_receive_init    ( Three_D_View *, int, gen_func * , void * ) ;
extern int AFNI_receive_control ( Three_D_View *, int, void * ) ;

extern void AFNI_3d_linefill( int  ,int * ,int * ,int * ,
                              int *,int **,int **,int ** ) ;

/*-----------------------------------------------------------*/
/*----------------- data for Talairach To -------------------*/

#undef USE_TALAIRACH_TO
#ifdef USE_TALAIRACH_TO

#define TTO_CMAX    32
#define TTO_LMAX    (TTO_CMAX+32)
#define TTO_FORMAT  "%s [%.0f,%.0f,%.0f]"

typedef struct {
   float xx,yy,zz ;
   char name[TTO_CMAX] ;
} TTO_point ;

#define TTO_COUNT 2

#ifdef MAIN
   TTO_point TTO_list[TTO_COUNT] = {
      {  0, -1, -1, "Anterior Commissure" } ,
      {  0, 23,  0, "Posterior Commissure" }
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

#endif /* USE_TALAIRACH_TO */

/*-------------------------------------------------------*/
/*--------------  registration of functions -------------*/

extern void log10_func  ( int , float * ) ;  /* a sample 0D function */
extern void ssqrt_func  ( int , float * ) ;  /* another */

extern void osfilt3_func( int , double,double , float * ) ;  /* a sample 1D function */
extern void median3_func( int , double,double , float * ) ;  /* another */

extern void AFNI_register_nD_function( int , char * , generic_func * , int ) ;

#define AFNI_register_0D_function(cc,ff) AFNI_register_nD_function(0,(cc),(ff),0)
#define AFNI_register_1D_function(cc,ff) AFNI_register_nD_function(1,(cc),(ff),0)
#define AFNI_register_2D_function(cc,ff) AFNI_register_nD_function(2,(cc),(ff),0)

#define AFNI_register_1D_funcstr(cc,ff)  \
   AFNI_register_nD_function(1,(cc),(ff),RETURNS_STRING)

/*-----------------------------------------------------------*/
/*-----------------  initializations  -----------------------*/

/*** June 1995: modified to allow input via XGetDefault ***/

#define DEFAULT_NGRAY   80
#define DEFAULT_GAMMA   1.0

#define DEFAULT_NCOLOVR 20
#define MAX_NCOLOVR     99

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
   "white"   , "#dddddd"   , "#bbbbbb"  , "black"
} ;

static char * INIT_def_labovr[DEFAULT_NCOLOVR] = {
   "yellow" , "yell-oran" , "oran-yell" , "orange"   , "oran-red" , "red"   ,
   "dk-blue", "blue"      , "lt-blue1"  , "lt-blue2" , "blue-cyan", "cyan"  ,
   "green"  , "limegreen" , "violet"    , "hotpink"  ,
   "white"  , "gry-dd"    , "gry-bb"    , "black"
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
