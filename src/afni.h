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
#include <time.h>

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
              resize_images , keep_logo , pos_func ,
              destruct , tlrc_big , warp_4D , unique_dcs ;
      int ncolor , datum , ignore , allow_rt , skip_afnirc ;
      int xtwarns ;
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
      int disable_done ;      /* 21 Aug 2008 [rickr] */

      int yes_niml ;          /* 28 Feb 2002 */
      int port_niml ;         /* 10 Dec 2002 */

      char * script_fname ;   /* 21 Jan 2003 */
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
#define UNDERLAY_THRFUNC   2   /* 29 Jul 2003: eliminated in all code! */

#define UNDERLAY_ANAT_BVAL     (1 << UNDERLAY_ANAT   )
#define UNDERLAY_ALLFUNC_BVAL  (1 << UNDERLAY_ALLFUNC)
#define UNDERLAY_THRFUNC_BVAL  (1 << UNDERLAY_THRFUNC)

#define LAST_UNDERLAY_TYPE 1  /* 29 Jul 2003: changed from 2 */
#define ISFUNC_UNDERLAY(uu) \
   ((uu)==UNDERLAY_ALLFUNC||(uu)==UNDERLAY_THRFUNC)

static char * UNDERLAY_typestr[] =
   { "bkgd:ULay" , "bkgd:OLay" , "ulay:O@Thr" } ;

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

#include "AFNI_label.h"
#define AVERZHN AFNI_VERSION_LABEL    /* 21 chars long */

#ifdef  __cplusplus
extern "C" {
#endif

#ifdef MAIN
#define AFNI_about \
     "************************************************\n"  \
     "* GPL AFNI: Analysis of Functional NeuroImages *\n"  \
     "*           Version " AVERZHN          "       *\n"  \
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

/*-- callbacks to receive various kinds of transactions --*/

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
#define RECEIVE_TIMEINDEX      111  /* 29 Jan 2003 */

#define RECEIVE_BASEVAL        101
#define RECEIVE_LASTVAL        111
#define RECEIVE_NUMVAL         (RECEIVE_LASTVAL-RECEIVE_BASEVAL+1)

typedef struct {                             /* 29 Mar 1999 */
      gen_func * receiver_func ;
      void *     receiver_data ;
      int        receiver_mask ;
      char *     receiver_funcname ;         /* 20 Feb 2003 */
      int        last_when[RECEIVE_NUMVAL] ; /* 08 Sep 2009 */
} AFNI_receiver ;

typedef struct {
      int   i1 , j2 , k3 ;  /* integer coordinates of current point */
      float xi , yj , zk ;  /* float (mm) coordinates (take priority) */

      int   i1_old , j2_old , k3_old ;  /* for jumpback */

      Boolean   xhairs_show_montage , xhairs_periodic , xhairs_all ;
      THD_ivec3 xhairs_ndown , xhairs_nup , xhairs_nskip ; /* montage crosshairs */
      int       time_index , top_index , time_on ;

      int xhairs_orimask ;    /* 31 Dec 1998 */

      int       anat_index , fim_index , thr_index ; /* 30 Nov 1997 */

      Boolean crosshair_visible , inverted_pause ;
      int     crosshair_gap , crosshair_ovcolor , crosshair_gap_old ;

      int view_type     ,  /* one of the VIEW_ constants in 3ddata.h */
          underlay_type ;  /* one of the UNDERLAY_ constants above */

      int sess_num , anat_num , func_num ;  /* codes pointing to datasets */

      XmString old_crosshair_label ;

      Boolean    func_visible , force_anat_wod , force_func_wod ,
                 pts_visible , show_voxind ;
      int        func_visible_count ;
      float      func_threshold , resam_vox ;
      float      func_thresh_top ;              /* 23 Jul 1997 */
      int        func_resam_mode , anat_resam_mode , pts_color ;
      int        thr_resam_mode ;               /* 09 Dec 1997 */
      int        thr_onoff ;                    /* 28 Jun 2007 */
      int        thr_sign ;                     /* 08 Aug 2007 */

      /* 3/24/95: range data for conversion of pbar
                  values to thresholding values in the data */

      Boolean  use_autorange , use_posfunc ;
      float    fim_autorange , fim_range ;
      XmString old_range_label , autorange_label ;

      char     anat_val[32] , func_val[32] , thr_val[32] ;

      /** Feb 1998: stuff for the "receive" modules **/
      /** Mar 1999: modified to allow for more than one receiver **/

      AFNI_receiver **receiver ;
      int             num_receiver ;
      int             drawing_enabled , drawing_mode ;
      Pixel           drawing_pixel ;

      int writeownsize ; /* 01 Aug 1999 */

      int tempflag ;     /* 15 Mar 2000: for quick communication of state */

      int see_ttatlas ;  /* 25 Jul 2001 */

      int view_setter ;  /* 20 Feb 2003 */

      float func_pval ;  /* 06 Feb 2004 */
      float func_qval ;  /* 23 Jan 2008 */

      int stats_anat_ok,     /* 29 Mar 2005: set in AFNI_range_label() */
          stats_func_ok,     /*   to indicate if the sub-brick range  */
          stats_thresh_ok ;  /*   statistics are loaded properly     */

      int   i1_icor , j2_icor , k3_icor;  /* for InstaCorr -- 08 May 2009 */
      float xi_icor , yj_icor , zk_icor ; /* DICOM coords -- 17 Mar 2010 */

} AFNI_view_info ;

#define AXIAL    1       /* 20 Feb 2003: view_setter codes */
#define SAGITTAL 2
#define CORONAL  3

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
       if( qq >= BKGD_COUNT || (qq > 0 && !AFNI_noenv("AFNI_VALUE_LABEL")) ){ \
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

      Widget crosshair_menu, crosshair_dicom_pb, crosshair_spm_pb ; /* 12 Mar 2004 */

      Widget        xhair_rowcol ;
      MCW_arrowval *crosshair_av ;
      MCW_bbox     *xhall_bbox ;

      MCW_arrowval *crosshair_color_av ;
      MCW_arrowval *time_index_av ;

      Widget        gap_wrap_rowcol ;
      MCW_arrowval *crosshair_gap_av ;
      MCW_bbox     *wrap_bbox ;

      Widget view_frame , view_rowcol ,
             xyz_rowcol   , yzx_rowcol   , zxy_rowcol   ,
             name_xyz_lab , name_yzx_lab , name_zxy_lab ,   /* xyz = Axial    */
             image_xyz_pb , image_yzx_pb , image_zxy_pb ,   /* yzx = Sagittal */
             graph_xyz_pb , graph_yzx_pb , graph_zxy_pb  ;  /* zxy = Coronal  */

      Boolean do_bkgd_lab ;

      Widget pop_whereami_pb , pop_ttren_pb ;
      MCW_textwin *pop_whereami_twin ;

      Widget pop_sumato_pb ;
      Widget pop_mnito_pb ;  /* 01 May 2002 */

      Widget pop_environment_pb ; /* 05 Nov 2003 */
      Widget pop_drawdataset_pb ; /* 17 May 2005 */

      Widget pop_instacorr_pb ;   /* 06 May 2009 */
      Widget pop_icorrjump_pb ;
} AFNI_imaging_widgets ;

/*--- 19 Aug 2002: Switch Surface control box ---*/

typedef struct {
      Widget wtop , rowcol , top_lab , done_pb ;
      int nrow , nall ;
      Widget *surf_rc ;
      MCW_bbox **surf_bbox ;                       /* 19 Feb 2003 */
      MCW_arrowval **surf_node_av, **surf_line_av ;
      MCW_arrowval **surf_ledg_av ;                /* 26 Feb 2003 */
      MCW_arrowval *boxsize_av , *linewidth_av ;   /* 23 Feb 2003 */
} AFNI_surface_widgets ;

/*---*/

struct Three_D_View ;  /* incomplete type definition */

typedef struct {
  Widget wtop, rowcol;      /* containers */
  Widget top_lab;           /* overall report text */
  Widget top_menu , histrange_pb , fwhm_pb ;
  MCW_bbox *histsqrt_bbox ;

  MCW_arrowval *cmode_av ;  /* first row of controls */
  Widget clust3d_pb, savetable_pb, index_lab, prefix_tf, done_pb ;
  Widget savemask_pb ;      /* 01 May 2008 */

  Widget dataset_pb ;       /* second row of controls */
  MCW_arrowval *from_av, *to_av, *aver_av ;

  Widget dset_lab ;         /* label after second row */

  int nrow, nall, is_open ;
  Widget *clu_rc ;          /* rows of widgets */
  Widget *clu_lab ;
  Widget *clu_jump_pb ;
  Widget *clu_plot_pb ;
  Widget *clu_save_pb ;
  Widget *clu_flsh_pb ;
  Widget *clu_alph_lab ;

  THD_3dim_dataset *dset ;  /* selected from dataset_pb */
  int coord_mode ;
  int receive_on ;
  float hbot,htop ;
  float fwhm ;
} AFNI_clu_widgets ;      /** not yet used **/

/*---*/

typedef struct {
  Widget        rc ;
  MCW_bbox     *tog_bbox ;
  MCW_arrowval *menu_av ;
  Widget        chooser_pb ;
  MCW_arrowval *index_av ;
  Widget        chooser_lab ;
  Widget        string_lab ;
  Widget        string_text ;
} ICALC_widget_row ;

typedef struct {
  Widget wtop, rowcol ;  /* top level containers */

  Widget actar ;         /* action area holding control buttons */

  Widget olay_expr_text ;

  ICALC_widget_row  war[26] ;
  void             *var[26] ;

  int is_open ;
  struct Three_D_View *im3d ;
} ICALC_widget_set ;

typedef struct {
   int is_good ; char *prefix ;

   char *olay_expr ;
   void *olay_pcode ;
   THD_3dim_dataset *dset_master ;

   int               intyp[26] ;
   THD_3dim_dataset *inset[26] ; int inidx[26] ;
   double            inval[26] ;

   int dshift_mode , cxcode ;
   int dshift  [26] ;
   int dshift_i[26] ;
   int dshift_j[26] ;
   int dshift_k[26] ;
   int dshift_l[26] ;
   int has_sym[26] , has_predefined, has_xyz, mangle_xyz ;
} ICALC_setup ;

#undef  INIT_ICALC_setup
#define INIT_ICALC_setup(qcs)                                \
 do{ int qx ;                                                \
     (qcs) = (ICALC_setup *)calloc(1,sizeof(ICALC_setup)) ;  \
     for( qx=0 ; qx < 26 ; qx++ ) (qcs)->intyp[qx] = -666 ;  \
 } while(0)

#undef  DESTROY_ICALC_setup
#define DESTROY_ICALC_setup(qcs)     \
 do{ if( (qcs) != NULL ){            \
       int aa ;                      \
       XtFree((qcs)->ulay_expr) ;    \
       XtFree((qcs)->olay_expr) ;    \
       XtFree((qcs)->the_expr) ;     \
       free((qcs)) ; (qcs) = NULL ;  \
 }} while(0)

/*---*/

#include "gicor.h"   /* ZSS Jan 2010 */

/*---*/

typedef struct {
      Widget     frame , rowcol ;
      MCW_bbox * view_bbox ;

      Widget     marks_frame , marks_rowcol ;
      Widget     define_marks_pb ;
      MCW_bbox * see_marks_bbox ;
      int        marks_enabled ;

      Widget     func_frame , func_rowcol ;
      Widget     define_func_pb ;
      MCW_bbox * see_func_bbox ;

      Widget     define_dmode_pb ;

      Widget dataset_frame     , dataset_rowcol    ,
             choose_anat_pb    , choose_func_pb    ,
             popchoose_sess_pb , popchoose_anat_pb , popchoose_func_pb ;

      Widget choose_rowcol , rescan_pb, nimlpo_pb ;                    /* 02 Feb 2007 */
      Widget session_rowcol, sess_lab , choose_sess_pb, read_sess_pb ; /* 03 Dec 2009 */
      int    session_horz ;                                            /* 29 Apr 2010 */

      Boolean marks_pb_inverted , func_pb_inverted , dmode_pb_inverted ;

      Widget choose_surf_pb ;  /* 19 Aug 2002 */
      AFNI_surface_widgets *swid ;
} AFNI_viewing_widgets ;

extern void AFNI_sesslab_EV( Widget, XtPointer, XEvent *, Boolean * ) ; /* 30 Apr 2010 */

#define OPEN_PANEL(iq,panel)                                            \
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

#define THR_PVAL_LABEL_NONE " [N/A] "

#define THR_TOP_EXPON  4         /* 30 Nov 1997 */
#define THR_FACTOR     0.0001    /* pow(10,-THR_TOP_EXPON) */
#define THR_TOP_VALUE  9999.0    /* pow(10,THR_TOP_EXPON)-1 */

#undef USE_FUNC_FIM              /* 09 Dec 1997 */

typedef struct {
      Widget frame , rowcol ;

      Widget thr_rowcol , thr_label , thr_scale , thr_pval_label ;
      MCW_arrowval * thr_top_av ;

      Widget thr_menu ;
      MCW_bbox *thr_onoff_bbox ;
      Widget thr_autothresh_pb ;
      MCW_arrowval *thr_sign_av ;  /* 08 Aug 2007 */
      Widget thr_fdr_pb ;          /* 29 Jan 2008 */

      Widget inten_rowcol , inten_label ;
      MCW_pbar     *inten_pbar ;
      MCW_arrowval *inten_av ;
      MCW_bbox     *inten_bbox ;

      Widget pbar_menu , pbar_equalize_pb , pbar_settop_pb ,
             pbar_readin_pb , pbar_writeout_pb ;
      MCW_arrowval *pbar_palette_av ;
      Widget pbar_showtable_pb ;
      Widget pbar_environment_pb ; /* 10 Feb 2004 */

      Widget pbar_saveim_pb  ;                  /* 15 Jun 2000 */
      MCW_arrowval *pbar_transform0D_av ;
      generic_func *pbar_transform0D_func ;
      int           pbar_transform0D_index ;
      MCW_arrowval *pbar_transform2D_av ;
      generic_func *pbar_transform2D_func ;
      int           pbar_transform2D_index ;

      Widget options_rowcol , options_top_rowcol , options_label ;
      MCW_arrowval *options_vedit_av ;
      Widget ulaclu_rowcol , vedit_frame ;
      MCW_bbox     *underlay_bbox ;
      Widget clu_rowcol, clu_clear_pb, clu_cluster_pb, clu_report_pb ;  /* 05 Sep 2006 */

      Widget icor_rowcol  , icor_pb  , icor_label  ; /* 05 May 2009 */

      Widget icalc_rowcol , icalc_pb , icalc_label ; /* 18 Sep 2009 */

      Widget gicor_rowcol , gicor_pb , gicor_label ; /* 22 Dec 2009 */

      Widget         buck_frame , buck_rowcol ;
      MCW_arrowval * anat_buck_av , *fim_buck_av , *thr_buck_av ;  /* 30 Nov 1997 */

      Widget range_frame , range_rowcol , range_label ;
      MCW_bbox     *range_bbox ;
      MCW_arrowval *range_av ;

#ifdef USE_FUNC_FIM
      Widget fim_frame , fim_rowcol , fim_dset_label , fim_mbar ;
      FIM_menu *fim_menu ;
#endif

      Widget bkgd_lab ;

      MCW_arrowval *range_rotate_av ;  /* 30 Mar 2001 */

      MCW_bbox *see_ttatlas_bbox ;     /* 25 Jul 2001 */

      AFNI_clu_widgets   *cwid ;       /* 18 Dec 2007 */
      char               *clu_rep ;
      MCW_cluster_array  *clu_list;
      int                 clu_index;
      int                 clu_num ;
      mri_cluster_detail *clu_det ;

      ICALC_widget_set   *iwid ;       /* 17 Sep 2009 */
} AFNI_function_widgets ;

extern void AFNI_func_autothresh_CB(Widget,XtPointer,XtPointer) ; /* 25 Jul 2007 */
extern void AFNI_func_thrsign_CB( MCW_arrowval * , XtPointer ) ;  /* 08 Aug 2007 */
extern void AFNI_func_fdr_CB    (Widget,XtPointer,XtPointer) ;    /* 29 Jan 2008 */

#define PBAR_MODEBUT  0
#define PBAR_MODEPOS  (1 << PBAR_MODEBUT)

#define RANGE_AUTOBUT 0
#define RANGE_AUTOVAL (1 << RANGE_AUTOBUT)

/** On Motif 2.0 on Linux, resized pbar pieces causes the
    threshold scale to behave bizarrely.  This macro is a fixup **/

#ifdef FIX_SCALE_SIZE_PROBLEM
#  define FIX_SCALE_SIZE(iqqq)                                           \
     do{ int sel_height ;  XtPointer sel_ptr=NULL ;                      \
         XtVaGetValues( (iqqq)->vwid->func->thr_scale ,                  \
                           XmNuserData , &sel_ptr , NULL ) ;             \
         sel_height = PTOI(sel_ptr) ;                                    \
         XtVaSetValues( (iqqq)->vwid->func->thr_scale ,                  \
                           XmNheight , sel_height , NULL ) ;             \
         XtManageChild((iqqq)->vwid->func->thr_scale) ;                  \
       } while(0)
#  define HIDE_SCALE(iqqq) XtUnmanageChild((iqqq)->vwid->func->thr_scale)
#else
#  define FIX_SCALE_SIZE(iqqq) /* nada */
#  define HIDE_SCALE(iqqq)     /* nada */
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
      Widget         misc_runscript_pb ;  /* 22 Jan 2003 */

      Widget         misc_readme_env_pb ; /* 05 Aug 2004 */

      Widget         misc_motd_pb ;       /* 29 Nov 2005 */
      Widget         misc_hist_pb ;       /* 05 Mar 2008 */

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
   Widget hidden_gamberi_pb ;  /* 14 Oct 2003 */
   Widget hidden_ranpoem_pb ;  /* 15 Oct 2003 */
   Widget hidden_speech_pb  ;  /* 25 Nov 2003 */
   Widget hidden_faces_pb   ;  /* 17 Dec 2004 */
   Widget hidden_browser_pb ;  /* 22 Apr 2005 */
   Widget hidden_broutim_pb ;  /* 06 Jun 2005 */
   Widget hidden_broutext_pb;  /* 21 Dec 2005 */
   Widget hidden_splashes_pb;  /* 12 Sep 2007 */

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

      int butx , buty ;        /* 17 May 2005 */
} AFNI_widget_set ;

/** picture controls **/

#define PICTURE_SET(im,px)                                            \
   do{ if( (im)->type == AFNI_3DDATA_VIEW )                           \
        XtVaSetValues((im)->vwid->picture,XmNlabelPixmap,(px),NULL ); \
   } while(0)

#define PICTURE_ON(im)   PICTURE_SET(im,logo_pixmap)
#define PICTURE_DEAD(im) PICTURE_SET(im,XmUNSPECIFIED_PIXMAP)
#define PICTURE_OFF(im)  PICTURE_SET(im,pict_pixmap[AFNI_controller_index(im)%NPICT])

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

typedef struct Three_D_View {
      int type , opened ;
      MCW_DC *dc ;

      THD_session      *ss_now ;   /* session now being viewed */
      THD_3dim_dataset *anat_dset[LAST_VIEW_TYPE+1] ,   /* datasets now */
                       *fim_dset [LAST_VIEW_TYPE+1]  ;  /* being viewed */
      THD_3dim_dataset *anat_now , *fim_now ;  /* REALLY now being viewed */

      AFNI_view_info   *vinfo ;  /* information about what's being viewed */

      AFNI_fimmer_type *fimdata ; /* information about fimming */

      FD_brick  *b123_anat , *b231_anat , *b312_anat ; /* anat */
      FD_brick  *b123_fim  , *b231_fim  , *b312_fim  ; /* funcs */
      FD_brick  *b123_ulay , *b231_ulay , *b312_ulay ; /* underlays */

      MCW_imseq   *s123    , *s231      , *s312 ;      /* viewers */
      MCW_grapher *g123    , *g231      , *g312 ;      /* graphs */

      AFNI_widget_set  *vwid ;
      char window_title[THD_MAX_NAME] ;
      int ignore_seq_callbacks ;

      THD_dataxes *wod_daxes ;                 /* 02 Nov 1996 */
      THD_warp *anat_voxwarp , *fim_voxwarp ;
      int anat_wod_flag , fim_wod_flag ;

      KILL_list kl ;
      XtPointer parent ;

      int brand_new ;                           /* 07 Dec 2001 */

      THD_warp *fim_selfwarp ;                 /* 27 Aug 2002 */

      int dummied ;                             /* 27 Jan 2004 */

      VEDIT_settings vedset ;                   /* 05 Sep 2006 */
      char *vedlabel ;                          /* 27 Mar 2007 */
      int   vedskip ;

      ICOR_setup   *iset ;                       /* 05 May 2009 */
      ICALC_setup  *icalc_setup ;                /* 18 Sep 2009 */
      GICOR_setup  *giset ;                      /* 22 Dec 2009 */
} Three_D_View ;

/*! Force re-volume-editing when this viewer is redisplayed */

#define IM3D_VEDIT_FORCE(iq) (iq)->vedset.flags=1

/*! Turn clusterized display off in this viewer */

#define UNCLUSTERIZE(iq)                                                   \
 do{ int redis=0 ;                                                         \
     AFNI_vedit_clear((iq)->fim_now); VEDIT_clear_label((iq));             \
     AFNI_cluster_dispkill((iq));                                          \
     if( (iq)->vwid->func->clu_rep != NULL ){                              \
       free((iq)->vwid->func->clu_rep) ;                                   \
       (iq)->vwid->func->clu_rep = NULL ; redis++ ;                        \
     }                                                                     \
     DESTROY_CLARR((iq)->vwid->func->clu_list);                            \
     if( (iq)->vedset.code ) redis++ ;                                     \
     (iq)->vedset.flags = (iq)->vedset.code = 0; AFNI_set_thr_pval((iq));  \
     if( (iq)->vinfo->func_visible && redis ) AFNI_redisplay_func((iq)) ;  \
 } while(0) ;

#define STOP_COLOR "#770000"
#define GO_COLOR   "#005500"
#define WORK_COLOR "#888800"

#define INSTACORR_LABEL_ON(iq)                                          \
 do{ MCW_set_widget_label((iq)->vwid->func->icor_label,"** Ready **") ; \
     MCW_set_widget_bg   ((iq)->vwid->func->icor_label,GO_COLOR,0   ) ; \
 } while(0)

#define INSTACORR_LABEL_OFF(iq)                                         \
 do{ MCW_set_widget_label((iq)->vwid->func->icor_label,"*NOT Ready*") ; \
     MCW_set_widget_bg   ((iq)->vwid->func->icor_label,STOP_COLOR,0 ) ; \
 } while(0)

#define INSTACORR_LABEL_WORKING(iq)                                     \
 do{ MCW_set_widget_label((iq)->vwid->func->icor_label,"..Working..") ; \
     MCW_set_widget_bg   ((iq)->vwid->func->icor_label,WORK_COLOR,0 ) ; \
 } while(0)

/*! Change InstaCorr popup buttons status */

#define SENSITIZE_INSTACORR(iq,bb)                           \
 do{ XtSetSensitive((iq)->vwid->imag->pop_instacorr_pb,bb) ; \
     XtSetSensitive((iq)->vwid->imag->pop_icorrjump_pb,bb) ; \
 } while(0)

/*! Allow InstaCorr in this viewer */

#define ENABLE_INSTACORR(iq)           \
 do{ SENSITIZE_INSTACORR((iq),True) ;  \
     INSTACORR_LABEL_ON((iq)) ;        \
 } while(0)

/*! Turn InstaCorr off in this viewer */

#define DISABLE_INSTACORR(iq)                                                 \
 do{ SENSITIZE_INSTACORR((iq),False) ;                                        \
     INSTACORR_LABEL_OFF((iq)) ;                                              \
     (iq)->vinfo->i1_icor = (iq)->vinfo->j2_icor = (iq)->vinfo->k3_icor = -1; \
     AFNI_misc_CB((iq)->vwid->func->icor_pb,(XtPointer)(iq),NULL) ;           \
 } while(0)

/*--- similar stuff for Group InstaCorr [22 Dec 2009] ---*/

#define ISREADY_ICOR(iq)        ISVALID_ICOR_setup((iq)->iset)

#define ISREADY_GICOR(iq)       ( (iq)->giset != NULL && (iq)->giset->ready )

#define ISREADY_EITHER_ICOR(iq) ( ISREADY_ICOR(iq) || ISREADY_GICOR(iq) )

#define GRPINCORR_LABEL_ON(iq)                                             \
 do{ if( (iq)->vwid->func->gicor_rowcol != NULL ){                         \
       MCW_set_widget_label((iq)->vwid->func->gicor_label,"** Ready **") ; \
       MCW_set_widget_bg   ((iq)->vwid->func->gicor_label,GO_COLOR,0   ) ; \
     } } while(0)

#define GRPINCORR_LABEL_OFF(iq)                                            \
 do{ if( (iq)->vwid->func->gicor_rowcol != NULL ){                         \
       MCW_set_widget_label((iq)->vwid->func->gicor_label,"*NOT Ready*") ; \
       MCW_set_widget_bg   ((iq)->vwid->func->gicor_label,STOP_COLOR,0 ) ; \
     } } while(0)

#define GRPINCORR_LABEL_WORKING(iq)                                        \
 do{ if( (iq)->vwid->func->gicor_rowcol != NULL ){                         \
       MCW_set_widget_label((iq)->vwid->func->gicor_label,"..Working..") ; \
       MCW_set_widget_bg   ((iq)->vwid->func->gicor_label,WORK_COLOR,0 ) ; \
       MCW_set_widget_fg   ((iq)->vwid->func->gicor_label,"#000088"    ) ; \
     } } while(0)

#define ENABLE_GRPINCORR(iq)          \
 do{ SENSITIZE_INSTACORR((iq),True) ; \
     (iq)->giset->ready = 1 ;         \
     GRPINCORR_LABEL_ON((iq)) ;       \
 } while(0)

#define DISABLE_GRPINCORR(iq)                                                    \
 do{ if( (iq)->vwid->func->gicor_rowcol != NULL ){                               \
       SENSITIZE_INSTACORR((iq),False) ;                                         \
       if( (iq)->giset != NULL ) (iq)->giset->ready = 0 ;                        \
       GRPINCORR_LABEL_OFF((iq)) ;                                               \
       (iq)->vinfo->i1_icor = (iq)->vinfo->j2_icor = (iq)->vinfo->k3_icor = -1;  \
       AFNI_misc_CB((iq)->vwid->func->gicor_pb,(XtPointer)(iq),NULL) ;           \
     }                                                                           \
 } while(0)

#define GRPINCORR_ready(iq) \
 (IM3D_OPEN(iq) && (iq)->giset != NULL) ? (iq)->giset->ready : 0

/** InstaCalc stuff [18 Sep 2009] **/

#define INSTACALC_LABEL_ON(iq)                                           \
 do{ MCW_set_widget_label((iq)->vwid->func->icalc_label,"*Computed!*") ; \
     MCW_set_widget_bg   ((iq)->vwid->func->icalc_label,GO_COLOR,0   ) ; \
 } while(0)

#define INSTACALC_LABEL_OFF(iq)                                          \
 do{ MCW_set_widget_label((iq)->vwid->func->icalc_label,"*NOT Ready*") ; \
     MCW_set_widget_bg   ((iq)->vwid->func->icalc_label,STOP_COLOR,0 ) ; \
 } while(0)

#define DISABLE_INSTACALC(iq)                                            \
 do{ INSTACALC_LABEL_OFF(iq) ;                                           \
     if( (iq)->icalc_setup != NULL ) (iq)->icalc_setup->is_good = 0 ;    \
     if( (iq)->vwid->func->iwid != NULL )                                \
       XtUnmapWidget((iq)->vwid->func->iwid->wtop) ;                     \
 } while(0)

/*! Is any image viewer window open? */

#define IM3D_IMAGIZED(iq) \
 ( (iq)->s123 != NULL || (iq)->s231 != NULL || (iq)->s312 != NULL )

/*! Is any graph viewer window open? */

#define IM3D_GRAPHIZED(iq) \
 ( (iq)->g123 != NULL || (iq)->g231 != NULL || (iq)->g312 != NULL )

/*! Is any image or graph viewer doing a timer thing? [21 Dec 2006] */

#define IM3D_TIMERIZED(iq)                              \
 ( ((iq)->s123 != NULL && (iq)->s123->timer_id > 0) ||  \
   ((iq)->s231 != NULL && (iq)->s231->timer_id > 0) ||  \
   ((iq)->s312 != NULL && (iq)->s312->timer_id > 0) ||  \
   ((iq)->g123 != NULL && (iq)->g123->timer_id > 0) ||  \
   ((iq)->g231 != NULL && (iq)->g231->timer_id > 0) ||  \
   ((iq)->g312 != NULL && (iq)->g312->timer_id > 0)   )

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
         (iq)->fim_now->self_warp = (iq)->fim_selfwarp ; \
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

extern void AFNI_inconstancy_check( Three_D_View *, THD_3dim_dataset * ); /* 06 Sep 2006 */

extern Three_D_View * AFNI_find_open_controller(void) ; /* 05 Mar 2002 */
extern void AFNI_popup_message( char * ) ;

extern void   AFNI_start_version_check(void) ;   /* 21 Nov 2002 */
extern int    AFNI_version_check      (void) ;
extern char * AFNI_make_update_script (void) ;   /* 20 Nov 2003 */

extern char * AFNI_get_friend(void) ;      /* 26 Feb 2001 */
extern char * AFNI_get_date_trivia(void) ; /* 25 Nov 2002 */
extern int AFNI_get_todays_trivia( char *** ) ; /* 27 Nov 2007 */

#define OPEN_CONTROLLER(iq)                                  \
 do{ XtRealizeWidget((iq)->vwid->top_shell) ;                \
     while(XtWindow((iq)->vwid->top_shell)==(Window)NULL) ;  \
     AFNI_startup_3dview(iq); (iq)->opened = 1;              \
 } while(0)

#define CLOSE_CONTROLLER(iq) ( AFNI_closedown_3dview(iq),                \
                               XtUnrealizeWidget((iq)->vwid->top_shell), \
                               AFNI_clus_popdown(iq) ,                   \
                               (iq)->opened = 0 )

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

#ifdef  __cplusplus
}
#endif

#include "afni_plugin.h"

#ifdef  __cplusplus
extern "C" {
#endif

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
   extern PLUGIN_interface * ICOR_init(char *);          /* 29 Apr 2009 */
   extern PLUGIN_interface * GICOR_init(char *);         /* 22 Dec 2009 */
#endif

extern void GICOR_setup_func(NI_stream, NI_element *) ;        /* 22 Dec 2009 */
extern void GICOR_process_dataset( NI_element *nel, int ct ) ; /* 23 Dec 2009 */

extern void ICALC_make_widgets( Three_D_View *im3d ) ;   /* 18 Sep 2009 */

typedef struct {                /* windows and widgets */
   XtPointer_array *windows ;   /* allowed to interrupt */
   XtPointer_array *widgets ;   /* 'real-time' functions */
} MCW_interruptables ;

#ifndef MAX_CONTROLLERS
#define MAX_CONTROLLERS 10  /* 12 Nov 2002: increased from 5, per MSB */
#endif

/*-------------- Here there be global variables.  So shoot me. --------------*/

#ifdef  __cplusplus
}
#endif

#include "afni_setup.h"  /* 19 Dec 1997 */

#ifdef  __cplusplus
extern "C" {
#endif

typedef struct {
   int numchan ;
   int status ;
   int numdset ;
   THD_3dim_dataset **dset ;
} RT_status ;

#define RT_STARTUP    1  /* status codes [01 Jun 2009] */
#define RT_CONTINUE   2
#define RT_FINISHED   3

typedef struct {
   MCW_DC *dc ;                                  /* display context for everyone */
   THD_sessionlist *sslist ;                     /* all sessions viewable */
   MRI_IMARR *timeseries ;                       /* all timeseries available */
   Three_D_View *controllers[MAX_CONTROLLERS] ;  /* all controllers available */
   MCW_interruptables interruptables ;           /* windows and widgets */

   MCW_function_list registered_0D ;             /* registered functions */
   MCW_function_list registered_1D ;
   MCW_function_list registered_2D ;

   int controller_lock , ignore_lock ;
   int have_dummy_dataset ;
   int sesstrail ;                               /* 23 Oct 1998 */

   THD_coorder cord ;

#ifdef ALLOW_PLUGINS
   struct AFNI_plugin_array *plugins ;           /* plugins */
#endif

   PBAR_palette_table *gpt ;

   int time_lock ;                               /* 03 Nov 1998 */

   int hints_on ;                                /* 01 Aug 1999 */

   float fim_bkthr_perc ;                        /* 02 Jun 1999 */

   MCW_function_list registered_fim ;            /* 30 Jan 2000 */

   int ijk_lock ;                                /* 11 Sep 2000 */

   THD_session *session ;                        /* 20 Dec 2001 */

   MCW_function_list registered_slice_proj ;     /* 31 Jan 2002 */

   Htable *warptable ;                           /* 28 Aug 2002 */

   RT_status *realtime_status ;                  /* 01 Jun 2009 */
   gen_func  *realtime_callback ;

} AFNI_library_type ;

#ifdef MAIN
   AFNI_library_type GLOBAL_library ;
   int GLOBAL_num_dsets = 0 ;
   char *GLOBAL_motd = NULL ;                     /* 29 Nov 2005 */
   char *GLOBAL_browser = NULL ;                  /* 30 Dec 2005 */
#else
   extern AFNI_library_type GLOBAL_library ;
   extern int GLOBAL_num_dsets ;
   extern char *GLOBAL_motd ;
   extern char *GLOBAL_browser ;
#endif

extern void AFNI_display_motd( Widget w ) ;       /* 29 Nov 2005 */
extern void AFNI_display_hist( Widget w ) ;       /* 05 Mar 2008 */

#define FIM_THR          (0.01*GLOBAL_library.fim_bkthr_perc)  /* 02 Jun 1999 */
#define SET_FIM_bkthr(v) (GLOBAL_library.fim_bkthr_perc = (v))

#define DISABLE_LOCK    (GLOBAL_library.ignore_lock=1)
#define ENABLE_LOCK     (GLOBAL_library.ignore_lock=0)
#define BEEPIT          XBell(GLOBAL_library.dc->display,100)
#define ALLOW_realtime  GLOBAL_argopt.allow_rt
#define ELIDE_quality   GLOBAL_argopt.elide_quality
#define GPT             GLOBAL_library.gpt
#define NO_frivolities  GLOBAL_argopt.no_frivolities
#define SESSTRAIL       GLOBAL_library.sesstrail
#define AFNI_VERBOSE    (!GLOBAL_argopt.quiet)  /* 25 Oct 2001 */

#define THE_DISPLAY     (GLOBAL_library.dc->display)  /* 02 Aug 2002 */
#define THE_TOPSHELL    (GLOBAL_library.controllers[0]->vwid->top_shell)
#define A_CONTROLLER    (GLOBAL_library.controllers[0])

# define SUMA_ENABLED   GLOBAL_argopt.enable_suma

#define DOING_REALTIME_WORK (GLOBAL_library.interruptables.windows != NULL)

#define UNDUMMYIZE                                                              \
 do { GLOBAL_library.have_dummy_dataset = 0 ;                                   \
      XtSetSensitive(GLOBAL_library.controllers[0]->vwid->prog->clone_pb,True); \
    } while(0)

/*-----------------------------------------------------------*/
/*------------------------ prototypes -----------------------*/

extern int AFNI_vnlist_func_overlay( Three_D_View *,int, SUMA_irgba **,int * ) ;
extern int AFNI_vol2surf_func_overlay( Three_D_View *, SUMA_irgba **,
                                       int, int, int, float **, float * );
extern float * AFNI_v2s_node_timeseries(THD_session *, THD_3dim_dataset *,
                                int, int, int, int);  /* 29 Apr 2009 [rickr] */

extern void AFNI_parse_args( int argc , char * argv[] );

extern void AFNI_splashup   (void) ;  /* 02 Aug 1999 */
extern void AFNI_splashdown (void) ;
extern void AFNI_splashraise(void) ;  /* 25 Sep 2000 */
extern void AFNI_faceup     (void) ;  /* 17 Dec 2004 */
extern void AFNI_allsplash  (void) ;  /* 12 Sep 2007 */
extern int  AFNI_splash_isopen(void); /* 10 Nov 2005 */
extern void AFNI_broutim_CB (Widget,XtPointer,XtPointer) ; /* 06 Jun 2005 */
extern void AFNI_broutext_CB(Widget,XtPointer,XtPointer) ; /* 21 Dec 2005 */

extern void AFNI_quit_CB           ( Widget wcall , XtPointer cd , XtPointer cbs );
extern void AFNI_quit_timeout_CB   ( XtPointer , XtIntervalId * ) ;
extern void AFNI_startup_timeout_CB( XtPointer , XtIntervalId * ) ;
extern void AFNI_vcheck_flasher    ( Three_D_View * ) ;

extern void AFNI_startup_layout_CB  ( XtPointer, XtIntervalId * ) ;    /* 23 Sep 2000 */
extern void AFNI_save_layout_CB     ( Widget, XtPointer, XtPointer ) ;
extern void AFNI_finalsave_layout_CB( Widget, XtPointer, MCW_choose_cbs * ) ;
extern void AFNI_startup_script_CB  ( XtPointer, XtIntervalId * ) ;    /* 21 Jan 2003 */
extern void AFNI_run_script_CB      ( Widget, XtPointer, XtPointer ) ; /* 22 Jan 2003 */
extern void AFNI_finalrun_script_CB ( Widget, XtPointer, MCW_choose_cbs * ) ;

#define AFNI_run_script(ss) AFNI_startup_script_CB((XtPointer)(ss),NULL)

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

extern void AFNI_thresh_lock_carryout( Three_D_View * ) ; /* 06 Feb 2004 */
extern void AFNI_pbar_lock_carryout  ( Three_D_View * ) ; /* 07 Feb 2004 */
extern void AFNI_equate_pbars        ( Three_D_View *, Three_D_View * ) ;
extern void AFNI_thrdrag_lock_carryout( Three_D_View * ) ;
extern void AFNI_range_lock_carryout( Three_D_View * ) ;  /* 23 Feb 2004 */

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
extern void AFNI_invert_CB( Widget, XtPointer, XtPointer ) ; /* 02 Feb 2007 */
extern void AFNI_nimlpo_CB( Widget, XtPointer, XtPointer ) ; /* 02 Feb 2007 */
extern void AFNI_process_NIML_data( int , void * , int ) ;   /* 01 Feb 2008 */

extern char * AFNI_controller_label( Three_D_View * im3d ); /* 01 Apr 1999 */
extern void AFNI_set_window_titles( Three_D_View * im3d );

extern void AFNI_crosshair_visible_CB( MCW_arrowval * , XtPointer ) ;
extern void AFNI_view_xyz_CB         ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_marktog_CB          ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_marks_action_CB     ( Widget , XtPointer , XtPointer ) ;

extern void AFNI_viewbut_EV( Widget, XtPointer, XEvent *, Boolean * ) ;
extern void AFNI_cluster_EV( Widget, XtPointer, XEvent *, Boolean * ) ;
extern void AFNI_clus_update_widgets( Three_D_View *im3d ) ;
extern void AFNI_clus_popdown( Three_D_View *im3d ) ;

extern void AFNI_update_dataset_viewing( THD_3dim_dataset * ); /* 21 Jul 2009 */

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

typedef struct {
  int ndset ;
  THD_3dim_dataset **dset ;
  void (*cb)(Widget , XtPointer , MCW_choose_cbs *) ;
} AFNI_dataset_choose_stuff ;

extern void AFNI_switchview_CB        ( Widget , XtPointer , XtPointer ) ;
extern void AFNI_see_marks_CB         ( Widget , XtPointer , XtPointer ) ;
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
extern void AFNI_check_obliquity     ( Widget , THD_3dim_dataset * ) ;

extern void AFNI_crosshair_pop_CB    ( Widget , XtPointer , XtPointer ) ; /* 12 Mar 2004 */
extern void AFNI_crosshair_EV        ( Widget , XtPointer , XEvent * , Boolean * ) ;
extern void AFNI_crosshair_relabel   ( Three_D_View * ) ;

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
extern int  AFNI_rescan_session( int ) ;
extern void AFNI_rescan_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_rescan_all_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_rescan_timeseries_CB( Widget , XtPointer , XtPointer ) ;

extern void AFNI_block_rescan( int bb ) ;    /* 09 Nov 2005 */
extern void AFNI_rescan_timeout_CB( XtPointer , XtIntervalId * ) ;

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
extern int AFNI_tsname_in_library( char *nam ) ; /* 10 May 2009 */

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

extern void   AFNI_vedit_CB       ( MCW_arrowval * , XtPointer ) ; /* 05 May 2009 */
extern int    AFNI_icor_setref    ( Three_D_View *im3d ) ;
extern void   AFNI_icor_setref_locked( Three_D_View *im3d ) ;      /* 15 May 2009 */

extern int    AFNI_icor_setref_anatijk( Three_D_View *, int,int,int ) ;      /* 17 Mar 2010 */
extern int    AFNI_icor_setref_xyz    ( Three_D_View *, float,float,float );
extern int    AFNI_gicor_setref_xyz   ( Three_D_View *, float,float,float ); /* 23 Dec 2009 */

extern Boolean AFNI_refashion_dataset( Three_D_View * ,
                                       THD_3dim_dataset *, THD_dataxes * , int ) ;

#define REDISPLAY_OPTIONAL 0
#define REDISPLAY_OVERLAY  1
#define REDISPLAY_ALL      2

extern void AFNI_set_viewpoint( Three_D_View * , int,int,int , int ) ;
extern void AFNI_redisplay_func( Three_D_View * ) ; /* 05 Mar 2002 */
extern void AFNI_view_setter( Three_D_View *, MCW_imseq *) ; /* 26 Feb 2003 */
extern void AFNI_range_setter( Three_D_View *, MCW_imseq *); /* 04 Nov 2003 */

extern void AFNI_coord_filer_setup( Three_D_View *im3d ) ; /* 07 May 2010 */

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

extern THD_warp * AFNI_find_warp( THD_3dim_dataset * ,
                                  THD_3dim_dataset *  ) ; /* 28 Aug 2002 */

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

extern void AFNI_handler( char * ) ;

extern void AFNI_thr_scale_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_set_thr_pval( Three_D_View * im3d ) ;
extern void AFNI_thr_scale_drag_CB( Widget , XtPointer , XtPointer ) ;

extern float AFNI_get_autothresh( Three_D_View * ) ;        /* 05 Mar 2007 */
extern void AFNI_set_threshold( Three_D_View * , float  ) ;

extern void AFNI_inten_pbar_CB( MCW_pbar * , XtPointer , int ) ;
extern void AFNI_inten_av_CB( MCW_arrowval * , XtPointer ) ;
extern char * AFNI_inten_av_texter ( MCW_arrowval *, XtPointer ) ; /* 30 Jan 2003 */

extern void   AFNI_set_thresh_top( Three_D_View * , float ) ;
extern char * AFNI_thresh_tlabel_CB( MCW_arrowval * , XtPointer ) ;
extern void   AFNI_thresh_top_CB( MCW_arrowval * , XtPointer ) ;

extern void AFNI_set_valabel( FD_brick *, int, MRI_IMAGE *, char * ) ;

extern void AFNI_init_niml( void ) ; /* 28 Feb 2002 */
extern int  AFNI_have_niml( void ) ; /* 02 Feb 2007 */

extern void AFNI_choose_surface_CB( Widget , XtPointer , XtPointer ) ; /* 19 Aug 2002 */
extern void AFNI_update_surface_widgets( Three_D_View * ) ;
extern void AFNI_update_all_surface_widgets( THD_session * ) ;

extern void AFNI_init_suma_color( int, char *, char * ) ;   /* 06 Sep 2006 */
extern void AFNI_get_suma_color( int, rgbyte *, rgbyte * ); /* 07 Sep 2006 */

extern void AFNI_disable_suma_overlay( int ) ;  /* 16 Jun 2003 */

#ifdef  __cplusplus
}
#endif

/*-------------------------------------------------------------------
  Include prototypes for actual data warping and slicing here.
--------------------------------------------------------------------*/

#include "afni_warp.h"

#ifdef  __cplusplus
extern "C" {
#endif

/*------------------------------------------------------------------*/

extern THD_3dim_dataset * AFNI_follower_dataset( THD_3dim_dataset * ,
                                                 THD_3dim_dataset *  ) ;

extern void AFNI_make_descendants( THD_sessionlist * ) ;
extern void AFNI_mark_for_death  ( THD_sessionlist * ) ;
extern void AFNI_andersonville   ( THD_sessionlist * , Boolean ) ;
extern void AFNI_force_adoption  ( THD_session * , Boolean ) ;

extern MRI_IMAGE * AFNI_func_overlay( int , FD_brick * ) ;

extern MRI_IMAGE * AFNI_newfunc_overlay( MRI_IMAGE *, float,float ,  /* 30 Jan 2003 */
                                         MRI_IMAGE *,
                                         float,float, rgbyte * ) ;

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
extern void AFNI_editenv_CB ( Widget , XtPointer , XtPointer );

extern void AFNI_add_timeseries( MRI_IMAGE * ) ;
extern void AFNI_replace_timeseries( MRI_IMAGE * ) ; /* 10 May 2009 */

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
#define RECEIVE_TIMEINDEX_MASK   256  /* 29 Jan 2003 */

#define RECEIVE_ALL_MASK       ( 1 | 2 | 4 | 8 | 16 | 32 | 64 | 128 | 256 )

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
#define DRAWING_LINEWIDTH       13   /* 08 Oct 2002 */

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

#define TIMEINDEX_STARTUP       98   /* 29 Jan 2003 */
#define TIMEINDEX_SHUTDOWN      99

#define EVERYTHING_SHUTDOWN    666

/* modes for the process_drawing routine */

#define SINGLE_MODE           1000
#define PLANAR_MODE           2000
#define THREED_MODE           3000
#define SPECIAL_MODE        100000
#define UNDO_MODE           102000
#define INCVAL_MODE         103000
#define DECVAL_MODE         104000

extern void AFNI_toggle_drawing ( Three_D_View * ) ;
extern int AFNI_receive_init    ( Three_D_View *, int, gen_func *, void *, char * ) ;
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
extern void AFNI_process_timeindex  ( Three_D_View * ) ; /* 29 Jan 2003 */

extern void AFNI_do_bkgd_lab( Three_D_View * ) ;         /* 08 Mar 2002 */

extern MRI_IMAGE * AFNI_ttatlas_overlay(Three_D_View *, int,int,int,int, MRI_IMAGE *) ;

extern void AFNI_3d_linefill( int  ,int * ,int * ,int * ,
                              int *,int **,int **,int ** ) ;

/*-----------------------------------------------------------*/
/*----------------- data for Talairach To -------------------*/
/*--------- Some tables and some associated variables -------*/
/*--------- have been moved to thd_atlas_query.c and --------*/
/*--------- thd_atlas_query.h and included in libmri.a -------*/
/*--------- ZSS Feb. 06 --------------------------------------*/

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

extern void AFNI_set_thr_index ( Three_D_View * , int ) ; /* 30 Nov 2005 */
extern void AFNI_set_anat_index( Three_D_View * , int ) ;
extern void AFNI_set_fim_index ( Three_D_View * , int ) ;

extern void AFNI_driver_register( char * , int (*)(char *) ) ;

/*-------------------------------------------------------*/
/*--------------  registration of functions -------------*/

/* sample 0D transform functions */

extern void log10_func( int, float * ) ;
extern void ssqrt_func( int, float * ) ;

/* sample 1D transform functions */

extern void osfilt3_func( int, double,double, float * ) ;
extern void median3_func( int, double,double, float * ) ;
extern void absfft_func ( int, double,double, float * ) ;
extern void ztone_func  ( int, double,double, float * ) ; /* 02 Sep 2009 */
extern void adpt_wt_mn9 ( int, double,double, float * ) ; /* 04 Sep 2009 */

extern void L1normalize_func( int, double,double, float * ) ; /* 03 Sep 2009 */
extern void L2normalize_func( int, double,double, float * ) ; /* 03 Sep 2009 */

/* 31 Jan 2002: sample slice_proj transform functions */

extern float max_proj ( int, float * ) ;
extern float min_proj ( int, float * ) ;
extern float mean_proj( int, float * ) ;

extern float extreme_proj( int, float * ) ;  /* 02 Feb 2002 */
extern float osfilt_proj ( int, float * ) ;  /* 07 Dec 2007 */
extern float mad_proj    ( int, float * ) ;  /* 07 Dec 2007 */

extern float adaptive_weighted_mean( int , float * ) ;  /* 04 Sep 2009 */

/* sample 2D transform functions */

extern void median9_box_func ( int, int, double,double, float * ) ;
extern void winsor9_box_func ( int, int, double,double, float * ) ;
extern void osfilt9_box_func ( int, int, double,double, float * ) ;
extern void fft2D_absfunc    ( int, int, double,double, float * ) ;
extern void fft2D_phasefunc  ( int, int, double,double, float * ) ;
extern void median21_box_func( int, int, double,double, float * ) ;
extern void winsor21_box_func( int, int, double,double, float * ) ;
extern void adapt_mean_21_box_func( int, int, double,double, float * ) ; /* 04 Sep 2009 */

extern void AFNI_register_nD_function( int, char *, generic_func *, int ) ;
extern void AFNI_register_nD_func_init( int nd , generic_func *fin ) ;

#define AFNI_register_0D_function(cc,ff) \
   AFNI_register_nD_function(0,(char *)(cc),(generic_func *)(ff),0)
#define AFNI_register_1D_function(cc,ff) \
   AFNI_register_nD_function(1,(char *)(cc),(generic_func *)(ff),0)
#define AFNI_register_2D_function(cc,ff) \
   AFNI_register_nD_function(2,(char *)(cc),(generic_func *)(ff),0)

#define AFNI_register_slice_proj(cc,ff)  \
   AFNI_register_nD_function(-1,(char *)(cc),(generic_func *)(ff),0)   /* 31 Jan 2002 */

#define AFNI_register_1D_funcstr(cc,ff)  \
   AFNI_register_nD_function(1,(char *)(cc),(generic_func *)(ff),RETURNS_STRING)

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
   "white"   , "#dddddd"   , "#bbbbbb"  , "#010101" ,

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

#ifdef  __cplusplus
}
#endif

#endif /* _AFNI_HEADER_ */
