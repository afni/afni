#ifndef _MCW_TO3D_HEADER_
#define _MCW_TO3D_HEADER_

#include "mrilib.h"
#include "imseq.h"
#include "killer.h"
#include "afni_warp.h"
#include "mcw_glob.h"

#include <Xm/SeparatoG.h>

#include <Xm/Display.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>

#define NO_NAMES

typedef struct {
   int   xorient,yorient,zorient , voxshape , voxcontig ,
         xyz_centered , view_type ,
         dataset_type , anatomy_type , function_type , nx,ny,nz ;

   float fov , xsize,ysize,zsize , zspacing ,
         xorigin,yorigin,zorigin ;

   char dataset_name[THD_MAX_NAME] ,
        short_label1[THD_MAX_LABEL] , short_label2[THD_MAX_LABEL] ;

   char dataset_type_string[THD_MAX_NAME] ,
        anatomy_type_string[THD_MAX_NAME] ,
        function_type_string[THD_MAX_NAME] ,
        geometry_parent_filename[THD_MAX_NAME] ,
        anatomy_parent_filename[THD_MAX_NAME] ,
        output_filename[THD_MAX_NAME] ,
        session_filename[THD_MAX_NAME] ;

   char geometry_dataname[THD_MAX_NAME] ,
	anatomy_dataname[THD_MAX_NAME] ;

   int  nimage , nvals , nosave ;

#ifndef OMIT_DATASET_IDCODES
   MCW_idcode anatomy_parent_idcode ;
#endif

   int   need_stat_aux ;
   float stat_aux[MAX_STAT_AUX] ;

   int   xincode , yincode , zincode ;
   float xin_bot,xin_top , yin_bot,yin_top , zin_bot,zin_top ;

   int ntt , nzz , t_then_z , tunits ;
   float TR ;
   float * tpattern ;
} to3d_data ;

#define INCODE_NONE -666
#define INCODE_FOV   100
#define INCODE_SLAB  200

#define ORCODE(aa) \
  ( (aa)=='R' ? ORI_R2L_TYPE : (aa)=='L' ? ORI_L2R_TYPE : \
    (aa)=='P' ? ORI_P2A_TYPE : (aa)=='A' ? ORI_A2P_TYPE : \
    (aa)=='I' ? ORI_I2S_TYPE : (aa)=='S' ? ORI_S2I_TYPE : ILLEGAL_TYPE )

typedef struct {
   Widget topshell , topform ;

   MCW_arrowval * xorient_av , * yorient_av , * zorient_av ,
                * xsize_av   , * ysize_av   , * zsize_av   ,
                * xorigin_av , * yorigin_av , * zorigin_av ,
                * fov_av     ,                * zspacing_av  ;
   MCW_bbox     * voxshape_bbox , * voxcontig_bbox , * centered_bbox ;

   Widget region_separator ;

   Widget
#ifndef NO_NAMES
          dataset_name_label    , dataset_name_textfield    ,
          short_label1_label    , short_label1_textfield    ,
          short_label2_label    , short_label2_textfield    ,
#endif
          geometry_parent_label , geometry_parent_textfield ,
          anatomy_parent_label  , anatomy_parent_textfield   ;

#ifndef NO_NAMES
   Widget geometry_dataname_label , geometry_dataname_textfield ,
	  anatomy_dataname_label  , anatomy_dataname_textfield  ;
#endif

   MCW_arrowval * dataset_type_av , * function_type_av , * anatomy_type_av ;

   Widget output_file_label , output_file_textfield ;
   Widget session_file_label , session_file_textfield ;

   Widget action_frame , action_rowcol ,
          button_help_pb , open_view_pb , save_file_pb , quit_pb ;
   Widget swap_pb ;  /* 14 Sep 1998 */

   /** April 1996: new widgets **/

   Widget       xorigin_label , yorigin_label , zorigin_label ;
   MCW_arrowval * view_type_av ;
   Widget       stat_aux_label , stat_aux_textfield , datum_label , TR_label ;

   /** Miscellaneous other stuff */

   MCW_DC    * dc ;
   MCW_imseq * seq ;
} to3d_widget_set ;

#define T3D_NAME_WIDTH   25
#define T3D_FORM_SPACING 11

static char orbuf[2] ;
#define SET_ORIGIN_LABEL(ww,cc)           \
   ( orbuf[1] = '\0' ,                    \
     orbuf[0] = ORIENT_typestr[(cc)][0] , \
     MCW_set_widget_label((ww),orbuf) )

/*-----------------------------------------------------------------*/

static char * T3D_voxshape_label[3] = {
   "cubical" , "square" , "irregular"
} ;

#define VOXSHAPE_CUBICAL   1
#define VOXSHAPE_SQUARE    2
#define VOXSHAPE_IRREGULAR 4

static char * T3D_voxcontig_label[3] = {
   "contiguous" , "unif.noncontig" , "irreg.noncontig" } ;

#define VOXCONTIG_YES   1
#define VOXCONTIG_UNIF  2
#define VOXCONTIG_IRREG 4

static char * T3D_centered_label[3] = {
  "x axis centered" , "y axis centered" , "z axis centered" } ;

#define XCENTERED  1
#define YCENTERED  2
#define ZCENTERED  4

/*-----------------------------------------------------------------*/

#define NCOLOVR 2
static char * FD_colovr[NCOLOVR] = { "yellow"  , "cyan" } ;

#define NGRAY 80   /* default for -ncolor */
#define GAMMA 1.0  /* default for -gamma  */

/*-----------------------------------------------------------------*/

void T3D_create_widgets(void) ;
void T3D_read_images(void) ;
void T3D_setup_stat_aux(void) ;

void T3D_stat_aux_CB  (Widget, XtPointer, XtPointer );
void T3D_voxshape_CB  (Widget, XtPointer, XtPointer );
void T3D_voxcontig_CB (Widget, XtPointer, XtPointer );
void T3D_centered_CB  (Widget, XtPointer, XtPointer );
void T3D_open_view_CB (Widget, XtPointer, XtPointer );
void T3D_save_file_CB (Widget, XtPointer, XtPointer );
void T3D_quit_CB      (Widget, XtPointer, XtPointer );
void T3D_swap_CB      (Widget, XtPointer, XtPointer );

void T3D_geometry_parent_CB(Widget, XtPointer, XtPointer );
void T3D_anatomy_parent_CB (Widget, XtPointer, XtPointer );
void T3D_pointer_leave_EV  (Widget, XtPointer, XEvent * , Boolean * ) ;

void T3D_imseq_CB( MCW_imseq * , FD_brick * , ISQ_cbs * ) ;

XtPointer T3D_getim( int , int , FD_brick * ) ;

#define RESET_QUIT T3D_quit_CB(NULL,NULL,NULL)

char * T3D_text_display( MCW_arrowval * , XtPointer ) ;

void    T3D_initialize_user_data(void) ;
Boolean T3D_check_data(Boolean) ;

void T3D_orient_av_CB ( MCW_arrowval * , XtPointer ) ;
void T3D_origin_av_CB ( MCW_arrowval * , XtPointer ) ;
void T3D_fov_av_CB    ( MCW_arrowval * , XtPointer ) ;
void T3D_size_av_CB   ( MCW_arrowval * , XtPointer ) ;
void T3D_type_av_CB   ( MCW_arrowval * , XtPointer ) ;

void T3D_set_dependent_geometries(void) ;
void T3D_widgets_to_data(void) ;
void T3D_data_to_widgets(void) ;

void T3D_poperr(char *,char *) ;

void T3D_fix_dataset_dimen(void) ;
void Syntax() ;

void AFNI_load_defaults( Widget ) ;

int decode_location( char * str , float * val , int * dcode ) ;

static int   INIT_ngray ;
static float INIT_gamma ;
static float INIT_fov ;

#endif /* _MCW_TO3D_HEADER_ */
