/**************************************************/
/*! Stuff to make DLLs work as plugins on CYGWIN. */
/**************************************************/

#ifdef CYGWIN   /**** only use this stuff in CYGWIN ****/

#include "mri_render.h"
#include "mcw_graf.h"
#include "parser.h"

#ifdef BG_INSIDE_AFNI  /*--- this stuff is actuated in AFNI itself ---*/

#define NUM_BG_array (sizeof(BG_array)/sizeof(void *))

static void *BG_array[] = {        /* Array of pointers to functions */
    (void *)AFNI_add_interruptable ,       /* and data inside AFNI that some */
    (void *)AFNI_can_transform_vector ,    /* plugin needs to do its job.    */
    (void *)AFNI_controller_index ,        /* If you add something here, be  */
    (void *)AFNI_controller_label ,        /* sure to add it in the stuff    */
    (void *)AFNI_force_adoption ,          /* below, as well.                */
    (void *)AFNI_hintize_pbar ,
    (void *)AFNI_initialize_view ,
    (void *)AFNI_jumpto_dicom ,
    (void *)AFNI_make_descendants ,
    (void *)AFNI_modify_viewing ,
    (void *)AFNI_needs_dset_ijk ,
    (void *)AFNI_palette_label_CB ,
    (void *)AFNI_process_drawnotice ,
    (void *)AFNI_process_interrupts ,
    (void *)AFNI_purge_unused_dsets ,
    (void *)AFNI_range_rotate_av_CB ,
    (void *)AFNI_receive_control ,
    (void *)AFNI_receive_init ,
    (void *)AFNI_register_nD_function ,
    (void *)AFNI_set_cursor ,
    (void *)AFNI_transform_vector ,
    (void *)AV_assign_fval ,
    (void *)AV_assign_ival ,
    (void *)AV_colsize ,
    (void *)AV_format_fval ,
    (void *)AV_fval_to_char ,
    (void *)DC_rgb_to_ovrgb ,
    (void *)DC_yokify ,
    (void *)ENV_add_numeric ,
    (void *)GRAF_get_setup ,
    (void *)GRAF_put_setup ,
    (void *)MCW_action_area ,
    (void *)MCW_alter_widget_cursor ,
    (void *)MCW_av_substring_CB ,
    (void *)MCW_choose_integer ,
    (void *)MCW_choose_ovcolor ,
    (void *)MCW_choose_string ,
    (void *)MCW_choose_strlist ,
    (void *)MCW_expose_widget ,
    (void *)MCW_histo_bytes ,
    (void *)MCW_invert_widget ,
    (void *)MCW_pbar_to_mri ,
    (void *)MCW_popdown_meter ,
    (void *)MCW_popup_message ,
    (void *)MCW_popup_meter ,
    (void *)MCW_reghelp_children ,
    (void *)MCW_reghint_children ,
    (void *)MCW_register_help ,
    (void *)MCW_register_hint ,
    (void *)MCW_set_bbox ,
    (void *)MCW_set_meter ,
    (void *)MCW_set_widget_label ,
    (void *)MCW_val_bbox ,
    (void *)MCW_widget_geom ,
    (void *)PARSER_evaluate_one ,
    (void *)PARSER_evaluate_vector ,
    (void *)PARSER_generate_code ,
    (void *)PCOR_get_coef ,
    (void *)PCOR_get_pcor ,
    (void *)PCOR_update_float ,
    (void *)PLUTO_4D_to_typed_fim ,
    (void *)PLUTO_add_dset ,
    (void *)PLUTO_add_hint ,
    (void *)PLUTO_beep ,
    (void *)PLUTO_commandstring ,
    (void *)PLUTO_copy_dset ,
    (void *)PLUTO_cpu_time ,
    (void *)PLUTO_dset_redisplay ,
    (void *)PLUTO_dset_redisplay_mode ,
    (void *)PLUTO_elapsed_time ,
    (void *)PLUTO_find_dset ,
    (void *)PLUTO_fixup_names ,
    (void *)PLUTO_force_redisplay ,
    (void *)PLUTO_histoplot ,
    (void *)PLUTO_imseq_addto ,
    (void *)PLUTO_imseq_popim ,
    (void *)PLUTO_imseq_rekill ,
    (void *)PLUTO_imseq_retitle ,
    (void *)PLUTO_popdown_meter ,
    (void *)PLUTO_popup_dset_chooser ,
    (void *)PLUTO_popup_meter ,
    (void *)PLUTO_popup_worker ,
    (void *)PLUTO_prefix_ok ,
    (void *)PLUTO_register_timeout ,
    (void *)PLUTO_register_timeseries ,
    (void *)PLUTO_register_workproc ,
    (void *)PLUTO_report ,
    (void *)PLUTO_scatterplot ,
    (void *)PLUTO_set_butcolor ,
    (void *)PLUTO_set_meter ,
    (void *)PLUTO_set_sequence ,
    (void *)PLUTO_set_topshell ,
    (void *)PLUTO_string_index ,
    (void *)PLUTO_turnoff_options ,
    (void *)RWC_visibilize_CB ,
    (void *)RWC_visibilize_widget ,
    (void *)TTRR_get_params ,
    (void *)add_dataset_to_PLUGIN_interface ,
    (void *)add_number_to_PLUGIN_interface ,
    (void *)add_option_to_PLUGIN_interface ,
    (void *)add_string_to_PLUGIN_interface ,
    (void *)add_timeseries_to_PLUGIN_interface ,
    (void *)alter_MCW_pbar ,
    (void *)alter_PLUGIN_strval_width ,
    (void *)drive_MCW_grapher ,
    (void *)drive_MCW_imseq ,
    (void *)free_PCOR_references ,
    (void *)free_PCOR_voxel_corr ,
    (void *)get_PLUGIN_strval ,
    (void *)get_idcode_from_PLUGIN_interface ,
    (void *)get_number_from_PLUGIN_interface ,
    (void *)get_optiontag_from_PLUGIN_interface ,
    (void *)get_string_from_PLUGIN_interface ,
    (void *)get_timeseries_from_PLUGIN_interface ,
    (void *)load_PBAR_palette_array ,
    (void *)make_PLUGIN_dataset_link ,
    (void *)new_MCW_arrowval ,
    (void *)new_MCW_bbox ,
    (void *)new_MCW_colormenu ,
    (void *)new_MCW_graf ,
    (void *)new_MCW_optmenu ,
    (void *)new_MCW_pasgraf ,
    (void *)new_MCW_pbar ,
    (void *)new_MCW_textwin ,
    (void *)new_PCOR_references ,
    (void *)new_PCOR_voxel_corr ,
    (void *)new_PLUGIN_interface_1999 ,
    (void *)new_PLUGIN_strval ,
    (void *)open_MCW_imseq ,
    (void *)peek_optiontag_from_PLUGIN_interface ,
    (void *)redraw_MCW_pasgraf ,
    (void *)refit_MCW_optmenu ,
    (void *)set_MCW_pasgraf ,
    (void *)update_MCW_pbar ,
    (void *)update_PCOR_references ,
    (void *)THD_coorder_fill ,

    (void *)&GLOBAL_library ,           /* AFNI global data */
    (void *)&GLOBAL_argopt ,
    (void *)&afni48_pixmap ,
    (void *)&afni48_good ,
    (void *) TTO_list ,                 /* note: no '&' */
NULL } ;

/** Send the array of pointers from AFNI to a plugin **/

static int BG_array_send( DYNAMIC_handle handle )
{
   void (*BG_array_receive)() ;

   DYNAMIC_SYMBOL( handle , "BG_array_receive" , BG_array_receive ) ;
   if( BG_array_receive == NULL ){
     fprintf(stderr,"CYGWIN-ize: can't find symbol BG_array_receive()\n") ;
     return -1 ;
   }

   BG_array_receive( NUM_BG_array , BG_array ) ;
   return 0 ;
}

#else                  /*--- this stuff is actuated in each plugin ---*/

/**** Variables to mask the AFNI global functions called from plugins. ****/

static Boolean (*BG_drive_MCW_grapher)( MCW_grapher * , int , XtPointer ) ;
static Boolean (*BG_drive_MCW_imseq)( MCW_imseq * , int , XtPointer ) ;
static MCW_arrowval * (*BG_new_MCW_arrowval)( Widget , char * , int,int,int,int,int,int , gen_func * , XtPointer , str_func * , XtPointer ) ;
static MCW_arrowval * (*BG_new_MCW_colormenu)( Widget , char * label , MCW_DC * , int , int , int , gen_func * , XtPointer ) ;
static MCW_arrowval * (*BG_new_MCW_optmenu)( Widget , char * , int,int,int,int, gen_func * , XtPointer , str_func * , XtPointer ) ;
static MCW_bbox * (*BG_new_MCW_bbox)( Widget , int , char * lab[] , int , int , XtCallbackProc , XtPointer ) ;
static MCW_graf * (*BG_new_MCW_graf)( Widget wpar , MCW_DC * , char * title , gen_func * cbfunc , void * cbdata ) ;
static MCW_idcode * (*BG_get_idcode_from_PLUGIN_interface)( PLUGIN_interface * ) ;
static MCW_imseq * (*BG_open_MCW_imseq)( MCW_DC * , get_ptr , XtPointer ) ;
static MCW_pasgraf * (*BG_new_MCW_pasgraf)( Widget wpar , MCW_DC * dc , char * title ) ;
static MCW_pbar * (*BG_new_MCW_pbar)( Widget , MCW_DC * , int,int , float,float , gen_func * , XtPointer ) ;
static MCW_textwin * (*BG_new_MCW_textwin)( Widget, char *, int ) ;
static MRI_IMAGE * (*BG_MCW_pbar_to_mri)( MCW_pbar *,int,int ) ;
static MRI_IMAGE * (*BG_get_timeseries_from_PLUGIN_interface)( PLUGIN_interface * ) ;
static PARSER_code * (*BG_PARSER_generate_code)( char * ) ;
static PCOR_references * (*BG_new_PCOR_references)(int) ;
static PCOR_voxel_corr * (*BG_new_PCOR_voxel_corr)(int,int) ;
static PLUGIN_interface * (*BG_new_PLUGIN_interface_1999)( char *, char *, char *, int, cptr_func * , char * ) ;
static PLUGIN_strval * (*BG_new_PLUGIN_strval)( Widget , char * ) ;
static THD_3dim_dataset * (*BG_PLUTO_4D_to_typed_fim)( THD_3dim_dataset * old_dset , char * new_prefix , int new_datum , int ignore , int detrend , generic_func * user_func , void * user_data ) ;
static THD_3dim_dataset * (*BG_PLUTO_copy_dset)( THD_3dim_dataset *, char * ) ;
static THD_3dim_dataset * (*BG_PLUTO_find_dset)( MCW_idcode * ) ;
static THD_fvec3 (*BG_AFNI_transform_vector)( THD_3dim_dataset * , THD_fvec3 , THD_3dim_dataset * ) ;
static TTRR_params * (*BG_TTRR_get_params)(void) ;
static Widget (*BG_MCW_action_area)( Widget , MCW_action_item * , int ) ;
static Widget (*BG_MCW_popup_message)( Widget , char * , int ) ;
static Widget (*BG_MCW_popup_meter)( Widget , int ) ;
static char * (*BG_AFNI_controller_label)( Three_D_View * im3d );
static char * (*BG_AFNI_palette_label_CB)( MCW_arrowval * , XtPointer ) ;
static char * (*BG_AV_format_fval)( float ) ;
static char * (*BG_MCW_av_substring_CB)( MCW_arrowval * , XtPointer ) ;
static char * (*BG_PLUTO_commandstring)( PLUGIN_interface * plint ) ;
static char * (*BG_get_PLUGIN_strval)( PLUGIN_strval * ) ;
static char * (*BG_get_optiontag_from_PLUGIN_interface)( PLUGIN_interface * ) ;
static char * (*BG_get_string_from_PLUGIN_interface)( PLUGIN_interface * ) ;
static char * (*BG_peek_optiontag_from_PLUGIN_interface)( PLUGIN_interface * ) ;
static double (*BG_PARSER_evaluate_one)( PARSER_code *, double atoz[] ) ;
static double (*BG_PLUTO_cpu_time)(void) ;
static double (*BG_PLUTO_elapsed_time)(void) ;
static float  (*BG_get_number_from_PLUGIN_interface)( PLUGIN_interface * ) ;
static int  (*BG_AFNI_jumpto_dicom)( Three_D_View * , float, float, float  ) ;
static int  (*BG_AFNI_needs_dset_ijk)(void) ;
static int  (*BG_MCW_val_bbox)( MCW_bbox * ) ;
static int (*BG_AFNI_can_transform_vector)( THD_3dim_dataset *, THD_3dim_dataset * );
static int (*BG_AFNI_controller_index)( Three_D_View * ) ;
static int (*BG_AFNI_receive_control)( Three_D_View *, int,int, void * ) ;
static int (*BG_AFNI_receive_init)( Three_D_View *, int, gen_func * , void * ) ;
static int (*BG_AV_colsize)() ;
static int (*BG_PLUTO_add_dset)( PLUGIN_interface *, THD_3dim_dataset *, int ) ;
static int (*BG_PLUTO_prefix_ok)( char * ) ;
static int (*BG_PLUTO_string_index)( char * , int , char ** ) ;
static void   (*BG_MCW_choose_integer)( Widget , char * , int,int,int , gen_func *, XtPointer );
static void   (*BG_MCW_choose_ovcolor)( Widget , MCW_DC * , int , gen_func * , XtPointer ) ;
static void   (*BG_MCW_choose_string)( Widget, char *, char *, gen_func *, XtPointer );
static void   (*BG_MCW_choose_strlist)( Widget, char *, int, int, char * strlist[], gen_func *, XtPointer );
static void   (*BG_PLUTO_imseq_addto)( void * , MRI_IMAGE * ) ;
static void   (*BG_PLUTO_imseq_rekill)( void *, generic_func *, void * ) ;
static void   (*BG_PLUTO_imseq_retitle)( void * , char * ) ;
static void * (*BG_PLUTO_imseq_popim)( MRI_IMAGE *, generic_func *, void * ) ;
static void (*BG_AFNI_add_interruptable)( Widget ) ;
static void (*BG_AFNI_force_adoption)( THD_session * , Boolean ) ;
static void (*BG_AFNI_hintize_pbar)( MCW_pbar * ,  float ) ;
static void (*BG_AFNI_initialize_view)( THD_3dim_dataset * , Three_D_View * ) ;
static void (*BG_AFNI_make_descendants)( THD_sessionlist * ) ;
static void (*BG_AFNI_modify_viewing)( Three_D_View * , Boolean ) ;
static void (*BG_AFNI_process_drawnotice)( Three_D_View * ) ;
static void (*BG_AFNI_process_interrupts)( Widget ) ;
static void (*BG_AFNI_purge_unused_dsets)(void) ;
static void (*BG_AFNI_range_rotate_av_CB)( MCW_arrowval *, XtPointer );
static void (*BG_AFNI_register_nD_function)( int , char * , generic_func * , int ) ;
static void (*BG_AFNI_set_cursor)( int ) ;
static void (*BG_AV_assign_fval)( MCW_arrowval * , float ) ;
static void (*BG_AV_assign_ival)( MCW_arrowval * , int ) ;
static void (*BG_AV_fval_to_char)( float , char * ) ;
static void (*BG_DC_rgb_to_ovrgb)( MCW_DC *, int,int *,int,byte *, byte *, byte *) ;
static void (*BG_DC_yokify)( Widget , MCW_DC * ) ;
static void (*BG_ENV_add_numeric)( char * , char * , int , int , int , int , generic_func * ) ;
static void (*BG_GRAF_get_setup)( MCW_graf * , int * , int * , int * , int * ) ;
static void (*BG_GRAF_put_setup)( MCW_graf * , int   , int * , int * , int   ) ;
static void (*BG_MCW_alter_widget_cursor)( Widget,int , char * , char * ) ;
static void (*BG_MCW_expose_widget)( Widget ) ;
static void (*BG_MCW_histo_bytes)( int nb , byte * bar , int * har ) ;
static void (*BG_MCW_invert_widget)( Widget ) ;
static void (*BG_MCW_popdown_meter)( Widget ) ;
static void (*BG_MCW_reghelp_children)( Widget , char * ) ;
static void (*BG_MCW_reghint_children)( Widget , char * ) ;
static void (*BG_MCW_register_help)( Widget , char * ) ;
static void (*BG_MCW_register_hint)( Widget , char * ) ;
static void (*BG_MCW_set_bbox)( MCW_bbox * , int ) ;
static void (*BG_MCW_set_meter)( Widget , int ) ;
static void (*BG_MCW_set_widget_label)( Widget , char * ) ;
static void (*BG_MCW_widget_geom)( Widget , int * , int * , int * , int * ) ;
static void (*BG_PARSER_evaluate_vector)( PARSER_code * pc, double  * atoz[], int nv, double vout[] ) ;
static void (*BG_PCOR_get_coef)(PCOR_references *, PCOR_voxel_corr *, float *) ;
static void (*BG_PCOR_get_pcor)(PCOR_references *, PCOR_voxel_corr *, float *) ;
static void (*BG_PCOR_update_float)( float * , PCOR_references * , PCOR_voxel_corr * ) ;
static void (*BG_PLUTO_add_hint)( PLUGIN_interface * , char * ) ;
static void (*BG_PLUTO_beep)(void) ;
static void (*BG_PLUTO_dset_redisplay)( THD_3dim_dataset * ) ;
static void (*BG_PLUTO_dset_redisplay_mode)( THD_3dim_dataset * , int ) ;
static void (*BG_PLUTO_fixup_names)(void) ;
static void (*BG_PLUTO_force_redisplay)( void ) ;
static void (*BG_PLUTO_histoplot)( int, float, float, int *, char *, char *, char * , int,int ** ) ;
static void (*BG_PLUTO_popdown_meter)( PLUGIN_interface * ) ;
static void (*BG_PLUTO_popup_dset_chooser)( Widget, int, int, int_func *, void_func *, void * ) ;
static void (*BG_PLUTO_popup_meter)( PLUGIN_interface * ) ;
static void (*BG_PLUTO_popup_worker)( PLUGIN_interface * , char * , int ) ;
static void (*BG_PLUTO_register_timeout)( int, generic_func *, XtPointer ) ;
static void (*BG_PLUTO_register_timeseries)( char * , MRI_IMAGE * ) ;
static void (*BG_PLUTO_register_workproc)( XtWorkProc , XtPointer ) ;
static void (*BG_PLUTO_report)( PLUGIN_interface * , char * ) ;
static void (*BG_PLUTO_scatterplot)( int , float *, float *, char *, char *, char * ) ;
static void (*BG_PLUTO_set_butcolor)( PLUGIN_interface *, char * ) ;
static void (*BG_PLUTO_set_meter)( PLUGIN_interface * , int ) ;
static void (*BG_PLUTO_set_sequence)( PLUGIN_interface *, char * ) ;
static void (*BG_PLUTO_set_topshell)( PLUGIN_interface *, Widget ) ;
static void (*BG_PLUTO_turnoff_options)( PLUGIN_interface * ) ;
static void (*BG_RWC_visibilize_CB)( Widget , XtPointer , XtPointer ) ;
static void (*BG_RWC_visibilize_widget)( Widget ) ;
static void (*BG_add_dataset_to_PLUGIN_interface)( PLUGIN_interface *, char *, int,int,int ) ;
static void (*BG_add_number_to_PLUGIN_interface)( PLUGIN_interface *, char *, int, int, int, int, int ) ;
static void (*BG_add_option_to_PLUGIN_interface)( PLUGIN_interface *, char *, char *, int ) ;
static void (*BG_add_string_to_PLUGIN_interface)( PLUGIN_interface *, char *, int, char **, int) ;
static void (*BG_add_timeseries_to_PLUGIN_interface)( PLUGIN_interface *, char * ) ;
static void (*BG_alter_MCW_pbar)( MCW_pbar * , int , float * ) ;
static void (*BG_alter_PLUGIN_strval_width)( PLUGIN_strval * , int ) ;
static void (*BG_free_PCOR_references)(PCOR_references *) ;
static void (*BG_free_PCOR_voxel_corr)(PCOR_voxel_corr *) ;
static void (*BG_load_PBAR_palette_array)( MCW_pbar * , PBAR_palette_array * , int ) ;
static void (*BG_make_PLUGIN_dataset_link)( THD_3dim_dataset *, PLUGIN_dataset_link * ) ;
static void (*BG_redraw_MCW_pasgraf)( MCW_pasgraf * gp ) ;
static void (*BG_refit_MCW_optmenu)( MCW_arrowval * , int,int,int,int, str_func * , XtPointer ) ;
static void (*BG_set_MCW_pasgraf)( MCW_pasgraf * gp , byte * func ) ;
static void (*BG_update_MCW_pbar)( MCW_pbar * ) ;
static void (*BG_update_PCOR_references)(float *,PCOR_references *) ;
static void (*BG_THD_coorder_fill)( char * , THD_coorder * ) ;

static AFNI_library_type *BG_GLOBAL_library ;    /* AFNI global data */
static AF_options *BG_GLOBAL_argopt ;
static Pixmap *BG_afni48_pixmap ;
static int *BG_afni48_good ;
static TTO_point *BG_TTO_list ;

#define NUM_BG_array (sizeof(BG_array)/sizeof(void *))

static void **BG_array[] = {
    (void **) &BG_AFNI_add_interruptable ,       /* AFNI global funcs */
    (void **) &BG_AFNI_can_transform_vector ,
    (void **) &BG_AFNI_controller_index ,
    (void **) &BG_AFNI_controller_label ,
    (void **) &BG_AFNI_force_adoption ,
    (void **) &BG_AFNI_hintize_pbar ,
    (void **) &BG_AFNI_initialize_view ,
    (void **) &BG_AFNI_jumpto_dicom ,
    (void **) &BG_AFNI_make_descendants ,
    (void **) &BG_AFNI_modify_viewing ,
    (void **) &BG_AFNI_needs_dset_ijk ,
    (void **) &BG_AFNI_palette_label_CB ,
    (void **) &BG_AFNI_process_drawnotice ,
    (void **) &BG_AFNI_process_interrupts ,
    (void **) &BG_AFNI_purge_unused_dsets ,
    (void **) &BG_AFNI_range_rotate_av_CB ,
    (void **) &BG_AFNI_receive_control ,
    (void **) &BG_AFNI_receive_init ,
    (void **) &BG_AFNI_register_nD_function ,
    (void **) &BG_AFNI_set_cursor ,
    (void **) &BG_AFNI_transform_vector ,
    (void **) &BG_AV_assign_fval ,
    (void **) &BG_AV_assign_ival ,
    (void **) &BG_AV_colsize ,
    (void **) &BG_AV_format_fval ,
    (void **) &BG_AV_fval_to_char ,
    (void **) &BG_DC_rgb_to_ovrgb ,
    (void **) &BG_DC_yokify ,
    (void **) &BG_ENV_add_numeric ,
    (void **) &BG_GRAF_get_setup ,
    (void **) &BG_GRAF_put_setup ,
    (void **) &BG_MCW_action_area ,
    (void **) &BG_MCW_alter_widget_cursor ,
    (void **) &BG_MCW_av_substring_CB ,
    (void **) &BG_MCW_choose_integer ,
    (void **) &BG_MCW_choose_ovcolor ,
    (void **) &BG_MCW_choose_string ,
    (void **) &BG_MCW_choose_strlist ,
    (void **) &BG_MCW_expose_widget ,
    (void **) &BG_MCW_histo_bytes ,
    (void **) &BG_MCW_invert_widget ,
    (void **) &BG_MCW_pbar_to_mri ,
    (void **) &BG_MCW_popdown_meter ,
    (void **) &BG_MCW_popup_message ,
    (void **) &BG_MCW_popup_meter ,
    (void **) &BG_MCW_reghelp_children ,
    (void **) &BG_MCW_reghint_children ,
    (void **) &BG_MCW_register_help ,
    (void **) &BG_MCW_register_hint ,
    (void **) &BG_MCW_set_bbox ,
    (void **) &BG_MCW_set_meter ,
    (void **) &BG_MCW_set_widget_label ,
    (void **) &BG_MCW_val_bbox ,
    (void **) &BG_MCW_widget_geom ,
    (void **) &BG_PARSER_evaluate_one ,
    (void **) &BG_PARSER_evaluate_vector ,
    (void **) &BG_PARSER_generate_code ,
    (void **) &BG_PCOR_get_coef ,
    (void **) &BG_PCOR_get_pcor ,
    (void **) &BG_PCOR_update_float ,
    (void **) &BG_PLUTO_4D_to_typed_fim ,
    (void **) &BG_PLUTO_add_dset ,
    (void **) &BG_PLUTO_add_hint ,
    (void **) &BG_PLUTO_beep ,
    (void **) &BG_PLUTO_commandstring ,
    (void **) &BG_PLUTO_copy_dset ,
    (void **) &BG_PLUTO_cpu_time ,
    (void **) &BG_PLUTO_dset_redisplay ,
    (void **) &BG_PLUTO_dset_redisplay_mode ,
    (void **) &BG_PLUTO_elapsed_time ,
    (void **) &BG_PLUTO_find_dset ,
    (void **) &BG_PLUTO_fixup_names ,
    (void **) &BG_PLUTO_force_redisplay ,
    (void **) &BG_PLUTO_histoplot ,
    (void **) &BG_PLUTO_imseq_addto ,
    (void **) &BG_PLUTO_imseq_popim ,
    (void **) &BG_PLUTO_imseq_rekill ,
    (void **) &BG_PLUTO_imseq_retitle ,
    (void **) &BG_PLUTO_popdown_meter ,
    (void **) &BG_PLUTO_popup_dset_chooser ,
    (void **) &BG_PLUTO_popup_meter ,
    (void **) &BG_PLUTO_popup_worker ,
    (void **) &BG_PLUTO_prefix_ok ,
    (void **) &BG_PLUTO_register_timeout ,
    (void **) &BG_PLUTO_register_timeseries ,
    (void **) &BG_PLUTO_register_workproc ,
    (void **) &BG_PLUTO_report ,
    (void **) &BG_PLUTO_scatterplot ,
    (void **) &BG_PLUTO_set_butcolor ,
    (void **) &BG_PLUTO_set_meter ,
    (void **) &BG_PLUTO_set_sequence ,
    (void **) &BG_PLUTO_set_topshell ,
    (void **) &BG_PLUTO_string_index ,
    (void **) &BG_PLUTO_turnoff_options ,
    (void **) &BG_RWC_visibilize_CB ,
    (void **) &BG_RWC_visibilize_widget ,
    (void **) &BG_TTRR_get_params ,
    (void **) &BG_add_dataset_to_PLUGIN_interface ,
    (void **) &BG_add_number_to_PLUGIN_interface ,
    (void **) &BG_add_option_to_PLUGIN_interface ,
    (void **) &BG_add_string_to_PLUGIN_interface ,
    (void **) &BG_add_timeseries_to_PLUGIN_interface ,
    (void **) &BG_alter_MCW_pbar ,
    (void **) &BG_alter_PLUGIN_strval_width ,
    (void **) &BG_drive_MCW_grapher ,
    (void **) &BG_drive_MCW_imseq ,
    (void **) &BG_free_PCOR_references ,
    (void **) &BG_free_PCOR_voxel_corr ,
    (void **) &BG_get_PLUGIN_strval ,
    (void **) &BG_get_idcode_from_PLUGIN_interface ,
    (void **) &BG_get_number_from_PLUGIN_interface ,
    (void **) &BG_get_optiontag_from_PLUGIN_interface ,
    (void **) &BG_get_string_from_PLUGIN_interface ,
    (void **) &BG_get_timeseries_from_PLUGIN_interface ,
    (void **) &BG_load_PBAR_palette_array ,
    (void **) &BG_make_PLUGIN_dataset_link ,
    (void **) &BG_new_MCW_arrowval ,
    (void **) &BG_new_MCW_bbox ,
    (void **) &BG_new_MCW_colormenu ,
    (void **) &BG_new_MCW_graf ,
    (void **) &BG_new_MCW_optmenu ,
    (void **) &BG_new_MCW_pasgraf ,
    (void **) &BG_new_MCW_pbar ,
    (void **) &BG_new_MCW_textwin ,
    (void **) &BG_new_PCOR_references ,
    (void **) &BG_new_PCOR_voxel_corr ,
    (void **) &BG_new_PLUGIN_interface_1999 ,
    (void **) &BG_new_PLUGIN_strval ,
    (void **) &BG_open_MCW_imseq ,
    (void **) &BG_peek_optiontag_from_PLUGIN_interface ,
    (void **) &BG_redraw_MCW_pasgraf ,
    (void **) &BG_refit_MCW_optmenu ,
    (void **) &BG_set_MCW_pasgraf ,
    (void **) &BG_update_MCW_pbar ,
    (void **) &BG_update_PCOR_references ,
    (void **) &BG_THD_coorder_fill ,

    (void **) &BG_GLOBAL_library ,    /* AFNI global data */
    (void **) &BG_GLOBAL_argopt ,
    (void **) &BG_afni48_pixmap ,
    (void **) &BG_afni48_good ,
    (void **) &BG_TTO_list ,
NULL } ;

/** Receive the array of global pointers from AFNI **/

void BG_array_receive( int nar , void *ar[] )
{
   int ii ;
   if( nar <= 0 || ar == NULL ) return ;
   if( nar > NUM_BG_array ) nar = NUM_BG_array ;
   for( ii=0 ; ii < NUM_BG_array ; ii++ )
      if( BG_array[ii] != NULL ) *(BG_array[ii]) = ar[ii] ;
   return ;
}

int MAIN__(){ return 0; }  /* for f2c */

/**** Macros to make it possible to call the AFNI global functions
      via the pointers above using the same name as the original function ****/

#define AFNI_add_interruptable BG_AFNI_add_interruptable
#define AFNI_can_transform_vector BG_AFNI_can_transform_vector
#define AFNI_controller_index BG_AFNI_controller_index
#define AFNI_controller_label BG_AFNI_controller_label
#define AFNI_force_adoption BG_AFNI_force_adoption
#define AFNI_hintize_pbar BG_AFNI_hintize_pbar
#define AFNI_initialize_view BG_AFNI_initialize_view
#define AFNI_jumpto_dicom BG_AFNI_jumpto_dicom
#define AFNI_make_descendants BG_AFNI_make_descendants
#define AFNI_modify_viewing BG_AFNI_modify_viewing
#define AFNI_needs_dset_ijk BG_AFNI_needs_dset_ijk
#define AFNI_palette_label_CB BG_AFNI_palette_label_CB
#define AFNI_process_drawnotice BG_AFNI_process_drawnotice
#define AFNI_process_interrupts BG_AFNI_process_interrupts
#define AFNI_purge_unused_dsets BG_AFNI_purge_unused_dsets
#define AFNI_range_rotate_av_CB BG_AFNI_range_rotate_av_CB
#define AFNI_receive_control BG_AFNI_receive_control
#define AFNI_receive_init BG_AFNI_receive_init
#define AFNI_register_nD_function BG_AFNI_register_nD_function
#define AFNI_set_cursor BG_AFNI_set_cursor
#define AFNI_transform_vector BG_AFNI_transform_vector
#define AV_assign_fval BG_AV_assign_fval
#define AV_assign_ival BG_AV_assign_ival
#define AV_colsize BG_AV_colsize
#define AV_format_fval BG_AV_format_fval
#define AV_fval_to_char BG_AV_fval_to_char
#define DC_rgb_to_ovrgb BG_DC_rgb_to_ovrgb
#define DC_yokify BG_DC_yokify
#define ENV_add_numeric BG_ENV_add_numeric
#define GRAF_get_setup BG_GRAF_get_setup
#define GRAF_put_setup BG_GRAF_put_setup
#define MCW_action_area BG_MCW_action_area
#define MCW_alter_widget_cursor BG_MCW_alter_widget_cursor
#define MCW_av_substring_CB BG_MCW_av_substring_CB
#define MCW_choose_integer BG_MCW_choose_integer
#define MCW_choose_ovcolor BG_MCW_choose_ovcolor
#define MCW_choose_string BG_MCW_choose_string
#define MCW_choose_strlist BG_MCW_choose_strlist
#define MCW_expose_widget BG_MCW_expose_widget
#define MCW_histo_bytes BG_MCW_histo_bytes
#define MCW_invert_widget BG_MCW_invert_widget
#define MCW_pbar_to_mri BG_MCW_pbar_to_mri
#define MCW_popdown_meter BG_MCW_popdown_meter
#define MCW_popup_message BG_MCW_popup_message
#define MCW_popup_meter BG_MCW_popup_meter
#define MCW_reghelp_children BG_MCW_reghelp_children
#define MCW_reghint_children BG_MCW_reghint_children
#define MCW_register_help BG_MCW_register_help
#define MCW_register_hint BG_MCW_register_hint
#define MCW_set_bbox BG_MCW_set_bbox
#define MCW_set_meter BG_MCW_set_meter
#define MCW_set_widget_label BG_MCW_set_widget_label
#define MCW_val_bbox BG_MCW_val_bbox
#define MCW_widget_geom BG_MCW_widget_geom
#define PARSER_evaluate_one BG_PARSER_evaluate_one
#define PARSER_evaluate_vector BG_PARSER_evaluate_vector
#define PARSER_generate_code BG_PARSER_generate_code
#define PCOR_get_coef BG_PCOR_get_coef
#define PCOR_get_pcor BG_PCOR_get_pcor
#define PCOR_update_float BG_PCOR_update_float
#define PLUTO_4D_to_typed_fim BG_PLUTO_4D_to_typed_fim
#define PLUTO_add_dset BG_PLUTO_add_dset
#define PLUTO_add_hint BG_PLUTO_add_hint
#define PLUTO_beep BG_PLUTO_beep
#define PLUTO_commandstring BG_PLUTO_commandstring
#define PLUTO_copy_dset BG_PLUTO_copy_dset
#define PLUTO_cpu_time BG_PLUTO_cpu_time
#define PLUTO_dset_redisplay BG_PLUTO_dset_redisplay
#define PLUTO_dset_redisplay_mode BG_PLUTO_dset_redisplay_mode
#define PLUTO_elapsed_time BG_PLUTO_elapsed_time
#define PLUTO_find_dset BG_PLUTO_find_dset
#define PLUTO_fixup_names BG_PLUTO_fixup_names
#define PLUTO_force_redisplay BG_PLUTO_force_redisplay
#define PLUTO_histoplot BG_PLUTO_histoplot
#define PLUTO_imseq_addto BG_PLUTO_imseq_addto
#define PLUTO_imseq_popim BG_PLUTO_imseq_popim
#define PLUTO_imseq_rekill BG_PLUTO_imseq_rekill
#define PLUTO_imseq_retitle BG_PLUTO_imseq_retitle
#define PLUTO_popdown_meter BG_PLUTO_popdown_meter
#define PLUTO_popup_dset_chooser BG_PLUTO_popup_dset_chooser
#define PLUTO_popup_meter BG_PLUTO_popup_meter
#define PLUTO_popup_worker BG_PLUTO_popup_worker
#define PLUTO_prefix_ok BG_PLUTO_prefix_ok
#define PLUTO_register_timeout BG_PLUTO_register_timeout
#define PLUTO_register_timeseries BG_PLUTO_register_timeseries
#define PLUTO_register_workproc BG_PLUTO_register_workproc
#define PLUTO_report BG_PLUTO_report
#define PLUTO_scatterplot BG_PLUTO_scatterplot
#define PLUTO_set_butcolor BG_PLUTO_set_butcolor
#define PLUTO_set_meter BG_PLUTO_set_meter
#define PLUTO_set_sequence BG_PLUTO_set_sequence
#define PLUTO_set_topshell BG_PLUTO_set_topshell
#define PLUTO_string_index BG_PLUTO_string_index
#define PLUTO_turnoff_options BG_PLUTO_turnoff_options
#define RWC_visibilize_CB BG_RWC_visibilize_CB
#define RWC_visibilize_widget BG_RWC_visibilize_widget
#define TTRR_get_params BG_TTRR_get_params
#define add_dataset_to_PLUGIN_interface BG_add_dataset_to_PLUGIN_interface
#define add_number_to_PLUGIN_interface BG_add_number_to_PLUGIN_interface
#define add_option_to_PLUGIN_interface BG_add_option_to_PLUGIN_interface
#define add_string_to_PLUGIN_interface BG_add_string_to_PLUGIN_interface
#define add_timeseries_to_PLUGIN_interface BG_add_timeseries_to_PLUGIN_interface
#define alter_MCW_pbar BG_alter_MCW_pbar
#define alter_PLUGIN_strval_width BG_alter_PLUGIN_strval_width
#define drive_MCW_grapher BG_drive_MCW_grapher
#define drive_MCW_imseq BG_drive_MCW_imseq
#define free_PCOR_references BG_free_PCOR_references
#define free_PCOR_voxel_corr BG_free_PCOR_voxel_corr
#define get_PLUGIN_strval BG_get_PLUGIN_strval
#define get_idcode_from_PLUGIN_interface BG_get_idcode_from_PLUGIN_interface
#define get_number_from_PLUGIN_interface BG_get_number_from_PLUGIN_interface
#define get_optiontag_from_PLUGIN_interface BG_get_optiontag_from_PLUGIN_interface
#define get_string_from_PLUGIN_interface BG_get_string_from_PLUGIN_interface
#define get_timeseries_from_PLUGIN_interface BG_get_timeseries_from_PLUGIN_interface
#define load_PBAR_palette_array BG_load_PBAR_palette_array
#define make_PLUGIN_dataset_link BG_make_PLUGIN_dataset_link
#define new_MCW_arrowval BG_new_MCW_arrowval
#define new_MCW_bbox BG_new_MCW_bbox
#define new_MCW_colormenu BG_new_MCW_colormenu
#define new_MCW_graf BG_new_MCW_graf
#define new_MCW_optmenu BG_new_MCW_optmenu
#define new_MCW_pasgraf BG_new_MCW_pasgraf
#define new_MCW_pbar BG_new_MCW_pbar
#define new_MCW_textwin BG_new_MCW_textwin
#define new_PCOR_references BG_new_PCOR_references
#define new_PCOR_voxel_corr BG_new_PCOR_voxel_corr
#define new_PLUGIN_interface_1999 BG_new_PLUGIN_interface_1999
#define new_PLUGIN_strval BG_new_PLUGIN_strval
#define open_MCW_imseq BG_open_MCW_imseq
#define peek_optiontag_from_PLUGIN_interface BG_peek_optiontag_from_PLUGIN_interface
#define redraw_MCW_pasgraf BG_redraw_MCW_pasgraf
#define refit_MCW_optmenu BG_refit_MCW_optmenu
#define set_MCW_pasgraf BG_set_MCW_pasgraf
#define update_MCW_pbar BG_update_MCW_pbar
#define update_PCOR_references BG_update_PCOR_references
#define THD_coorder_fill BG_THD_coorder_fill

/** Access global AFNI data this roundabout way.  The data is passed as
    pointers, so we need to dereference them (unless they are pointers anyhoo). **/

#define GLOBAL_library   (*BG_GLOBAL_library)
#define GLOBAL_argopt    (*BG_GLOBAL_argopt)
#define afni48_pixmap    (*BG_afni48_pixmap)
#define afni48_good      (*BG_afni48_good)
#define TTO_list         (BG_TTO_list)           /* already a pointer */

#endif /* BG_INSIDE_AFNI */
#endif /* CYGWIN */
