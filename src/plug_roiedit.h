/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/***********************************************************************
 * plug_roiedit.h		- see plug_roiedit.c v1.7
 ***********************************************************************
*/

#define rWARNING( string ) fprintf( stderr, "\033[1m%s\033[0m\n", string )
#define rERROR( string ) fprintf( stderr, "\007\033[2m%s\033[0m\n", string )

#define R_BOUND_VAL    -250
#define R_HL_BAD_VAL   -280
#define R_BUTTON_SPACE    4
#define R_DIST_STEP     0.1
#define R_FILE_L	128

#ifdef LINUX

#define R_HOSTID_MASK	0xffff00ff
#define R_HOSTID_VAL	0x6a8d006a

#else

#define R_HOSTID_MASK	0xffffff00
#define R_HOSTID_VAL	0x8d6a6a00

#endif

typedef struct
{
    int   x, y, z;
} r_ipt_t;

typedef struct
{
    int * points;
    int   used;
    int   M;
} points_t;

typedef struct
{
    Display * display;

    /* main widgets */

    Widget    main;
    Widget    mainForm;

    int       main_is_open;

    Widget    save_as_file_d;	/* dialog with save as info           */

    /* white/grey widgets */

    Widget    wtgr_main;        /* main white/grey widget */
    Widget    wtgr_mainRC;      /* main row/column widget */

    Widget    wt_diag_conn_w;   /* widget for diagonal connect level  */
    Widget    wt_fill_val_w;    /* widget for user fill value         */
    Widget    wt_min_nbrs_w;    /* widget for minimum neighbors value */
    Widget    wt_range_min_w;   /* minimum value in search range      */
    Widget    wt_range_max_w;   /* maximum value in search range      */

    Widget    gr_range_min_w;   /* minimum value in search range      */
    Widget    gr_range_max_w;   /* maximum value in search range      */
    Widget    gr_fill_val_w;    /* widget for user fill value         */

    XmStringCharSet charset;
} r_X_s;

typedef struct
{
    /* general */
    int     point_value;        /* value at initial fill point */
    int     point_coord;        /* location of initial fill point */
    int     adjust_point;       /* boolean - should we adjust initial point */

    char    save_as_name[ R_FILE_L ];	/* string to save a copy as      */


    /* white matter filler info */
    int     wt_fill_val;                /* value to use in fill process  */
    int     wt_range_min;
    int     wt_range_max;
    int     wt_diag_connect;            /* level of diagonal connection */

    /* grey matter filler info */
    int     gr_fill_val;                /* value to use in fill process  */
    int     gr_range_min;
    int     gr_range_max;

    int     gr_max_dist;                /* max distance in grey search */


    /* afni */
    THD_3dim_dataset * anat;    /* background anatomy data for searching  */
    THD_3dim_dataset * func;    /* foreground functional data for display */

    short * adata;              /* pointer to anatomical background data */
    short * fdata;              /* pointer to functional foreground data */
    float   factor;

    int     nx, ny, nz;         /* dimensions of data */
    int     nxy;
    int     nvox;               /* total size of data */


    /* boundary stuff */
    points_t Bold;
    points_t Bnew;
    points_t border;

    short  * neighbors;
    short  * undo_data;		/* always contains previous image 	*/

    int      min_nbrs;
    int      strong_borders;
} r_alg_s;

typedef struct
{
    Widget    main;             /* main application widget */
    Widget    mainRC;           /* main rowcolumn widget   */

    Widget    fillval_w;        /* widget holding integer  */

    int       fill_val;         /* value to fill with      */
    int       afni_undo;        /* flag denoting when afni is undoing a curve */
    points_t  A, B;             /* two point structures for interpolation */
} interp_s;

typedef struct
{
    Widget   main;
    Widget   mainRC;

    Widget   fillval_w;
    Widget   maxsize_w;

    int      max_size;
    int      fill_val;

    points_t wtgr_edge;
    points_t gr_edge;
    points_t filled;
} holes_s;

typedef struct
{
    points_t  plist;
    r_ipt_t   source;
    r_ipt_t   dest;

    int       cur_pt;           /* flag denoting first or second point */
} r_pt_conn_s;


r_X_s       gRX;                /* X structure for RCR                      */
r_alg_s     gRA;                /* algorithm structure                      */
interp_s    gRI;                /* interpolation structure (both X and alg) */
holes_s     gRH;                /* hole filling structure  (both X and alg) */
r_pt_conn_s gRCP;               /* point connection structure               */
char        gRmessage[ 1024 ];  /* error messgae memory                     */


static void     r_afni_set_fill_point    ( int *, r_alg_s * );
static void     r_any_cb_apply           ( Widget, XtPointer, XtPointer );
static void     r_any_cb_fill_stats      ( Widget, XtPointer, XtPointer );
static void     r_any_cb_hide            ( Widget, char *, XtPointer );
static void     r_any_cb_raise           ( Widget, char *, XtPointer );
static void     r_any_cb_undo            ( Widget, XtPointer, XtPointer );
static void     r_any_cb_unfill          ( Widget, XtPointer, XtPointer );
static void     r_main_cb_help           ( Widget, XtPointer, XtPointer );
static void     r_main_cb_quit           ( void );
static void     r_main_cb_show_structs   ( void );
static void     r_main_cb_saveas         ( Widget, int, XtPointer );
static void     r_main_mk_main_shell     ( void );
static void     r_main_mk_save_as_fr	 ( Widget );
static void     r_main_mk_show_buttons   ( void );
static void	r_main_show_alg_vals     ( r_alg_s * );
static void	r_main_show_HOL_vals     ( holes_s * );
static void	r_main_show_INT_vals     ( interp_s * );
static void	r_main_show_pt_conn_vals ( r_pt_conn_s * );

static int      r_add_to_boundary        ( points_t *, int );
static int	r_check_host		 ( void );
static void     r_histogram              ( r_alg_s *, int, int, int );
static r_ipt_t  r_index2pt               ( int, int, int, int );
static double   r_p_distance             ( r_ipt_t, r_ipt_t );
static int	r_save_dataset_as	 ( char *, int );

static void     r_init_afni_vars         ( r_alg_s *, THD_3dim_dataset * );
static int      r_init_Alg_values        ( r_alg_s * );
static int      r_init_holes_vals        ( holes_s * );
static int      r_init_interp_vals       ( interp_s * );
static int      r_init_pt_conn_s         ( r_pt_conn_s * );

static void     r_HL_cb_fill             ( Widget, XtPointer, XtPointer );
static void     r_HL_cb_set_fill_val     ( Widget, XtPointer, XtPointer );
static void     r_HL_cb_set_maxsize      ( Widget, XtPointer, XtPointer );
static int	r_HL_check_neighbors     ( points_t *, int );
static Widget   r_HL_mk_buttons          ( holes_s *, Widget );
static Widget   r_HL_mk_fillval_fr       ( holes_s *, Widget );
static void     r_HL_mk_main_shell       ( holes_s * );
static Widget   r_HL_mk_maxsize_fr       ( holes_s *, Widget );

static void     r_INT_cb_fill            ( Widget, XtPointer, XtPointer );
static void     r_INT_cb_set_fill_val    ( Widget, XtPointer, XtPointer );
static Widget   r_INT_mk_app_buttons     ( interp_s *, Widget );
static Widget   r_INT_mk_fillval_fr      ( interp_s *, Widget );
static void     r_INT_mk_main_shell      ( interp_s * );

static void     r_gr_cb_fill             ( Widget, XtPointer, XtPointer );
static void     r_gr_cb_set_max_dist     ( Widget, XtPointer, XtPointer );
static void     r_gr_cb_set_range        ( Widget, XtPointer, XtPointer );
static void     r_gr_set_fill_val        ( Widget, XtPointer, XtPointer );
static int      r_gr_check_insert        ( r_alg_s *, points_t *, int );
static Widget   r_gr_mk_fill_buttons     ( r_X_s *, Widget );
static Widget   r_gr_mk_fillval_fr       ( r_X_s *, Widget );
static Widget   r_gr_mk_main_frame       ( r_X_s *, Widget );
static void     r_gr_mk_misc_buttons     ( r_X_s *, Widget );
static Widget   r_gr_mk_max_dist_w       ( r_X_s *, Widget );
static Widget   r_gr_mk_range_fr         ( r_X_s *, Widget );

static int      r_wtgr_calc_max_frm_val  ( int value );
static int      r_wtgr_calc_min_frm_val  ( int value );
static void     r_wtgr_cb_suggest_limits ( Widget, XtPointer, XtPointer );
static void     r_wtgr_mk_main_shell     ( r_X_s * );
 
static int      r_wt_bad_ngbr_exists     ( r_alg_s *, int, int );
static void     r_wt_cb_activate_SA      ( void );
static void     r_wt_cb_fill             ( Widget, XtPointer, XtPointer );
static void     r_wt_cb_SB_toggle        ( Widget, XtPointer, XtPointer );
static void     r_wt_cb_set_diag_conn    ( Widget, XtPointer, XtPointer );
static void     r_wt_cb_set_fill_val     ( Widget, XtPointer, XtPointer );
static void     r_wt_cb_set_min_nbrs     ( Widget, XtPointer, XtPointer );
static void     r_wt_cb_set_range        ( Widget, XtPointer, XtPointer );
static int      r_wt_check_insert        ( r_alg_s *, int );
static Widget   r_wt_mk_diag_conn_fr     ( r_X_s *, Widget );
static void     r_wt_mk_fill_buttons     ( r_X_s *, Widget );
static Widget   r_wt_mk_fillval_fr       ( r_X_s *, Widget );
static Widget   r_wt_mk_main_frame       ( r_X_s *, Widget );
static void     r_wt_mk_misc_buttons     ( r_X_s *, Widget );
static Widget   r_wt_mk_nbrs_fr          ( r_X_s *, Widget );
static Widget   r_wt_mk_range_fr         ( r_X_s *, Widget );
static Widget   r_wt_mk_strong_bord_fr   ( r_X_s *, Widget );
static void     r_wt_set_neighbors       ( r_alg_s * );

static Widget   r_mk_scale_bar           ( Widget, char *, int, int,
                                          int, int, XtCallbackProc );

static void r_junk (void);


/*-----------------  for logging of messages  ---------------------------*/
/*#define R_LOG_INFO_D*/
#ifdef R_LOG_INFO_D


#define R_LOG_FILE	"/var/tmp/.rickr.log.GF"

#define R_LOG( string ) fprintf( stderr, "\007\033[2m%s\033[0m\n", string )


FILE * gr_logfile


static int	r_open_log_file		( void );

#endif
