
/*	 usage types 		*/

#define	 USENOOP 	0
#define	 USEFULL 	1
#define	 USEOTHER 	2

/*	 operation values	*/

#define  NO_OP	 		0
#define  STAT_OP 		1
#define  BI_STAT_OP 		2
#define  HIST_OP 		10
#define  CORR_OP 		20
#define  POINTS_OP 		25
#define  SLICE_VALS_OP 		27
#define  UTEST_OP 		30
#define  UTEST1_OP 		40

/*	 operation strings	*/

#define  STAT_ST 	"stats"
#define  BI_STAT_ST 	"bi_stats"
#define  HIST_ST 	"hist"
#define  CORR_ST 	"correlate"
#define  POINTS_ST 	"points"
#define  SLICE_VALS_ST 	"slice_vals"
#define  UTEST_ST 	"u_test"
#define  UTEST1_ST 	"u_test_1m"


/*	 general definitions	*/

#define  MAX_INPUTS      	3
#define  MAX_SUB_BRICKS 	2000

#define  R_MAX_AFNI_DSETS	3
#define  R_FILE_L		128
#define  R_MESSAGE_L		1024
#define  R_MAX_BINS		10000

typedef struct
{
    /* set before reading afni info */
    int                 must_be_short;               /* must input be short   */
    int                 want_floats;                 /* create fimage ?       */
    int                 subs_must_equal;             /* require same number   */
    int                 max_subs;                    /* max number of subs    */

    /* basic afni stuff */
    THD_3dim_dataset  * dset    [ R_MAX_AFNI_DSETS ];
    short            ** simage  [ R_MAX_AFNI_DSETS ]; /* must allocate        */
    float             * factor  [ R_MAX_AFNI_DSETS ]; /* must allocate        */
    int                 subs    [ R_MAX_AFNI_DSETS ]; /* number of subbricks  */

    int                 nx, ny, nz, nvox;

    /* other */
    float             * fimage  [ R_MAX_AFNI_DSETS ]; /* shorts with factor   */

    u_short             max_u_short;                  /* no factor considered */
    int                 num_dsets;                    /* number read in       */
} r_afni_s;


typedef enum operation_enum
	{ no_op, hist_op, stats_op, last_op } op_enum;

static char * gr_op_strings[] =
	{ "hist", "stats" };
static int    gr_num_ops      = 2;
static char * gr_yn_strings[] = { "y", "n" };
static int    gr_num_yn_strings = 2;


typedef struct
{
    op_enum	operation;	/* which function to execute    */

    float	min, max;	/* restrictions on data values  */
    int		use_min;
    int		use_max;
    int		use_tails;

    int		num_bins;	/* number of bins for histogram */

    char      * comment;	/* comment to print for point display */
    int		use_comment;

    int		slice_number;	/* slice number to print out	*/
    int		use_LPI;	/* boolean			*/

    char      * outfile;	/* memory is on the afni side   */
    FILE      * outfp;
} mask_opt_s;


/*----------------------------------------------------------------------*/

static  int     check_usage             ( int, char **, mask_opt_s * );
static  int	file_exists		( char *, char * );
static  char *	fill_afni_struct	( r_afni_s * );
static  FILE *  open_file               ( char *, char * );
static  void    print_stats_header      ( FILE * );
static  void    print_empty_stats       ( FILE * );
static  char *  process                 ( r_afni_s *, mask_opt_s * );
static  char *  process_args            ( r_afni_s *, mask_opt_s *,
					  PLUGIN_interface * );
static  u_short r_get_max_u_short	( u_short *, int );
static  int     read_dataset            ( char *, int );
static  void    usage                   ( char *, char *, int, mask_opt_s * );

static  int     op_type                 ( char * );

static  int     assign_afni_floats      ( r_afni_s * );
static  void    assign_min_max          ( float *, long, float *, float * );
static  char *  calc_hist               ( r_afni_s *, mask_opt_s * );
static  char *  calc_stats              ( r_afni_s *, mask_opt_s * );

static  void    do_stats                ( r_afni_s *, float *, long, float,
                                          float, int, FILE *, long *, float *,
                                          float * );
static  void    do_bi_stats             ( r_afni_s *, mask_opt_s * );

static  long    get_mask_size           ( r_afni_s *, int, int );
static  long    mask_all_shorts_to_float( r_afni_s *, int, int, float * );
static  long    mask_shorts_to_float    ( r_afni_s *, float *, int, int, int );
static  long    mask_shorts_to_short    ( r_afni_s *, short *, int, int, int );
static  int     num_places              ( float, int );
        int     short_test              ( const void *, const void * );



/*----------------------------------------------------------------------

Adding a new function to maskcalc:

- define function_OP, func_ST, USEfunc
- add F_OP to read_args( )	( check subbricks in read_dataset )
- add F_OP to check_usage ( )
- add F_ST usage to usage( )	( and add to USEFULL )
- add F_OP to op_type( )
- add F_OP to process( )
- write new function and prototype

  ----------------------------------------------------------------------
*/
