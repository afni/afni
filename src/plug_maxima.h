/*----------------------------------------------------------------------
 *
 *  plug_maxima.h
 *
 *----------------------------------------------------------------------
*/


/*  AFNI stuff  */
/*--------------------  defines    ----------------------------------------*/

#define R_MAX_AFNI_DSETS          3
#define R_MESSAGE_L             256
#define R_FILE_L                128

#define rWARNING( string ) fprintf( stderr, "\033[1m%s\033[0m\n", string )
#define rERROR(   string ) fprintf( stderr, "\007\033[2m%s\033[0m\n", string )


/*--------------------  typedefs   ----------------------------------------*/

typedef struct
{
    /* set before reading afni info */
    int                 must_be_short;               /* must input be short   */
    int                 want_floats;                 /* create fimage ?       */
    int                 subs_must_equal;             /* require same number   */
    int                 max_subs;                    /* max number of subs    */

    /* basic afni stuff */
    THD_3dim_dataset  * dset    [ R_MAX_AFNI_DSETS ];
    short             * simage  [ R_MAX_AFNI_DSETS ];
    float               factor  [ R_MAX_AFNI_DSETS ];
    int                 subs    [ R_MAX_AFNI_DSETS ]; /* number of subbricks  */

    int                 nx, ny, nz, nvox;

    /* other */
    float             * fimage  [ R_MAX_AFNI_DSETS ]; /* shorts with factor   */

    u_short             max_u_short;                  /* no factor considered */
    int                 num_dsets;                    /* number read in       */
} r_afni_s;


/*--------------------  globals    ----------------------------------------*/

extern char grMessage [ R_MESSAGE_L ];          /* global message string */




/*-- maxima stuff ---*/


#define MAX_MASK_FILL_VAL	1

#define MAX_SORT_N_REMOVE_STYLE	1
#define MAX_WEIGHTED_AVE_STYLE	2
#define MAX_MAX_STYLE		2

typedef struct
{
    int   * plist;
    int     used;
    int     M;
} point_list_s;


typedef struct
{
    THD_3dim_dataset  * dset;			/* input dset 		      */
    short             * sdata;			/* short data from dset       */
    short             * result;			/* requires own M 	      */
    int			nx, ny, nz, nxy, nvox;

    point_list_s        P;			/* point list in result       */

    int			extrema_count;		/* number of extrema 	      */

    int			data_type;		/* MRI_short, etc	      */
    int			adn_type;		/* HEAD_ANAT/FUNC 	      */
    int			func_type;		/* FUNC_FIM_TYPE? 	      */

    char 		outfile[ R_FILE_L ];	/* output prefix 	      */

    float		cutoff;		        /* acceptable value cutoff    */
    float		min_dist;	        /* distance between extrema   */
    float		out_rad;	        /* mask radius around extrema */

    int			negatives;		/* find negative extrema      */
    int			ngbr_style;		/* what to do with neighbors  */
    int			overwrite;		/* do we allow overwrite      */
    int			quiet;			/* no text output of results  */
    int			true_max;		/* no neighbor equality       */
} maxima_s;


static int	add_point_to_list  ( point_list_s *, int );
static int	apply_fill_radius  ( maxima_s * );
static int	apply_min_dist     ( maxima_s * );
static int	clear_around_point ( int, maxima_s *, point_list_s * );
static int	create_point_list  ( maxima_s * );
static int	display_coords     ( r_afni_s *, maxima_s * );
static int	find_local_maxima  ( maxima_s * );
static void	free_memory        ( r_afni_s *, maxima_s * );
static void	full_usage         ( char * );
static int	init_maxima_s	   ( maxima_s *, r_afni_s *, char * );
static int 	init_afni_s	   ( r_afni_s * );
static char *	process_args	   ( r_afni_s *, maxima_s *,PLUGIN_interface *);
static int	process_data	   ( maxima_s * );
static int	radial_fill	   ( int, int, int, maxima_s * );
static int	weighted_index     ( point_list_s *, maxima_s * );
static int	write_results      ( r_afni_s *, maxima_s *,PLUGIN_interface *);

static void	show_maxima_s      ( maxima_s * );

int 		point_comp_neg     ( const void *, const void * );
int 		point_comp_pos     ( const void *, const void * );


int r_set_afni_s_from_dset( r_afni_s *, THD_3dim_dataset * );
u_short r_get_max_u_short( ushort *, int );


/* global var for comparasin function use */
static short *  gr_orig_data = NULL;



