#ifndef _IMON_H_
#define _IMON_H_

/*----------------------------------------------------------------------*/

#define IFM_PROG_NAME   "Imon"

#define INIT_ALLOC         200       /* initial number of structs        */
#define MAX_FLEN           200       /* maximum characters in filename   */
#define IFM_PAD_LEN         20       /* padding for I-file expansion     */
#define IFM_EPSILON      0.001       /* slice epsilon                    */
#define IFM_STAT_ALLOC      20       /* allocation blocksize - run stats */
#define IFM_MAX_RUN_NAPS     3       /* maximum number of mid-run naps   */

#define IFM_MIN_NICE_INC   -19       /* minimum nice value increment     */
#define IFM_MAX_NICE_INC    20       /* maximum nice value increment     */

#define IFM_USE_SHORT        1       /* usage constants                  */
#define IFM_USE_LONG         2
#define IFM_USE_VERSION      3

#define IFM_DEBUG_DEFAULT    1       /* default debug level: show status */
#define IFM_MAX_DEBUG        3       /* maximum debug level		 */

/*----------------------------------------------------------------------*/
                                    /* from Ifile.c ... */
typedef struct                      /* stuff extracted from GE I.* image */
{
    int   good;                     /* is this a good image? */
    int   nx, ny;                   /* image matrix */
    int   uv17;                     /* apparently codes for scan index */
    float dx,dy,dz, zoff, tr,te;    /* various dimensions */
    char  orients[8];               /* orientation string */
} ge_header_info;

typedef struct                      /* extra stuff from mri_read.c     */
{
    int   bpp;                      /* bits per pixel                  */
    int   cflag;                    /* compression flag (0=compressed) */
    int   hdroff;                   /* offset of image header          */
    int   skip;                     /* offset of image data into file  */
    int   swap;                     /* did we do byte swapping?        */
    float xyz[9];
} ge_extras;

typedef struct
{
    ge_header_info   geh;           /* array of ge_header_info structs  */
    ge_extras        gex;           /* array of ge_extras structs       */
    int              index;         /* index into fnames array          */
} finfo_t;

typedef struct
{
    int              nused;         /* number of elements assigned      */
    int              nalloc;        /* number of elements allocated for */
    finfo_t        * flist;         /* array of finfo structures        */

    char           * start_dir;     /* user input starting directory    */
    char           * glob_dir;      /* wildcard format to search for    */
    int              nfiles;        /* number of files in list          */
    char          ** fnames;        /* corresponding file names         */

    int              nice;          /* nice offset (must be >= 0)       */
} param_t;

typedef struct			    /* used for the stats_t struct      */
{
    int  volumes;		    /* number of volumes in this run    */
    char first_im[MAX_FLEN];	    /* file name for first image        */
} run_t;

typedef struct			/* used to output statistics at the end */
{
    int     slices;		/* the number of slices in each volume  */
    float   z_first, z_last;	/* bounding range for slice locations   */
    float   z_delta;		/* slice thickness                      */

    int     nalloc;		/* number of run_t structures allocated */
    int     nused;		/* number of run_t structures in use    */
    run_t * runs;		/* array of run_t strcutrues            */
} stats_t;

typedef struct
{
    ge_header_info geh;                  /* sample GE header structure       */
    int            nim;                  /* number of images in this volume  */
    int            first_im, last_im;    /* indicies into the fnames list    */
    char           first_file[MAX_FLEN]; /* file name of first slice image   */
    char           last_file [MAX_FLEN]; /* file name of last slice image    */
    float          z_first;              /* z location of first slice image  */
    float          z_last;               /* z location of last slice image   */
    float          z_delta;              /* signed slice thickness           */
    int            seq_num;              /* sequence number in TRs (1-based) */
    int            run;                  /* run number                       */
} vol_t;

typedef struct
{
    int level;
} IFM_debug;

/*----------------------------------------------------------------------*/

static int check_stalled_run  ( int run, int seq_num, int naps, int nap_time );
static int dir_expansion_form ( char * sin, char ** sexp );
static int find_first_volume  ( vol_t * v, param_t * p );
static int find_more_volumes  ( vol_t * v, param_t * p );
static int find_next_zoff     ( param_t * p, int start, float zoff );
static int init_options       ( param_t * p, int argc, char * argv[] );
static int nap_time_from_tr   ( float tr );
static int read_ge_files      ( param_t * p, int next, int max );
static int read_ge_header     ( char * pathname, ge_header_info * hi,
	                        ge_extras * E);
static int scan_ge_files      ( finfo_t * flist, char ** fnames,
				int next, int nfiles );
static int set_volume_stats   ( vol_t * v );
static int show_run_stats     ( stats_t * s );
static int swap_4             ( void * ptr );

static void hf_signal         ( int signum );

/* volume scanning */
static int volume_match  ( vol_t * vin, vol_t * vout, param_t * p, int start );
static int volume_search ( vol_t * V, param_t * p, int * start, int maxsl );

/* information functions */
static int idisp_hf_param_t     ( char * info, param_t * p );
static int idisp_hf_vol_t       ( char * info, vol_t * v );
static int idisp_ge_extras      ( char * info, ge_extras * E );
static int idisp_ge_header_info ( char * info, ge_header_info * I );

static int usage                ( char * prog, int level );

/*----------------------------------------------------------------------*/
/* macros */

#define IFM_BIG_ERROR_MESG( I_str, I_file, I_ez, I_az, I_run, I_s1, I_sn )  \
	do {								\
	    fprintf( stderr, "\007\n"					\
		    "***********************************************\n" \
		    "Error: %s\n"					\
		    "       file              : %s\n"			\
		    "       expected z-offset : %.4f\n"      		\
		    "       actual z-offset   : %.4f\n"      		\
		    "       run               : %d\n"                   \
		    "       slice number      : %d (of %d)\n"           \
		    "***********************************************\n",\
		I_str, I_file, I_ez, I_az, I_run, I_s1, I_sn );         \
	} while (0)

#endif /* _IMON_H_ */
