
#ifndef _IMON_H_
#define _IMON_H_

/*----------------------------------------------------------------------*/

#define IFM_PROG_NAME   "Imon"

#define IFM_MAX_FLEN       200       /* maximum characters in filename   */
#define IFM_PAD_LEN         20       /* padding for I-file expansion     */
#define IFM_EPSILON      0.001       /* slice epsilon                    */
#define IFM_STAT_ALLOC      20       /* allocation blocksize - run stats */
#define IFM_MAX_IM_ALLOC    40       /* initial limit for read_ge_files  */
#define IFM_MAX_RUN_NAPS     3       /* maximum number of mid-run naps   */
#define IFM_MAX_GE_FAILURES  3       /* file read failures before exit   */
#define IFM_MAX_NT       32767       /* maximum valid num time points    */
#define IFM_SUFFIX_LEN      10       /* allocated space for I-file sufix */

#define IFM_MIN_NICE_INC   -19       /* minimum nice value increment     */
#define IFM_MAX_NICE_INC    20       /* maximum nice value increment     */

#define IFM_USE_SHORT        1       /* usage constants                  */
#define IFM_USE_LONG         2
#define IFM_USE_VERSION      3

#define IFM_DEBUG_DEFAULT    1       /* default debug level: show status */
#define IFM_MAX_DEBUG        3       /* maximum debug level		 */

#define IFM_GERT_SCRIPT "GERT_Reco2" /* output script, like GERT_Reco    */

/* -- define copies -- */

#define LSB_FIRST	     1
#define MSB_FIRST	     2

/*-----------------------------------------------------------------------*/
                                    /* from Ifile.c ... */
typedef struct                      /* stuff extracted from GE I.* image */
{
    int   good;                     /* is this a good image?           */
    int   nx, ny;                   /* image matrix                    */
    int   uv17;                     /* apparently codes for scan index */
    float dx,dy,dz, zoff, tr,te;    /* various dimensions              */
    char  orients[8];               /* orientation string              */
} ge_header_info;

typedef struct                      /* extra stuff from mri_read.c     */
{
    int   bpp;                      /* bits per pixel                  */
    int   cflag;                    /* compression flag (0=compressed) */
    int   hdroff;                   /* offset of image header          */
    int   skip;                     /* offset of image data into file  */
    int   swap;                     /* did we do byte swapping?        */
    int   kk;			    /* z-orient info (1=LR, 2=PA, 3=IS)*/
    float xyz[9];
} ge_extras;

typedef struct
{
    ge_header_info   geh;           /* array of ge_header_info structs  */
    ge_extras        gex;           /* array of ge_extras structs      */
    int              index;         /* index into fnames array        */
    int              bytes;         /* size of image in bytes        */
    void           * image;	    /* actual image data            */
} finfo_t;

typedef struct
{
    int		     nalloc;	    /* number of images allocated for   */
    int		     nused;	    /* number of images in use now      */
    int		     ary_len;       /* length of allocated im_ary array */
    int		     im_size;	    /* size of each individual image    */
    void          ** im_ary;	    /* array of images                  */
    void           * x_im;          /* extra image for afni comm        */
} im_store_t;

typedef struct  /* user options */
{
    char           * start_file;    /* user-specified starting file     */
    char           * start_dir;     /* user input starting directory    */
    char           * drive_cmd;     /* DRIVE_AFNI command to send       */
    char	  ** argv;	    /* passed to the program            */
    int              argc;
    int              nt;            /* user input time points per run   */
    int              nice;          /* nice offset (must be >= 0)       */
    int              debug;         /* debug level                      */
    int              gert_reco;     /* output GERT_Reco2 script         */

    /* realtime options */
    int              rt;            /* run in real-time afni mode       */
    int              swap;          /* swap bytes when sending data     */
    int              rev_bo;        /* reverse BYTEORDER command        */
    char           * host;          /* pointer to hostname              */
} opts_t;

typedef struct
{
    int              nused;         /* number of elements assigned      */
    int              nalloc;        /* number of elements allocated for */
    finfo_t        * flist;         /* array of finfo structures        */
    im_store_t       im_store;      /* structure to hold actual images  */

    char           * glob_dir;      /* wildcard format to search for    */
    int              nfiles;        /* number of files in list          */
    char          ** fnames;        /* corresponding file names         */

    opts_t           opts;          /* user specified options           */
} param_t;

typedef struct			    /* used for the stats_t struct      */
{
    int  volumes;		    /* number of volumes in this run    */
    char f1name[IFM_MAX_FLEN];	    /* file name for first image        */
} run_t;

typedef struct			/* used to output statistics at the end */
{
    int     slices;		/* the number of slices in each volume  */
    float   z_first, z_last;	/* bounding range for slice locations   */
    float   z_delta;		/* slice thickness                      */

    int     nalloc;		/* number of run_t structures allocated */
    int     nused;		/* number of run_t structures in use    */
    int     nvols;		/* number of volumes in a run           */
    run_t * runs;		/* array of run_t strcutrues            */
} stats_t;

typedef struct
{
    ge_header_info geh;                  /* sample GE header structure       */
    int            nim;                  /* number of images in this volume  */
    int            fl_1;                 /* first index into flist           */
    int            fn_1, fn_n;           /* indicies into the fnames list    */
    char           first_file[IFM_MAX_FLEN]; /*file name of first slice image*/
    char           last_file [IFM_MAX_FLEN]; /*file name of last slice image */
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
/* macros */

#define IFM_BIG_ERROR_MESG( I_str, I_file, I_ez, I_az, I_run, I_s1, I_sn )  \
	do {								\
	    fprintf( stderr, "\007\n"					\
		    "***********************************************\n" \
		    "Error: %s\n"					\
		    "       current file      : %s\n"			\
		    "       expected z-offset : %.4f\n"      		\
		    "       actual z-offset   : %.4f\n"      		\
		    "       current run       : %d\n"                   \
		    "       slice number      : %d (of %d)\n"           \
		    "***********************************************\n",\
		I_str, I_file, I_ez, I_az, I_run, I_s1, I_sn );         \
	} while (0)

#endif /* _IMON_H_ */
