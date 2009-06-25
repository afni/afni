
#ifndef _IMON_H_
#define _IMON_H_

/*----------------------------------------------------------------------*/

#define IFM_MAX_FLEN       200       /* maximum characters in filename   */
#define IFM_PAD_LEN         20       /* padding for I-file expansion     */
#define IFM_EPSILON       0.01       /* slice epsilon                    */
#define IFM_STAT_ALLOC      20       /* allocation blocksize - run stats */
#define IFM_MAX_IM_ALLOC    40       /* initial limit for read_ge_files  */
#define IFM_MAX_RUN_NAPS     2       /* maximum number of mid-run naps   */
#define IFM_MAX_GE_FAILURES  3       /* file read failures before exit   */
#define IFM_MAX_NT       32767       /* maximum valid num time points    */
#define IFM_SUFFIX_LEN      10       /* allocated space for I-file sufix */

#define IFM_MIN_NICE_INC   -19       /* minimum nice value increment     */
#define IFM_MAX_NICE_INC    20       /* maximum nice value increment     */

#define IFM_USE_SHORT        1       /* usage constants                  */
#define IFM_USE_LONG         2
#define IFM_USE_HIST         3
#define IFM_USE_VERSION      4

#define IFM_IM_FTYPE_NONE    0       /* valid image file types:          */
#define IFM_IM_FTYPE_GEMS5   1       /* GEMS 5.x                         */
#define IFM_IM_FTYPE_DICOM   4       /* DICOM                            */

#define IFM_DEBUG_DEFAULT    1       /* default debug level: show status */
#define IFM_MAX_DEBUG        5       /* maximum debug level              */

#define IFM_GERT_SCRIPT "GERT_Reco2" /* output script, like GERT_Reco    */
#define IFM_GERT_DICOM  "GERT_Reco_dicom" /* DICOM GERT_Reco script      */
#define IFM_SLICE_PAT   "alt+z"

/* -- define copies -- */

#define LSB_FIRST            1
#define MSB_FIRST            2

/*-----------------------------------------------------------------------*/
                                    /* from Ifile.c ... */
typedef struct                      /* stuff extracted from GE I.* image */
{
    int   good;                     /* is this a good image?           */
    int   nx, ny;                   /* image matrix                    */
    int   uv17;                     /* apparently codes for scan index */
    int   index;                    /* image counter                   */
    float dx,dy,dz, zoff, tr,te;    /* various dimensions              */
                                    /* dxyz in mm, tr in seconds       */
    char  orients[8];               /* orientation string              */
} ge_header_info;

typedef struct                      /* extra stuff from mri_read.c     */
{
    int   bpp;                      /* bits per pixel                  */
    int   cflag;                    /* compression flag (0=compressed) */
    int   hdroff;                   /* offset of image header          */
    int   skip;                     /* offset of image data into file  */
    int   swap;                     /* did we do byte swapping?        */
    int   kk;                       /* z-orient info (1=LR, 2=PA, 3=IS)*/
    float xorg;                     /* x and y axes origins            */
    float yorg;
    float xyz[9];
} ge_extras;

typedef struct
{
    ge_header_info   geh;           /* ge_header_info struct for this file */
    ge_extras        gex;           /* ge_extras struct for this file     */
    int              index;         /* index into fnames array           */
    int              bytes;         /* size of image in bytes           */
    void           * image;         /* actual image data               */
} finfo_t;

typedef struct
{
    char          ** str;           /* list of actual strings */
    int              nalloc;        /* number allocated for  */
    int              nused;         /* number in use        */
} string_list;

typedef struct
{
    int              nalloc;        /* number of images allocated for   */
    int              nused;         /* number of images in use now      */
    int              ary_len;       /* length of allocated im_ary array */
    int              im_size;       /* size of each individual image    */
    void          ** im_ary;        /* array of images                  */
    void           * x_im;          /* extra image for afni comm        */
} im_store_t;

typedef struct  /* user options */
{
    char           * start_file;    /* user-specified starting file     */
    char           * start_dir;     /* user input starting directory    */
    char           * dicom_glob;    /* globbing form for dicom files    */
    char           * infile_list;   /* file holding input filenames     */
    char           * sp;            /* slice acquisition pattern        */
    char           * gert_outdir;   /* output directory for GERT_Reco2  */
    char          ** argv;          /* passed to the program            */
    int              argc;
    float            tr;            /* user input TR, overrides files   */
    float            ep;            /* epsilon - defaut to IFM_EPSILON  */
    int              nt;            /* user input time points per run   */
    int              num_slices;    /* first volume must match          */
    int              nice;          /* nice offset (must be >= 0)       */
    int              pause;         /* pause time between volumes (ms)  */
    float            sleep_frac;    /* TR fraction to sleep (default 2) */
    int              sleep_init;    /* pre-first vol sleep time (ms)    */
    int              sleep_vol;     /* between-vol sleep time (ms)      */
    int              debug;         /* debug level                      */
    int              quit;          /* quit when no new images found    */
    int              use_dicom;     /* flag for dicom (not GE) images   */
    int              use_last_elem; /* use last element in DICOM images */
    int              show_sorted_list; /* display sorted list and quit  */

    /* GERT_Reco options */
    int              gert_reco;     /* output GERT_Reco script          */
    char           * gert_filename; /* GERT_Reco script name            */
    char           * gert_prefix;   /* to3d prefix in GERT_Reco script  */
    int              gert_nz;       /* override nz=1 in script          */

    /* DICOM organization options */
    int              dicom_org;     /* flag to organize dicom files     */
    int              sort_num_suff; /* flag to sort by numerical suffix */
    int              rev_org_dir;   /* flag to reverse dicom_org dir    */
    int              rev_sort_dir;  /* flag to reverse glob sort dir    */
    char           * flist_file;    /* filename to save file list to    */

    /* realtime options */
    int              rt;            /* run in real-time afni mode       */
    int              swap;          /* swap bytes when sending data     */
    int              rev_bo;        /* reverse BYTEORDER command        */
    char           * host;          /* pointer to hostname              */
    string_list      drive_list;    /* list of DRIVE_AFNI commands      */
    string_list      wait_list;     /* list of DRIVE_AFNI commands      */
    string_list      rt_list;       /* list of real-time commands       */
} opts_t;

typedef struct
{
    int              ftype;         /* one of IFM_IM_FTYPE_*            */
    int              nused;         /* number of elements assigned      */
    int              nalloc;        /* number of elements allocated for */
    finfo_t        * flist;         /* array of finfo structures        */
    im_store_t       im_store;      /* structure to hold actual images  */

    char           * glob_dir;      /* wildcard format to search for    */
    int              nfiles;        /* number of files in list          */
    char          ** fnames;        /* corresponding file names         */

    opts_t           opts;          /* user specified options           */
} param_t;

typedef struct                           /* used for the stats_t struct      */
{
    ge_header_info geh;                  /* first GE header structure        */
    ge_extras      gex;                  /* first GE extras structure        */
    int            volumes;              /* number of volumes in this run    */
    int            f1index;              /* index into fnames list           */
    char           f1name[IFM_MAX_FLEN]; /* file name for first image        */
} run_t;

typedef struct                  /* used to output statistics at the end */
{
    int     slices;             /* the number of slices in each volume  */
    float   z_first, z_last;    /* bounding range for slice locations   */
    float   z_delta, image_dz;  /* slice thickness (and from image)     */

    int     nalloc;             /* number of run_t structures allocated */
    int     nused;              /* number of run_t structures in use    */
    int     nvols;              /* number of volumes in a run           */
    int     oblique;            /* is the data oblique                  */
    run_t * runs;               /* array of run_t strcutrues            */
} stats_t;

typedef struct
{
    ge_header_info geh;                  /* first GE header structure        */
    ge_extras      gex;                  /* first GE extras structure        */
    int            nim;                  /* number of images in this volume  */
    int            fl_1;                 /* first index into flist           */
    int            fn_1, fn_n;           /* indicies into the fnames list    */
    char           first_file[IFM_MAX_FLEN]; /*file name of first slice image*/
    char           last_file [IFM_MAX_FLEN]; /*file name of last slice image */
    float          z_first;              /* z location of first slice image  */
    float          z_last;               /* z location of last slice image   */
    float          z_delta;              /* signed slice thickness           */
    float          image_dz;             /* dz from image (maybe oblique dz) */
    int            oblique;              /* data is oblique                  */
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
        do {                                                            \
            fprintf( stderr, "\007\n"                                   \
                    "***********************************************\n" \
                    "Error: %s\n"                                       \
                    "       current file      : %s\n"                   \
                    "       expected z-offset : %.4f\n"                 \
                    "       actual z-offset   : %.4f\n"                 \
                    "       current run       : %d\n"                   \
                    "       slice number      : %d (of %d)\n"           \
                    "***********************************************\n",\
                I_str, I_file, I_ez, I_az, I_run, I_s1, I_sn );         \
        } while (0)

#endif /* _IMON_H_ */
