
#ifndef _DIMON_H_
#define _DIMON_H_

/*----------------------------------------------------------------------*/

#define IFM_MAX_FLEN       200       /* maximum characters in filename   */
#define IFM_PAD_LEN         20       /* padding for I-file expansion     */
#define IFM_EPSILON       0.01       /* slice epsilon                    */
#define IFM_STAT_ALLOC      20       /* allocation blocksize - run stats */
#define IFM_MAX_IM_ALLOC    40       /* initial limit for read_ge_files  */
#define IFM_MAX_VOL_SLICES 3000      /* max slices per volume            */
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
#define IFM_IM_FTYPE_AFNI    10      /* AFNI/NIFTI                       */

#define IFM_DEBUG_DEFAULT    1       /* default debug level: show status */
#define IFM_MAX_DEBUG        5       /* maximum debug level              */

#define IFM_GERT_SCRIPT "GERT_Reco2" /* output script, like GERT_Reco    */
#define IFM_GERT_DICOM  "GERT_Reco_dicom" /* DICOM GERT_Reco script      */
#define IFM_SLICE_PAT   "alt+z"

/* finfo_t entry states */
#define IFM_FSTATE_FAILED   -2       /* complete failure             */
#define IFM_FSTATE_SKIP     -1       /* ready to read image          */
#define IFM_FSTATE_UNKNOWN   0       /* ready to read image          */
#define IFM_FSTATE_DONE      1       /* processing complete          */

#define IFM_FSTATE_TO_PROC   2       /* have image, ready to process */
#define IFM_FSTATE_TO_SORT   3       /* waiting for successful sort  */
#define IFM_FSTATE_TO_READ   4       /* ready to read image          */

#define IFM_MAX_READ_ERRORS  2       /* after this, go to failed state */
#define IFM_NUM_RETRIES      2       /* volume retries before failing  */

#define IFM_SORT_UNKNOWN    -1       /* -sort_method possibilities:    */
#define IFM_SORT_UNSPEC      0       /*    not specified               */
#define IFM_SORT_NONE        1       /*    no realtime sorting         */
#define IFM_SORT_ACQ_TIME    2       /*    default RT sort             */
#define IFM_SORT_DEFAULT     3       /*    default RT sort             */
#define IFM_SORT_GEME        4       /*    GE multi-echo index         */
#define IFM_SORT_NUM_SUFF    5       /*    numerical file suffix       */
#define IFM_SORT_ZPOSN       6       /*    z-coordinate                */
#define IFM_SORT_NUM_METHODS 6       /*    should match top index      */

/* -- define copies -- */

#define LSB_FIRST            1
#define MSB_FIRST            2


/*-----------------------------------------------------------------------*/

typedef struct  /* user options */
{
    char           * start_file;    /* user-specified starting file     */
    char           * start_dir;     /* user input starting directory    */
    char           * dicom_glob;    /* globbing form for dicom files    */
    char           * infile_list;   /* file holding input filenames     */
    char           * sp;            /* slice acquisition pattern        */
    char           * gert_outdir;   /* output directory for GERT_Reco2  */
    char           * file_type;     /* NULL or AFNI/GEMS/DICOM          */
    char          ** argv;          /* passed to the program            */
    int              argc;
    float            tr;            /* user input TR, overrides files   */
    float            ep;            /* epsilon - defaut to IFM_EPSILON  */
    int              nt;            /* user input time points per run   */
    int              num_slices;    /* first volume must match          */
    int              max_images;    /* max allowed images per volume    */
    int              max_quiet_trs; /* max TRs w/out data before quit   */
    int              nice;          /* nice offset (must be >= 0)       */
    int              pause;         /* pause time between volumes (ms)  */
    float            sleep_frac;    /* TR fraction to sleep (default 2) */
    int              sleep_init;    /* pre-first vol sleep time (ms)    */
    int              sleep_vol;     /* between-vol sleep time (ms)      */
    int              debug;         /* debug level                      */
    int              quit;          /* quit when no new images found    */
    int              no_wait;       /* never wait for more data         */
    int              assume_dicom_mosaic; /* useful for 3D format       */
    int              use_last_elem; /* use last element in DICOM images */
    int              use_slice_loc; /* use Slice Loc for zoff           */
    int              use_obl_origin;/* maybe apply to3d -oblique_origin */
    int              ushort2float;  /* convert all shorts to float      */
    int              show_sorted_list; /* display sorted list and quit  */

    /* GERT_Reco options */
    int              gert_reco;     /* output GERT_Reco script          */
    char           * gert_filename; /* GERT_Reco script name            */
    char           * gert_prefix;   /* to3d prefix in GERT_Reco script  */
    char           * chan_prefix;   /* _chan_ par of prefix in script   */
    int              gert_nz;       /* override nz=1 in script          */
    int              gert_format;   /* dataset format: 0=AFNI, 1=NIFTI  */
                                    /* (see -gert_write_as_*)           */
    int              gert_exec;     /* execute the reco script          */
    int              gert_quiterr;  /* pass -quit_on_err option to to3d */

    /* DICOM organization options */
    int              dicom_org;     /* flag to organize dicom files     */
    int              sort_num_suff; /* flag to sort by numerical suffix */
    int              sort_acq_time; /* flag to sort by acq time         */
    int              order_as_zt;   /* change tminor to slice minor order */
    int              read_all;      /* flag to read all images at once  */
    int              rev_org_dir;   /* flag to reverse dicom_org dir    */
    int              rev_sort_dir;  /* flag to reverse glob sort dir    */
    int              save_errors;   /* save details in cases of errors  */
    char           * flist_file;    /* filename to save file list to    */
    char           * flist_details; /* filename to save list details to */
    char           * sort_method;   /* method for realtime sorting      */

    /* realtime options */
    int              rt;            /* run in real-time afni mode       */
    int              swap;          /* swap bytes when sending data     */
    int              rev_bo;        /* reverse BYTEORDER command        */
    int              num_chan;      /* number of data channels to use   */
    char           * te_list;       /* list of echo times (in one string)*/
    char           * host;          /* pointer to hostname              */
    string_list      drive_list;    /* list of DRIVE_AFNI commands      */
    string_list      wait_list;     /* list of DRIVE_AFNI commands      */
    string_list      rt_list;       /* list of real-time commands       */
} opts_t;

typedef struct                      /* stuff extracted from GE I.* image */
{
    int   good;                     /* is this a good image?           */
    int   nx, ny;                   /* image matrix                    */
    int   uv17;                     /* apparently codes for scan index */
    int   index;                    /* image counter                   */
    int   im_index;                 /* image index, if one exists      */
    float atime;                    /* acquisition time, if found      */
    float slice_loc;                /* slice location, if found        */
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
    int   ge_me_index;              /* GE multi-echo index             */
    int   ge_nim_acq;               /* number of images in acq         */
    int   sop_iuid_maj;             /* DICOM SOP IUD - major value     */
    int   sop_iuid_min;             /* DICOM SOP IUD - minor value     */
    float xorg;                     /* x and y axes origins            */
    float yorg;
    float xyz[9];
} ge_extras;

typedef struct
{
    int   im_is_volume;             /* mostly from g_image_info */
    int   nslices;
    int   mos_nx, mos_ny;
} mosaic_info;

typedef struct
{
    ge_header_info   geh;           /* ge_header_info struct for this file */
    ge_extras        gex;           /* ge_extras struct for this file      */
    mosaic_info      minfo;         /* info describing mosaic structure    */
    int              findex;        /* index into fim_o list               */
    int              sindex;        /* realtime sort index, if used        */
    int              state;         /* to read, read, processed, failed    */
    int              bad_reads;     /* number of read failures             */
    int              nbytes;        /* size of image in bytes              */
    char           * fname;         /* copy of file name                   */
    void           * imdata;        /* actual image data                   */
} finfo_t;


typedef struct
{
    int              ftype;         /* one of IFM_IM_FTYPE_*            */
    char           * glob_dir;      /* wildcard format to search for    */

    /* sorted by name, alphabetically or by numeric extension, say */
    string_list      fnames_prev;   /* previous and current (sorted?)   */
    string_list      fnames_cur;    /*    file name lists (MCW_f_e)     */

    int              fnames_done;   /* finished getting new fnames      */

    /* fim_o gets incremental differences between fnames_prev and cur,
       i.e. at each scan, differences are appended to fim_o according to
            the file name ordering of those lists (usually alphabetical)
       The sorting in fim_sind is based on the contents of the files.  */
    finfo_t       * fim_o;         /* orig structure array, as found   */

    int              nfim;          /* length of both finfo_t lists  */
    int              nfalloc;       /* number of elements allocated for */
    /* fim_update: -1 = wait, 0 = go, >0 = # first names to ignore      */
    int              fim_update;    /* flag: any changes to propogate?  */
    int              fim_skip;      /* # 2 ignore, from -start_dir/file */
    /* fim_start: all prior images are done with processing             */
    int              fim_start;     /* starting index into fim_sind     */
    int              max2read;      /* max images to read at a time     */

    opts_t           opts;          /* user specified options           */
} param_t;

typedef struct                           /* used for the stats_t struct      */
{
    ge_header_info geh;                  /* first GE header structure        */
    ge_extras      gex;                  /* first GE extras structure        */
    int            volumes;              /* number of volumes in this run    */
    int            f1index;              /* index into fnames list           */
    char         * f1name;               /* file name for first image        */
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
    int     mos_nslices;        /* number of slices in mosaic           */
    run_t * runs;               /* array of run_t strcutrues            */
} stats_t;

typedef struct
{
    ge_header_info geh;                  /* first GE header structure        */
    ge_extras      gex;                  /* first GE extras structure        */
    mosaic_info    minfo;                /* info describing mosaic structure */
    int            nim;                  /* number of images in this volume  */
    int            fs_1;                 /* first index into fim_s list      */
    char         * first_file;           /* file name of first slice image   */
    char         * last_file;            /* file name of last slice image    */
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

#endif /* _DIMON_H_ */
