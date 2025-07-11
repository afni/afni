/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "afni.h"
#include "parser.h"

#if 0
# define VMCHECK do{ if(verbose > 1) MCHECK; } while(0)
#else
# define VMCHECK /* nada */
#endif

#if 0 /*  ZSS June 2011. Delete useless code after dust has settled.  */
   #define TCP_CONTROL "tcp:*:7954"      /* control channel specification */
#endif

#define INFO_SIZE  (32*1024)          /* change this ==> change SHM_CHILD below */
#define SHM_CHILD  "shm:afnibahn:32K" /* for data from the child */

#define SHORT_DELAY      1            /* msec */
#define LONG_DELAY      10

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

#include "coxplot.h"

/***********************************************************************
  Plugin to accept data from an external process and assemble it
  into a dataset.  Initial implementation was for the Bruker 3T/60
  system at the Medical College of Wisconsin.  Now works with generic
  external "image source" software.  See README.realtime for info.
************************************************************************/

/** 24 Jun 2002: modified to allow nzz=1 for UCSD trolls                     **/
/** 27 Jun 2003: added BYTEORDER command for automatic byte swapping [rickr] **/
/** 30 Jun 2003: allow MRI_complex data type when using BYTEORDER    [rickr] **/
/** 30 Oct 2003: if possible, compute function on registered data    [rickr] **/
/** 29 Jan 2004: allow 100 chars in root_prefix via PREFIX (from 31) [rickr]
               * x-axis of 3-D motion graphs changed from time to reps
               * plot_ts_... functions now use reg_rep for x-axis values
               * reg_graph_xr is no longer scaled by TR
               * added (float *)reg_rep, for graphing with x == rep num
               * added RT_set_grapher_pinnums(), to call more than once
               * added GRAPH_XRANGE and GRAPH_YRANGE command strings for
                     control over the scales of the motion graph
               * if GRAPH_XRANGE and GRAPH_YRANGE commands are both passed,
                     do not display the final (scaled) motion graph          **/
/** 13 Feb 2004: added RT_MAX_PREFIX for incoming PREFIX command     [rickr]
               * if GRAPH_?RANGE is given, disable 'pushing'
                     (see plot_ts_xypush())
               * added GRAPH_EXPR command, to compute and display a single
                     motion curve, instead of the normal six
                     (see p_code, etc., reg_eval, and RT_parser_init())      **/
/** 31 Mar 2004: added ability to send registration parameters       [rickr]
               * If the AFNI_REALTIME_MP_HOST_PORT environment variable is
                     set (as HOST:PORT, e.g. localhost:53214), then the six
                     registration correction parameters will be sent to that
                     host/port via a tcp socket.  This is done only in the
                     case of graphing the 3D registration parameters.
               * added RT_input variables to manage the new socket
               * added RT_mp_comm_...() functions
               * modified yar[] logic in RT_registration_3D_realtime() to
                     pass the registration parameters before adjusting the
                     base pointers for plot_ts_addto()                       **/
/** 02 Apr 2004: move RT_mp_comm_close() from last plot check  [tross/rickr] **/
/** 10 May 2005: added TPATTERN command to set timing pattern        [rickr] **/
/** 13 Sep 2005: add empty markers to appropriate datasets           [rickr] **/
/** 11 Nov 2006: pass ROI means via RT_mp_*, one per ROI per TR      [rickr] **/
/** 16 Oct 2008: added capability to write data to disk in real time [vinai] **/
/** 01 Jun 2009: added ability to have a callback called at each
                 time an update is sent to AFNI -- for further
                 processing of some hideous sort, I suppose          [RWCox] **/
/** 02 Jun 2009: added ability to merge multichannel data            [RWCox] **/
/** 02 Jun 2010: added ability to register merged data, and to align
                 channels via the same merge registration parameters [rickr] **/
/** 15 Mar 2012: added AR_Mask_Dset, for per-run mask control        [rickr] **/
/**  2 Jan 2020: added All_Data_light method                         [JGC]   **/
/** 13 Jan 2020: merged reorder of ROI average operation             [rickr] **/
/** 14 Jan 2020: apply regtime to ext reg dset; fix pre-merge align  [rickr]
               * dx,y,z were used for multiple things, corrupting the
                 channel merge values                                        **/
/** 23 Jan 2020: added ROIs_and_data mask method for JGC             [rickr]
                 This was given a new mask method (and in rr.py), sending 2
                 lengths to the receiver, one for #(non-1)ROIs, one for the
                 number of mask==1 voxels.                                   **/

#define RT_VERSION_STR "23 Jan 2020: ROIs_and_data"


/**************************************************************************/
/*********************** struct for reading data **************************/

#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
#include <ctype.h>

#define DTYPE_2DZ   77
#define DTYPE_2DZT  78
#define DTYPE_3D    79
#define DTYPE_3DT   80
#define DTYPE_3DTM  81

#define ZORDER_ALT  33
#define ZORDER_SEQ  34
#define ZORDER_EXP  39
#define NZMAX       1024

#define RT_MAX_EXPR     1024    /* max size for parser expression  */
#define RT_MAX_PREFIX    100    /* max size for output file prefix */

#define RT_MP_DEF_PORT 53214    /* default port for sending motion params */

#define DEFAULT_XYFOV    240.0
#define DEFAULT_XYMATRIX  64
#define DEFAULT_ZDELTA     5.0
#define DEFAULT_ZNUM       2
#define DEFAULT_TR         1.0

#define RT_NBUF          INFO_SIZE
#define NNAME            128

#define ORCODE(aa) \
  ( (aa)=='R' ? ORI_R2L_TYPE : (aa)=='L' ? ORI_L2R_TYPE : \
    (aa)=='P' ? ORI_P2A_TYPE : (aa)=='A' ? ORI_A2P_TYPE : \
    (aa)=='I' ? ORI_I2S_TYPE : (aa)=='S' ? ORI_S2I_TYPE : ILLEGAL_TYPE )

#define MAX_CHAN 32    /* 30 Jul 2002 */

static int        num_open_controllers ;                       /* 02 Aug 2002 */
static int            open_controller_index[MAX_CONTROLLERS] ;
static Three_D_View * open_controller      [MAX_CONTROLLERS] ;

static int num_runs = 0 ;  /* 20 May 2009 = # times new acquisitions started */

typedef struct {

   int info_ok , no_data ;        /* status flags */

   int         image_mode ;       /* 28 Apr 2000 */
   void      * image_handle ;
   MRI_IMAGE * image_space ;

   char     name_data[NNAME] ;    /* where to get image data */
   IOCHAN * ioc_data ;            /* IO channel for image data */

   char     name_info[NNAME] ;    /* where to get control data */
   IOCHAN * ioc_info ;            /* IO channel for control data */
   pid_t    child_info ;          /* pid of child that will give me control information */
   double   child_start_time ;    /* 10 Dec 1998: elapsed time when child process was forked */

   int   nxx  ,nyy  ,nzz  ,       /* matrix */
         orcxx,orcyy,orczz ;      /* orientations */

   float xxfov , yyfov , zzfov ;  /* field of view */
   float dxx   , dyy   , dzz ,    /* voxel sizes */
         xxorg,yyorg,zzorg ;      /* grid origins */
   int   xxdcode,yydcode,zzdcode; /* direction code for x,y,z offsets */

   float xxoff , yyoff , zzoff ;  /* offsets (instead of xxorg, etc.) [18 Dec 2002] */

   float zgap ;                   /* extra gap between slices [18 Dec 2002] */
                                  /* (only used if zzfov is used instead of dzz) */

   int   xcen ,ycen ,zcen  ;      /* centering of axes? */

   float tr ;                     /* TR */
   int   nr ;                     /* expected # volumes */

   int   dtype ;                  /* dataset type: a DTYPE code */
   int   zorder ;                 /* 2D slice ordering: a ZORDER code */
   int   zorder_lock ;            /* 22 Feb 1999: lock zorder value */
   int   tpattern ;               /* 2D slice timing     10 May 2005 [rickr] */
   int   nzseq , zseq[NZMAX] ;    /* slice input order */
   int   nstimes ;                /* slice times, tpattern = ZORDER_EXP */
   float stimes[NZMAX];
   int   datum ;                  /* a MRI_type code from mrilib.h */
   int   swap_on_read ;           /* flag: swap bytes?   26 Jun 2003 [rickr] */

   int   nbuf ;                   /* current buffer size */
   char  buf[RT_NBUF] ;           /* buffer for reading command strings */

   char root_prefix[THD_MAX_PREFIX] ;  /* name of dataset (sort of) */
   char storage_mode;             /* if implied by specified prefix */

   /* 01 Aug 2002: these items are now indexed by MAX_CHAN
                   to allow for multiple channel acquisitions */

   int num_chan ;                               /* number of dataset channels */
   int cur_chan ;                               /* current channel index */

   int afni_status[MAX_CHAN] ;                  /* does AFNI know about this dataset yet? */
   THD_3dim_dataset * dset[MAX_CHAN] ;          /* AFNI dataset under construction */
   Three_D_View * im3d[MAX_CHAN] ;              /* AFNI controller which gets it */
   THD_session * sess[MAX_CHAN] ;               /* AFNI session which gets it */
   int           sess_num[MAX_CHAN] ;           /* AFNI session index */
   char * sbr[MAX_CHAN] ;                       /* sub-brick under construction */
   char * im[MAX_CHAN] ;                        /* place for next slice to be read */
   int nvol[MAX_CHAN] ;                         /* # volumes read so far */
   int nsl[MAX_CHAN] ;                          /* # slices read so far */
   float TE[MAX_CHAN] ;           /* echo times, per channel - for T2* est */

   int sbr_size ;                 /* # bytes per sub-brick */
   int imsize ;                   /* # bytes per image */
   MRI_IMARR * bufar ;            /* buffer for input images */

   THD_3dim_dataset * func_dset ; /* functional dataset, if any */
   int func_status ;              /* does AFNI know about this dataset yet? */
   int func_code ;                /* how to compute function */
   int func_condit ;              /* condition that function computation is in now */
   int_func * func_func ;         /* function to compute the function */

   /*-- April 2012 Cameron Craddock real-time detrend + ort correction */
   THD_3dim_dataset *detrend_dset ;   /* detrended dataset, if any */
   int detrend_status ;               /* does AFNI know about this dataset yet? */
   int detrend_nvol ;                 /* number of volumes detrended so far */
   unsigned char detrend_mode ;       /* specifies which regressors should be removed */
   int detrend_polort ;               /* specifies degree of poly to be removed */
   float detrend_fwhm;                /* specifies the extent of spatial smoothing */
   /* end CC */

   /*-- 07 Apr and 01 Aug 1998: real-time image registration stuff --*/

   THD_3dim_dataset *reg_dset ;       /* registered dataset, if any */
   THD_3dim_dataset *reg_base_dset ;  /* registration base dataset */
   MRI_2dalign_basis **reg_2dbasis ;  /* stuff for each slice */
   THD_3dim_dataset *t2star_ref_dset ;/* T2* ref base dataset */
   int reg_base_index ;               /* where to start? */
   int reg_mode ;                     /* how to register? */
   int reg_base_mode ;                /* how to apply registration base */
   int reg_status ;                   /* does AFNI know about this dataset yet? */
   int reg_nvol ;                     /* number of volumes registered so far */
   int reg_graph ;                    /* 17 Aug 1998: to graph, or not to graph */
   int reg_nest ;                     /* number of estimated parameters */
   float *reg_tim , *reg_dx  ,
         *reg_dy  , *reg_phi  ;       /* estimated motion parameters */

   /*--  Oct 1998: more stuff for 3D registration --*/

   float *reg_dz , *reg_theta , *reg_psi , *reg_rep ;
   MRI_3dalign_basis * reg_3dbasis ;
   int iha , ax1,hax1 , ax2,hax2 , ax3,hax3 ;
   MEM_topshell_data * mp ;

   int   reg_resam , reg_final_resam ;
   int   reg_graph_xnew, reg_graph_ynew ;
   float reg_graph_xr , reg_graph_yr ;

   /*-- Feb 2004 [rickr]: parser fields for GRAPH_EXPR command --*/
   PARSER_code * p_code ;                       /* parser expression code    */
   char          p_expr   [RT_MAX_EXPR+1] ;     /* user's parser expression  */
   double        p_atoz   [26] ;                /* source values to evaluate */
   int           p_has_sym[26] ;                /* symbol indices            */
   int           p_max_sym ;                    /* max index+1 of p_has_sym  */
   float       * reg_eval ;                     /* EXPR evaluation results   */

   /*-- Mar 2004 [rickr]: tcp comm fields for motion params (for Tom Ross) --*/
   int           mp_tcp_use ;     /* are we using tcp comm for motion params */
   int           mp_tcp_sd ;      /* socket descriptor                       */
   int           mp_port ;        /* destination port for motion params      */
   char          mp_host[128] ;   /* destination host for motion params      */
   int           mp_nmsg ;        /* count the number of sent messages       */
   int           mp_npsets ;      /* count the number of sent data lists     */

   /*-- Nov 2006 [rickr]: pass mask aves along with MP vals (for TRoss)    --*/
   byte *        mask ;           /* mask from g_mask_dset (from plugin)     */
   int           mask_nvals ;     /* number of non-zero mask values to use   */
   int           mask_nset ;      /* number of set voxels in mask            */
   int           mask_nones ;     /* number of val==1 voxels in max          */
   double *      mask_aves ;      /* averages over each mask value           */
   int           mask_init;    /* Cameorn Craddock       */

   /*-- Jul 2008 [rickr]: for oblique data                                 --*/
   int           is_oblique ;     /* flag: is the dataset oblique            */
   float         oblique_mat[16]; /* oblique transformation matrix           */

   double elapsed , cpu ;         /* times */
   double last_elapsed ;
   int    last_nvol ;

   int    num_note ;              /* 01 Oct 2002 */
   char **note ;

   int    marked_for_death ;      /* 10 Dec 2002 */

   /*-- Jun 2009 [RWCox]: merged multi-channel dataset --*/

   THD_3dim_dataset *mrg_dset ;
   int mrg_status ;               /* does AFNI know about this dataset yet? */
   int mrg_nvol ;                 /* number of volumes merged so far */

   THD_3dim_dataset * reg_chan_dset[MAX_CHAN];  /* 17 May 2010 [rickr]     */
   int reg_chan_status[MAX_CHAN];               /* does AFNI know of these */
   int reg_chan_mode;                           /* see RT_CM_RMODE_*       */
   char * chan_list_str;          /* list of channels in string format     */
   int  * chan_list;              /* list of channels  13 Jul 2010 [rickr] */

} RT_input ;

#define SHOW_TIMES                                              \
  fprintf(stderr,"RT: cpu time = %.2f  elapsed time = %.2f\n" , \
          PLUTO_cpu_time()-rtin->cpu , PLUTO_elapsed_time()-rtin->elapsed )

/**************************************************************************/

static char helpstring[] =
   " Purpose: Controlling realtime dataset acquisitions.\n"
   "\n"
   " USAGE:\n"
   " Set the controls to the state you want BEFORE realtime image input\n"
   " begins, then press one of the 'Set' buttons to send the control\n"
   " information to AFNI.\n"
   "\n"
   " INPUTS:\n"
   " Images Only = If 'No', then the input images will be used\n"
   "                 to assemble a dataset.  If 'Yes', then the\n"
   "                 input images will be displayed but not made\n"
   "                 into a dataset or otherwise saved.\n"
   "                 (They will only be stored in the image viewer\n"
   "                  that is opened - you can use 'Save:bkg' to\n"
   "                  write them to disk, if desired, before\n"
   "                  closing the 'Realtime Images' viewer window.)\n"
   "               If this input is 'Yes', then none of the other\n"
   "                 inputs below have much meaning!\n"
   "\n"
   " Root     = Root prefix to be used for new datasets.\n"
   "              The actual prefix will be of the form Root#001\n"
   "\n"
   " Update   = How often AFNI's display will be updated\n"
   "             = How many 3D bricks are gathered before\n"
   "               the images and graphs are redrawn:\n"
   "                0 --> don't update until all data is acquired\n"
   "                1 --> redraw images and graphs every time\n"
   "                2 --> redraw every other brick, etc.\n"
   "\n"
   " Function = Controls what kind of function analysis is done\n"
   "              during realtime input of time dependent datasets.\n"
   "\n"
   " Verbose  = If set to 'Yes', the plugin will print out progress\n"
   "              reports as information flows into it.\n"
   "              If set to 'Very', prints out LOTS of information.\n"
   "\n"
   " Registration = If activated, image registration will take place\n"
   "                  either in realtime or at the end of image acquisition.\n"
   "                * The un-registered dataset will be named something like\n"
   "                  Root#001 and the aligned dataset Root#001%reg2D.\n"
   "                * You can choose either 2D (slice-wise) registration,\n"
   "                  or 3D (volume-wise) registration.\n"
   "                * The 3D case also allows a quicker mode where the volumes\n"
   "                  aren't actually registered, but estimates of the motion\n"
   "                  parameters are computed for graphing purposes.  This is\n"
   "                  done in realtime.\n"
   "                * If 3D realtime or estimate modes are chosen, the motion\n"
   "                  parameters can be graphed in realtime - see 'Graph' below.\n"
   " Resampling   = Determines the interpolation method used:\n"
   "                  Cubic     = fastest, least accurate interpolation\n"
   "                  Quintic   = intermediate in speed and accuracy\n"
   "                  Heptic    = another intermediate interpolation method\n"
   "                  Fourier   = slowest, most accurate\n"
   "                  Hept+Four = use Heptic for estimation, Fourier for\n"
   "                              the final alignment\n"
   "              N.B.: Linear interpolation is always used for '3D: estimate'.\n"
   "                    Quintic/Heptic interpolation is only available for 3D.\n"
   "\n"
   " Reg Base     = If activated, the user choose the base dataset and image\n"
   "                  for registration.  Reg Base allows 3 choices of which\n"
   "                  choice the Base Image index applies to:\n"
   "                    Current Run        : next RT dataset to be acquired\n"
   "                    Current Run & Keep : same, and store for future runs\n"
   "                    External Dataset   : choose a specific 'Extern Dset'\n"
   "                  It is likely that one would choose Current & Keep, as\n"
   "                  then all runs would be aligned to the first one, rather\n"
   "                  than each run aligned to its own Base Image.\n"
   "                * Note that the Base Image index also applies to Extern.\n"
   "                * Note that using Current for the first run, and then\n"
   "                  choosing that dataset as Extern for future runs should\n"
   "                  be identical to using Current & Keep to begin with.\n"
   " Extern Dset  = The dataset to apply if Reg Base is 'External Dataset'\n"
   "                  For other Reg Base choices, this is ignored.\n"
   "                * Note that the Base Image index also applies to Extern.\n"
   " Base Image   = The value sets the time index in the Reg Base dataset to\n"
   "                  which the alignment will take place.\n"
   " Src Chan     = For multi-channel/echo data, specify which channel will\n"
   "                  used to drive registration.\n"
   "\n"
   " Graph        = If set to 'Yes', will display a graph of the estimated\n"
   "                  motion parameters at the end of the run.\n"
   "                * If set to 'Realtime', will also show a cruder graph that\n"
   "                  alters in realtime (if realtime registration is active).\n"
   " NR [x-axis]  = If a realtime graph is generated, this entry specifies\n"
   "                  how many repetitions (TR's) to expect.\n"
   " YR [y-axis]  = If a realtime graph is generated, this entry specifies\n"
   "                  the vertical range for each motion parameter.\n"
   "\n"
   " Mask         = Specify a mask for sending extra information to external\n"
   "                port (e.g. to serial_helper or realtime_receiver.py),\n"
   "                based on 'Vals to Send'.\n"
   "\n"
   " Vals to Send = Controls sending extra data to serial_helper TCP port\n"
   "                (perhaps to serial_helper or realtime_receiver.py).\n"
   "\n"
   "                  None             : do not send extra information\n"
   "                  Motion Only      : send only the 6 motion parameters\n"
   "                  ROI Means        : also send a mean for each Mask ROI\n"
   "                  All Data (heavy) : send all Mask voxel data, with extra\n"
   "                                   - per voxel send 8 values:\n"
   "                                     1D_index i j k x y z value\n"
   "                  All Data (light) : send all Mask voxel data, no extras\n"
   "                                   - per voxel send 1 value\n"
   "                  ROIs and data    : send ROIs averages, plus raw data\n"
   "                                   - per non-1 mask ROI, send average\n"
   "                                   - per ==1 mask voxel, send raw value\n"
   "                                     (mixing ROI means and Data (light))\n"
   "\n"
   "                * Sending vals requires '3D realtime registration', as it\n"
   "                  is based on the 3D registration parameters and the\n"
   "                  registered data.\n"
   "\n"
   "                --> see 'Mask' and 'Registration', above\n"
   "                --> see example F from 'Dimon -help'\n"
   "                --> see 'realtime_receiver.py -help'\n"
   "\n"
   " ChannelMerge = Turn on method for merging multi-channel information.\n"
   "                * Has no effect if only one channel of data is sent.\n"
   "                * Only works if the data channels are float or complex\n"
   "                  data (doesn't work with shorts or bytes).\n"
   "                   * except: Opt Comb works on short data\n"
   "                * Creates an extra '_mrg' dataset.\n"
   "                * none     ==> don't combine channels at all\n"
   "                  sum      ==> add channels [output is float or complex]\n"
   "                  L1 norm  ==> sum(absolute values) [output is float]\n"
   "                  L2 norm  ==> sqrt(sum of squares) [output is float]\n"
   "                  T2* est  ==> sqrt(sum of squares) [output is float]\n"
   "                  Opt Comb ==> 'Optimally Combine' the echoes\n"
   "                     - needs 'T2* Ref Dset' specified\n"
   "                     - registration probably needs extern dset, as EPI\n"
   "                       should end up aligned to T2* reference\n"
   "                     - 1. register echoes (channels) based on 'Src Chan'\n"
   "                     - 2. applies reg params to all echoes\n"
   "                     - 3. merge using OC method to weight the echoes\n"
   "MergeRegister = Turn on method for registering ChannelMerge dataset.  So\n"
   "                  instead of a registered channel 0 dataset, merge the\n"
   "                  channels via the ChannelMerge method, then apply the\n"
   "                  same xform to each channel.\n"
   "                  Then all channels should be in register.\n"
   "                * Requires application of ChannelMerge\n"
   "                * Created reg3D dset is registered ChannelMerge dataset\n"
   "                * Creates chanreg dsets if MergeReg set to reg channels\n"
   "                MergeRegister values:\n"
   "                    none         ==> no merge registration\n"
   "                    reg merged   ==> register merged channels\n"
   "                    reg channels ==> apply merge xform to all channels,\n"
   "                       - register all channels based on 'Src Chan'\n"
   "                       - apply registration params to all chan/echoes\n"
   "                         i.e. create 'registered' dataset per channel\n"
   "                       - merge the registered channels\n"
   " Chan List    = Select a list of channels to merge (otherwise use all).\n"
   "                  Use sub-brick style syntax to select which channels to\n"
   "                  merge, e.g. 0..$(2) would be the even channels, and\n"
   "                  0,3,7..12 would mean channels 0,3,7,8,9,10,11,12.\n"
   "                * Note that these are 0-based channel indices.\n"
   "\n"
   " RT Write     = Turns on real time writing of individual time point data\n"
   "                  sets (either the acquired or volume registered data) to\n"
   "                  disk.\n"
   "                    Off        : do nothing [default]\n"
   "                    Acquired   : write each volume as it is acquired\n"
   "                    Registered : write each registered volume\n"
   "                    Merged     : write each merged volume\n"
   "                                 (after being merged across channels)\n"
   " T2* Ref Dset = Specify a single volume T2* dataset to be used for the\n"
   "                  Opt Comb (optimally combined) ChannelMerge method.\n"
   "\n"
   " Detrend      = <insert useful information here>\n"
   "\n"
   "                * Currently, Mask ROI detrending comes from the 'Mask'\n"
   "                  block above.  That will change.  In the future, ROI\n"
   "                  average sending (Vals to Send) should be possible after\n"
   "                  (separate) ROI detrending.\n"
   "\n"
   "MULTICHANNEL ACQUISITION [Aug 2002]:\n"
   " Multiple image channels can be acquired (e.g., from multi-coil and/or\n"
   " multi-echo sequences).  Each image channel goes into a separate dataset.\n"
   "  * These datasets will have names like Root#007_03 (for the 3rd channel\n"
   "    in the 7th acquisition).\n"
   "  * Functional activation cannot be computed with multichannel acquisition.\n"
   "  * Registration CAN be computed with multichannel acquisition.\n"
   "\n"
   "HOW TO SEND DATA:\n"
   " See file README.realtime for details on control information.\n"
   " See program rtfeedme.c for an example of how to send realtime data.\n"
   " See program Dimon for another example of how to send realtime data.\n"
   "\n"
;

/** variables encoding the state of options from the plugin interface **/

static char root[THD_MAX_PREFIX] = "rt." ;  /* default prefix */
static int  update               = 1  ;     /* default update frequency */

#define NFUNC  2
static char * FUNC_strings[NFUNC] = { "None" , "FIM" } ;
static int func_code = 0 ;

static int image_mode = 0 ;  /* 28 Apr 2000 */

#define FUNC_NONE  0  /* symbolic values for the func_code */
#define FUNC_RTFIM 1

#define INIT_MODE     -1  /* modes for RT_fim_recurse (AKA func_func) */
#define UPDATE_MODE   -2
#define FINAL_MODE    -3

int RT_fim_recurse( RT_input * , int ) ;

int_func * FUNC_funcs[NFUNC] = { NULL , (int_func *)RT_fim_recurse } ;

#define NYESNO 2
#define NVERB  3
static char * VERB_strings[NVERB] = { "No" , "Yes" , "Very" } ;
static int verbose = 1 ;

  static int regtime  = 3 ;  /* index of base time for registration */
  static int regmode  = 0 ;  /* index into REG_strings */
  static int reggraph = 0 ;  /* graphing mode */

  static int g_reg_src_chan = 0 ;  /* chan for reg source 27 Oct 2016 [rcr] */

  /* registriation base globals to apply in RT_main     27 Aug 2009 [rickr] */
  static int g_reg_base_mode = 0 ;           /* index into REG_BASE_strings */
  static THD_3dim_dataset * g_reg_base_dset = NULL ;  /* single volume dset */
  static THD_3dim_dataset * g_t2star_ref_dset = NULL ;  /* also a single volume dset */

  static int reg_resam = 1 ;    /* index into REG_resam_strings */
  static int   reg_nr  = 100 ;  /* units of TR */
  static float reg_yr  = 1.0 ;  /* mm and degrees */

#define NGRAPH 3
static char * GRAPH_strings[NGRAPH] = { "No" , "Yes" , "Realtime" } ;

#define NRESAM 5
  static char * REG_resam_strings[NRESAM] = {
     "Cubic" , "Quintic" , "Heptic" , "Fourier" , "Hept+Four" } ;
  static int REG_resam_ints[NRESAM] = {
     MRI_CUBIC , MRI_QUINTIC , MRI_HEPTIC , MRI_FOURIER , -666 } ;

# define NREG 6
  static char * REG_strings[NREG] = {
    "None" , "2D: realtime" , "2D: at end"
           , "3D: realtime" , "3D: at end" , "3D: estimate" } ;

  static char * REG_strings_ENV[NREG] = { /* ' ' -> '_'  17 May 2005 [rickr] */
    "None" , "2D:_realtime" , "2D:_at_end"
           , "3D:_realtime" , "3D:_at_end" , "3D:_estimate" } ;

# define RT_RBASE_MODE_CUR      0
# define RT_RBASE_MODE_CUR_KEEP 1
# define RT_RBASE_MODE_EXTERN   2
# define NREG_BASE 3
  static char * REG_BASE_strings[NREG_BASE] = {
    "Current Run", "Current Run & Keep", "External Dataset" } ;

  static char * REG_BASE_strings_ENV[NREG_BASE] = {
    "Current_Run", "Current_Run_Keep", "External_Dataset" } ;

#define N_RT_MASK_METHODS 6     /* 15 Jul 2008 [rickr] */
  static char * RT_mask_strings[N_RT_MASK_METHODS] = {
    "None" , "Motion Only", "ROI means" ,
    "All Data (heavy)" , "All Data (light)", "ROIs and data"} ;

/* RT mask mechods:

   None                 : nothing sent
   Motion_Only          : 6 motion parameters, per time point
   ROI_means            : 6 motion, plus the mean over each ROI index
   All_Data             : big: 6 motion plus index, ijk, xyz, value for each vox
   All_Data_light       : 6 motion, plus each voxel value (over mask)
   ROIs_and_data        : 6 motion,
                          each ROI average (for ROI index > 1)
                          each voxel value (for ROI index == 1)
*/
  static char * RT_mask_strings_ENV[N_RT_MASK_METHODS] = {
    "None" , "Motion_Only", "ROI_means" ,
    "All_Data", "All_Data_light", "ROIs_and_data"} ;

/* Variables to enable writing individual time-point volumes to disk */
#define RT_WRITE_NOTHING       0
#define RT_WRITE_ACQUIRED      1
#define RT_WRITE_REGISTERED    2
#define RT_WRITE_MERGED        3  /* 10 Jul 2009 */
#define N_RT_WRITE_MODES       4
  static char * RT_write_strings[N_RT_WRITE_MODES] = {"Off", "Acquired",
                                                      "Registered", "Merged"};
  static int    RTdatamode = 0 ;      /* mode of writing to disk in real time */
  static int    RT_written[MAX_CHAN]; /* num time points already written out */
  static int    RT_written_mrg = 0 ;  /* 10 Jul 2009 - RWCox */

/* Variables for multi-channel merger operation (if any) */

#define RT_CHMER_NONE      0
#define RT_CHMER_SUM       1
#define RT_CHMER_L1NORM    2
#define RT_CHMER_L2NORM    3
/* Begin FMRIF changes for RT T2* estimates and optimally combined data - VR */
#define RT_CHMER_T2STAREST 4
#define RT_CHMER_OPT_COMB  5
#define N_RT_CHMER_MODES   6
   static char *RT_chmrg_strings[N_RT_CHMER_MODES] =
          { "none" , "sum" , "L1 norm" , "L2 norm" , "T2* est", "Opt Comb"} ;
   static char *RT_chmrg_labels[N_RT_CHMER_MODES] =
          { "none" , "sum" , "L1" , "L2" , "T2star", "optComb" } ;
/* End FMRIF changes for RT T2* estimates - VR */
   static int RT_chmrg_mode  = 0 ;
   static int RT_chmrg_datum = -1 ;
   static char * RT_chmrg_list = NULL ;

   MRI_IMAGE * RT_mergerize( RT_input * , int ) ;
static int RT_merge( RT_input *, int, int ) ;  /* 28 Oct 2016 [rickr] */

/* merge before or after registration */
#define RT_CM_NO_MERGE          0
#define RT_CM_MERGE_BEFORE_REG  1
#define RT_CM_MERGE_AFTER_REG   2

/* variables for regisrtation of merged dataset  17 May 2010 [rickr] */
#define RT_CM_RMODE_NONE     0
#define RT_CM_RMODE_REG_MRG  1  /* register merged dataset           */
#define RT_CM_RMODE_REG_CHAN 2  /* also apply xform to raw channels  */
#define N_RT_CM_RMODES       3
/* Begin FMRIF changes - wrong variable used here ? - VR */
   static char *RT_chmrg_rmode_strings[N_RT_CM_RMODES] =
                { "none" , "reg merged" , "reg channels" } ;
   static char *RT_chmrg_rmode_labels[N_RT_CM_RMODES] =
                { "none" , "reg_merge" , "reg_chan" } ;
/* End FMRIF changes - wrong variable used here ? - VR */
   static int RT_chmrg_reg_mode = RT_CM_RMODE_NONE;

# define REGMODE_NONE      0
# define REGMODE_2D_RTIME  1
# define REGMODE_2D_ATEND  2
# define REGMODE_3D_RTIME  3
# define REGMODE_3D_ATEND  4
# define REGMODE_3D_ESTIM  5

# define REG_IS_2D(mm) ( (mm) == REGMODE_2D_RTIME || (mm) == REGMODE_2D_ATEND )

# define REG_IS_3D(mm) \
  ( (mm)==REGMODE_3D_RTIME || (mm)==REGMODE_3D_ATEND || (mm)==REGMODE_3D_ESTIM )

# define REG_MAKE_DSET(mm)                                   \
  ( (mm) == REGMODE_2D_RTIME || (mm) == REGMODE_2D_ATEND ||  \
    (mm) == REGMODE_3D_RTIME || (mm) == REGMODE_3D_ATEND   )

/* Cameron Craddock defines to support real-time detrending
   set up as bit masks to make it easier to support all
   permutations */
# define RT_DETREND_NONE    0x00  /* do not detrend in realtime */
# define RT_DETREND_MOTION  0x01  /* remove motion params */
# define RT_DETREND_GM      0x02  /* remove global mean */
# define RT_DETREND_MASK    0x04  /* remove mask averages */
# define RT_DETREND_FRISTON 0x08  /* remove Friston 24 regressors */
# define RT_DETREND_POLORT  0x10  /* remove polynomial regressor 
                                     determined by rtin->detrend_polort */
# define RT_DETREND_SMOOTH  0x20  /* spatially smooth data after detrend */ 

   static unsigned char rt_detrend_mode = 0;    /* initialize to no detrending */
   static int rt_detrend_polort = -1; /* no polort */
   static int rt_detrend_motion = 0;  /* no motion */
   static int rt_detrend_addort = 0;  /* no additional orts */
   static float rt_detrend_fwhm = 0.0; /* no smoothing */

# define N_RT_DETREND_MODES 4
   static char *RT_detrend_strings[N_RT_DETREND_MODES] =
                { "None" , "Global Mean (GM)", "Mask ROIs", "GM + Mask" };
# define N_RT_MOTION_DETREND_MODES 3
   static char *RT_detrend_motion_strings[N_RT_MOTION_DETREND_MODES] =
                { "None", "6 parameter" , "Friston 24 parameter" } ;
/* CC end modifications */ 

typedef struct {
   int     len;
   char ** list;
} rt_string_list;

# define RT_DRIVE_LIMIT 4096

  static int g_mask_val_type = 1 ;                /* RT_mask_strings index   */
  static int g_show_times = 0 ;                   /* show MP comm times      */
  static int g_MP_send_ver = 0 ;                  /* send MP version         */
  static char * g_mask_dset_name = NULL ;         /* strcpy(), from env      */
  static THD_3dim_dataset * g_mask_dset = NULL ;  /* mask aves w/ MP vals    */
  static rt_string_list drive_wait_list = {0,NULL} ;      /* DRIVE_WAIT list */

/* 12 Aug 2008: allow popups to be disabled [jwhite] */
static int doPopups = TRUE;

/************ global data for reading data *****************/

static IOCHAN * ioc_control = NULL ;     /* control channel */
static RT_input * rtinp = NULL ;         /* only read 1 stream at a time */
static PLUGIN_interface * plint = NULL ; /* AFNI plugin structure */

/************ prototypes ***********/

DEFINE_PLUGIN_PROTOTYPE

char * RT_main( PLUGIN_interface * ) ;
RwcBoolean RT_worker( XtPointer ) ;
RT_input * new_RT_input( IOCHAN * ) ;
int RT_check_listen(void) ;
int RT_acquire_info( char * ) ;
int RT_process_info( int , char * , RT_input * ) ;
void RT_start_dataset( RT_input * ) ;
void RT_read_image( RT_input * , char * ) ;
int RT_process_data( RT_input * ) ;
void RT_process_image( RT_input * ) ;
void RT_finish_dataset( RT_input * ) ;
void RT_tell_afni( RT_input * , int ) ;
void RT_start_child( RT_input * ) ;
void RT_check_info( RT_input * , int ) ;
void RT_process_xevents( RT_input * ) ;  /* 13 Oct 2000 */

void RT_tell_afni_one( RT_input * , int , int ) ;  /* 01 Aug 2002 */

/* Cameron Craddock prototype function for real-time detrend */
void RT_detrend( RT_input * rtin, int mode );
void RT_detrend_getenv( RT_input * rtin );
/* CC end */

/* registration, RT_mp and related */
void RT_registration_2D_atend( RT_input * rtin ) ;
void RT_registration_2D_setup( RT_input * rtin ) ;
void RT_registration_2D_close( RT_input * rtin ) ;
void RT_registration_2D_onevol( RT_input * rtin , int tt ) ;
void RT_registration_2D_realtime( RT_input * rtin ) ;

void RT_registration_3D_atend( RT_input * rtin ) ;
void RT_registration_3D_setup( RT_input * rtin ) ;
void RT_registration_3D_close( RT_input * rtin ) ;
void RT_registration_3D_onevol( RT_input * rtin , int tt ) ;
void RT_registration_3D_realtime( RT_input * rtin ) ;

int  RT_registration_set_vr_base(RT_input * rtin);

static int  RT_mp_comm_alive    ( int, int, char * );
static int  RT_mp_comm_close    ( RT_input * rtin, int );
static int  RT_mp_comm_init     ( RT_input * rtin );
static int  RT_mp_comm_init_vars( RT_input * rtin );
static int  RT_mp_comm_send_data( RT_input * rtin, float *mp[6],int nt,int sub);
static int  RT_mp_comm_send_data_wrapper(RT_input * rtin, int tfirst, int ntt);

/* Cameron Craddock, init masking without setting up 
   communication */
static int RT_mp_init_mask( RT_input * rtin );

static int  RT_mp_check_env_for_mask( void );
static int  RT_mp_rm_env_mask   ( void );
static int  RT_mp_get_mask_aves ( RT_input * rtin, int sub );
static int  RT_mp_set_mask_data ( RT_input * rtin, float * data, int sub );
static int  RT_mp_set_mask_data_light ( RT_input * rtin, float * data, int sub);
static int  RT_mp_set_mask_aves_n_data( RT_input * rtin, float * data, int sub);

static int  RT_mp_mask_free     ( RT_input * rtin );  /* 10 Nov 2006 [rickr] */

static int  RT_mp_getenv        ( void );
static int  RT_mp_show_time     ( char * mesg ) ;
static int  RT_show_duration    ( char * mesg ) ;
static int  RT_when_to_merge    ( void );
static int  RT_will_register_merged_dset(RT_input * rtin);    /* 27 Oct 2016 */

/* string list functions (used for DRIVE_WAIT commands) */
static int add_to_rt_slist    ( rt_string_list * slist, char * str );
static int free_rt_string_list( rt_string_list * slist );
static int rt_run_drive_wait_commands( rt_string_list * slist );

static int remove_SM_suffix(char prefix[]);
static char * ends_at(char * text, char * suffix);

int  RT_parser_init         ( RT_input * rtin );
void RT_set_grapher_pinnums ( int pinnum );

void RT_test_callback(void *junk) ;      /* 01 Jun 2009 */

#define TELL_NORMAL  0
#define TELL_WRITE   1
#define TELL_FINAL   2

#define WRITE_INTERVAL 37.954  /* seconds */

#define REAP_CHILDREN
#ifdef REAP_CHILDREN
#  define GRIM_REAPER waitpid(-1,NULL,WNOHANG)
#else
#  define GRIM_REAPER /* nada */
#endif

#define USE_RT_STARTUP
#ifdef  USE_RT_STARTUP
/********************** Register a work process **********************/

static void RT_startup( XtPointer junk )
{
   PLUTO_register_workproc( RT_worker , NULL ) ;
   return ;
}
#endif

/***********************************************************************
   Set up the interface to the user
   09 Oct 2000: allow all options to be initialized via the environment
************************************************************************/

PLUGIN_interface * PLUGIN_init( int ncall )
{
   char * ept ; /* 09 Oct 2000 */

   if( ncall > 0 )        return NULL ;  /* only one interface */
   if( ! ALLOW_realtime ) return NULL ;  /* do nothing if not allowed */
   AFNI_block_rescan(1) ;                /* 10 Nov 2005 */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface("RT Options","Set Real-Time Acquisition Options",
                                helpstring , PLUGIN_CALL_VIA_MENU ,
                                (cptr_func *)RT_main  ) ;

   PLUTO_add_hint( plint , "Set Real-Time Acquisition Options" ) ;

   PLUTO_set_sequence( plint , "A:AArealtime" ) ;
   PLUTO_set_butcolor( plint , "hot" ) ;

   PLUTO_set_runlabels( plint , "Set+Keep" , "Set+Close" ) ;  /* 04 Nov 2003 */

   /*-- 28 Apr 2000: Images Only mode --*/

   ept = getenv("AFNI_REALTIME_Images_Only") ;  /* 09 Oct 2000 */
   if( ept != NULL ){
      int ii = PLUTO_string_index( ept , NYESNO , VERB_strings ) ;
      if( ii >= 0 && ii < NYESNO ) image_mode = ii ;
   }

   PLUTO_add_option( plint , "" , "Mode" , FALSE ) ;
   PLUTO_add_string( plint , "Images Only" , NYESNO,VERB_strings,image_mode ) ;

   /*-- next line of input: Prefix for output dataset --*/

   ept = getenv("AFNI_REALTIME_Root") ;        /* 09 Oct 2000 */
   if( !THD_filename_pure(ept) ) ept = NULL ;
   if( ept != NULL ) MCW_strncpy(root,ept,THD_MAX_PREFIX) ;

   PLUTO_add_option( plint , "" , "Root" , FALSE ) ;
   PLUTO_add_string( plint , "Root" , 0, (ept!=NULL) ? &ept : NULL , 19 ) ;

   /*-- next line of input: Update frequency --*/

   ept = getenv("AFNI_REALTIME_Update") ;      /* 09 Oct 2000 */
   if( ept != NULL ){
      int ii = (int) rint(strtod(ept,NULL)) ;
      if( ii >= 0 && ii <= 19 ) update = ii ;
   }

   PLUTO_add_option( plint , "" , "Update" , FALSE ) ;
   PLUTO_add_number( plint , "Update" , 0,19,0 , update , FALSE ) ;

   /*-- next line of input: Function computation --*/

   ept = getenv("AFNI_REALTIME_Function") ;   /* 09 Oct 2000 */
   if( ept != NULL ){
      int ii = PLUTO_string_index( ept , NFUNC , FUNC_strings ) ;
      if( ii >= 0 && ii < NFUNC ) func_code = ii ;
   }

   PLUTO_add_option( plint , "" , "Function" , FALSE ) ;
   PLUTO_add_string( plint , "Function" , NFUNC , FUNC_strings , func_code ) ;

   /*-- next line of input: Verbosity flag --*/

   ept = getenv("AFNI_REALTIME_Verbose") ;   /* 09 Oct 2000 */
   if( ept != NULL ){
      int ii = PLUTO_string_index( ept , NVERB , VERB_strings ) ;
      if( ii >= 0 && ii < NVERB ) verbose = ii ;
   }

   /* Cameron Craddock added to allow show times to be set by 
      GUI interface */
   ept = getenv("AFNI_REALTIME_SHOW_TIMES");
   if( ept != NULL ){
      int ii = PLUTO_string_index( ept , NYESNO , VERB_strings ) ;
      if( ii >= 0 && ii < NYESNO ) g_show_times = ii ;
   }

   PLUTO_add_option( plint , "" , "Verbose" , FALSE ) ;
   PLUTO_add_string( plint , "Verbose" , NVERB , VERB_strings , verbose ) ;
   PLUTO_add_string( plint , "Show Times" , NYESNO, VERB_strings, g_show_times ) ;

   /*-- next line of input: registration mode --*/

   ept = getenv("AFNI_REALTIME_Registration") ;  /* 09 Oct 2000 */
   if( ept != NULL ){
      int ii = PLUTO_string_index( ept , NREG , REG_strings ) ;
      /* on failure, try alternate forms               17 May 2005 [rickr] */
      if ( ii < 0 ) ii = PLUTO_string_index( ept , NREG , REG_strings_ENV ) ;
      if( ii >= 0 && ii < NREG ) regmode = ii ;
   }

   ept = getenv("AFNI_REALTIME_Resampling") ;    /* 09 Oct 2000 */
   if( ept != NULL ){
      int ii = PLUTO_string_index( ept , NRESAM , REG_resam_strings ) ;
      if( ii >= 0 && ii < NRESAM ) reg_resam = ii ;
   }

   PLUTO_add_option( plint , "" , "Registration" , FALSE ) ;
   PLUTO_add_string( plint , "Registration" , NREG , REG_strings , regmode ) ;
   PLUTO_add_string( plint , "Resampling",NRESAM, REG_resam_strings, reg_resam);
   PLUTO_add_hint  ( plint , "resampling method for registered data");

   /*-- registration base type, dset and index     27 Aug 2009 [rickr] --*/

   ept = getenv("AFNI_REALTIME_Reg_Base_Mode") ;  /* 04 Sep 2009 [rickr] */
   if( ept != NULL ){
      int ii = PLUTO_string_index( ept , NREG_BASE , REG_BASE_strings ) ;
      if (ii < 0) ii = PLUTO_string_index(ept, NREG_BASE, REG_BASE_strings_ENV);
      if( ii >= 0 && ii < NREG_BASE ) g_reg_base_mode = ii ;
   }

   ept = getenv("AFNI_REALTIME_Base_Image") ;     /* 09 Oct 2000 */
   if( ept != NULL ){
      int ii = (int) rint(strtod(ept,NULL)) ;
      if( ii >= 0 && ii <= 9999 ) regtime = ii ;
   }

   ept = getenv("AFNI_REALTIME_Reg_Source_Chan") ;    /* 27 Oct 2016 */
   if( ept != NULL ){
      int ii = (int) rint(strtod(ept,NULL)) ;
      if( ii >= 0 && ii <= 9999 ) g_reg_src_chan = ii ;
   }

   PLUTO_add_option(plint, "" , "Registration Base" , FALSE ) ;
   PLUTO_add_hint  (plint, "choose registration base dataset and sub-brick");
   PLUTO_add_string(plint, "Reg Base", NREG_BASE, REG_BASE_strings,
                           g_reg_base_mode );
   PLUTO_add_hint  (plint, "registration base dataset, and whether to store");
   PLUTO_add_dataset( plint , "Extern Dset", ANAT_ALL_MASK, FUNC_ALL_MASK,
                                      DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_hint( plint , "choose mask dataset for serial_helper" ) ;
   PLUTO_add_number(plint, "Base Image" , 0,9999,0 , regtime , TRUE ) ;
   PLUTO_add_hint  (plint, "registration base dataset index");
   PLUTO_add_number(plint, "Src Chan" , 0,9999,0 , g_reg_src_chan , TRUE ) ;
   PLUTO_add_hint  (plint, "registration source channel");


   /*-- next line of input: registration graphing --*/

   ept = getenv("AFNI_REALTIME_Graph")  ;  /* 09 Oct 2000 */
   if( ept != NULL ){
      int ii = PLUTO_string_index( ept , NGRAPH , GRAPH_strings ) ;
      if( ii >= 0 && ii < NGRAPH ) reggraph = ii ;
   }

   ept = getenv("AFNI_REALTIME_NR") ;     /* 09 Oct 2000 */
   if( ept != NULL ){
      int ii = (int) rint(strtod(ept,NULL)) ;
      if( ii >= 5 && ii <= 9999 ) reg_nr = ii ;
   }

   ept = getenv("AFNI_REALTIME_YR") ;     /* 09 Oct 2000 */
   if( ept != NULL ){
      float ff = strtod(ept,NULL) ;
      if( ff > 0.0 ) reg_yr = ff ;
   }

   PLUTO_add_option( plint , "" , "Graphing" , FALSE ) ;
   PLUTO_add_string( plint , "Graph" , NGRAPH , GRAPH_strings , reggraph ) ;
   PLUTO_add_number( plint , "NR [x-axis]" , 5,9999,0 , reg_nr , TRUE ) ;
   PLUTO_add_number( plint , "YR [y-axis]" , 1,100,1 , (int)(reg_yr*10.0),TRUE);

   /* see AFNI_REALTIME_Mask_Dset for setting Mask dset */

   /* try to set the mask vals option from the env */
   ept = getenv("AFNI_REALTIME_Mask_Vals")  ;  /* 15 Jul 2008 [rickr] */
   if( ept != NULL ){
      int ii = PLUTO_string_index(ept, N_RT_MASK_METHODS, RT_mask_strings_ENV);
      if( ii >= 0 && ii < N_RT_MASK_METHODS ) g_mask_val_type = ii;
   }

   /* but if no HOST_PORT, it must be cleared */
   ept = getenv("AFNI_REALTIME_MP_HOST_PORT") ;
   if( ept == NULL && g_mask_val_type ) {
      fprintf(stderr,"** RTM: clearing Mask_Vals setting from %s\n"
                     "        as AFNI_REALTIME_MP_HOST_PORT is unset\n",
              (g_mask_val_type >= 0 && g_mask_val_type <= N_RT_MASK_METHODS) ?
              RT_mask_strings_ENV[g_mask_val_type] : "INVALID");
      g_mask_val_type = 0;
   }

   /* mask dataset option line   10 Nov 2006 [rickr] */
   PLUTO_add_option( plint , "" , "Masking" , FALSE ) ;
   PLUTO_add_dataset( plint , "Mask", ANAT_ALL_MASK, FUNC_ALL_MASK,
                                      DIMEN_3D_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_hint( plint , "choose mask dataset for serial_helper" ) ;
   PLUTO_add_string( plint , "Vals to Send" , N_RT_MASK_METHODS ,
                             RT_mask_strings , g_mask_val_type ) ;
   PLUTO_add_hint( plint , "choose which mask data to send to serial_helper" ) ;

   /*-- Adding options for merging channels --*/

   PLUTO_add_option( plint , "" , "ChannelMerging" , FALSE ) ;

   /* channel merge modes: 02 Jun 2009 */

   RT_chmrg_mode = (int)AFNI_numenv("AFNI_REALTIME_CHMERMODE") ;
   if( RT_chmrg_mode < 0 || RT_chmrg_mode >= N_RT_CHMER_MODES ) RT_chmrg_mode=0;
   PLUTO_add_string( plint , "ChannelMerge" , N_RT_CHMER_MODES ,
                              RT_chmrg_strings , RT_chmrg_mode ) ;

   /* channel merge registration modes: 17 May 2010 [rickr] */

   RT_chmrg_reg_mode = (int)AFNI_numenv("AFNI_REALTIME_CM_REG_MODE") ;
   if( RT_chmrg_reg_mode < 0 || RT_chmrg_reg_mode >= N_RT_CM_RMODES )
     RT_chmrg_reg_mode = 0 ;
   PLUTO_add_string( plint , "MergeRegister" , N_RT_CM_RMODES ,
                              RT_chmrg_rmode_strings , RT_chmrg_reg_mode ) ;

   /* channel list, using sub-brick syntax    13 Jul 2010 [rickr] */
   ept = getenv("AFNI_REALTIME_MRG_CHANLIST") ;
   if( ept ) {
      if( RT_chmrg_list ) free(RT_chmrg_list);
      RT_chmrg_list = nifti_strdup(ept);
   }

   PLUTO_add_string( plint , "Chan List" , 0, (ept!=NULL) ? &ept : NULL , 13 ) ;

   /*---------------------------------------------------------*/
   /*-- a line to put misc options: RT writing and T2* dset --*/

   /* initialize g_t2star_ref_dset - 
    * it can be overwritten in the plugin  23 Jan 2017 */
   /* rcr - but if coming through the plugin, do not do this */
   /* rcr - fix (if we allow this, free an old dataset of this sort)*/
   ept = getenv("AFNI_REALTIME_T2star_ref");
   if( ept ) {
      /* remove any old one, first */
      if( g_t2star_ref_dset ) DSET_delete(g_t2star_ref_dset);
      g_t2star_ref_dset = THD_open_dataset(ept);
      if( g_t2star_ref_dset )
         fprintf(stderr,"== RTMerge: have T2star_ref from env, %s\n", ept);
      else
         fprintf(stderr,"** RTMerge: bad T2star_ref from env, %s\n", ept);
   } else
         fprintf(stderr,"-- no T2star_ref from env\n");

   PLUTO_add_option( plint , "" , "MiscOptions" , FALSE ) ;

   /*-- Adding options for writing individual time-point volumes to disk --*/
   RTdatamode = (int)AFNI_numenv("AFNI_REALTIME_WRITEMODE") ;
   if( RTdatamode < 0 || RTdatamode >= N_RT_WRITE_MODES ) RTdatamode = 0 ;
   PLUTO_add_string( plint , "RT Write" , N_RT_WRITE_MODES, RT_write_strings,
                     RTdatamode ) ;

   /*-- Filling this line out with T2* reference data set --*/

   PLUTO_add_dataset( plint , "T2* Ref Dset", ANAT_ALL_MASK, FUNC_ALL_MASK,
                                      DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_hint  (plint, "Reference data set with T2* values.");



   /* Cameron Craddock added to support real time detrend */
   ept = getenv("AFNI_REALTIME_DETREND_MODE") ;   
   if( ept != NULL ){
      int ii = (int) rint(strtod(ept,NULL)) ;
      if( ii >= 0 && ii <= 32 ) rt_detrend_mode = ii ;
   }
  
   /* convert the detrend node to the appropriate string
      array indices */
   rt_detrend_addort=0;
   if ( rt_detrend_mode & RT_DETREND_GM ) rt_detrend_addort+=1;
   if ( rt_detrend_mode & RT_DETREND_MASK ) rt_detrend_addort+=2;

   rt_detrend_motion=0;
   if ( rt_detrend_mode & RT_DETREND_MOTION ) rt_detrend_motion+=1;
   if ( rt_detrend_mode & RT_DETREND_FRISTON ) rt_detrend_motion+=1;
 
   ept = getenv("AFNI_REALTIME_DETREND_POLORT") ;  
   if( ept != NULL ){
      int ii = (int) rint(strtod(ept,NULL)) ;
      if( ii >= -1 && ii <= 99 ) rt_detrend_polort = ii ;
   }

   ept = getenv("AFNI_REALTIME_DETREND_FWHM") ;   
   if( ept != NULL ){
      float ff = (float)(strtod(ept,NULL)) ;
      if( ff >= 0.0 ) rt_detrend_fwhm = ff ;
   }

   PLUTO_add_option( plint , "" , "Detrend" , FALSE ) ;
   PLUTO_add_string( plint , "Detrend", N_RT_DETREND_MODES, RT_detrend_strings, 
       rt_detrend_addort ) ;
   PLUTO_add_hint( plint , 
       "select orts you would like to including in the RT detrending model" ) ;
   PLUTO_add_string( plint , "Motion Orts", N_RT_MOTION_DETREND_MODES , 
       RT_detrend_motion_strings , rt_detrend_motion ) ;
   PLUTO_add_hint( plint , 
       "select motion parameters you would like to include" ) ;
   PLUTO_add_number( plint , "Polort" , -1,99,0 , rt_detrend_polort , TRUE ) ;
   PLUTO_add_hint( plint , 
       "select the degree of polynomial you would like to include"
       " -1 = none, 0 = mean, 1 = linear, etc... " ) ;
   PLUTO_add_number(plint, "FWHM (mm)", 0.0,99.0,0.0, rt_detrend_fwhm, TRUE) ;
   PLUTO_add_hint( plint , 
       "Set the FWHM for spatial smoothing, 0.0 = no smoothing" ) ;
   /* CC end */

   /***** Register a work process *****/

#ifndef USE_RT_STARTUP
   PLUTO_register_workproc( RT_worker , NULL ) ;
#else
   PLUTO_register_timeout( 1954 , (generic_func *)RT_startup , NULL ) ;
#endif

   /***** 12 Oct 2000: set the graphing geometry, if any *****/

   ept = getenv("AFNI_REALTIME_volreg_graphgeom") ;
   if( ept != NULL ){
      char *str = malloc(strlen(ept)+20) ;
      sprintf(str,"AFNI_tsplotgeom=%s",ept) ;
      putenv(str) ;
   }

   /***** set the doPopups flag *****/
   /* simplified as yes/no var; removed _No from var     2 Sep 2014 [rickr] */
   doPopups = AFNI_yesenv("AFNI_REALTIME_Popups") ; /* 12 Aug 2008 [jwhite] */

   /***** go home to mama (i.e., AFNI) *****/

   ALLOW_realtime = 1 ;  /* flag to AFNI that realtime work is started */
   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

char * RT_main( PLUGIN_interface * plint )
{
   char * tag , * str , * new_prefix ;
   static char buf[256] ;
   int    check_reg=0 ;  /* check for 3D RT reg vs mask data */

   if( plint == NULL )
      return "*********************\n"
             "RT_main:  NULL input\n"
             "*********************"  ;

   /** loop over input from AFNI **/

#if 0                         /* 09 Oct 2000: turn this "feature" off */
   regmode = REGMODE_NONE ;   /* no registration if not ordered explicitly */
#endif

   while( (tag=PLUTO_get_optiontag(plint)) != NULL ){

      /* 28 Apr 2000: added "Images Only" mode */

      if( strcmp(tag,"Mode") == 0 ){
         str        = PLUTO_get_string(plint) ;
         image_mode = PLUTO_string_index( str , NYESNO , VERB_strings ) ;
         continue ;
      }

      if( strcmp(tag,"Root") == 0 ){
         new_prefix = PLUTO_get_string(plint) ;
         if( ! THD_filename_pure(new_prefix) )
            return "**************************\n"
                   "RT_main:  bad root prefix\n"
                   "**************************"  ;
         strcpy(root,new_prefix) ;
         continue ;
      }

      if( strcmp(tag,"Update") == 0 ){
         update = PLUTO_get_number(plint) ;
         continue ;
      }

      if( strcmp(tag,"Function") == 0 ){
         str       = PLUTO_get_string(plint) ;
         func_code = PLUTO_string_index( str , NFUNC , FUNC_strings ) ;
         continue ;
      }

      if( strcmp(tag,"Verbose") == 0 ){
         str     = PLUTO_get_string(plint) ;
         verbose = PLUTO_string_index( str , NVERB , VERB_strings ) ;
         g_show_times = PLUTO_string_index( str, NYESNO, VERB_strings );
         continue ;
      }

      if( strcmp(tag,"Registration") == 0 ){
         str       = PLUTO_get_string(plint) ;
         regmode   = PLUTO_string_index( str , NREG , REG_strings ) ;

         /* regtime   = PLUTO_get_number(plint); moved to Reg Base below... */

         str       = PLUTO_get_string(plint) ;
         reg_resam = PLUTO_string_index( str , NRESAM , REG_resam_strings ) ;
         check_reg = 1;  /* check for 3D RT reg vs mask data */
         continue ;
      }

      /* 2 Sep 2009 [rickr] */
      if( strcmp(tag,"Registration Base") == 0 ){
         MCW_idcode * idc ;
         if( g_reg_base_dset ) { /* since changing: nuke any existing dset */
            DSET_delete(g_reg_base_dset);
            g_reg_base_dset = NULL;
         }

         str             = PLUTO_get_string(plint) ;
         g_reg_base_mode = PLUTO_string_index(str, NREG_BASE, REG_BASE_strings);

         idc = PLUTO_get_idcode(plint) ;
         g_reg_base_dset = PLUTO_find_dset(idc);     /* might be NULL */

         regtime = PLUTO_get_number(plint) ;

         g_reg_src_chan = PLUTO_get_number(plint) ;  /* 27 Oct 2016 */

         /* if extern dset, copy single volume */
         if( g_reg_base_mode == RT_RBASE_MODE_EXTERN ) {
            if( ! g_reg_base_dset )
               return "**************************************\n"
                      "RT_opts: choose Reg Base 'Extern Dset'\n"
                      "**************************************" ;
            g_reg_base_dset = THD_copy_one_sub(g_reg_base_dset, regtime);
            regtime = 0;
            if( ! g_reg_base_dset ) {
               sprintf(buf, /* rely on being static */
                "*********************************************\n"
                "RT_opts: failed to load Extern Dset, index %d\n"
                "*********************************************", regtime);
               return buf;
            } else if ( verbose > 1 ) {
               fprintf(stderr,"RT: extracting reg_base_dset[%d]\n", regtime);
            }
         } else if ( g_reg_base_dset ) {
            fprintf(stderr,"** ignoring Reg Base Dset...\n");
            g_reg_base_dset = NULL;
         }

         continue ;
      }

      if( strcmp(tag,"Graphing") == 0 ){
         str      = PLUTO_get_string(plint) ;
         reggraph = PLUTO_string_index( str , NGRAPH , GRAPH_strings ) ;

         reg_nr   = PLUTO_get_number(plint) ;
         reg_yr   = PLUTO_get_number(plint) ;

         /* 12 Oct 2000: set pin_num on all graphs now open */

         if( reg_nr >= MIN_PIN && reg_nr <= MAX_PIN && IM3D_OPEN(plint->im3d) )
             RT_set_grapher_pinnums(reg_nr);

         continue ;
      }

      /* 10 Nov 2006 [rickr] */
      if( strcmp(tag,"Masking") == 0 ){
         MCW_idcode * idc = PLUTO_get_idcode(plint) ;
         g_mask_dset = PLUTO_find_dset(idc) ;
         str = PLUTO_get_string(plint) ;
         g_mask_val_type = PLUTO_string_index(str , N_RT_MASK_METHODS ,
                                                    RT_mask_strings ) ;

         if ( g_mask_val_type > 1 && !g_mask_dset )
            return "*************************\n"
                   "RT_opts: bad mask dataset\n"
                   "*************************" ;

         /* since this is now read from the env, we need to put it there */
         /* CC: changed this to _ENV */
         sprintf(buf, "AFNI_REALTIME_Mask_Vals %s",
                      RT_mask_strings_ENV[g_mask_val_type]);
         AFNI_setenv(buf);

         if (verbose)
            fprintf(stderr,"RTM: %s mask dataset, method '%s'\n",
                 g_mask_dset ? "found":"no", RT_mask_strings[g_mask_val_type]);

         RT_mp_rm_env_mask() ; /* delete any mask from env */

         check_reg = 1;  /* check for 3D RT reg vs mask data */

         continue ;
      }

      if( strcmp(tag,"ChannelMerging") == 0 ){
         str           = PLUTO_get_string(plint) ;
         RT_chmrg_mode = PLUTO_string_index( str , N_RT_CHMER_MODES,
                                                   RT_chmrg_strings ) ;
         str               = PLUTO_get_string(plint) ;
         RT_chmrg_reg_mode = PLUTO_string_index( str , N_RT_CM_RMODES,
                                                   RT_chmrg_rmode_strings ) ;
         if( RT_chmrg_list ) free(RT_chmrg_list);
         RT_chmrg_list = nifti_strdup(PLUTO_get_string(plint)) ;

         if (verbose)
            fprintf(stderr,"RT: mrg_mode = %s, reg_mode = %s, mrg_list = %s\n",
                    RT_chmrg_strings[RT_chmrg_mode],
                    RT_chmrg_rmode_strings[RT_chmrg_reg_mode],
                    RT_chmrg_list ? RT_chmrg_list : "<empty>");
         continue ;
      }

      if( strcmp(tag,"MiscOptions") == 0 ){
         MCW_idcode * idc;

         str        = PLUTO_get_string(plint) ;
         RTdatamode = PLUTO_string_index( str , N_RT_WRITE_MODES,
                                                RT_write_strings ) ;

         /* if coming through the plugin, afni already knows about the
          * dataset, so we do not need to delete it
          * (or we make a plugin copy, as with g_reg_base_dset) */
         idc = PLUTO_get_idcode(plint) ;
         g_t2star_ref_dset = PLUTO_find_dset(idc);     /* might be NULL */

         if (verbose)
            fprintf(stderr,
               "RTM: reg base mode '%s', index %d, dset %s, src chan %d,"
               " t2* ref %s\n",
               REG_BASE_strings[g_reg_base_mode], regtime,
               g_reg_base_dset ? "<found>" : "<empty>",
               g_reg_src_chan,
               g_t2star_ref_dset ? "<found>" : "<empty>");

         continue ;
      }

    /* Cameron Craddock added to support realtime detrending */
    if( strcmp(tag,"Detrend") == 0 )
    {
        rt_detrend_mode = 0;

        /* get the index of the detrend string for the additional ort */
        str = PLUTO_get_string(plint) ;
        rt_detrend_addort = PLUTO_string_index(str, N_RT_DETREND_MODES,
            RT_detrend_strings);

        /* convert the string index to a mode */
        if( rt_detrend_addort == 1 ) 
           rt_detrend_mode = rt_detrend_mode | RT_DETREND_GM; 
        if( rt_detrend_addort == 2 ) 
           rt_detrend_mode = rt_detrend_mode | RT_DETREND_MASK; 
        if( rt_detrend_addort == 3 ) 
           rt_detrend_mode = rt_detrend_mode | RT_DETREND_MASK | RT_DETREND_GM;

         /* CC get the index of the string for the motion regressors */
         str = PLUTO_get_string(plint);
         rt_detrend_motion = PLUTO_string_index(str, N_RT_MOTION_DETREND_MODES, 
                 RT_detrend_motion_strings);

         /* convert the string index to a mode */
         if( rt_detrend_motion == 1 )
             rt_detrend_mode = rt_detrend_mode | RT_DETREND_MOTION;
         if( rt_detrend_motion == 2 )
             rt_detrend_mode = rt_detrend_mode | RT_DETREND_FRISTON;

         rt_detrend_polort = (int)rint(PLUTO_get_number(plint)) ;
         rt_detrend_fwhm   = PLUTO_get_number(plint) ;

         /* CC if the user specifies detrend to MASK ROIs, make 
            sure that they also specified a MASK under masking,
            this assumes that masking is interpreted before detrend */
         if (( rt_detrend_mode & RT_DETREND_MASK ) && !g_mask_dset )
         {
             return "*******************************************************\n"
                    "RT_opts: RT detrend with Mask ORTs requires valid mask \n"
                    "           select one under Mask 'Choose Dataset' \n"
                    "*******************************************************" ;
         }

         /* CC if the user specifies detrend to motion, make 
            sure that they also specified RT motion correction,
            this assumes that motion correction is interpreted before detrend */
         if (( rt_detrend_mode & RT_DETREND_MOTION ) && 
             ( regmode != 1 ) && ( regmode != 3 ))
         {
             return "*******************************************************\n"
                    "RT_opts: RT detrend with motion ORTs requires real-time \n"
                    "         registration, set under Registration \n"
                    "*******************************************************" ;
         }

         /* CC now set the appropriate environment variables */
         snprintf( buf, 255, "AFNI_REALTIME_DETREND_MODE %d", rt_detrend_mode );
         AFNI_setenv( buf );
         snprintf( buf, 255, "AFNI_REALTIME_DETREND_POLORT %d",
                   rt_detrend_polort );
         AFNI_setenv( buf );
         snprintf( buf, 255, "AFNI_REALTIME_DETREND_FWHM %g", rt_detrend_fwhm );
         AFNI_setenv( buf );
         
         continue ;
      }
      /* CC end */

      /** How the hell did this happen? **/

      sprintf(buf,"*****************\n"
                  "Illegal optiontag: %s\n"
                  "*****************"      , tag ) ;
      return buf ;

   }  /* end of loop over active input options */

   if ( check_reg && g_mask_val_type > 0 && regmode != REGMODE_3D_RTIME ) {
      sprintf(buf, "******************************************\n"
                   "RT_opts: have RT mask method '%s',\n"
                   "         but no 3D realtime registration\n"
                   "         ==> will not process RT mask data\n" 
                   "******************************************",
                   RT_mask_strings[g_mask_val_type]);
      PLUTO_popup_transient(plint, buf);
   }

   /*-- 28 Apr 2000: if in Image Only mode, turn some stuff off --*/

   if( image_mode ){
      func_code = 0 ;
      regmode   = 0 ;
      reggraph  = 0 ;
   }

   PLUTO_turnoff_options( plint ) ;  /* 21 Feb 2001 */

   return NULL ;  /* nothing bad happened */
}

/***************************************************************************
   Listen for an incoming real-time data control connection.
   Return 1 if the connection is established,
   return 0 if no connection is ready, return -1 if an error occurs.
****************************************************************************/

int RT_check_listen(void)
{
   int jj ;
   char name[64];
   static int newcon = 1 ;

   /** see if we need to start listening **/

   if( ioc_control == NULL ){
      if( verbose )
         fprintf(stderr,"RT: starting to listen for control stream.\n") ;
      sprintf(name,"tcp:*:%d", get_port_named("AFNI_CONTROL_PORT"));
      ioc_control = iochan_init( name , "accept" ) ;
      newcon      = 1 ;
      if( ioc_control == NULL ){
         fprintf(stderr,"RT: can't listen for control stream\a\n") ;
         return -1 ;
      }
   }

   /** Check if the network channel is active **/

   jj = iochan_goodcheck(ioc_control,SHORT_DELAY) ;

   if( jj == 1 ){  /* external connection! */

      if( newcon ){
         fprintf(stderr,"RT:---------------------------------------\n") ;
         fprintf(stderr,"RT: connected to control stream %s\n",ioc_control->name) ;
         newcon = 0 ;
      } else {
/**
         if( verbose > 1 )
            fprintf(stderr,"RT: still waiting for control data.\n") ;
**/
      }

      if( ! TRUST_host(ioc_control->name) ){
         fprintf(stderr,"RT: untrusted host connection - closing!\a\n") ;
         fprintf(stderr,"==> set environment variable AFNI_TRUSTHOST to IP address\n") ;
         IOCHAN_CLOSENOW(ioc_control) ;
         return 0 ;
      }

      jj = iochan_readcheck(ioc_control,0) ;  /* is something ready to read? */

      if( jj > 0 && verbose > 1 ) fprintf(stderr,"RT: control data is present!\n") ;

      return jj ;

   } else if( jj == -1 ){  /* something bad! */

      fprintf(stderr,"RT: failure while listening for control stream!\a\n") ;
      IOCHAN_CLOSENOW(ioc_control) ;
      return 0 ;
   }

   return 0 ;  /* no external connection yet */
}

/***************************************************************************/
/**             macro to close down the realtime input stream             **/

#define CLEANUP(n) cleanup_rtinp(n)

/**------------ 01 Aug 1998: replace the macro with a function -----------**/

#undef  FREEUP
#define FREEUP(x) do{ if( (x) != NULL ){free((x)); (x)=NULL;} } while(0)

/* 10 Dec 2002: add keep_ioc_data flag,
                so as not to close the data channel;
                note that the pointer to it will be lost when rtinp is freed */

void cleanup_rtinp( int keep_ioc_data )
{
   int cc ;

   if( !keep_ioc_data )
     IOCHAN_CLOSENOW(rtinp->ioc_data) ;    /* close open I/O channels */

   IOCHAN_CLOSENOW(rtinp->ioc_info) ;

   if( rtinp->child_info > 0 )             /* destroy child process */
      kill( rtinp->child_info , SIGTERM ) ;

   DESTROY_IMARR(rtinp->bufar) ;           /* destroy any buffered images */

   for( cc=0 ; cc < MAX_CHAN ; cc++ ){     /* 01 Aug 2002: loop over channels */
     if( rtinp->sbr[cc] != NULL )
       free( rtinp->sbr[cc] ) ;            /* destroy buffered data */
   }

   if( rtinp->reg_2dbasis != NULL ){       /* destroy registration setup */
      int kk ;
      for( kk=0 ; kk < rtinp->nzz ; kk++ )
         mri_2dalign_cleanup( rtinp->reg_2dbasis[kk] ) ;
      free( rtinp->reg_2dbasis ) ;
   }

   if( rtinp->reg_3dbasis != NULL ){
      mri_3dalign_cleanup( rtinp->reg_3dbasis ) ;
   }

   FREEUP( rtinp->reg_tim   ) ; FREEUP( rtinp->reg_dx    ) ;
   FREEUP( rtinp->reg_dy    ) ; FREEUP( rtinp->reg_dz    ) ;
   FREEUP( rtinp->reg_phi   ) ; FREEUP( rtinp->reg_psi   ) ;
   FREEUP( rtinp->reg_theta ) ; FREEUP( rtinp->reg_rep   ) ;
   FREEUP( rtinp->reg_eval  ) ;

   if( rtinp->image_handle != NULL )
      PLUTO_imseq_rekill( rtinp->image_handle,NULL,NULL ) ;  /* 28 Apr 2000 */

   if( rtinp->image_space != NULL ){
      mri_clear_data_pointer(rtinp->image_space) ; mri_free(rtinp->image_space) ;
   }

   /* 01 Oct 2002: free stored notes */

   if( rtinp->num_note > 0 && rtinp->note != NULL ){
     int kk ;
     for( kk=0 ; kk < rtinp->num_note ; kk++ ) FREEUP( rtinp->note[kk] ) ;
     FREEUP(rtinp->note) ;
   }

   /* 13 Jul 2010 [rickr] : free channel list and string */
   FREEUP( rtinp->chan_list_str ) ;
   FREEUP( rtinp->chan_list ) ;

   free(rtinp) ; rtinp = NULL ;            /* destroy data structure */
   ioc_control = NULL ;                    /* ready to listen again */
   GRIM_REAPER ;                           /* reap dead child, if any */
}

/*********************************************************************
  Real time routine called every so often by Xt.
  This handles connections from other processes that want to
  send AFNI data.

  This is a work process -- it returns False if it wants to be
  called again, and return True if it is done.  For real-time
  AFNI-izing, this should always return False.
**********************************************************************/

RwcBoolean RT_worker( XtPointer elvis )
{
   int jj ;

#if 0
   if( first ){
      if( verbose ) fprintf(stderr,"RT: first call to RT_worker()\n") ;
      first = 0 ;
   }
#endif

   /**--------------------------------------------------------------------**/
   /** if we are waiting for a connection, check to see if it is good yet **/

   if( rtinp == NULL ){         /* not doing real-time input at this time */
      static int nerr = 0 ;
      jj = RT_check_listen() ;             /* see if someone wants to talk */
      if( jj < 0 ){                        /* quit if there's an error */
         nerr++ ; iochan_sleep(1) ;
         return (nerr > 9) ? True : False; /* 12 Oct 2000: if a lot of errors */
      }

      nerr = 0 ;                           /* reset error count */
      if( jj == 0 ) return False ;         /* try later if no connection now */
      rtinp = new_RT_input(NULL) ;         /* try to make a new input struct */
      IOCHAN_CLOSENOW( ioc_control ) ;     /* not needed any more */
      if( rtinp == NULL ) return False ;   /* try later (?) */

      /* rcr - here : add details     if (verbose > 1) */
   }

   /**---------------------------------------------------------------**/
   /**---- at this point, the rtinp struct is good,              ----**/
   /**---- meaning that we are in the business of acquiring data ----**/
   /**---------------------------------------------------------------**/

   /**-----------------------------------------------------------------------**/
   /** if input data stream has gone bad, close down the real-time operation **/

   if( iochan_goodcheck(rtinp->ioc_data,0) != 1 ){

      if( rtinp->sbr[0] != NULL ){     /* if we started acquisition */
         if( verbose > 1 )
            fprintf(stderr,"RT: data stream closed down.\n") ;
         VMCHECK ;
         RT_finish_dataset( rtinp ) ;  /* then we can finish it */
      } else {
         fprintf(stderr,"RT: data stream closed before dataset was fully defined!\a\n") ;
      }

      CLEANUP(0) ; return False ;
   }

   /**-----------------------------------------------------------------**/
   /**---- See if any data is waiting to be read from the child   ----**/
   /**---- process that is supposed to supply control information ----**/

#define CHILD_MAX_WAIT 66.6  /* seconds */

   if( rtinp->child_info > 0 ){

      jj = iochan_readcheck( rtinp->ioc_info , SHORT_DELAY ) ;  /** data ready? **/

      if( jj == 0 ){       /** 10 Dec 1998: child not sending data? **/
         double et = PLUTO_elapsed_time() - rtinp->child_start_time ;
         double mw = CHILD_MAX_WAIT ;
         char *eee = getenv("AFNI_REALTIME_CHILDWAIT") ;
         if( eee != NULL ){
            double val=strtod(eee,NULL); if(val >= 1.0) mw = val;
         }

         if( et > mw ){  /* don't wait any more, give up */
            fprintf(stderr,
                    "RT: no data from child after %f seconds!  Giving up.\a\n",et) ;
            CLEANUP(0) ; return False ;
         }

      } else if( jj < 0 ){   /** something bad happened? **/

         fprintf(stderr,"RT: child info stream closed prematurely!\a\n") ;
         CLEANUP(0) ; return False ;

      } else if( jj > 0 ){   /** something good happened? **/

         char * info = (char *) malloc( sizeof(char) * INFO_SIZE ) ;
         int   ninfo , ii ;

         /* read all data possible from the info channel */

         if( verbose )
            fprintf(stderr,"RT: receiving data from child process\n") ;
         VMCHECK ;

         ninfo = 0 ;
         while(1){
            ii = iochan_recv( rtinp->ioc_info , info+ninfo , INFO_SIZE-ninfo ) ;
            if( ii < 1 ) break ;
            ninfo += ii ;
            if( ninfo >= INFO_SIZE ){
               fprintf(stderr,"RT: child process sent too much info!\a\n") ;
               break ;
            }
            ii = iochan_readcheck( rtinp->ioc_info , SHORT_DELAY ) ;
            if( ii < 1 ) break ;
         }
         IOCHAN_CLOSENOW( rtinp->ioc_info ) ; rtinp->child_info = 0 ;

         if( ninfo <= 0 ){
            fprintf(stderr,"RT: child info stream returned no data!\a\n") ;
            CLEANUP(0) ; return False ;
         }

         if( verbose > 1 )
            fprintf(stderr,"RT: child info stream returned %d bytes.\n",ninfo) ;
         VMCHECK ;

         /* process the info and store it in the real-time struct */

         jj = RT_process_info( ninfo , info , rtinp ) ; free(info) ;

         /* check if data was processed OK */

         if( jj <= 0 ){
            fprintf(stderr,"RT: child info was badly formatted!\a\n") ;
            CLEANUP(0) ; return False ;
         }

         /* if we already received the first image data,
            then all the setup info should be OK.  if not, quit */

         if( ! rtinp->no_data && ! rtinp->info_ok ){
            fprintf(stderr,"RT: child info was incomplete or erroneous!\a\n") ;
            RT_check_info( rtinp , 1 ) ;
            if (doPopups){  /* 12 Aug 2008 [jwhite] */ 
              PLUTO_beep(); 
              PLUTO_popup_transient(plint, " \n" 
                    "      Heads down!\n" 
                    "Realtime header was bad!\n"); 
            }
            CLEANUP(0) ; return FALSE ;
         }

         /* if all the setup info is OK, we can create the dataset now */

         if( rtinp->sbr[0] == NULL && rtinp->info_ok ){
            if( verbose > 1 )
               fprintf(stderr,"RT: info complete --> creating dataset.\n") ;
            VMCHECK ;
            RT_start_dataset( rtinp ) ;
         }
      }

      /** if nothing happened (good or bad),
          will try to read it again on the next time thru **/
   }

   /**---------------------------------------------**/
   /** See if any image data is waiting to be read **/

   jj = iochan_readcheck( rtinp->ioc_data , SHORT_DELAY ) ;

   if( jj == 0 ){     /** no data available now **/

#ifdef WRITE_INTERVAL
      double delt ;
      double mw = WRITE_INTERVAL ;
      char *eee = getenv("AFNI_REALTIME_WRITEWAIT") ;
      if( eee != NULL ){
         double val=strtod(eee,NULL); if(val >= 1.0) mw = val;
      }

      /** if there is no image data for a long time,
          and there is something new to write out,
          then write the datasets out again.         **/

      if( !rtinp->no_data                    &&
          rtinp->nvol[0] > rtinp->last_nvol  &&
          !rtinp->image_mode                 &&  /* 28 Apr 2000 */
          (delt=PLUTO_elapsed_time()-rtinp->last_elapsed) > mw  ){

         int cmode , cc ;

         fprintf(stderr,"RT: no image data for %g seconds --> saving to disk.\n",delt) ;

         if (doPopups) { /* 12 Aug 2008 [jwhite] */ 
           PLUTO_popup_transient( plint , " \n" 
               " Pause in input data stream:\n" 
               " Saving current dataset(s) to disk.\n" ) ;
         }

         RT_tell_afni(rtinp,TELL_NORMAL) ;

         cmode = THD_get_write_compression() ;      /* 20 Mar 1998 */
         THD_set_write_compression(COMPRESS_NONE) ;
         SHOW_AFNI_PAUSE ;

         for( cc=0 ; cc < rtinp->num_chan ; cc++ ) {
           if( okay_to_add_markers(rtinp->dset[cc]) ) /* 13 Sep 2005 [rickr] */
              rtinp->dset[cc]->markers = create_empty_marker_set() ;
           DSET_overwrite( rtinp->dset[cc] ) ;
         }

         if( rtinp->func_dset != NULL )
            DSET_overwrite( rtinp->func_dset ) ;

         THD_set_write_compression(cmode) ;  sync() ; /* 08 Mar 2000: sync disk */
         SHOW_AFNI_READY ;

         rtinp->last_nvol    = rtinp->nvol[0] ;
         rtinp->last_elapsed = PLUTO_elapsed_time() ;
      }
#endif

      return False ;
   }

   if( jj < 0 ){               /** something bad happened to data channel **/
      if( verbose > 1 )
         fprintf(stderr,"RT: data stream closed down.\n") ;
      VMCHECK ;
      if( rtinp->sbr[0] != NULL ) RT_finish_dataset( rtinp ) ;
      CLEANUP(0) ; return False ;
   }

   /************************************************/
   /** If here, data is ready on the data channel **/

   /**-----------------------------------------------------**/
   /** If this is the first time reading the data channel, **/
   /** read and process any header info present there.     **/
   /** (Will read the image data a little farther below.)  **/

   if( rtinp->no_data ){
      int ii , nb ;

      /** if so ordered, start a child process to get header info **/

      if( strlen(rtinp->name_info) > 0 ) RT_start_child( rtinp ) ;

      /** read initial stuff from the data channel **/

      fprintf(stderr,"RT: receiving image metadata") ; fflush(stderr) ;

      nb = iochan_recv( rtinp->ioc_data , rtinp->buf , RT_NBUF ) ;

      if( nb <= 0 ){
         fprintf(stderr,"\nRT: recv on data stream fails on first try!\a\n") ;
         CLEANUP(0) ; return False ;
      }

      /* read any more that comes available very quickly [06 Aug 2002] */

      while( nb < RT_NBUF-1 ){
        ii = iochan_readcheck( rtinp->ioc_data , SHORT_DELAY ) ;
        if( ii <= 0 ) break ;
        ii = iochan_recv( rtinp->ioc_data , rtinp->buf+nb , RT_NBUF-nb ) ;
        if( ii <= 0 ) break ;
        nb += ii ;
      }
      rtinp->nbuf = nb ;

      fprintf(stderr,"=%d bytes\n",rtinp->nbuf) ;

      if (doPopups){  /* 12 Aug 2008 [jwhite] */ 
        PLUTO_beep() ; 
        PLUTO_popup_transient( plint , " \n" 
            "***************************\n" 
            "*       Heads Up!         *\n" 
            "* Incoming realtime data! *\n" 
            "***************************\n" ) ; 
      }

      g_show_times = AFNI_yesenv("AFNI_REALTIME_SHOW_TIMES") ;
      if( verbose > 1 ) g_show_times = 1;
      if( g_show_times ) RT_show_duration("starting to receive realtime data");

      /** process header info in the data channel;
          note that there must be SOME header info this first time **/

      jj = RT_process_info( rtinp->nbuf , rtinp->buf , rtinp ) ;

      if( jj <= 0 ){                  /* header info not OK */

         fprintf(stderr,"RT: initial image metadata is badly formatted!\a\n") ;
         CLEANUP(0) ; return False ;

      } else if( jj < rtinp->nbuf ){  /* data bytes left after header info */

         memmove( rtinp->buf , rtinp->buf + jj , rtinp->nbuf - jj ) ;
         rtinp->nbuf = rtinp->nbuf - jj ;

      } else {                        /* no data left after header info */
         rtinp->nbuf = 0 ;
      }

      if( verbose > 1 )
         fprintf(stderr,
                 "RT: processed %d bytes of header info from data stream\n",jj) ;
      VMCHECK ;

      rtinp->no_data = 0 ;  /* can't say we never received data */

      /* if we have already received the child process info,
         or if there is no child process info, then all should be OK now */

      if( rtinp->child_info == 0 && ! rtinp->info_ok ){
         fprintf(stderr,"RT: image header info was incomplete!\a\n") ;
         fprintf(stderr,"==> check image source program and README.realtime\n");
         RT_check_info( rtinp , 1 ) ;
         if (doPopups){ /* 12 Aug 2008 [jwhite] */ 
           PLUTO_beep() ; 
           PLUTO_popup_transient( plint , " \n" 
               "      Heads down!\n" 
               "Realtime header was bad!\n" ) ; 
         }
         CLEANUP(0) ; return FALSE ;
      }
   }

   /**-------------------------------**/
   /** At last -- process image data **/

   jj = RT_process_data( rtinp ) ;

   if( jj < 0 ){
      fprintf(stderr,"RT: data stream aborted during image read!\n") ;
      if( rtinp->sbr[0] != NULL ) RT_finish_dataset( rtinp ) ;
      CLEANUP(0) ; return False ;
   }

   /* 10 Dec 2002: if data stream marked itself for death,
                   then close down the dataset,
                   but don't close down the data stream itself;
                   instead, create a new RT_input context for
                   reading new metadata+image data from the
                   same data stream                          */

   if( rtinp->marked_for_death ){
      RT_input *new_rtinp ;
      fprintf(stderr,"RT: data stream says to close dataset.\n") ;
      if( rtinp->sbr[0] != NULL ) RT_finish_dataset( rtinp ) ;
      fprintf(stderr,"RT: starting to read new data from existing data stream.\n");
      new_rtinp = new_RT_input( rtinp->ioc_data ) ; /* new RT_input context */
      CLEANUP(1) ;                                 /* will delete old rtinp */
      rtinp = new_rtinp ; return False ;
   }

   /*** continue normally! ***/

   rtinp->last_elapsed = PLUTO_elapsed_time() ;  /* record this time */
   return False ;
}

/*---------------------------------------------------------------------
   13 Oct 2000: process some X events, to make AFNI less sluggish
-----------------------------------------------------------------------*/

#define MAX_NEV 6
#if MAX_NEV > 0
void RT_process_xevents( RT_input * rtin )
{
   Display * dis = THE_DISPLAY ;
   XEvent ev ; int nev=0 ;

   XSync( dis , False ) ;
   while( nev++ < MAX_NEV &&
          XCheckMaskEvent(dis ,
                          ButtonMotionMask   |PointerMotionMask|
                          ButtonPressMask    |ButtonReleaseMask|
                          KeyPressMask       |KeyReleaseMask   |
                          StructureNotifyMask|ExposureMask      ,&ev) ){

          XtDispatchEvent( &ev ) ;  /* do the actual work for this event */
   }
   XmUpdateDisplay(THE_TOPSHELL) ;
   if( verbose > 1 && nev > 1 )
      fprintf(stderr,"RT: processed %d events\n",nev-1);
   return ;
}
#else
void RT_process_xevents( RT_input * rtin ){}  /* doesn't do much */
#endif

/*--------------------------------------------------------------------------
   Initialize a new RT_input structure; returns pointer to new structure.
   Will read from the control channel, which will tell it what data
   channel to open, and if it needs to fork a child process to gather
   header info.

   10 Dec 2002: new input ioc_data is a pointer to an IOCHAN that
                should be used for data acquisition:
                  - if this is not NULL, then the control channel is NOT used
                  - instead, the RT_input struct is setup to read
                     directly from ioc_data (which should already be good)
                  - in this case, you can't use an info_command; all the
                     metadata must come from the ioc_data channel
----------------------------------------------------------------------------*/

RT_input * new_RT_input( IOCHAN *ioc_data )
{
   RT_input *rtin ;
   int ii , cc ;
   Three_D_View *im3d ;

   if( verbose > 0 ) fprintf(stderr,"RT: version %s\n", RT_VERSION_STR);

   if( ioc_data == NULL ){ /*** THE OLD WAY: get info from control channel ***/

     int ncon ;
     char *con , *ptr ;

     if( verbose > 0 )
        fprintf(stderr,"RT: init RT input via control channel\n");

     /** wait until data can be read, or something terrible happens **/

     if( iochan_readcheck(ioc_control,-1) <= 0 ){
        fprintf(stderr,"RT: control stream fails readcheck!\a\n") ;
        return NULL ;
     }

     /** make new structure **/

     rtin = (RT_input *) calloc( 1 , sizeof(RT_input) ) ;
     con  = (char *)     malloc( INFO_SIZE ) ;

     if( rtin == NULL || con == NULL ){
       fprintf(stderr,"RT: malloc fails in new_RT_input!\a\n") ; EXIT(1) ;
     }

     /** read all data possible from control channel **/

     ncon = 0 ;  /* read all data possible from the control channel */
     while(1){
        ii = iochan_recv( ioc_control , con+ncon , INFO_SIZE-ncon ) ;
        if( ii < 1 ) break ;
        ncon += ii ;
        if( ncon >= INFO_SIZE ){
           fprintf(stderr,"RT: control stream buffer overflow!\a\n") ;
           break ;
        }
        iochan_sleep( SHORT_DELAY ) ;
     }

     if( ncon < 1 ){
        fprintf(stderr,"RT: control stream sends no data!\a\n") ;
        free(rtin) ; free(con) ; return NULL ;
     }

     /** The data we now have in 'con' should be of the form
            image_channel_spec\n
            info_command\n\0
         where 'image_channel_spec' is an IOCHAN specifier saying
           how AFNI should read the image data
         and 'info_command' is the name of a command to run that
           will write the dataset control info to stdout (where
           a child of AFNI will get it using the 'popen' routine).
           If no 'info_command' is needed (all the dataset info will
           come thru on the image channel), then the info_command
           should just be skipped.
         Each string should be less than NNAME characters long!
     **/

     ncon = MIN( ncon , INFO_SIZE-2 ) ;
     con[ncon+1] = '\0' ;               /* make sure it is NUL terminated */

     /** copy first string -- this is image_channel_spec **/

     ii = 0 ;
     for( ptr=con ; ii < NNAME && *ptr != '\0' && *ptr != '\n' ; ptr++ ){
        rtin->name_data[ii++] = *ptr ;
     }
     if( ii >= NNAME ){
        fprintf(stderr,"RT: control image_channel_spec buffer overflow!\a\n") ;
        ii = NNAME - 1 ;
     }
     rtin->name_data[ii] = '\0' ;

     /** open data channel **/

     rtin->ioc_data = iochan_init( rtin->name_data , "accept" ) ;
     if( rtin->ioc_data == NULL ){
        fprintf(stderr,"RT: failure to open data IOCHAN %s\a\n",rtin->name_data) ;
        free(rtin) ; free(con) ; return NULL ;
     }

     if( verbose > 1 )
        fprintf(stderr,"RT: opened data stream %s\n",rtin->name_data) ;
     VMCHECK ;

     /** if more follows, that is a command for a child process
         (which will not be started until the first image data arrives) **/

     ii = 0 ;
     if( *ptr != '\0' ){
        for( ptr++ ; ii < NNAME && *ptr != '\0' && *ptr != '\n' ; ptr++ ){
           rtin->name_info[ii++] = *ptr ;
        }
        if( ii >= NNAME ){
           fprintf(stderr,"RT: control info_command buffer overflow!\a\n") ;
           ii = NNAME - 1 ;
        }
     }
     rtin->name_info[ii] = '\0' ;

     if( verbose > 1 ){
        if( strlen(rtin->name_info) > 0 )
           fprintf(stderr,"RT: info command for child will be '%s'\n",rtin->name_info) ;
        else
           fprintf(stderr,"RT: no info command given.\n") ;
     }

     free(con) ;
     VMCHECK ;

     /** the command (if any) will be run later **/

   } else {  /*** THE NEW WAY: directly connect to ioc_data channel [10 DEC 2002] ***/

     /** make new structure **/

     if( verbose > 0 )
        fprintf(stderr,"RT: init RT input via data channel\n");

     rtin = (RT_input *) calloc( 1 , sizeof(RT_input) ) ;
     if( rtin == NULL ){
       fprintf(stderr,"RT: malloc fails in new_RT_input!\a\n") ; EXIT(1) ;
     }

     MCW_strncpy( rtin->name_data , ioc_data->name , NNAME ) ;  /* not really needed */
     rtin->ioc_data     = ioc_data ;
     rtin->name_info[0] = '\0' ;                                /* no child */

   } /*** end of THE NEW (and easier) WAY of starting up ***/

   rtin->ioc_info   = NULL ;
   rtin->child_info = 0 ;

   /** wait until the data channel is good **/

   if( verbose )
      fprintf(stderr,"RT: waiting for data stream to become good.\n") ;
   VMCHECK ;

   while(1){
      ii = iochan_goodcheck(rtin->ioc_data,1000) ;             /* wait up to 1000 msec */
           if( ii >  0 )                 break ;               /* is good! */
      else if( ii == 0 && verbose > 1 ) fprintf(stderr,".") ; /* not good yet */
      else {                                                   /* is bad! */
         fprintf(stderr,"RT: data stream fails to become good!\a\n") ;
         IOCHAN_CLOSENOW(rtin->ioc_data) ; free(rtin) ;
         return NULL ;
      }
   }

   if( verbose > 1 )
      fprintf(stderr,"RT: data stream is now bodaciously good.\n") ;
   VMCHECK ;

   /** initialize internal constants in the struct **/

   rtin->info_ok = 0 ; rtin->no_data = 1 ;

   rtin->image_mode   = image_mode ;  /* 28 Apr 2000 */
   rtin->image_handle = NULL ;
   rtin->image_space  = NULL ;

#if 0                               /*** THE VERY OLD WAY to set things up ***/
   rtin->nxx = DEFAULT_XYMATRIX ;
   rtin->nyy = DEFAULT_XYMATRIX ;
   rtin->nzz = DEFAULT_ZNUM ;

   rtin->orcxx = ORI_S2I_TYPE ;
   rtin->orcyy = ORI_A2P_TYPE ;
   rtin->orczz = ORI_L2R_TYPE ;

   rtin->xxfov = DEFAULT_XYFOV ;
   rtin->yyfov = DEFAULT_XYFOV ;
   rtin->zzfov = -1.0 ;
   rtin->dxx   = DEFAULT_XYFOV / DEFAULT_XYMATRIX ;
   rtin->dyy   = DEFAULT_XYFOV / DEFAULT_XYMATRIX ;
   rtin->dzz   = DEFAULT_ZDELTA ;

   rtin->zgap  = 0.0 ;                  /* 18 Dec 2002 */
   rtin->xxoff = rtin->yyoff = rtin->zzoff = 0.0 ;

   rtin->xxorg = 0.5 * (rtin->nxx - 1) * rtin->dxx ; rtin->xcen = 1 ;
   rtin->yyorg = 0.5 * (rtin->nyy - 1) * rtin->dyy ; rtin->ycen = 1 ;
   rtin->zzorg = 0.5 * (rtin->nzz - 1) * rtin->dzz ; rtin->zcen = 1 ;
   rtin->xxdcode = ILLEGAL_TYPE ;
   rtin->yydcode = ILLEGAL_TYPE ;
   rtin->zzdcode = ILLEGAL_TYPE ;

   rtin->tr     = DEFAULT_TR ;
   rtin->nr     = 1 ;
   rtin->dtype  = DTYPE_2DZT ;
   rtin->datum  = MRI_short  ;

   rtin->zorder      = ZORDER_ALT ;
   rtin->zorder_lock = 0 ;              /* 22 Feb 1999 */
#else
   rtin->nxx = -1 ;
   rtin->nyy = -1 ;
   rtin->nzz = -1 ;

   rtin->orcxx = -1 ;
   rtin->orcyy = -1 ;
   rtin->orczz = -1 ;

   rtin->xxfov = 0.0 ;
   rtin->yyfov = 0.0 ;
   rtin->zzfov = 0.0 ;
   rtin->dxx   = 0.0 ;
   rtin->dyy   = 0.0 ;
   rtin->dzz   = 0.0 ;

   rtin->zgap  = 0.0 ;                  /* 18 Dec 2002 */
   rtin->xxoff = rtin->yyoff = rtin->zzoff = 0.0 ;

   rtin->xxorg = 0.0 ; rtin->xcen = 1 ; rtin->xxdcode = ILLEGAL_TYPE ;
   rtin->yyorg = 0.0 ; rtin->ycen = 1 ; rtin->yydcode = ILLEGAL_TYPE ;
   rtin->zzorg = 0.0 ; rtin->zcen = 1 ; rtin->zzdcode = ILLEGAL_TYPE ;

   rtin->tr     = DEFAULT_TR ;
   rtin->nr     = 1 ;
   rtin->dtype  = DTYPE_2DZT ;
   rtin->datum  = MRI_short  ;
   rtin->nzseq  = 0 ;
   rtin->nstimes= 0 ;                   /* 25 Apr 2011 [rickr] */

   rtin->zorder      = ZORDER_ALT ;
   rtin->zorder_lock = 0 ;              /* 22 Feb 1999 */
   rtin->tpattern    = ZORDER_ALT ;     /* 10 May 2005 [rickr] */
#endif

   rtin->swap_on_read = 0 ;  /* wait for BYTEORDER     26 Jun 2003 [rickr] */

   rtin->nbuf   = 0    ;     /* no input buffer data yet */
   rtin->imsize = 0 ;        /* don't know how many bytes/image yet */
   rtin->bufar  = NULL ;     /* no input image buffer yet */

   im3d = plint->im3d ;                           /* default controller */
   if( !IM3D_OPEN(im3d) )                         /* may not be open */
      im3d = AFNI_find_open_controller() ;

   /** 01 Aug 2002: initialize dataset dependent items */

   for( cc=0 ; cc < MAX_CHAN ; cc++ ){
      rtin->afni_status[cc] = 0 ;      /* AFNI is ignorant of the dataset */
      rtin->dset[cc]        = NULL ;   /* no dataset yet */
      rtin->sbr[cc]         = NULL ;   /* no brick space yet */
      rtin->im[cc]          = NULL ;
      rtin->nvol[cc]        = 0 ;
      rtin->nsl[cc]         = 0 ;
      rtin->TE[cc]          = 0.0 ;    /* 12 Mar 2015 [rickr] */
      rtin->im3d[cc]        = im3d ;
      rtin->sess[cc]        = GLOBAL_library.sslist->ssar[im3d->vinfo->sess_num] ;
      rtin->sess_num[cc]    = im3d->vinfo->sess_num ;
   }

   rtin->num_chan = 1 ;   /* default number of channels */
   rtin->cur_chan = 0 ;   /* current channel index */

   strcpy( rtin->root_prefix , root ) ;
   /* nuke any .nii, say, and note for later          30 May, 2014 [rickr] 
    *   This will be set after any ADN_prefix adjustment, because the
    *   root_prefix is appended to in many and compounding ways.
    *   Unfortunately, this method cannot distinguish between .nii
    *   and .nii.gz, which might be what Vinai would prefer.               */
   rtin->storage_mode = remove_SM_suffix(rtin->root_prefix) ;

   /** setup realtime function evaluation **/

   rtin->func_dset   = NULL ;  /* no function yet (if ever) */
   rtin->func_status = 0 ;     /* AFNI is ignorant */
   rtin->func_condit = 0 ;     /* no condition yet */

   /* function evaluation is not done on time-independent datasets */

   rtin->func_code   = (rtin->dtype == DTYPE_2DZ || rtin->dtype == DTYPE_3D)
                       ? FUNC_NONE : func_code ;

   rtin->func_func   = FUNC_funcs[rtin->func_code] ; /* where to evaluate */

   rtin->mrg_dset    = NULL ;     /* 02 Jun 2009 */
   rtin->mrg_status  = 0 ;
   rtin->mrg_nvol    = 0 ;

   /*----- environment sets registration base dataset? [22 Dec 2011] -----*/

   if( g_reg_base_dset == NULL ){
#undef  RBENV
#define RBENV "AFNI_REALTIME_External_Dataset"
     char *dname = getenv(RBENV) ;
     THD_slist_find slf ; THD_3dim_dataset *bset=NULL ;
     if( dname != NULL ){
       if( THD_filename_pure(dname) ){
         slf = THD_dset_in_session( FIND_PREFIX, dname, im3d->ss_now ) ; bset = slf.dset ;
         if( bset && verbose > 1 )
            fprintf(stderr,"RT Ext Reg: have session dname %s\n", dname) ;

         if( bset == NULL ){
           MCW_idcode idcode ;
           MCW_strncpy( idcode.str, dname, MCW_IDSIZE ) ;
           slf = THD_dset_in_session( FIND_IDCODE, &idcode, im3d->ss_now ) ; bset = slf.dset ;
           if( bset && verbose > 1 )
              fprintf(stderr,"RT Ext Reg: have session idcode %s\n", dname) ;
         }
       }
       if( bset == NULL ){
         bset = THD_open_dataset(dname) ;
         if( bset && verbose > 1 )
            fprintf(stderr,"RT Ext Reg: have open dset %s\n", dname) ;
       }

       /* could apply later THD_copy_one_sub(g_reg_base_dset, regtime) here */
       if( bset != NULL ){
         INFO_message("env.var. sets registration base: %s=%s",RBENV,dname) ;
         g_reg_base_dset = bset ;
         g_reg_base_mode = RT_RBASE_MODE_EXTERN ;
       } else {
         WARNING_message("env.var. fails to find dataset: %s=%s",RBENV,dname) ;
       }
     }
   } /* end of setting base dataset from environment */

/* April 2012 Cameron Craddock initialize variables for RT detrend */
   rtin->detrend_dset    = NULL;
   rtin->detrend_mode    = RT_DETREND_NONE;
   rtin->detrend_status  = 0;
   rtin->detrend_nvol    = 0; /* number of detrended volumes */
   rtin->detrend_polort  = -1;
   /* end CC */

   rtin->reg_base_index = regtime ;  /* save these now, in case the evil */
   rtin->reg_mode       = regmode ;  /* user changes them on the fly!    */
   rtin->reg_base_mode  = g_reg_base_mode ;
   rtin->reg_dset       = NULL ;

   for( cc=0 ; cc < MAX_CHAN ; cc++ ) {
      rtin->reg_chan_dset[cc]   = NULL ;
      rtin->reg_chan_status[cc] = 0 ;
   }
   rtin->reg_chan_mode  = RT_CM_RMODE_NONE;       /* 16 May 2010 [rickr] */
   rtin->chan_list_str  = NULL;                   /* 13 Jul 2010 [rickr] */
   rtin->chan_list      = NULL;

   /* if dset to copy, dupe one vol                  04 Sep 2009 [rickr] */
   if(g_reg_base_dset) {
      /* be sure regtime makes sense */
      if( regtime < 0 || regtime >= DSET_NVALS(g_reg_base_dset) ) {
         fprintf(stderr,"** bad regtime %d for g_reg_base_dset\n", regtime);
         regtime = 0;
      }
      rtin->reg_base_dset=THD_copy_one_sub(g_reg_base_dset, regtime);
      regtime = 0;
      rtin->reg_base_index = regtime;
   } else
      rtin->reg_base_dset=NULL;

   rtin->reg_2dbasis    = NULL ;
   rtin->reg_status     = 0 ;        /* AFNI knows nothing. NOTHING.     */
   rtin->reg_nvol       = 0 ;        /* number volumes registered so far */

   rtin->reg_nest  = 0 ;
   rtin->reg_tim   = (float *) malloc( sizeof(float) ) ;
   rtin->reg_dx    = (float *) malloc( sizeof(float) ) ;
   rtin->reg_dy    = (float *) malloc( sizeof(float) ) ;
   rtin->reg_phi   = (float *) malloc( sizeof(float) ) ;

   rtin->reg_dz       = (float *) malloc( sizeof(float) ) ;
   rtin->reg_theta    = (float *) malloc( sizeof(float) ) ;
   rtin->reg_psi      = (float *) malloc( sizeof(float) ) ;
   rtin->reg_rep      = (float *) malloc( sizeof(float) ) ;
   rtin->reg_eval     = (float *) malloc( sizeof(float) ) ;
   rtin->reg_3dbasis  = NULL ;
   rtin->mp           = NULL ;    /* no plot yet */

   rtin->reg_graph_xnew = 0 ;     /* did the user update reg_graph_xr */
   rtin->reg_graph_ynew = 0 ;     /* did the user update reg_graph_yr */
   rtin->reg_graph_xr = reg_nr ;  /* will scale by TR when we know it */
   rtin->reg_graph_yr = reg_yr ;

   rtin->p_code = NULL ;          /* init parser code    12 Feb 2004 [rickr] */

   rtin->mp_tcp_use = 0 ;         /* tcp motion param    30 Mar 2004 [rickr] */
   rtin->mp_tcp_sd  = 0 ;
   rtin->mp_port    = RT_MP_DEF_PORT ;
   rtin->mp_nmsg    = 0 ;
   rtin->mp_npsets  = 0 ;
   strcpy(rtin->mp_host, "localhost") ;

   /* just a local copy of the global t2star dataset pointer */
   if(g_t2star_ref_dset) rtin->t2star_ref_dset=g_t2star_ref_dset;
   else                  rtin->t2star_ref_dset=NULL;

   rtin->mask       = NULL ;      /* mask averages, to send w/motion params */
   rtin->mask_aves  = NULL ;      /*                    10 Nov 2006 [rickr] */
   rtin->mask_init = 0 ;  /* CC: flag to indicate that mask was initialized */
   rtin->mask_nvals = 0 ;
   rtin->mask_nset  = 0 ;         /* total number of set mask voxles        */
   rtin->mask_nones = 0 ;         /* total number of mask voxels == 1       */

   rtin->is_oblique = 0 ;         /* oblique info       10 Jul 2008 [rickr] */
   memset(rtin->oblique_mat, 0, sizeof(rtin->oblique_mat)) ;

   rtin->reg_resam = REG_resam_ints[reg_resam] ;
   if( rtin->reg_resam < 0 ){                    /* 20 Nov 1998: */
      rtin->reg_resam       = MRI_HEPTIC ;       /* special case */
      rtin->reg_final_resam = MRI_FOURIER ;
   } else {
      rtin->reg_final_resam = -1 ;
   }

   rtin->reg_graph = reggraph ;
   if( regmode==REGMODE_3D_ESTIM && reggraph==0 ) rtin->reg_graph = 1;

   /** record the times for later reportage **/

   rtin->elapsed = PLUTO_elapsed_time() ; rtin->last_elapsed = rtin->elapsed ;
   rtin->cpu     = PLUTO_cpu_time() ;     rtin->last_nvol = 0 ;

   rtin->num_note = 0 ;      /* 01 Oct 2002: notes list */
   rtin->note     = NULL ;

   rtin->marked_for_death = 0 ;  /* 10 Dec 2002 */

   return rtin ;
}

/* remove any storage mode suffix           30 May 2014 [rickr] */
/* return the implied storage mode (0 means to ignore)          */
static int remove_SM_suffix(char prefix[])
{
   char * sp = NULL;
   int    smode = 0;

   ENTRY("remove_SM_suffix");

   if( (sp = ends_at(prefix, ".nii")) != NULL ) {
      *sp = '\0';
      smode = STORAGE_BY_NIFTI;
   } else if( (sp = ends_at(prefix, ".nii.gz")) != NULL ) {
      *sp = '\0';
      smode = STORAGE_BY_NIFTI;
   }

   if( smode && verbose )
      fprintf(stderr,"RT: will apply storage mode %d (%s)\n",
              smode, storage_mode_str(smode));

   RETURN(smode);
}

/* if 'text' ends with 'suffix', return a pointer to that
 * position within 'text' */
static char * ends_at(char * text, char * suffix)
{
   int tlen, slen;
   if( !text || !suffix ) return 0;

   tlen = strlen(text);
   slen = strlen(suffix);

   if( tlen < slen || tlen == 0 || slen == 0 ) return 0;

   if ( ! strcmp(text+tlen-slen, suffix) ) return text+tlen-slen;
   else                                    return NULL;
}



/*-------------------------------------------------------------------------
   Create a child process to run a command that will send header info
   back to the parent AFNI process.
---------------------------------------------------------------------------*/

void RT_start_child( RT_input * rtin )
{
   pid_t child_pid ;

   if( rtin == NULL || strlen(rtin->name_info) == 0 ) return ;  /* sanity */

   child_pid = fork() ;             /* AKA bifurcation */

   if( child_pid == (pid_t)(-1) ){  /* real bad news */
     fprintf(stderr,"RT: can't fork child process! Quitting NOW!\a\n"); EXIT(1);
   }

   if( child_pid > 0 ){              /** I'm the parent **/

      if( verbose > 1 )
         fprintf(stderr,"RT: forked a child process to execute '%s'\n",rtin->name_info) ;
      VMCHECK ;

      /** open a channel to communicate with the child **/

      rtin->child_info = child_pid ;
      rtin->ioc_info   = iochan_init( SHM_CHILD , "accept" ) ;
      if( rtinp->ioc_info == NULL ){
        kill( child_pid , SIGTERM ) ;
        fprintf(stderr,"RT: can't create read stream from child! Quitting NOW!\a\n");
        EXIT(1) ;
      }

      rtin->child_start_time = PLUTO_elapsed_time() ;  /* 10 Dec 1998 */

      /** next time thru RT_worker will try to read data from this channel **/

   } else {                          /** I'm the child **/

      RT_acquire_info( rtin->name_info ) ;  /* get info, send back to parent */
      _exit(0) ;
   }

   return ;
}

/*-------------------------------------------------------------------------
---------------------------------------------------------------------------
   This routine is only called from the child process:
     Run a command, get its output, send it back to the parent, exit.
     The entire existence of the child is confined to this routine.
---------------------------------------------------------------------------
---------------------------------------------------------------------------*/

int RT_acquire_info( char * command )
{
   FILE * fp ;
   char * info = (char *) malloc( sizeof(char) * INFO_SIZE ) ; int ninfo = 0 ;
   IOCHAN * ioc ;
   int jj ;

   /** create channel back to parent **/

   ioc = iochan_init( SHM_CHILD , "create" ) ;
   if( ioc == NULL ){
      fprintf(stderr,"RT: child fails to open stream back to parent! Child dies!\a\n");
      _exit(1) ;
   }

   /** run command, read its output into this pipe **/

   fp = popen( command , "r" ) ;
   if( fp == NULL ){
      fprintf(stderr,"RT: child fails to open pipe to command='%s' ; Child dies!\a\n",
              command) ;
      IOCHAN_CLOSENOW(ioc) ; _exit(1) ;
   }

   /** read pipe until nothing more **/

   while( afni_fgets(info+ninfo,INFO_SIZE-ninfo,fp) != NULL ){
      ninfo = strlen(info) ;
   }
   pclose(fp) ;

   /** send output back to parent **/

   jj = iochan_writecheck(ioc,-1) ;  /* wait until ready */
   if( jj < 0 ){
      fprintf(stderr,"RT: child can't write IOCHAN to parent! Child dies!\a\n") ;
      IOCHAN_CLOSENOW(ioc) ; _exit(1) ;
   }

   iochan_sendall( ioc , info , ninfo+1 ) ;        /* include the NUL character */
   iochan_sleep(LONG_DELAY) ;                      /* wait a bit */
   while( ! iochan_clearcheck(ioc,LONG_DELAY) )    /* loop until cleared */
      iochan_sleep(LONG_DELAY) ;

   iochan_sleep(LONG_DELAY) ;                      /* once more, just for luck */

                                                   /**************************/
   free(info); IOCHAN_CLOSENOW(ioc); _exit(0);     /** END OF CHILD PROCESS **/
}                                                  /**************************/

/****************************************************************************
   Check the realtime input structure for OK-ness, and set its internal flag.
   If if it bad, and prt isn't 0, print a message about the error(s).
*****************************************************************************/

#define EPR(s) fprintf(stderr,"RT: HEADER DATA ERROR - %s\a\n",(s))

#define OR3OK(x,y,z) ( ((x)&6) + ((y)&6) + ((z)&6) == 6 )

void RT_check_info( RT_input * rtin , int prt )
{
   if( rtin == NULL ) return ;

   /*-- 28 Apr 2000: if in Image Only mode, do fewer checks --*/

   if( rtin->image_mode ){

      rtin->info_ok = ( rtin->nxx > 1 )                         &&
                      ( rtin->nyy > 1 )                         &&
                      ( AFNI_GOOD_DTYPE(rtin->datum) ) ;

      if( rtin->info_ok || !prt ) return ;  /* if good, or if no print */

      if( !(rtin->nxx > 1)                ) EPR("Image x-dimen not > 1") ;
      if( !(rtin->nyy > 1)                ) EPR("Image y-dimen not > 1") ;
      if( !(AFNI_GOOD_DTYPE(rtin->datum)) ) EPR("Bad datum") ;
      return ;
   }

   /*-- below here: must construct dataset, so do all necessary checks --*/

   rtin->info_ok = ( rtin->dtype > 0 )                            &&
                   ( THD_filename_pure(rtin->root_prefix) )       &&
                   ( strlen(rtin->root_prefix) < THD_MAX_PREFIX ) &&
                   ( rtin->tr > 0 )                               &&
                   ( rtin->dzz > 0 || rtin->zzfov > 0 )           &&
                   ( rtin->xxfov > 0 )                            &&
                   ( rtin->yyfov > 0 )                            &&
                   ( rtin->nxx > 1 )                              &&
                   ( rtin->nyy > 1 )                              &&
                   ( rtin->nzz >= 1 )                             &&
                   ( AFNI_GOOD_DTYPE(rtin->datum) )               &&
                   ( rtin->zorder > 0 )                           &&
                   ( rtin->tpattern > 0 )                         &&
             ( rtin->nstimes == 0 || rtin->nstimes == rtin->nzz ) &&
                   ( rtin->orcxx >= 0 )                           &&
                   ( rtin->orcyy >= 0 )                           &&
                   ( rtin->orczz >= 0 )                           &&
                   ( OR3OK(rtin->orcxx,rtin->orcyy,rtin->orczz) )    ;

   if( rtin->info_ok || !prt ) return ;  /* if good, or if no print */

   /* print error messages */

   if( !(rtin->dtype > 0)                            ) EPR("Bad acquisition type") ;
   if( !(THD_filename_pure(rtin->root_prefix))       ) EPR("Bad prefix") ;
   if( !(strlen(rtin->root_prefix) < THD_MAX_PREFIX) ) EPR("Overlong prefix") ;
   if( !(rtin->tr > 0)                               ) EPR("TR is not positive") ;
   if( !(rtin->dzz > 0 || rtin->zzfov > 0)           ) EPR("Slice thickness not positive") ;
   if( !(rtin->xxfov > 0)                            ) EPR("x-FOV not positive") ;
   if( !(rtin->yyfov > 0)                            ) EPR("y-FOV not positive") ;
   if( !(rtin->nxx > 1)                              ) EPR("Image x-dimen not > 1") ;
   if( !(rtin->nyy > 1)                              ) EPR("Image y-dimen not > 1") ;
   if( !(rtin->nzz >= 1)                             ) EPR("Slice count (z-dimen) not >= 1") ;
   if( !(AFNI_GOOD_DTYPE(rtin->datum))               ) EPR("Bad datum") ;
   if( !(rtin->zorder > 0)                           ) EPR("Slice ordering illegal") ;
   if( !(rtin->tpattern > 0)                         ) EPR("Timing pattern illegal") ;
   if( rtin->nstimes && rtin->nstimes != rtin->nzz   ) EPR("Num slice times != num slices") ;
   if( !(rtin->orcxx >= 0)                           ) EPR("x-orientation illegal") ;
   if( !(rtin->orcyy >= 0)                           ) EPR("y-orientation illegal") ;
   if( !(rtin->orczz >= 0)                           ) EPR("z-orientation illegal") ;
   if( !(OR3OK(rtin->orcxx,rtin->orcyy,rtin->orczz)) ) EPR("Inconsistent xyz-orientations") ;

   return ;
}

/* mostly allow for verbose output */
static int RT_mp_comm_alive(int sock, int verb, char * mesg)
{
    char buf[4];
    int  wc, rc, ac, tr = -2;

    ac = tcp_alivecheck(sock);
    if( !ac ) {
        if( mesg ) fprintf(stderr,"** %s: socket has gone bad\n", mesg);
        else       fprintf(stderr,"** RT_comm socket has gone bad\n");
    }

    if( verb ) {  /* check and print extra info */
        wc = tcp_writecheck(sock, 1);
        rc = tcp_readcheck(sock, 1);

        if( rc ) tr = tcp_recv(sock, buf, 1, MSG_PEEK);

        fprintf(stderr,"-- alive test: wc=%d, rc=%d, ac=%d, tr=%d\n",
                wc, rc, ac, tr);
    }

    return ac;
}

/*---------------------------------------------------------------------------
   Close the socket connection.                        30 Mar 2004 [rickr]

   new_tcp_use should be set to either 0 or -1, where -1 means not
   to allow any init again (this run)

   return   0 : on success
          < 0 : on error
-----------------------------------------------------------------------------*/
static int RT_mp_comm_close( RT_input * rtin, int new_tcp_use )
{
    char magic_bye[] = { 0xde, 0xad, 0xde, 0xad, 0 };
    int  rv;

    if ( rtin->mp_tcp_use <= 0 || rtin->mp_tcp_sd <= 0 )
        return 0;

    if( tcp_alivecheck(rtin->mp_tcp_sd) && tcp_writecheck(rtin->mp_tcp_sd,0) ){
        if( verbose > 1 ) fprintf(stderr,"RTM: sending magic_bye...\n");
        if( (rv = send(rtin->mp_tcp_sd, magic_bye, 4, 0)) <= 0 )
            fprintf(stderr,"** bad return of %d closing RT MP socket\n",rv);
    }

    fprintf(stderr,"RTM: closing motion param socket, "
                   "sent %d param sets over %d messages\n",
                   rtin->mp_npsets, rtin->mp_nmsg);

    /* in any case, close the socket */
    close(rtin->mp_tcp_sd);
    rtin->mp_tcp_sd  = 0;
    /* verifythat tcp_use goes to either 0 or -1 */
    rtin->mp_tcp_use = (new_tcp_use == -1) ? new_tcp_use : 0;
    rtin->mp_npsets  = 0;
    rtin->mp_nmsg    = 0;

    /* free up any mask memory */
    RT_mp_mask_free(rtin);

    return 0;
}

/* set up the pointer to yar and call send_data */
static int RT_mp_comm_send_data_wrapper(RT_input * rtin, int tfirst, int ntt)
{
   float * yar[7] ;

   /* do we have anything to do here? */
   if( ntt <= tfirst ) return 0;
   if( ! rtin->mp && rtin->mp_tcp_use <= 0) return 0;

   yar[0] = rtin->reg_rep   + tfirst ;
   yar[1] = rtin->reg_dx    + tfirst ;
   yar[2] = rtin->reg_dy    + tfirst ;
   yar[3] = rtin->reg_dz    + tfirst ;
   yar[4] = rtin->reg_phi   + tfirst ;
   yar[5] = rtin->reg_psi   + tfirst ;
   yar[6] = rtin->reg_theta + tfirst ;

   /* send the data off over tcp connection               30 Mar 2004 [rickr] */
   RT_mp_comm_send_data( rtin, yar+1, ntt-tfirst, tfirst );

   return 0;
}

/*---------------------------------------------------------------------------
   Send the current motion params.                        30 Mar 2004 [rickr]

   if masks are set, pass masked averages from sub-brick 'sub' of dset[0]

   return   0 : on success
          < 0 : on error
-----------------------------------------------------------------------------*/
static int RT_mp_comm_send_data(RT_input *rtin, float *mp[6], int nt, int sub)
{
    static float * data = NULL;
    static int     dvals = 0;
    int   b2send, remain, tr_vals;
    int   c, c2, dind, mpindex, nblocks, bsize;

    if ( rtin->mp_tcp_use != 1 || nt <= 0 ) return 0;

    if ( rtin->mp_tcp_sd <= 0 ) return -1;

    if( ! g_mask_val_type ) return 0;

    /* verify that the socket is still good */
    if ( ! RT_mp_comm_alive(rtin->mp_tcp_sd, 0, "pre data send") ) {
        RT_mp_comm_close(rtin, 0);
        return -1;
    }

    /* how many values do we need to send? */
    if( g_mask_val_type == 1 ) {
        tr_vals = 6;    /* enough for motion */
        bsize   = 6;
    } else if( g_mask_dset && g_mask_val_type == 2 ) {
        tr_vals = 256 + 6;               /* enough for motion and many masks */
        bsize   = 6 + rtin->mask_nvals;          /* single block size per TR */
    } else if( g_mask_dset && g_mask_val_type == 3 ) {
        bsize   = 6 + rtin->mask_nset*8;         /* motion plus all vals */
        tr_vals = bsize;
    } else if( g_mask_dset && g_mask_val_type == 4) {
        bsize   = 6 + rtin->mask_nset; /* motion and all vals (light-weight) */
        tr_vals = bsize; 
    } else if( g_mask_dset && g_mask_val_type == 5) {
                        /* motion plus non-1 mask averages plus all-1 voxels */
        if( rtin->mask_nvals <= 0 ) {
           /* do we allow this? */
           fprintf(stderr,"** RTM: no mask to send data for\n");
           bsize = 6;
        } else {
           bsize = 6 + rtin->mask_nvals-1 + rtin->mask_nones;
        }
        tr_vals = bsize; 
    } else {
        fprintf(stderr,"** need mask to send data\n");
        RT_mp_comm_close(rtin, 0);
        return -1;
    }

    /* maybe we need more space for data */
    if( !data || dvals < tr_vals*2 ) {
        dvals = tr_vals * 2;
        data = (float *)realloc(data, dvals*sizeof(float));
        if(!data) {
            fprintf(stderr,"** failed to alloc %d floats for mask data\n"
                           "   closing MP communication...\n" ,dvals);
            RT_mp_comm_close(rtin, 0);
            return -1;
        }
    }

    nblocks = dvals / bsize;                 /* max blocks per iteration */
    mpindex = 0;            /* index into mp list (nvals may be partial) */
    while( mpindex < nt )
    {
        remain = nt - mpindex;
        b2send = MIN(remain, nblocks);

        /* copy floats to 'data' */
        for ( c = 0; c < b2send; c++ )   /* for each block */
        {
            for ( c2 = 0; c2 < 6; c2++ )                /* send 6 mp params */
                data[bsize*c+c2] = mp[c2][mpindex];

            /* process mask, if desired (ROI averages or all set values) */
            if( g_mask_dset && g_mask_val_type == 2 ) {
                if( ! RT_mp_get_mask_aves(rtin, sub+mpindex) ) {
                    for ( c2 = 0; c2 < rtin->mask_nvals; c2++ )
                        data[bsize*c+6+c2] = (float)rtin->mask_aves[c2+1];
                } else {  /* bad failure, close socket */
                    RT_mp_comm_close(rtin, 0);
                    return -1;
                }
            } else if( g_mask_dset && g_mask_val_type == 3 ) {
                dind = bsize*c+6;  /* note initial offset */
                if( RT_mp_set_mask_data(rtin, data+dind, sub+mpindex) ) {
                    RT_mp_comm_close(rtin, 0);
                    return -1;
                }
            } else if( g_mask_dset && g_mask_val_type == 4 ) {
                dind = bsize*c+6;  /* note initial offset */
                if( RT_mp_set_mask_data_light(rtin, data+dind, sub+mpindex) ) {
                    RT_mp_comm_close(rtin, 0);
                    return -1;
                }
            } else if( g_mask_dset && g_mask_val_type == 5 ) {
                /* still compute mask averages (don't bother to exclude 1) */
                if( RT_mp_get_mask_aves(rtin, sub+mpindex) ) {
                    RT_mp_comm_close(rtin, 0);
                    return -1;
                }

                dind = bsize*c+6;  /* note initial offset (motion) */
                if( RT_mp_set_mask_aves_n_data(rtin, data+dind, sub+mpindex)) {
                    RT_mp_comm_close(rtin, 0);
                    return -1;
                }
            }

            mpindex++;  /* that was one TR */
        }

        /* verify that the socket is still good */
        if ( ! RT_mp_comm_alive(rtin->mp_tcp_sd, 0, "send data") ) {
            RT_mp_comm_close(rtin, 0);
            return -1;
        }

        if ( send(rtin->mp_tcp_sd, data, bsize*b2send*sizeof(float), 0) == -1 )
        {
            fprintf(stderr,"** failed to send %d floats, closing socket...\n",
                    bsize*b2send);
            RT_mp_comm_close(rtin, 0);
            return -1;
        }
        if( g_show_times ) {
           char mesg[64];
           sprintf(mesg,"mp_comm_send_data %d floats for %d trs",
                   bsize*b2send, nt);
           RT_mp_show_time(mesg);
        }

        /* keep track of num messages and num param sets */
        rtin->mp_nmsg++;
        rtin->mp_npsets += b2send;
    }

    if( verbose > 1 )
       fprintf(stderr,"RTM, sening time point %d\n", sub);

    return 0;
}


/*---------------------------------------------------------------------------
   for each set mask voxel, fill data with 8 floats:
        index  i  j  k  x  y  z  dset_value

   return 0 on success
-----------------------------------------------------------------------------*/
static int RT_mp_set_mask_data( RT_input * rtin, float * data, int sub )
{
    THD_fvec3   fvec;
    THD_ivec3   ivec;
    void      * dptr;
    float       ffac;
    int         dind, iv, nvox;
    int         i, j, k, nx, nxy;

    if( !ISVALID_DSET(rtin->reg_dset) || DSET_NVALS(rtin->reg_dset) <= sub ){
       fprintf(stderr,"** RT_mp_set_mask_data: not set for sub-brick %d\n",sub);
       return -1;
    }

    if( sub < 0 ) return 0;

    if( !rtin->mask || !data || rtin->mask_nvals <= 0 ) {
       fprintf(stderr,"** RT_mp_set_mask_data: no mask information to apply\n");
       return -1;
    }

    /* note dimensions */
    nvox = DSET_NVOX(g_mask_dset);
    if( DSET_NVOX(rtin->reg_dset) != nvox ){
        /* terminal: whine, blow away the mask and continue */
        fprintf(stderr,"** nvox for mask (%d) != nvox for reg_dset (%d)\n"
                       "   terminating mask processing...\n",
                nvox, DSET_NVOX(rtin->reg_dset) );
        return RT_mp_mask_free(rtin);
    }
    nx = DSET_NX(g_mask_dset);
    nxy = nx * DSET_NY(g_mask_dset);

    ffac = DSET_BRICK_FACTOR(rtin->reg_dset, sub);
    if( ffac == 0.0 ) ffac = 1.0;

    dind = 0;   /* index into output data array */
    dptr = (void *) DSET_ARRAY(rtin->reg_dset, sub);
    for( iv=0 ; iv < nvox ; iv++ ) {
       if( !rtin->mask[iv] ) continue;  /* if not in mask, skip */

       /* we have a good voxel, fill everything but value, first */

       IJK_TO_THREE(iv,i,j,k,nx,nxy);   /* get i,j,k indices */
       LOAD_IVEC3(ivec,i,j,k);
       data[dind++] = (float)iv;        /* set index and i,j,k */
       data[dind++] = (float)i;
       data[dind++] = (float)j;
       data[dind++] = (float)k;

       /* convert ijk to dicom xyz */
       fvec = THD_3dind_to_3dmm_no_wod(g_mask_dset, ivec);
       fvec = THD_3dmm_to_dicomm(g_mask_dset, fvec);

       data[dind++] = fvec.xyz[0];      /* set x,y,z coords */
       data[dind++] = fvec.xyz[1];
       data[dind++] = fvec.xyz[2];

       if( rtin->datum == MRI_short )   /* and finally set data value */
           data[dind++] = ((short *)dptr)[iv]*ffac;
       else if( rtin->datum == MRI_float )
           data[dind++] = ((float *)dptr)[iv]*ffac;
       else if( rtin->datum == MRI_byte )
           data[dind++] = ((byte *)dptr)[iv]*ffac;
    }

    return 0;
}

/*---------------------------------------------------------------------------
   Set non-1 ROI averages and all val==1 mask values.     19 Jan 2020 [rickr]
   (motion has already been set)

        nmask-1 + nones values

   return 0 on success
-----------------------------------------------------------------------------*/
static int RT_mp_set_mask_aves_n_data( RT_input * rtin, float * data, int sub )
{
    void      * dptr;
    float       ffac;
    int         dind, nvox, iv;
    int         nx, nxy;

    if( !ISVALID_DSET(rtin->reg_dset) || DSET_NVALS(rtin->reg_dset) <= sub ){
       fprintf(stderr,"** RT_mp_SMDL: not set for sub-brick %d\n",sub);
       return -1;
    }

    if( sub < 0 ) return 0;

    if( !rtin->mask || !data || rtin->mask_nvals <= 0 ) {
       fprintf(stderr,"** RT_mp_SMDL: no mask information to apply\n");
       return -1;
    }

    /* note dimensions */
    nvox = DSET_NVOX(g_mask_dset);
    if( DSET_NVOX(rtin->reg_dset) != nvox ){
        /* terminal: whine, blow away the mask and continue */
        fprintf(stderr,"** nvox for mask (%d) != nvox for reg_dset (%d)\n"
                       "   terminating mask processing...\n",
                nvox, DSET_NVOX(rtin->reg_dset) );
        return RT_mp_mask_free(rtin);
    }
    nx = DSET_NX(g_mask_dset);
    nxy = nx * DSET_NY(g_mask_dset);

    ffac = DSET_BRICK_FACTOR(rtin->reg_dset, sub);
    if( ffac == 0.0 ) ffac = 1.0;

    /* init counters */
    dind = 0;   /* index into output data array */
    dptr = (void *) DSET_ARRAY(rtin->reg_dset, sub);

    /* start by setting non-1 mask aves (skip first iv) */
    for ( iv = 1; iv < rtin->mask_nvals; iv++ )
       data[dind++] = (float)rtin->mask_aves[iv+1];

    for( iv=0 ; iv < nvox ; iv++ ) {
       /* if mask val is not exactly 1, skip */
       if( rtin->mask[iv] != 1 ) continue;

       /* we have a good voxel */

       /* set the data value (convert to float) */
       if( rtin->datum == MRI_short )
           data[dind++] = ((short *)dptr)[iv]*ffac;
       else if( rtin->datum == MRI_float )
           data[dind++] = ((float *)dptr)[iv]*ffac;
       else if( rtin->datum == MRI_byte )
           data[dind++] = ((byte *)dptr)[iv]*ffac;
    }

    return 0;
}

/*---------------------------------------------------------------------------
   Like RT_mp_set_mask_data, but for each masked voxel, send only that voxel
   data value (so no index, i,j,k, x,y,z).

   return 0 on success
-----------------------------------------------------------------------------*/
static int RT_mp_set_mask_data_light( RT_input * rtin, float * data, int sub )
{
    void      * dptr;
    float       ffac;
    int         dind, nvox, iv;
    int         nx, nxy;

    if( !ISVALID_DSET(rtin->reg_dset) || DSET_NVALS(rtin->reg_dset) <= sub ){
       fprintf(stderr,"** RT_mp_SMDL: not set for sub-brick %d\n",sub);
       return -1;
    }

    if( sub < 0 ) return 0;

    if( !rtin->mask || !data || rtin->mask_nvals <= 0 ) {
       fprintf(stderr,"** RT_mp_SMDL: no mask information to apply\n");
       return -1;
    }

    /* note dimensions */
    nvox = DSET_NVOX(g_mask_dset);
    if( DSET_NVOX(rtin->reg_dset) != nvox ){
        /* terminal: whine, blow away the mask and continue */
        fprintf(stderr,"** nvox for mask (%d) != nvox for reg_dset (%d)\n"
                       "   terminating mask processing...\n",
                nvox, DSET_NVOX(rtin->reg_dset) );
        return RT_mp_mask_free(rtin);
    }
    nx = DSET_NX(g_mask_dset);
    nxy = nx * DSET_NY(g_mask_dset);

    ffac = DSET_BRICK_FACTOR(rtin->reg_dset, sub);
    if( ffac == 0.0 ) ffac = 1.0;

    dind = 0;   /* index into output data array */
    dptr = (void *) DSET_ARRAY(rtin->reg_dset, sub);
    for( iv=0 ; iv < nvox ; iv++ ) {
       if( !rtin->mask[iv] ) continue;  /* if not in mask, skip */

       /* we have a good voxel */

       /* set the data value (convert to float) */
       if( rtin->datum == MRI_short )
           data[dind++] = ((short *)dptr)[iv]*ffac;
       else if( rtin->datum == MRI_float )
           data[dind++] = ((float *)dptr)[iv]*ffac;
       else if( rtin->datum == MRI_byte )
           data[dind++] = ((byte *)dptr)[iv]*ffac;
    }

    return 0;
}

/*---------------------------------------------------------------------------
   Compute averages over mask ROIs for this sub-brick.    10 Nov 2006 [rickr]

   return 0 on success
-----------------------------------------------------------------------------*/
static int RT_mp_get_mask_aves( RT_input * rtin, int sub )
{
    static int       * nvals = NULL;
    static int         nval_len = 0;
    THD_3dim_dataset * source_set = NULL;
    byte             * mask = rtin->mask;
    float              ffac;
    int                c, nvox, datum;

    if( ISVALID_DSET(rtin->mrg_dset) ) source_set = rtin->mrg_dset;
    else                               source_set = rtin->reg_dset;

    if( !ISVALID_DSET(source_set) || DSET_NVALS(source_set) <= sub ){
       fprintf(stderr,"** RT_mp_get_mask_aves: not set for sub-brick %d\n",sub);
       return -1;
    }

    /* rcr - fix (want DSET_BRICK_TYPE, but do we drag in all of 3ddata.h? globals?) */
    if ( source_set == rtin->mrg_dset )
       datum = source_set->dblk->brick->imarr[0]->kind;
    else
       datum = rtin->datum;

    if( sub < 0 ) return 0;

    if( !rtin->mask || !rtin->mask_aves || rtin->mask_nvals <= 0 ) {
       fprintf(stderr,"** RT_mp_get_mask_aves: no mask information to apply\n");
       return -1;
    }

    nvox = DSET_NVOX(g_mask_dset);
    if( DSET_NVOX(source_set) != nvox ){
        /* terminal: whine, blow away the mask and continue */
        fprintf(stderr,"** nvox for mask (%d) != nvox for reg_dset (%d)\n"
                       "   terminating mask processing...\n",
                nvox, DSET_NVOX(source_set) );
        return RT_mp_mask_free(rtin);
    }

   /* verify local memory for sizes (mask is 0..nvals) */
    if( nval_len != (rtin->mask_nvals+1) ){
        nval_len = rtin->mask_nvals+1;
        nvals = (int *)realloc(nvals, nval_len * sizeof(int));
        if( !nvals ){
            fprintf(stderr,"** GMA: failed alloc of %d ints\n",nval_len);
            return RT_mp_mask_free(rtin);
        }
    }

    /* all is well, computed ROI averages */

    /* init sums and counts to zero (nval_len is max mask index +1) */
    for(c = 1; c < nval_len; c++ ) rtin->mask_aves[c] = 0.0;
    for(c = 1; c < nval_len; c++ ) nvals[c] = 0;

    ffac = DSET_BRICK_FACTOR(source_set, sub);
    if( ffac == 1.0 ) ffac = 0.0;


    /* try to be efficient... */
    switch( datum ){
        int iv, m;
        case MRI_short:{
           short * dar = (short *) DSET_ARRAY(source_set, sub);
           if( ffac ) {
               for( iv=0 ; iv < nvox ; iv++ )
                   if( (m = mask[iv]) ){
                       rtin->mask_aves[m] += (dar[iv] * ffac);
                       nvals[m] ++;
                   }
           }
           else {
               for( iv=0 ; iv < nvox ; iv++ )
                   if( (m = mask[iv]) ){
                       rtin->mask_aves[m] += dar[iv];
                       nvals[m] ++;
                   }
           }
        }
        break ;

        case MRI_float:{
           float * dar = (float *) DSET_ARRAY(source_set, sub);
           if( ffac ) {
               for( iv=0 ; iv < nvox ; iv++ )
                   if( (m = mask[iv]) ){
                       rtin->mask_aves[m] += (dar[iv] * ffac);
                       nvals[m] ++;
                   }
           }
           else {
               for( iv=0 ; iv < nvox ; iv++ )
                   if( (m = mask[iv]) ){
                       rtin->mask_aves[m] += dar[iv];
                       nvals[m] ++;
                   }
           }
        }
        break ;

        case MRI_byte:{
           byte * dar = (byte *) DSET_ARRAY(source_set, sub);
           if( ffac ) {
               for( iv=0 ; iv < nvox ; iv++ )
                   if( (m = mask[iv]) ){
                       rtin->mask_aves[m] += (dar[iv] * ffac);
                       nvals[m] ++;
                   }
           }
           else {
               for( iv=0 ; iv < nvox ; iv++ )
                   if( (m = mask[iv]) ){
                       rtin->mask_aves[m] += dar[iv];
                       nvals[m] ++;
                   }
           }
        }
        break ;
    }

    /* now take means */
    for( c = 1; c < nval_len; c++ )
    {
        if( nvals[c] ) rtin->mask_aves[c] /= (double)nvals[c];
        if( verbose > 1 )
            fprintf(stderr,"RTM: sub-brick %d, mask %d: %d vals, mean %f\n",
                    sub, c, nvals[c], rtin->mask_aves[c]);
    }
    if( verbose == 1 ) /* then only first mask */
        fprintf(stderr,"RTM: brick %d, mask %d (of %d): %d vals, mean %f\n",
                sub, 1, nval_len-1, nvals[1], rtin->mask_aves[1]);

    return 0;
}

static int RT_mp_mask_free( RT_input * rtin )
{
    if( rtin->mask )     { free(rtin->mask);      rtin->mask      = NULL; }
    if( rtin->mask_aves ){ free(rtin->mask_aves); rtin->mask_aves = NULL; }
    rtin->mask_nset  = 0;
    rtin->mask_nones = 0;
    rtin->mask_nvals = 0;
    rtin->mask_init = 0; /* Cameron Craddock turn off the mask */
    return 1;
}

/* show time at ms resolution, wrapping per hour */
static int RT_show_duration( char * mesg )
{
   struct timeval  tval ;
   struct timezone tzone ;
   static int s_sec = 0, s_msec = -1;
   int        sec = 0, msec = 0, sdiff, mdiff;

   gettimeofday( &tval , &tzone ) ;
   sec = ((int)tval.tv_sec)%3600;
   msec = ((int)tval.tv_usec)/1000;
   if ( s_msec < 0 ) mdiff = 0;
   else {
      sdiff = sec-s_sec;  if ( sdiff < 0 ) sdiff += 3600;
      mdiff = 1000*sdiff + msec-s_msec;
   }
   s_sec = sec;
   s_msec = msec;

   if( mesg ) fprintf(stderr,"RT TIME (%s): ", mesg);
   else       fprintf(stderr,"RT TIME : ");

   fprintf(stderr,"%d sec, %d ms (%d ms passed)\n", sec, msec, mdiff);

   return 0;
}

/* show time at ms resolution, wrapping per hour */
static int RT_mp_show_time( char * mesg )
{
   struct timeval  tval ;
   struct timezone tzone ;

   gettimeofday( &tval , &tzone ) ;

   if( mesg ) fprintf(stderr,"++ RT TIME (%s): ", mesg);
   else       fprintf(stderr,"++ RT TIME : ");

   fprintf(stderr,"%d seconds, %d ms\n", ((int)tval.tv_sec)%3600,
                                         ((int)tval.tv_usec)/1000);

   return 0;
}

/*---------------------------------------------------------------------------
   Check env vars for MP communication.

   NOTE: anything applied here that is also in the interface needs to
         be set as an env var if the interface is applied (so the most
         recent action is effective).

         currently includes: AFNI_REALTIME_Mask_Vals

   return 0 on success
-----------------------------------------------------------------------------*/
static int RT_mp_getenv( void )
{
    char * estr = NULL;

    estr = getenv("AFNI_REALTIME_Mask_Vals");
    if( estr != NULL ){
        int ii = PLUTO_string_index(estr,N_RT_MASK_METHODS,RT_mask_strings_ENV);
        if( ii >= 0 && ii < N_RT_MASK_METHODS ) g_mask_val_type = ii;
        if(verbose) fprintf(stderr,"++ RTM getenv: mvals = %s\n",
                              RT_mask_strings_ENV[g_mask_val_type]);
        if(verbose>1) fprintf(stderr,"-- RTM getenv, set from %s\n", estr);
    }

    if( ! g_mask_val_type ) return 0;

    /* do we want to spit out times? */
    estr = getenv("AFNI_REALTIME_SHOW_TIMES") ;
    g_show_times = (estr && (*estr == 'y' || *estr == 'Y'));
    if(estr && verbose>1)
        fprintf(stderr,"++ RTM getenv: show_times = %d\n", g_show_times);
    if( g_show_times ) RT_mp_show_time("mp_comm_init");

    /* maybe the user wants to set the hello version */
    estr = getenv("AFNI_REALTIME_SEND_VER") ;
    if( estr && (*estr == 'y' || *estr == 'Y') ) {
        g_MP_send_ver = 1;
        if( verbose ) fprintf(stderr,"RTM: will send hello version\n");
    }

    return 0;
}

/*---------------------------------------------------------------------------
   Initialize the motion parameter communications.        30 Mar 2004 [rickr]

   return   0 : on success
          < 0 : on error
-----------------------------------------------------------------------------*/
static int RT_mp_comm_init( RT_input * rtin )
{
    struct sockaddr_in   sin;
    struct hostent     * hostp;
    char                 magic_hi[] = { 0xab, 0xcd, 0xef, 0xab };
    int                  sd, send_nvals = 0, rv, mask_req;

     /* if error or not in use, return */
    if( RT_mp_getenv() || ! g_mask_val_type) {
        rtin->mp_tcp_use = -1;
        return -1;
    }

    /* set the 'hello' mode (based on the mask values choice)
     *    0 : original magic_hi
     *    1 : magic_hi[3] += 1   -> also send num_extra as int
     *    2 : magic_hi[3] += 2   -> also send num_extra as int for ALL_DATA
     *    3 : magic_hi[3] += 3   -> also send num_extra for ALL_DATA light
     *    4 : magic_hi[3] += 4   -> also send num_extra for nROIs and nones
     *
     * if we are prepared to send mask info, and hello mode is set, then
     * note to also send mask_nvals (or mask_nset) and possibly nones
     */
    mask_req = 0; /* note separately whether a mask is required */
    if( g_MP_send_ver ) {  /* send VERSION info */
        if ( g_mask_val_type == 1 ) {                       /* motion only */
            send_nvals = 0;
            magic_hi[3] += 1;
        } else if ( g_mask_val_type == 2 ) {                /* averages */
            mask_req = 1;
            send_nvals = rtin->mask_nvals;
            magic_hi[3] += 1;
        } else if( g_mask_val_type == 3 ) {                 /* all data */
            mask_req = 1;
            send_nvals = rtin->mask_nset;
            magic_hi[3] += 2;
        } else if( g_mask_val_type == 4 ) {                 /* data only */
            mask_req = 1;
            send_nvals = rtin->mask_nset;
            magic_hi[3] += 3;
        } else if( g_mask_val_type == 5 ) {         /* ROI aves and data */
            mask_req = 1;
            send_nvals = rtin->mask_nvals-1;
            /* NOTE: we will also send rtin->mask_nones, below */
            magic_hi[3] += 4;
        } else {                                            /* bad combo */
            fprintf(stderr,"** RTM init: unprepared for mask_val type %d\n",
                    g_mask_val_type);
            rtin->mp_tcp_use = -1;
            return -1;
        }
    }

    /* check the mask separately */
    if( mask_req && ! g_mask_dset ) {
       fprintf(stderr,"** RTM init: mask is required with mask_val type %d\n",
               g_mask_val_type);
       rtin->mp_tcp_use = -1;
       return -1;
    }

    if ( rtin->mp_tcp_sd != 0 )
        fprintf(stderr,"** warning, did we not close the MP socket?\n");

    if ( (hostp = gethostbyname(rtin->mp_host)) == NULL )
    {
        fprintf(stderr,"** cannot lookup host '%s'\n", rtin->mp_host);
        rtin->mp_tcp_use = -1;
        return -1;
    }

    /* fill the sockaddr_in struct */
    memset(&sin, 0, sizeof(sin));
    sin.sin_family      = AF_INET;
    sin.sin_addr.s_addr = ((struct in_addr *)(hostp->h_addr))->s_addr;
    sin.sin_port        = htons(rtin->mp_port);

    /* get a socket */
    if ( (sd = socket(AF_INET, SOCK_STREAM, 0)) == -1 )
    {
        perror("** RT_mp_comm socket");
        rtin->mp_tcp_use = -1;   /* let us not try, try again */
        return -1;
    }

    if ( connect(sd, (struct sockaddr *)&sin, sizeof(sin)) == -1 )
    {
        perror("** RT_mp_comm connect");
        rtin->mp_tcp_use = -1;
        return -1;
    }

    /* send the hello message */
    if ( (rv = send(sd, magic_hi, 4*sizeof(char), 0)) == -1 )
    {
        perror("** RT_mp_comm send hello");
        RT_mp_comm_close(rtin, -1);
        return -1;
    } else if ( rv != 4*sizeof(char) ) {
        fprintf(stderr,"** RT magic_hi: sent only %d of 4 bytes\n", rv);
        RT_mp_comm_close(rtin, -1);
    }

    /* do we send nvals for an altered magic_hi? */
    if ( g_MP_send_ver ) {
        if ( (rv = send(sd, &send_nvals, sizeof(int), 0)) == -1 )
        {
            perror("** RT_mp_comm send hello nvals");
            RT_mp_comm_close(rtin, -1);
            return -1;
        } else if ( rv != sizeof(int) ) {
            fprintf(stderr,"** RT nextra: sent only %d of 4 bytes\n", rv);
            RT_mp_comm_close(rtin, -1);
            return -1;
        }
        if( verbose )
            fprintf(stderr,"RTM: sent nextra = %d with hello\n", send_nvals);

        /* in the case of ROIs_and_data, also send rtin->nones */
        if( g_mask_val_type == 5 ) {
            send_nvals = rtin->mask_nones;
            if ( (rv = send(sd, &send_nvals, sizeof(int), 0)) == -1 )
            {
                perror("** RT_mp_comm send hello nones");
                RT_mp_comm_close(rtin, -1);
                return -1;
            } else if ( rv != sizeof(int) ) {
                fprintf(stderr,"** RT nones: sent only %d of 4 bytes\n", rv);
                RT_mp_comm_close(rtin, -1);
                return -1;
            }
            if( verbose )
                fprintf(stderr,"RTM: sent nones = %d with hello\n", send_nvals);
        }
    }

    fprintf(stderr,"RTM: opened motion param socket to %s:%d\n",
            rtin->mp_host, rtin->mp_port);

    /* everything worked out, we're good to Vincent */

    rtin->mp_tcp_sd = sd;

    return 0;
}


/*---------------------------------------------------------------------------
   Initialize the motion parameter communication variables. 30 Mar 2004 [rickr]

   We are expecting AFNI_REALTIME_MP_HOST_PORT to read "hostname:port".

   return   0 : on success
          < 0 : on error
-----------------------------------------------------------------------------*/
static int RT_mp_comm_init_vars( RT_input * rtin )
{
    char * ept, * cp;
    int    len;

    if ( rtin->mp_tcp_use < 0 )     /* we've failed out, do not try again */
        return 0;

    if ( rtin->mp_tcp_sd != 0 )
        fprintf(stderr,"** warning, did we not close the MP socket?\n");
    rtin->mp_tcp_sd   = 0;

    /* for now, we will only init this if the HOST:PORT env var exists */
    ept = getenv("AFNI_REALTIME_MP_HOST_PORT") ;  /* 09 Oct 2000 */
    if( ept == NULL ) {
        rtin->mp_tcp_use = -1;
        if( verbose > 1 )
           fprintf(stderr,"RTM: no MP_HOST_PORT, shutting down mp_tcp_use\n");
        return 0;
    }

    cp = strchr(ept, ':');      /* find ':' separator */

    if ( cp == NULL || !isdigit(*(cp+1)) )
    {
        fprintf(stderr,"** env var AFNI_REALTIME_MP_HOST_PORT must be in the "
                       "form hostname:port_num\n   (var is '%s')\n", ept);
        return -1;
    }

    len = cp - ept;     /* length of hostname */
    if ( len > 127 )
    {
        fprintf(stderr,"** motion param hostname restricted to 127 bytes,\n"
                       "   found %d from host in %s\n", len, ept);
        return -1;
    }

    fprintf(stderr,"RTM: found motion param env var '%s'\n", ept);

    rtin->mp_port = atoi(cp+1);
    strncpy(rtin->mp_host, ept, len);
    rtin->mp_host[len] = '\0';
    rtin->mp_tcp_use = 1;

    RT_mp_check_env_for_mask(); /* possibly set the mask based on env var */

    /* now set up the mask data, if g_mask_dset is set */
    if( g_mask_dset ) RT_mp_init_mask(rtin);

    return 0;
}

/* CC 2012; 3 Sep 2014 [rickr]
   initialize mask independent of the RT_mp_comm_init_vars, e.g. to use
   the mask values for detrending but not sending them anywhere */
static int RT_mp_init_mask( RT_input * rtin )
{
    int c, max;

    /* uses global variable, check that it is set */
    if( g_mask_dset == NULL ) {
        rtin->mask_init = -1;
        return -1;
    }

    if( rtin->mask_init == 1 ) return 0;      /* already done */

    fprintf(stderr,"RTM MASK: applying mask dataset %s...\n",
        DSET_FILECODE(g_mask_dset));

    if( rtin->mask ) free(rtin->mask) ; /* in case of change */
    if( thd_multi_mask_from_brick(g_mask_dset, 0, &rtin->mask) )
    {
        fprintf(stderr,"** RTM: failed to make mask from mask dset\n");
        rtin->mask = NULL;
        g_mask_dset = NULL;
        rtin->mask_init = -1;
        return -1;
    }

    /* now compute mask_nvals and allocate mask_aves */
    rtin->mask_nset = 0;
    for( c = 0, max = rtin->mask[0]; c < DSET_NVOX(g_mask_dset); c++ )
    {
        /* start by skipping non-mask voxels 19 Jan 2020 [rickr] */
        if( ! rtin->mask[c] ) continue;

        rtin->mask_nset++;                              /* count all set */

        if( rtin->mask[c] == 1  ) rtin->mask_nones++;   /* count vox==1  */
        if( rtin->mask[c] > max ) max = rtin->mask[c];  /* track max     */

    }

    rtin->mask_nvals = max;
    if( ! max )
    {
        fprintf(stderr,"** RTM: empty mask\n");
        rtin->mask = NULL;
        g_mask_dset = NULL;
        rtin->mask_init = -1;
        return 0;
    }

    /* and allocate, include unused index 0 */
    if((rtin->mask_aves = (double *)realloc(rtin->mask_aves,
        (max+1)*sizeof(double))) == NULL )
    {
        fprintf(stderr, "RTM: realloc failed!");
        rtin->mask_init = -1;
        return -1;
    }

    if( verbose ) fprintf(stderr,"RTM: have %d-value mask\n", max);

    rtin->mask_init=1;

    return 0;
}

/*---------------------------------------------------------------------------
   If AFNI_REALTIME_Mask_Dset is set, make sure g_mask_dset points to it.
   Let NONE mean the same as not being set.

   return   0 : on success
          < 0 : on error                              14 Mar 2011 [rickr]
-----------------------------------------------------------------------------*/
int RT_mp_check_env_for_mask( void )
{
    char * ept = getenv("AFNI_REALTIME_Mask_Dset");
    
    /* if no env mask, nuke any that is set and return */
    if( !ept || !strcmp(ept, "None") || !strcmp(ept, "NONE") || !strlen(ept) ){
        RT_mp_rm_env_mask(); /* then clear if set */
        return 0;
    }

    /*--- so ept should be applied ---*/

    /* if old name is the same, just keep it and move on */
    if( g_mask_dset_name && !strcmp(ept, g_mask_dset_name) ) {
        if( verbose ) fprintf(stderr,"RTM: keeping AR_Mask_Dset %s\n",
                              g_mask_dset_name);
        return 0;
    }

    /*--- we need a mask dataset ---*/

    RT_mp_rm_env_mask(); /* start by clearing any old one */

    /* copy dset name with new memory */
    g_mask_dset_name = strdup(ept);
    if( ! g_mask_dset_name ) {
        fprintf(stderr,"** dset mask name malloc failure\n");
        return -1;
    }

    g_mask_dset = THD_open_dataset(g_mask_dset_name) ;
    if( ! g_mask_dset ) {
        fprintf(stderr,"** failed to open mask dset %s\n", g_mask_dset_name);
        RT_mp_rm_env_mask();
        return -1;
    }
    DSET_load(g_mask_dset);  /* just to be sure */

    /* display this fact, regardless of verbose */
    fprintf(stderr,"RTM: setting AR_Mask_Dset from env: %s\n",g_mask_dset_name);

    return 0;
}

/* clear the g_mask_dset vars, if the name is set */
int RT_mp_rm_env_mask( void )
{
   if( ! g_mask_dset_name ) return 0;

   if( verbose ) fprintf(stderr,"RTM: clearing mask dataset name %s\n",
                         g_mask_dset_name);

   free(g_mask_dset_name); g_mask_dset_name = NULL;

   if( g_mask_dset ) {
      if( verbose ) fprintf(stderr,"RTM: clearing mask dataset\n");
      DSET_delete(g_mask_dset);
      g_mask_dset = NULL;
   }

   return 0;
}


/*---------------------------------------------------------------------------
   Initialize the parser fields from the user expression.  12 Feb 2004 [rickr]

   return   0 : on success
          < 0 : on error
-----------------------------------------------------------------------------*/
int RT_parser_init( RT_input * rtin )
{
    PARSER_set_printout(1);
    rtin->p_code = PARSER_generate_code( rtin->p_expr );

    if ( ! rtin->p_code )
    {
        fprintf(stderr,"** cannot parse expression '%s'\n", rtin->p_expr);
        return -1;
    }

    /* p_max_sym will be 0 if nothing is marked, 26 if z is, etc. */
    PARSER_mark_symbols( rtin->p_code, rtin->p_has_sym );
    for ( rtin->p_max_sym = 26; rtin->p_max_sym > 0; rtin->p_max_sym-- )
        if ( rtin->p_has_sym[rtin->p_max_sym - 1] )
            break;

    if ( rtin->p_max_sym > 6 )
    {
        fprintf(stderr,"** parser expression may only contain symbols a-f\n");
        return -2;
    }

    return 0;
}

/*---------------------------------------------------------------------------
   Process command strings in the buffer, up to the first '\0' character.
   Returns the number of characters processed, which will be one more than
   the index of the first '\0'.  If an error occurs, -1 is returned.
-----------------------------------------------------------------------------*/

#define BADNEWS     fprintf(stderr,"RT: illegal header info=%s\a\n",buf)
#define STARTER(st) (strncmp(buf,st,strlen(st)) == 0)
#define NBUF        1024

int RT_process_info( int ninfo , char * info , RT_input * rtin )
{
   int jj , nstart,nend , nbuf , nord , nb , nq ;
   int numte = 0;
   char buf[NBUF] , str32[32] ;

   if( rtin == NULL || info == NULL || ninfo == 0 ) return -1 ;

   if( drive_wait_list.len > 0 && verbose )
      fprintf(stderr,"** clearing old DRIVE_WAIT list\n");
   free_rt_string_list( &drive_wait_list );

   for( nend=0 ; nend < ninfo && info[nend] != '\0' ; nend++ ) ; /* nada */
   if( nend == ninfo ){
      fprintf(stderr,"RT: info string not NUL-terminated!\a\n") ;
      return -1 ;
   }

   /******************************************/
   /** Scan ahead until next command string **/

   nstart = 0 ;
   while( nstart < nend ){

      /** skip whitespace **/

      for( ; nstart < nend && isspace(info[nstart]) ; nstart++ ) ; /* nada */
      if( nstart >= nend ) break ;

      /** copy characters into buf until the next '\n' or '\0' **/

      for( nbuf=0,jj=nstart ; nbuf < NBUF && info[jj] != '\n' && info[jj] != '\0' ; ){
         buf[nbuf++] = info[jj++] ;
      }
      if( nbuf == NBUF ){
         fprintf(stderr,"RT: line buffer overflow in control information!\a\n") ;
         nbuf-- ;
      }
      buf[nbuf] = '\0' ; nstart = jj ;

      if( verbose > 1 )
         fprintf(stderr,"RT: info line buffer=%s\n",buf) ;
      VMCHECK ;

      /************************************/
      /*** Scan for legal input strings ***/

      if( STARTER("ACQUISITION_TYPE") ){
         char typ[32] ;

         sscanf( buf , "ACQUISITION_TYPE %31s" , typ ) ;

              if( strcmp(typ,"2D+z")  == 0 ) rtin->dtype = DTYPE_2DZ ;
         else if( strcmp(typ,"2D+zt") == 0 ) rtin->dtype = DTYPE_2DZT ;
         else if( strcmp(typ,"3D")    == 0 ) rtin->dtype = DTYPE_3D ;
         else if( strcmp(typ,"3D+t")  == 0 ) rtin->dtype = DTYPE_3DT ;
         /* allow 3D ordering, but with slice timing   3 Aug 2015 [rickr] */
         /* (apply as 3DT, except allow for timing info)                  */
         else if( strcmp(typ,"3D+timing") == 0 ) rtin->dtype = DTYPE_3DTM ;
         else
              BADNEWS ;

      /* Some troublesome physicist, let's just call him "Tom" (possibly
       * named by his mother, Mrs. Ross), wants control over the ranges
       * in the motion correction graph window.
       *                                                29 Jan 2004 [rickr] */

      } else if( STARTER("GRAPH_XRANGE") ){
         float fval = 0.0 ;
         sscanf( buf , "GRAPH_XRANGE %f" , &fval ) ;
         if( fval >= MIN_PIN && fval <= MAX_PIN ) {
             rtin->reg_graph_xnew = 1;
             rtin->reg_graph_xr   = fval;

             if( rtin->reg_graph && REG_IS_3D(rtin->reg_mode) &&
                 IM3D_OPEN(plint->im3d) )
             {
                 plot_ts_xypush(1-rtin->reg_graph_xnew, 1-rtin->reg_graph_ynew);
                 RT_set_grapher_pinnums((int)(fval+0.5));
             }

         } else
              BADNEWS ;

      } else if( STARTER("GRAPH_YRANGE") ){
         float fval = 0.0 ;
         sscanf( buf , "GRAPH_YRANGE %f" , &fval ) ;
         if( fval > 0.0 ) {
            rtin->reg_graph_ynew = 1;
            rtin->reg_graph_yr   = fval ;

            /* if the user sets scales, don't 'push'    11 Feb 2004 [rickr] */
            if( rtin->reg_graph && REG_IS_3D(rtin->reg_mode) )
                plot_ts_xypush(1-rtin->reg_graph_xnew, 1-rtin->reg_graph_ynew);
         } else
            BADNEWS ;

      /* Allow the user to specify an expression, to evalue the six motion
       * parameters into one.
       *                                                12 Feb 2004 [rickr] */
      } else if( STARTER("GRAPH_EXPR") ){
         sscanf( buf , "GRAPH_EXPR %1024s" , rtin->p_expr ) ;
         rtin->p_expr[RT_MAX_EXPR] = '\0';
         if ( RT_parser_init(rtin) != 0 )
            BADNEWS ;

      } else if( STARTER("NAME") ){
         char npr[THD_MAX_PREFIX] = "\0" ;
         /* RT_MAX_PREFIX is used here, currently 100 */
         sscanf( buf , "NAME %100s" , npr ) ; /* 31->100  29 Jan 2004 [rickr] */
         if( THD_filename_pure(npr) ) {
            strcpy( rtin->root_prefix , npr ) ;
            rtin->storage_mode = remove_SM_suffix(rtin->root_prefix) ;
         } else
              BADNEWS ;

      } else if( STARTER("PREFIX") ){           /* 01 Aug 2002 */
         char npr[THD_MAX_PREFIX] = "\0" ;
         /* RT_MAX_PREFIX is used here, currently 100 */
         sscanf( buf , "PREFIX %100s" , npr ) ;
         if( THD_filename_pure(npr) ) {
            strcpy( rtin->root_prefix , npr ) ;
            rtin->storage_mode = remove_SM_suffix(rtin->root_prefix) ;
         } else
              BADNEWS ;

      } else if( STARTER("NOTE") ) {            /* 01 Oct 2002: notes list */
         int nn = rtin->num_note ;
         if( nbuf > 6 ){
           int ii ;
           rtin->note = realloc( rtin->note , sizeof(char *)*(nn+1) ); /* extend array */
           rtin->note[nn] = strdup(buf+5) ;                            /* skip "NOTE "  */
           for( ii=0 ; rtin->note[nn][ii] != '\0' ; ii++ )             /* '\n' insertion */
             if( rtin->note[nn][ii] == '\a' || rtin->note[nn][ii] == '\f' )
               rtin->note[nn][ii] = '\n';
           rtin->num_note ++ ;
         } else
           BADNEWS ;

      } else if( STARTER("NUMVOL") ){
         int val = 0 ;
         sscanf( buf , "NUMVOL %d" , &val ) ;
         if( val > 0 ) rtin->nr = val ;
         else
              BADNEWS ;

      } else if( STARTER("TR") ){          /* units are seconds */
         float val = 0.0 ;
         sscanf( buf , "TR %f" , &val ) ;
         if( val > 0.0 ) rtin->tr = val ;
         else
              BADNEWS ;

      } else if( STARTER("ZDELTA") ){
         float val = 0.0 ;
         sscanf( buf , "ZDELTA %f" , &val ) ;
         if( val > 0.0 ) rtin->dzz = val ;
         else
              BADNEWS ;
         if( verbose > 1 )
            fprintf(stderr,"RT: dzz = %g\n",rtin->dzz) ;
         VMCHECK ;

      } else if( STARTER("ZGAP") ){               /* 18 Dec 2002 */
         float val = 0.0 ;
         sscanf( buf , "ZGAP %f" , &val ) ;
         if( val >= 0.0 ) rtin->zgap = val ;
         else
              BADNEWS ;
         if( verbose > 1 )
            fprintf(stderr,"RT: zgap = %g\n",rtin->zgap) ;
         VMCHECK ;

      } else if( STARTER("XYZOFF") ){             /* 18 Dec 2002 */
         float xval = 0.0 , yval = 0.0 , zval = 0.0 ;
         sscanf( buf , "XYZOFF %f %f %f" , &xval , &yval , &zval ) ;
         rtin->xxoff = xval ; rtin->xcen = 1 ;
         rtin->yyoff = yval ; rtin->ycen = 1 ;
         rtin->zzoff = zval ; rtin->zcen = 1 ;
         if( verbose > 1 )
            fprintf(stderr,"RT: offset = %g %g %g\n",rtin->xxoff,rtin->yyoff,
                                                     rtin->zzoff) ;
         VMCHECK ;

      } else if( STARTER("ZFIRST") ){   /* set z origin */
         float val = 0.0 ;
         char dcode = ' ' ;
         sscanf( buf , "ZFIRST %f%c" , &val,&dcode ) ;
         rtin->zzorg   = val ;
         rtin->zcen    = 0 ;
         rtin->zzdcode = ORCODE(dcode) ;
         if( verbose > 1 )
            fprintf(stderr,"RT: zzorg = %g%c\n" ,
                    rtin->zzorg ,
                    (rtin->zzdcode < 0) ? ' ' : ORIENT_first[rtin->zzdcode] ) ;
         VMCHECK ;

      } else if( STARTER("XYZFIRST") ){  /* 10 Dec 2002: set all origins */
         float xf=0.0,yf=0.0,zf=0.0 ;
         char  xc=' ',yc=' ',zc=' ' ;
         sscanf( buf , "XYZFIRST %f%c%f%c%f%c" , &xf,&xc,&yf,&yc,&zf,&zc ) ;
         rtin->xxorg = xf ; rtin->xcen = 0 ; rtin->xxdcode = ORCODE(xc) ;
         rtin->yyorg = yf ; rtin->ycen = 0 ; rtin->yydcode = ORCODE(yc) ;
         rtin->zzorg = zf ; rtin->zcen = 0 ; rtin->zzdcode = ORCODE(zc) ;
         if( verbose > 1 )
            fprintf(stderr,"RT: xxorg=%g%c yyorg=%g%c zzorg=%g%c\n" ,
                    rtin->xxorg ,
                    (rtin->xxdcode < 0) ? ' ' : ORIENT_first[rtin->xxdcode] ,
                    rtin->yyorg ,
                    (rtin->yydcode < 0) ? ' ' : ORIENT_first[rtin->yydcode] ,
                    rtin->zzorg ,
                    (rtin->zzdcode < 0) ? ' ' : ORIENT_first[rtin->zzdcode]
                   ) ;
         VMCHECK ;

      } else if( STARTER("XYFOV") ){
         float xval = 0.0 , yval = 0.0 , zval = 0.0 ;
         sscanf( buf , "XYFOV %f %f %f" , &xval , &yval , &zval ) ;
         if( xval > 0.0 ){
            rtin->xxfov = xval ;
            rtin->yyfov = (yval > 0.0) ? yval : xval ;
            if( zval > 0.0 ) rtin->zzfov = zval ;
         } else
                BADNEWS ;
         if( verbose > 1 )
            fprintf(stderr,"RT: fov = %g %g %g\n",rtin->xxfov,rtin->yyfov,rtin->zzfov) ;
         VMCHECK ;

      } else if( STARTER("XYMATRIX") ){
         int xval = 0 , yval = 0 , zval = 0 ;
         sscanf( buf , "XYMATRIX %d %d %d" , &xval , &yval , &zval ) ;
         if( xval > 1 ){
            rtin->nxx = xval ;
            rtin->nyy = (yval > 1) ? yval : xval ;
            if( zval > 0 ){
               rtin->nzz = zval ;
               if( rtin->nzz < 1 ) fprintf(stderr,"RT: # slices = %d!\a\n",zval) ;
            }
         } else
                BADNEWS ;
         if( verbose > 1 )
            fprintf(stderr,"RT: matrix = %d %d %d\n",rtin->nxx,rtin->nyy,rtin->nzz) ;
         VMCHECK ;

      } else if( STARTER("ZNUM") ){
         int zval = 0 ;
         sscanf( buf , "ZNUM %d" , &zval ) ;
         if( zval > 0 ){
             rtin->nzz = zval ;
             if( rtin->nzz < 1 ) fprintf(stderr,"RT: # slices = %d!\a\n",zval) ;
         }
         else
              BADNEWS ;
         if( verbose > 1 && rtin->nzz >= 1 )
            fprintf(stderr,"RT: # slices = %d\n",rtin->nzz) ;
         VMCHECK ;

      } else if( STARTER("DATUM") ){
         int ii ;
         char tstr[32] = "\0" ;
         sscanf( buf , "DATUM %31s" , tstr ) ;
         for( ii=0 ; ii <= LAST_MRI_TYPE ; ii++ )
            if( strcmp(tstr,MRI_TYPE_name[ii]) == 0 ) break ;

         if( AFNI_GOOD_DTYPE(ii) ) rtin->datum = ii ;
         else
              BADNEWS ;
         if( verbose > 1 )
            fprintf(stderr,"RT: datum code = %d\n",rtin->datum) ;
         VMCHECK ;

      } else if( STARTER("BYTEORDER") ){    /* 27 Jun 2003:           [rickr] */
         int bo = 0 ;
         char tstr[10] = "\0" ;
         sscanf( buf, "BYTEORDER %9s", tstr ) ;

         /* first, note the incoming endian */
         if      ( strncmp(tstr,"LSB_FIRST",9) == 0 ) bo = LSB_FIRST ;
         else if ( strncmp(tstr,"MSB_FIRST",9) == 0 ) bo = MSB_FIRST ;
         else
             BADNEWS ;

         /* if different from the local endian, we will swap bytes */
         if ( bo != 0 ) {
            int local_bo, one = 1;
            local_bo = (*(char *)&one == 1) ? LSB_FIRST : MSB_FIRST ;

            /* if we are informed, and the orders differ, we will swap */
            if ( bo != local_bo )
                rtin->swap_on_read = 1 ;
         }

         if( verbose > 1 )
            fprintf(stderr,"RT: BYTEORDER string = '%s', swap_on_read = %d\n",
                    BYTE_ORDER_STRING(bo), rtin->swap_on_read) ;
      } else if( STARTER("LOCK_ZORDER") ){  /* 22 Feb 1999:                   */
         rtin->zorder_lock = 1 ;            /* allow program to 'lock' zorder,*/
                                            /* so that later changes are      */
      } else if( STARTER("ZORDER") ){       /* ineffective.                   */
         if( ! rtin->zorder_lock ){
           str32[0] = '\0';  nb = 0; nord = 0 ; nq = 0 ;
           sscanf( buf , "ZORDER %31s%n" , str32 , &nb ) ;
                if( strcmp(str32,"alt") == 0 ) rtin->zorder = ZORDER_ALT ;
           else if( strcmp(str32,"seq") == 0 ) rtin->zorder = ZORDER_SEQ ;
           else if( strcmp(str32,"explicit") == 0 ){
              rtin->zorder = ZORDER_EXP ;
              do{                                  /* get all numbers */
                 rtin->zseq[nord] = -1 ; nq = -1 ;
                 sscanf(buf+nb , "%d%n" , &(rtin->zseq[nord]) , &nq) ;
                 if( nq < 1 ) break ;              /* failed to get */
                 nb += nq ; nord++ ;               /* otherwise, increment */
              } while( nb < nbuf ) ;               /* until end of buffer */
              rtin->nzseq = nord ;                 /* number found */
              if( nord < 1 || nord > NZMAX ) BADNEWS ;
           }
           else
                BADNEWS ;
         }

      } else if( STARTER("TPATTERN") ){               /* get timing pattern  */
         if( ! rtin->zorder_lock ){                   /* 10 May 2005 [rickr] */
           str32[0] = '\0';  nb = 0; nord = 0 ; nq = 0 ;
           sscanf( buf , "TPATTERN %31s%n" , str32, &nb ) ;
                if( strcmp(str32,"alt+z") == 0 ) rtin->tpattern = ZORDER_ALT ;
           else if( strcmp(str32,"seq+z") == 0 ) rtin->tpattern = ZORDER_SEQ ;
           else if( strcmp(str32,"explicit") == 0 ) {
              /* extract explicit slice timing           25 Apr 2011 [rickr] */
              rtin->tpattern = ZORDER_EXP ;
              do{                                  /* get all numbers */
                 rtin->stimes[nord] = -1 ; nq = -1 ;
                 sscanf(buf+nb , "%f%n" , &(rtin->stimes[nord]) , &nq) ;
                 if( nq < 1 ) break ;              /* failed to get */
                 nb += nq ; nord++ ;               /* otherwise, increment */
              } while( nb < nbuf ) ;               /* until end of buffer */
              rtin->nstimes = nord ;                 /* number found */
              if( nord < 1 || nord > NZMAX ) BADNEWS ;
           }
           else
                BADNEWS ;
         }

      } else if( STARTER("XYZAXES") ){
         int ii , orx=-1,ory=-1,orz=-1 ;
         char xstr[32] = "\0" , ystr[32] = "\0" , zstr[32] = "\0" ;

         sscanf( buf , "XYZAXES %31s %31s %31s" , xstr,ystr,zstr ) ;

         for( ii=0 ; ii < 6 ; ii++ ){
            if( strcmp(xstr,ORIENT_shortstr[ii]) == 0 ||
                strcmp(xstr,ORIENT_typestr[ii])  == 0 ||
                strcmp(xstr,ORIENT_tinystr[ii])  == 0   ) orx = ii ;

            if( strcmp(ystr,ORIENT_shortstr[ii]) == 0 ||
                strcmp(ystr,ORIENT_typestr[ii])  == 0 ||
                strcmp(ystr,ORIENT_tinystr[ii])  == 0   ) ory = ii ;

            if( strcmp(zstr,ORIENT_shortstr[ii]) == 0 ||
                strcmp(zstr,ORIENT_typestr[ii])  == 0 ||
                strcmp(zstr,ORIENT_tinystr[ii])  == 0   ) orz = ii ;
         }

         if( orx >= 0 && ory >= 0 && orz >= 0 && OR3OK(orx,ory,orz) ){
            rtin->orcxx = orx ;
            rtin->orcyy = ory ;
            rtin->orczz = orz ;
         } else
                BADNEWS ;

      } else if( STARTER("NUM_CHAN") ){     /* 01 Aug 2002 */
         int nn=0 ;
         sscanf( buf , "NUM_CHAN %d",&nn) ;
         if( nn >= 1 && nn <= MAX_CHAN )
           rtin->num_chan = nn ;
         else
                BADNEWS ;

      } else if( STARTER("OBLIQUE_XFORM") ){     /* 10 Jul 2008 */
         sscanf( buf ,
             "OBLIQUE_XFORM %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f",
             rtin->oblique_mat+0,  rtin->oblique_mat+ 1, rtin->oblique_mat+ 2,
             rtin->oblique_mat+3,  rtin->oblique_mat+ 4, rtin->oblique_mat+ 5,
             rtin->oblique_mat+6,  rtin->oblique_mat+ 7, rtin->oblique_mat+ 8,
             rtin->oblique_mat+9,  rtin->oblique_mat+10, rtin->oblique_mat+11,
             rtin->oblique_mat+12, rtin->oblique_mat+13, rtin->oblique_mat+14,
             rtin->oblique_mat+15);
         rtin->is_oblique = 1;

         if( verbose > 1 )
            fprintf(stderr,"RT: %s\n", buf);
         else
            fprintf(stderr,"RT: received OBLIQUE_XFORM\n");

      } else if( STARTER("DRIVE_AFNI") ){   /* 30 Jul 2002 */
         char cmd[RT_DRIVE_LIMIT]="\0" ;
         int ii, len = strlen(buf) ;
         if( len < 11 ){
            fprintf(stderr,"RT: DRIVE_AFNI lacks command\n") ;
         } else if ( len > RT_DRIVE_LIMIT ) {
            fprintf(stderr,"RT: DRIVE_AFNI length exceeds %d bytes, ignoring\n",
                    RT_DRIVE_LIMIT) ;
         } else {  /* the command is everything after "DRIVE_AFNI " */
            MCW_strncpy(cmd,buf+11, RT_DRIVE_LIMIT) ;
            if( verbose > 1 )
               fprintf(stderr,"RT: command DRIVE_AFNI %s\n",cmd) ;
            ii = AFNI_driver( cmd ) ;  /* just do it */
            if( ii < 0 )
               fprintf(stderr,"RT: command DRIVE_AFNI %s **FAILS**\n",cmd) ;
         }

      } else if( STARTER("DRIVE_WAIT") ){   /* 21 Aug 2008 */
         int len = strlen(buf) ;

         if( len < 11 ){
            fprintf(stderr,"RT: DRIVE_WAIT lacks command\n") ;
         } else {  /* the command is everything after "DRIVE_WAIT " */
            add_to_rt_slist( &drive_wait_list, buf+11 );
            if(verbose>1) fprintf(stderr,"RT: command DRIVE_WAIT %s\n",buf+11);
         }

      } else if( STARTER("ECHO_TIMES") ){   /* 12 Mar 2015 [rickr] */
         /* just see what we find, like explicit TPATTERN, and verify against
          * num_chan later (being more kind and gentle than I feel) */
         nb = strlen("ECHO_TIMES");  nord = 0 ;  nq = 0 ;
         numte = 0;
         do {
            sscanf(buf+nb, "%f%n", &(rtin->TE[numte]) , &nq) ;
            if( nq < 1 ) break;
            if( rtin->TE[numte] <= 0.0 ) {
               WARNING_message("bad echo time at start of %-20s", buf+nb);
               BADNEWS ;
            }
            nb += nq;
            numte++; /* got one */
         } while( nb < nbuf && numte < MAX_CHAN );
         /* test against num_chan later */

         if( numte < 1 ) BADNEWS ;
         else if( verbose > 1 )
            fprintf(stderr,"RT: found %d echo times in %s\n", numte, buf);

      } else {                              /* this is bad news */
         BADNEWS ;
      }
   }  /* end of loop over command buffers */

   /* check numte against num_chan (can equal, or numte == 1) */
   /* rcr - fix (fail if mismatch?  allow many 'real' channels and 1 te)*/
      
   if( numte > 0 ) {
      if( rtin->num_chan > 1 && numte == 1 ) /* then dupe */
         for( jj=1; jj < rtin->num_chan; jj++ ) rtin->TE[jj] = rtin->TE[0] ;
      else if( rtin->num_chan != numte ) {
         fprintf(stderr, "RT: have numte = %d but num_chan = %d\n",
                 numte, rtin->num_chan);
         /* leave as init to 0.0 or dupe?  okay, dupe last one */
         for( jj=numte; jj < rtin->num_chan; jj++ )
            rtin->TE[jj] = rtin->TE[numte-1] ;
      }
   }

   /** now, determine if enough information exists to create a dataset **/

   if( rtin->image_mode ){
      rtin->nzz      = 1 ;  /* 28 Apr 2000 */
      rtin->num_chan = 1 ;  /* 01 Aug 2002 */
   }

   /** 01 Aug 2002: turn some things off in multi-channel mode **/
   /** 06 Sep 2017: let user control registration (could be one channel) **/

   if( rtin->num_chan > 1 && RT_chmrg_reg_mode == RT_CM_RMODE_NONE ){

     /* leave user in charge of registration, particularly if this is */
     /* multi-echo data                            6 Sep 2017 [rickr] */

     // if( rtin->reg_mode > 0 && verbose )
     //   fprintf(stderr,"RT: %d channel acquisition => no registration!\n",
     //           rtin->num_chan) ;

     if( rtin->func_code > 0 && verbose )
       fprintf(stderr,"RT: %d channel acquisition => no function!\n"    ,
               rtin->num_chan) ;

     rtin->func_code = FUNC_NONE ;     /* no function */
     rtin->func_func = NULL ;
     /* leave user in charge of registration     6 Sep 2017 [rickr] */
     // rtin->reg_mode  = REGMODE_NONE ;  * no registration */
     // rtin->reg_graph = 0 ;
   }

   if( rtin->nzz == 1 ){                   /* 24 Jun 2002: 1 slice only? */
     rtin->zorder = ZORDER_SEQ ;
     rtin->tpattern = ZORDER_SEQ ;
     if( REG_IS_3D(rtin->reg_mode) ){
       rtin->reg_mode = REGMODE_NONE ;
       fprintf(stderr,"RT: can't do 3D registration on 2D dataset!\n") ;
     }
   }

   /** 27 Oct 2016 [rickr]: check g_reg_src_chan against num_chan **/
   if( (g_reg_src_chan < 0) || (g_reg_src_chan >= rtin->num_chan) ) {
     fprintf(stderr,"RT: ** bad src_chan %d for %d channels, clearning...\n",
             g_reg_src_chan, rtin->num_chan);
     g_reg_src_chan = 0;
   }

   RT_check_info( rtin , 0 ) ;

   /** if possible, now compute the number of bytes in each input image **/

   if( AFNI_GOOD_DTYPE(rtin->datum) && rtin->nxx > 0 && rtin->nyy > 0 ){
      int n1 = mri_datum_size( rtin->datum ) ;

      if( rtin->dtype == DTYPE_2DZT || rtin->dtype == DTYPE_2DZ )
         rtin->imsize = rtin->nxx * rtin->nyy * n1 ;
      else if( rtin->nzz > 0 )
         rtin->imsize = rtin->nxx * rtin->nyy * rtin->nzz * n1 ;
   }

   /* validate data type when swapping */
   if ( rtin->swap_on_read == 1 ) {

      if( (rtin->datum != MRI_short) &&         /* if the type is not okay, */
          (rtin->datum != MRI_int)   &&         /* then turn off swapping   */
          (rtin->datum != MRI_float) &&
          (rtin->datum != MRI_complex) )
      {
         if( rtin->datum != MRI_byte )          /* don't complain about bytes */
             fprintf(stderr,"RT: BYTEORDER applies only to short, int, float "
                            "or complex\n");
         rtin->swap_on_read = 0;
      }
   }

   /** return the number of characters processed **/

   return (nend+1) ;
}

/*--------------------------------------------------------------------
   Create the dataset and prepare to receive image data.  Some of this
   code is taken shamelessly from to3d.c (I wrote it, I can steal it).
   This routine should only be called when rtin->info_ok is true.
----------------------------------------------------------------------*/

void RT_start_dataset( RT_input * rtin )
{
   THD_3dim_dataset *dset=NULL ;
   THD_ivec3 nxyz , orixyz ;
   THD_fvec3 dxyz , orgxyz ;
   int nvox , n1 , ii , cc, base_ind ;
   char npr[THD_MAX_PREFIX] , ccpr[THD_MAX_PREFIX] ;

   /*********************************************/
   /** 28 Apr 2000: Image Only mode is simpler **/

   if( rtin->image_mode ){
      nvox = rtin->nxx * rtin->nyy ;
      n1   = mri_datum_size( rtin->datum ) ;

      rtin->sbr_size = nvox * n1 ;                /* size of image space */
      rtin->sbr[0]   = malloc( rtin->sbr_size ) ; /* image space */
      rtin->imsize   = rtin->sbr_size ;
      rtin->im[0]    = rtin->sbr[0] ;             /* image space again */
      rtin->nzz      = 1 ;

      rtin->image_space = mri_new_vol_empty( rtin->nxx, rtin->nyy, 1, rtin->datum ) ;
      mri_fix_data_pointer( rtin->sbr[0] , rtin->image_space ) ;

      return ;
   }

   /*******************************************/
   /** Must create a dataset and other stuff **/

   /*******************************************************************/
   /** 02 Aug 2002: in multi-channel mode, need multiple controllers **/

   if( rtin->num_chan > 1 ){
     int ic , tc , nc=AFNI_count_controllers() , sn ;
     int maxnc ;  /* 20 May 2009 -- for MCW and Andre Jesmanowicz */

     Three_D_View *im3d = plint->im3d ;             /* default controller */
     if( !IM3D_OPEN(im3d) )                         /* may not be open */
       im3d = AFNI_find_open_controller() ;

     sn = im3d->vinfo->sess_num ;                   /* session for all channels */

     /* 20 May 2009: limit the number of controllers to allow */

     maxnc = (int)AFNI_numenv("AFNI_REALTIME_MAX_CONTROLLERS") ;
          if( maxnc <= 0              ) maxnc = 2 ;
     else if( maxnc > MAX_CONTROLLERS ) maxnc = MAX_CONTROLLERS ;

     /* open new controllers if needed */

     if( nc < rtin->num_chan && nc < maxnc ){
       nc = MIN( rtin->num_chan , maxnc ) - nc ;  /* number to add */

       fprintf(stderr,
           "RT: %d channel data requires opening %d more AFNI controller%c\n",
           rtin->num_chan , nc , (nc==1) ? ' ' : 's' ) ;

       for( ic=0 ; ic < nc ; ic++ )                         /* add them */
         AFNI_clone_controller_CB(NULL,NULL,NULL) ;
     }

     /* make list of open controllers */

     num_open_controllers = AFNI_count_controllers() ;
     for( ic=nc=0 ; ic < MAX_CONTROLLERS ; ic++ ){
       if( IM3D_OPEN(GLOBAL_library.controllers[ic]) ){
         open_controller[nc]       = GLOBAL_library.controllers[ic] ;
         open_controller_index[nc] = ic ;
         nc++ ;
       }
     }
     nc = num_open_controllers ;  /* nugatory or moot */

     /* now assign channels to controllers */

     tc = MIN( nc , rtin->num_chan ) ; tc = MIN( tc , maxnc ) ;
     for( cc=0 ; cc < rtin->num_chan ; cc++ ){
       if( cc < tc ) rtin->im3d[cc] = open_controller[cc] ; /* next available */
       else          rtin->im3d[cc] = NULL ;                /* none available */

       /* all channels go to the 1st session directory */

       rtin->sess_num[cc] = sn ;
       rtin->sess[cc]     = GLOBAL_library.sslist->ssar[sn] ;
     }

   /* single channel mode: make sure calling controller is still open */

   } else {
     Three_D_View *im3d = plint->im3d ;             /* default controller */
     if( !IM3D_OPEN(im3d) )                         /* may not be open */
       im3d = AFNI_find_open_controller() ;

     rtin->im3d[0]     = im3d ;
     rtin->sess_num[0] = rtin->im3d[0]->vinfo->sess_num ;
     rtin->sess[0]     = GLOBAL_library.sslist->ssar[rtin->sess_num[0]] ;
   }

   /***************************/
   /** Let there be dataset! **/  /* 01 Aug 2002: or datasets */

   for( cc=0 ; cc < rtin->num_chan ; cc++ ){
     rtin->dset[cc] = EDIT_empty_copy(NULL) ;  /* a really empty dataset */

     tross_Append_History( rtin->dset[cc] , "plug_realtime: creation" ) ;

     if( rtin->num_note > 0 && rtin->note != NULL ){  /* 01 Oct 2002 */
       for( ii=0 ; ii < rtin->num_note ; ii++ )
         tross_Add_Note( rtin->dset[cc] , rtin->note[ii] ) ;
     }
   }

   /******************************************/
   /** make a good dataset prefix (somehow) **/

   num_runs++ ;  /* 20 May 2009 */

   /* maybe the user prefers to start each new prefix at --001 */
   base_ind = num_runs;
   if( AFNI_yesenv("AFNI_REALTIME_reset_output_index") ) base_ind = 1;

   for( ii=base_ind ; ; ii++ ){  /* will loop until it succeeds! */

      /* use __001 etc., as '#' evaluates to a comment and '-' is
         used for options                     27 Jul 2009 [rickr] */
      sprintf( npr , "%.*s__%03d" , RT_MAX_PREFIX, rtin->root_prefix , ii ) ;

      if( rtin->num_chan == 1 ){                  /* the old way */

         if( PLUTO_prefix_ok(npr) ) break ;

      } else {                                    /* 02 Aug 2002: the multichannel way */

        for( cc=0 ; cc < rtin->num_chan ; cc++ ){ /* check each channel prefix */
          sprintf(ccpr, "%s_%02d", npr,cc+1 ) ;
          if( !PLUTO_prefix_ok(ccpr) ) break ;    /* not good? quit cc loop */
        }
        if( cc == rtin->num_chan ) break ;        /* all were good? we are done */
      }
   }

   /**********************************************************/
   /** correct the spatial information for axes orientation **/

   rtin->dxx = rtin->xxfov / rtin->nxx ;
   rtin->dyy = rtin->yyfov / rtin->nyy ;

   /* 18 Dec 2002: add zgap here, per UCSD request */

   if( rtin->zzfov > 0 ) rtin->dzz = rtin->zzfov / rtin->nzz + rtin->zgap ;

   /* 18 Dec 2002: add xxoff (etc.) here, per UCSD request */

   if( rtin->xcen ) rtin->xxorg = 0.5 * (rtin->nxx - 1) * rtin->dxx + rtin->xxoff ;
   if( rtin->ycen ) rtin->yyorg = 0.5 * (rtin->nyy - 1) * rtin->dyy + rtin->yyoff ;
   if( rtin->zcen ) rtin->zzorg = 0.5 * (rtin->nzz - 1) * rtin->dzz + rtin->zzoff ;

   /* if axis direction is a 'minus', voxel increments are negative */

   if( ORIENT_sign[rtin->orcxx] == '-' ) rtin->dxx = - rtin->dxx ;
   if( ORIENT_sign[rtin->orcyy] == '-' ) rtin->dyy = - rtin->dyy ;
   if( ORIENT_sign[rtin->orczz] == '-' ) rtin->dzz = - rtin->dzz ;

   /* 10 Dec 2002:
      We now allow direction codes input for x and y as well,
      so must duplicate the complicated zzorg logic for setting
      the xxorg and yyorg signs.  This duplicated logic follows
      the zzdcode stuff, below.                                 */

#if 0
   /* if axis direction is a 'plus',
      then the origin is in a 'minus' direction,
      so the origin offset must have its sign changed */  /*** THE OLD WAY ***/

   if( ORIENT_sign[rtin->orcxx] == '+' ) rtin->xxorg = - rtin->xxorg ;
   if( ORIENT_sign[rtin->orcyy] == '+' ) rtin->yyorg = - rtin->yyorg ;
#endif

   /* z-origin is complex, because the remote program might
      have given a direction code, or it might not have.
      We must set the sign of the z-origin based on the
      sign of the axis direction if no specific
      direction code is given, otherwise it should be
      based on the specific direction code (rtin->zzdcode). */

   if( rtin->zzdcode < 0 ){  /* no specific code */

      if( ORIENT_sign[rtin->orczz] == '+' ) rtin->zzorg = - rtin->zzorg ;

   } else {  /* have code */

      /* check if code matches the axis direction
         (or is the opposite, which is also OK).  */

      if( rtin->orczz != rtin->zzdcode                 &&
          rtin->orczz != ORIENT_OPPOSITE(rtin->zzdcode)  ){  /* this is bad */

         fprintf(stderr,"RT: ZFIRST direction code = %c but Z axis = %s!\a\n",
                 ORIENT_first[rtin->zzdcode] , ORIENT_shortstr[rtin->orczz] ) ;

         if( ORIENT_sign[rtin->orczz] == '+' ) rtin->zzorg = - rtin->zzorg ;

      } else {  /* things are OK */

         if( ORIENT_sign[rtin->zzdcode] == '+' ) rtin->zzorg = - rtin->zzorg ;
      }
   }

   /* 10 Dec 2002: duplicate logic for xxorg */

   if( rtin->xxdcode < 0 ){
      if( ORIENT_sign[rtin->orcxx] == '+' ) rtin->xxorg = - rtin->xxorg ;
   } else {
      if( rtin->orcxx != rtin->xxdcode                 &&
          rtin->orcxx != ORIENT_OPPOSITE(rtin->xxdcode)  ){
         fprintf(stderr,"RT: XFIRST direction code = %c but X axis = %s!\a\n",
                 ORIENT_first[rtin->xxdcode] , ORIENT_shortstr[rtin->orcxx] ) ;
         if( ORIENT_sign[rtin->orcxx] == '+' ) rtin->xxorg = - rtin->xxorg ;
      } else {  /* things are OK */
         if( ORIENT_sign[rtin->xxdcode] == '+' ) rtin->xxorg = - rtin->xxorg ;
      }
   }

   /* 10 Dec 2002: duplicate logic for yyorg */

   if( rtin->yydcode < 0 ){
      if( ORIENT_sign[rtin->orcyy] == '+' ) rtin->yyorg = - rtin->yyorg ;
   } else {
      if( rtin->orcyy != rtin->yydcode                 &&
          rtin->orcyy != ORIENT_OPPOSITE(rtin->yydcode)  ){
         fprintf(stderr,"RT: YFIRST direction code = %c but Y axis = %s!\a\n",
                 ORIENT_first[rtin->yydcode] , ORIENT_shortstr[rtin->orcyy] ) ;
         if( ORIENT_sign[rtin->orcyy] == '+' ) rtin->yyorg = - rtin->yyorg ;
      } else {  /* things are OK */
         if( ORIENT_sign[rtin->yydcode] == '+' ) rtin->yyorg = - rtin->yyorg ;
      }
   }

   /************************************************/
   /** add the spatial information to the dataset **/  /* 01 Aug 2002: datasets */

   nxyz.ijk[0]   = rtin->nxx   ; dxyz.xyz[0]   = rtin->dxx ;
   nxyz.ijk[1]   = rtin->nyy   ; dxyz.xyz[1]   = rtin->dyy ;
   nxyz.ijk[2]   = rtin->nzz   ; dxyz.xyz[2]   = rtin->dzz ;

   orixyz.ijk[0] = rtin->orcxx ; orgxyz.xyz[0] = rtin->xxorg ;
   orixyz.ijk[1] = rtin->orcyy ; orgxyz.xyz[1] = rtin->yyorg ;
   orixyz.ijk[2] = rtin->orczz ; orgxyz.xyz[2] = rtin->zzorg ;

   for( cc=0 ; cc < rtin->num_chan ; cc++ ){  /* 01 Aug 2002: loop over datasets */

     if( rtin->num_chan == 1 )
       strcpy( ccpr , npr ) ;                      /* 1 channel prefix */
     else
       sprintf( ccpr , "%s_%02d" , npr , cc+1 ) ;  /* new prefix for multi-channel */

     EDIT_dset_items( rtin->dset[cc] ,
                         ADN_prefix      , ccpr ,
                         ADN_datum_all   , rtin->datum ,
                         ADN_nxyz        , nxyz ,
                         ADN_xyzdel      , dxyz ,
                         ADN_xyzorg      , orgxyz ,
                         ADN_xyzorient   , orixyz ,
                         ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                         ADN_nvals       , 1 ,
                         ADN_type        , HEAD_ANAT_TYPE ,
                         ADN_view_type   , VIEW_ORIGINAL_TYPE ,
                         ADN_func_type   , ANAT_EPI_TYPE ,
                      ADN_none ) ;

      /* and set the mode based on what was passed */
      if( rtin->storage_mode )
         rtin->dset[cc]->dblk->diskptr->storage_mode = rtin->storage_mode;

      /******************************************/
      /** add time axis information, if needed **/

      if( rtin->dtype == DTYPE_3DT ||                      /* all slices   */
         (rtin->dtype == DTYPE_2DZT && rtin->nzz == 1) ){  /* simultaneous */

         EDIT_dset_items( rtin->dset[cc] ,
                             ADN_ntt      , 1 ,
                             ADN_ttorg    , 0.0 ,
                             ADN_ttdel    , rtin->tr ,
                             ADN_ttdur    , 0.0 ,
                             ADN_tunits   , UNITS_SEC_TYPE ,
                          ADN_none ) ;

      } else if( rtin->dtype == DTYPE_2DZT || rtin->dtype == DTYPE_3DTM ){
         /* slices at different times */

         float * tpattern  = (float *) malloc( sizeof(float) * rtin->nzz ) ;
         float   tframe    = rtin->tr / rtin->nzz ;
         float   tsl ;
         int     ii ;

         /* check for explicit timing (e.g. Siemens)    25 apr 2011 [rickr] */
         if( rtin->tpattern == ZORDER_EXP && rtin->nstimes == rtin->nzz ){
            for( ii=0 ; ii < rtin->nzz ; ii++ )
               tpattern[ii] = rtin->stimes[ii] ;
         } else if( rtin->zorder==ZORDER_ALT || rtin->tpattern == ZORDER_ALT ){
            /* alternating +z direction */
            tsl = 0.0 ;
            for( ii=0 ; ii < rtin->nzz ; ii+=2 ){
               tpattern[ii] = tsl ; tsl += tframe ;
            }
            for( ii=1 ; ii < rtin->nzz ; ii+=2 ){
               tpattern[ii] = tsl ; tsl += tframe ;
            }
         /* make sequential a terminating case     10 May 2005 [rickr] */
         } else {
            tsl = 0.0 ;
            for( ii=0 ; ii < rtin->nzz ; ii++ ){
               tpattern[ii] = tsl ; tsl += tframe ;
            }
         }

         EDIT_dset_items( rtin->dset[cc] ,
                             ADN_ntt      , 1 ,
                             ADN_ttorg    , 0.0 ,
                             ADN_ttdel    , rtin->tr ,
                             ADN_ttdur    , 0.0 ,
                             ADN_tunits   , UNITS_SEC_TYPE ,
                             ADN_nsl      , rtin->nzz ,
                             ADN_zorg_sl  , rtin->zzorg ,
                             ADN_dz_sl    , rtin->dzz ,
                             ADN_toff_sl  , tpattern ,
                          ADN_none ) ;

         free( tpattern ) ;
      }

      /*****************************************************************/
      /** add oblique information, if supplied    10 Jul 2008 [rickr] **/
      if( rtin->is_oblique )
         memcpy(rtin->dset[cc]->daxes->ijk_to_dicom_real.m,
                rtin->oblique_mat, 16*sizeof(float));

      rtin->afni_status[cc] = 0 ;  /* uninformed at this time */
      DSET_lock(rtin->dset[cc]) ;  /* 20 Mar 1998 */
   }

   /*---- 02 Jun 2009: make a dataset for merger, if need be ----*/

   /* if not appropriate for merging, disable */
   if( RT_chmrg_mode && 
        (rtin->num_chan == 1 || !MRI_IS_FLOAT_TYPE(rtin->datum) ) ) {
     /* fail only if not in OPT_COMB mode */
     if( ! (RT_chmrg_mode==RT_CHMER_OPT_COMB && rtin->datum == MRI_short) ) {
        if( verbose > 0 )
           fprintf(stderr,"** RTCM: cancel merge, num_chan=%d, datum=%d,"
                          "chmrg_mode=%d\n", rtin->num_chan, rtin->datum,
                                             RT_chmrg_mode);
        RT_chmrg_mode = 0 ;  /* disable merger */
     }
   }

   if( RT_chmrg_mode > 0 ){
     char qbuf[128] ; int mdatum=MRI_float ; /* default merge datum is float */

     rtin->mrg_dset = EDIT_empty_copy( rtin->dset[0] ) ;
     sprintf(qbuf,"plug_realtime: merger %s",RT_chmrg_strings[RT_chmrg_mode]) ;
     tross_Append_History( rtin->mrg_dset , qbuf ) ;

     strcpy(ccpr,npr) ; strcat(ccpr,"%mrg_") ;
     strcat(ccpr,RT_chmrg_labels[RT_chmrg_mode]) ;
     EDIT_dset_items( rtin->mrg_dset , ADN_prefix , ccpr , ADN_none ) ;
      /* and set the mode based on what was passed */
     if( rtin->storage_mode )
        rtin->mrg_dset->dblk->diskptr->storage_mode = rtin->storage_mode;

     DSET_lock(rtin->mrg_dset) ;

     if( rtin->datum == MRI_complex && RT_chmrg_mode == RT_CHMER_SUM )
       mdatum = MRI_complex ;
     EDIT_dset_items( rtin->mrg_dset , ADN_datum_all , mdatum , ADN_none ) ;
     RT_chmrg_datum = mdatum ;

     if( rtin->num_note > 0 && rtin->note != NULL ){
       for( ii=0 ; ii < rtin->num_note ; ii++ )
         tross_Add_Note( rtin->mrg_dset , rtin->note[ii] ) ;
     }

     /* and copy any channel merge list */
     if( rtin->chan_list_str ) free(rtin->chan_list_str);
     rtin->chan_list_str = nifti_strdup(RT_chmrg_list);
   }

   /* if reg_chan_mode is set, verify there is something to do
      note: reg_chan_mode specifies to register the mrg_dset, and maybe
            the channel dsets as followers          19 May 2010 [rickr] */
   rtin->reg_chan_mode = RT_chmrg_reg_mode;
   if( RT_will_register_merged_dset(rtin) ) {
     if( ! rtin->mrg_dset ) {
       if( verbose > 0 ) fprintf(stderr,"** RTCM: no merge dset to register\n");
       rtin->reg_chan_mode = RT_CM_RMODE_NONE;
     } else if ( rtin->reg_mode != REGMODE_3D_RTIME ) {
       fprintf(stderr,"** RTCM: 3D RT registration required for merge reg\n");
       rtin->reg_chan_mode = RT_CM_RMODE_NONE;
     } else if( verbose > 0 && rtin->reg_chan_mode == RT_CM_RMODE_REG_MRG)
       fprintf(stderr,"RTCM: plan to register mrg_dset\n");
     else if( verbose > 0 && rtin->reg_chan_mode >= RT_CM_RMODE_REG_CHAN )
       fprintf(stderr,"RTCM: plan to register %d channels, mode %s\n",
               rtin->num_chan, RT_chmrg_rmode_labels[rtin->reg_chan_mode]);
   }


   if( REG_MAKE_DSET(rtin->reg_mode) &&
       ((rtin->dtype==DTYPE_2DZT) || (rtin->dtype==DTYPE_3DT)) ||
        (rtin->dtype==DTYPE_3DTM) ){

      /* if registering mrg_dset, use it as base for reg_dset */
      if( RT_will_register_merged_dset(rtin) ) {
         if( verbose > 1 )
            fprintf(stderr,"RTCM: using MERGE dset for registration grid\n");
         rtin->reg_dset = EDIT_empty_copy( rtin->mrg_dset ) ;
      } else
         rtin->reg_dset = EDIT_empty_copy( rtin->dset[g_reg_src_chan] ) ;

      tross_Append_History( rtin->reg_dset , "plug_realtime: registration" ) ;

      strcpy(ccpr,npr) ;
           if( REG_IS_2D(rtin->reg_mode) ) strcat(ccpr,"%reg2D") ;
      else if( REG_IS_3D(rtin->reg_mode) ) strcat(ccpr,"%reg3D") ;
      else                                 strcat(ccpr,"%reg"  ) ;

      EDIT_dset_items( rtin->reg_dset , ADN_prefix , ccpr , ADN_none ) ;
      /* and set the mode based on what was passed */
      if( rtin->storage_mode )
         rtin->reg_dset->dblk->diskptr->storage_mode = rtin->storage_mode;

      DSET_lock(rtin->reg_dset) ;

      if( rtin->num_note > 0 && rtin->note != NULL ){  /* 01 Oct 2002 */
        for( ii=0 ; ii < rtin->num_note ; ii++ )
          tross_Add_Note( rtin->reg_dset , rtin->note[ii] ) ;
      }
   }



   /* Cameron Craddock April 2012 added for real-time detrend */
   /* get the detrending environment variables */
   RT_detrend_getenv( rtin );
   if( rtin->detrend_mode > RT_DETREND_NONE )
   { /* CC add test here to see if detrending enabled */
      char qbuf[THD_MAX_PREFIX];

      /* CC create a dataset using reg_dset as a template */
      if(( rtin->reg_mode == REGMODE_2D_RTIME ) || 
         ( rtin->reg_mode == REGMODE_3D_RTIME ))
      {
          if( verbose > 1 )
             fprintf(stderr,"RTCM: using reg dset for detrending grid\n");
    
          rtin->detrend_dset = EDIT_empty_copy( rtin->reg_dset ) ;
          /* CC add _detrend to the end of the filename */
          strcat(ccpr,"_detrend");
      }
      else
      {
          if( verbose > 1 )
              fprintf(stderr,"RTCM: using base dset for detrending grid\n");
    
          rtin->detrend_dset = EDIT_empty_copy( rtin->dset[0] ) ;
          /* CC add _detrend to the end of the filename */
          strcpy( ccpr , npr ); strcat(ccpr,"_detrend");
      }

      /* CC add parameters to this string as they come in */
      snprintf( qbuf, THD_MAX_PREFIX,
          "plug_realtime: detrending mode=0x%x, polort=%d", 
          rtin->detrend_mode, rtin->detrend_polort );
      tross_Append_History( rtin->detrend_dset , qbuf ) ;

      /* set the prefix and make sure that it is float */
      EDIT_dset_items( rtin->detrend_dset , ADN_prefix , ccpr , 
                       ADN_datum_all, MRI_float, ADN_none ) ;
      DSET_lock(rtin->detrend_dset) ;

      /* CC just copied this from reg_dset, copies some notes from dataset,
         (i think) */ 
      if( rtin->num_note > 0 && rtin->note != NULL ){
        for( ii=0 ; ii < rtin->num_note ; ii++ )
          tross_Add_Note( rtin->detrend_dset , rtin->note[ii] ) ;
      }
   }
   /* CC set these to zero again */
   rtin->detrend_status    = 0 ;
   rtin->detrend_nvol      = 0 ;
   /* CC end */

   /* if registering channels according to mrg_dset, fill reg_chan_dset */
   /* (based on channels, not mrg_dset)             26 May 2010 [rickr] */
   if( rtin->reg_chan_mode >= RT_CM_RMODE_REG_CHAN ) {
      char qbuf[THD_MAX_PREFIX];

      for( cc=0 ; cc < rtin->num_chan ; cc++ ){

        dset = EDIT_empty_copy( rtin->dset[0] ) ;
        sprintf(qbuf,"plug_realtime: chan merge %s",
                RT_chmrg_rmode_strings[rtin->reg_chan_mode]) ;
        tross_Append_History( dset, qbuf );

        strcpy(ccpr,npr) ; strcat(ccpr,"%chanreg");
        sprintf(qbuf, "%s_%02d", ccpr, cc+1);

        EDIT_dset_items( dset , ADN_prefix , qbuf , ADN_none ) ;
        /* and set the mode based on what was passed */
        if( rtin->storage_mode )
           dset->dblk->diskptr->storage_mode = rtin->storage_mode;

        DSET_lock(dset);

        if( rtin->num_note > 0 && rtin->note != NULL ){
          for( ii=0 ; ii < rtin->num_note ; ii++ )
            tross_Add_Note( dset , rtin->note[ii] ) ;
        }

        rtin->reg_chan_dset[cc] = dset ;
      }
   }

   rtin->reg_status    = 0 ;
   rtin->reg_nvol      = 0 ;

   /* No longer scale to time units as x-axis is now in terms of reps.
    *                                      *** 29 Jan 2004 [rickr] ***
    *  rtin->reg_graph_xr *= rtin->tr ;    *** scale to time units ***
    */

   /***********************************************/
   /** now prepare space for incoming image data **/

   nvox = rtin->nxx * rtin->nyy * rtin->nzz ;
   n1   = mri_datum_size( rtin->datum ) ;

   rtin->sbr_size = nvox * n1 ;                /* size of sub-brick */
   for( cc=0 ; cc < rtin->num_chan ; cc++ ){
     rtin->sbr[cc] = malloc( rtin->sbr_size ) ; /* space to hold sub-brick */
     if( rtin->sbr[cc] == NULL ){
       fprintf(stderr,
               "RT: can't malloc data space for real-time channel %02d!\a\n",
               cc+1) ;
       EXIT(1) ;
     }
   }

   if( rtin->dtype == DTYPE_2DZT || rtin->dtype == DTYPE_2DZ )
      rtin->imsize = rtin->nxx * rtin->nyy * n1 ;
   else
      rtin->imsize = nvox * n1 ;

   for( cc=0 ; cc < rtin->num_chan ; cc++ ){
     rtin->im[cc]   = rtin->sbr[cc] ;  /* place to put first image in sub-brick */
     rtin->nsl[cc]  = 0 ;              /* number of slices gathered so far */
     rtin->nvol[cc] = 0 ;              /* number of volumes gathered so far */

     RT_written[cc] = 0 ;              /* for real time data writing to disk */
   }
   RT_written_mrg = 0 ;

   /** if there is already data stored in the temporary buffer
       (acquired before this routine was called), then we want
       to place it into the dataset now.                       **/

   if( rtin->bufar != NULL ){
      int ii , upsave ;
      MRI_IMAGE * ibb ;
      char * bbb ;

      if( verbose > 1 )
         fprintf(stderr,"RT: putting %d buffered images into dataset\n" ,
                        IMARR_COUNT(rtin->bufar) ) ;
      VMCHECK ;

      upsave = update ;  /* don't do updates to AFNI during this step */
      update = 0 ;

      for( ii=0 ; ii < IMARR_COUNT(rtin->bufar) ; ii++ ){
        ibb = IMARR_SUBIMAGE(rtin->bufar,ii) ;                    /* next buffered image */
        bbb = (char *) MRI_BYTE_PTR( ibb ) ;                      /* pointer to its data */
        memcpy( rtin->im[rtin->cur_chan] , bbb , rtin->imsize ) ; /* copy into sub-brick */
        mri_free( ibb ) ;                                         /* free buffered image */
        RT_process_image( rtin ) ;                                /* send into dataset   */
      }
      FREE_IMARR( rtin->bufar ) ;   /* throw this away like a used Kleenex */

      update = upsave ;

      if( verbose > 1 )
         fprintf(stderr,"RT: buffered images all placed into dataset\n") ;
      VMCHECK ;
   }

   /** dataset(s) now created and ready to boogie! **/
   /** popup a message to keep the user occupied.  **/

   { char str[1280] ;
     char acq[128] , stiming[64]="" , *sli ;

     switch( rtin->dtype ){
       case DTYPE_2DZ:  strcpy(acq,"2D+z (1 volume, by slice")        ; break ;
       case DTYPE_2DZT: strcpy(acq,"2D+zt (multivolume, by slice")    ; break ;
       case DTYPE_3D:   strcpy(acq,"3D (1 volume, all at once)")      ; break ;
       case DTYPE_3DT:  strcpy(acq,"3D+t (multivolume, by 3D array)") ; break ;
       case DTYPE_3DTM: strcpy(acq,"3D+t (multivol, by 3D w/timing)") ; break ;
       default:         strcpy(acq,"Bizarro world")                   ; break ;
     }

     if( rtin->dtype == DTYPE_2DZ || rtin->dtype == DTYPE_2DZT ||
                                     rtin->dtype == DTYPE_3DTM ){
       switch( rtin->zorder ){
         case ZORDER_ALT: strcat(acq," - interleaved order)") ; break ;
         case ZORDER_SEQ: strcat(acq," - sequential order)")  ; break ;
         case ZORDER_EXP: strcat(acq," - explicit order)")    ; break ;
       }
     }

     switch( rtin->orczz ){
       case ORI_I2S_TYPE:
       case ORI_S2I_TYPE: sli = "(Axial)"    ; break ;

       case ORI_R2L_TYPE:
       case ORI_L2R_TYPE: sli = "(Sagittal)" ; break ;

       case ORI_P2A_TYPE:
       case ORI_A2P_TYPE: sli = "(Coronal)"  ; break ;

       default:           sli = "\0"         ; break ;  /* say what? */
     }

     if( rtin->tpattern == ZORDER_EXP && rtin->nstimes == rtin->nzz )
        strcpy(stiming, "                   (explicit slice timing)\n");
     else *stiming = '\0';

     sprintf(str," \n"
                 " ** Realtime Header Information **\n"
                 "\n"
                 " Dataset prefix  : %s\n"
                 " Brick Dimensions: %d x %d x %d\n"
                 " Voxel Grid Size : %.4f x %.4f x %.4f (mm)\n"
                 " Grid Orientation: %s x %s x %s %s\n"
                 " Grid Offset     : %.1f x %.1f x %.1f (mm)\n"
                 " Datum Type      : %s\n"
                 " Number Channels : %d\n"
                 " Acquisition Type: %s\n%s" ,
             npr ,
             rtin->nxx , rtin->nyy , rtin->nzz ,
             fabs(rtin->dxx) , fabs(rtin->dyy) , fabs(rtin->dzz) ,
             ORIENT_shortstr[rtin->orcxx],ORIENT_shortstr[rtin->orcyy],
               ORIENT_shortstr[rtin->orczz] , sli ,
             rtin->xxorg , rtin->yyorg , rtin->zzorg ,
             MRI_TYPE_name[rtin->datum] ,
             rtin->num_chan ,
             acq , stiming
          ) ;

     if (doPopups) PLUTO_popup_transient(plint,str);
   }

   /*-- 01 Jun 2009: setup the global realtime status structure --*/

   { RT_status *rts = GLOBAL_library.realtime_status ;
     if( rts != NULL ){
       if( rts->dset != NULL ){ free(rts->dset) ; rts->dset = NULL ; }
     } else {
       rts = (RT_status *)calloc(1,sizeof(RT_status)) ;
       GLOBAL_library.realtime_status = rts ;
     }
     rts->numchan = rtin->num_chan ;
     rts->status  = RT_STARTUP ;
     rts->numdset = rtin->num_chan ;
     if( rtin->mrg_dset != NULL ) rts->numdset++ ;
     if( rtin->reg_dset != NULL ) rts->numdset++ ;
     /* CC add in detrend dataset */
     if( rtin->detrend_dset != NULL ) rts->numdset++ ;
     /* if we have reg_chan_dset datasets, add them   27 May 2010 [rickr] */
     if( rtin->reg_chan_dset[0] != NULL ) rts->numdset += rtin->num_chan ;

     rts->dset = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)*rts->numdset) ;
     for( cc=0 ; cc < rtin->num_chan ; cc++ ) rts->dset[cc] = rtin->dset[cc] ;
     if( rtin->mrg_dset != NULL ) rts->dset[cc++] = rtin->mrg_dset ;
     if( rtin->reg_dset != NULL ) rts->dset[cc++] = rtin->reg_dset ;
     /* CC add in detrend dataset */
     if( rtin->detrend_dset != NULL ) rts->dset[cc++] = rtin->detrend_dset ;
     if( rtin->reg_chan_dset[0] != NULL )          /* 27 May 2010 [rickr] */
        for( ii=0 ; ii < rtin->num_chan ; ii++ )
           rts->dset[cc++] = rtin->reg_chan_dset[ii] ;

#if 0
     GLOBAL_library.realtime_callback = RT_test_callback ;  /* for testing */
#endif
   }

   return ;
}

/* return whether this is true, whether we will register the
 * merged dataset                        27 Oct 2016 [rickr] */
static int RT_will_register_merged_dset(RT_input * rtin)
{
   /* if( rtin->reg_chan_mode > RT_CM_RMODE_NONE ) */
   if ( RT_when_to_merge() == RT_CM_MERGE_BEFORE_REG )
      return 1;

   return 0;
}

/* return whether to merge before registration, after, or not at all
 *                                               27 Oct 2016 [rickr] */
static int RT_when_to_merge( void )
{
   if( RT_chmrg_mode == RT_CHMER_NONE )     return RT_CM_NO_MERGE;
   if( RT_chmrg_mode == RT_CHMER_OPT_COMB ) return RT_CM_MERGE_AFTER_REG;

   /* otherwise, merge before registration */
   return RT_CM_MERGE_BEFORE_REG;
}

/*--------------------------------------------------------------------
  Read one image (or volume) into the space pointed to by im.
----------------------------------------------------------------------*/

#define COMMAND_MARKER        "Et Earello Endorenna utulien!!"
#define COMMAND_MARKER_LENGTH 30

void RT_read_image( RT_input * rtin , char * im )
{
   int need , have , nbuffed ;

   /** sanity checks **/

   if( rtin == NULL || im == NULL ){
     fprintf(stderr,"RT: illegal inputs to RT_read_image!\a\n") ;
     EXIT(1) ;
   }

   if( rtin->imsize <= 0 ){
     fprintf(stderr,"RT: image data present, but don't know its size!\a\n") ;
     EXIT(1) ;
   }

   /** see if any data in buffer already **/

   have = rtin->nbuf ;

   /** if we have data already, use it first **/

   if( have > 0 ){

      nbuffed = MIN( have , rtin->imsize ) ;  /* how much to read from buffer */
      memcpy( im , rtin->buf , nbuffed ) ;    /* copy it into image */

      if( nbuffed < have ){                   /* didn't use up all of buffer */
         memmove( rtin->buf , rtin->buf + nbuffed , rtin->nbuf - nbuffed ) ;
         rtin->nbuf = rtin->nbuf - nbuffed ;
      } else {                                /* used up all of buffer */
         rtin->nbuf = 0 ;
      }
   } else {
      nbuffed = 0 ;
   }

   /** at this point, nbuffed is the number of bytes read from the buffer.
       if we need to read more bytes from the I/O channel, then do so now. **/

   need = rtin->imsize - nbuffed ;  /* number of bytes left to fill image */

   /* read this many bytes for sure (waiting if need be) */

   if( need > 0 )
      iochan_recvall( rtin->ioc_data , im + nbuffed , need ) ;

   /* 10 Dec 2002:
      Check if the command string is present at the start of im.
      If it is, then mark rtin for death.                       */

   if( memcmp(im,COMMAND_MARKER,COMMAND_MARKER_LENGTH) == 0 )
     rtin->marked_for_death = 1 ;
   else {
      /* we have a complete image, check for byte swapping */
      if ( rtin->swap_on_read != 0 ) {
         if( rtin->datum == MRI_short )
            mri_swap2( rtin->imsize / 2, (short *)im );
         else
            mri_swap4( rtin->imsize / 4, (int *)im );
      }
   }

   return ;
}

/*--------------------------------------------------------------------
   Read images and put them into place.  Note that if there is any
   data left in the rtin buffer, it will go into the image first.
----------------------------------------------------------------------*/

int RT_process_data( RT_input * rtin )
{
   /** can we create a dataset yet? **/

   if( rtin->sbr[0] == NULL && rtin->info_ok ){
      if( verbose > 1 )
         fprintf(stderr,"RT: info complete --> creating dataset.\n") ;
      VMCHECK ;
      RT_start_dataset( rtin ) ;
   }

   /** read images as long as there is data to suck up **/

   while( rtin->nbuf > 0 || iochan_readcheck(rtin->ioc_data,0) > 0 ){

      if( rtin->im[0] != NULL ){  /** process data into dataset directly **/

         RT_read_image( rtin , rtin->im[rtin->cur_chan] ) ; /* read into dataset buffer */

         if( rtin->marked_for_death ) return 0 ;            /* 10 Dec 2002 */

         RT_process_image( rtin ) ;                         /* process it for the dataset */

      } else {                 /** read data into temporary buffer space **/
                               /** (will have to process it later, dude) **/
         MRI_IMAGE * newim ;
         char * newbuf ;

         if( rtin->imsize <= 0 ){
           fprintf(stderr,"RT: image data present, but don't know its size!\a\n") ;
           EXIT(1) ;
         }

         if( rtin->bufar == NULL )    /* initialize buffer for input images */
           INIT_IMARR(rtin->bufar) ;

         if( verbose > 1 && rtin->bufar->num % 10 == 0 ){
           fprintf(stderr,"RT: reading image into buffer[%d]\n",rtin->bufar->num) ;
           VMCHECK ;
         }

         newim  = mri_new( rtin->imsize , 1 , MRI_byte ) ; /* make space for next image */
         newbuf = (char *) MRI_BYTE_PTR(newim) ;           /* pointer to image data */
         ADDTO_IMARR( rtin->bufar , newim ) ;              /* add to saved image list */
         RT_read_image( rtin , newbuf ) ;                  /* read image data */
         if( rtin->marked_for_death ) return 0 ;           /* 10 Dec 2002 */
      }

      RT_process_xevents( rtinp ) ;

   }  /* end of loop over reading images as long as we can */

   /** return an error code if the input data channel has gone bad **/

/**
   if( iochan_goodcheck(rtin->ioc_data,0) <= 0 ) return -1 ;
**/

   return 1 ;
}

/*--------------------------------------------------------------------
   Function to be called if the user kills the Image Only viewer
----------------------------------------------------------------------*/

static void RT_image_kfun(void * kdata)                /* 28 Apr 2000 */
{
   RT_input * rtin = (RT_input *) kdata ;
   if( rtin != NULL ) rtin->image_handle = NULL ;
   return ;
}

/*-------------------------------------------------------------------
   Given image data stored in rtin->im, process it into the dataset
   that is under construction.  This routine should only be called
   after RT_start_dataset has been executed.
---------------------------------------------------------------------*/

#define MIN_TO_GRAPH 2

void RT_process_image( RT_input * rtin )
{
   int vdone=0 , cc = rtin->cur_chan ;

   /** 28 Apr 2000: Image Only mode stuff **/

   if( rtin->image_mode ){
      if( rtin->image_handle != NULL ){
         PLUTO_imseq_addto( rtin->image_handle , rtin->image_space ) ;
      } else {
         rtin->image_handle = PLUTO_imseq_popim( rtin->image_space,
                                (generic_func *)RT_image_kfun,rtin ) ;
         PLUTO_imseq_retitle( rtin->image_handle , "Realtime Images" ) ;
      }
      return ;
   }

   /** check if new image completes the current volume **/

   if( rtin->dtype == DTYPE_2DZT || rtin->dtype == DTYPE_2DZ ){

      if( verbose > 1 )
         fprintf(stderr,"RT: read image into dataset brick %d slice %d.\n",
                 rtin->nvol[cc],rtin->nsl[cc]) ;

      rtin->nsl[cc] ++ ;                     /* 1 more slice */
      vdone = (rtin->nsl[cc] == rtin->nzz) ; /* have all slices? */

   } else if( rtin->dtype == DTYPE_3DT || rtin->dtype == DTYPE_3D ||
              rtin->dtype == DTYPE_3DTM ){

#if 0
      if( verbose > 1 )
        fprintf(stderr,"RT: read image into dataset brick %d\n",rtin->nvol[cc]) ;
#endif

      vdone = 1 ;                        /* 3D gets all data at once */
   }

   /** if volume is done, add it to the dataset **/

   if( vdone ){

      rtin->nvol[cc] ++ ;        /* 1 more volume is acquired! */

      /* Cameron Craddock added this to ease profiling */
      if( g_show_times && verbose > 1 )
      {
          char vmesg[64];
          sprintf(vmesg,"begin processing vol %d",rtin->nvol[0]-1);
          RT_show_duration(vmesg);
      }

      if( verbose > 1 )
        fprintf(stderr,"RT: now have %d complete sub-bricks in channel %02d.\n",
                rtin->nvol[cc],cc+1) ;
      VMCHECK ;

      /* first time: put this volume in as "substitute" for empty 1st brick
         later:      add new volume at end of chain                         */

      if( rtin->nvol[cc] == 1 )
         EDIT_substitute_brick( rtin->dset[cc], 0, rtin->datum, rtin->sbr[cc]);
      else
         EDIT_add_brick( rtin->dset[cc] , rtin->datum , 0.0 , rtin->sbr[cc] ) ;

      VMCHECK ;
      if( verbose > 1 )
         fprintf(stderr,"RT: added brick to dataset in channel %02d\n",cc+1) ;
      VMCHECK ;

      /* must also change the number of times recorded
         [EDIT_add_brick does 'nvals' correctly, but not 'ntt'] */

      if( rtin->dtype == DTYPE_3DT || rtin->dtype == DTYPE_2DZT ||
          rtin->dtype == DTYPE_3DTM ){
         EDIT_dset_items( rtin->dset[cc], ADN_ntt, rtin->nvol[cc], ADN_none ) ;
         if( verbose > 1 )
           fprintf(stderr,"RT: altered ntt in dataset header in channel %02d\n",
                   cc+1) ;
         VMCHECK ;
      } else if( rtin->nvol[cc] > 1 ){
         fprintf(stderr,"RT: have %d bricks for time-independent dataset!\a\n",
                 rtin->nvol[cc]) ;
         VMCHECK ;
      }

      /** 02 Jun 2009: merger operations?
                       if have completed a full set of channels, that is **/

      /* rcr OC - check for pre-reg merge (after last channel is acquired) */
      if( cc+1 == rtin->num_chan && 
          RT_when_to_merge() == RT_CM_MERGE_BEFORE_REG ) {
        if( verbose > 1 )
           fprintf(stderr,"++ about to pre-reg merge\n");
        RT_merge( rtin, cc, -1);
      }

      /* do registration before function computation   - 30 Oct 2003 [rickr] */
      switch( rtin->reg_mode ){
           case REGMODE_2D_RTIME: RT_registration_2D_realtime( rtin ) ;
           break ;

           case REGMODE_3D_RTIME:
           case REGMODE_3D_ESTIM: RT_registration_3D_realtime( rtin ) ;
           break ;
      }

      /* Cameron Craddock added this to ease profiling */
      if( g_show_times && verbose > 1 ) {
          char vmesg[64];
          sprintf(vmesg,"volume %d through motion correction",rtin->nvol[0]-1);
          RT_show_duration(vmesg);
      }

      /* rcr OC - check for post-reg merge              1 Nov 2016 [rickr] */
      if( cc+1 == rtin->num_chan && 
          RT_when_to_merge() == RT_CM_MERGE_AFTER_REG ) {

         int tt, ntt=DSET_NUM_TIMES( rtin->dset[g_reg_src_chan] ) ;

         for( tt = rtin->mrg_nvol; tt < ntt && tt < rtin->reg_nvol; tt++ ) {
            if( verbose > 1 )
               fprintf(stderr,"++ about to post-reg merge time index %d\n", tt);
            RT_merge( rtin, cc, tt);
         }
      }

      /* possibly send mot params and ROI aves   2 Mar 2017 [rickr] */
      if ( rtin->mp_tcp_use > 0 && rtin->reg_mode == REGMODE_3D_RTIME )
      {
         int tfirst = rtin->mp_npsets, ttop;
         if ( RT_when_to_merge() == RT_CM_MERGE_AFTER_REG )
            ttop = rtin->mrg_nvol;
         else
            ttop = rtin->reg_nvol;

         RT_mp_comm_send_data_wrapper( rtin, tfirst, ttop );
      }

      /* Cameron Craddock
         If a mask is specified, but for some reason the communication
         fails, or is not initialized, or if real time graphing is turned
         off then go ahead and calculate the mask averages. */

      /*  mask_init == 0 means that the mask isn't initialized */
      if(( rtin->mp_tcp_use != 1 ) && ( rtin->mask_init == 0 ))
      {
          RT_mp_check_env_for_mask(); /* possibly set mask based on env var */

          /* now set up the mask data, if g_mask_dset is set */
          if( g_mask_dset ) RT_mp_init_mask( rtin );
          else              rtin->mask_init = -1;
      }

      /* MP communication is turned off, but the mask is initialized 
         calculate mask averages */
      if(( rtin->mp_tcp_use != 1 ) && ( rtin->mask_init == 1 ))
      {
          if( RT_mp_get_mask_aves(rtin, rtin->reg_nvol - 1) )
          {
              /* something bad happened, free memory
                 and disable future attempts */
              fprintf(stderr, "RT: Error calculating mask averages,"
                  " I won't try that again!\n" );
              rtin->mask_init = -1;
          }
      }

               
      /* Cameron Craddock - detrend data */
      if ( rtin->detrend_mode > RT_DETREND_NONE )
      {
          RT_detrend( rtin, TELL_NORMAL );
          if( g_show_times && verbose > 1 ) 
          {
              char vmesg[64];
              sprintf(vmesg,"volume %d through detrend",rtin->nvol[0]-1);
              RT_show_duration(vmesg);
          }
      }

      /* Seems like a good place to write individual volumes to disk in *
       * near real time - vinai.                                        */

      if ( RTdatamode ) /* if not zero, and writing data in real time   */
      {
         THD_3dim_dataset * tmp_dset = NULL ;
         char               RT_prefix[THD_MAX_PREFIX] ;
         int                cc, RT_sub_brick_id[2];

         RT_sub_brick_id[0] = 1;

         if ( RTdatamode == RT_WRITE_ACQUIRED ) /* write acquired data */
         {
            for( cc=0 ; cc < rtinp->num_chan ; cc++ )
            {
               for ( ; RT_written[cc] < rtin->dset[cc]->dblk->nvals ; )
               {
                  RT_sub_brick_id[1] = RT_written[cc];

                  /* copy acquired data sets for writing now */
                  tmp_dset = THD_copy_dset_subs(rtin->dset[cc],RT_sub_brick_id);

                  /* add channel and rep index to the name of the written *
                   * RT data sets                                         */
                  sprintf ( RT_prefix, "%s_ch%03d_nr_%06d",
                            rtin->dset[cc]->dblk->diskptr->prefix,
                            cc, RT_sub_brick_id[1] ) ;
                  EDIT_dset_items(tmp_dset, ADN_prefix, RT_prefix, ADN_none);
                  /* and set the mode based on what was passed */
                  if( rtin->storage_mode )
                     tmp_dset->dblk->diskptr->storage_mode = rtin->storage_mode;


                  if( verbose > 0 )
                     fprintf(stderr,"RT: writing acquired volume %s\n",
                             RT_prefix);

                  DSET_overwrite ( tmp_dset ) ; /* write...        */
                  DSET_delete( tmp_dset ) ;     /* and delete copy */
                  tmp_dset = NULL ;

                  RT_written[cc] += 1;
               }
            }
         }

         else if ( RTdatamode == RT_WRITE_REGISTERED )   /* write registered data */
         {
            /* check to make sure we are past the base registration brik */
            if ( rtin->reg_dset->dblk->nvals >= rtin->reg_base_index )
            {
               for ( ; RT_written[0] < rtin->reg_dset->dblk->nvals ; )
               {
                  RT_sub_brick_id[1] = RT_written[0];

                  tmp_dset = THD_copy_dset_subs(rtin->reg_dset,RT_sub_brick_id);

                  /* again - modify name for RT sets as needed */
                  sprintf ( RT_prefix, "%s_nr_%06d",
                            rtin->reg_dset->dblk->diskptr->prefix,
                            RT_written[0] ) ;
                  EDIT_dset_items(tmp_dset, ADN_prefix, RT_prefix, ADN_none);
                  /* and set the mode based on what was passed */
                  if( rtin->storage_mode )
                     tmp_dset->dblk->diskptr->storage_mode = rtin->storage_mode;

                  if( verbose > 0 )
                     fprintf(stderr,"RT: writing registered volume %s\n",
                             RT_prefix);

                  DSET_overwrite ( tmp_dset ) ; /* write...        */
                  DSET_delete( tmp_dset ) ;     /* and delete copy */
                  tmp_dset = NULL ;

                  RT_written[0] += 1;
               }
            }
         }

         /* 10 Jul 2009: write merged dataset brick -- RWCox */

         else if( RTdatamode == RT_WRITE_MERGED && rtin->mrg_dset != NULL ){

            for ( ; RT_written_mrg < rtin->mrg_dset->dblk->nvals ; )
               {
                  RT_sub_brick_id[1] = RT_written_mrg ;

                  /* copy acquired data sets for writing now */
                  tmp_dset = THD_copy_dset_subs(rtin->mrg_dset,RT_sub_brick_id);

                  /* add channel and rep index to the name of the written *
                   * RT data sets                                         */
                  sprintf ( RT_prefix, "%s_mrg_nr_%06d",
                            rtin->mrg_dset->dblk->diskptr->prefix,
                            RT_sub_brick_id[1] ) ;
                  EDIT_dset_items(tmp_dset, ADN_prefix, RT_prefix, ADN_none);
                  /* and set the mode based on what was passed */
                  if( rtin->storage_mode )
                     tmp_dset->dblk->diskptr->storage_mode = rtin->storage_mode;

                  if( verbose > 0 )
                     fprintf(stderr,"RT: writing merged volume %s\n",RT_prefix);

                  DSET_overwrite ( tmp_dset ) ; /* write...        */
                  DSET_delete( tmp_dset ) ;     /* and delete copy */
                  tmp_dset = NULL ;

                  RT_written_mrg += 1;
               }
            }

            /* RCR: do we want to consider writing registered datasets,
               merged and individual channels? */
      }

      /* Cameron Craddock added this to ease profiling */
      if( g_show_times && verbose > 1 ) 
      {
          char vmesg[64];
          sprintf(vmesg,"volume %d through real time writing",rtin->nvol[0]-1);
          RT_show_duration(vmesg);
      }

      /** compute function, maybe? **/

      if( rtin->func_code > 0 ){
         int jj=0 ;

         /** if needed, initialize the function computations **/

         if( rtin->func_condit == 0 ){
#if 0
            jj = rtin->func_func( rtin , INIT_MODE ) ;
#else
            AFNI_CALL_VALU_2ARG( rtin->func_func , int,jj ,
                                 RT_input *,rtin , int,INIT_MODE ) ;
#endif
            if( jj < 0 ){ rtin->func_code = 0 ; rtin->func_func = NULL ; }
            rtin->func_condit = 1 ;  /* initialized */
         }

         /** do the function computations for this volume **/

         if( rtin->func_code > 0 )
#if 0
            jj = rtin->func_func( rtin , rtin->nvol[cc] - 1 ) ;
#else
            AFNI_CALL_VALU_2ARG( rtin->func_func , int,jj ,
                                 RT_input *,rtin , int,rtin->nvol[cc]-1 ) ;
#endif
         /* Cameron Craddock added this to ease profiling */
         if( g_show_times && verbose > 1 ) 
         {
            char vmesg[64];
            sprintf(vmesg,"volume %d through compute function",rtin->nvol[0]-1);
            RT_show_duration(vmesg);
         }
      }

      /** make space for next sub-brick to arrive **/

      if( verbose > 1 )
         fprintf(stderr,"RT: malloc-ing %d bytes for next volume in channel %02d\n",
                 rtin->sbr_size,cc+1) ;
      VMCHECK ;

      rtin->sbr[cc] = malloc( rtin->sbr_size ) ;
      if( rtin->sbr[cc] == NULL ){
        fprintf(stderr,"RT: can't malloc real-time brick %d for channel %02d\a\n",
                rtin->nvol[cc]+1,cc+1) ;
        EXIT(1) ;
      }
      if( verbose > 1 )
         fprintf(stderr,"RT: malloc succeeded\n") ;
      VMCHECK ;

      rtin->im[cc]  = rtin->sbr[cc] ;  /* location of slice #0 within sub-brick */
      rtin->nsl[cc] = 0 ;              /* number of slices gathered so far */

      /** tell AFNI about this dataset, maybe **/

      if( update > 0 ){  /* if we want updates every so often */
         int doit ;

         if( verbose > 1 )
            fprintf(stderr,"RT: checking for update status\n") ;
         VMCHECK ;

         doit = ( (rtin->dtype==DTYPE_3DT || rtin->dtype==DTYPE_2DZT
                                          || rtin->dtype==DTYPE_3DTM) &&
                  (cc+1 == rtin->num_chan)                            && /* 01 Aug 2002 */
                  (rtin->nvol[cc] == MIN_TO_GRAPH ||
                   (rtin->nvol[cc] > MIN_TO_GRAPH && rtin->nvol[cc] % update == 0)) ) ;

         if( doit ){
            if( verbose > 1 )
               fprintf(stderr,"RT: about to tell AFNI about dataset.\n") ;
            VMCHECK ;
            RT_tell_afni(rtin,TELL_NORMAL) ;
         }
      }

   } else {  /** need to add more slices before volume is done **/

      int noff=0 ;  /* slice position in output brick */

      if( rtin->zorder == ZORDER_SEQ ){  /* sequential:       */
                                         /* slice position is */
         noff = rtin->nsl[cc] ;          /* just slice number */

      } else if ( rtin->zorder == ZORDER_ALT ){  /* alternating: */

         int nhalf = (rtin->nzz + 1)/2 ;         /* number in first 1/2 */

         if( rtin->nsl[cc] < nhalf )
            noff = 2 * rtin->nsl[cc] ;               /* first half of slices */
         else
            noff = 1 + 2 * (rtin->nsl[cc] - nhalf) ; /* second half of slices */
      }

      rtin->im[cc] = rtin->sbr[cc] + (noff * rtin->imsize) ; /* where next slice goes */

   }

   /* 01 Aug 2002: update which channel gets the next image */

   if( rtin->num_chan > 1 )
     rtin->cur_chan = (cc+1) % rtin->num_chan ;

   /* Cameron Craddock added this to ease profiling */
   if( g_show_times && verbose > 1 ) 
   {
       char vmesg[64];
       sprintf(vmesg,"volume %d processing complete",rtin->nvol[0]-1);
       RT_show_duration(vmesg);
   }
   return ;
}

static int RT_merge( RT_input * rtin, int chan, int tt )
{
   MRI_IMAGE * mrgim ;
   int         iv;

   /* if not merging, we are out of here */
   if( chan+1 != rtin->num_chan || RT_chmrg_mode == 0 ) return 0;

   if ( tt >= 0 ) iv = tt;
   else           iv = rtin->nvol[chan]-1 ;  /* sub-brick index */

   if ( verbose > 1 )
      fprintf(stderr,"-- RTMerge, merging chan=%d, tindex=%d, num_chan=%d,"
                     " mode=%d\n", chan, iv, rtin->num_chan, RT_chmrg_mode);

   /* 10 Jul 2010 [rickr]: maybe merge only a subset of channels */
   /* note: the channel int list can only be created "now", since
            we must know how many channels there are to use */
   /* if string is length zero, skip   13 Jan, 2017 [rickr] */
   if(rtin->chan_list_str && strlen(rtin->chan_list_str) && ! rtin->chan_list){
     rtin->chan_list = MCW_get_labels_intlist(NULL, rtin->num_chan,
                                              rtin->chan_list_str);
     if( !rtin->chan_list ) {
        fprintf(stderr,"** failed to make channel list (%d) from '%s'\n",
                rtin->num_chan, rtin->chan_list_str);
        free(rtin->chan_list_str);  rtin->chan_list_str = NULL;
     } else if( verbose )
        fprintf(stderr,"RTM: using list of %d chans for merge from %s\n",
                rtin->chan_list[0], rtin->chan_list_str);
   }
  
   mrgim = RT_mergerize(rtin, iv) ;
   if( mrgim == NULL ){
     ERROR_message("RT can't merge channels at time index #%d",iv) ;
   } else {
     if( iv == 0 )
       EDIT_substitute_brick( rtin->mrg_dset , 0 , (int)mrgim->kind ,
                                              mri_data_pointer(mrgim) ) ;
     else
       EDIT_add_brick( rtin->mrg_dset , (int)mrgim->kind , 0.0 ,
                                              mri_data_pointer(mrgim) ) ;

     mri_clear_data_pointer(mrgim); mri_free(mrgim);
     if( verbose > 1 )
       fprintf(stderr,"RT: added brick #%d to merged dataset\n",iv) ;
     EDIT_dset_items( rtin->mrg_dset , ADN_ntt , iv+1 , ADN_none ) ;
     rtin->mrg_nvol = iv+1 ;
   }
   VMCHECK ;
  
   /* Cameron Craddock added this to ease profiling */
   if( g_show_times && verbose > 1 ) {
     char vmesg[64];
     sprintf(vmesg,"volume %d through mergering",rtin->nvol[0]-1);
     RT_show_duration(vmesg);
   }

   return 1;
}

/*-------------------------------------------------------------------
   Tell AFNI about all datasets we are building.
---------------------------------------------------------------------*/

void RT_tell_afni( RT_input *rtin , int mode )
{
   int cc ;

   if( rtin == NULL ) return ;

   /*** tell separately for each one ***/

   for( cc=0 ; cc < rtin->num_chan ; cc++ )
     RT_tell_afni_one( rtin , mode , cc ) ;

   /*** at the end of acquisition, show messages ***/

   if( mode == TELL_FINAL && ISVALID_DSET(rtin->dset[0]) ){
     char qbuf[256*(MAX_CHAN+1)] , zbuf[256] ; int qq=0 ;
     sprintf( qbuf ,
                " \n"
                " Acquisition Terminated\n\n"
                " Brick Dimensions: %d x %d x %d  Datum: %s\n\n" ,
               rtin->nxx,rtin->nyy,rtin->nzz , MRI_TYPE_name[rtin->datum] );

     for( cc=0 ; cc < rtin->num_chan ; cc++ ){
       if( ISVALID_DSET(rtin->dset[cc]) )
         sprintf( zbuf ,
                  " Channel %02d: dataset %s has %d sub-bricks\n",
                  cc+1 , DSET_FILECODE(rtin->dset[cc]) , DSET_NVALS(rtin->dset[cc]) ) ;
       else
         sprintf( zbuf ,
                  " Channel %d: INVALID DATASET?!\n",cc) ;
       strcat( qbuf , zbuf ) ;
     }
     strcat(qbuf,"\n") ;
     if( ISVALID_DSET(rtin->mrg_dset) ){
       sprintf( zbuf ,
                " Merger%4.4s: dataset %s has %d sub-bricks\n" ,
                RT_chmrg_labels[RT_chmrg_mode] ,
                DSET_FILECODE(rtin->mrg_dset) , DSET_NVALS(rtin->mrg_dset) ) ;
       strcat( qbuf , zbuf ) ; qq++ ;
     }
     if( ISVALID_DSET(rtin->reg_dset) ){
       sprintf( zbuf ,
                " Registered : dataset %s has %d sub-bricks\n" ,
                DSET_FILECODE(rtin->reg_dset) , DSET_NVALS(rtin->reg_dset) ) ;
       strcat( qbuf , zbuf ) ; qq++ ;
     }
     /* Cameron Craddock add code for detrending */
     if( ISVALID_DSET(rtin->detrend_dset) ){
       sprintf( zbuf ,
           " Detrended : dataset %s has %d sub-bricks\n" ,
           DSET_FILECODE(rtin->detrend_dset), DSET_NVALS(rtin->detrend_dset) );
       strcat( qbuf , zbuf ) ; qq++ ;
     }
     /* CC end */
     if( rtin->reg_chan_mode >= RT_CM_RMODE_REG_CHAN && 
         ISVALID_DSET(rtin->reg_chan_dset[0])) {
       int nvals = DSET_NVALS(rtin->reg_chan_dset[0]);
       for( cc=1 ; cc < rtin->num_chan ; cc++ ){
         if( ! ISVALID_DSET(rtin->reg_chan_dset[cc]) ||
              DSET_NVALS(rtin->reg_chan_dset[cc]) != nvals ) {
           sprintf( zbuf , " Reg Channel %d: INVALID DATASET?!\n",cc) ;
           break;
         }
       }
       if( cc == rtin->num_chan )
          sprintf( zbuf , " Reg Channels: %d dsets have %d sub-bricks\n",
                   rtin->num_chan, nvals );
       strcat( qbuf , zbuf ) ; qq++ ;
     }
     if( qq ) strcat(qbuf,"\n") ;

     if (doPopups) { /* 12 Aug 2008 [jwhite] */ 
       PLUTO_beep(); 
       PLUTO_popup_transient(plint,qbuf);
     }

     if( verbose ) fputs(qbuf, stderr);
     if( verbose > 1 ) SHOW_TIMES ;

     sync() ;  /* 08 Mar 2000: sync disk */
   }

   /* execute any pending DRIVE_WAIT commands  (moved) 3 Sep 2008 [rickr] */
   if( drive_wait_list.len > 0 )
      rt_run_drive_wait_commands( &drive_wait_list );

   /* invoke the external callback function, if it exists [01 Jun 2009] */

   if( GLOBAL_library.realtime_callback != NULL ){
     RT_status *rts = GLOBAL_library.realtime_status ;
     if( mode == TELL_FINAL ) rts->status = RT_FINISHED ;
     /* Cameron Craddock added this to ease profiling */
     if( g_show_times && verbose > 1 ) 
     {
         char vmesg[64];
         sprintf(vmesg,"volume %d starting callback",rtin->nvol[0]-1);
         RT_show_duration(vmesg);
     }
     AFNI_CALL_VOID_1ARG( GLOBAL_library.realtime_callback , void* , NULL ) ;
     if( mode != TELL_FINAL ) rts->status = RT_CONTINUE ;

     /* Cameron Craddock added this to ease profiling */
     if( g_show_times && verbose > 1 ) 
     {
         char vmesg[64];
         sprintf(vmesg,"volume %d through callback",rtin->nvol[0]-1);
         RT_show_duration(vmesg);
     }
   }

   if( mode == TELL_FINAL ){   /* moved here 01 Jun 2009 */
      if( rtin->func_dset != NULL )
        THD_force_malloc_type( rtin->func_dset->dblk , DATABLOCK_MEM_ANY ) ;

      if( rtin->reg_dset != NULL )
        THD_force_malloc_type( rtin->reg_dset->dblk , DATABLOCK_MEM_ANY ) ;

      /* Cameron modification to support detrended dataset, just copying
         because we do this to every other dataset */
      if( rtin->detrend_dset != NULL )
        THD_force_malloc_type( rtin->detrend_dset->dblk , DATABLOCK_MEM_ANY ) ;
      /* CC end */

      if( rtin->mrg_dset != NULL )
        THD_force_malloc_type( rtin->mrg_dset->dblk , DATABLOCK_MEM_ANY ) ;

      for( cc=0 ; cc < rtin->num_chan ; cc++ ) {  /* 27 May 2010 [rickr] */
          if( !ISVALID_DSET(rtin->reg_chan_dset[cc]) ) break;
          THD_force_malloc_type( rtin->reg_chan_dset[cc]->dblk ,
                                 DATABLOCK_MEM_ANY ) ;
      }

      for( cc=0 ; cc < rtin->num_chan ; cc++ )
        THD_force_malloc_type( rtin->dset[cc]->dblk , DATABLOCK_MEM_ANY ) ;

      AFNI_purge_unused_dsets() ;
   }

   return ;
}

/*-------------------------------------------------------------------
   Tell AFNI about one dataset we are building.
---------------------------------------------------------------------*/

void RT_tell_afni_one( RT_input *rtin , int mode , int cc )
{
   Three_D_View *im3d ;
   THD_session *sess ;
   THD_3dim_dataset *old_anat=NULL ;
   int ii , id , afni_init=0 ;
   char clll=0 , *cstr ;
   MCW_arrowval *tav=NULL ;

   /** sanity check **/

   if( rtin == NULL || !ISVALID_DSET(rtin->dset[cc]) ) return ;

   im3d = rtin->im3d[cc] ;                      /* AFNI controller */
   sess = rtin->sess[cc] ;                      /* AFNI session */

   if( im3d != NULL ){   /* 05 Aug 2002: allow for NULL im3d controller */

     tav  = im3d->vwid->imag->time_index_av ;   /* time index widget */
     ii   = AFNI_controller_index( im3d ) ;
     cstr = AFNI_controller_label( im3d ) ; clll = cstr[1] ;

     old_anat = im3d->anat_now ;  /* the default */
   }

   /**--- deal with the input dataset ---**/

   if( rtin->afni_status[cc] == 0 ){  /** first time for this dataset **/

      EDIT_dset_items( rtin->dset[cc],
                         ADN_directory_name,sess->sessname,
                       ADN_none ) ;

      if( im3d != NULL ){
        if( verbose )
           fprintf(stderr , "RT: sending dataset %s with %d bricks\n"
                            "    to AFNI[%c], session %s\n" ,
                   DSET_FILECODE(rtin->dset[cc]) , rtin->nvol[cc] ,
                   clll , sess->sessname ) ;

      }
      THD_load_statistics( rtin->dset[cc] ) ;

      /** put it into the current session in the current controller **/

      if( GLOBAL_library.have_dummy_dataset ) UNDUMMYIZE ;

      id = sess->num_dsset ;

      if( id >= THD_MAX_SESSION_SIZE ){
        fprintf(stderr,"RT: max number of anat datasets exceeded!\a\n") ;
        EXIT(1) ;
      }

      SET_SESSION_DSET(rtin->dset[cc], sess,id,VIEW_ORIGINAL_TYPE);
      sess->num_dsset = id+1 ;
      POPDOWN_strlist_chooser ;

      if( ISFUNC(rtin->dset[cc]) )
        AFNI_force_adoption( sess , False ) ;

      /** tell AFNI controller to jump to this dataset and session **/

      if( im3d != NULL ){
        if( ISANAT(rtin->dset[cc]) )
           im3d->vinfo->anat_num = sess->num_dsset - 1 ;
        else
           im3d->vinfo->func_num = sess->num_dsset - 1 ;

        im3d->vinfo->sess_num = rtin->sess_num[cc] ;

        im3d->vinfo->time_index = 0 ;
        AV_assign_ival( tav , 0 ) ;
      }

      afni_init             = 1 ; /* below: will initialize AFNI to see this dataset */
      rtin->afni_status[cc] = 1 ; /* mark dataset to show that AFNI knows about it now */

   } else {  /** 2nd or later call for this dataset **/

      THD_update_statistics( rtin->dset[cc] ) ;

      if( im3d != NULL ){
        if( mode != TELL_FINAL && verbose )
          fprintf(stderr,"RT: update with %d bricks in channel %02d to AFNI[%c]\n",
                  rtin->nvol[cc],cc+1,clll) ;

        tav->fmax = tav->imax = im3d->vinfo->time_index = rtin->nvol[cc] - 1  ;
        AV_assign_ival( tav , tav->imax ) ;
      }

   }

   /**--- Deal with the computed function, if any ---**/

   if( rtin->func_dset != NULL ){
      int jj ;

#if 0
      jj = rtin->func_func( rtin , UPDATE_MODE ) ;  /* put data into dataset */
#else
      AFNI_CALL_VALU_2ARG( rtin->func_func , int,jj ,
                           RT_input *,rtin , int,UPDATE_MODE ) ;
#endif

      if( rtin->func_condit > 1 ){             /* data actually inside */

         THD_load_statistics( rtin->func_dset ) ; /* statistickizification */

         if( rtin->func_status == 0 ){  /** first time for this dataset **/

            if( im3d != NULL && verbose )
               fprintf(stderr , "RT: sending dataset %s with %d bricks\n"
                                "    to AFNI controller [%c] session %s\n" ,
                       DSET_FILECODE(rtin->func_dset) , DSET_NVALS(rtin->func_dset) ,
                       clll , sess->sessname ) ;

            EDIT_dset_items( rtin->func_dset, ADN_directory_name,sess->sessname, ADN_none ) ;
            id = sess->num_dsset ;
            if( id >= THD_MAX_SESSION_SIZE ){
              fprintf(stderr,"RT: max number of datasets exceeded!\a\n") ;
              EXIT(1) ;
            }
            SET_SESSION_DSET(rtin->func_dset, sess, id,VIEW_ORIGINAL_TYPE);
            sess->num_dsset++ ;
            AFNI_force_adoption( sess , False ) ;
            POPDOWN_strlist_chooser ;

            if( im3d != NULL ){
              im3d->vinfo->func_num = sess->num_dsset - 1 ;
              AFNI_SETUP_FUNC_ON(im3d) ;
            }

            rtin->func_status = 1 ;   /* AFNI knows now */
            afni_init = 1 ;           /* below: tell AFNI to look at this function */

         } else {  /** 2nd or later call for this dataset **/

            /**--- actually, there's nothing to do here ---**/

         }
      }  /* end of if functional dataset actually has data */
   }  /* end of if functional dataset exists */

   /**--- Deal with the merged dataset, if any ---**/

   if( rtin->mrg_dset != NULL && rtin->mrg_nvol > 0 && cc == 0 ){

      if( rtin->mrg_status == 0 ){  /** first time for this dataset **/

         THD_load_statistics( rtin->mrg_dset ) ;

         if( verbose )
           fprintf(stderr , "RT: sending dataset %s with %d bricks to AFNI\n" ,
                   DSET_FILECODE(rtin->mrg_dset) , DSET_NVALS(rtin->mrg_dset)  ) ;

         EDIT_dset_items( rtin->mrg_dset, ADN_directory_name,sess->sessname, ADN_none ) ;

         id = sess->num_dsset ;
         if( id >= THD_MAX_SESSION_SIZE ){
           fprintf(stderr,"RT: max number of datasets exceeded!\a\n") ;
           EXIT(1) ;
         }
         SET_SESSION_DSET(rtin->mrg_dset, sess, id, VIEW_ORIGINAL_TYPE);
         sess->num_dsset = id+1 ;
         POPDOWN_strlist_chooser ;

         rtin->mrg_status = 1 ;   /* AFNI knows about this dataset now */

      } else {  /** 2nd or later call for this dataset **/

         THD_update_statistics( rtin->mrg_dset ) ;
      }
   }

   /**--- Deal with the registered dataset, if any ---**/

   if( rtin->reg_dset != NULL && rtin->reg_nvol > 0 ){

      if( rtin->reg_status == 0 ){  /** first time for this dataset **/

         THD_load_statistics( rtin->reg_dset ) ;

         if( im3d != NULL ){
           if( verbose )
                 fprintf(stderr , "RT: sending dataset %s with %d bricks\n"
                                  "    to AFNI controller [%c] session %s\n" ,
                         DSET_FILECODE(rtin->reg_dset) , DSET_NVALS(rtin->reg_dset) ,
                         clll , sess->sessname ) ;
         }

         EDIT_dset_items( rtin->reg_dset, ADN_directory_name,sess->sessname, ADN_none ) ;

         id = sess->num_dsset ;
         if( id >= THD_MAX_SESSION_SIZE ){
           fprintf(stderr,"RT: max number of datasets exceeded!\a\n") ;
           EXIT(1) ;
         }
         SET_SESSION_DSET(rtin->reg_dset, sess, id, VIEW_ORIGINAL_TYPE);
         sess->num_dsset = id+1 ;
         POPDOWN_strlist_chooser ;

         rtin->reg_status = 1 ;   /* AFNI knows about this dataset now */

      } else {  /** 2nd or later call for this dataset **/

         THD_update_statistics( rtin->reg_dset ) ;
      }
   }

   /* Cameron Craddock modifications to handle real-time detrend,
      just copied this from the registered dataset code above */
   /**--- Deal with the detrended dataset, if any ---**/

   if( rtin->detrend_dset != NULL && rtin->detrend_nvol > 0 ){

      if( rtin->detrend_status == 0 ){  /** first time for this dataset **/

         THD_load_statistics( rtin->detrend_dset ) ;

         if( im3d != NULL ){
           if( verbose )
                 fprintf(stderr , "RT: sending dataset %s with %d bricks\n"
                                  "    to AFNI controller [%c] session %s\n" ,
                         DSET_FILECODE(rtin->detrend_dset) ,
                         DSET_NVALS(rtin->detrend_dset) ,
                         clll , sess->sessname ) ;
         }

         EDIT_dset_items( rtin->detrend_dset,
                          ADN_directory_name,sess->sessname, ADN_none ) ;

         id = sess->num_dsset ;
         if( id >= THD_MAX_SESSION_SIZE ){
           fprintf(stderr,"RT: max number of datasets exceeded!\a\n") ;
           EXIT(1) ;
         }
         SET_SESSION_DSET(rtin->detrend_dset, sess, id, VIEW_ORIGINAL_TYPE);
         sess->num_dsset = id+1 ;
         POPDOWN_strlist_chooser ;

         rtin->detrend_status = 1 ;   /* AFNI knows about this dataset now */

      } else {  /** 2nd or later call for this dataset **/

         THD_update_statistics( rtin->detrend_dset ) ;
      }
   }
   /* CC end */

   /**--- Deal with the registered channel datasets, if any ---**/

   if( rtin->reg_chan_dset[cc] != NULL && rtin->reg_nvol > 0 ){

      if( rtin->reg_chan_status[cc] == 0 ){ /** first time for this dataset **/

         THD_load_statistics( rtin->reg_chan_dset[cc] ) ;

         if( verbose )
           fprintf(stderr , "RT: sending dataset %s with %d bricks to AFNI\n" ,
                   DSET_FILECODE(rtin->reg_chan_dset[cc]) ,
                   DSET_NVALS(rtin->reg_chan_dset[cc])  ) ;

         EDIT_dset_items( rtin->reg_chan_dset[cc],
                          ADN_directory_name,sess->sessname, ADN_none ) ;

         id = sess->num_dsset ;
         if( id >= THD_MAX_SESSION_SIZE ){
           fprintf(stderr,"RT: max number of datasets exceeded!\a\n") ;
           EXIT(1) ;
         }
         SET_SESSION_DSET(rtin->reg_chan_dset[cc], sess, id, VIEW_ORIGINAL_TYPE);
         sess->num_dsset = id+1 ;
         POPDOWN_strlist_chooser ;

         rtin->reg_chan_status[cc] = 1 ; /* AFNI knows about this dataset now */

      } else {  /** 2nd or later call for this dataset **/

         THD_update_statistics( rtin->reg_chan_dset[cc] ) ;
      }

   }

   /**--- actually talk to AFNI now ---**/

   if( afni_init ){  /* tell AFNI to view new dataset(s) */

     if( im3d != NULL ){
       AFNI_SETUP_VIEW(im3d,VIEW_ORIGINAL_TYPE) ;
       if( EQUIV_DSETS(rtin->dset[cc],old_anat) ) THD_update_statistics( rtin->dset[cc] ) ;
       else                                       THD_load_statistics  ( rtin->dset[cc] ) ;

       AFNI_initialize_view( old_anat , im3d ) ;  /* Geronimo! */
     }

   } else {          /* just tell AFNI to refresh the images/graphs */

      Three_D_View *qq3d ;
      int review ;

      /* check all controllers to see if they are looking at the same datasets */

      for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
         qq3d = GLOBAL_library.controllers[ii] ;
         if( !IM3D_OPEN(qq3d) ) break ;  /* skip this one */

         review = (qq3d == im3d)                             ||   /* same viewer?  */
                  EQUIV_DSETS(rtin->dset[cc],qq3d->anat_now) ||   /* or same anat? */
                  ( rtin->func_dset != NULL   &&                  /* or same func? */
                    qq3d->vinfo->func_visible &&
                    EQUIV_DSETS(rtin->func_dset,qq3d->fim_now) ) ;

         review = review || ( rtin->reg_dset != NULL &&      /* or same reg?  */
                              rtin->reg_nvol > 0     &&
                              EQUIV_DSETS(rtin->reg_dset,qq3d->anat_now) ) ;

         /* Cameron Craddock modified to support realtime detrend, just 
            copied and modified the code above */

         review = review || ( rtin->detrend_dset != NULL && /* or same detr? */
                              rtin->detrend_nvol > 0     &&
                              EQUIV_DSETS(rtin->detrend_dset,qq3d->anat_now) ) ;
         /* CC end */

         review = review || ( rtin->mrg_dset != NULL &&      /* or same mrg?  */
                              rtin->mrg_nvol > 0     &&
                              EQUIV_DSETS(rtin->mrg_dset,qq3d->anat_now) ) ;

         review = review || ( rtin->reg_chan_dset[cc] != NULL &&
                              rtin->reg_nvol > 0              &&
                              EQUIV_DSETS(rtin->reg_chan_dset[cc],
                                          qq3d->anat_now) ) ;

         if( review ){
#if 1      /** new code to enforce time index update: 20 May 2009 */
           DISABLE_LOCK ;
           AFNI_time_index_CB( qq3d->vwid->imag->time_index_av ,
                               (XtPointer)qq3d ) ;
           ENABLE_LOCK ;
#else
           AFNI_modify_viewing( qq3d , False ) ;  /* Crazy Horse! */
#endif
         }
      }
   }

   if( im3d != NULL ) XmUpdateDisplay( THE_TOPSHELL ) ;

   /**--- if this is the final call, do some cleanup stuff ---**/

   if( mode == TELL_FINAL ){

      int cmode ;

      if( verbose > 1 )
         fprintf(stderr,"RT: finalizing dataset to AFNI (including disk output).\n") ;

#if 0
      if( im3d != NULL )
        fprintf(stderr , "RT: sending dataset %s with %d bricks\n"
                         "    to AFNI controller [%c] session %s\n" ,
                DSET_FILECODE(rtin->dset[cc]) , rtin->nvol[cc] ,
                clll , sess->sessname ) ;
#endif

#if 0
      THD_load_statistics( rtin->dset[cc] ) ;
#endif

      /* 20 Mar 1998: write in uncompressed mode for speed */

      cmode = THD_get_write_compression() ;
      THD_set_write_compression(COMPRESS_NONE) ;
      SHOW_AFNI_PAUSE ;

      if( okay_to_add_markers(rtinp->dset[cc]) ) /* 13 Sep 2005 [rickr] */
         rtinp->dset[cc]->markers = create_empty_marker_set() ;

      DSET_overwrite( rtin->dset[cc] ) ;
      DSET_unlock( rtin->dset[cc] ) ;  /* 20 Mar 1998 */

      if( rtin->func_dset != NULL ){
         int jj ;
#if 0
         jj = rtin->func_func( rtin , FINAL_MODE ) ;
#else
         AFNI_CALL_VALU_2ARG( rtin->func_func , int,jj ,
                              RT_input *,rtin , int,FINAL_MODE ) ;
#endif
         DSET_overwrite( rtin->func_dset ) ;
         DSET_unlock( rtin->func_dset ) ;  /* 20 Mar 1998 */
      }

      /* check cc == 0                         2 Jun 2010 [rickr] */
      if( rtin->reg_dset != NULL && rtin->reg_nvol > 0 && cc == 0 ){
         DSET_overwrite( rtin->reg_dset ) ;
         DSET_unlock( rtin->reg_dset ) ;
      }

      /* Cameron Craddock to support real-time detrending, just
         copying everything that you do for reg_dset, i hope 
         that this will write out the dataset at the end of
         the acquisition */
      if( rtin->detrend_dset != NULL && rtin->detrend_nvol > 0 && cc == 0 ){
         /* finalize detrend here */
         RT_detrend( rtin, TELL_FINAL );
         DSET_overwrite( rtin->detrend_dset ) ;
         DSET_unlock( rtin->detrend_dset ) ;
      }
      /* CC end */

      if( rtin->mrg_dset != NULL && rtin->mrg_nvol > 0 && cc == 0 ){
         DSET_overwrite( rtin->mrg_dset ) ;
         DSET_unlock( rtin->mrg_dset ) ;
      }

      if( rtin->reg_chan_dset[cc] != NULL && rtin->reg_nvol > 0 ){
         DSET_overwrite( rtin->reg_chan_dset[cc] ) ;
         DSET_unlock( rtin->reg_chan_dset[cc] ) ;
      }

      THD_set_write_compression(cmode) ;  /* restore compression mode */

      AFNI_force_adoption( sess , GLOBAL_argopt.warp_4D ) ;
      AFNI_make_descendants( GLOBAL_library.sslist ) ;

      SHOW_AFNI_READY ;
      if( verbose > 1 )
         fprintf(stderr,"RT: finished finalizing dataset to AFNI"
                        " (including disk output).\n") ;
   }

   return ;
}

/*--------------------------------------------------------------------------
  This routine is called when no more data will be added to the
  incoming dataset(s).
----------------------------------------------------------------------------*/

void RT_finish_dataset( RT_input * rtin )
{
   int cc , nbad=0 ;

   if( rtin->image_mode ){
      if( verbose > 1 ) SHOW_TIMES ;
      return ;
   }

   for( cc=0 ; cc < rtin->num_chan ; cc++ ){

      if( ! ISVALID_3DIM_DATASET(rtin->dset[cc]) ){
        fprintf(stderr,"RT: attempt to finish channel %02d with incomplete dataset!\a\n",cc+1) ;
        nbad++ ; continue ;
      }

      if( rtin->nvol[cc] < 1 ){
        fprintf(stderr,"RT: attempt to finish channel %02d with 0 completed bricks!\a\n",cc+1) ;
        DSET_delete( rtin->dset[cc] ) ; rtin->dset[cc] = NULL ;
        if( rtin->func_dset != NULL ){
           DSET_delete( rtin->func_dset ) ; rtin->func_dset = NULL ;
        }
        if( rtin->reg_dset != NULL ){
           DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
        }

        /* Cameron Craddock to support real-time detrend, just copying
           everything (well most things) that you do to reg_dset */
        if( rtin->detrend_dset != NULL ){
           DSET_delete( rtin->detrend_dset ) ; rtin->detrend_dset = NULL ;
        }
        /* CC end */

        if( rtin->reg_base_dset != NULL ){
           DSET_delete( rtin->reg_base_dset ) ; rtin->reg_base_dset = NULL ;
        }
        if( rtin->mrg_dset != NULL ){
           DSET_delete( rtin->mrg_dset ) ; rtin->mrg_dset = NULL ;
        }
        if( rtin->reg_chan_dset[cc] != NULL ){
           DSET_delete(rtin->reg_chan_dset[cc]); rtin->reg_chan_dset[cc]=NULL;
        }
        nbad++ ;
      }

      if( rtin->nsl[cc] > 0 )
         fprintf(stderr,"RT: finish channel %02d with %d slices unused!\a\n",
                 cc+1,rtin->nsl[cc]);

      fprintf(stderr,"RT: finish channel %02d with %d bricks completed.\n",
              cc+1,rtin->nvol[cc]) ;
   }

   if( verbose ) SHOW_TIMES ;
   if( nbad    ) return ;

   /*--- Do the "at end" registration, if ordered and if possible ---*/

   switch( rtin->reg_mode ){
      case REGMODE_2D_ATEND: RT_registration_2D_atend( rtin ) ; break ;
      case REGMODE_3D_ATEND: RT_registration_3D_atend( rtin ) ; break ;
   }

   /*--- 17 Aug 1998: Graph the motion parameters, if desired ---*/

   if( rtin->reg_graph && rtin->reg_nest > 1 && REG_IS_2D(rtin->reg_mode) ){
      float * yar[4] ;  /* not Tasha */
      int * iar , ii , nn = rtin->reg_nest ;
      static char * nar[3] = { "\\Delta x [mm]" , "\\Delta y [mm]" , "\\phi   [\\degree]" } ;

      if( verbose > 1 )
         fprintf(stderr,"RT: graphing estimated 2D motion parameters\n") ;

      /* sort the arrays by time */

      iar    = (int *)   malloc( sizeof(int)   * nn ) ;  /* index */
      yar[0] = (float *) malloc( sizeof(float) * nn ) ;  /* time  */
      yar[1] = (float *) malloc( sizeof(float) * nn ) ;  /* dx    */
      yar[2] = (float *) malloc( sizeof(float) * nn ) ;  /* dy    */
      yar[3] = (float *) malloc( sizeof(float) * nn ) ;  /* phi   */

      for( ii=0 ; ii < nn ; ii++ ){
         iar[ii] = ii ; yar[0][ii] = rtin->reg_tim[ii] ;
      }

      qsort_floatint( nn , yar[0] , iar ) ;     /* sort by time, carry index */

      for( ii=0 ; ii < nn ; ii++ ){             /* use index to reorder params */
         yar[1][ii] = rtin->reg_dx [ iar[ii] ] ;
         yar[2][ii] = rtin->reg_dy [ iar[ii] ] ;
         yar[3][ii] = rtin->reg_phi[ iar[ii] ] ;
      }

#if 0 /* leave out for now           20 Jun 2012 [rickr] */
      X11_SET_NEW_PLOT ;          /* 01 May 2012 - RWCox */
      plot_ts_setTHIK(0.004f) ; plot_ts_setthik(0.0015f) ;
#endif
      plot_ts_lab( THE_DISPLAY ,
                   nn , yar[0] , -3 , yar+1 ,
                   "time" , NULL , DSET_FILECODE(rtin->dset[0]) , nar , NULL ) ;

      free(iar) ; free(yar[0]) ; free(yar[1]) ; free(yar[2]) ; free(yar[3]) ;
   }

   /*--- Oct 1998: do 3D graphing ---*/
   /*--- Jan 2004: if both reg_graph_xnew and reg_graph_ynew, don't plot */

   if( rtin->reg_graph && rtin->reg_nest > 1 && REG_IS_3D(rtin->reg_mode) &&
       ( rtin->reg_graph_xnew == 0 || rtin->reg_graph_ynew == 0 ) ){
      float * yar[7] ;
      int     ycount = -6 ;
      int     nn = rtin->reg_nest ;
      static char * nar[6] = {
         "\\Delta I-S [mm]" , "\\Delta R-L [mm]" , "\\Delta A-P [mm]" ,
         "Roll [\\degree]" , "Pitch [\\degree]" , "Yaw [\\degree]"  } ;

      char *ttl = malloc( strlen(DSET_FILECODE(rtin->dset[0])) + 32 ) ;
      strcpy(ttl,"\\noesc ") ;
      strcat(ttl,DSET_FILECODE(rtin->dset[0])) ;
      if( rtin->reg_mode == REGMODE_3D_ESTIM ) strcat(ttl," [Estimate]") ;

      if( verbose > 1 )
         fprintf(stderr,"RT: graphing estimated 3D motion parameters\n") ;

      /* arrays are already sorted by time */

      yar[0] = rtin->reg_rep   ;  /* repetition numbers */
      yar[1] = rtin->reg_dx    ;  /* dx    */
      yar[2] = rtin->reg_dy    ;  /* dy    */
      yar[3] = rtin->reg_dz    ;  /* dz    */
      yar[4] = rtin->reg_phi   ;  /* roll  */
      yar[5] = rtin->reg_psi   ;  /* pitch */
      yar[6] = rtin->reg_theta ;  /* yaw   */

      if ( rtin->p_code )
      {
         ycount = 1;
         yar[1] = rtin->reg_eval;
      }

#if 0 /* leave out for now          20 Jun 2012 [rickr] */

      /***** note: doing this results in a white image window in RT
                   (i.e. when registering, at the end of a run) *****/

      X11_SET_NEW_PLOT ;         /* 01 May 2012 - RWCox */
      plot_ts_setTHIK(0.004f) ; plot_ts_setthik(0.0015f) ;
#endif
      plot_ts_lab( THE_DISPLAY ,
                   nn , yar[0] , ycount , yar+1 ,
                   "Reps (TR)" , "Motion parameters" , ttl , nar , NULL ) ;

      free(ttl) ;
   }

   /* close any open tcp connection */
   if ( rtin->mp_tcp_use > 0 )
      RT_mp_comm_close( rtin, 0 );

   /* Cameron Craddock if mask is still initialized free it */
   if ( rtin->mask_init > 0 )
      RT_mp_mask_free( rtin );

   /* if we have a parser expression, free it */
   if ( rtin->p_code )
   {
      free( rtin->p_code ) ;
      rtin->p_code = NULL ;
   }

   /** tell afni about it one last time **/

   RT_tell_afni(rtin,TELL_FINAL) ;

   return ;
}

/*****************************************************************************
  The collection of functions for handling slice registration in this plugin.
******************************************************************************/

/*---------------------------------------------------------------------------
  Set the sizes of any open graph windows.

  Moved Bob's code to its own function.                  2004 Jan 28  [rickr]
-----------------------------------------------------------------------------*/

void RT_set_grapher_pinnums( int pinnum )
{
    /* 12 Oct 2000: set pin_num on all graphs now open */

    if( pinnum < MIN_PIN || pinnum > MAX_PIN || !IM3D_OPEN(plint->im3d) )
        return;

    drive_MCW_grapher( plint->im3d->g123, graDR_setpinnum, (XtPointer)ITOP(pinnum) );
    drive_MCW_grapher( plint->im3d->g231, graDR_setpinnum, (XtPointer)ITOP(pinnum) );
    drive_MCW_grapher( plint->im3d->g312, graDR_setpinnum, (XtPointer)ITOP(pinnum) );
}

/*---------------------------------------------------------------------------
  Do pieces of 2D registration during realtime.
-----------------------------------------------------------------------------*/

void RT_registration_2D_realtime( RT_input * rtin )
{
   int tt , ntt ;

   if( rtin->reg_dset == NULL ) return ;

   /*-- check to see if we need to setup first --*/

   if( rtin->reg_2dbasis == NULL ){  /* need to setup */

      /* check if enough data to setup */

      if( rtin->reg_base_index >= rtin->nvol[0] ) return ;  /* can't setup */

      /* setup the registration process */

      if( verbose )
         fprintf(stderr,"RT: setting up 2D registration 'realtime'\n") ;

      SHOW_AFNI_PAUSE ;
      RT_registration_2D_setup( rtin ) ;  /* may take a little while */

      if( rtin->reg_2dbasis == NULL ){
         fprintf(stderr,"RT: can't setup %s registration!\a\n",
                 REG_strings[REGMODE_2D_RTIME] ) ;
         DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
         rtin->reg_mode = REGMODE_NONE ;
         SHOW_AFNI_READY ; return ;
      }
   }

   /*-- register all sub-bricks that aren't done yet --*/

   ntt = DSET_NUM_TIMES( rtin->dset[g_reg_src_chan] ) ;
   for( tt=rtin->reg_nvol ; tt < ntt ; tt++ )
      RT_registration_2D_onevol( rtin , tt ) ;

   /*-- my work here is done --*/

   XmUpdateDisplay( THE_TOPSHELL ) ;
   SHOW_AFNI_READY ; return ;
}

/*---------------------------------------------------------------------------
  Do 2D registration of all slices at once, when the realtime dataset
  is all present and accounted for.
-----------------------------------------------------------------------------*/

void RT_registration_2D_atend( RT_input * rtin )
{
   int tt , ntt ;

   /* check if have enough data to register as ordered */

   if( rtin->reg_base_index >= rtin->nvol[g_reg_src_chan] ){
      fprintf(stderr,"RT: can't do %s registration: not enough 3D volumes!\a\n",
              REG_strings[REGMODE_2D_ATEND] ) ;
      DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
      rtin->reg_mode = REGMODE_NONE ;
      return ;
   }

   /* set up the registration process */

   if( verbose )
      fprintf(stderr,"RT: starting 2D registration 'at end'\n") ;

   SHOW_AFNI_PAUSE ;
   RT_registration_2D_setup( rtin ) ;

   if( rtin->reg_2dbasis == NULL ){
      fprintf(stderr,"RT: can't setup %s registration!\a\n",
              REG_strings[REGMODE_2D_ATEND] ) ;
      DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
      rtin->reg_mode = REGMODE_NONE ;
      SHOW_AFNI_READY ; return ;
   }

   /* register each volume into the new dataset */

   ntt = DSET_NUM_TIMES( rtin->dset[g_reg_src_chan] ) ;
   for( tt=0 ; tt < ntt ; tt++ ){
      XmUpdateDisplay( THE_TOPSHELL ) ;
      RT_registration_2D_onevol( rtin , tt ) ;
      if( verbose == 1 ) fprintf(stderr,"%d",tt%10) ;
   }
   if( verbose == 1 ) fprintf(stderr,"\n") ;

   /* un-setup the registration process */

   RT_registration_2D_close( rtin ) ;
   if( verbose ) SHOW_TIMES ;
   SHOW_AFNI_READY ; return ;
}

/*-----------------------------------------------------------------------
   Setup the 2D registration data for each slice
-------------------------------------------------------------------------*/

void RT_registration_2D_setup( RT_input * rtin )
{
   int ibase = rtin->reg_base_index ;
   int kk , nx,ny,nz , kind , nbar ;
   MRI_IMAGE * im ;
   char * bar ;

   if( RT_registration_set_vr_base(rtin) ) return;  /* 26 Aug 2009 [rickr] */

   nx   = DSET_NX( rtin->dset[0] ) ;
   ny   = DSET_NY( rtin->dset[0] ) ;
   nz   = DSET_NZ( rtin->dset[0] ) ;
   kind = DSET_BRICK_TYPE( rtin->dset[0] , ibase ) ;

   rtin->reg_nvol  = 0 ;

   rtin->reg_2dbasis = (MRI_2dalign_basis **)
                         malloc( sizeof(MRI_2dalign_basis *) * nz ) ;

   im   = mri_new_vol_empty( nx,ny,1 , kind ) ;     /* fake image for slices */

   /* set pointer to base volume                         26 Aug 2009 [rickr] */
   if( rtin->reg_base_dset ) bar = DSET_BRICK_ARRAY(rtin->reg_base_dset, 0);
   else                      bar = DSET_BRICK_ARRAY(rtin->dset[g_reg_src_chan],
                                                    ibase);

   nbar = im->nvox * im->pixel_size ;               /* offset for each slice */

   for( kk=0 ; kk < nz ; kk++ ){                         /* loop over slices */
      mri_fix_data_pointer( bar + kk*nbar , im ) ;             /* base image */
      rtin->reg_2dbasis[kk] = mri_2dalign_setup( im , NULL ) ;
   }

   kk = rtin->reg_resam ;
   if( kk == MRI_QUINTIC || kk == MRI_HEPTIC )
      kk = MRI_BICUBIC ; /* quintic & heptic not available in 2D */

   mri_2dalign_method( MRI_BILINEAR, MRI_BICUBIC, kk ) ;

   mri_fix_data_pointer( NULL , im ) ; mri_free( im ) ;   /* get rid of this */
   return ;
}

/*---------------------------------------------------------------------------
    Undo the routine just above!
-----------------------------------------------------------------------------*/

void RT_registration_2D_close( RT_input * rtin )
{
   int kk , nz ;

   nz = DSET_NZ( rtin->dset[0] ) ;
   for( kk=0 ; kk < nz ; kk++ )
      mri_2dalign_cleanup( rtin->reg_2dbasis[kk] ) ;

   free( rtin->reg_2dbasis ) ; rtin->reg_2dbasis = NULL ;
   return ;
}

/*-----------------------------------------------------------------------
   Carry out the 2D registration for each slice in the tt-th sub-brick
-------------------------------------------------------------------------*/

#ifndef D2RFAC
#  define D2RFAC (PI/180.0)
#  define R2DFAC (180.0/PI)
#endif

void RT_registration_2D_onevol( RT_input * rtin , int tt )
{
   int kk , nx,ny,nz , kind , nbar ;
   MRI_IMAGE * im , * rim , * qim ;
   char * bar , * rar , * qar ;
   float dx,dy,phi ;        /* 17 Aug 1998 */
   int   nest , nxy ;

   /*-- sanity check --*/

   if( rtin->dset[g_reg_src_chan] == NULL || rtin->reg_dset == NULL ) return ;

   nx   = DSET_NX( rtin->dset[0] ) ;
   ny   = DSET_NY( rtin->dset[0] ) ; nxy = nx * ny ;
   nz   = DSET_NZ( rtin->dset[0] ) ;
   kind = DSET_BRICK_TYPE( rtin->dset[0] , 0 ) ;

   im   = mri_new_vol_empty( nx,ny,1 , kind ) ;     /* fake image for slices */
   /* ptr to input volume   */
   bar  = DSET_BRICK_ARRAY( rtin->dset[g_reg_src_chan] , tt ) ;
   nbar = im->nvox * im->pixel_size ;               /* offset for each slice */

   /* make space for new sub-brick in reg_dset */

   if( verbose > 1 )
      fprintf(stderr,"RT: 2D registering sub-brick %d",tt) ;

   rar = (char *) malloc( sizeof(char) * nx*ny*nz * im->pixel_size ) ;

   if( rar == NULL ){
      fprintf(stderr,"RT: can't malloc space for registered dataset!\a\n") ;
      DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
      rtin->reg_mode = REGMODE_NONE ;
      return ;
   }

   /*-- loop over slices --*/

   for( kk=0 ; kk < nz ; kk++ ){

      if( verbose > 1 ) fprintf(stderr,".") ;

      mri_fix_data_pointer( bar + kk*nbar , im ) ;  /* image to register */

      /* registration! */

      rim = mri_2dalign_one( rtin->reg_2dbasis[kk] , im , &dx , &dy , &phi ) ;

      /* 17 Aug 1998: save estimated motion parameters */

      nest = rtin->reg_nest ;
      rtin->reg_tim = (float *) realloc( (void *) rtin->reg_tim ,
                                         sizeof(float) * (nest+1) ) ;
      rtin->reg_dx  = (float *) realloc( (void *) rtin->reg_dx  ,
                                         sizeof(float) * (nest+1) ) ;
      rtin->reg_dy  = (float *) realloc( (void *) rtin->reg_dy  ,
                                         sizeof(float) * (nest+1) ) ;
      rtin->reg_phi = (float *) realloc( (void *) rtin->reg_phi ,
                                         sizeof(float) * (nest+1) ) ;

      rtin->reg_tim[nest] = THD_timeof_vox( tt , kk*nxy ,
                                            rtin->dset[g_reg_src_chan] ) ;
      rtin->reg_dx [nest] = dx * DSET_DX(rtin->dset[0]) ;
      rtin->reg_dy [nest] = dy * DSET_DY(rtin->dset[0]) ;
      rtin->reg_phi[nest] = phi * R2DFAC             ; rtin->reg_nest ++ ;

      /* convert output image to desired type;
         set qar to point to data in the converted registered image */

      switch( kind ){
         case MRI_float:                        /* rim is already floats */
            qar = (char *) MRI_FLOAT_PTR(rim) ;
         break ;

         case MRI_short:
            qim = mri_to_short(1.0,rim) ; mri_free(rim) ; rim = qim ;
            qar = (char *) MRI_SHORT_PTR(rim) ;
         break ;

         case MRI_byte:
            qim = mri_to_byte(rim) ; mri_free(rim) ; rim = qim ;
            qar = (char *) MRI_BYTE_PTR(rim) ;
         break ;

         default:
            fprintf(stderr,"RT: can't do 2D registration on %s images!\a\n",
                    MRI_TYPE_name[kind] ) ;
            DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
            rtin->reg_mode = REGMODE_NONE ;
            free(rar) ;
            mri_free(rim) ; mri_fix_data_pointer(NULL,im) ; mri_free(im) ;
         return ;
      }

      /* copy data from registered image into output sub-brick */

      memcpy( rar + kk*nbar , qar , nbar ) ;

      mri_free(rim) ; /* don't need this no more no how */
   }

   mri_fix_data_pointer(NULL,im) ; mri_free(im) ;   /* get rid of this */

   /*-- attach rar to reg_dset --*/

   if( tt == 0 )
      EDIT_substitute_brick( rtin->reg_dset , 0 , rtin->datum , rar ) ;
   else
      EDIT_add_brick( rtin->reg_dset , rtin->datum , 0.0 , rar ) ;

   rtin->reg_nvol = tt+1 ;

   EDIT_dset_items( rtin->reg_dset , ADN_ntt , rtin->reg_nvol ,  ADN_none ) ;

   if( verbose > 1 ) fprintf(stderr,"\n") ;
   return ;
}
/*---------------------------------------------------------------------------*/


/*****************************************************************************
  The collection of functions for handling volume registration in this plugin.
******************************************************************************/

/*---------------------------------------------------------------------------
   Called when the user kills the realtime graph of motion parameters
-----------------------------------------------------------------------------*/

void MTD_killfunc( MEM_topshell_data *mp )
{
   /* if mp is for active input, then set the active one to nada */

   if( mp == NULL ) return ;
   if( rtinp != NULL && mp == rtinp->mp ){
      if( verbose ) fprintf(stderr,"RT: user killed active realtime graph\n") ;
      rtinp->mp = NULL ;
   } else {
      if( verbose ) fprintf(stderr,"RT: user killed inactive realtime graph\n") ;
   }

   if( mp->userdata != NULL ){ free(mp->userdata) ; mp->userdata = NULL ; }
   return ;
}

/*---------------------------------------------------------------------------
  Do pieces of 3D registration during realtime.
-----------------------------------------------------------------------------*/

void RT_registration_3D_realtime( RT_input *rtin )
{
   int tt , ntt , ttbot ;

   /*-- check to see if we need to setup first --*/

   if( rtin->reg_3dbasis == NULL ){  /* need to setup */

      /* check if enough data to setup (0 -> g_reg_src_chan 27 Oct 2016) */
      if( rtin->reg_base_index >= rtin->nvol[g_reg_src_chan] )
         return ;  /* can't setup */

      /* rcr OC - applies to 1 and 2 */
      if( RT_will_register_merged_dset(rtin) &&
          rtin->reg_base_index >= rtin->mrg_nvol ) return ;

      /* setup the registration process */

      if( verbose ){
         if( rtin->reg_mode == REGMODE_3D_ESTIM )
            fprintf(stderr,"RT: setting up 3D registration 'estimate'\n") ;
         else
            fprintf(stderr,"RT: setting up 3D registration 'realtime'\n") ;
      }

      SHOW_AFNI_PAUSE ;
      RT_registration_3D_setup( rtin ) ;  /* may take a while */

      if( rtin->reg_3dbasis == NULL ){
         fprintf(stderr,"RT: can't setup %s registration!\a\n",
                 REG_strings[rtin->reg_mode] ) ;
         if( rtin->reg_dset != NULL ){
            DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
         }
         rtin->reg_mode = REGMODE_NONE ;
         SHOW_AFNI_READY ; return ;
      }

      /* realtime graphing? */

      if( rtin->reg_graph == 2 ){
         int    ycount = -6 ;  /* default number of graphs */
         static char * nar[6] = {
            "\\Delta I-S [mm]" , "\\Delta R-L [mm]" , "\\Delta A-P [mm]" ,
            "Roll [\\degree]" , "Pitch [\\degree]" , "Yaw [\\degree]"  } ;

         char *ttl = malloc( strlen(DSET_FILECODE(rtin->dset[0])) + 32 ) ;
         strcpy(ttl,"\\noesc ") ;
         strcat(ttl,DSET_FILECODE(rtin->dset[0])) ;
         if( rtin->reg_mode == REGMODE_3D_ESTIM ) strcat(ttl," [Estimate]") ;

         /* if p_code, only plot the reg_eval */
         if ( rtin->p_code )
            ycount = 1;

         plot_ts_setTHIK(0.005f) ; plot_ts_setthik(0.002f) ;
         rtin->mp = plot_ts_init( GLOBAL_library.dc->display ,
                                  0.0,rtin->reg_graph_xr-1 ,
                                  ycount,-rtin->reg_graph_yr,rtin->reg_graph_yr,
                                  "reps (TR)", "motion parameters" , ttl, nar , NULL ) ;

         if( rtin->mp != NULL ) rtin->mp->killfunc = (void_func *)MTD_killfunc ;

         free(ttl) ;

         /* set up comm for motion params    30 Mar 2004 [rickr] */
         RT_mp_comm_init_vars( rtin ) ;
         if ( rtin->mp_tcp_use > 0 ) RT_mp_comm_init( rtin ) ;
      }
   }

   /*-- register all sub-bricks that aren't done yet --*/

   /* rcr OC - applies to 1 and 2 */
   if( RT_will_register_merged_dset(rtin) )
      ntt = DSET_NUM_TIMES( rtin->mrg_dset ) ;
   else
      /* do not proceed with registration until all channels are complete */
      ntt = DSET_NUM_TIMES( rtin->dset[rtin->num_chan-1] ) ;


   ttbot = rtin->reg_nvol ;
   for( tt=ttbot ; tt < ntt ; tt++ )
      RT_registration_3D_onevol( rtin , tt ) ;

   /* even if user closed graph window, proceed if mp_tcp_use is set */
   if( ntt > ttbot && (rtin->mp || rtin->mp_tcp_use > 0) ){
      float        * yar[7] ;
      int            ycount = -6 ;

      yar[0] = rtin->reg_rep   + ttbot ;
      yar[1] = rtin->reg_dx    + ttbot ;
      yar[2] = rtin->reg_dy    + ttbot ;
      yar[3] = rtin->reg_dz    + ttbot ;
      yar[4] = rtin->reg_phi   + ttbot ;
      yar[5] = rtin->reg_psi   + ttbot ;
      yar[6] = rtin->reg_theta + ttbot ;

      /* send the data off over tcp connection           30 Mar 2004 [rickr] */
      /* - if merge after reg, send data after merging    2 Mar 2017 [rickr] */
      /* *** no longer send data at this point ***       19 Dec 2019 [rickr] */
      /*     (only call RT_mp_comm_send_data_wrapper from RT_process_image)  */

      if( g_show_times ) {
          char vmesg[64];
          sprintf(vmesg,"registered %d vol(s), %d..%d",ntt-ttbot,ttbot,ntt-1);
          RT_show_duration(vmesg);
      }

      if( ttbot > 0 )  /* modify yar after send_data, when ttbot > 0 */
      {
          int c;
          for ( c = 0; c < 7; c++ )  /* apply the old ttbot-- */
              yar[c]--;
          ttbot-- ;
      }

      /* if p_code, only plot the reg_eval */
      if ( rtin->p_code )
      {
         ycount = 1;
         yar[1] = rtin->reg_eval + ttbot;
      }

      if( rtin->mp )
         plot_ts_addto( rtin->mp , ntt-ttbot , yar[0] , ycount , yar+1 ) ;
   }

   /*-- my work here is done --*/

   XmUpdateDisplay( THE_TOPSHELL ) ;
   SHOW_AFNI_READY ; return ;
}

/*---------------------------------------------------------------------------
  Do 3D registration of all slices at once, when the realtime dataset
  is all present and accounted for.
-----------------------------------------------------------------------------*/

void RT_registration_3D_atend( RT_input * rtin )
{
   int tt , ntt ;

   /* check if have enough data to register as ordered */

   if( rtin->reg_base_index >= rtin->nvol[0] ){
      fprintf(stderr,"RT: can't do %s registration: not enough 3D volumes!\a\n",
              REG_strings[rtin->reg_mode] ) ;
      DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
      rtin->reg_mode = REGMODE_NONE ;
      return ;
   }

   /* set up the registration process */

   if( verbose )
      fprintf(stderr,"RT: starting 3D registration 'at end'\n") ;

   SHOW_AFNI_PAUSE ;
   RT_registration_3D_setup( rtin ) ;

   if( rtin->reg_3dbasis == NULL ){
      fprintf(stderr,"RT: can't setup %s registration!\a\n",
              REG_strings[rtin->reg_mode] ) ;
      DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
      rtin->reg_mode = REGMODE_NONE ;
      SHOW_AFNI_READY ; return ;
   }

   /* register each volume into the new dataset */

   ntt = DSET_NUM_TIMES( rtin->dset[g_reg_src_chan] ) ;
   if( verbose == 1 ) fprintf(stderr,"RT: ") ;
   for( tt=0 ; tt < ntt ; tt++ ){
      XmUpdateDisplay( THE_TOPSHELL ) ;
      RT_registration_3D_onevol( rtin , tt ) ;
      if( verbose == 1 ) fprintf(stderr,"%d",tt%10) ;
   }
   if( verbose == 1 ) fprintf(stderr,"\n") ;

   /* un-setup the registration process */

   RT_registration_3D_close( rtin ) ;
   if( verbose ) SHOW_TIMES ;
   SHOW_AFNI_READY ; return ;
}

/*-----------------------------------------------------------------------
   Verify and possibly store the volume registration base dataset.

   The global g_reg_base_dset variable will be set anytime the base needs
   to be stored, which is:
        1. EXTERN: the base/index is from an External dataset
           In RT_main: nuke and set g_reg_base_dset from the chosen dset.
           In any case, apply any g_reg_base_dset in new_RT_input.
        2. CUR_KEEP: from current run, keep for future
           Here (set_vr_base), if g_reg_base_dset is NULL, set to current
           dset and appropriate volume.  If g_reg_base_dset is not NULL,
           it was done previously.
           In the previous case, g_reg_base_dset will have been _copied_ to
           rtin->reg_base_dset in new_RT_input.
        *. NOT done for CUR: current run and index
    So if rtin->reg_base_dset, either EXTERN or old CUR_KEEP cases.  Else
    new CUR_KEEP or just CUR.  Note, rtin->reg_base_dset is always a copy.
-------------------------------------------------------------------------*/
int RT_registration_set_vr_base(RT_input * rtin)
{
   THD_3dim_dataset * dset;
   int                code;

   ENTRY("RT_registration_set_vr_base");

   if( rtin->reg_base_mode == RT_RBASE_MODE_CUR ) RETURN(0); /* nothing to do */

   /* note dset to register                     27 May 2010 [rickr] */
   /* rcr OC - applies to 1 and 2
               also, can choose channel */
   if( RT_will_register_merged_dset(rtin) )
      dset = rtin->mrg_dset;
   else
      dset = rtin->dset[g_reg_src_chan];  /* 0->g_reg_src_chan  27 Oct 2016 */

   /* If CUR_KEEP, create the global base dataset (deleting any old one).
      Note that we still do not need to set rtin->reg_base_dset. */
   if( rtin->reg_base_mode == RT_RBASE_MODE_CUR_KEEP && ! g_reg_base_dset ) {
      g_reg_base_dset = THD_copy_one_sub(dset, rtin->reg_base_index);
      if( ! g_reg_base_dset ) { 
         if (doPopups){/* 12 Aug 2008 [jwhite] */ 
           PLUTO_beep() ; 
           PLUTO_popup_transient( plint , "Failed to set volreg base dset!" );
         }
         /* 21 Sep 2011 [jlisinski] */
         fprintf(stderr, "** Failed to set volreg base dset");
         RETURN(1);
      }
      RETURN(0);
   }

   /* CHOOSE dset, so verify grid, etc. */
   code = THD_dataset_mismatch(rtin->reg_base_dset, dset);
   if( code ) { 
      if (doPopups){/* 12 Aug 2008 [jwhite] */ 
        PLUTO_beep() ;
        PLUTO_popup_transient(plint, "Dataset mismatch with volreg base dset!");
      }
      fprintf(stderr, "** Dataset mismatch with volreg base: code = %d\n",code);
      RETURN(1);
   }

   RETURN(0);
}

/*-----------------------------------------------------------------------
   Setup the 3D registration data
-------------------------------------------------------------------------*/

#define VL_MIT  9     /* iterations */
#define VL_DXY  0.05  /* voxels */
#define VL_DPH  0.07  /* degrees */
#define VL_DEL  0.70  /* voxels */

void RT_registration_3D_setup( RT_input * rtin )
{
   THD_3dim_dataset * dset;             /* registration dset may vary */
   int ibase = rtin->reg_base_index ;
   int kk ;
   MRI_IMAGE * im ;
   char * ept ;

   if( RT_registration_set_vr_base(rtin) ) return;  /* failure */

   /* note dset to register                     27 May 2010 [rickr] */
   /* rcr OC - add case for option 3 */
   if( RT_will_register_merged_dset(rtin) ) dset = rtin->mrg_dset;
   else                                     dset = rtin->dset[g_reg_src_chan];

   /*-- extract info about coordinate axes of dataset --*/

   rtin->iha  = THD_handedness( dset )   ;  /* LH or RH? */

   rtin->ax1  = THD_axcode( dset , 'I' ) ;
   rtin->hax1 = rtin->ax1 * rtin->iha      ;      /* roll */

   rtin->ax2  = THD_axcode( dset , 'R' ) ;
   rtin->hax2 = rtin->ax2 * rtin->iha      ;      /* pitch */

   rtin->ax3  = THD_axcode( dset , 'A' ) ;
   rtin->hax3 = rtin->ax3 * rtin->iha      ;      /* yaw */

   if( rtin->reg_base_dset ) im = DSET_BRICK(rtin->reg_base_dset,0) ;
   else                      im = DSET_BRICK(dset,ibase) ;

   im->dx = fabs( DSET_DX(dset) ) ;  /* must set voxel dimensions */
   im->dy = fabs( DSET_DY(dset) ) ;
   im->dz = fabs( DSET_DZ(dset) ) ;

   switch( rtin->reg_mode ){
      default: rtin->reg_3dbasis = NULL ;   /* should not occur */
      return ;

      case REGMODE_3D_RTIME:                /* actual registration */
      case REGMODE_3D_ATEND:
         if( verbose > 1 )fprintf(stderr, "RT: do full registration\n" );
         ept = getenv("AFNI_REALTIME_volreg_maxite") ;
         kk  = VL_MIT ;
         if( ept != NULL ){
            kk = strtol(ept,NULL,10) ; if( kk <= 0 ) kk = VL_MIT ;
         }
         mri_3dalign_params( kk , VL_DXY , VL_DPH , VL_DEL ,
                 abs(rtin->ax1)-1 , abs(rtin->ax2)-1 , abs(rtin->ax3)-1 , -1 ) ;

                                                           /* noreg , clipit */
         mri_3dalign_method( rtin->reg_resam , verbose==2 ,   0     , 1       );

         mri_3dalign_final_regmode( rtin->reg_final_resam ) ;

         rtin->reg_3dbasis = mri_3dalign_setup( im ,  NULL ) ;
      break ;

      case REGMODE_3D_ESTIM:               /* just estimate motion */
         if( verbose > 1 )fprintf(stderr, "RT: just estimate motion\n" );
         ept = getenv("AFNI_REALTIME_volreg_maxite_est") ;
         kk  = 1 ;
         if( ept != NULL ){
            kk = strtol(ept,NULL,10) ; if( kk <= 0 ) kk = 1 ;
         }

         mri_3dalign_params( kk , VL_DXY , VL_DPH , 2.0*VL_DEL ,
                 abs(rtin->ax1)-1 , abs(rtin->ax2)-1 , abs(rtin->ax3)-1 , -1 ) ;

         kk = MRI_CUBIC ;                     /* noreg , clipit */
         mri_3dalign_method( kk , verbose==2 ,   1     , 0        ) ;

         rtin->reg_3dbasis = mri_3dalign_setup( im ,  NULL ) ;
#if 0
         kk = MRI_LINEAR ;
         mri_3dalign_method( kk , verbose==2 ,   1     , 0        ) ;
#endif
      break ;
   }

   rtin->reg_nvol  = 0 ;
   return ;
}

/*---------------------------------------------------------------------------
    Undo the routine just above!
-----------------------------------------------------------------------------*/

void RT_registration_3D_close( RT_input * rtin )
{
   mri_3dalign_cleanup( rtin->reg_3dbasis ) ;
   rtin->reg_3dbasis = NULL ;
   return ;
}

/*-----------------------------------------------------------------------
   Carry out the 3D registration for the tt-th sub-brick
-------------------------------------------------------------------------*/

#ifndef D2RFAC
#  define D2RFAC (PI/180.0)
#  define R2DFAC (180.0/PI)
#endif

void RT_registration_3D_onevol( RT_input *rtin , int tt )
{
   THD_3dim_dataset *source;
   MRI_IMARR *imarr , *outarr=NULL;
   MRI_IMAGE *rim , *qim , *tim ;
   char *qar ;
   float dx,dy,dz , roll,pitch,yaw , ddx=0.0,ddy=0.0,ddz=0.0 ;
   int   nest, cc, datum ;

   /*-- sanity check --*/

   /* rcr OC - option to set source channel
        ChannelMerge MergeRegister modes (RT_CM_RMODE_*)
           - NONE
           - reg merged
           - reg M and chans
           - reg chans only     (merging probably happens after chan reg)
        Registration SourceChan -1..(Nchan-1)
           - -1 means last channel (since possibly unknown?)
           - 0 is default in 0-based list

        have g_reg_src_chan from AFNI_REALTIME_Reg_Source_Chan & GUI
        set RT_chmrg_reg_mode
        trace references to reg_chan_mode and RT_CM_RMODE_NONE
        add variable for Registration:SourceChan
        for RT_CM_RMODE_POST_MRG: merge reg_chan_dset list
            - merging requires float data?
        update Help
        with multi-chan, no reg_dset
          - allow reg in multi- mode, esp given Src Chan
          - still add 'chans only' to MergeRegister list
            (i.e. register channels without merging)
          - again: maybe change MergeRegister to be (just Register?)
                - FLAG to register channels
                - MERGE: pre-reg or post-reg?
             or
                FLAG? : register chan or merge
                - none
                - merge pre-reg
                - merge and reg chans
                - reg chans and merge
                - just reg chans
          - afni hangs at end of run, waiting for Dimon to close
   */

   if( RT_will_register_merged_dset(rtin) ) {
      if(verbose && !tt)   /* chat at time point 0 */
         fprintf(stderr,"RTCM: using mrg_dset as reg source\n");
      source = rtin->mrg_dset ;
   }
   else {
      if(verbose && !tt) /* chat at time point 0 */
         fprintf(stderr,"RT: use chan[%d] dset as reg source\n",
                 g_reg_src_chan);
      source = rtin->dset[g_reg_src_chan] ; /* 0 -> g_reg_src_chan */
   }

   if( source == NULL ) return ;

   /* merge datum might differ from rtin->datum */
   datum = DSET_BRICK_TYPE(source, 0);

   /* if merging after registration and insufficient channel data, bail */
   /* rcr - fix (this should not apply) */
   if ( RT_when_to_merge() == RT_CM_MERGE_AFTER_REG &&
         rtin->nvol[rtin->num_chan-1] <= tt )
      return;

   /* otherwise, merge before registration */

   /*------------------------- actual registration -------------------------*/

   if( verbose > 1 )
      fprintf(stderr,"RT: 3D registering sub-brick %d\n",tt) ;

   qim     = DSET_BRICK(source,tt) ;
   qim->dx = fabs( DSET_DX(source) ) ;  /* must set voxel dimensions */
   qim->dy = fabs( DSET_DY(source) ) ;
   qim->dz = fabs( DSET_DZ(source) ) ;

   /* rcr OC - source should be properly set
             - then 3dalign_one from qim
             - then mri_3dalign_apply(imarr, ...) */
      /* rcr OC - if none, else for 3 use chan# */

   /* always do the base, then apply afterwards        27 Oct 2016 [rickr] */
   rim = mri_3dalign_one( rtin->reg_3dbasis , qim ,
                          &roll , &pitch , &yaw , &dx , &dy , &dz ) ;

   /* if aligning channels, apply the parameters to the channels */
   if( rtin->reg_chan_mode >= RT_CM_RMODE_REG_CHAN ) {
      /* align mrg_dset and all channels as followers  27 May 2010 [rickr] */
      /* input imarr should have source image and then each channel        */
      if(verbose>1 && !tt) { /* chat at time point 0 */
         fprintf(stderr,"RTCM: applying align params to %d channels\n",
                 rtin->num_chan);
         fprintf(stderr,"RTCM init motion [%d]: %f %f %f %f %f %f\n",
                 tt, roll, pitch, yaw, dx, dy, dz);
      }
      INIT_IMARR(imarr);
      /* is this needed (aaaa5555)?  if so, needs alt var names
         dx = qim->dx ; dy = qim->dy ; dz = qim->dz ;   * store for channels */

      for( cc = 0; cc < rtin->num_chan; cc++ ) {
         tim = DSET_BRICK(rtin->dset[cc],tt) ;
         /* is this needed (aaaa5555)?
            tim->dx = dx ; tim->dy = dy ; tim->dz = dz ;  * from qim */
         ADDTO_IMARR(imarr, tim);
      }

      /* rcr OC - change mri_3dalign_oneplus() to mri_3dalign_apply()     */
      /*          (and specify to keep the input datum, if short or byte) */
      outarr = mri_3dalign_apply( rtin->reg_3dbasis , imarr ,
                                  roll , pitch , yaw , dx , dy , dz , 1 ) ;
      if( outarr == NULL ) {
         fprintf(stderr,"** mri_3dalign_apply returns NULL\n");
         return;
      }

      FREE_IMARR(imarr);        /* but do not free the images */
   }

   /*--------------- massage and store movement parameters ----------------*/

   roll  *= R2DFAC ; if( rtin->hax1 < 0 ) roll  = -roll  ;
   pitch *= R2DFAC ; if( rtin->hax2 < 0 ) pitch = -pitch ;
   yaw   *= R2DFAC ; if( rtin->hax3 < 0 ) yaw   = -yaw   ;

   switch( source->daxes->xxorient ){
      case ORI_R2L_TYPE: ddy =  dx; break; case ORI_L2R_TYPE: ddy = -dx; break;
      case ORI_P2A_TYPE: ddz = -dx; break; case ORI_A2P_TYPE: ddz =  dx; break;
      case ORI_I2S_TYPE: ddx =  dx; break; case ORI_S2I_TYPE: ddx = -dx; break;
   }

   switch( source->daxes->yyorient ){
      case ORI_R2L_TYPE: ddy =  dy; break; case ORI_L2R_TYPE: ddy = -dy; break;
      case ORI_P2A_TYPE: ddz = -dy; break; case ORI_A2P_TYPE: ddz =  dy; break;
      case ORI_I2S_TYPE: ddx =  dy; break; case ORI_S2I_TYPE: ddx = -dy; break;
   }

   switch( source->daxes->zzorient ){
      case ORI_R2L_TYPE: ddy =  dz; break; case ORI_L2R_TYPE: ddy = -dz; break;
      case ORI_P2A_TYPE: ddz = -dz; break; case ORI_A2P_TYPE: ddz =  dz; break;
      case ORI_I2S_TYPE: ddx =  dz; break; case ORI_S2I_TYPE: ddx = -dz; break;
   }

   nest = rtin->reg_nest ;
   rtin->reg_tim = (float *) realloc( (void *) rtin->reg_tim ,
                                      sizeof(float) * (nest+1) ) ;
   rtin->reg_dx  = (float *) realloc( (void *) rtin->reg_dx  ,
                                      sizeof(float) * (nest+1) ) ;
   rtin->reg_dy  = (float *) realloc( (void *) rtin->reg_dy  ,
                                      sizeof(float) * (nest+1) ) ;
   rtin->reg_dz  = (float *) realloc( (void *) rtin->reg_dz  ,
                                      sizeof(float) * (nest+1) ) ;
   rtin->reg_phi = (float *) realloc( (void *) rtin->reg_phi ,
                                      sizeof(float) * (nest+1) ) ;
   rtin->reg_psi = (float *) realloc( (void *) rtin->reg_psi ,
                                      sizeof(float) * (nest+1) ) ;
   rtin->reg_theta = (float *) realloc( (void *) rtin->reg_theta ,
                                      sizeof(float) * (nest+1) ) ;
   rtin->reg_rep   = (float *) realloc( (void *) rtin->reg_rep ,
                                      sizeof(float) * (nest+1) ) ;
   rtin->reg_eval   = (float *) realloc( (void *) rtin->reg_eval ,
                                      sizeof(float) * (nest+1) ) ;

   rtin->reg_tim[nest]   = THD_timeof_vox( tt , 0 , source ) ;
   rtin->reg_dx [nest]   = ddx   ;
   rtin->reg_dy [nest]   = ddy   ;
   rtin->reg_dz [nest]   = ddz   ;
   rtin->reg_phi[nest]   = roll  ;
   rtin->reg_psi[nest]   = pitch ;
   rtin->reg_theta[nest] = yaw   ;
   rtin->reg_rep[nest]   = tt    ;

   if( verbose > 1 ) fprintf(stderr,"RTCM motion [%d]: %f %f %f %f %f %f\n",
                             tt, roll, pitch, yaw, ddx, ddy, ddz);

   /* evaluate parser expression, since all input values are assigned here */
   if ( rtin->p_code )
   {
       int c ;

       /* clear extras, just to be safe */
       for ( c = 6; c < 26; c++ ) rtin->p_atoz[c] = 0;

       rtin->p_atoz[0] = ddx ; rtin->p_atoz[1] = ddy  ; rtin->p_atoz[2] = ddz;
       rtin->p_atoz[3] = roll; rtin->p_atoz[4] = pitch; rtin->p_atoz[5] = yaw;

       /* actually set the value via parser evaluation */
       rtin->reg_eval[nest] = PARSER_evaluate_one(rtin->p_code, rtin->p_atoz);
   }

   rtin->reg_nest ++ ; rtin->reg_nvol = tt+1 ;

   /*-- convert output image to desired type;
        set qar to point to data in the converted registered image --*/

   if( rim != NULL && rtin->reg_dset != NULL ){
      switch( datum ){
         case MRI_float:                        /* rim is already floats */
            qar = (char *) MRI_FLOAT_PTR(rim) ;
         break ;

         case MRI_short:
            qim = mri_to_short(1.0,rim) ; mri_free(rim) ; rim = qim ;
            qar = (char *) MRI_SHORT_PTR(rim) ;
         break ;

         case MRI_byte:
            qim = mri_to_byte(rim) ; mri_free(rim) ; rim = qim ;
            qar = (char *) MRI_BYTE_PTR(rim) ;
         break ;

         case MRI_complex:   /* 20 May 2009: for MCW and Andre Jesmanowicz */
            qar = (char *) MRI_COMPLEX_PTR(rim) ;
         break ;

         /* this case should not occur: */

         default:
            fprintf(stderr,"RT: can't do 3D registration on %s images!\a\n",
                    MRI_TYPE_name[datum] ) ;
            DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
            rtin->reg_mode = REGMODE_NONE ;
            mri_free(rim) ;
         return ;
      }

      /* attach to reg_dset */

      if( tt == 0 )
         EDIT_substitute_brick( rtin->reg_dset , 0 , datum , qar ) ;
      else
         EDIT_add_brick( rtin->reg_dset , datum , 0.0 , qar ) ;

      EDIT_dset_items( rtin->reg_dset , ADN_ntt , rtin->reg_nvol ,  ADN_none ) ;

      mri_fix_data_pointer(NULL,rim) ; mri_free(rim) ;   /* get rid of this */

      /*-- if registering channels with respect to 'source' params,
           store results (no conversion)         2 Jun 2010 [rickr] --*/
      if( rtin->reg_chan_mode >= RT_CM_RMODE_REG_CHAN && outarr ) {
         for( cc = 0; cc < rtin->num_chan; cc++ ) {
            /* no more imarr[cc+1], since reg master is no longer in it */
            if( tt == 0 ) EDIT_substitute_brick(rtin->reg_chan_dset[cc], 0,
                                       rtin->datum, outarr->imarr[cc]->im) ;
            else EDIT_add_brick(rtin->reg_chan_dset[cc], rtin->datum,
                                0.0, outarr->imarr[cc]->im) ;
            EDIT_dset_items(rtin->reg_chan_dset[cc], ADN_ntt,rtin->reg_nvol ,
                                                     ADN_none ) ;
         }
         FREE_IMARR(outarr);  /* but do not free the images */
      }
   }

   return ;
}

/*---------------------------------------------------------------------------*/

#ifndef FIM_THR
#define FIM_THR  0.0999
#endif

#define MIN_UPDT 5

/*--------------------------------------------------------------------------
  Recursively update the functional image computation.
    mode = (if >= 0) index of sub-brick to process this time
           (if <  0) one of the MODE codes above:
             INIT_MODE:   create a new dataset, and initialize workspaces
             UPDATE_MODE: put values into dataset bricks
             FINAL_MODE:  clean up workspaces

  The return value is 1 if all is OK, -1 if something bad happens.

  Adapted 27 Apr 1997 from AFNI_fimmer_compute in afni_fimmer.c -- only
  the computational stuff is left; the display stuff is relegated to
  another routine.
-----------------------------------------------------------------------------*/

#define IFree(x) do{ if( (x)!=NULL ){ free(x) ; (x)=NULL ; } } while(0)

int RT_fim_recurse( RT_input *rtin , int mode )
{
   static Three_D_View * im3d ;
   static THD_3dim_dataset * dset_time ;
   static MRI_IMAGE * ref_ts ;
   static MRI_IMAGE * ort_ts ;

   static THD_3dim_dataset * new_dset ;
   static int nvox , ngood_ref , it1 , dtyp , nxyz , nupdt ;
   static float * vval=NULL , * aval=NULL , * rbest=NULL , * abest=NULL ;
   static int   * indx=NULL ;
   static PCOR_references ** pc_ref=NULL ;
   static PCOR_voxel_corr ** pc_vc=NULL ;
   static int nx_ref , ny_ref ;

   static int fim_nref , nx_ort , ny_ort , internal_ort ;
   static float * ortar ;
   static float * ref_vec = NULL ;
   static int    nref_vec = -666 ;

   static int polort = 1 ;  /* 02 Jun 1999 */

   static char new_prefix[THD_MAX_PREFIX] ;
   static char old_prefix[THD_MAX_PREFIX] ;

   static int first_pass = 1;     /* flag for first FIM computation   30 Oct 2003 [rickr] */
   int        start_index;        /* in case of loop FIM computation  30 Oct 2003 [rickr] */

   int ifim, it, iv, ivec, nnow, itbot , ip ;
   float fthr , topval , stataux[MAX_STAT_AUX] ;
   float * tsar ;
   short * bar ;
   void  * ptr ;

   char strbuf[128] ;  /* 03 Jun 1999 */

   /******************/
   /** sanity check **/

   if( rtin == NULL ) return -1 ;

   /****************************/
   /***** initialize stuff *****/

   if( mode == INIT_MODE ){

      /* assign dset_time first   30 Oct 2003 [rickr] */
      dset_time = rtin->dset[g_reg_src_chan] ;

      /* now check for use of registered dataset        30 Oct 2003 [rickr] */
      if( (rtin->reg_mode == REGMODE_2D_RTIME) ||
          (rtin->reg_mode == REGMODE_3D_RTIME) )
         dset_time = rtin->reg_dset;

      if( dset_time == NULL ) return -1 ;

      im3d      = rtin->im3d[0] ;         if( im3d      == NULL ) return -1 ;
      ref_ts    = im3d->fimdata->fimref ; if( ref_ts    == NULL ) return -1 ;
      ort_ts    = im3d->fimdata->fimort ; /* NULL is legal here */
      nupdt     = 0 ;                     /* number of updates done yet */

      polort    = im3d->fimdata->polort ; /* 02 Jun 1999 */

      if( ort_ts != NULL ){               /* load some dimensions */
         nx_ort = ort_ts->nx ;
         ny_ort = ort_ts->ny ;
         ortar  = MRI_FLOAT_PTR(ort_ts) ;
         internal_ort = 0 ;
      } else {
         nx_ort = ny_ort = 0 ;
         ortar  = NULL ;
         internal_ort = 1 ;
      }
      fim_nref = (internal_ort) ? (polort+2) : (ny_ort+polort+2) ;

      if( nref_vec < fim_nref ){  /* allocate space for ref work vector */
          ref_vec = (float *) XtRealloc( (char *)ref_vec, sizeof(float)*fim_nref ) ;
         nref_vec = fim_nref ;
      }

      itbot     = im3d->fimdata->init_ignore ;  /* certainly don't use these points! */
      nx_ref    = ref_ts->nx ;                  /* load dimensions */
      ny_ref    = ref_ts->ny ;
      ngood_ref =  0 ;
      it1       = -1 ;
      for( ivec=0 ; ivec < ny_ref ; ivec++ ){           /* scan ref vectors */
         tsar = MRI_FLOAT_PTR(ref_ts) + (ivec*nx_ref) ; /* ptr to ivec-th vector */
         ifim = 0 ;                                     /* count # good entries */
         for( it=itbot ; it < nx_ref ; it++ ){
            if( tsar[it] < WAY_BIG ){ ifim++ ; if( it1 < 0 ) it1 = it ; }
         }

         if( ifim < MIN_UPDT ){
            fprintf(stderr,"RTfim: ideal timeseries has too few good entries!\a\n") ;
            return -1 ;
         }

         ngood_ref = MAX( ifim , ngood_ref ) ;
      }

      /** at this point, ngood_ref = max number of good reference points,
          and                  it1 = index of first point used in first reference **/

      dtyp = DSET_BRICK_TYPE(dset_time,0) ;  /* type of each voxel */
      if( ! AFNI_GOOD_FUNC_DTYPE(dtyp) ){
         fprintf(stderr,"RTfim: input dataset has unsupported datum %s!\a\n",
                 MRI_TYPE_name[dtyp] ) ;
         return -1 ;
      }

      if( nx_ort > 0 && nx_ort < nx_ref ){  /* check for compatibility */
         fprintf(stderr,
                 "RTfim: WARNING -- ort length=%d  ideal length=%d\n",
                 nx_ort , nx_ref ) ;
      }

      /*--- Create a new prefix ---*/

      MCW_strncpy( old_prefix , DSET_PREFIX(dset_time) , THD_MAX_PREFIX-3 ) ;

      for( ifim=1 ; ifim < 99 ; ifim++ ){
         sprintf( new_prefix , "%s@%d" , old_prefix , ifim ) ;
         if( PLUTO_prefix_ok(new_prefix) ) break ;
      }
      if( ifim == 99 ){
         fprintf(stderr,"RTfim: can't create new prefix!\a\n") ;
         return -1 ;
      }

      nxyz =  dset_time->dblk->diskptr->dimsizes[0]    /* total voxel count */
            * dset_time->dblk->diskptr->dimsizes[1]
            * dset_time->dblk->diskptr->dimsizes[2] ;

      return 1 ;
   }

   /**************************/
   /***** finalize stuff *****/

   if( mode == FINAL_MODE ){

      /*--- End of recursive updates; now free temporary workspaces ---*/

      if( pc_ref != NULL ){
         for( ivec=0 ; ivec < ny_ref ; ivec++ ){
            free_PCOR_references(pc_ref[ivec]) ;
            free_PCOR_voxel_corr(pc_vc[ivec]) ;
         }
      }
      IFree(vval) ; IFree(indx)  ; IFree(pc_ref) ; IFree(pc_vc)  ;
      IFree(aval) ; IFree(rbest) ; IFree(abest)  ;

      first_pass = 1;                /* reset for future datasets    30 Oct 2003 [rickr] */

      return 1 ;
   }

   /*********************************************/
   /***** update the dataset under creation *****/

   if( mode == UPDATE_MODE ){

      if( nupdt < MIN_UPDT ) return 1 ;  /* can't do anything yet */

      rtin->func_condit = 2 ;  /* updated at least once */

      /*--- set the statistical parameters ---*/

      stataux[0] = nupdt ;               /* number of points used */
      stataux[1] = (ny_ref==1) ? 1 : 2 ; /* number of references  */
      stataux[2] = fim_nref - 1 ;        /* number of orts        */
      for( iv=3 ; iv < MAX_STAT_AUX ; iv++ ) stataux[iv] = 0.0 ;

      (void) EDIT_dset_items( new_dset, ADN_stat_aux, stataux, ADN_none ) ;

      /*** Compute brick arrays for new dataset ***/

      if( ny_ref == 1 ){

      /*** Just 1 ref vector --> load values directly into dataset ***/

         /*--- get alpha (coef) into vval,
               find max value, scale into brick array ---*/

         PCOR_get_coef( pc_ref[0] , pc_vc[0] , vval ) ;

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(vval[iv]) > topval ) topval = fabs(vval[iv]) ;

         bar = DSET_ARRAY( new_dset , FUNC_ival_fim[FUNC_COR_TYPE] ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * vval[iv] + 0.499) ;

            stataux[0] = 1.0/topval ;
         } else {
            stataux[0] = 0.0 ;
         }

         /*--- get correlation coefficient (pcor) into vval,
               scale into brick array (with fixed scaling factor) ---*/

         PCOR_get_pcor( pc_ref[0] , pc_vc[0] , vval ) ;

         bar = DSET_ARRAY( new_dset , FUNC_ival_thr[FUNC_COR_TYPE] ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         for( iv=0 ; iv < nvox ; iv++ )
            bar[indx[iv]] = (short)(FUNC_COR_SCALE_SHORT * vval[iv] + 0.499) ;

         stataux[1] = 1.0 / FUNC_COR_SCALE_SHORT ;

      } else {

      /*** Multiple references --> find best correlation at each voxel ***/

         /*--- get first ref results into abest and rbest (best so far) ---*/

         PCOR_get_coef( pc_ref[0] , pc_vc[0] , abest ) ;
         PCOR_get_pcor( pc_ref[0] , pc_vc[0] , rbest ) ;

         /*--- for each succeeding ref vector,
               get results into aval and vval,
               if |vval| > |rbest|, then use that result instead ---*/

         for( ivec=1 ; ivec < ny_ref ; ivec++ ){

            PCOR_get_coef( pc_ref[ivec] , pc_vc[ivec] , aval ) ;
            PCOR_get_pcor( pc_ref[ivec] , pc_vc[ivec] , vval ) ;

            for( iv=0 ; iv < nvox ; iv++ ){
               if( fabs(vval[iv]) > fabs(rbest[iv]) ){
                  rbest[iv] = vval[iv] ;
                  abest[iv] = aval[iv] ;
               }
            }
         }

         /*--- at this point, abest and rbest are the best
               results, so scale them into the dataset bricks ---*/

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(abest[iv]) > topval ) topval = fabs(abest[iv]) ;

         bar = DSET_ARRAY( new_dset , FUNC_ival_fim[FUNC_COR_TYPE] ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * abest[iv] + 0.499) ;

            stataux[0] = 1.0/topval ;
         } else {
            stataux[0] = 0.0 ;
         }

         bar = DSET_ARRAY( new_dset , FUNC_ival_thr[FUNC_COR_TYPE] ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         for( iv=0 ; iv < nvox ; iv++ )
            bar[indx[iv]] = (short)(FUNC_COR_SCALE_SHORT * rbest[iv] + 0.499) ;

         stataux[1] = 1.0 / FUNC_COR_SCALE_SHORT ;
      }

      /* set scaling factors for each sub-brick */

      (void) EDIT_dset_items( new_dset, ADN_brick_fac, stataux, ADN_none ) ;
      return 1 ;
   }

   /*********************************************************/
   /***** at this point, must process time point # mode *****/

   if( mode < it1 ) return 1 ;  /* nothing to do before first time */

   /*---- at first time, find values above threshold to fim:
            find the mean of the first array,
            compute the threshold (fthr) from it,
            make indx[i] be the 3D index of the i-th voxel above threshold ----*/

   /* be sure dset_time has enough sub-bricks for this time point   30 Oct 2003 [rickr] */
   if ( (DSET_NVALS(dset_time) <= mode) || (DSET_ARRAY(dset_time,mode) == NULL) )
      return 1;

#if 0                           /* change to first_pass test */
   if( mode == it1 )
#endif

   /* check for first fim computation with this dataset            30 Oct 2003 [rickr] */
   if( first_pass == 1 ){

      switch( dtyp ){  /* process each datum type separately */

         case MRI_short:{
            short * dar = (short *) DSET_ARRAY(dset_time,it1) ;
            for( iv=0,fthr=0.0 ; iv < nxyz ; iv++ ) fthr += abs(dar[iv]) ;
            fthr = FIM_THR * fthr / nxyz ;
            for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
               if( abs(dar[iv]) > fthr ) nvox++ ;
            indx = (int *) malloc( sizeof(int) * nvox ) ;
            if( indx == NULL ){
               fprintf(stderr,"RTfim: indx malloc failure!\a\n") ;
               EXIT(1) ;
            }
            for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
               if( abs(dar[iv]) > fthr ) indx[nvox++] = iv ;
         }
         break ;

         case MRI_float:{
            float * dar = (float *) DSET_ARRAY(dset_time,it1) ;
            for( iv=0,fthr=0.0 ; iv < nxyz ; iv++ ) fthr += fabs(dar[iv]) ;
            fthr = FIM_THR * fthr / nxyz ;
            for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
               if( fabs(dar[iv]) > fthr ) nvox++ ;
            indx = (int *) malloc( sizeof(int) * nvox ) ;
            if( indx == NULL ){
              fprintf(stderr,"RTfim: indx malloc failure!\a\n") ;
              EXIT(1) ;
            }
            for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
               if( fabs(dar[iv]) > fthr ) indx[nvox++] = iv ;
         }
         break ;

         case MRI_byte:{
            byte * dar = (byte *) DSET_ARRAY(dset_time,it1) ;
            for( iv=0,fthr=0.0 ; iv < nxyz ; iv++ ) fthr += dar[iv] ;
            fthr = FIM_THR * fthr / nxyz ;
            for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
               if( dar[iv] > fthr ) nvox++ ;
            indx = (int *) malloc( sizeof(int) * nvox ) ;
            if( indx == NULL ){
              fprintf(stderr,"RTfim: indx malloc failure!\a\n") ;
              EXIT(1) ;
            }
            for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
               if( dar[iv] > fthr ) indx[nvox++] = iv ;
         }
         break ;
      }

      /** allocate space for voxel values that are above the threshold **/
      /** (nvox = # voxels above threshold;  nxyz = total # voxels)    **/

      if( verbose > 1 )
         fprintf(stderr,"RTfim: %d/%d voxels being FIMmed.\n",nvox,nxyz) ;
      VMCHECK ;

      vval = (float *) malloc( sizeof(float) * nvox) ;
      if( vval == NULL ){
        fprintf(stderr,"RTfim: vval malloc failure!\a\n") ;
        EXIT(1) ;
      }

      /** allocate extra space for comparing results from multiple ref vectors **/

      if( ny_ref > 1 ){
         aval  = (float *) malloc( sizeof(float) * nvox) ;
         rbest = (float *) malloc( sizeof(float) * nvox) ;
         abest = (float *) malloc( sizeof(float) * nvox) ;
         if( aval==NULL || rbest==NULL || abest==NULL ){
           fprintf(stderr,"RTfim: abest malloc failure!\a\n") ;
           EXIT(1) ;
         }
      } else {
         aval = rbest = abest = NULL ;
      }

      /*--- initialize recursive updates ---*/

      pc_ref = (PCOR_references **) malloc( sizeof(PCOR_references *) * ny_ref ) ;
      pc_vc  = (PCOR_voxel_corr **) malloc( sizeof(PCOR_voxel_corr *) * ny_ref ) ;

      if( pc_ref == NULL || pc_vc == NULL ){
        fprintf(stderr,"RTfim: can't malloc recursion space!\a\n") ;
        EXIT(1) ;
      }

      for( ivec=0 ; ivec < ny_ref ; ivec++ ){
         pc_ref[ivec] = new_PCOR_references( fim_nref ) ;
         pc_vc[ivec]  = new_PCOR_voxel_corr( nvox , fim_nref ) ;
         if( pc_ref[ivec] == NULL || pc_vc[ivec] == NULL ){
           fprintf(stderr,"RTfim: can't malloc refs and corr!\a\n") ;
           EXIT(1) ;
         }
      }

      /*--- Make a new dataset to hold the output ---*/

      rtin->func_dset = new_dset = EDIT_empty_copy( dset_time ) ;
      if( new_dset == NULL ){
        fprintf(stderr,"RTfim: can't create empty dataset!\a\n") ;
        EXIT(1) ;
      }
      tross_Append_History( new_dset , "plug_realtime: FIM" ) ;

      it = EDIT_dset_items( new_dset ,
                               ADN_prefix      , new_prefix ,
                               ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                               ADN_type        , ISHEAD(dset_time)
                                                 ? HEAD_FUNC_TYPE : GEN_FUNC_TYPE ,
                               ADN_func_type   , FUNC_COR_TYPE ,
                               ADN_nvals       , FUNC_nvals[FUNC_COR_TYPE] ,
                               ADN_datum_all   , MRI_short ,
                               ADN_ntt         , 0 ,
                            ADN_none ) ;
      /* and set the mode based on what was passed */
      if( rtin->storage_mode )
         new_dset->dblk->diskptr->storage_mode = rtin->storage_mode;

      if( it > 0 )
         fprintf(stderr,"RTfim: WARNING -- %d errors in dataset creation!\a\n",it) ;

      /** attach bricks to this dataset **/

      for( iv=0 ; iv < new_dset->dblk->nvals ; iv++ ){
         ptr = malloc( DSET_BRICK_BYTES(new_dset,iv) ) ;
         if( ptr == NULL ){
           fprintf(stderr,"RTfim: can't malloc dataset bricks!\a\n") ;
           EXIT(1) ;
         }
         mri_fix_data_pointer( ptr ,  DSET_BRICK(new_dset,iv) ) ;

         sprintf(strbuf,"#%d",iv) ;              /* 03 Jun 1999 */
         EDIT_BRICK_LABEL(new_dset,iv,strbuf) ;  /* add a label to each brick */
      }

      DSET_lock( rtin->func_dset ) ; /* 20 Mar 1998 */

   }  /*---- end of initializing at first important time point ----*/

   /*---- do recursive update for this time point ----*/

   /* process as loop, in case this is the first time (and we need to backtrack) */
   /*                                                        30 Oct 2003 [rickr] */
   if ( first_pass == 1 )
   {
      first_pass  = 0;
      start_index = it1;      /* start from timepoint of first FIM computation */
   }
   else
      start_index = mode;     /* if not first pass, the "loop" will be this one iteration */

   for ( it = start_index; it <= mode; it++ )
   {
      nnow = 0 ;     /* number of refs used this time */

      if( it >= nx_ref ) return 1 ;  /* can't update without references! */

      for( ivec=0 ; ivec < ny_ref ; ivec++ ){           /* for each ideal time series */
         tsar = MRI_FLOAT_PTR(ref_ts) + (ivec*nx_ref) ;
         if( tsar[it] >= WAY_BIG ) continue ;           /* skip this time series */

         ref_vec[0] = 1.0 ;         /* we always supply ort for constant */
         for( ip=1 ; ip <= polort ; ip++ )              /* 02 Jun 1999:    */
            ref_vec[ip] = ref_vec[ip-1] * ((float)it) ; /* and polynomials */

         if( internal_ort ){
            ref_vec[ip] = tsar[it] ;                    /* ideal */
         } else {
            for( iv=0 ; iv < ny_ort ; iv++ )            /* any other orts? */
               ref_vec[iv+ip] = (it < nx_ort) ? ortar[it+iv*nx_ort]
                                              : 0.0 ;

            ref_vec[ny_ort+ip] = tsar[it] ;             /* ideal */
         }

         update_PCOR_references( ref_vec , pc_ref[ivec] ) ;  /* Choleski refs */

         /* load data from dataset into local float array vval */

         switch( dtyp ){
            case MRI_short:{
               short * dar = (short *) DSET_ARRAY(dset_time,it) ;
               for( iv=0 ; iv < nvox ; iv++ ) vval[iv] = (float) dar[indx[iv]] ;
            }
            break ;

            case MRI_float:{
               float * dar = (float *) DSET_ARRAY(dset_time,it) ;
               for( iv=0 ; iv < nvox ; iv++ ) vval[iv] = (float) dar[indx[iv]] ;
            }
            break ;

            case MRI_byte:{
               byte * dar = (byte *) DSET_ARRAY(dset_time,it) ;
               for( iv=0 ; iv < nvox ; iv++ ) vval[iv] = (float) dar[indx[iv]] ;
            }
            break ;
         }

         PCOR_update_float( vval , pc_ref[ivec] , pc_vc[ivec] ) ;  /* Choleski data */
         nnow++ ;

      }
      if( nnow > 0 ) nupdt++ ;  /* at least one ideal vector was used */
   }

   return 1 ;
}

/* for storing a list of strings (e.g. DRIVE_WAIT commands) */
static int add_to_rt_slist( rt_string_list * slist, char * str )
{
    if( !slist || !str ) return 1;

    /* add a pointer to the array */
    slist->len++;
    slist->list = (char **)realloc(slist->list, slist->len*sizeof(char *));
    if( !slist->list ) {
        fprintf(stderr,"** failed to realloc slist of %d items\n", slist->len);
        slist->len = 0;
        return 1;
    }

    slist->list[slist->len-1] = strdup(str);
    if( !slist->list[slist->len-1] ) {
        fprintf(stderr,"** failed to add string '%s' to slist\n", str);
        free_rt_string_list(slist);
        return 1;
    }

    return 0;
}

static int free_rt_string_list( rt_string_list * slist )
{
    int c;

    if( !slist ) return 1;
    if( !slist->list || slist->len <= 0 ) return 0;

    for( c = 0; c < slist->len; c++ )
        if( slist->list[c] ) free( slist->list[c] );
    free(slist->list);
    slist->list = NULL;
    slist->len = 0;

    return 0;
}

/* execute any pending DRIVE_WAIT commands   22 Aug 2008 [rickr] */
static int rt_run_drive_wait_commands( rt_string_list * slist )
{
   char * cmd;
   int    c;

   if( !slist || !slist->list || slist->len <= 0 ) return 0 ;

   for( c = 0; c < drive_wait_list.len; c++ ) {
      cmd = drive_wait_list.list[c];
      if(verbose > 1) fprintf(stderr,"RT: executing DRIVE_WAIT cmd: %s\n",cmd);
      if( AFNI_driver( cmd ) < 0 )
         fprintf(stderr,"RT: **FAILED DRIVE_WAIT** : %s \n", cmd) ;
   }

   free_rt_string_list(&drive_wait_list);  /* clear old commands */

   return 0;
}

/*-------------------------------------------------------------------------*/
/* Test realtime_callback function [01 Jun 2009] */

void RT_test_callback(void *junk)
{
   RT_status *rts = GLOBAL_library.realtime_status ;
   int cc , nval,nbr ;

   if( rts == NULL ){ ERROR_message("bad call to RT_test_callback"); return; }

   INFO_message("RT_test_callback: numchan=%d status=%d numdset=%d",
                rts->numchan , rts->status , rts->numdset ) ;

   for( cc=0 ; cc < rts->numdset ; cc++ ){
     if( !ISVALID_DSET(rts->dset[cc]) ){
       ININFO_message(" dset[%d] invalid!",cc) ;
     } else {
       nval = DSET_NVALS(rts->dset[cc]) ;
       nbr  = THD_count_databricks(rts->dset[cc]->dblk) ;
       ININFO_message(" dset[%d] '%s': nvals=%d  nbr=%d",
                      cc , DSET_HEADNAME(rts->dset[cc]) , nval,nbr ) ;
     }
   }

   return ;
}

/*-------------------------------------------------------------------------*/
/* Function to merge a collection of datasets, at sub-brick #iv. */
/*
 *   dlist : list of dataset indices to apply in merge, if set
 *           - created via MCW_get_intlist, so dlist[0] is the length
 */

/* was mrgim = RT_mergerize(rtin->num_chan, rtin->dset, iv, rtin->chan_list);
MRI_IMAGE * RT_mergerize(int nds , THD_3dim_dataset **ds , int iv, int * dlist)
   but pass rtin instead of elements                     12 Mar 2015 [rickr] */

MRI_IMAGE * RT_mergerize(RT_input * rtin, int iv)
{
   THD_3dim_dataset **ds ;
   int * dlist = rtin->chan_list ;
   int nds = rtin->num_chan ;

   float *far[MAX_CHAN] ; complex *car[MAX_CHAN] ;
   int cc , nvox , idatum , ii , ndsets=rtin->num_chan ;
   MRI_IMAGE *mrgim ;
   float *fmar=NULL , *ftar ; complex *cmar=NULL , *ctar ;
   short *sar[MAX_CHAN] , *star ;  /* OPT_COMB or T2STAREST */

   ds = rtin->dset ;

   if( nds <= 1 || ds == NULL || !ISVALID_DSET(ds[0]) ) return NULL ;
   if( iv < 0 || iv >= DSET_NVALS(ds[0]) )              return NULL ;

   /* maybe apply merge dataset list          13 Jul 2010 [rickr] */
   if( dlist && dlist[0] > 0 ) {
      ndsets = dlist[0];

      if( ndsets > nds ) {
         fprintf(stderr,"** RT_mergerize: dlist longer than num channels!\n");
         return NULL;
      }

      cc = 0;  /* success status */
      for( ii=1 ; ii <= ndsets ; ii++ )
         if( dlist[ii] < 0 || dlist[ii] >= nds ) {
            fprintf(stderr,
                    "** RT_mergerize: bad channel in list (%d chan): #%d = %d\n",
                    nds, ii, dlist[ii]);
            cc = 1;
         }
      if( cc > 0 ) return NULL;
   }

   /* get pointers to input data arrays */

   idatum = DSET_BRICK_TYPE(ds[0],iv) ;
   switch( idatum ){
     default: return NULL ; /* should never happen */

     case MRI_short:
       for( cc=0 ; cc < ndsets ; cc++ )
          /* if dlist is set, get the index from there */
          sar[cc] = dlist ? DSET_ARRAY(ds[dlist[cc+1]],iv)
                          : DSET_ARRAY(ds[cc],iv) ;
     break ;

     case MRI_float:
       for( cc=0 ; cc < ndsets ; cc++ )
          /* if dlist is set, get the index from there */
          far[cc] = dlist ? DSET_ARRAY(ds[dlist[cc+1]],iv)
                          : DSET_ARRAY(ds[cc],iv) ;
     break ;

     case MRI_complex:
       for( cc=0 ; cc < ndsets ; cc++ )
          /* if dlist is set, get the index from there */
          car[cc] = dlist ? DSET_ARRAY(ds[dlist[cc+1]],iv)
                          : DSET_ARRAY(ds[cc],iv) ;
     break ;
   }

   /* make output image/array */

   nvox  = DSET_NVOX(ds[0]) ;
   mrgim = mri_new_conforming( DSET_BRICK(ds[0],iv) , RT_chmrg_datum ) ;
   if( mrgim == NULL ) return NULL ;  /* should never happen */

   /* get pointer to output array */

   switch( RT_chmrg_datum ){
     default: mri_free(mrgim) ; return NULL ; /* should never happen */
     case MRI_float:   fmar = MRI_FLOAT_PTR  (mrgim) ; break ;
     case MRI_complex: cmar = MRI_COMPLEX_PTR(mrgim) ; break ;
   }

   /*** do the mergerizing ***/

   switch( RT_chmrg_mode ){

     default: mri_free(mrgim) ; return NULL ; /* should never happen */

     /*** sum of absolute values ***/

     case RT_CHMER_L1NORM:  /* output datum is always float */
       switch( idatum ){
         case MRI_float:
           for( cc=0 ; cc < ndsets ; cc++ ){
             ftar = far[cc] ;
             for( ii=0 ; ii < nvox ; ii++ ) fmar[ii] += fabsf(ftar[ii]) ;
           }
         break ;

         case MRI_complex:
           for( cc=0 ; cc < ndsets ; cc++ ){
             ctar = car[cc] ;
             for( ii=0 ; ii < nvox ; ii++ ) fmar[ii] += sqrtf(CSQR(ctar[ii])) ;
           }
         break ;
       }
     break ; /* done with L1 */

     /*** sqrt of sum of squares ***/

     case RT_CHMER_L2NORM:  /* output datum is always float */
       switch( idatum ){
         case MRI_float:
           for( cc=0 ; cc < ndsets ; cc++ ){
             ftar = far[cc] ;
             for( ii=0 ; ii < nvox ; ii++ ) fmar[ii] += SQR(ftar[ii]) ;
           }
         break ;

         case MRI_complex:
           for( cc=0 ; cc < ndsets ; cc++ ){
             ctar = car[cc] ;
             for( ii=0 ; ii < nvox ; ii++ ) fmar[ii] += CSQR(ctar[ii]) ;
           }
         break ;
       }
       for( ii=0 ; ii < nvox ; ii++ ) fmar[ii] = sqrtf(fmar[ii]) ;
     break ; /* done with L2 */

     /* Begin FMRIF changes for RT T2* estimates - VR */

     case RT_CHMER_T2STAREST:  /* output datum is always float */
        if( idatum != MRI_float )
           fprintf(stderr,"** type of T2star est dataset must be float\n");
        else {
           float sumX, sumY, sumX2, sumXY, diffSumX2, logY;

           sumX = sumX2 = 0.0, diffSumX2 = 1.0;
           for (cc=0 ; cc < ndsets ; cc++)
           {
              sumX  += (rtin->TE[cc]);
              sumX2 += (rtin->TE[cc] * rtin->TE[cc]);
           }
           diffSumX2 = ((ndsets * sumX2) - (sumX * sumX));

           for (ii=0 ; ii < nvox ; ii++)
           {
              sumY = sumXY = 0.0;
              for (cc=0 ; cc < ndsets ; cc++)
              {
                 ftar   = (far[cc]);

                 if (ftar[ii] > 0.5)
                    logY   = log (ftar[ii]);
                 else
                    logY   = 0.0;

                 sumY  += (logY);
                 sumXY += (logY * rtin->TE[cc]);
              }

              /* Regression Formula:                                          *
               *                                                              *
               * Slope     = ((N * sum (X * Y)) - (sum (X) * sum (Y))) /      *
               *             ((N * sum (X * X)) - ((sum (X)^2)))              *
               *                                                              *
               * Intercept = (sum(Y) - Slope * sum(X)) / N                    *
               *                                                              *
               * For this simple model, slope = -1 / T2*, so T2* = -1 / slope *
               *                                                              *
               * Statement for slope computed via linear regression is:       *
               *                                                              *
               *   fmar[ii] = ((ndsets * sumXY) - (sumX * sumY)) / diffSumX2; *
               *                                                              *
               * For T2* value directly from linear regression coefficients:  */
               if (((sumX * sumY) - (ndsets * sumXY)) > 0.001)
                  fmar[ii] = diffSumX2 / ((sumX * sumY) - (ndsets * sumXY));
               else
                  fmar[ii] = 0.0;
           }
       }
     break ; /* done with RT_CHMER_T2STAREST */

     /* End FMRIF changes for RT T2* estimates - VR */

     /* Begin FMRIF changes for RT Optimal combined echo data - VR */

     case RT_CHMER_OPT_COMB :  /* output datum is always float */
        {
           float *t2star_ref, sumTEsByExpTEs;

           DSET_load(rtin->t2star_ref_dset);
           t2star_ref = DSET_ARRAY(rtin->t2star_ref_dset, 0);

           /* warn on bad (probably unset) TEs */
           for (cc=0 ; cc < ndsets ; cc++)
              if( rtin->TE[cc] <= 0.0 ) {
                 fprintf(stderr,"** RT OC: bad TE[%d] = %f\n",
                         cc, rtin->TE[cc]);
              }

           for (ii=0 ; ii < nvox ; ii++)
           {
               /* def make_optcom(data,t2s,tes):

                   """
                   Generates the optimally combined time series. 

                   Parameters:
                   -----------
                   data: this is the original ME dataset with the mean in a (Nv,Ne,Nt) array.
                   t2s:  this is the static T2s map in a (Nv,) array.
                   tes:  echo times in a (Ne,) array.

                   Returns:
                   --------
                   octs: optimally combined time series in a (Nv,Nt) array.    
                   """

                   Nv,Ne,Nt = data.shape
                   ft2s  = t2s[:,np.newaxis]
                   alpha = tes * np.exp(-tes /ft2s)
                   alpha = np.tile(alpha[:,:,np.newaxis],(1,1,Nt))
                   octs  = np.average(data,axis = 1,weights=alpha)
                   return octs

               weight (T2*)[i] = TE[i] * exp (-TE[i]/T2*(fit)) /
                                 Sum ( TE[i] * exp(-TE[i]/T2*(fit)) )

               */

             if( t2star_ref[ii] != 0.0 ) {
               sumTEsByExpTEs = 0.0;
               for (cc=0 ; cc < ndsets ; cc++)
               {
                  sumTEsByExpTEs += ( rtin->TE[cc] 
                                 * exp (-1.0*rtin->TE[cc] / t2star_ref[ii]) );
               }

               for (cc=0 ; cc < ndsets ; cc++)
               {
                  if (idatum == MRI_short) {
                     star   = (sar[cc]);
                     fmar[ii] += (star[ii] * rtin->TE[cc] 
                              * exp (-1.0 * rtin->TE[cc] / t2star_ref[ii]));
                  }
                  else { /* idatum == MRI_float */
                     ftar   = (far[cc]);
                     fmar[ii] += (ftar[ii] * rtin->TE[cc] 
                              * exp (-1.0 * rtin->TE[cc] / t2star_ref[ii]));
                  }
               }

               /* rcr - fix (Vinai, is 0.001 reasonable?) */
               if( sumTEsByExpTEs > 0.001 )
                  fmar[ii] = fmar[ii] / sumTEsByExpTEs;
               else
                  fmar[ii] = 0.0;
             } else {
               fmar[ii] = 0.0;
             }
           }
       }
     break ; /* done with RT_CHMER_OPT_COMB */

     /* End FMRIF changes for RT Optimal combined echo data - VR */

     /*** sum of input values ***/

     fflush (stdout);

     case RT_CHMER_SUM:  /* output datum is same as input datum */
       switch( idatum ){
         case MRI_float:
           for( cc=0 ; cc < ndsets ; cc++ ){
             ftar = far[cc] ;
             for( ii=0 ; ii < nvox ; ii++ ) fmar[ii] += ftar[ii] ;
           }
         break ;

         case MRI_complex:
           for( cc=0 ; cc < ndsets ; cc++ ){
             ctar = car[cc] ;
             for( ii=0 ; ii < nvox ; ii++ ){
               cmar[ii].r += ctar[ii].r ; cmar[ii].i += ctar[ii].i ;
             }
           }
         break ;
       }
     break ; /* done with SUM */
   }

   /*** done done done ***/

   return mrgim ;
}

#define RT_LONG_STR_LEN ( 255 )

/* Cameron Craddock Functions to implement realtime detrending */

/* RT_detrend_getenv access the AFNI_REALTIME_DETREND_MODE and 
   AFNI_REALTIME_DETREND_POLORT environment variables and 
   copies the results to rtin */
void RT_detrend_getenv( RT_input * rtin )
{
    /* for assesing the environment variables */
    char * ept;

    /* temporary integers and loop counters */
    int ii = 0;
             
    /* string for error reporting */
    char rt_errorString [RT_LONG_STR_LEN];

    /* for AFNI error checking */
    ENTRY( "RT_detrend_getenv" );

    /* get the environment variables */
    ept = getenv("AFNI_REALTIME_DETREND_MODE");
    if( ept != NULL )
    {
        int ii = (int) rint(strtod(ept,NULL)) ;
        /* we put detrend_mode into rtin, so it
           can be used outside of this function */
        if( ii >= 0 && ii <= 32 )
        {
            rtin->detrend_mode = ii;
        }
        else
        {
            snprintf( rt_errorString, RT_LONG_STR_LEN, 
              "RT Detrend: Invalid detrend_mode %d Turning off detrend.",ii);
            fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
            PLUTO_popup_transient( plint, rt_errorString );
            rtin->detrend_mode = RT_DETREND_NONE;
            rtin->detrend_polort = -1;
        }
    }
    
    ept = getenv("AFNI_REALTIME_DETREND_POLORT");
    if( ept != NULL )
    {
        int ii = (int) rint(strtod(ept,NULL));
        if( ii >= -1 && ii <= 99 )
        {
            rtin->detrend_polort = ii;
        }
        else
        {
            snprintf( rt_errorString, RT_LONG_STR_LEN, 
              "RT Detrend: Invalid detrend_polort %d Turning off detrend.",ii);
            fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
            PLUTO_popup_transient( plint, rt_errorString );
            rtin->detrend_mode = RT_DETREND_NONE;
            rtin->detrend_polort = -1;
        }
    }

    /* make sure polort flag is set if specified */
    if ( rtin->detrend_polort > -1 )
        rtin->detrend_mode = rtin->detrend_mode | RT_DETREND_POLORT;

    ept = getenv("AFNI_REALTIME_DETREND_FWHM");
    if( ept != NULL )
    {
        fprintf( stderr, "## PARSE RT FWHM:%s\n", ept);
        float ff = (float)strtod(ept,NULL);
        if( ff >= -1 )
        {
            rtin->detrend_fwhm = ff;
        }
        else
        {
            snprintf( rt_errorString, RT_LONG_STR_LEN, 
              "RT Detrend: Invalid detrend_fwhm %g Turning off detrend.",ff);
            fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
            PLUTO_popup_transient( plint, rt_errorString );
            rtin->detrend_mode = RT_DETREND_NONE;
            rtin->detrend_polort = -1;
            rtin->detrend_fwhm = 0.0;
        }
    }

    /* make sure polort flag is set if specified */
    if ( rtin->detrend_fwhm > 0.0 )
        rtin->detrend_mode = rtin->detrend_mode | RT_DETREND_SMOOTH;

    fprintf( stderr,
             "## PARSE RT Detrend: RT Options mode 0x%x polort %d fwhm %g\n",
             rtin->detrend_mode, rtin->detrend_polort, rtin->detrend_fwhm );
    EXRETURN;
}

/* RT_detrend makes use of Bob's recursive regression method
   to orthogonalize the received data to a user specified set
   of regressors. Everything that we need is either in the 
   AFNI_REALTIME_DETREND_MODE or AFNI_REALTIME_DETREND_POLORT
   environment variables or in the RT_input data structure. 
   This code relies heavily on the code found in the RT_fim_recurse
   function. */
void RT_detrend( RT_input * rtin, int mode )
{
    /* these are static variables, and their values persist
       across several call so this function */

    /* flag to indicate whether or not we have been initialized */
    static int detrendIsInitialized = 0;

    /* pointer to access the imaging data */
    static THD_3dim_dataset * dset_time = NULL;
 
    /* data structures for recursive regression */
    static PCOR_references * pc_ref=NULL ;
    static PCOR_voxel_corr * pc_vc=NULL ;

    /* parameters which specify regressors */
    static int    num_motion_ort = 0;
    static int    detrend_num_ort = 0;
    static float* detrend_orts = NULL;

    /* arrays to hold mask, datatype, and temp image data */

    int nvol = 0; /* counts the number of volumes received, 
                     this will be set to either rtin->nvol[0] 
                     or rtin->reg_nvol depending on whether 
                     registration is on or off */
    static int dtype;
    static int nxyz = 0;
    static int nvox = 0;
    static float * vval = NULL; /* hold masked voxel data */
    static float ** fit = NULL; /* hold lsq fit */ 
    static byte * mask = NULL;  /* for holding the mask */
    static int * indx = NULL;
    static float fthr = 0.0;    /* threshold for automasking */

    /* constants for detrending */
    static float fx=-1.0f;
    static float fy=-1.0f;
    static float fz=-1.0f; 
    static int nrep=-1;

    /* file pointer to write out ORTs */
    static char ort_filename[ RT_LONG_STR_LEN ];
    static FILE* ort_fp;

    /* non-static vars, some counters, temp ints, etc */
    int iv = 0; /* voxel counter */
    int it = 0; /* current time point */
    int io = 0; /* ort counter */
    int count = 0;

    /* to hold the estimate of the global mean */
    float global_mean;
    float * dval = NULL;      /* hold detrended data */
    MRI_IMAGE *detim = NULL ; /* temporary output */
   
    /* string for error reporting */
    char rt_errorString [RT_LONG_STR_LEN];

    /* string to extract dset prefix */
    char prefix[THD_MAX_PREFIX];

    /* for AFNI error checking */
    ENTRY( "RT_detrend" ); 

    /* sanity check */
    if (( rtin == NULL ) || 
        ( mode == TELL_FINAL ) || 
        ( rtin->detrend_mode == RT_DETREND_NONE )) 
    {
        if( rtin == NULL )
        {
            snprintf( rt_errorString, RT_LONG_STR_LEN, 
                "RT Detrend: ERROR: rtin == NULL! Turning off detrend.");
        }
        else if ( mode == TELL_FINAL )
        {
            snprintf( rt_errorString, RT_LONG_STR_LEN, 
                "RT Detrend: Acquisition completed. Turning off detrend.");
        }
        else
        {
            snprintf( rt_errorString, RT_LONG_STR_LEN, 
                "RT Detrend: Detrend is off? Turning it off again.");
        }

        fprintf(stderr, "RT_DETREND: %s\n", rt_errorString);
        if (doPopups)
        {  
             PLUTO_popup_transient( plint, rt_errorString );
        }

        /* free allocated memory */ 
        IFree( detrend_orts );
        IFree( vval );
        IFree( mask );
        IFree( indx );

        if( pc_ref != NULL )
        {
            free_PCOR_references(pc_ref);
            pc_ref = NULL;
        }

        if( pc_vc != NULL )
        {
            free_PCOR_voxel_corr(pc_vc);
            pc_ref = NULL;
        }

        for( iv = 0; iv < detrend_num_ort; iv++ )
        {
            IFree( fit[iv] );
        }
        IFree( fit );
        detrend_num_ort = 0;

        /* close file */
        if( ort_fp != NULL )
        {
            fclose( ort_fp );
        }

        /* turn off future detrending */
        rtin->detrend_mode = RT_DETREND_NONE;
        rtin->detrend_polort = -1;

        /* de-initialize */
        detrendIsInitialized = 0;

        /* return */
        EXRETURN;
    }

    /* there is some difference between when data becomes 
       available. If registration is off, we just care that
       at least one image is received. If online motion 
       correction is selected, then we want to make sure
       that at least one image has passed through motion 
       correction. There is some descrepencies between these
       two values depending on whether motion correction
       is performed to the first image of the series 
       being received, or if registering to an external dataset.
       Failure to account for this will result in the automask
       function returning NULL and detrending being turned off.
       This was a segmentation fault, but I modified mri_automask
       to just return NULL instead of giving us the finger */
       
    nvol=rtin->nvol[0];
    if( (rtin->reg_mode == REGMODE_2D_RTIME) || 
        (rtin->reg_mode == REGMODE_3D_RTIME))
    {
        nvol=rtin->reg_nvol;
    }

    /* don't do anything unless we have at least one image */
    if( nvol > 0 )
    {
        /* get the index of the current image */
        it = rtin->nvol[0] - 1;

        /* check to see if we are initialized */
        if(( detrendIsInitialized == 0 ))
        {
            /* get a pointer to the dataset */
            dset_time = rtin->dset[0]; /* if no registration */

            /* now check for use of registered dataset */
            if( (rtin->reg_mode == REGMODE_2D_RTIME) || 
                (rtin->reg_mode == REGMODE_3D_RTIME))
            {
                if( verbose )
                  fprintf( stderr,
                           "RT DETREND: detrending motion corrected data\n" );
                dset_time = rtin->reg_dset;
            }
            else if (( rtin->detrend_mode & RT_DETREND_MOTION ) ||
                     ( rtin->detrend_mode & RT_DETREND_FRISTON ))
            {
                /* motion ort regression specified, but registration
                   not being performed, turn it off */
                snprintf( rt_errorString, RT_LONG_STR_LEN, 
                  "RT Detrend: Motion ort specified, but no RT Motion."
                  " Turning of motion ort.");
                fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                PLUTO_popup_transient( plint, rt_errorString );

                /* turn off motion detrending */
                if ( rtin->detrend_mode & RT_DETREND_FRISTON )
                {
                    rtin->detrend_mode -= RT_DETREND_FRISTON;
                }
                else
                {
                    rtin->detrend_mode -= RT_DETREND_MOTION;
                }
    
                /* return */
                EXRETURN;
            }
            else if( verbose )
               fprintf(stderr,
                       "RT DETREND: detrending non-motion corrected data\n" );

            /* more sanity check, verify data pointer is not NULL */
            if( dset_time == NULL )
            {
                /* dataset is NULL? it shouldn't be */
                snprintf( rt_errorString, RT_LONG_STR_LEN, 
                  "RT Detrend: dset_time == NULL! Turning off detrend.");
                fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                PLUTO_popup_transient( plint, rt_errorString );
                /* turn off future detrending */
                rtin->detrend_mode = RT_DETREND_NONE;
                rtin->detrend_polort = -1;
    
                /* return */
                EXRETURN;
            }

            /* more sanity check, verify image pointer is not NULL */
            if( DSET_BRICK(dset_time,it) == NULL )
            {
                /* dataset is NULL? it shouldn't be */
                snprintf( rt_errorString, RT_LONG_STR_LEN, 
                  "RT Detrend: dset_time[%d] == NULL! Turning off detrend.",it);
                fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                PLUTO_popup_transient( plint, rt_errorString );
                /* turn off future detrending */
                rtin->detrend_mode = RT_DETREND_NONE;
                rtin->detrend_polort = -1;
    
                /* return */
                EXRETURN;
            }


            /* determine the total number of voxels in the image */
            nxyz = dset_time->dblk->diskptr->dimsizes[0] *
                   dset_time->dblk->diskptr->dimsizes[1] *
                   dset_time->dblk->diskptr->dimsizes[2];

            /* determine the dataset type */
            dtype = DSET_BRICK_TYPE(dset_time,0);
            if( !AFNI_GOOD_FUNC_DTYPE(dtype) )
            {
                /* bad dtype?it shouldn't be */
                snprintf( rt_errorString, RT_LONG_STR_LEN, 
                  "RT Detrend: Bad dtype %d! Turning off detrend.",dtype);
                fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                PLUTO_popup_transient( plint, rt_errorString );
                /* turn off future detrending */
                rtin->detrend_mode = RT_DETREND_NONE;
                rtin->detrend_polort = -1;
    
                /* return */
                EXRETURN;
            }

            /* perform masking using mri_automask_image  */
            if(( mask = mri_automask_image(DSET_BRICK(dset_time,it))) == NULL )
            {
                snprintf( rt_errorString, RT_LONG_STR_LEN, 
                  "RT Detrend: Automask failed! Turning off detrend.");
                fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                PLUTO_popup_transient( plint, rt_errorString );
                /* turn off future detrending */
                rtin->detrend_mode = RT_DETREND_NONE;
                rtin->detrend_polort = -1;

                /* return */
                EXRETURN;
            }

            for( iv=0, nvox=0; iv < nxyz; iv++ )
            {
                if( mask[iv] != 0 ) nvox++;
            }
 
            indx = (int *) malloc( sizeof(int) * nvox ) ;
            if( indx == NULL )
            {
                fprintf(stderr,"RT detrend: indx malloc failure!\a\n");
                /* turn off future detrending */
                rtin->detrend_mode = RT_DETREND_NONE;
                rtin->detrend_polort = -1;
  
                /* return */
                EXRETURN;
            }

            for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
            {
                 if( mask[iv] != 0 ) indx[nvox++] = iv;
            }


            if( verbose )
                fprintf(stderr, "RT_DETREND: %d voxels of %d (%f percent)"
                                " remain after applying automask\n",
                                nvox, nxyz, (float)nvox/(float)nxyz );

            /* make sure we have some voxels left */
            if( nvox < 1 )
            {
                fprintf(stderr,
                        "RT detrend: no voxels survived automask (%d)!\a\n",
                        nvox) ;
                /* turn off future detrending */
                rtin->detrend_mode = RT_DETREND_NONE;
                rtin->detrend_polort = -1;

                /* free allocated memory */
                IFree( mask );
                IFree( indx );
    
                /* return */
                EXRETURN;
            }

            /* allocate temp space to hold just in-brain voxels */
            if(( vval = (float*)malloc( nvox * sizeof(float))) == NULL )
            {
                fprintf(stderr,
                        "RT detrend: ERROR! Could not allocate vval!\a\n");
                /* turn off future detrending */
                rtin->detrend_mode = RT_DETREND_NONE;
                rtin->detrend_polort = -1;

                /* free allocated memory */
                IFree( mask );
                IFree( indx );
    
                /* return */
                EXRETURN;
            }

            /* make sure that some ORTs were specified and not just smoothing */
            if ( rtin->detrend_mode & ~RT_DETREND_SMOOTH )
            {

                /* setup for ORT removal */ 

                /* open ort file, go ahead and do it here, so that we can 
                   create a header for the ort file */
                FILENAME_TO_PREFIX( DSET_BRIKNAME( rtin->detrend_dset ), prefix );
                if (prefix[0] == '\0')
                   strncpy(prefix, DSET_BRIKNAME( rtin->detrend_dset ),
                                   THD_MAX_PREFIX );
                snprintf( ort_filename, RT_LONG_STR_LEN, "%s_orts.1D", prefix );
          
                if((ort_fp = fopen(ort_filename, "w")) == NULL) 
                {
                   fprintf(stderr,
                           "RT_DETREND: ERROR Could not open ORT file %s\n",
                           ort_filename );
                   /* just pretend that we never even tried to open the file */
                }
                else
                {
                    fprintf(stderr, "RT_DETREND: Opened %s for writing orts\n",
                            ort_filename );
    
                    /* print comment to the ort file */
                    fprintf( ort_fp, "# rt_detrend mode 0x%x, polort %d\n",
                             rtin->detrend_mode, 
                        rtin->detrend_polort );
                }
    
                /* calculate the number of orts */
                detrend_num_ort = 0;
                if( rtin->detrend_polort > -1 )
                {
                    /* add an extra for the constant */
                    detrend_num_ort += rtin->detrend_polort + 1;
    
                    /* write the polort info to the ort file */
                    if ( ort_fp != NULL )
                    {
                        fprintf(ort_fp,"#POLORT#0");
                        for( io=1;io<rtin->detrend_polort+1;io++)
                        {
                            fprintf( ort_fp, "\tPOLORT#%d", io );
                        }
                    }
                }
    
                /* motion, could be 2D or 3D */
                num_motion_ort = 0;
                if( REG_IS_2D( rtin->reg_mode ))
                {
                    if( rtin->detrend_mode & RT_DETREND_FRISTON )
                    {
                        num_motion_ort = 12;
                    }
                    else if( rtin->detrend_mode & RT_DETREND_MOTION )
                    {
                        num_motion_ort = 3;
                    }
                }
                else if( REG_IS_3D( rtin->reg_mode ))
                {
                    if( rtin->detrend_mode & RT_DETREND_FRISTON )
                    {
                        num_motion_ort = 24;
                    }
                    else if (rtin->detrend_mode & RT_DETREND_MOTION )
                    {
                        num_motion_ort = 6;
                    }
                }
                detrend_num_ort += num_motion_ort;
    
                /* write the motion ort info to the ort file */
                if ( num_motion_ort > 0 )
                {
                    if ( ort_fp != NULL )
                    {
                        if( detrend_num_ort == num_motion_ort )
                        {
                            fprintf(ort_fp,"#MOTION#0");
                            for( io=1;io<num_motion_ort;io++)
                            {
                                fprintf( ort_fp, "\tMOTION#%d", io );
                            }
                        }
                        else
                        {
                            for( io=0;io<num_motion_ort;io++)
                            {
                                fprintf( ort_fp, "\tMOTION#%d", io );
                            }
                        }
                    }
                }
    
                /* global mean */
                if( rtin->detrend_mode & RT_DETREND_GM )
                {
                    detrend_num_ort++;
    
                    /* write the global mask info to the ort file */
                    if ( ort_fp != NULL )
                    {
                        if( detrend_num_ort == 1 )
                        {
                            fprintf(ort_fp,"#GM");
                        }
                        else
                        {
                            fprintf(ort_fp,"\tGM");
                        }
                    }
                }
    
                /* MASK ROI time courses */
                if( rtin->detrend_mode & RT_DETREND_MASK )
                {
                    /* check to see if any masks ROIs exist */ 
                    if( rtin->mask_nvals > 0 )
                    {
                        detrend_num_ort+=rtin->mask_nvals;
                    }
                    else
                    {
                        /* if not turn off this option */
                        rtin->detrend_mode -= RT_DETREND_MASK;
                    }
    
                    /* write the mask orts to the ort file */
                    if ( ort_fp != NULL )
                    {
                        if( detrend_num_ort == rtin->mask_nvals )
                        {
                            fprintf(ort_fp,"#MASK#0");
                            for( io=1;io<rtin->mask_nvals;io++)
                            {
                                fprintf( ort_fp, "\tMASK#%d", io );
                            }
                        }
                        else
                        {
                            for( io=0;io<rtin->mask_nvals;io++)
                            {
                                fprintf( ort_fp, "\tMASK#%d", io );
                            }
                        }
                    }
                }
    
                /* finish writing the header to the ort file */
                if ( ort_fp != NULL )
                {
                    fprintf( ort_fp, "\n" );
                }
   
                /* make sure we calculated some (>0) orts */ 
                if( detrend_num_ort == 0 )
                {
                    /* print an error and turn off future detrending */
                    snprintf( rt_errorString, RT_LONG_STR_LEN, 
                      "RT Detrend: ORT detrending flags specified, but"
                      " no orts calculated.");
                    fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                    PLUTO_popup_transient( plint, rt_errorString );
    
                    /* free allocated memory */ 
                    IFree( indx );
                    IFree( mask );
                    IFree( vval );
    
                    /* close file */
                    if( ort_fp != NULL )
                    {
                        fclose( ort_fp );
                    }
    
                    /* turn off future detrending */
                    rtin->detrend_mode = RT_DETREND_NONE;
                    rtin->detrend_polort = -1;
    
                    /* return */
                    EXRETURN;
                }

                /* now we have the number of orts, so we can allocate
                   an array to hold them, since we are using recursive
                   regression, we only need one timepoint */
                detrend_orts = (float*)malloc(detrend_num_ort * sizeof(float));
                if( detrend_orts == NULL )
                {
                    /* couldn't get memory, so print an error and turn off
                       future detrending */
                    snprintf( rt_errorString, RT_LONG_STR_LEN, 
                      "RT Detrend: Could not allocate ort array!"
                      " Turning off detrend.");
                    fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                    PLUTO_popup_transient( plint, rt_errorString );
    
                    /* free allocated memory */ 
                    IFree( indx );
                    IFree( mask );
                    IFree( vval );
    
                    /* close file */
                    if( ort_fp != NULL )
                    {
                        fclose( ort_fp );
                    }
    
                    /* turn off future detrending */
                    rtin->detrend_mode = RT_DETREND_NONE;
                    rtin->detrend_polort = -1;
    
                    /* return */
                    EXRETURN;
                }
    
                /* allocate PCOR_references structure */
                /* make sure we aren't double allocating for some reason */
                if(( pc_ref = new_PCOR_references( detrend_num_ort )) == NULL )
                {
                    /* no memory, print an error and turn off detrending */
                    snprintf( rt_errorString, RT_LONG_STR_LEN, 
                      "RT Detrend: Could not allocate PCOR references!"
                      " Turning off detrend.");
                    fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                    PLUTO_popup_transient( plint, rt_errorString );
    
                    /* free allocated memory */ 
                    IFree(detrend_orts);
                    IFree( mask );
                    IFree( indx );
                    IFree( vval );
    
                    /* close file */
                    if( ort_fp != NULL )
                    {
                        fclose( ort_fp );
                    }
    
                    /* turn off future detrending */
                    rtin->detrend_mode = RT_DETREND_NONE;
                    rtin->detrend_polort = -1;
    
                    /* return */
                    EXRETURN;
                }
    
                /* allocate PCOR_voxel_cor structure */
                pc_vc = new_PCOR_voxel_corr( nvox, detrend_num_ort );
                if( pc_vc == NULL )
                {
                    /* no memory, print an error and turn off detrending */
                    snprintf( rt_errorString, RT_LONG_STR_LEN, 
                      "RT Detrend: Could not allocate PCOR voxel corr!"
                      " Turning off detrend.");
                    fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                    PLUTO_popup_transient( plint, rt_errorString );
    
                    IFree( detrend_orts );
                    IFree( mask );
                    IFree( indx );
                    IFree( vval );
    
                    if( pc_ref != NULL )
                    {
                        free_PCOR_references(pc_ref);
                        pc_ref = NULL;
                    }
    
                    /* close file */
                    if( ort_fp != NULL )
                    {
                        fclose( ort_fp );
                    }
    
                    /* turn off future detrending */
                    rtin->detrend_mode = RT_DETREND_NONE;
                    rtin->detrend_polort = -1;
    
                    /* return */
                    EXRETURN;
                }
    
                /* allocate PCOR_voxel_cor structure */
                if(( fit = malloc( detrend_num_ort * sizeof(float*))) == NULL )
                {
                    /* no memory, print an error and turn off detrending */
                    snprintf( rt_errorString, RT_LONG_STR_LEN, 
                      "RT Detrend: Could not allocate PCOR voxel corr!"
                      " Turning off detrend.");
                    fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                    PLUTO_popup_transient( plint, rt_errorString );
    
                    IFree( detrend_orts );
                    IFree( mask );
                    IFree( indx );
                    IFree( vval );
    
                    if( pc_ref != NULL )
                    {
                        free_PCOR_references(pc_ref);
                        pc_ref = NULL;
                    }
    
                    if( pc_vc != NULL )
                    {
                        free_PCOR_voxel_corr(pc_vc);
                        pc_ref = NULL;
                    }
    
                    /* close file */
                    if( ort_fp != NULL )
                    {
                        fclose( ort_fp );
                    }
    
                    /* turn off future detrending */
                    rtin->detrend_mode = RT_DETREND_NONE;
                    rtin->detrend_polort = -1;
    
                    /* return */
                    EXRETURN;
                }
               
                for( iv = 0; iv < detrend_num_ort; iv ++ )
                {
                    fit[iv] = NULL;
                }
     
                for( iv = 0; iv < detrend_num_ort; iv ++ )
                {
                    /* allocate PCOR_voxel_cor structure */
                    if(( fit[iv] = malloc( nvox * sizeof(float))) == NULL )
                    {
                        /* no memory, print an error and turn off detrending */
                        snprintf( rt_errorString, RT_LONG_STR_LEN, 
                          "RT Detrend: Could not allocate fit voxel buf!"
                          " Turning off detrend.");
                        fprintf(stderr, "RT_DETREND: ERROR: %s\n",
                                rt_errorString);
                        PLUTO_popup_transient( plint, rt_errorString );
        
                        IFree( detrend_orts );
                        IFree( mask );
                        IFree( indx );
                        IFree( vval );
    
                        if( pc_ref != NULL )
                        {
                            free_PCOR_references(pc_ref);
                            pc_ref = NULL;
                        }
    
                        if( pc_vc != NULL )
                        {
                            free_PCOR_voxel_corr(pc_vc);
                            pc_ref = NULL;
                        }
    
                        for( iv = 0; iv < detrend_num_ort; iv++ )
                        {
                            IFree( fit[iv] );
                        }
                        IFree( fit );
    
                        /* close file */
                        if( ort_fp != NULL )
                        {
                            fclose( ort_fp );
                        }
    
                        /* turn off future detrending */
                        rtin->detrend_mode = RT_DETREND_NONE;
                        rtin->detrend_polort = -1;
        
                        /* return */
                        EXRETURN;
                    }
                }
            } /* if DETREND ORTS */
            else if ( rtin->detrend_mode & RT_DETREND_SMOOTH )
            {
                if ( verbose )
                    fprintf(stderr,
                       "RT_DETREND: No detrending, just smoothing data\n" );
            }
            else
            {
                /* print an error and turn off future detrending */
                snprintf( rt_errorString, RT_LONG_STR_LEN, 
                  "RT Detrend: Unknown detrend mode 0x%x.", rtin->detrend_mode);
                fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                PLUTO_popup_transient( plint, rt_errorString );

                /* free allocated memory */ 
                IFree( indx );
                IFree( mask );
                IFree( vval );

                /* close file */
                if( ort_fp != NULL )
                {
                    fclose( ort_fp );
                }

                /* turn off future detrending */
                rtin->detrend_mode = RT_DETREND_NONE;
                rtin->detrend_polort = -1;

                /* return */
                EXRETURN;
            }
            
    
            if ( rtin->detrend_mode & RT_DETREND_SMOOTH )
            {
                /* copied code from mri_blur3D_addfwhm_speedy, would
                   just call this function, but the voxel dimensions
                   are not correctly set until BRICK is written */
                int dx=0.0f, dy=0.0f, dz=0.0f;
                dx = rtin->dxx;
                if( dx == 0.0f ) dx = 1.0f; else if( dx < 0.0f ) dx = -dx;
                dy = rtin->dyy;
                if( dy == 0.0f ) dy = 1.0f; else if( dy < 0.0f ) dy = -dy;
                dz = rtin->dzz;
                if( dz == 0.0f ) dz = 1.0f; else if( dz < 0.0f ) dz = -dz;

                mri_blur3D_getfac(rtin->detrend_fwhm, dx, dy, dz,
                     &nrep, &fx, &fy, &fz );

                if( nrep < 0 || fx < 0.0f || fy < 0.0f || fz < 0.0f )
                {
                    /* Error calculating smoothing factors, return error */
                    snprintf( rt_errorString, RT_LONG_STR_LEN, 
                      "RT Detrend: Error calculating smoothing factors!"
                      " Turning off detrend.");
                    fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                    PLUTO_popup_transient( plint, rt_errorString );
       
                    IFree( detrend_orts );
                    IFree( mask );
                    IFree( indx );
                    IFree( vval );
   
                    if( pc_ref != NULL )
                    {
                        free_PCOR_references(pc_ref);
                        pc_ref = NULL;
                    }
  
                    if( pc_vc != NULL )
                    {
                        /* was ivoxel, noted by Y Halchenko */
                        free_PCOR_voxel_corr(pc_vc);
                        pc_ref = NULL;
                    }
 
                    for( iv = 0; iv < detrend_num_ort; iv++ )
                    {
                        IFree( fit[iv] );
                    }
                    IFree( fit );

                    /* close file */
                    if( ort_fp != NULL )
                    {
                        fclose( ort_fp );
                    }

                    /* turn off future detrending */
                    rtin->detrend_mode = RT_DETREND_NONE;
                    rtin->detrend_polort = -1;
    
                    /* return */
                    EXRETURN;
                }

                if (verbose)
                    fprintf(stderr, "RT_DETREND: Smoothing data to %g"
                            " (%g,%g,%g) using (%g,%g,%g)x%d\n",
                        rtin->detrend_fwhm, rtin->dxx, rtin->dyy, rtin->dzz,
                        fx, fy, fz, nrep );
            }

            /* that should be all of the initialization */
            detrendIsInitialized = 1;

            if( verbose )
                fprintf(stderr,
                    "RT_DETREND finished initialization %d 0x%x %d %d %d\n",
                    it, rtin->detrend_mode, rtin->detrend_polort,
                    detrend_num_ort, nvox);
        }

        /* Initialized, so lets proceed */

        /* first copy vals for this image, calculating the gm as we go */
        global_mean = 0.0;

        switch( dtype )
        {
            case MRI_short:
            {
                short* dar = (short*)DSET_ARRAY(dset_time,it);
                for( iv = 0; iv < nvox; iv++ )
                {
                    vval[iv]=(float)dar[indx[iv]];
                    global_mean+=vval[iv];
                }
            }
            break;
            case MRI_float:
            {
                float* dar = (float*)DSET_ARRAY(dset_time,it);
                for( iv = 0; iv < nvox; iv++ )
                {
                    vval[iv]=(float)dar[indx[iv]];
                    global_mean+=vval[iv];
                }
            }
            break;
            case MRI_byte:
            {
                byte * dar = (byte*)DSET_ARRAY(dset_time,it);
                for( iv = 0; iv < nvox; iv++ )
                {
                    vval[iv]=(float)dar[indx[iv]];
                    global_mean+=vval[iv];
                }
            }
            break;
        }

        /* finish the mean */
        global_mean = global_mean / (float)nvox;

        /* allocate the output image */
        detim = mri_new_conforming( DSET_BRICK(dset_time,it) , MRI_float ) ;
        if( detim == NULL )
        {
            fprintf(stderr,"RT detrend: ERROR! Could not allocate detim!\a\n");
            /* free allocated memory */ 
            IFree( detrend_orts );
            IFree( vval );
            IFree( mask );
            IFree( indx );

            if( pc_ref != NULL )
            {
                free_PCOR_references(pc_ref);
                pc_ref = NULL;
            }

            if( pc_vc != NULL )
            {
                free_PCOR_voxel_corr(pc_vc);
                pc_ref = NULL;
            }

            for( iv = 0; iv < detrend_num_ort; iv++ )
            {
                IFree( fit[iv] );
            }
            IFree( fit );

            detrend_num_ort=0;

            /* close file */
            if( ort_fp != NULL )
            {
                fclose( ort_fp );
            }

            /* turn off future detrending */
            rtin->detrend_mode = RT_DETREND_NONE;
            rtin->detrend_polort = -1;

            /* de-initialize */
            detrendIsInitialized = 0;

            EXRETURN;
        }

        if( verbose > 1)
            fprintf( stderr, "RT_DETREND: Allocated detim for dset %d\n", it );
    
        /* get a pointer to img buffer */
        dval = MRI_FLOAT_PTR(detim);
    
        if( dval == NULL )
        {
            fprintf(stderr, "RT_DETREND: ERROR dval is NULL!\n" );
        }

        /* zero out the output buffer */
        memset(dval, 0, sizeof(float)*nxyz ) ;

        /* make sure that some ORTs were specified and not just smoothing */
        if (( rtin->detrend_mode & ~RT_DETREND_SMOOTH ) &&
            ( detrend_num_ort > 0 ))
        {
            /* now build the regressors */
            count = 0;
    
            /* polynomial detrend_orts */
            if( rtin->detrend_polort + 1 <= detrend_num_ort )
            {
                detrend_orts[0]=1.0;
                for( iv = 1; iv < rtin->detrend_polort+1; iv++ )
                {
                    detrend_orts[iv] = it * detrend_orts[iv-1];
                }
                count += rtin->detrend_polort+1;
            }
            else
            {
                snprintf( rt_errorString, RT_LONG_STR_LEN, 
                  "RT Detrend: Not enough room on ort array for polorts. (%d>%d)"
                  "skipping", rtin->detrend_polort+1, detrend_num_ort);
                fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                PLUTO_popup_transient( plint, rt_errorString );
            }
    
            /* motion detrend_orts */
            if( num_motion_ort > 0 )
            { 
                if( count + num_motion_ort <= detrend_num_ort )
                {
                    if( num_motion_ort == 3 )
                    {
                        detrend_orts[ count + 0 ] = rtin->reg_dx[it];
                        detrend_orts[ count + 1 ] = rtin->reg_dy[it];
                        detrend_orts[ count + 2 ] = rtin->reg_phi[it];
                    }
                    else if( num_motion_ort == 6 )
                    {
                        detrend_orts[ count + 0 ] = rtin->reg_dx[it];
                        detrend_orts[ count + 1 ] = rtin->reg_dy[it];
                        detrend_orts[ count + 2 ] = rtin->reg_dz[it];
                        detrend_orts[ count + 3 ] = rtin->reg_phi[it];
                        detrend_orts[ count + 4 ] = rtin->reg_psi[it];
                        detrend_orts[ count + 5 ] = rtin->reg_theta[it];
                    }
                    else if( num_motion_ort == 12 )
                    {
                        detrend_orts[ count +  0 ] = rtin->reg_dx[it];
                        detrend_orts[ count +  1 ] = rtin->reg_dy[it];
                        detrend_orts[ count +  2 ] = rtin->reg_phi[it];
                        detrend_orts[ count +  3 ] =
                                it > 0 ? rtin->reg_dx[it - 1] : 0.0  ;
                        detrend_orts[ count +  4 ] =
                                it > 0 ? rtin->reg_dy[it - 1] : 0.0  ;
                        detrend_orts[ count +  5 ] =
                                it > 0 ? rtin->reg_phi[it - 1] : 0.0  ;
                        detrend_orts[ count +  6 ] = detrend_orts[count + 0] *
                            detrend_orts[ count + 0 ];
                        detrend_orts[ count +  7 ] = detrend_orts[count + 1] * 
                            detrend_orts[ count + 1 ];
                        detrend_orts[ count +  8 ] = detrend_orts[count + 2] * 
                            detrend_orts[ count + 2 ];
                        detrend_orts[ count +  9 ] = detrend_orts[count + 3] * 
                            detrend_orts[ count + 3 ];
                        detrend_orts[ count + 10 ] = detrend_orts[count + 4] * 
                            detrend_orts[ count + 4 ];
                        detrend_orts[ count + 11 ] = detrend_orts[count + 5] * 
                            detrend_orts[ count + 5 ];
                    }
                    else if( num_motion_ort == 24 )
                    {
                        /* Friston 1996 model for correcting for motion */
                        detrend_orts[ count +  0 ] = rtin->reg_dx[it];
                        detrend_orts[ count +  1 ] = rtin->reg_dy[it];
                        detrend_orts[ count +  2 ] = rtin->reg_dz[it];
                        detrend_orts[ count +  3 ] = rtin->reg_phi[it];
                        detrend_orts[ count +  4 ] = rtin->reg_psi[it];
                        detrend_orts[ count +  5 ] = rtin->reg_theta[it];
                        detrend_orts[ count +  6 ] =
                                        it > 0 ? rtin->reg_dx[it - 1] : 0.0;
                        detrend_orts[ count +  7 ] =
                                        it > 0 ? rtin->reg_dy[it - 1] : 0.0;
                        detrend_orts[ count +  8 ] =
                                        it > 0 ? rtin->reg_dz[it - 1] : 0.0;
                        detrend_orts[ count +  9 ] =
                                        it > 0 ? rtin->reg_phi[it - 1] : 0.0;
                        detrend_orts[ count + 10 ] =
                                        it > 0 ? rtin->reg_psi[it - 1] : 0.0;
                        detrend_orts[ count + 11 ] =
                                        it > 0 ? rtin->reg_theta[it - 1] : 0.0;
                        detrend_orts[ count + 12 ] = detrend_orts[count + 0] * 
                            detrend_orts[ count + 0 ];
                        detrend_orts[ count + 13 ] = detrend_orts[count + 1] * 
                            detrend_orts[ count + 1 ];
                        detrend_orts[ count + 14 ] = detrend_orts[count + 2] * 
                            detrend_orts[ count + 2 ];
                        detrend_orts[ count + 15 ] = detrend_orts[count + 3] * 
                            detrend_orts[ count + 3 ];
                        detrend_orts[ count + 16 ] = detrend_orts[count + 4] * 
                            detrend_orts[ count + 4 ];
                        detrend_orts[ count + 17 ] = detrend_orts[count + 5] * 
                            detrend_orts[ count + 5 ];
                        detrend_orts[ count + 18 ] = detrend_orts[count + 6] * 
                            detrend_orts[ count + 6 ];
                        detrend_orts[ count + 19 ] = detrend_orts[count + 7] * 
                            detrend_orts[ count + 7 ];
                        detrend_orts[ count + 20 ] = detrend_orts[count + 8] * 
                            detrend_orts[ count + 8 ];
                        detrend_orts[ count + 21 ] = detrend_orts[count + 9] * 
                            detrend_orts[ count + 9 ];
                        detrend_orts[ count + 22 ] = detrend_orts[count + 10] * 
                            detrend_orts[ count + 10 ];
                        detrend_orts[ count + 23 ] = detrend_orts[count + 11] * 
                            detrend_orts[ count + 11 ];
                    }
                    count += num_motion_ort;
                }
                else
                {
                    snprintf( rt_errorString, RT_LONG_STR_LEN, 
                      "RT Detrend: Not enough room on ort array for motion\n"
                      "           (%d + %d > %d), skipping",
                      count, num_motion_ort, detrend_num_ort);
                    fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                    PLUTO_popup_transient( plint, rt_errorString );
                }
            }
    
            /* global mean ort */
            if( rtin->detrend_mode & RT_DETREND_GM )
            {
                if( count + 1 <= detrend_num_ort )
                {
                    detrend_orts[ count ]=global_mean;
                    count ++;
                }
                else
                {
                  snprintf( rt_errorString, RT_LONG_STR_LEN, 
                   "RT Detrend: Not enough room on ort array for global_mean.\n"
                   "            (%d + %d > %d), skipping",
                   count, 1, detrend_num_ort);
                  fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                  PLUTO_popup_transient( plint, rt_errorString );
                }
            }
    
            /* mask averages ort */
            if( rtin->detrend_mode & RT_DETREND_MASK )
            {
                if( count + rtin->mask_nvals <= detrend_num_ort )
                {
                    for( iv = 0; iv < rtin->mask_nvals; iv++ )
                    {
                        detrend_orts[iv+count] = (float)rtin->mask_aves[iv+1];
                    }
                    count += rtin->mask_nvals;
                }
                else
                {
                  snprintf( rt_errorString, RT_LONG_STR_LEN, 
                  "RT Detrend: Not enough room on ort array for global_mean.\n"
                  "            (%d + %d > %d), skipping",
                  count, rtin->mask_nvals, detrend_num_ort);
                  fprintf(stderr, "RT_DETREND: ERROR: %s\n", rt_errorString);
                  PLUTO_popup_transient( plint, rt_errorString );
                }
            }
   
            if ( verbose > 1 ) 
              fprintf( stderr, "RT_DETREND: added %d detrend_orts out of %d\n",
                       count, detrend_num_ort );
    
            /* output the orts to file */
            if( ort_fp != NULL )
            {
                if( detrend_num_ort > 0 )
                {
                    fprintf( ort_fp, "%f", detrend_orts[0] );
                    for( io = 1; io < detrend_num_ort; io++ )
                    {
                        fprintf( ort_fp, "\t%f", detrend_orts[io] );
                    }
                    fprintf( ort_fp, "\n" );
                }
            }
    
            /* update the regressors */
            update_PCOR_references( detrend_orts, pc_ref );
            if ( verbose > 1 )
              fprintf(stderr,"RT_DETREND: Updated references for dset %d\n",it);
    
            /* update the voxel corrs */
            PCOR_update_float( vval, pc_ref, pc_vc );
            if ( verbose > 1 )
              fprintf(stderr,"RT_DETREND: Updated voxel corr for dset %d\n",it);
    
    
            /* if it > num_ort perform detrend, otherwise dont */ 
            if( rtin->nvol[0] > detrend_num_ort )
            {
                /* get the fit */
                PCOR_get_lsqfit( pc_ref, pc_vc, fit );
                if ( verbose > 1 )
                  fprintf(stderr,"RT_DETREND: Calculated fit for dset %d\n",it);
    
                /* perform the detrend */
                for ( iv = 0; iv < nvox; iv++ )
                {
                    dval[ indx[iv] ] = vval[iv];
                    for( io = 0; io < detrend_num_ort; io++ )
                    {
                        if( fit[io] != NULL )
                        {
                            dval[ indx[iv] ] -=
                                (float)(fit[io][iv] * detrend_orts[io]);
                        }
                    }
                }
            }
            else
            {
              if ( verbose > 1 )
                fprintf( stderr, "RT_DETREND: Too early to detrend dset %d"
                         " (%d)\n", it, detrend_num_ort );
              /* not detrending yet, just copy non-detrended values */
              for ( iv = 0; iv < nvox; iv++ )
              {
                  dval[ indx[iv] ] = vval[iv];
              }
            }
        }
        else if ( rtin->detrend_mode & RT_DETREND_SMOOTH )
        {
          if ( verbose > 1 )
            fprintf( stderr, "RT_DETREND: smoothing only, copying over data"
                     " %d\n", it );
          /* not detrending yet, just copy non-detrended values */
          for ( iv = 0; iv < nvox; iv++ )
          {
              dval[ indx[iv] ] = vval[iv];
          }
        }
        else
        {
            fprintf( stderr, "RT_DETREND: unrecognized RT MODE 0x%x for %d,"
                     " skipping\n", rtin->detrend_mode, it );
        }

        /* smooth image in place if so desired */
        if(( rtin->detrend_mode & RT_DETREND_SMOOTH ) &&
           ( rtin->detrend_fwhm > 0.0 ))
        {
            if ( verbose > 1 )
               fprintf(stderr, "RT_DETREND: Smoothing data to %g (%g,%g,%g)\n",
                rtin->detrend_fwhm, rtin->dxx, rtin->dyy, rtin->dzz);
            if( fx>0.0 && fy>0.0 && fz>0.0 && rtin->nxx > 2 && rtin->nyy > 2 &&
                rtin->nzz > 2 )
            {
                mri_blur3D_inmask_speedy(detim, mask, fx, fy, fz, nrep);
            }
            else
            {
                mri_blur3D_inmask(detim, mask, fx, fy, fz, nrep);
            }
        }

        if( mri_data_pointer(detim) == NULL )
        {
          fprintf( stderr, "RT_DETREND: Strange, mri_data_pointer is NULL for"
                   " dset %d\n", it );
        }

        /* put the detrended values into rtin->detrend_dset */
        if( rtin->detrend_nvol == 0 )
        {
          if (verbose > 1)
            fprintf(stderr,"RT_DETREND: Writing dset %d via substitution\n",it);
          EDIT_substitute_brick( rtin->detrend_dset, 0, MRI_float,
                                 mri_data_pointer(detim) );
        }
        else
        {
          if (verbose > 1)
             fprintf(stderr,"RT_DETREND: Writing dset %d via append\n",it);
          EDIT_add_brick(rtin->detrend_dset, MRI_float, 0.0,
                         mri_data_pointer(detim) );
        }

        /* get rid of detim */
        mri_clear_data_pointer(detim); mri_free(detim);

        /* iterate the number of detrended volumes */
        rtin->detrend_nvol++;

        /* iterate the dset number of time points */
        EDIT_dset_items( rtin->detrend_dset , ADN_ntt , rtin->detrend_nvol ,
                         ADN_none ) ;

        if( verbose > 1 )
            fprintf( stderr, "RT_DETREND: Updated ntt after dset %d to %d\n",
                     it, rtin->detrend_nvol );

    }
    EXRETURN;
}
/* CC end */

