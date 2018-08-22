/*---------------------------------------------------------------------------*/
/*
  Program to calculate the delay between FMRI time series and the ideal
  reference time series.
  This program, which is a command line version of the plugin Hilbert Delay 98,
  uses Doug Ward's 3dfim+'s skeleton to read and write the bricks.
*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3ddelay"                        /* name of this program */
#define PROGRAM_AUTHOR "Ziad Saad (with help from B Douglas Ward)"  /* program author */
#define PROGRAM_DATE "Jul 22 2005"               /* date of last program mod */

/*---------------------------------------------------------------------------*/

#define MAX_FILES 20                        /* maximum number of ideal files */
                                            /* = maximum number of ort files */
#define RA_error FIM_error

/*---------------------------------------------------------------------------*/

#include "mrilib.h"
#include "matrix.h"

#include "delay.c"


/*--------------------- strings for output format --------------------*/
/* do not change the order in this string*/
static char * method_strings[] = { "Seconds" , "Degrees" , "Radians"} ;
static char * yn_strings[] = { "n" , "y" };

/* for printing potentially NULL strings         22 July 2005 [rickr] */
#undef  CHECK_NULL_STR
#define CHECK_NULL_STR(str) (str) ? (str) : "(NULL)"

/*#define ZDBG*/
#ifdef ZDBG
   #define IPOSx 8
   #define IPOSy 38
   #define IPOSz 7
#endif

#define NUM_METHOD_STRINGS (sizeof(method_strings)/sizeof(char *))
#define NUM_YN_STRINGS (sizeof(yn_strings)/sizeof(char *))

/* do not change these three*/
#define METH_SECONDS 0
#define METH_DEGREES 1
#define METH_RADIANS 2

#undef  DELAY
#define DELAY    0
#define XCOR     1
#define XCORCOEF 2
#ifndef NOWAYXCORCOEF
   #define NOWAYXCORCOEF 0               /* A flag value indicating that something lethal went on */
#endif


#define NBUCKETS 4            /* Number of values per voxel in Buket data set */
#define DELINDX 0               /* index of delay value in results vector */
#define COVINDX 1               /* index of covariance value in results vector */
#define COFINDX 2               /* index of cross correlation coefficient value in results vector */
#define VARINDX 3               /* index of FMRI time course variance value in results vector */

static char * DELAY_OUTPUT_TYPE_name[NBUCKETS] =
  { "Delay", "Covariance", "Corr. Coef.", "Variance" } ;

#define YUP  1
#define NOPE 0

#define ERROR_NOTHINGTODO    1            /* Nothing to do in hilbertdelay_V2 function */
#define ERROR_LARGENSEG      2            /* Too many segments specified in hilbertdelay_V2 function */
#define ERROR_LONGDELAY      3            /* Could not detect zero crossing before half of time course was gone */
#define ERROR_WRONGUNIT      8            /* Wrong units selected to pass to the delay functions */
#define ERROR_WARPVALUES   9
#define ERROR_FSVALUES      10
#define ERROR_TVALUES      11
#define ERROR_TaUNITVALUES   12
#define ERROR_TaWRAPVALUES   13
#define ERROR_FILEOPEN      15
#define ERROR_SERIESLENGTH   16
#define ERROR_OPTIONS      17
#define ERROR_NULLTIMESERIES    18
#define ERROR_OUTCONFLICT    19
#define ERROR_BADLENGTH      20
/*---------------------------------------------------------------------------*/

typedef struct DELAY_options
{
   float fs;         /* Sampling frequency */
                 /* it is only used for the log file in this version*/
                 /* the ts_func, gives TR automatically */
   float T;         /* Stimulus period */
   float co;         /* Correlation Coefficient Threshold*/
   int unt;         /* Delay units */
   int wrp;         /* flag for Polar Wrap */
   int rev;       /* flat for reversing phase */
   float scl;     /* multiply phase by scl */
   int Navg;         /* number of data sets averaged to obtain the brick (for statistical stuff) */
   int Nfit;         /* Number of fit parameters (for statistical stuff) */
   int Nseg;         /* Number of segments */
   int ignore;      /* number ofpoints to ignore from time courses */
   int Pover;      /* Percent overlap */
   int ln;         /* length of FMRI vector */
   /* int dtrnd; */      /* remove linear trend or just the mean */
   int biasrem;      /* flag for removing delay bias */
   int Dsamp;      /* flag for correction of non uniform sampling start time */
   int errcode;      /* error code number returned from hdelay */
   int out;         /* flag for writing delays to a file */
   int outts;      /* flag for writing time series to a file */
   float *rvec;  /* reference time series */
   int nxx;
   int nyy;
   int nzz;
   FILE * outwrite;
   FILE * outwritets;
   FILE * outlogfile;

   int NFirst;              /* first image from input 3d+time dataset to use */
   int NLast;               /* last image from input 3d+time dataset to use */
   int N;                   /* number of usable data points from input data */
   int polort;              /* degree of polynomial for baseline model */
   int num_ortts;           /* number of ort time series */
   int num_idealts;         /* number of ideal time series */
   int q;                   /* number of parameters in the baseline model */
   int p;                   /* number of parameters in the baseline model
                 plus number of ideals */

   float fim_thr;           /* threshold for internal fim mask */
   float cdisp;             /* minimum correlation coefficient for display */

   char * outname; /* Name of ascii output files */
   char * outnamets; /* Name of ascii output files */
   char * outnamelog; /* Name of ascii output files */
   
   char * input_filename;   /* input 3d+time dataset filename */
   char * mask_filename;    /* input mask dataset filename */
   char * input1D_filename; /* input fMRI measurement time series */

   int num_ort_files;                  /* number of ort files */
   char * ort_filename[MAX_FILES];     /* input ort time series file names */
   int num_ideal_files;                /* number of ideal files */
   char * ideal_filename[MAX_FILES];   /* input ideal time series file names */
   char * bucket_filename;             /* output bucket dataset file name */

   int output_type[NBUCKETS];   /* output type options */
   byte *bmask;
} DELAY_options;


/*---------------------------------------------------------------------------*/
/*
  Get the time series for one voxel from the AFNI 3d+time data set.
*/

void extract_ts_array
(
  THD_3dim_dataset * dset_time,      /* input 3d+time dataset */
  int iv,                            /* get time series for this voxel */
  float * ts_array                   /* time series data for voxel #iv */
);

/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void FIM_error (char * message)
{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to display 3ddelay help menu.
*/

void display_help_menu()
{
  printf (
"The program estimates the time delay between each voxel time series    \n"
"in a 3D+time dataset and a reference time series[1][2].                \n"
"The estimated delays are relative to the reference time series.\n"
"For example, a delay of 4 seconds means that the voxel time series \n"
"is delayed by 4 seconds with respect to the reference time series.\n\n"
"                                                                       \n"
"Usage:                                                                 \n"
"3ddelay                                                                 \n"
"-input fname       fname = filename of input 3d+time dataset           \n"
"                   DO NOT USE CATENATED timeseries! Time axis is assumed\n"
"                   to be continuous and not evil.\n"
"-ideal_file rname  rname = input ideal time series file name           \n"
"   The length of the reference time series should be equal to           \n"
"     that of the 3d+time data set. \n"
"     The reference time series vector is stored in an ascii file.        \n"
"     The programs assumes that there is one value per line and that all  \n"
"     values in the file are part of the reference vector.                \n"
"     PS: Unlike with 3dfim, and FIM in AFNI, values over 33333 are treated\n"
"     as part of the time series.                                          \n"
"-fs fs             Sampling frequency in Hz. of data time series (1/TR). \n"
"-T  Tstim          Stimulus period in seconds. \n"
"                   If the stimulus is not periodic, you can set Tstim to 0.\n"
"[-prefix bucket]   The prefix for the results Brick.\n" 
"                   The first subbrick is for Delay.\n"
"                   The second subbrick is for Covariance, which is an \n"
"                   estimate of the power in voxel time series at the\n"
"                   frequencies present in the reference time series.\n"
"                   The third subbrick is for the Cross Correlation \n"
"                   Coefficients between FMRI time series and reference time\n"
"                   series. The fourth subbrick contains estimates of the\n"
"                   Variance of voxel time series. \n"
"                   The default prefix is the prefix of the input dset \n"
"                   with a '.DEL' extension appended to it.\n"
"\n"
"[-polort order]    Detrend input time series with polynomial of order\n"
"                   'order'. If you use -1 for order then the program will\n"
"                   suggest an order for you (about 1 for each 150 seconds)\n"
"                   The minimum recommended is 1. The default is -1 for auto\n"
"                   selection. This is the same as option Nort in the plugin\n"
"                   version.\n"
"[-nodtrnd]         Equivalent to polort 0, whereby only the mean is removed.\n"
"           NOTE:   Regardless of these detrending options, No detrending is \n"
"                   done to the reference time series.\n"
"\n"
"[-uS/-uD/-uR]      Units for delay estimates. (Seconds/Degrees/Radians)\n"
"                   You can't use Degrees or Radians as units unless \n"
"                   you specify a value for Tstim > 0.\n"
"[-phzwrp]          Delay (or phase) wrap.\n"
"                   This switch maps delays from: \n"
"                   (Seconds) 0->T/2 to 0->T/2 and T/2->T to -T/2->0\n"
"                   (Degrees) 0->180 to 0->180 and 180->360 to -180->0\n" 
"                   (Radians) 0->pi to 0->pi and pi->2pi to -pi->0\n"
"                   You can't use this option unless you specify a \n"
"                   value for Tstim > 0.\n"
"[-nophzwrp]        Do not wrap phase (default).\n"
"[-phzreverse]      Reverse phase such that phase -> (T-phase)\n"
"[-phzscale SC]     Scale phase: phase -> phase*SC (default no scaling)\n"   
"\n"
"[-bias]            Do not correct for the bias in the estimates [1][2]\n"
"[-nobias | -correct_bias] Do correct for the bias in the estimates\n"
"                          (default).\n"
"\n"
"[-dsamp]           Correct for slice timing differences        (default).\n"
"[-nodsamp ]        Do not correct for slice timing differences .\n"
"\n"
"[-mask mname]      mname = filename of 3d mask dataset                 \n"
"                   only voxels with non-zero values in the mask would be \n"
"                   considered.                                           \n"
"\n"
"[-nfirst fnum]     fnum = number of first dataset image to use in      \n"
"                     the delay estimate. (default = 0)                 \n"
"[-nlast  lnum]     lnum = number of last dataset image to use in       \n"
"                     the delay estimate. (default = last)              \n"
"\n"
"[-co CCT]          Cross Correlation Coefficient threshold value.\n"
"                   This is only used to limit the ascii output (see below).\n"
"\n"
"[-asc [out]]       Write the results to an ascii file for voxels with \n"
"[-ascts [out]]     cross correlation coefficients larger than CCT.\n"
"                   If 'out' is not specified, a default name similar \n"
"                   to the default output prefix is used.\n"
"                   -asc, only files 'out' and 'out.log' are written to disk\n"
"                   (see ahead)\n"
"                   -ascts, an additional file, 'out.ts', is written to disk\n"
"                   (see ahead)\n"
"                   There a 9 columns in 'out' which hold the following\n"
"                   values:\n"
"                    1- Voxel Index (VI) : Each voxel in an AFNI brick has a\n"
"                          unique index.\n"
"                          Indices map directly to XYZ coordinates.\n"
"                          See AFNI plugin documentations for more info.\n"
"                    2..4- Voxel coordinates (X Y Z): Those are the voxel \n"
"                          slice coordinates. You can see these coordinates\n"
"                          in the upper left side of the AFNI window.\n"
"                          To do so, you must first switch the voxel \n"
"                          coordinate units from mm to slice coordinates. \n"
"                          Define Datamode -> Misc -> Voxel Coords ?\n"
"                          PS: The coords that show up in the graph window\n"
"                              may be different from those in the upper left\n"
"                              side of AFNI's main window.\n"
"                    5- Duff : A value of no interest to you. It is preserved\n"
"                              for backward compatibility.\n"
"                    6- Delay (Del) : The estimated voxel delay.\n"
"                    7- Covariance (Cov) : Covariance estimate.\n"
"                    8- Cross Correlation Coefficient (xCorCoef) : \n"
"                          Cross Correlation Coefficient.\n"
"                    9- Variance (VTS) : Variance of voxel's time series.\n"
"\n"
"                   The file 'out' can be used as an input to two plugins:\n"
"                     '4Ddump' and '3D+t Extract'\n\n"
"                   The log file 'out.log' contains all parameter settings \n"
"                   used for generating the output brick. \n"
"                   It also holds any warnings generated by the plugin.\n"
"                   Some warnings, such as 'null time series ...' , or \n"
"                   'Could not find zero crossing ...' are harmless. '\n"
"                   I might remove them in future versions.\n\n"
"                   A line (L) in the file 'out.ts' contains the time series \n"
"                   of the voxel whose results are written on line (L) in the\n"
"                   file 'out'.\n"
"                   The time series written to 'out.ts' do not contain the\n"
"                   ignored samples, they are detrended and have zero mean.\n"
"\n"
"                                                                      \n"
"Random Comments/Advice:\n"
"   The longer you time series, the better. It is generally recomended that\n"
"   the largest delay be less than N/10, N being time series' length.\n"
"   The algorithm does go all the way to N/2.\n\n"
"   If you have/find questions/comments/bugs about the plugin, \n"
"   send me an E-mail: saadz@mail.nih.gov\n\n"
"                          Ziad Saad Dec 8 00.\n\n"
"   [1] : Bendat, J. S. (1985). The Hilbert transform and applications \n"
"         to correlation measurements, Bruel and Kjaer Instruments Inc.\n"
"          \n"
"   [2] : Bendat, J. S. and G. A. Piersol (1986). Random Data analysis and\n"
"         measurement procedures, John Wiley & Sons.\n"
"   Author's publications on delay estimation using the Hilbert Transform:\n"
"   [3] : Saad, Z.S., et al., Analysis and use of FMRI response delays. \n"
"         Hum Brain Mapp, 2001. 13(2): p. 74-93.\n"
"   [4] : Saad, Z.S., E.A. DeYoe, and K.M. Ropella, Estimation of FMRI \n"
"         Response Delays.  Neuroimage, 2003. 18(2): p. 494-504.\n"
"\n" 
    );

  PRINT_COMPILE_DATE ; exit(0);
}


/*---------------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/* taken from #include "/usr/people/ziad/Programs/C/DSP_in_C/dft.h" */
/* dft.h - function prototypes and structures for dft and fft functions */


#include "plug_delay_V2.h"


/***********************************************************************
  Plugin to compute a 3D+time dataset voxelwise delay with respect to
  a reference waveform
************************************************************************/



/*----------------- prototypes for internal routines -----------------*/


void DELAY_tsfuncV2() ;                      /* the timeseries routine */

void show_ud ( DELAY_options* ud,int loc);

void write_ud ( DELAY_options* ud);

void indexTOxyz ( DELAY_options* ud, int ncall, int *xpos , int *ypos , int *zpos);

void xyzTOindex (struct DELAY_options* option_data, int *ncall, int xpos , int ypos , int zpos);     

void error_report ( DELAY_options* ud, int ncall );

void writets ( DELAY_options* ud,float * ts);

/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the input options.
*/

void initialize_options
(
  DELAY_options * option_data    /* fim program options */
)

{
  int is=-1;                     /* index */



   byte *bmask;

  /*----- Initialize default values -----*/
  option_data->fs = 0;
  option_data->co = 0.5;
  option_data->T = 0;
  option_data->unt = METH_SECONDS;
  option_data->wrp = 0;
  option_data->rev = 0;
  option_data->scl = 1.0;
  option_data->Navg = 1;
  option_data->Nfit = 2;
  option_data->Nseg = 1;
  option_data->ignore = 0;
  option_data->Pover = 0;
  option_data->ln = 0;
  option_data->biasrem = 1;
  option_data->Dsamp = 1;
  option_data->errcode = 0;
  option_data->out = 0;
  option_data->outts = 0;
  option_data->rvec = NULL;
  option_data->outwrite = NULL;
  option_data->outwritets = NULL;
  option_data->outlogfile = NULL;
  option_data->nxx = 64;
  option_data->nyy = 64;
  option_data->nzz = 20;
  option_data->NFirst = 0;
  option_data->NLast  = 32767;
  option_data->polort = -1;

   /* Left over from 3dfim+.c remove inthe future, with care !*/
  option_data->num_ortts = 0;
  option_data->num_idealts = 0;
  option_data->N      = 0;
  option_data->fim_thr = 0.0999;
  option_data->cdisp = -1.0;
  option_data->q = 0;
  option_data->p = 0;
  option_data->num_ort_files = 0;
  option_data->num_ideal_files = 0;


  for (is=0; is<NBUCKETS; ++is) option_data->output_type[is] = -1;

  /*----- Initialize file names -----*/
  option_data->input_filename = NULL;
  option_data->mask_filename = NULL;
  option_data->input1D_filename = NULL;
  option_data->bucket_filename = NULL;
  option_data->outname = NULL;
  option_data->outnamets = NULL;
  option_data->outnamelog = NULL;
  option_data->bmask = NULL;

  for (is = 0;  is < MAX_FILES;  is++)
    {
      option_data->ort_filename[is] = NULL;
      option_data->ideal_filename[is] = NULL;
    }

}


/*---------------------------------------------------------------------------*/
/*
  Routine to get user specified input options.
*/

void get_options
(
  int argc,                        /* number of input arguments */
  char ** argv,                    /* array of input arguments */
  DELAY_options * option_data        /* fim program options */
)

{
  int nopt = 1;                     /* input option argument counter */
  int ival, index;                  /* integer input */
  float fval;                       /* float input */
  char message[THD_MAX_NAME];       /* error message */
  int k;                            /* ideal time series index */


  /*----- does user request help menu? -----*/
  if (argc < 2 || strcmp(argv[1], "-help") == 0)  display_help_menu();


  /*----- initialize the input options -----*/
  initialize_options (option_data);


  /*----- main loop over input options -----*/
  while (nopt < argc )
    {

      if (strcmp(argv[nopt], "-polort") == 0 ||
          strcmp(argv[nopt], "-Nort") == 0)
      {
        char *qpt ;
        nopt++;
        if (nopt >= argc)  FIM_error ("need argument after -polort ");
        option_data->polort = (int)strtod(argv[nopt],&qpt);
        if( *qpt != '\0' ) WARNING_message("Illegal non-numeric value after -polort") ;
        nopt++;
        continue;
      }

      /*-----   -input filename   -----*/
      if (strcmp(argv[nopt], "-input") == 0)
      {
        nopt++;
        if (nopt >= argc)  FIM_error ("need argument after -input ");
        option_data->input_filename = malloc (sizeof(char)*THD_MAX_NAME);
        MTEST (option_data->input_filename);
        strcpy (option_data->input_filename, argv[nopt]);
        nopt++;
        continue;
      }

      /*-----   -mask filename   -----*/
      if (strcmp(argv[nopt], "-mask") == 0)
      {
        nopt++;
        if (nopt >= argc)  FIM_error ("need argument after -mask ");
        option_data->mask_filename = malloc (sizeof(char)*THD_MAX_NAME);
        MTEST (option_data->mask_filename);
        strcpy (option_data->mask_filename, argv[nopt]);
        nopt++;
        continue;
      }
  

      /*-----   -nfirst num  -----*/
      if (strcmp(argv[nopt], "-nfirst") == 0)
      {
        nopt++;
        if (nopt >= argc)  FIM_error ("need argument after -nfirst ");
        sscanf (argv[nopt], "%d", &ival);
        if (ival < 0)
          FIM_error ("illegal argument after -nfirst ");
        option_data->NFirst = ival;
        nopt++;
        continue;
      }


      /*-----   -nlast num  -----*/
      if (strcmp(argv[nopt], "-nlast") == 0)
      {
        nopt++;
        if (nopt >= argc)  FIM_error ("need argument after -nlast ");
        sscanf (argv[nopt], "%d", &ival);
        if (ival < 0)
          FIM_error ("illegal argument after -nlast ");
        option_data->NLast = ival;
        nopt++;
        continue;
      }

      /*-----   -fs num  -----*/
      if (strcmp(argv[nopt], "-fs") == 0)
      {
        nopt++;
        if (nopt >= argc)  FIM_error ("need argument after -fs ");
        sscanf (argv[nopt], "%f", &fval);
        if (fval < 0)
          FIM_error ("illegal argument after -fs ");
        option_data->fs = fval;
        nopt++;
        continue;
      }
 
 

      /*-----   -T num  -----*/
      if (strcmp(argv[nopt], "-T") == 0)
      {
        nopt++;
        if (nopt >= argc)  FIM_error ("need argument after -T ");
        sscanf (argv[nopt], "%f", &fval);
        if (fval < 0)
          FIM_error ("illegal argument after -T ");
        option_data->T = fval;
        nopt++;
        continue;
      }

      /*-----   -ideal_file rname   -----*/
      if (strcmp(argv[nopt], "-ideal_file") == 0)
      {
        nopt++;
        if (nopt >= argc)  FIM_error ("need argument after -ideal_file");

        k = option_data->num_ideal_files;
        if (k+1 > MAX_FILES)
          {
            sprintf (message, "Too many ( > %d ) ideal time series files. ",
                MAX_FILES);
            FIM_error (message);
          }

        option_data->ideal_filename[k]
          = malloc (sizeof(char)*THD_MAX_NAME);
        MTEST (option_data->ideal_filename[k]);
        strcpy (option_data->ideal_filename[k], argv[nopt]);
        option_data->num_ideal_files++;
        nopt++;
        continue;
      }
 

      /*-----   -prefix filename   -----*/
      if (strcmp(argv[nopt], "-prefix") == 0)
      {
        nopt++;
        if (nopt >= argc)  FIM_error ("need file prefixname after -bucket ");
        option_data->bucket_filename = malloc (sizeof(char)*THD_MAX_NAME);
        MTEST (option_data->bucket_filename);
        strcpy (option_data->bucket_filename, argv[nopt]);
        nopt++;
        continue;
      }
 
      
      /*-----   -uS  -----*/
      if (strcmp(argv[nopt], "-uS") == 0)
      {
        option_data->unt = METH_SECONDS;
        nopt++;
        continue;
      }

      /*-----   -uR  -----*/
      if (strcmp(argv[nopt], "-uR") == 0)
      {
        option_data->unt = METH_RADIANS;
        nopt++;
        continue;
      }

      /*-----   -uD  -----*/
      if (strcmp(argv[nopt], "-uD") == 0)
      {
        option_data->unt = METH_DEGREES;
        nopt++;
        continue;
      }

      /*-----   -phzwrp  -----*/
      if (strcmp(argv[nopt], "-phzwrp") == 0)
      {
        option_data->wrp = 1;
        nopt++;
        continue;
      }
      if (strcmp(argv[nopt], "-nophzwrp") == 0)
      {
        option_data->wrp = 0;
        nopt++;
        continue;
      }
      
      /*-----   -phzreverse  -----*/
      if (strcmp(argv[nopt], "-phzreverse") == 0)
      {
        option_data->rev = 1;
        nopt++;
        continue;
      }
      
      if (strcmp(argv[nopt], "-phzscale") == 0)
      {
        nopt++;
        if (nopt >= argc)  FIM_error ("need argument after -phzscale");
        sscanf (argv[nopt], "%f", &fval);
        if (fval < 0 || fval > 10)
          FIM_error ("illegal argument after -phzscale ");
        option_data->scl = fval;
        nopt++;
        continue;
      }
      /*-----   -bias  -----*/
      if (strcmp(argv[nopt], "-bias") == 0)
      {
        option_data->biasrem = 0;
        nopt++;
        continue;
      }
      if (strcmp(argv[nopt], "-nobias") == 0 ||
          strcmp(argv[nopt], "-No_bias") == 0 ||
          strcmp(argv[nopt], "-correct_bias") == 0)
      {
        option_data->biasrem = 1;
        nopt++;
        continue;
      }

       /*-----   -nodsamp  -----*/
      if (strcmp(argv[nopt], "-nodsamp") == 0)
      {
        option_data->Dsamp = 0;
        nopt++;
        continue;
      }
      if (strcmp(argv[nopt], "-dsamp") == 0)
      {
        option_data->Dsamp = 1;
        nopt++;
        continue;
      }
       /*-----   -nodtrnd  -----*/
      if (strcmp(argv[nopt], "-nodtrnd") == 0)
      {
        /* option_data->dtrnd = 0; */
        option_data->polort = 0;
        nopt++;
        continue;
      }

       /*-----   -co num -----*/
      if (strcmp(argv[nopt], "-co") == 0)
      {
        nopt++;
        if (nopt >= argc)  FIM_error ("need argument after -co");
        sscanf (argv[nopt], "%f", &fval);
        if (fval < 0)
          FIM_error ("illegal argument after -co ");
        option_data->co = fval;
        nopt++;
        continue;
      }

      /*-----   -asc out   -----*/
      if (strcmp(argv[nopt], "-asc") == 0)
      {
        nopt++;
        option_data->out = 1;
        if (nopt >= argc)  {
           option_data->outname = NULL;
          option_data->outnamelog = NULL;
         
         continue; }
        if (strncmp(argv[nopt], "-", 1) == 0) {
           option_data->outname = NULL;
         option_data->outnamelog = NULL;
         continue; }
           
        option_data->outname = malloc (sizeof(char)*THD_MAX_NAME);
        option_data->outnamelog = malloc (sizeof(char)*(THD_MAX_NAME+4));
      
        MTEST (option_data->outname);
        MTEST (option_data->outnamelog);
        strcpy (option_data->outname, argv[nopt]);
        sprintf (option_data->outnamelog, "%s.log", option_data->outname);
   
        nopt++;
        continue;
      }

      /*-----   -ascts out   -----*/
      if (strcmp(argv[nopt], "-ascts") == 0)
      {
        nopt++;
        option_data->out = 1;
        option_data->outts = 1;
        if (nopt >= argc)  {
           option_data->outname = NULL;
          option_data->outnamelog = NULL;
         option_data->outnamets = NULL;
         continue; }
        if (strncmp(argv[nopt], "-", 1) == 0) {
          option_data->outnamelog = NULL;
         option_data->outnamets = NULL;
           option_data->outname = NULL;
         continue; }
           
        option_data->outname = malloc (sizeof(char)*THD_MAX_NAME);
        option_data->outnamelog = malloc (sizeof(char)*(THD_MAX_NAME+4));
        option_data->outnamets = malloc (sizeof(char)*(THD_MAX_NAME+3));

        MTEST (option_data->outname);
       MTEST (option_data->outnamets);
        MTEST (option_data->outnamelog);
   
        strcpy (option_data->outname, argv[nopt]);
        sprintf (option_data->outnamets, "%s.ts", option_data->outname);
        sprintf (option_data->outnamelog, "%s.log", option_data->outname);
      
        nopt++;
        continue;
      }

 
      
      
      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      FIM_error (message);
 
    }

}


/*---------------------------------------------------------------------------*/
/*
  Read one time series from specified file name.  This file name may have
  a column selector attached.
*/

float * read_one_time_series
(
  char * ts_filename,          /* time series file name (plus column index) */
  int * ts_length              /* output value for time series length */
)

{
  char message[THD_MAX_NAME];    /* error message */
  char * cpt;                    /* pointer to column suffix */
  char filename[THD_MAX_NAME];   /* time series file name w/o column index */
  char subv[THD_MAX_NAME];       /* string containing column index */
  MRI_IMAGE * im, * flim;  /* pointers to image structures
               -- used to read 1D ASCII */
  float * far=NULL;             /* pointer to MRI_IMAGE floating point data */
  int nx;                  /* number of time points in time series */
  int ny;                  /* number of columns in time series file */
  int iy;                  /* time series file column index */
  int ipt;                 /* time point index */
  float * ts_data;         /* input time series data */


  /*----- Read the time series file -----*/
  flim = mri_read_1D( ts_filename ) ;
  if (flim == NULL)
    {
      sprintf (message,  "Unable to read time series file: %s",  ts_filename);
      FIM_error (message);
    }


  /*----- Set pointer to data, and set dimensions -----*/
  far = MRI_FLOAT_PTR(flim);
  nx = flim->nx;
  ny = flim->ny; iy = 0 ;
  if( ny > 1 ){
    fprintf(stderr,"WARNING: time series %s has %d columns\n",ts_filename,ny);
  }



  /*----- Save the time series data -----*/
  *ts_length = nx;
  ts_data = (float *) malloc (sizeof(float) * nx);
  MTEST (ts_data);
  for (ipt = 0;  ipt < nx;  ipt++)
    ts_data[ipt] = far[ipt + iy*nx]; 


  mri_free (flim);  flim = NULL;

  return (ts_data);
}


/*---------------------------------------------------------------------------*/
/*
  Read multiple time series from specified file name.  This file name may have
  a column selector attached.
*/

MRI_IMAGE * read_time_series
(
  char * ts_filename,      /* time series file name (plus column selectors) */
  int ** column_list       /* list of selected columns */
)

{
  char message[THD_MAX_NAME];    /* error message */
  char * cpt;                    /* pointer to column suffix */
  char filename[THD_MAX_NAME];   /* time series file name w/o column index */
  char subv[THD_MAX_NAME];       /* string containing column index */
  MRI_IMAGE * im, * flim;  /* pointers to image structures
               -- used to read 1D ASCII */
  float * far;             /* pointer to MRI_IMAGE floating point data */
  int nx;                  /* number of time points in time series */
  int ny;                  /* number of columns in time series file */


  /*----- Read the time series file -----*/
  flim = mri_read_1D(ts_filename) ;
  if (flim == NULL)
    {
      sprintf (message,  "Unable to read time series file: %s",  ts_filename);
      FIM_error (message);
    }


  far = MRI_FLOAT_PTR(flim);
  nx = flim->nx;
  ny = flim->ny;
  *column_list = NULL;  /* mri_read_1D does column selection */

  return (flim);
}


/*---------------------------------------------------------------------------*/
/*
  Read the input data files.
*/

void read_input_data
(
  DELAY_options * option_data,        /* fim program options */
  THD_3dim_dataset ** dset_time,    /* input 3d+time data set */
  THD_3dim_dataset ** mask_dset,    /* input mask data set */
  float ** fmri_data,               /* input fMRI time series data */
  int * fmri_length,                /* length of fMRI time series */
  MRI_IMAGE ** ort_array,           /* ort time series arrays */
  int ** ort_list,                  /* list of ort time series */
  MRI_IMAGE ** ideal_array,         /* ideal time series arrays */
  int ** ideal_list                 /* list of ideal time series */
)

{
  char message[THD_MAX_NAME];    /* error message */

  int num_ort_files;       /* number of ort time series files */
  int num_ideal_files;     /* number of ideal time series files */
  int is;                  /* time series file index */
  int polort;              /* degree of polynomial for baseline model */
  int num_ortts;           /* number of ort time series */
  int num_idealts;         /* number of ideal time series */
  int q;                   /* number of parameters in the baseline model */
  int p;                   /* number of parameters in the baseline model
                           plus number of ideals */
  int nref_in=-1  ;
  int nmsk = 0;
  float **ref_in=NULL  ;
  THD_3dim_dataset *newset=NULL; 
  MRI_IMARR *corder_inar=NULL ;


  /*----- Initialize local variables -----*/
  polort          = option_data->polort;
  num_ort_files   = option_data->num_ort_files;
  num_ideal_files = option_data->num_ideal_files;


  /*----- Read the input fMRI measurement data -----*/
  if (option_data->input1D_filename != NULL)
    {
      /*----- Read the input fMRI 1D time series -----*/
      *fmri_data = read_one_time_series (option_data->input1D_filename,
                fmri_length);
      if (*fmri_data == NULL)
   {
     sprintf (message,  "Unable to read time series file: %s",
         option_data->input1D_filename);
     FIM_error (message);
   }
      *dset_time = NULL;
    }

  else if (option_data->input_filename != NULL)
    {
      /*----- Read the input 3d+time dataset -----*/
      *dset_time = THD_open_one_dataset (option_data->input_filename);
      if (!ISVALID_3DIM_DATASET(*dset_time))
   {
     sprintf (message,  "Unable to open data file: %s",
         option_data->input_filename);
     FIM_error (message);
   }
      DSET_load(*dset_time); CHECK_LOAD_ERROR(*dset_time);

      if (option_data->mask_filename != NULL)
   {
     /*----- Read the input mask dataset -----*/
     *mask_dset = THD_open_dataset (option_data->mask_filename);
     if (!ISVALID_3DIM_DATASET(*mask_dset))
       {
         sprintf (message,  "Unable to open mask file: %s",
             option_data->mask_filename);
         FIM_error (message);
       }
     DSET_load(*mask_dset); CHECK_LOAD_ERROR(*mask_dset);
     nmsk = thd_mask_from_brick(*mask_dset, 0, 0, &(option_data->bmask), 1);
     if (!nmsk) {
         sprintf (message,  "Empty mask from %s nothing to do",
             option_data->mask_filename);
         FIM_error (message);
     }
     if (DSET_NX(*mask_dset) != DSET_NX(*dset_time) ||
         DSET_NX(*mask_dset) != DSET_NX(*dset_time) ||
         DSET_NX(*mask_dset) != DSET_NX(*dset_time) ) {
         sprintf (message,  "Mask %s and Time series %s dimensions mismatch.",
             option_data->mask_filename, option_data->input_filename);
         FIM_error (message);
     }
   }
 
   {
 
      /* detrend that baby */
      if (option_data->polort == -1) { /* choose your goose */
         option_data->polort = DSET_NVALS(*dset_time)/150+1;
         fprintf(stdout,"Notice 3ddelay: Choosing polort = %d\n",
                  option_data->polort);
      }
      nref_in = option_data->polort+1;
      fprintf(stdout,"poly. detrending: "
               "%d baseline funcs, %d time points\n",
               nref_in, DSET_NVALS(*dset_time)) ;
      ref_in = THD_build_polyref( nref_in , DSET_NVALS(*dset_time) ) ;
      if( ref_in == NULL ) ERROR_exit("THD_build_trigref failed!") ;
      if (!(newset = THD_detrend_dataset( *dset_time , nref_in ,
                                          ref_in , 2 ,
                                          0 ,
                                          option_data->bmask , &corder_inar )))
      {
         sprintf (message,  "detrending failed!");
         FIM_error (message);
      }
 
      DSET_delete(*dset_time); *dset_time=newset; newset = NULL;
   }
    }
  else
    FIM_error ("Must specify input measurement data");


  /*----- Read the input ideal time series files -----*/
  for (is = 0;  is < num_ideal_files;  is++)
    {
      ideal_array[is] = read_time_series (option_data->ideal_filename[is],
                 &(ideal_list[is]));

      if (ideal_array[is] == NULL)
   {
     sprintf (message,  "Unable to read ideal time series file: %s",
         option_data->ideal_filename[is]);
     FIM_error (message);
   }
    }


  /*----- Count number of ort and number of ideal time series -----*/
  num_ortts = 0;
  for (is = 0;  is < num_ort_files;  is++)
    {
      if (ort_list[is] == NULL)
   num_ortts += ort_array[is]->ny;
      else
   num_ortts += ort_list[is][0];
    }
  q = polort + 1 + num_ortts;

  num_idealts = 0;
  for (is = 0;  is < num_ideal_files;  is++)
    {
      if (ideal_list[is] == NULL)
   num_idealts += ideal_array[is]->ny;
      else
   num_idealts += ideal_list[is][0];
    }
  p = q + num_idealts;

  option_data->num_ortts = num_ortts;
  option_data->num_idealts = num_idealts;
  option_data->q = q;
  option_data->p = p;


}


/*---------------------------------------------------------------------------*/
/*
  Routine to check whether one output file already exists.
*/

void check_one_output_file
(
  THD_3dim_dataset * dset_time,     /* input 3d+time data set */
  char * filename                   /* name of output file */
)

{
  char message[THD_MAX_NAME];      /* error message */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int ierror;                         /* number of errors in editing data */


  /*----- make an empty copy of input dataset -----*/
  new_dset = EDIT_empty_copy( dset_time ) ;


  ierror = EDIT_dset_items( new_dset ,
             ADN_prefix , filename ,
             ADN_label1 , filename ,
             ADN_self_name , filename ,
             ADN_type , ISHEAD(dset_time) ? HEAD_FUNC_TYPE :
                                                   GEN_FUNC_TYPE ,
             ADN_none ) ;

  if( ierror > 0 )
    {
      sprintf (message,
          "*** %d errors in attempting to create output dataset!\n",
          ierror);
      FIM_error (message);
    }

  if( THD_is_file(new_dset->dblk->diskptr->header_name) )
    {
      sprintf (message,
          "Output dataset file %s already exists "
          " -- cannot continue!\a\n",
          new_dset->dblk->diskptr->header_name);
      FIM_error (message);
    }

  /*----- deallocate memory -----*/ 
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to check whether output files already exist.
*/

void check_output_files
(
  DELAY_options * option_data,       /* fim program options */
  THD_3dim_dataset * dset_time     /* input 3d+time data set */
)

{

  if ((option_data->bucket_filename != NULL)
      && (option_data->input1D_filename == NULL))
    check_one_output_file (dset_time, option_data->bucket_filename);

}


/*---------------------------------------------------------------------------*/
/*
  Routine to check for valid inputs.
*/

void check_for_valid_inputs
(
  DELAY_options * option_data,      /* fim program options */
  THD_3dim_dataset * dset_time,   /* input 3d+time data set */
  THD_3dim_dataset * mask_dset,   /* input mask data set */
  int fmri_length,                /* length of input fMRI time series */
  MRI_IMAGE ** ort_array,         /* ort time series arrays */
  MRI_IMAGE ** ideal_array        /* ideal time series arrays */
)

{
  char message[THD_MAX_NAME];  /* error message */
  int is;                  /* ideal index */
  int num_ort_files;       /* number of ort time series files */
  int num_ideal_files;     /* number of ideal time series files */
  int num_idealts;         /* number of ideal time series */
  int nt;                  /* number of images in input 3d+time dataset */
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int N;                   /* number of usable time points */
   int lncheck;
   byte LocalHead = 0;
 
  /*----- Initialize local variables -----*/
  if (option_data->input1D_filename != NULL)
    nt = fmri_length;
  else
    nt = DSET_NUM_TIMES (dset_time);

  num_ort_files   = option_data->num_ort_files;
  num_ideal_files = option_data->num_ideal_files;
  num_idealts     = option_data->num_idealts;


  NFirst = option_data->NFirst;

  NLast = option_data->NLast; 
  if (NLast > nt-1)  NLast = nt-1;
  option_data->NLast = NLast;

  N = NLast - NFirst + 1;
  option_data->N = N;


  /*----- Check number of ideal time series -----*/
  if (num_idealts < 1)  FIM_error ("No ideal time series?");


  /*----- If mask is used, check for compatible dimensions -----*/
  if (mask_dset != NULL)
    {
      if ( (DSET_NX(dset_time) != DSET_NX(mask_dset))
      || (DSET_NY(dset_time) != DSET_NY(mask_dset))
      || (DSET_NZ(dset_time) != DSET_NZ(mask_dset)) )
   {
     sprintf (message, "%s and %s have incompatible dimensions",
         option_data->input_filename, option_data->mask_filename);
     FIM_error (message);
   }

      if (DSET_NVALS(mask_dset) != 1 )
   FIM_error ("Must specify 1 sub-brick from mask dataset");
    }




  /*----- Check whether any of the output files already exist -----*/
  if( THD_deathcon() ) check_output_files (option_data, dset_time);

  /*----- Read in reference time series -----*/
   option_data->ln = option_data->NLast - option_data->NFirst + 1;
   option_data->rvec = (float *)    malloc (sizeof(float) * option_data->ln+1);       MTEST (option_data->rvec);

  /*------- Load Reference Time Series ------------------*/
if (LocalHead) fprintf(stderr,"Checking ref\n");
  lncheck = float_file_size (option_data->ideal_filename[0]);
  if (lncheck != nt)
     {
      printf("Error: Reference filename contains %d values.\n %d values were expected.\n", lncheck, nt);
      exit (1);
   }

if (LocalHead) fprintf(stderr,"Checking ref2\n");
   if (Read_part_file_delay (option_data->rvec, option_data->ideal_filename[0], option_data->NFirst,option_data->NLast) <= 0) {
      printf("Error: Reference filename could not be read or contain too few values.\n");
      exit (1);
   }

if (LocalHead) fprintf(stderr,"Bucket names\n");   
  /* --- decide on the bucket name ----*/
   if (option_data->bucket_filename == NULL)
   {
       option_data->bucket_filename = malloc (sizeof(char)*THD_MAX_NAME);
      MTEST (option_data->bucket_filename);
      sprintf (option_data->bucket_filename, "%s.DEL", DSET_PREFIX (dset_time));
      /*make sure that prefix is OK*/
      check_output_files (option_data, dset_time);
   }
     
  /* --- decide on the output name ----*/
   /* The log file is created no matter what */
   if (option_data->outname == NULL)
      {
         option_data->outnamelog = malloc (sizeof(char)*(THD_MAX_NAME+4));
         MTEST (option_data->outnamelog);
         sprintf (option_data->outnamelog, "%s.log", option_data->bucket_filename);
      }
   if (option_data->out || option_data->outts)
   {
      if (option_data->outname == NULL)
      {
         option_data->outname = malloc (sizeof(char)*THD_MAX_NAME);
         MTEST (option_data->outname);
         sprintf (option_data->outname, "%s", option_data->bucket_filename);
      }
      if (option_data->outts)
      {
         option_data->outnamets = malloc (sizeof(char)*(THD_MAX_NAME+3));
         MTEST (option_data->outnamets);
         sprintf (option_data->outnamets, "%s.ts", option_data->outname);
      }
   }

 /* check some options */
 if (option_data->scl != 1.0 && option_data->wrp) {
   printf("Error: -phzwrp is not good  with -phzscale not set to 1.0\n");
   exit (1); 
 }   
 
 /* ------- Open files for writing -------------*/
    
if (LocalHead) fprintf(stderr,"Output files\n");   
   option_data->outlogfile = fopen (option_data->outnamelog,"w"); /* open log file regardless */
   
   if (option_data->out == YUP)                           /* open outfile */
            {               
               option_data->outwrite = fopen (option_data->outname,"w");
               
               if (option_data->outts == YUP)
                  {
                     option_data->outwritets = fopen (option_data->outnamets,"w");
                     
                  }
               
               if ((option_data->outwrite == NULL) || (option_data->outlogfile == NULL) ||\
                   (option_data->outwritets == NULL && option_data->outts == YUP) )
                  {
                     printf ("\nCould not open ascii output files for writing\n");
                     exit (1);
                  }
   
            }
   
   /* Write out user variables to Logfile */
   write_ud (option_data);         /* writes user data to a file */

   #ifdef ZDBG
       show_ud(option_data, 1);
   #endif

}


/*---------------------------------------------------------------------------*/
/*
  Allocate memory for output volumes.
*/

void allocate_memory
(
  DELAY_options * option_data,  /* fim program options */
  THD_3dim_dataset * dset,    /* input 3d+time data set */
  float *** fim_params_vol    /* array of volumes containing fim parameters */
)

{
  int ip;                    /* parameter index */
  int nxyz;                  /* total number of voxels */
  int ixyz;                  /* voxel index */


  /*----- Initialize local variables -----*/
  nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;


  /*----- Allocate memory space for fim parameters -----*/
  *fim_params_vol  = (float **) malloc (sizeof(float *) * NBUCKETS); 
  MTEST(*fim_params_vol);

  for (ip = 0;  ip < NBUCKETS;  ip++)
    {
              (*fim_params_vol)[ip]  = (float *) malloc (sizeof(float) * nxyz);
              MTEST((*fim_params_vol)[ip]);
              for (ixyz = 0;  ixyz < nxyz;  ixyz++)
                {
                  (*fim_params_vol)[ip][ixyz]  = 0.0;
                }
    }

}




/*---------------------------------------------------------------------------*/
/*
  Perform all program initialization.
*/

void initialize_program
(
  int argc,                         /* number of input arguments */
  char ** argv,                     /* array of input arguments */
  DELAY_options ** option_data,       /* fim algorithm options */
  THD_3dim_dataset ** dset_time,    /* input 3d+time data set */
  THD_3dim_dataset ** mask_dset,    /* input mask data set */
  float ** fmri_data,               /* input fMRI time series data */
  int * fmri_length,                /* length of fMRI time series */
  MRI_IMAGE ** ort_array,           /* ort time series arrays */
  int ** ort_list,                  /* list of ort time series */
  MRI_IMAGE ** ideal_array,         /* ideal time series arrays */
  int ** ideal_list,                /* list of ideal time series */
  float *** fim_params_vol    /* array of volumes containing fim parameters */
)

{
  byte LocalHead = 0;

   if (LocalHead) fprintf(stderr,"In init prog.\n");
  /*----- Allocate memory -----*/
  *option_data = (DELAY_options *) malloc (sizeof(DELAY_options));

 
  /*----- Get command line inputs -----*/
  get_options (argc, argv, *option_data);


  /*----- Read input data -----*/
   if (LocalHead) fprintf(stderr,"Reading input\n");
   read_input_data (*option_data, dset_time, mask_dset, fmri_data, fmri_length,
         ort_array, ort_list, ideal_array, ideal_list);


  /*----- Check for valid inputs -----*/
  check_for_valid_inputs (*option_data, *dset_time, *mask_dset,
           *fmri_length, ort_array, ideal_array);


  /*----- Allocate memory for output volumes -----*/
   if (LocalHead) fprintf(stderr,"Allocation\n");
  if ((*option_data)->input1D_filename == NULL)
    allocate_memory (*option_data, *dset_time, fim_params_vol);

   if (LocalHead) fprintf(stderr,"returning from init prog.\n");
}


/*---------------------------------------------------------------------------*/
/*
  Get the time series for one voxel from the AFNI 3d+time data set.
*/

void extract_ts_array
(
  THD_3dim_dataset * dset_time,      /* input 3d+time dataset */
  int iv,                            /* get time series for this voxel */
  float * ts_array                   /* time series data for voxel #iv */
)

{
  MRI_IMAGE * im;          /* intermediate float data */
  float * ar;              /* pointer to float data */
  int ts_length;           /* length of input 3d+time data set */
  int it;                  /* time index */


  /*----- Extract time series from 3d+time data set into MRI_IMAGE -----*/
  im = THD_extract_series (iv, dset_time, 0);


  /*----- Verify extraction -----*/
  if (im == NULL)  FIM_error ("Unable to extract data from 3d+time dataset");


  /*----- Now extract time series from MRI_IMAGE -----*/
  ts_length = DSET_NUM_TIMES (dset_time);
  ar = MRI_FLOAT_PTR (im);
  for (it = 0;  it < ts_length;  it++)
    {
      ts_array[it] = ar[it];
    }


  /*----- Release memory -----*/
  mri_free (im);   im = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Save results for this voxel.
*/

void save_voxel
(
  int iv,                      /* current voxel index */ 
  float * fim_params,          /* array of fim parameters */
  float ** fim_params_vol      /* array of volumes of fim output parameters */
)

{
  int ip;                   /* parameter index */


  /*----- Saved user requested fim parameters -----*/
  for (ip = 0;  ip < NBUCKETS;  ip++)
    {
      if (fim_params_vol[ip] != NULL)
   fim_params_vol[ip][iv]  = fim_params[ip];
    }

}


/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/*
  Calculate the response delay for each voxel.
*/

void calculate_results
(
  DELAY_options * option_data,  /* dela program options */
  THD_3dim_dataset * dset,    /* input 3d+time data set */
  THD_3dim_dataset * mask,    /* input mask data set */
  float * fmri_data,          /* input fMRI time series data */
  int fmri_length,            /* length of fMRI time series */
  MRI_IMAGE ** ort_array,     /* ort time series arrays */
  int ** ort_list,            /* list of ort time series */
  MRI_IMAGE ** ideal_array,   /* ideal time series arrays */
  int ** ideal_list,          /* list of ideal time series */
  float ** fim_params_vol     /* array of volumes of fim output parameters */
)

{
  float * ts_array = NULL;    /* array of measured data for one voxel */
  float mask_val[1];          /* value of mask at current voxel */

  int q=-1;                      /* number of parameters in the baseline model */
  int p=-1;                      /* number of parameters in the baseline model
                  plus number of ideals */
  int m=-1;                      /* parameter index */
  int n=-1;                      /* data point index */


   matrix xdata;               /* independent variable matrix */
   matrix x_base;              /* extracted X matrix    for baseline model */
   matrix xtxinvxt_base;       /* matrix:  (1/(X'X))X'  for baseline model */
   matrix x_ideal[MAX_FILES];  /* extracted X matrices  for ideal models */
   matrix xtxinvxt_ideal[MAX_FILES];
                              /* matrix:  (1/(X'X))X'  for ideal models */
   vector y;                   /* vector of measured data */

   int ixyz=-1;                   /* voxel index */
   int nxyz=-1;                   /* number of voxels per dataset */

   int nt=-1;                  /* number of images in input 3d+time dataset */
   int NFirst=-1;              /* first image from input 3d+time dataset to use */
   int NLast=-1;               /* last image from input 3d+time dataset to use */
   int N=-1;                   /* number of usable data points */

   int num_ort_files=-1;       /* number of ort time series files */
   int num_ideal_files=-1;     /* number of ideal time series files */
   int polort=-1;              /* degree of polynomial ort */
   int num_ortts=-1;           /* number of ort time series */
   int num_idealts=-1;         /* number of ideal time series */

   int i=-1;                   /* data point index */
   int is=-1;                  /* ideal index */
   int ilag=-1;                /* time lag index */
   float stddev=-1.0;            /* normalized parameter standard deviation */
   char * label=NULL;            /* string containing stat. summary of results */
   int xpos=-1, ypos=-1, zpos=-1;
   double tzero=0.0 , tdelta=0.0 , ts_mean=0.0 , ts_slope=0.0;
   int   ii=-1,  iz=-1,izold=-1, nxy=-1, nuse=-1, use_fac=-1, kk=-1;
   float * x_bot = NULL;    /* minimum of stimulus time series */
   float * x_ave = NULL;    /* average of stimulus time series */
   float * x_top = NULL;    /* maximum of stimulus time series */
   int * good_list = NULL;  /* list of good (usable) time points */
   float ** rarray = NULL;  /* ranked arrays of ideal time series */
   float FimParams[NBUCKETS];  /* output delay parameters */
   float *  fac  = NULL ;  /* array of input brick scaling factors */
   float * vox_vect = NULL;          /* voxel time series */
   float *ref_ts=NULL; /*reference time series */
   float slp=0.0, delu=0.0, del=0.0,  xcor=0.0, xcorCoef=0.0,vts=0.0,
          vrvec=0.0, dtx=0.0 , x0=0.0,x1=0.0;
   int  actv=-1, opt=-1, iposdbg=-1;
   
   for (i=0;i<NBUCKETS;++i) FimParams[i]=-1.0;
   
   #ifdef ZDBG
      xyzTOindex (option_data, &iposdbg, IPOSx,  IPOSy , IPOSz);
      printf ("Debug for %d: %d, %d, %d\n\n", iposdbg, IPOSx,  IPOSy , IPOSz);
   #endif
 

  /*----- Initialize matrices and vectors -----*/
  matrix_initialize (&xdata);
  matrix_initialize (&x_base);
  matrix_initialize (&xtxinvxt_base);
  for (is =0;  is < MAX_FILES;  is++)
    {
      matrix_initialize (&x_ideal[is]);
      matrix_initialize (&xtxinvxt_ideal[is]);
    }
  vector_initialize (&y);


  /*----- Initialize local variables -----*/
  if (option_data->input1D_filename != NULL)
    {
      nxyz = 1;
      nt = fmri_length;
    }
  else
    {
      nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;
      nt = DSET_NUM_TIMES (dset);
    }

  NFirst = option_data->NFirst;
  NLast = option_data->NLast; 
  N = option_data->N;
  p = option_data->p;
  q = option_data->q;

  polort          = option_data->polort;
  num_idealts     = option_data->num_idealts;
  num_ort_files   = option_data->num_ort_files;
  num_ideal_files = option_data->num_ideal_files;


  /*----- Allocate memory -----*/
  ts_array = (float *) malloc (sizeof(float) * nt);      MTEST (ts_array);
  x_bot = (float *)    malloc (sizeof(float) * p);       MTEST (x_bot);
  x_ave = (float *)    malloc (sizeof(float) * p);       MTEST (x_ave);
  x_top = (float *)    malloc (sizeof(float) * p);       MTEST (x_top);
  good_list = (int *)  malloc (sizeof(int) * N);         MTEST (good_list);
  rarray = (float **)  malloc (sizeof(float *) * num_idealts);  MTEST (rarray);
  vox_vect = (float *)    malloc (sizeof(float) * nt);       MTEST (vox_vect);

  /*----- Initialize the independent variable matrix -----*/
  N = init_indep_var_matrix (q, p, NFirst, N, polort,
              num_ort_files, num_ideal_files,
              ort_array, ort_list, ideal_array, ideal_list,
              x_bot, x_ave, x_top, good_list, &xdata);
  option_data->N = N;




  /*----- Initialization for the regression analysis -----*/
  init_delay (q, p, N, num_idealts, xdata, &x_base,
             &xtxinvxt_base, x_ideal, xtxinvxt_ideal, rarray);


  vector_create (N, &y);

   /*----- set up to find time at each voxel -----*/

   tdelta = dset->taxis->ttdel ;
   if( DSET_TIMEUNITS(dset) == UNITS_MSEC_TYPE ) tdelta *= 0.001 ;
   if( tdelta == 0.0 ) tdelta = 1.0 ;
   izold  = -666 ;
   nxy    = dset->daxes->nxx * dset->daxes->nyy ;


   /*--- get scaling factors for input sub-bricks ---*/
   nuse      = option_data->NLast - option_data->NFirst + 1;
   fac = (float *) malloc( sizeof(float) * nuse ) ;   /* factors */ MTEST (fac);
 

   use_fac = 0 ;
   for( kk=0 ; kk < nuse ; kk++ ){
      fac[kk] = DSET_BRICK_FACTOR(dset,kk+option_data->NFirst) ;
      if( fac[kk] != 0.0 ) use_fac++ ;
      else                 fac[kk] = 1.0 ;
   }
   if( !use_fac ) FREEUP(fac) ;

  /*----- Loop over all voxels -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      #ifdef ZDBG
         if (ixyz == iposdbg)
            {
               printf ("\nAt voxel, checking for mask\n");
            }
      #endif

      indexTOxyz ( option_data , ixyz, &xpos , &ypos , &zpos);
      /*----- Apply mask? -----*/
      if (mask != NULL)
         {
           extract_ts_array (mask, ixyz, mask_val);
           if (mask_val[0] == 0.0)  continue;
         }
      
      #ifdef ZDBG
         if (ixyz == iposdbg)
            {
            printf ("Past the mask, extracting TS\n");
            }
      #endif


     /*----- Extract Y-data for this voxel -----*/
      
 
      if (option_data->input1D_filename != NULL)
         {
           for (i = 0;  i < N;  i++)
             vox_vect[i] = (float)fmri_data[good_list[i]+NFirst];
         }
      else
         {
           extract_ts_array (dset, ixyz, ts_array);
           for (i = 0;  i < N;  i++)
             vox_vect[i] = (float)ts_array[good_list[i]+NFirst];
         }
      #ifdef ZDBG
         if (ixyz == 34 + 34 * 64 + 6 * (64*64)) {
            fprintf(stderr,"\nigood (nuse=%d, N = %d)\n", nuse, N);
            for (i= 0;  i < N;  i++)
               fprintf(stderr,"%d\t", good_list[i]);
            fprintf(stderr,"\n");
            fprintf(stderr,"\nts\n");
            for (i= 0;  i < N;  i++)
               fprintf(stderr,"%.3f\t", vox_vect[i]);
            fprintf(stderr,"\n");
         }
         if (ixyz == iposdbg)
            {
               printf ("TS extracted\n");
            }
      #endif


   
   opt = 1; /* set to 0 for cleanup */
   
      /*** scale? ***/

      #ifdef ZDBG
      if (ixyz == iposdbg)
         {
            printf("Before Scale\n");
            printf("TS: %f\t%f\t%f\t%f\t%f\n", vox_vect[0], vox_vect[1],  vox_vect[2], vox_vect[3], vox_vect[4]);
            /*getchar ();*/
         }
      #endif
         
      if( use_fac )
         for( kk=0 ; kk < nuse ; kk++ ) vox_vect[kk] *= fac[kk] ;
 
      /* calculate the T0 and Tdelta */
      /** compute start time of this timeseries **/

      iz = ixyz / nxy ;    /* which slice am I in? */

      if( iz != izold ){          /* in a new slice? */
         tzero = THD_timeof( option_data->NFirst ,
                             dset->daxes->zzorg
                           + iz*dset->daxes->zzdel , dset->taxis ) ;
         izold = iz ;

         if( DSET_TIMEUNITS(dset) == UNITS_MSEC_TYPE ) tzero *= 0.001 ;

     if (option_data->Dsamp == YUP)
            dtx = (float) (tzero / tdelta) - option_data->NFirst;
         else
            dtx = 0.0;
      }
   
      #ifdef ZDBG
      if (ixyz == iposdbg)
         {
            printf("dtx = %f\n", dtx);
         }
      #endif

    option_data->errcode = hilbertdelay_V2 (vox_vect, option_data->rvec,option_data->ln,option_data->Nseg,option_data->Pover,opt,0,dtx,option_data->biasrem,&delu,&slp,&xcor,&xcorCoef,&vts,&vrvec);               /* cleanup time */
      #ifdef ZDBG
      if (ixyz == iposdbg)
         {
            printf ("%d err code @ixyz = %d\n", option_data->errcode, ixyz);
         }
      #endif
   if (option_data->errcode == 0) /* If there are no errors, proceed */
      { /*option_data->errcode == 0 inner loop */
               hunwrap (delu, (float)(option_data->fs), option_data->T, slp, option_data->wrp, option_data->unt, option_data->rev, option_data->scl, &del );
               
               actv = 1;                  /* assume voxel is active */
   
               if (xcorCoef < option_data->co) actv = 0;         /* determine if voxel is activated using xcorCoef  */
   
               if ((actv == 1) && (option_data->out == YUP))       /* if voxel is truly activated, write results to file without modifying return value */
                  {
                     indexTOxyz ( option_data , ixyz, &xpos , &ypos , &zpos);
                     fprintf (option_data->outwrite,"%d\t%d\t%d\t%d\t%f\t%f\t%f\t%f\t%f\n", ixyz , xpos , ypos , zpos ,  delu , del , xcor , xcorCoef , vts);
                     if (option_data->outts == YUP)
                        {
                           writets (option_data, vox_vect);   
                        }
                  }

      }/*option_data->errcode == 0 inner loop */
         
   else if (option_data->errcode == ERROR_LONGDELAY)
            {               
               error_report ( option_data , ixyz);   

               del = 0.0;                        /* Set all the variables to Null and don't set xcorCoef to an impossible value*/
               xcorCoef = 0.0;                  /*  because the data might still be OK */
               xcor = 0.0;

            }
         else if (option_data->errcode != 0)
            {
               error_report ( option_data , ixyz);   

               del = 0.0;                        /* Set all the variables to Null and set xcorCoef to an impossible value*/
               xcorCoef = NOWAYXCORCOEF;                  
               xcor = 0.0;
            }   
   
   
      FimParams[DELINDX] = del;
      FimParams[COVINDX] = xcor;
      FimParams[COFINDX] = xcorCoef;
      FimParams[VARINDX] = vts;
   
      #ifdef ZDBG
      if (ixyz == iposdbg)
         {
            printf("VI\tX\tY\tZ\tDuff\tDel\tCov\txCorCoef\tVTS\n");
            printf("%d\t%d\t%d\t%d\t", ixyz, xpos, ypos, zpos);
             printf("%f\t%f\t%f\t%f\t%f\n", delu, del, xcor, xcorCoef, vts);
            printf("TS: %f\t%f\t%f\t%f\t%f\n", vox_vect[0], vox_vect[1],  vox_vect[2], vox_vect[3], vox_vect[4]);
            printf("%f\t%f\t%f\t%f\t%f\n", dtx, delu, del, xcor, xcorCoef);
            /*getchar ();*/
         }
      #endif
   
      /*----- Save results for this voxel -----*/
      if (option_data->input1D_filename == NULL)
   save_voxel (ixyz, FimParams, fim_params_vol);
 
 
 
    }  /*----- Loop over voxels -----*/


   if (option_data->out == YUP)                           /* close outfile and outlogfile*/
            {
               fclose (option_data->outlogfile);
               fclose (option_data->outwrite);
               if (option_data->outts  == YUP) fclose (option_data->outwritets);
            }
            else
            {
               if (option_data->outlogfile != NULL)   fclose (option_data->outlogfile);      /* close outlogfile */
            }


  /*----- Dispose of matrices and vectors -----*/
  vector_destroy (&y);
  for (is = 0;  is < MAX_FILES;  is++)
    {
      matrix_destroy (&xtxinvxt_ideal[is]);
      matrix_destroy (&x_ideal[is]);
    }
  matrix_destroy (&xtxinvxt_base);
  matrix_destroy (&x_base);
  matrix_destroy (&xdata);


  /*----- Deallocate memory -----*/
  free (ts_array);     ts_array = NULL;
  free (x_bot);        x_bot = NULL;
  free (x_ave);        x_ave = NULL;
  free (x_top);        x_top = NULL;
  free (good_list);    good_list = NULL;
  free (vox_vect);       vox_vect = NULL;

  for (is = 0;  is < num_idealts;  is++)
    {
      free (rarray[is]);   rarray[is] = NULL;
    }
  free (rarray);       rarray = NULL;

}

/*---------------------------------------------------------------------------*/
/*
  Attach one sub-brick to output bucket data set.
*/

void attach_sub_brick
(
  THD_3dim_dataset * new_dset,      /* output bucket dataset */
  int ibrick,               /* sub-brick indices */
  float * volume,           /* volume of floating point data */
  int nxyz,                 /* total number of voxels */
  int brick_type,           /* indicates statistical type of sub-brick */
  char * brick_label,       /* character string label for sub-brick */
  int nsam,
  int nfit,
  int nort,                 /* degrees of freedom */
  short ** bar              /* bar[ib] points to data for sub-brick #ib */
)

{
  const float EPSILON = 1.0e-10;
  float factor;             /* factor is new scale factor for sub-brick #ib */


  /*----- allocate memory for output sub-brick -----*/
  bar[ibrick]  = (short *) malloc (sizeof(short) * nxyz);
  MTEST (bar[ibrick]);
  factor = EDIT_coerce_autoscale_new (nxyz, MRI_float, volume,
                  MRI_short, bar[ibrick]);

  if (factor < EPSILON)  factor = 0.0;
  else factor = 1.0 / factor;

  EDIT_misfit_report( DSET_FILECODE(new_dset) , ibrick ,
                      nxyz , factor , bar[ibrick] , volume ) ;

  /*----- edit the sub-brick -----*/
  EDIT_BRICK_LABEL (new_dset, ibrick, brick_label);
  EDIT_BRICK_FACTOR (new_dset, ibrick, factor);

  if (brick_type == FUNC_COR_TYPE)
    EDIT_BRICK_TO_FICO (new_dset, ibrick, nsam, nfit, nort);


  /*----- attach bar[ib] to be sub-brick #ibrick -----*/
  EDIT_substitute_brick (new_dset, ibrick, MRI_short, bar[ibrick]);

}

/*---------------------------------------------------------------------------*/
/*
  Routine to write one bucket data set.
*/

void write_bucket_data
(
  int argc,                         /* number of input arguments */
  char ** argv,                     /* array of input arguments */
  DELAY_options * option_data,        /* fim program options */
  float ** fim_params_vol      /* array of volumes of fim output parameters */
)

{
  THD_3dim_dataset * old_dset = NULL;      /* prototype dataset */
  THD_3dim_dataset * new_dset = NULL;      /* output bucket dataset */
  char output_prefix[THD_MAX_NAME];     /* prefix name for bucket dataset */
  char output_session[THD_MAX_NAME];    /* directory for bucket dataset */
  int nbricks;              /* number of sub-bricks in bucket dataset */
  short ** bar = NULL;      /* bar[ib] points to data for sub-brick #ib */

  int brick_type;           /* indicates statistical type of sub-brick */
  int brick_coef;           /* regression coefficient index for sub-brick */
  char brick_label[THD_MAX_NAME]; /* character string label for sub-brick */

  int ierror;               /* number of errors in editing data */
  float * volume;           /* volume of floating point data */

  int N;                    /* number of usable data points */
  int q;                    /* number of parameters in the ideal model */
  int num_idealts;           /* number of ideal time series */
  int ip;                   /* parameter index */
  int nxyz;                 /* total number of voxels */
  int ibrick;               /* sub-brick index */
  int nsam;
  int nfit;
  int nort;                 /* degrees of freedom */
  char label[THD_MAX_NAME];   /* general label for sub-bricks */


  /*----- read prototype dataset -----*/
  old_dset = THD_open_one_dataset (option_data->input_filename);


  /*----- Initialize local variables -----*/
  nxyz = old_dset->daxes->nxx * old_dset->daxes->nyy * old_dset->daxes->nzz;
  num_idealts = option_data->num_idealts;
  q = option_data->q;
  N = option_data->N;


  /*----- Calculate number of sub-bricks in the bucket -----*/
  nbricks = NBUCKETS;

  strcpy (output_prefix, option_data->bucket_filename);
  strcpy (output_session, "./");


  /*----- allocate memory -----*/
  bar  = (short **) malloc (sizeof(short *) * nbricks);
  MTEST (bar);


  /*-- make an empty copy of prototype dataset, for eventual output --*/
  new_dset = EDIT_empty_copy (old_dset);


  /*----- Record history of dataset -----*/
  tross_Copy_History( old_dset , new_dset ) ;
  tross_Make_History( PROGRAM_NAME , argc , argv , new_dset ) ;
  sprintf (label, "Output prefix: %s", output_prefix);
  tross_Append_History ( new_dset, label);


  /*----- delete prototype dataset -----*/
  THD_delete_3dim_dataset( old_dset , False );  old_dset = NULL ;


  /*----- Modify some structural properties.  Note that the nbricks
          just make empty sub-bricks, without any data attached. -----*/
  ierror = EDIT_dset_items (new_dset,
                            ADN_prefix,          output_prefix,
             ADN_directory_name,  output_session,
             ADN_type,            HEAD_FUNC_TYPE,
             ADN_func_type,       FUNC_BUCK_TYPE,
             ADN_datum_all,       MRI_short , 
                            ADN_ntt,             0,               /* no time */
             ADN_nvals,           nbricks,
             ADN_malloc_type,     DATABLOCK_MEM_MALLOC ,
             ADN_none ) ;

  if( ierror > 0 )
    {
      fprintf(stderr,
         "*** %d errors in attempting to create bucket dataset!\n",
         ierror);
      exit(1);
    }

  if (!THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(new_dset)))
    {
      fprintf(stderr,
         "*** Output dataset file %s already exists--cannot continue!\n",
         DSET_HEADNAME(new_dset));
      exit(1);
    }


  /*----- Attach individual sub-bricks to the bucket dataset -----*/
  ibrick = 0;
  for (ip = 0;  ip < NBUCKETS;  ip++)
    {                       
      strcpy (brick_label, DELAY_OUTPUT_TYPE_name[ip]);

      if (ip == COFINDX)
         {
           brick_type = FUNC_COR_TYPE;
           nsam = option_data->ln;  nort = option_data->polort;
           nfit = option_data->Nfit;
         }
      else
         {
           brick_type = FUNC_FIM_TYPE;
           nsam = 0; nfit = 0; nort = 0;
         }

      volume = fim_params_vol[ip];      
      attach_sub_brick (new_dset, ibrick, volume, nxyz,
         brick_type, brick_label, nsam, nfit, nort, bar);

      ibrick++;
    }


  /*----- write bucket data set -----*/
  if( !AFNI_noenv("AFNI_AUTOMATIC_FDR") ) {
     if ((ip = THD_create_all_fdrcurves( new_dset )) != 1) {
      fprintf(stderr,"*** Failed to add fdr curves! (%d)\n", ip);
     }else{
      fprintf(stderr,"+++ added %d fdrcurves\n",ip);
     }
  }

  THD_load_statistics (new_dset);
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  fprintf(stderr,"Wrote bucket dataset ");
  fprintf(stderr,"into %s\n", DSET_PREFIX(new_dset));


  /*----- deallocate memory -----*/ 
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;

}


/*---------------------------------------------------------------------------*/
/*
  Write out the user requested output files.
*/

void output_results
(
  int argc,                         /* number of input arguments */
  char ** argv,                     /* array of input arguments */
  DELAY_options * option_data,        /* fim algorithm options */
  float ** fim_params_vol      /* array of volumes of fim output parameters */
)

{
  int q;                    /* number of parameters in baseline model */
  int num_idealts;           /* number of ideal time series */
  int ib;                   /* sub-brick index */
  int is;                   /* ideal index */
  int ts_length;            /* length of impulse reponse function */
  int N;                    /* number of usable data points */


  /*----- Initialize local variables -----*/
  q = option_data->polort + 1;
  num_idealts = option_data->num_idealts;
  N = option_data->N;


  /*----- Write the bucket dataset -----*/
  if (option_data->bucket_filename != NULL)
  {
    write_bucket_data (argc, argv, option_data, fim_params_vol);
   }

}

/* ************************************************************ */
/* function to display user data input (debugging stuff)        */
/* ************************************************************ */

void show_ud (struct DELAY_options* option_data,int loc)
   {
      printf ("\n\nUser Data Values at location :%d\n",loc);
      printf ("option_data->rvec= (1st five elements only)");
      disp_vect (option_data->rvec,5);
      printf ("Input Data Set: %s\n", option_data->input_filename);
      printf ("ASCII output file: %s\n", option_data->outname);
      printf ("ASCII output file (log): %s\n", option_data->outnamelog);
      printf ("ASCII output file (ts): %s\n", option_data->outnamets);
      printf ("Mask Data Set: %s\n", option_data->mask_filename);
      printf ("Reference File Name: %s\n", option_data->ideal_filename[0]);
      printf ("Number of voxels in X direction: %d\n", option_data->nxx);
      printf ("Number of voxels in Y direction: %d\n", option_data->nyy);
      printf ("Number of voxels in Z direction: %d\n", option_data->nzz);
      printf ("option_data->fs= %f\n",option_data->fs);
      printf ("option_data->T= %f\n",option_data->T);
      printf ("option_data->unt= %d\n",option_data->unt);
      printf ("option_data->wrp= %d\n",option_data->wrp);
      printf ("option_data->rev= %d\n",option_data->rev);
      printf ("option_data->scl= %f\n",option_data->scl);
      printf ("option_data->Navg= %d\n",option_data->Navg);
      printf ("option_data->Nort (polort)= %d\n",option_data->polort);
      printf ("option_data->Nfit= %d\n",option_data->Nfit);
      printf ("option_data->Nseg= %d\n",option_data->Nseg);
      printf ("option_data->Pover= %d\n",option_data->Pover);
      if (option_data->polort > 1) {
         printf ("option_data->dtrnd= 1\n");
      } else {
         printf ("option_data->dtrnd= 0\n");
      }
      printf ("option_data->biasrem= %d\n",option_data->biasrem);
      printf ("option_data->Dsamp= %d\n",option_data->Dsamp);
      printf ("option_data->co= %f\n",option_data->co);
      printf ("option_data->ln= %d\n",option_data->ln);
      printf ("option_data->errcode= %d\n",option_data->errcode);
      printf ("option_data->out= %d\n",option_data->out);
      printf ("option_data->outts= %d\n",option_data->outts);
      printf ("Hit enter to continue\a\n\n");
      getchar ();
      return;
   }

/* ************************************************************ */
/* function to write user data input to log file        */
/* ************************************************************ */
void write_ud (struct DELAY_options* option_data)
   {
      fprintf (option_data->outlogfile,"\nLogfile output by Hilbert Delay98 plugin\n");
      fprintf (option_data->outlogfile,"\n\nUser Data Values \n");

                /* check for NULL filenames          22 July 2005 [rickr] */
      fprintf (option_data->outlogfile,"Input Data Set: %s\n",
          CHECK_NULL_STR(option_data->input_filename));
      fprintf (option_data->outlogfile,"Mask Data Set: %s\n",
          CHECK_NULL_STR(option_data->mask_filename));
      fprintf (option_data->outlogfile,"Ascii output file name: %s\n",
          CHECK_NULL_STR(option_data->outname));
      fprintf (option_data->outlogfile,"Reference File Name: %s\n",
          CHECK_NULL_STR(option_data->ideal_filename[0]));

      fprintf (option_data->outlogfile,"Number of voxels in X direction: %d\n", option_data->nxx);
      fprintf (option_data->outlogfile,"Number of voxels in Y direction: %d\n", option_data->nyy);
      fprintf (option_data->outlogfile,"Number of voxels in Z direction: %d\n", option_data->nzz);
      fprintf (option_data->outlogfile,"Sampling Frequency = %f\n",option_data->fs);
      fprintf (option_data->outlogfile,"Stimulus Period = %f\n",option_data->T);
      fprintf (option_data->outlogfile,"Delay units = %d\n",option_data->unt);
      fprintf (option_data->outlogfile,"Delay wrap = %d\n",option_data->wrp);
      fprintf (option_data->outlogfile,"Delay reverse = %d\n",option_data->rev);
      fprintf (option_data->outlogfile,"Delay scale = %f\n",option_data->scl);
      fprintf (option_data->outlogfile,"Number of segments = %d\n",option_data->Nseg);
      fprintf (option_data->outlogfile,"Length of reference time series = %d\n",option_data->ln);
      fprintf (option_data->outlogfile,"Number of fit parameters = %d\n",option_data->Nfit);
      fprintf (option_data->outlogfile,"Number of nuisance parameters (orts)= %d\n",option_data->polort);
      fprintf (option_data->outlogfile,"Percent overlap = %d\n",option_data->Pover);
      fprintf (option_data->outlogfile,"Plugin detrending = (Always 0, mandatory detrending is performed)\n");
      fprintf (option_data->outlogfile,"Bias correction = %d\n",option_data->biasrem);
      fprintf (option_data->outlogfile,"Acquisition time correction = %d\n",option_data->Dsamp);
      fprintf (option_data->outlogfile,"Correlation Coefficient Threshold= %f\n",option_data->co);
      fprintf (option_data->outlogfile,"Flag for Ascii output file  = %d\n",option_data->out);
      fprintf (option_data->outlogfile,"Flag for Ascii time series file output = %d\n",option_data->outts);
      fprintf (option_data->outlogfile,"\noption_data->errcode (debugging only)= %d\n\n",option_data->errcode);
      fprintf (option_data->outlogfile,"\nThe format for the output file is the following:\n");
      fprintf (option_data->outlogfile,"VI\tX\tY\tZ\tDuff\tDel\tCov\txCorCoef\tVTS\n");
      fprintf (option_data->outlogfile,"\nError Log <message> <index> <x> <y> <z>\n\n");
      
      return;
   }

/* ************************************************************ */
/* function to compute x, y, z coordinates from the index       */
/* ************************************************************ */

void indexTOxyz (struct DELAY_options* option_data, int ncall, int *xpos , int *ypos , int *zpos)     
   {
      *zpos = (int)ncall / (int)(option_data->nxx*option_data->nyy);
      *ypos = (int)(ncall - *zpos * option_data->nxx * option_data->nyy) / (int)option_data->nxx;
      *xpos = ncall - ( *ypos * option_data->nxx ) - ( *zpos * option_data->nxx * option_data->nyy ) ;
      return;
   }

void xyzTOindex (struct DELAY_options* option_data, int *ncall, int xpos , int ypos , int zpos)     
   {
      *ncall = zpos * ( option_data->nxx*option_data->nyy ) + ypos * option_data->nxx + xpos;
      return;
   }
   
/* ************************************************************ */
/* function to report errors encountered to the logfile         */
/* Only errors that happen during runtime (while delays are     */
/* computed, are handeled here, the others are handeled         */
/* instantaneously, and need not be logged                       */
/* ************************************************************ */

void error_report (struct DELAY_options* option_data, int ncall )
   {
      int xpos,ypos,zpos;
      indexTOxyz (option_data, ncall, &xpos , &ypos , &zpos);

      switch (option_data->errcode)
         {
            case ERROR_NOTHINGTODO:
               fprintf (option_data->outlogfile,"Nothing to do hilbertdelay_V2 call ");
               break;
            case ERROR_LARGENSEG:
               fprintf (option_data->outlogfile,"Number of segments Too Large ");
               break;
            case ERROR_LONGDELAY:
               fprintf (option_data->outlogfile,"Could not find zero crossing before maxdel limit ");
               break;
            case ERROR_SERIESLENGTH:
               fprintf (option_data->outlogfile,"Vectors have different length ");
               break;
            case ERROR_NULLTIMESERIES:
               fprintf (option_data->outlogfile,"Null time series vector ");
               break;
            default:
               fprintf (option_data->outlogfile,"De Fault, De Fault (%d), the two sweetest words in the english langage ! ",option_data->errcode);
               break;
         }   
      fprintf (option_data->outlogfile,"%d\t%d\t%d\t%d\t\n", ncall , xpos , ypos , zpos  );
      return;
   }
   
/* *************************************************************** */
/* function to write the time course into a line in the given file */
/* *************************************************************** */

void writets (struct DELAY_options * option_data,float * ts)

   {   
      int i;
      
      for (i=0;i<option_data->ln;++i)
         {
            fprintf (option_data->outwritets, "%f\t",ts[i]);
         }
      fprintf (option_data->outwritets,"\n");
   }

/*---------------------------------------------------------------------------*/

void terminate_program
(
  DELAY_options ** option_data,   /* fim program options */
  MRI_IMAGE ** ort_array,           /* ort time series arrays */
  int ** ort_list,                  /* list of ort time series */
  MRI_IMAGE ** ideal_array,         /* ideal time series arrays */
  int ** ideal_list,                /* list of ideal time series */
  float *** fim_params_vol      /* array of volumes of fim output parameters */
)

{
  int num_idealts;           /* number of ideal time series */
  int ip;                   /* parameter index */
  int is;                   /* ideal index */


  /*----- Initialize local variables -----*/
  num_idealts = (*option_data)->num_idealts;


  /*----- Deallocate memory for option data -----*/ 
  free (*option_data);  *option_data = NULL;


  /*----- Deallocate memory for ideal time series -----*/
  /*
  for (is = 0;  is < num_idealts;  is++)
    { free (ideal[is]);  ideal[is] = NULL; }
  */


  /*----- Deallocate space for volume data -----*/
  if (*fim_params_vol != NULL)
    {
      for (ip = 0;  ip < NBUCKETS;  ip++)
   {
     if ((*fim_params_vol)[ip] != NULL)
       { free ((*fim_params_vol)[ip]);   (*fim_params_vol)[ip] = NULL; }
   }

      free (*fim_params_vol);   *fim_params_vol  = NULL;
    }
   
      

}


/*---------------------------------------------------------------------------*/

int main
(
  int argc,                /* number of input arguments */
  char ** argv             /* array of input arguments */
)

{
  DELAY_options * option_data;              /* fim algorithm options */
  THD_3dim_dataset * dset_time = NULL;    /* input 3d+time data set */
  THD_3dim_dataset * mask_dset = NULL;    /* input mask data set */
  float * fmri_data = NULL;               /* input fMRI time series data */
  int fmri_length;                        /* length of fMRI time series */
  MRI_IMAGE * ort_array[MAX_FILES];       /* ideal time series arrays */
  int * ort_list[MAX_FILES];              /* list of ideal time series */
  MRI_IMAGE * ideal_array[MAX_FILES];     /* ideal time series arrays */
  int * ideal_list[MAX_FILES];            /* list of ideal time series */

  float ** fim_params_vol = NULL;
                                /* array of volumes of fim output parameters */
  byte LocalHead = 0;

  /*----- Identify software -----*/
#if 0
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR);
  printf ("Date:    %s \n", PROGRAM_DATE);
  printf ("\n");
#endif

   PRINT_VERSION("3ddelay") ; AUTHOR(PROGRAM_AUTHOR) ;
   mainENTRY("3ddelay main") ; machdep() ;
   AFNI_logger("3ddelay",argc,argv) ;

  /*----- Program initialization -----*/
  if (LocalHead) fprintf(stderr,"Initializing ...");
  initialize_program (argc, argv, &option_data, &dset_time, &mask_dset,
            &fmri_data, &fmri_length,
            ort_array, ort_list, ideal_array, ideal_list,
            &fim_params_vol);
  if (LocalHead) fprintf(stderr,"\n");


  /*----- Perform fim analysis -----*/
  if (LocalHead) fprintf(stderr,"Calculating ...");
  calculate_results (option_data, dset_time, mask_dset,
           fmri_data, fmri_length,
           ort_array, ort_list, ideal_array, ideal_list,
           fim_params_vol);
  if (LocalHead) fprintf(stderr,"\n");

  /*----- Deallocate memory for input datasets -----*/ 
  if (dset_time != NULL)
    { THD_delete_3dim_dataset (dset_time, False);  dset_time = NULL; }
  if (mask_dset != NULL)
    { THD_delete_3dim_dataset (mask_dset, False);  mask_dset = NULL; }


  /*----- Write requested output files -----*/
  if (LocalHead) fprintf(stderr,"Writing results...");
  if (option_data->input1D_filename == NULL)
    output_results (argc, argv, option_data, fim_params_vol);
  if (LocalHead) fprintf(stderr,"\n");


  /*----- Terminate program -----*/
  terminate_program (&option_data,
           ort_array, ort_list, ideal_array, ideal_list,
           &fim_params_vol);

}
