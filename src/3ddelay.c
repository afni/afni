/*---------------------------------------------------------------------------*/
/*
  Program to calculate the delay between FMRI time series and the the ideal 
  reference time series. 
  This program, which is a command line version of the plugin Hilbert Delay 98, 
  uses Doug Ward's 3dfim+'s skeleton to read and write the bricks. 
*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3ddelay"                        /* name of this program */
#define PROGRAM_AUTHOR "Ziad Saad (using B. Douglas Ward's 3dfim+ to read and write bricks)"  /* program author */
#define PROGRAM_DATE "Dec 8 2000"               /* date of last program mod */

/*---------------------------------------------------------------------------*/

#define MAX_FILES 20                        /* maximum number of ideal files */
                                            /* = maximum number of ort files */
#define RA_error FIM_error

/*---------------------------------------------------------------------------*/

#include "mrilib.h"
#include "matrix.h"

#include "delay.c"


/*--------------------- strings for output format --------------------*/

static char * method_strings[] = { "Seconds" , "Degrees" , "Radians"} ;
static char * yn_strings[] = { "n" , "y" }; 

/*#define ZDBG*/
#ifdef ZDBG
	#define IPOSx 8 
	#define IPOSy 38
	#define IPOSz 7
#endif
 
#define NUM_METHOD_STRINGS (sizeof(method_strings)/sizeof(char *))
#define NUM_YN_STRINGS (sizeof(yn_strings)/sizeof(char *))

#define METH_SECONDS 0
#define METH_RADIANS 1
#define METH_DEGREES 2

#define DELAY    0
#define XCOR     1
#define XCORCOEF 2
#ifndef NOWAYXCORCOEF
	#define NOWAYXCORCOEF 0					/* A flag value indicating that something lethal went on */
#endif


#define NBUCKETS 4				/* Number of values per voxel in Buket data set */
#define DELINDX 0					/* index of delay value in results vector */
#define COVINDX 1					/* index of covariance value in results vector */
#define COFINDX 2					/* index of cross correlation coefficient value in results vector */
#define VARINDX 3					/* index of FMRI time course variance value in results vector */

static char * DELAY_OUTPUT_TYPE_name[NBUCKETS] =
  { "Delay", "Covariance", "Corr. Coef.", "Variance" } ;

#define YUP  1
#define NOPE 0

#define ERROR_NOTHINGTODO 	1				/* Nothing to do in hilbertdelay_V2 function */
#define ERROR_LARGENSEG		2				/* Too many segments specified in hilbertdelay_V2 function */
#define ERROR_LONGDELAY		3				/* Could not detect zero crossing before half of time course was gone */
#define ERROR_WRONGUNIT		8				/* Wrong units selected to pass to the delay functions */
#define ERROR_WARPVALUES	9
#define ERROR_FSVALUES		10
#define ERROR_TVALUES		11
#define ERROR_TaUNITVALUES	12
#define ERROR_TaWRAPVALUES	13
#define ERROR_FILEOPEN		15
#define ERROR_SERIESLENGTH	16
#define ERROR_OPTIONS		17
#define ERROR_NULLTIMESERIES 	18
#define ERROR_OUTCONFLICT 	19
#define ERROR_BADLENGTH		20
/*---------------------------------------------------------------------------*/

typedef struct DELAY_options
{ 
	float fs;			/* Sampling frequency */
		  			/* it is only used for the log file in this version*/
		  			/* the ts_func, gives TR automatically */
	float T;			/* Stimulus period */
	float co;			/* Correlation Coefficient Threshold*/
	int unt;			/* Delay units */
	int wrp;			/* flag for Polar Wrap */
	int Navg;			/* number of data sets averaged to obtain the brick (for statistical stuff) */
	int Nort;		/* Number of nuisance parameters (orts) (for statistical stuff) */
	int Nfit;			/* Number of fit parameters (for statistical stuff) */
	int Nseg;			/* Number of segments */
	int ignore;		/* number ofpoints to ignore from time courses */
	int Pover;		/* Percent overlap */
	int ln;			/* length of FMRI vector */
	int dtrnd;		/* remove linear trend or just the mean */
	int biasrem;		/* flag for removing delay bias */
	int Dsamp;		/* flag for correction of non uniform sampling start time */
	int errcode;		/* error code number returned from hdelay */
	int out;			/* flag for writing delays to a file */
	int outts;		/* flag for writing time series to a file */
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
	"is delayed by 4 seconds with respectto the reference time series.\n\n"
	"                                                                       \n"
	"Usage:                                                                 \n"
	"3ddelay                                                                 \n"
	"-input fname       fname = filename of input 3d+time dataset           \n"
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
	"                   The second subbrick is for Covariance, which is an estimate\n" 
	"                   of the power in voxel time series at the frequencies present \n"
	"                   in the reference time series.\n"
	"                   The third subbrick is for the Cross Correlation Coefficients between\n"
	"                   FMRI time series and reference time series.\n"
	"                   The fourth subbrick contains estimates of the Variance of voxel time series.\n"
	"                   The default prefix is the prefix of the input 3D+time brick \n"
	"                   with a '.DEL' extension appended to it.\n"
	"[-uS/-uD/-uR]      Units for delay estimates. (Seconds/Degrees/Radians)\n"
	"                   You can't use Degrees or Radians as units unless \n"
	"                   you specify a value for Tstim > 0.\n"
	"[-phzwrp]          Delay (or phase) wrap.\n"
	"                   This switch maps delays from: \n" 
	"                   (Seconds) 0->T/2 to 0->T/2 and T/2->T to -T/2->0\n"  
	"                   (Degrees) 0->180 to 0->180 and 180->360 to -180->0\n"   
	"                   (Radians) 0->pi to 0->pi and pi->2pi to -pi->0\n" 
	"                   You can't use this option unless you specify a \n"
	"                   value for Tstim > 0.\n\n"
	"[-bias]            Do not correct for the bias in the estimates [1][2]\n" 
	"[-mask mname]      mname = filename of 3d mask dataset                 \n"
	"                   only voxels with non-zero values in the mask would be \n"
	"                   considered.                                           \n"
	"[-nfirst fnum]     fnum = number of first dataset image to use in      \n"
	"                     the delay estimate. (default = 0)                 \n"
	"[-nlast  lnum]     lnum = number of last dataset image to use in       \n"
	"                     the delay estimate. (default = last)              \n"
	"[-nodsamp ]        Do not correct a voxel's estimated delay by the time \n"
	"                   at which the slice containing that voxel was acquired.\n\n"
	"[-co CCT]          Cross Correlation Coefficient threshold value.\n"
	"                   This is only used to limit the ascii output (see below).\n"
   "[-nodtrnd]         Do not remove the linear trend from the data time series.\n"
   "                   Only the mean is removed. Regardless of this option, \n"
   "                   No detrending is done to the reference time series.\n"
	"[-asc [out]]       Write the results to an ascii file for voxels with \n"
	"[-ascts [out]]     cross correlation coefficients larger than CCT.\n"
	"                   If 'out' is not specified, a default name similar \n"
	"                   to the default output prefix is used.\n"
	"                   -asc, only files 'out' and 'out.log' are written to disk (see ahead)\n"
	"                   -ascts, an additional file, 'out.ts', is written to disk (see ahead)\n"
	"                   There a 9 columns in 'out' which hold the following values:\n"
	"                    1- Voxel Index (VI) : Each voxel in an AFNI brick has a unique index.\n"
	"                          Indices map directly to XYZ coordinates.\n"
	"                          See AFNI plugin documentations for more info.\n"
	"                    2..4- Voxel coordinates (X Y Z): Those are the voxel slice coordinates.\n"
	"                          You can see these coordinates in the upper left side of the \n"
	"                          AFNI window.To do so, you must first switch the voxel \n"
	"                          coordinate units from mm to slice coordinates. \n"
	"                          Define Datamode -> Misc -> Voxel Coords ?\n"
	"                          PS: The coords that show up in the graph window\n"
	"                              could be different from those in the upper left side \n"
	"                              of AFNI's main window.\n"
	"                    5- Duff : A value of no interest to you. It is preserved for backward \n"
	"                          compatibility.\n"
	"                    6- Delay (Del) : The estimated voxel delay.\n"
	"                    7- Covariance (Cov) : Covariance estimate.\n"
	"                    8- Cross Correlation Coefficient (xCorCoef) : Cross Correlation Coefficient.\n"
	"                    9- Variance (VTS) : Variance of voxel's time series.\n\n"
	"                   The file 'out' can be used as an input to two plugins:\n"
	"                     '4Ddump' and '3D+t Extract'\n\n"
	"                   The log file 'out.log' contains all parameter settings used for generating \n"
	"                   the output brick. It also holds any warnings generated by the plugin.\n"
	"                   Some warnings, such as 'null time series ...' , or \n"
	"                   'Could not find zero crossing ...' are harmless. '\n"
	"                   I might remove them in future versions.\n\n"
	"                   A line (L) in the file 'out.ts' contains the time series of the voxel whose\n"
	"                   results are written on line (L) in the file 'out'.\n"
	"                   The time series written to 'out.ts' do not contain the ignored samples,\n"
	"                   they are detrended and have zero mean.\n\n"
	"                                                                      \n"
	"Random Comments/Advice:\n"
	"   The longer you time series, the better. It is generally recomended that\n"
	"   the largest delay be less than N/10, N being the length of the time series.\n"
	"   The algorithm does go all the way to N/2.\n\n"
	"   If you have/find questions/comments/bugs about the plugin, \n"
	"   send me an E-mail: ziad@nih.gov\n\n"
	"                          Ziad Saad Dec 8 00.\n\n"
	"   [1] : Bendat, J. S. (1985). The Hilbert transform and applications to correlation measurements,\n"
	"          Bruel and Kjaer Instruments Inc.\n"
	"   [2] : Bendat, J. S. and G. A. Piersol (1986). Random Data analysis and measurement procedures, \n"
	"          John Wiley & Sons.\n"
   "   Author's publications on delay estimation using the Hilbert Transform:\n"
   "   [3] : Saad, Z.S., et al., Analysis and use of FMRI response delays. \n"
   "         Hum Brain Mapp, 2001. 13(2): p. 74-93.\n"
   "   [4] : Saad, Z.S., E.A. DeYoe, and K.M. Ropella, Estimation of FMRI Response Delays. \n"
   "         Neuroimage, 2002: p. in press.\n\n"   
    );

  exit(0);
}


/*---------------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/* taken from #include "/usr/people/ziad/Programs/C/DSP_in_C/dft.h" */
/* dft.h - function prototypes and structures for dft and fft functions */




/* COMPLEX STRUCTURE */

typedef struct {
    float real, imag;
} COMPLEX;

/* function prototypes for dft and inverse dft functions */

    extern void fft(COMPLEX *,int);
    extern void ifft(COMPLEX *,int);
    extern void dft(COMPLEX *,COMPLEX *,int);
    extern void idft(COMPLEX *,COMPLEX *,int);
    extern void rfft(float *,COMPLEX *,int);
    extern void ham(COMPLEX *,int);
    extern void han(COMPLEX *,int);
    extern void triang(COMPLEX *,int);
    extern void black(COMPLEX *,int);
    extern void harris(COMPLEX *,int);

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
  int is;                     /* index */


  /*----- Initialize default values -----*/
  option_data->fs = 0;
  option_data->co = 0.5;
  option_data->T = 0;
  option_data->unt = METH_SECONDS;
  option_data->wrp = 0;
  option_data->Navg = 1;
  option_data->Nort = 2;
  option_data->Nfit = 2;
  option_data->Nseg = 1;
  option_data->Pover = 0;
  option_data->dtrnd = 1;
  option_data->biasrem = 1;
  option_data->Dsamp = 1;
  option_data->outwrite = NULL;
  option_data->outwritets = NULL;
  option_data->outlogfile = NULL;
  option_data->nxx = 64;
  option_data->nyy = 64;
  option_data->nzz = 20;
  option_data->NFirst = 0;
  option_data->NLast  = 32767;
  option_data->out = 0;
  option_data->outts = 0;

	/* Left over from 3dfim+.c remove inthe future, with care !*/
  option_data->polort = 1;
  option_data->num_ortts = 0;
  option_data->num_idealts = 0;
  option_data->N      = 0;
  option_data->fim_thr = 0.0999;
  option_data->cdisp = -1.0;
  option_data->q = 0;
  option_data->p = 0;
  option_data->num_ort_files = 0;
  option_data->num_ideal_files = 0;



  /*----- Initialize file names -----*/
  option_data->input_filename = NULL;
  option_data->mask_filename = NULL;  
  option_data->input1D_filename = NULL;
  option_data->bucket_filename = NULL;
  option_data->outname = NULL;  

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

      /*-----   -bias  -----*/
      if (strcmp(argv[nopt], "-bias") == 0)
		{
		  option_data->biasrem = 0;
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

       /*-----   -nodtrnd  -----*/
      if (strcmp(argv[nopt], "-nodtrnd") == 0)
		{
		  option_data->dtrnd = 0;
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
  float * far;             /* pointer to MRI_IMAGE floating point data */
  int nx;                  /* number of time points in time series */
  int ny;                  /* number of columns in time series file */
  int iy;                  /* time series file column index */
  int ipt;                 /* time point index */
  float * ts_data;         /* input time series data */


  /*----- First, check file name for column index -----*/
  cpt = strstr (ts_filename, "[");
  if (cpt == NULL)
    {
      strcpy (filename, ts_filename);
      subv[0] = '\0';
    }
  else
    if (cpt == ts_filename)
      FIM_error ("Illegal time series filename on command line");
    else
      {
	int ii;
	ii = cpt - ts_filename;
	memcpy (filename, ts_filename, ii);
	filename[ii] = '\0';
	strcpy (subv, cpt);
      }

  
  /*----- Read the time series file -----*/
  im = mri_read_ascii (filename); 
  if (im == NULL)
    {
      sprintf (message,  "Unable to read time series file: %s",  filename);
      FIM_error (message);
    }

  
  /*----- Set pointer to data, and set dimensions -----*/
  flim = mri_transpose (im);   MTEST (flim);
  mri_free(im);   im = NULL;
  far = MRI_FLOAT_PTR(flim);
  nx = flim->nx;
  ny = flim->ny;
  

  /*----- Get the column index -----*/
  if (subv[0] == '\0')  /* no column index */
    {
      if (ny != 1)
	{
	  sprintf (message,
		   "Must specify column index for time series file: %s",
		   ts_filename);
	  FIM_error (message);
	}
      iy = 0;
    }
  else  /* process column index */
    {
      int * ivlist;
      
      ivlist = MCW_get_intlist (ny, subv);
      if ((ivlist == NULL) || (ivlist[0] != 1))
	{
	  sprintf (message,
		   "Illegal column selector for time series file: %s",
		   ts_filename);
	  FIM_error (message);
	}
      iy = ivlist[1];
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


  /*----- First, check file name for column index -----*/
  cpt = strstr (ts_filename, "[");
  if (cpt == NULL)
    {
      strcpy (filename, ts_filename);
      subv[0] = '\0';
    }
  else
    if (cpt == ts_filename)
      FIM_error ("Illegal time series filename on command line");
    else
      {
	int ii;
	ii = cpt - ts_filename;
	memcpy (filename, ts_filename, ii);
	filename[ii] = '\0';
	strcpy (subv, cpt);
      }

  
  /*----- Read the time series file -----*/
  im = mri_read_ascii (filename); 
  if (im == NULL)
    {
      sprintf (message,  "Unable to read time series file: %s",  filename);
      FIM_error (message);
    }

  
  /*----- Set pointer to data, and set dimensions -----*/
  flim = mri_transpose (im);   MTEST (flim);
  mri_free(im);   im = NULL;
  far = MRI_FLOAT_PTR(flim);
  nx = flim->nx;
  ny = flim->ny;
  

  /*----- Get the column indices -----*/
  if (subv[0] == '\0')  /* no column indices */
    {
      *column_list = NULL;
    }
  else  /* process column indices */
    {
      int * ivlist;
      
      ivlist = MCW_get_intlist (ny, subv);
      if ((ivlist == NULL) || (ivlist[0] < 1))
	{
	  sprintf (message,
		   "Illegal column selector for time series file: %s",
		   ts_filename);
	  FIM_error (message);
	}
      *column_list = ivlist;
    }


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
      THD_load_datablock ((*dset_time)->dblk);

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
	  THD_load_datablock ((*mask_dset)->dblk);
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
  check_output_files (option_data, dset_time);
 
  /*----- Read in reference time series -----*/
   option_data->ln = option_data->NLast - option_data->NFirst + 1;
	option_data->rvec = (float *)    malloc (sizeof(float) * option_data->ln+1);       MTEST (option_data->rvec);

  /*------- Load Reference Time Series ------------------*/
  lncheck = float_file_size (option_data->ideal_filename[0]);
  if (lncheck != nt)
  	{
		printf("Error: Reference filename contains %d values.\n %d values were expected.\n", lncheck, nt);
		exit (0);
	}
  
	Read_part_file_delay (option_data->rvec, option_data->ideal_filename[0], option_data->NFirst + 1,option_data->NLast + 1);  

	 
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
	
 /* ------- Open files for writing -------------*/
 	
	option_data->outlogfile = fopen (option_data->outnamelog,"w"); /* open log file regardless */
	
	if (option_data->out == YUP)									/* open outfile */
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
	write_ud (option_data);			/* writes user data to a file */

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
  /*----- Allocate memory -----*/
  *option_data = (DELAY_options *) malloc (sizeof(DELAY_options));

   
  /*----- Get command line inputs -----*/
  get_options (argc, argv, *option_data);


  /*----- Read input data -----*/
  read_input_data (*option_data, dset_time, mask_dset, fmri_data, fmri_length,
		   ort_array, ort_list, ideal_array, ideal_list);


  /*----- Check for valid inputs -----*/
  check_for_valid_inputs (*option_data, *dset_time, *mask_dset, 
			  *fmri_length, ort_array, ideal_array);
  

  /*----- Allocate memory for output volumes -----*/
  if ((*option_data)->input1D_filename == NULL)
    allocate_memory (*option_data, *dset_time, fim_params_vol);

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

  int q;                      /* number of parameters in the baseline model */
  int p;                      /* number of parameters in the baseline model 
			         plus number of ideals */
  int m;                      /* parameter index */
  int n;                      /* data point index */


  matrix xdata;               /* independent variable matrix */
  matrix x_base;              /* extracted X matrix    for baseline model */
  matrix xtxinvxt_base;       /* matrix:  (1/(X'X))X'  for baseline model */
  matrix x_ideal[MAX_FILES];  /* extracted X matrices  for ideal models */
  matrix xtxinvxt_ideal[MAX_FILES];     
                              /* matrix:  (1/(X'X))X'  for ideal models */
  vector y;                   /* vector of measured data */       

  int ixyz;                   /* voxel index */
  int nxyz;                   /* number of voxels per dataset */

  int nt;                  /* number of images in input 3d+time dataset */
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int N;                   /* number of usable data points */

  int num_ort_files;       /* number of ort time series files */
  int num_ideal_files;     /* number of ideal time series files */
  int polort;              /* degree of polynomial ort */
  int num_ortts;           /* number of ort time series */
  int num_idealts;         /* number of ideal time series */
  
  int i;                   /* data point index */
  int is;                  /* ideal index */
  int ilag;                /* time lag index */
  float stddev;            /* normalized parameter standard deviation */
  char * label;            /* string containing stat. summary of results */
	int xpos, ypos, zpos; 
   double tzero , tdelta , ts_mean , ts_slope;
   int   ii ,  iz,izold, nxy , nuse, use_fac, kk;
  float * x_bot = NULL;    /* minimum of stimulus time series */
  float * x_ave = NULL;    /* average of stimulus time series */
  float * x_top = NULL;    /* maximum of stimulus time series */
  int * good_list = NULL;  /* list of good (usable) time points */ 
  float ** rarray = NULL;  /* ranked arrays of ideal time series */
  float FimParams[NBUCKETS];  /* output delay parameters */

   float *  dtr  = NULL ;  /* will be array of detrending coeff */
   float *  fac  = NULL ;  /* array of input brick scaling factors */
	float * vox_vect; 			/* voxel time series */
	float *ref_ts; /*reference time series */
	float slp, delu, del,  xcor, xcorCoef,vts, vrvec, dtx, d0fac , d1fac , x0,x1;
	int  actv, opt, iposdbg;
	
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
	nuse      = DSET_NUM_TIMES(dset) - option_data->NFirst ;
   fac = (float *) malloc( sizeof(float) * nuse ) ;   /* factors */ MTEST (fac);
   

   use_fac = 0 ;
   for( kk=0 ; kk < nuse ; kk++ ){
      fac[kk] = DSET_BRICK_FACTOR(dset,kk+option_data->NFirst) ;
      if( fac[kk] != 0.0 ) use_fac++ ;
      else                 fac[kk] = 1.0 ;
   }
   if( !use_fac ) FREEUP(fac) ;

   /*--- setup for detrending ---*/

   dtr = (float *) malloc( sizeof(float) * nuse ) ;MTEST (dtr);
   

   d0fac = 1.0 / nuse ;
   d1fac = 12.0 / nuse / (nuse*nuse - 1.0) ;
   for( kk=0 ; kk < nuse ; kk++ )
      dtr[kk] = kk - 0.5 * (nuse-1) ;  /* linear trend, orthogonal to 1 */


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
      
		#ifdef ZDBG
		if (ixyz == iposdbg)
			{
				printf("Before Detrending\n");
				printf("TS: %f\t%f\t%f\t%f\t%f\n", vox_vect[0], vox_vect[1],  vox_vect[2], vox_vect[3], vox_vect[4]);
				/*getchar ();*/
			}
		#endif

		/** compute mean and slope **/

      x0 = x1 = 0.0 ;
      for( kk=0 ; kk < nuse ; kk++ ){
         x0 += vox_vect[kk] ; x1 += vox_vect[kk] * dtr[kk] ;
      }

      x0 *= d0fac ; x1 *= d1fac ;  /* factors to remove mean and trend */

      ts_mean  = x0 ;
      ts_slope = x1 / tdelta ;

      /** detrend? **/

      if( option_data->dtrnd )
         for( kk=0 ; kk < nuse ; kk++ ) vox_vect[kk] -= (x0 + x1 * dtr[kk]) ;
      else
         for( kk=0 ; kk < nuse ; kk++ ) vox_vect[kk] -= x0;
         
		#ifdef ZDBG
		if (ixyz == iposdbg)
			{
				printf("After Detrending (or just zero-meaning)\n");
				printf("TS: %f\t%f\t%f\t%f\t%f\n", vox_vect[0], vox_vect[1],  vox_vect[2], vox_vect[3], vox_vect[4]);
				/*getchar ();*/
			}
		#endif

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

 	option_data->errcode = hilbertdelay_V2 (vox_vect, option_data->rvec,option_data->ln,option_data->Nseg,option_data->Pover,opt,0,dtx,option_data->biasrem,&delu,&slp,&xcor,&xcorCoef,&vts,&vrvec);					/* cleanup time */
		#ifdef ZDBG
		if (ixyz == iposdbg)
			{
				printf ("%d err code @ixyz = %d\n", option_data->errcode, ixyz);
			}
		#endif
	if (option_data->errcode == 0) /* If there are no errors, proceed */
		{ /*option_data->errcode == 0 inner loop */
					hunwrap (delu, (float)(option_data->fs), option_data->T, slp, option_data->wrp, option_data->unt, &del );
					
					actv = 1;						/* assume voxel is active */
	
					if (xcorCoef < option_data->co) actv = 0;			/* determine if voxel is activated using xcorCoef  */
	
					if ((actv == 1) && (option_data->out == YUP)) 		/* if voxel is truly activated, write results to file without modifying return value */
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

					del = 0.0;								/* Set all the variables to Null and don't set xcorCoef to an impossible value*/
   				xcorCoef = 0.0;						/*  because the data might still be OK */
   				xcor = 0.0;

				}
			else if (option_data->errcode != 0)
				{
					error_report ( option_data , ixyz);	

					del = 0.0;								/* Set all the variables to Null and set xcorCoef to an impossible value*/
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
  

   if (option_data->out == YUP)									/* close outfile and outlogfile*/
				{
					fclose (option_data->outlogfile);
					fclose (option_data->outwrite);
					if (option_data->outts  == YUP) fclose (option_data->outwritets);
				}
				else
				{
					if (option_data->outlogfile != NULL)	fclose (option_data->outlogfile);		/* close outlogfile */
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
  free (vox_vect); 		vox_vect = NULL;
  
  for (is = 0;  is < num_idealts;  is++)
    {
      free (rarray[is]);   rarray[is] = NULL;
    }
  free (rarray);       rarray = NULL;
  
}


/*---------------------------------------------------------------------------*/
/*
  Convert one volume to another type, autoscaling:
     nxy   = # voxels
     itype = input datum type
     ivol  = pointer to input volume
     otype = output datum type
     ovol  = pointer to output volume (again, must be pre-malloc-ed)
  Return value is the scaling factor used (0.0 --> no scaling).
*/

float EDIT_coerce_autoscale_new( int nxyz ,
				 int itype,void *ivol , int otype,void *ovol )
{
  float fac=0.0 , top ;
  
  if( MRI_IS_INT_TYPE(otype) ){
    top = MCW_vol_amax( nxyz,1,1 , itype,ivol ) ;
    if (top == 0.0)  fac = 0.0;
    else  fac = MRI_TYPE_maxval[otype]/top;
  }
  
  EDIT_coerce_scale_type( nxyz , fac , itype,ivol , otype,ovol ) ;
  return ( fac );
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
  
  if (THD_is_file(DSET_HEADNAME(new_dset))) 
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
			  nsam = option_data->ln;  nort = option_data->Nort;
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
  printf("Writing `bucket' dataset ");
  printf("into %s\n", DSET_HEADNAME(new_dset));
  THD_load_statistics (new_dset);
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;

  
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
		printf ("option_data->Navg= %d\n",option_data->Navg);
		printf ("option_data->Nort= %d\n",option_data->Nort);
		printf ("option_data->Nfit= %d\n",option_data->Nfit);
		printf ("option_data->Nseg= %d\n",option_data->Nseg);
		printf ("option_data->Pover= %d\n",option_data->Pover);
		printf ("option_data->dtrnd= %d\n",option_data->dtrnd);
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
		fprintf (option_data->outlogfile,"Input Data Set: %s\n", option_data->input_filename);
		fprintf (option_data->outlogfile,"Mask Data Set: %s\n", option_data->mask_filename);
		fprintf (option_data->outlogfile,"Ascii output file name: %s\n", option_data->outname);
		fprintf (option_data->outlogfile,"Reference File Name: %s\n", option_data->ideal_filename[0]);
		fprintf (option_data->outlogfile,"Number of voxels in X direction: %d\n", option_data->nxx);
		fprintf (option_data->outlogfile,"Number of voxels in Y direction: %d\n", option_data->nyy);
		fprintf (option_data->outlogfile,"Number of voxels in Z direction: %d\n", option_data->nzz);
		fprintf (option_data->outlogfile,"Sampling Frequency = %f\n",option_data->fs);
		fprintf (option_data->outlogfile,"Stimulus Period = %f\n",option_data->T);
		fprintf (option_data->outlogfile,"Delay units = %d\n",option_data->unt);
		fprintf (option_data->outlogfile,"Delay wrap = %d\n",option_data->wrp);
		fprintf (option_data->outlogfile,"Number of segments = %d\n",option_data->Nseg);
		fprintf (option_data->outlogfile,"Length of reference time series = %d\n",option_data->ln);
		fprintf (option_data->outlogfile,"Number of fit parameters = %d\n",option_data->Nfit);
		fprintf (option_data->outlogfile,"Number of nuisance parameters (orts)= %d\n",option_data->Nort);
		fprintf (option_data->outlogfile,"Percent overlap = %d\n",option_data->Pover);
		fprintf (option_data->outlogfile,"Plugin detrending = %d (Always 0, mandatory detrending is performed)\n",option_data->dtrnd);
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
/* Only errors that happen during runtime (while delays are 	 */
/* computed, are handeled here, the others are handeled  		 */
/* instantaneously, and need not be logged 							 */
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

  
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR); 
  printf ("Date:    %s \n", PROGRAM_DATE);
  printf ("\n");

  /*----- Program initialization -----*/
  initialize_program (argc, argv, &option_data, &dset_time, &mask_dset, 
		      &fmri_data, &fmri_length, 
		      ort_array, ort_list, ideal_array, ideal_list, 
		      &fim_params_vol);


  /*----- Perform fim analysis -----*/
  calculate_results (option_data, dset_time, mask_dset, 
		     fmri_data, fmri_length,
		     ort_array, ort_list, ideal_array, ideal_list, 
		     fim_params_vol);
  

  /*----- Deallocate memory for input datasets -----*/   
  if (dset_time != NULL)  
    { THD_delete_3dim_dataset (dset_time, False);  dset_time = NULL; }
  if (mask_dset != NULL)  
    { THD_delete_3dim_dataset (mask_dset, False);  mask_dset = NULL; }


  /*----- Write requested output files -----*/
  if (option_data->input1D_filename == NULL)
    output_results (argc, argv, option_data, fim_params_vol);


  /*----- Terminate program -----*/
  terminate_program (&option_data, 
		     ort_array, ort_list, ideal_array, ideal_list, 
		     &fim_params_vol); 

}









