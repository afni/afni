/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "afni.h"
#include "afni_plugin.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/* -----------------------START---------------------------------*/
/* definition and decleration part to suport the main algorithm */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/*	Definitions of prototypes and declaration of support functions 
	this is taken from the list of include files that I use in the original code*/ 

/*-------------------------------------------------------------------*/
/* taken from #include "/usr/people/ziad/Programs/C/DSP_in_C/dft.h" */
/* dft.h - function prototypes and structures for dft and fft functions */




/* COMPLEX STRUCTURE */

typedef struct {
    float real, imag;
} COMPLEX;

/* function prototypes for dft and inverse dft functions */

static void fft(COMPLEX *,int);
static void ifft(COMPLEX *,int);
static void dft(COMPLEX *,COMPLEX *,int);
static void idft(COMPLEX *,COMPLEX *,int);
static void rfft(float *,COMPLEX *,int);
static void ham(COMPLEX *,int);
static void han(COMPLEX *,int);
static void triang(COMPLEX *,int);
static void black(COMPLEX *,int);
static void harris(COMPLEX *,int);

#include "plug_delay_V2.h"


/***********************************************************************
  Plugin to compute a 3D+time dataset voxelwise delay with respect to 
  a reference waveform
************************************************************************/
typedef struct 
	{
		  int nxx;			/* number of voxels in the x direction */
		  int nyy;			/* number of voxels in the y direction */
		  int nzz;			/* number of voxels in the z direction */
		  char *dsetname; /* prefix of data set analyzed */
		  char *refname;	/* reference vector filename */
		  float *rvec;		/* reference vetor */
		  float fs;			/* Sampling frequency */
		  						/* it is only used for the log file in this version*/
		  						/* the ts_func, gives TR automatically */
		  float T;			/* Stimulus period */
		  float co;			/* Correlation Coefficient Threshold*/
		  float dmask; 	/* delays to be masked will take this value ( Not used anymore )*/
		  int unt;			/* Delay units */
		  int wrp;			/* flag for Polar Wrap */
		  int Navg;			/* number of data sets averaged to obtain the brick (for statistical stuff) */
		  int Nort;		/* Number of nuisance parameters (orts) (for statistical stuff) */
		  int Nfit;			/* Number of fit parameters (for statistical stuff) */
		  int Nseg;			/* Number of segments */
		  int nsamp;		/* number of points in time series */
		  int ignore;		/* number ofpoints to ignore from time courses */
		  int Pover;		/* Percent overlap */
		  int ln;			/* length of FMRI vector */
		  int dtrnd;		/* remove linear trend or just the mean */
		  int biasrem;		/* flag for removing delay bias */
		  int Dsamp;		/* flag for correction of non uniform sampling start time */
		  int errcode;		/* error code number returned from hdelay */
		  int out;			/* flag for writing delays to a file */
		  int outts;		/* flag for writing time series to a file */
		  char * new_prefix; /* new prefix for data set */
		  char * strout;
		  FILE * outwrite;
		  FILE * outwritets;
		  FILE * outlogfile;
		  char outname[PLUGIN_MAX_STRING_RANGE]; /* output data file name */
	}hilbert_data_V2;

/*--------------------- string to 'help' the user --------------------*/

static char helpstring[] =
  "            Hilbert Delay98 Plugin \n\n"
  "The Plugin estimates the time delay between each voxel time series in a 3D+time dataset \n"
  "and a reference time series[1][2]. The estimated delays are relative to the reference time series.\n"
  "For example, a delay of 4 seconds means that the voxel time series is delayed by 4 seconds with respect\n"
  "to the reference time series.\n\n"
  "Plugin Inputs:\n\n"
  "   1- Data : \n"
  "      3d+time    -> 3D+time dataset to analyze.\n"
  "      Nort       -> Number of orts or nuisance parameters. \n"
  "                    It is set to 2 by default because the mean \n"
  "                    and linear trends are always removed from the time series.\n"
  "                    This value is used for estimating the p-value at the cross\n"
  "                    correlation threshold selected with AFNI's threshold slider.\n"
  "                    The p-value is estimated only when the cross correlation coefficient\n"
  "                    subbrick is used for thresholding.\n\n"
  "   2- Ref. :\n"
  "      Ref. Vect. -> 1D Reference time series vector. \n"
  "                    The reference time series vector is stored in an ascii file.\n"
  "                    The plugin assumes that there is one value per line and that all\n"
  "                    values in the file are part of the reference vector.\n"
  "                    PS: Unlike with 3dfim, and FIM in AFNI, values over 33333 are treated \n"
  "                        as part of the time series.\n" 
  "                    The reference vectors must be placed in a directory specified in \n"
  "                    AFNI_TSPATH environment variable. Read AFNI documentation for more info.\n"
  "      Ignore     -> Number of samples to ignore from the beginning of each voxel's time series.\n"
  "                    Ignore is NOT applied to the reference time series.\n"
  "      Dsamp      -> (Y/N) Correct a voxel's estimated delay by the time at which the slice\n"
  "                    containing that voxel was acquired.\n\n"
  "   3- Sig. :\n"
  "      fs in Hz   -> Sampling frequency in Hz. of data time series. \n"  
  "      Tstim sec  -> Stimulus period in seconds. \n"
  "                    If the stimulus is not periodic, you can set Tstim to 0.\n"
  "      C-Off      -> Cross Correlation Coefficient threshold value.\n"
  "                    This is only used to censor the ascii output (see below).\n"
  "      No-bias    -> (y/n) Correct for the bias in the cross correlation coefficient estimate [1][2].\n\n" 
  "   4- Alg. :\n"
  "      N seg.     -> Number of segments used to estimate the periodogram.\n"
  "                    (you can't modify this parameter in this version)\n"
  "      % ovrlp    -> Percent segment overlap when estimating periodogram.\n"
  "                    (you can't modify this parameter in this version)\n" 
  "      Units      -> (Seconds/Degrees/Radians) Units for delay estimates.\n"
  "                    You can't use Degrees or Radians as units unless you specify\n"
  "                    a value for Tstim > 0.\n"
  "      Phz Wrp    -> (Y/N) Delay (or phase) wrap.\n"
  "                    This switch maps delays from: \n" 
  "                    (Seconds) 0->T/2 to 0->T/2 and T/2->T to -T/2->0\n"  
  "                    (Degrees) 0->180 to 0->180 and 180->360 to -180->0\n"   
  "                    (Radians) 0->pi to 0->pi and pi->2pi to -pi->0\n" 
  "                    You can't use this option unless you specify a value for Tstim > 0.\n\n"
  "   5- Output :\n"
  "      AFNI Prfx  -> Prefix for output brick of the bucket type (fbuc).\n"
  "                    The first subbrick is for Delay.\n"
  "                    The second subbrick is for Covariance, which is an estimate\n" 
  "                    of the power in voxel time series at the frequencies present \n"
  "                    in the reference time series.\n"
  "                    The third subbrick is for the Cross Correlation Coefficients between\n"
  "                    FMRI time series and reference time series.\n"
  "                    The fourth subbrick contains estimates of the Variance of voxel time series.\n"
  "                    If you leave the field empty, a default prefix is used.\n"
  "                    The default prefix is the prefix of the input 3D+time brick \n"
  "                    with a '.DEL' extension appended to it.\n"
  "      Write      -> (Y/N) Write the results to an ascii file for voxels with \n"
  "                    Cross Correlation Coefficients larger than C-Off.\n"
  "                    Each line in the output file contains information for one voxel.\n"
  "                    There a 9 columns in the file which hold the following values:\n"
  "                    1- Voxel Index (VI) : Each voxel in an AFNI brick has a unique index.\n"
  "                                          Indices map directly to XYZ coordinates.\n"
  "                                          See AFNI plugin documentations for more info.\n"
  "                    2..4- Voxel coordinates (X Y Z): Those are the voxel slice coordinates.\n"
  "                                                     You can see these coordinates in the upper left side\n"
  "                                                     of the AFNI window. To do so, you must first switch the\n"
  "                                                     voxel coordinate units from mm to slice coordinates.\n"
  "                                                     Define Datamode -> Misc -> Voxel Coords ?\n"
  "                                                     PS: The coords that show up in the graph window\n"
  "                                                     could be different from those in the upper left side \n"
  "                                                     of AFNI's main window.\n"
  "                    5- Duff : A value of no interest to you. It is preserved for it's historical value.\n"
  "                    6- Delay (Del) : The estimated voxel delay.\n"
  "                    7- Covariance (Cov) : Covariance estimate.\n"
  "                    8- Cross Correlation Coefficient (xCorCoef) : Cross Correlation Coefficient estimate.\n"
  "                    9- Variance (VTS) : Variance of voxel's time series.\n"
  "                    This output file can be used as an input to two other plugins:\n"
  "                    '4Ddump' and '3D+t Extract'\n\n"
  "                    If Write is used, a log file is written too.\n"
  "                    The log file contains all parameter settings used for generating the output brick.\n"
  "                    The name of the log file is the same as 'Filename' (see below) with a '.log' extension\n"
  "                    appended at the end. The log file also holds any warnings generated by the plugin.\n"
  "                    Some warnings, such as 'null time series ...' , or 'Could not find zero crossing ...'\n"
  "                    are harmless, I might remove them in future versions.\n"
  "      Filename   -> Name of the ascii file to write results to.\n"
  "                    If the field is left empty, a default name similar \n"
  "                    to the default output prefix is used.\n"
  "      Write ts   -> (Y/N) Write the time series to an ascii file for voxels with \n"
  "                    Cross Correlation Coefficients larger than C-Off.\n" 
  "                    The file name is that used in 'Filename' with a '.ts' extension appended at the end.\n"
  "                    A line (L) in the file 'Filename.ts' contains the time series of the voxel whose\n"
  "                    results are written on line (L) in the file 'Filename'.\n"
  "                    The time series written to 'Filename.ts' do not contain the ignored samples,\n"
  "                    they are detrended and have zero mean.\n\n"
  "Random Comments/Advice:\n"
  "Of course, the longer you time series, the better. It is generally recomended that\n"
  "the largest delay be less than N/10, N being the length of the time series.\n"
  "The algorithm does go all the way to N/2, but that's not too good.\n\n"
  "Disclaimer: \n"
  "(Blaaa bla bla bla bla .... --> legal terms you probably wouldn't read) \n"
  "             I am not responsible for anything bad.\n\n"
  "If you have/find questions/comments/bugs about the plugin, \n"
  "send me an E-mail: ziad@image.bien.mu.edu\n\n"
  "                          Ziad Saad June 20 97, lastest update Aug 21 00.\n\n"
  "[1] : Bendat, J. S. (1985). The Hilbert transform and applications to correlation measurements,\n"
  "       Bruel and Kjaer Instruments Inc.\n"
  "[2] : Bendat, J. S. and G. A. Piersol (1986). Random Data analysis and measurement procedures, \n"
  "       John Wiley & Sons.\n"
;

/*--------------------- strings for output format --------------------*/
/* do not change the order in this string*/
static char * method_strings[] = { "Seconds" , "Degrees" , "Radians"} ;
static char * yn_strings[] = { "n" , "y" }; 

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
	#define NOWAYXCORCOEF 0					/* A flag value indicating that something lethal went on */
#endif

#define NBUCKETS 4				/* Number of values per voxel in Buket data set */
#define DELINDX 0					/* index of delay value in results vector */
#define COVINDX 1					/* index of covariance value in results vector */
#define COFINDX 2					/* index of cross correlation coefficient value in results vector */
#define VARINDX 3					/* index of FMRI time course variance value in results vector */

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

/*----------------- prototypes for internal routines -----------------*/

static char * DELAY_main( PLUGIN_interface * ) ;  /* the entry point */

static void DELAY_tsfuncV2( double T0 , double TR ,
                   int npts , float ts[] , double ts_mean , double ts_slope ,
                   void * udp , int nbrick , float * buckar) ;

static void show_ud (hilbert_data_V2* ud,int loc);

static void write_ud (hilbert_data_V2* ud);

static void indexTOxyz (hilbert_data_V2* ud, int ncall, int *xpos , int *ypos , int *zpos);

static void error_report (hilbert_data_V2* ud, int ncall );

static void writets (hilbert_data_V2* ud,float * ts);

/*---------------------------- global data ---------------------------*/

static PLUGIN_interface * global_plint = NULL ;

/***********************************************************************
   Set up the interface to the user:
    1) Create a new interface using "PLUTO_new_interface";

    2) For each line of inputs, create the line with "PLUTO_add_option"
         (this line of inputs can be optional or mandatory);

    3) For each item on the line, create the item with
        "PLUTO_add_dataset" for a dataset chooser,
        "PLUTO_add_string"  for a string chooser,
        "PLUTO_add_number"  for a number chooser.
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "Hilbert Delay98" ,
               "Time delay between FMRI and reference time series" ,
               helpstring ,
               PLUGIN_CALL_VIA_MENU , DELAY_main  ) ;

   global_plint = plint ;  /* make global copy */

   /*--------- 1st line: Input dataset ---------*/

   PLUTO_add_option( plint ,
    "Data" ,  /* label at left of input line */
    "Data" ,  /* tag to return to plugin */
    TRUE       /* is this mandatory? */
                   ) ;

   PLUTO_add_dataset(  plint ,
      "3D+time" ,        /* label next to button   */
      ANAT_ALL_MASK ,    /* take only EPI datasets */
      FUNC_ALL_MASK ,    /*  No fim funcs   */
      DIMEN_4D_MASK |    /* need 3D+time datasets  */
      BRICK_ALLREAL_MASK /* need real-valued datasets */
   ) ;
						 
	PLUTO_add_number( plint ,
   "Nort" ,  /* label next to chooser */
   1 ,         /* smallest possible value */
   100 ,        /* largest possible value (inactivated for now)*/
   0 ,         /* decimal shift (none in this case) */
   2 ,         /* default value */
   FALSE       /* allow user to edit value? */
                  ) ;
	
   /*---------- 2nd line: Input time series ----------*/
   
   PLUTO_add_option( plint ,
    "Ref." ,  /* label at left of input line */
    "Ref." ,  /* tag to return to plugin */
    TRUE       /* is this mandatory? */
                   ) ;

   PLUTO_add_timeseries(plint,"Ref. Vect."); 
   
   PLUTO_add_number( plint ,
   "Ignore" ,  /* label next to chooser */
   0 ,         /* smallest possible value */
   50 ,        /* largest possible value (inactivated for now)*/
   0 ,         /* decimal shift (none in this case) */
   0 ,         /* default value */
   FALSE       /* allow user to edit value? */
                  ) ;
	
	PLUTO_add_string( plint ,
    "Dsamp" ,  /*label next to textfield */
    2,yn_strings,  /*   strings to choose among */
    1          /* Default option */
                   ) ; 
                   
   /*---------- 3rd line: sampling frequency ----------*/

   PLUTO_add_option( plint ,
    "Sig." ,  /* label at left of input line */
    "Sig." ,  /* tag to return to plugin */
    TRUE       /* is this mandatory? */
                   ) ;

   PLUTO_add_number( plint ,
   "fs in Hz" ,  /* label next to chooser */
   0 ,         /* smallest possible value */
   2000 ,        /* largest possible value */
   1 ,         /* decimal shift (none in this case) */
   5 ,         /* default value */
   TRUE       /* allow user to edit value? */
                  ) ;
	
	PLUTO_add_number( plint ,
   "Tstim sec" ,  /* label next to chooser */
   0.0 ,         /* smallest possible value */
   500 ,        /* largest possible value */
   0 ,         /* decimal shift (none in this case) */
   40 ,         /* default value */
   TRUE       /* allow user to edit value? */
                  ) ;

	PLUTO_add_number( plint ,
   "C-Off" ,  /* label next to chooser */
   -10 ,         /* smallest possible value */
   10 ,        /* largest possible value */
   1 ,         /* decimal shift  */
   5 ,         /* default value */
   TRUE       /* allow user to edit value? */
                  ) ;
   
   
   PLUTO_add_string( plint ,
    "No-bias" ,  /*label next to textfield */
    2,yn_strings,  /*   strings to choose among */
    1          /* Default option */
                   ) ; 
                  

	
   /*---------- 4th line: Delay Units ----------*/

   PLUTO_add_option( plint ,
    "Alg." ,  /* label at left of input line */
    "Alg." ,  /* tag to return to plugin */
    TRUE        /* is this mandatory? */
                   ) ;

   PLUTO_add_number( plint ,
   "N seg." ,  /* label next to chooser */
   1 ,         /* smallest possible value */
   1 ,        /* largest possible value (turned Off for the moment, supporting code is present)*/
   0 ,         /* decimal shift (none in this case) */
   1 ,         /* default value */
   FALSE       /* allow user to edit value? */
                  ) ;
	
	PLUTO_add_number( plint ,
   "% ovrlp" ,  /* label next to chooser */
   0 ,         /* smallest possible value */
   0 ,        /* largest possible value (not implemented)*/
   0 ,         /* decimal shift (none in this case) */
   0 ,         /* default value */
   FALSE       /* allow user to edit value? */
                  ) ;

	
   PLUTO_add_string( plint ,
    "Units" ,  /* label next to textfield */
    3,method_strings,    /* strings to choose among */
    0          /* Default option */
                   ) ;
   
   PLUTO_add_string( plint ,
    "Phz Wrp" ,  /* label next to textfield */
    2,yn_strings,    /* strings to choose among */
    0          /* Default option */
                   ) ;
                  

   /*---------- 5th line: Output dataset ----------*/

   PLUTO_add_option( plint ,
    "Output" ,  /* label at left of input line */
    "Output" ,  /* tag to return to plugin */
    TRUE        /* is this mandatory? */
                   ) ;

   PLUTO_add_string( plint ,
    "AFNI Prfx" ,  /* label next to textfield */
    0,NULL ,    /* no fixed strings to choose among */
    19          /* 19 spaces for typing in value */
                   ) ;
	
	PLUTO_add_string( plint ,
    "Write" ,  /* label next to textfield */
    2,yn_strings ,    
    1          
                   ) ;
                   
   PLUTO_add_string( plint , "Filename" , 0 , NULL , 19 ) ;
   
   PLUTO_add_string( plint ,
    "Write ts" ,  /* label next to textfield */
    2,yn_strings ,    
    1          
                   ) ;

   /*--------- done with interface setup ---------*/
   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

static char * DELAY_main( PLUGIN_interface * plint )
{
   hilbert_data_V2 uda,*ud;
   MRI_IMAGE * tsim;
   MCW_idcode * idc ;         /* input dataset idcode */
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   char *tmpstr , * str , *nprfxstr;                 /* strings from user */
   int   ntime, nvec ,nprfx, i;
	float * vec , fs , T ;
		
	/* Allocate as much character space as Bob specifies in afni.h + a bit more */
	
	tmpstr = (char *) calloc (PLUGIN_MAX_STRING_RANGE+10,sizeof(char));
	nprfxstr = (char *) calloc (PLUGIN_MAX_STRING_RANGE+10,sizeof(char));
	
	if (tmpstr == NULL || nprfxstr == NULL) 
									  return "********************\n"
												"Could not Allocate\n"
												"a teeni weeni bit of\n"
												"Memory ! \n"
												"********************\n";
														
	ud = &uda;		/* ud now points to an allocated space */
	ud->errcode = 0;	/*reset error flag */
	
   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   /*--------- go to first input line ---------*/
		
   PLUTO_next_option(plint) ;

   idc      = PLUTO_get_idcode(plint) ;   /* get dataset item */
   old_dset = PLUTO_find_dset(idc) ;      /* get ptr to dataset */
   if( old_dset == NULL )
      return "*************************\n"
             "Cannot find Input Dataset\n"
             "*************************"  ;
   
   ud->dsetname = DSET_FILECODE (old_dset);
	ud->nsamp = DSET_NUM_TIMES (old_dset);
	ud->Navg = 1 ;    /* Navg does not play a role for the p value, averaging increases sensitivity */
	ud->Nort = PLUTO_get_number(plint) ; /* Should be two by default, for mean and linear trend */
	ud->Nfit = 2 ;  /* Always 2 for phase and amplitude for this plugin */
	/*--------- go to 2nd input line, input time series ---------*/
		
	PLUTO_next_option(plint) ;
	
	tsim = PLUTO_get_timeseries(plint);
	if (tsim == NULL) return "No Timeseries Input";
	
	ud->ln = (int)tsim -> nx;									/* number of points in each vector */
	nvec 	= tsim -> ny;									/* number of vectors */
	ud->rvec   = (float *) MRI_FLOAT_PTR(tsim);	/* vec[i+j*nx] = ith point of jth vector */
																/* for i=0 .. ntime-1 and j=0 .. nvec-1 */
	
	if (is_vect_null (ud->rvec,ud->ln) == 1) 	/* check if ref vect is all zeroes */
		{
			return "Reference vector is all zeros";	
		}
		
	ud->refname = tsim->name;
	ud->ignore = PLUTO_get_number(plint) ;    /* get number item */
	
	str = PLUTO_get_string(plint) ;
   ud->Dsamp = (int)PLUTO_string_index( str , NUM_YN_STRINGS , yn_strings ) ;
   
   /*--------- go to 3rd input line, sampling frequency, and stimulus period ---------*/
   	
   PLUTO_next_option(plint) ;
   
   ud->fs = PLUTO_get_number(plint) ;    /* get number item */
   ud->T = PLUTO_get_number(plint) ;    /* get number item */
   
   ud->co = PLUTO_get_number(plint) ;    /* get number item */
   str = PLUTO_get_string(plint) ;
   ud->biasrem = (int)PLUTO_string_index( str , NUM_YN_STRINGS , yn_strings ) ;
   
   /*--------- go to 4th input line, delay units and wrp option---------*/
		
   PLUTO_next_option(plint) ;

   ud->Nseg = (int)PLUTO_get_number(plint) ;    /* get number item */
   ud->Pover = (int)PLUTO_get_number(plint) ;    /* get number item */
   
   str = PLUTO_get_string(plint) ;      						/* get string item (the method) */
   ud->unt = (int)PLUTO_string_index( str ,      				/* find it in list it is from */
             	 NUM_METHOD_STRINGS ,
             	 method_strings ) ;
	
	str = PLUTO_get_string(plint) ;  
	ud->wrp = (int)PLUTO_string_index( str , NUM_YN_STRINGS , yn_strings ) ;
	
   /*--------- go to 5th input line Output prefix ---------*/
		
   PLUTO_next_option(plint) ;
		
   ud->new_prefix = PLUTO_get_string(plint) ;   /* get string item (the output prefix) */
	
	/* check to see if the field is empty */
	if (ud->new_prefix == NULL)
			nprfx = 0;
		else
			nprfx = 1;
	/* check if the size is larger than 0. I did not want to check for this unless it's allocated */
	if (nprfx == 1 && (int)strlen (ud->new_prefix) == 0)
		nprfx = 0;
		
	if (nprfx == 0)		/* now create the new name and make new_prefix point to it */
		{
			sprintf (nprfxstr,"%s.DEL",DSET_PREFIX (old_dset));
			ud->new_prefix = nprfxstr;
			/*printf ("New prefix is set to be : %s\n\a",ud->new_prefix);*/
		}
	
   if( ! PLUTO_prefix_ok(ud->new_prefix) )      /* check if it is OK */
      return "************************\n"
             "Output Prefix is illegal\n"
             "************************"  ;
	
	str = PLUTO_get_string(plint) ; 				/* write delays to file ? */
	ud->out = (int)PLUTO_string_index( str , NUM_YN_STRINGS , yn_strings );
	
	ud->strout = PLUTO_get_string(plint) ; 				/* strout is for the outiflename, which will be used after the debugging section */
	if (ud->strout == NULL)						/* if no output name is given, use the new_prefix */
		{ud->strout = ud->new_prefix;}
		else 
			{	
				if((int)strlen (ud->strout) == 0) ud->strout = ud->new_prefix;
			}
			
	str = PLUTO_get_string(plint) ; 
	ud->outts = (int)PLUTO_string_index( str , NUM_YN_STRINGS , yn_strings );
	
	/* ------------------Done with user parameters ---------------------------- */
	
	ud->nxx = (int)old_dset->daxes->nxx;				/* get data set dimensions */
	ud->nyy = (int)old_dset->daxes->nyy;
	ud->nzz = (int)old_dset->daxes->nzz;
	
	/* No need for users to set these options ...*/
	
	ud->dtrnd = 0;
	
	if (ud->ln != (ud->nsamp - ud->ignore))
		{
			ud->errcode = ERROR_BADLENGTH;
			return "***************************\n"
					 "Bad time series length \n"
					 "Check reference time series\n"
					 " or the ignore parameter   \n"
					 "***************************\n";
		}
	
	if ((ud->unt < 0) || (ud->unt > 2))										/* unt error Check */
	  	{
         ud->errcode = ERROR_WRONGUNIT;
         return "***********************\n"
         		 " internal error: (ziad)\n"
					 "unt values out of bound\n"
					 "***********************\n";			/*unt must be between 0 and 2 */
	  	}
	  
	  if ((ud->wrp < 0) || (ud->wrp > 1))										/* wrp error Check */
	  	{
         ud->errcode = ERROR_WARPVALUES;
         return "***********************\n"
         		 " internal error: (ziad)\n"
					 "wrp values out of bound\n"
					 "***********************\n";			/* wrp must be between 0 and 1*/
	  	}
	  
	  if (ud->fs < 0.0) {       /* fs error Check */
         ud->errcode = ERROR_FSVALUES;
         return "***********************\n"
         		 " internal error: (ziad)\n"
					 "fs value is negative !\n"
					 "***********************\n";			/* fs must be >= 0*/
        }
	  
	  if (ud->T < 0.0) {        /* T error Check */
         ud->errcode = ERROR_TVALUES;
         return "***********************\n"
         		 " internal error: (ziad)\n"
					 "T value is negative !\n"
					 "***********************\n";					/*T must be >= 0  */
        }
        
           	
     if ((ud->T == 0.0) && (ud->unt > 0))                /* unt error Check */
   	{
         ud->errcode = ERROR_TaUNITVALUES;
         return "***********************\n"
         		 " internal error: (ziad)\n"
					 "T and unt val. mismatch\n"
					 "***********************\n";			/*T must be specified, and > 0 in order to use polar units*/
   	}

    
    if ((ud->wrp == 1) && (ud->T == 0.0))                  /* wrp error Check */
        {
         ud->errcode = ERROR_TaWRAPVALUES;
         return "***********************\n"
         		 " internal error: (ziad)\n"
					 "wrp and T val. mismatch\n"
					 "***********************\n"; 			/*T must be specified, and > 0 in order to use polar warp*/
        }
	 if ((ud->out == NOPE) && (ud->outts == YUP))
	 		{
	 		 ud->errcode = ERROR_OUTCONFLICT;
	 		 return"***********************\n"
         		 "error: \n"
					 "Write flag must be on\n"
					 "to use Write ts\n"
					 "***********************\n";	
	 		
	 		}
	

	/* Open the logfile, regardless of the ascii output files */
	sprintf ( tmpstr , "%s.log" , ud->strout);
	ud->outlogfile = fopen (tmpstr,"w");


	if (ud->out == YUP)									/* open outfile */
				{					
					ud->outwrite = fopen (ud->strout,"w");
					
					if (ud->outts == YUP)
						{
							sprintf ( tmpstr , "%s.ts" , ud->strout);
							ud->outwritets = fopen (tmpstr,"w");
							
						}
					
					if ((ud->outwrite == NULL) || (ud->outlogfile == NULL) ||\
					    (ud->outwritets == NULL && ud->outts == YUP) )
						{
							ud->errcode = ERROR_FILEOPEN; 
							
							return "***********************\n"
									 "Could Not Write Outfile\n"
									 "***********************\n";
						}
	
				}
	
	/* Write out user variables to Logfile */
	write_ud (ud);			/* writes user data to a file */
	
	/*show_ud (ud,0);	*/			/* For some debugging */

   
   /*------------- ready to compute new dataset -----------*/

   new_dset = MAKER_4D_to_typed_fbuc ( old_dset ,             /* input dataset */
          ud->new_prefix ,           /* output prefix */
          -1,	/* negative value indicating data type is like original brick */
          ud->ignore ,               /* ignore count */
          1 ,   /* detrend = ON Let BOB do it*/
          NBUCKETS,					/*Number of values at each voxel*/
			 DELAY_tsfuncV2 ,         /* timeseries processor (bucket version)*/
			 (void *)ud,          /* data for tsfunc */
			 NULL							) ; 
										 
   /* Setup the label, keywords and types of subbricks */
	i = 0;
	while (i < NBUCKETS)
		{
			switch (i)
				{
					case DELINDX:					/* delay value in results vector */
						EDIT_BRICK_LABEL (new_dset,i,"Delay");
						EDIT_BRICK_ADDKEY (new_dset,i,"D");
						++i;
						break;
					case COVINDX:					/* covariance value in results vector */
						EDIT_BRICK_LABEL (new_dset,i,"Covariance");
						EDIT_BRICK_ADDKEY (new_dset,i,"I");
						++i;
						break;
					case COFINDX:					/* cross correlation coefficient value in results vector */
						EDIT_BRICK_LABEL (new_dset,i,"Corr. Coef.");
						EDIT_BRICK_ADDKEY (new_dset,i,"r");
						/* Here you must modify either ud->Nfit or ud->Nort or most likely ud->nsamp based on ud->Navg */
						EDIT_BRICK_TO_FICO (new_dset,i,ud->nsamp - ud->ignore,ud->Nfit,ud->Nort);
						++i;
						break;
					case VARINDX:					/* FMRI time course variance value in results vector */
						EDIT_BRICK_LABEL (new_dset,i,"Variance");
						EDIT_BRICK_ADDKEY (new_dset,i,"S2");
						++i;
						break;
					default :
						return "*********************\n"
								 "Internal Error (ziad)\n"
								 " Bad i value \n"
								 "*********************\n";
						break;
				}
				
		}
	
   
   if (!AFNI_noenv("AFNI_AUTOMATIC_FDR")) {
      THD_create_all_fdrcurves( new_dset );
   }
	PLUTO_add_dset( plint , new_dset , DSET_ACTION_MAKE_CURRENT ) ;
	
	

   if (ud->out == YUP)									/* close outfile and outlogfile*/
				{
					fclose (ud->outlogfile);
					fclose (ud->outwrite);
					if (ud->outts  == YUP) fclose (ud->outwritets);
				}
				else
				{
					if (ud->outlogfile != NULL)	fclose (ud->outlogfile);		/* close outlogfile */
				}
	
	free (tmpstr);		
	free (nprfxstr);
   return NULL ;  /* null string returned means all was OK */
}

/**********************************************************************
   Function that does the real work
***********************************************************************/

static void DELAY_tsfuncV2( double T0 , double TR ,
                   int npts , float ts[] , double ts_mean , double ts_slope ,
                   void * udp , int nbrick , float * buckar)
{
   static int nvox , ncall ;
	hilbert_data_V2 uda,*ud;
	float del, xcorCoef, buckara[4];
	float xcor=0.0 ,  tmp=0.0 , tmp2 = 0.0 ,  dtx = 0.0 ,\
			 delu = 0.0 , slp = 0.0 , vts = 0.0 , vrvec = 0.0 ;
	int i , is_ts_null , status , opt , actv , zpos , ypos , xpos ;
	
	ud = &uda;
	ud = (hilbert_data_V2 *) udp;
	
   /** is this a "notification"? **/

   if( buckar == NULL ){

      if( npts > 0 ){  /* the "start notification" */

         PLUTO_popup_meter( global_plint ) ;  /* progress meter  */
         nvox  = npts ;      /* keep track of   */
         ncall = 0 ;         /* number of calls */
			
      } else {  /* the "end notification" */
			
			opt = 0;					/* cleanup in hdelay */
   		status = hilbertdelay_V2 (ts,ud->rvec,ud->ln,ud->Nseg,ud->Pover,opt,ud->dtrnd,dtx,ud->biasrem,&tmp,&slp,&xcor,&tmp2,&vts,&vrvec);					/* cleanup time */
	
         PLUTO_set_meter( global_plint , 100 ) ; /* set meter to 100% */

      }
      return ;
   }

	/* In the old version, I had to check for a match between the lengths of the reference time series and FMRI time series
	This is now done before the function is called. */
   
   if (is_vect_null (ts,npts) == 1) /* check for null vectors */
   	{
   		ud->errcode = ERROR_NULLTIMESERIES;
			error_report (ud , ncall );	/* report the error */
   		
   		del = 0.0;								/* Set all the variables to Null and don't set xcorCoef to an impossible value*/
   		xcorCoef = 0.0;						/*  because the data might still be OK */
   		xcor = 0.0;
   	}
   
   if (ud->errcode == 0) 				/* if there are no errors, proceed */	
		{/* ud->errcode == 0 outer loop */
			opt = 1;					/* activate hdelay */
   		
   		/* transform dtx from seconds to sampling units and correct for the number of points ignored*/
   		if (ud->Dsamp == YUP) 
   			dtx = (float) (T0 / TR) - ud->ignore;
   		else
   			dtx = 0.0;
   			
   		ud->errcode = hilbertdelay_V2 (ts,ud->rvec,ud->ln,ud->Nseg,ud->Pover,opt,ud->dtrnd,dtx,ud->biasrem,&delu,&slp,&xcor,&xcorCoef,&vts,&vrvec);					/* cleanup time */
			
			if (ud->errcode == 0) /* If there are no errors, proceed */
				{ /*ud->errcode == 0 inner loop */
					hunwrap (delu, (float)(1/TR), ud->T, slp, ud->wrp, ud->unt, &del );
					
					actv = 1;						/* assume voxel is active */
	
					if (xcorCoef < ud->co) actv = 0;			/* determine if voxel is activated using xcorCoef  */
	
					if ((actv == 1) && (ud->out == YUP)) 		/* if voxel is truly activated, write results to file without modifying return value */
						{
							indexTOxyz ( ud , ncall, &xpos , &ypos , &zpos);
							fprintf (ud->outwrite,"%d\t%d\t%d\t%d\t%f\t%f\t%f\t%f\t%f\n", ncall , xpos , ypos , zpos ,  delu , del , xcor , xcorCoef , vts);
							if (ud->outts == YUP)
								{
									writets (ud,ts);	
								}
						}

				}/*ud->errcode == 0 inner loop */
			
			else if (ud->errcode == ERROR_LONGDELAY)
						{
							error_report ( ud , ncall);	
							
							del = 0.0;								/* Set all the variables to Null and don't set xcorCoef to an impossible value*/
   						xcorCoef = 0.0;						/*  because the data might still be OK */
   						xcor = 0.0;
							
						}
					else if (ud->errcode != 0)
								{
									error_report ( ud , ncall);	
									
									del = 0.0;								/* Set all the variables to Null and set xcorCoef to an impossible value*/
   								xcorCoef = NOWAYXCORCOEF;						
   								xcor = 0.0;
								}

   }/* ud->errcode == 0 outer loop */
   
	/* Now fill up the bucket array */
	
	buckar[DELINDX] = del;
	buckar[COVINDX] = xcor;
	buckar[COFINDX] = xcorCoef;
	buckar[VARINDX] = vts;


   /** set the progress meter to the % of completion **/
   ncall++ ;
   
   PLUTO_set_meter( global_plint , (100*ncall)/nvox ) ;
   
   ud->errcode = 0;	/* Rest error to nothing */
   
   return ;
}

/* ************************************************************ */
/* function to display user data input (debugging stuff)        */
/* ************************************************************ */

static void show_ud (hilbert_data_V2* ud,int loc)
	{
		printf ("\n\nUser Data Values at location :%d\n",loc);
		printf ("ud->dsetname= %s\n",ud->dsetname);
		printf ("ud->refname= %s\n",ud->refname);
		printf ("ud->rvec= (1st three elements only)");
		disp_vect (ud->rvec,3);
		printf ("ud->nxx= %d\n",ud->nxx);
		printf ("ud->nyy= %d\n",ud->nyy);
		printf ("ud->nzz= %d\n",ud->nzz);
		printf ("ud->fs= %f\n",ud->fs);
		printf ("ud->T= %f\n",ud->T);
		printf ("ud->co= %f\n",ud->co);
		printf ("ud->unt= %d\n",ud->unt);
		printf ("ud->wrp= %d\n",ud->wrp);
		printf ("ud->Navg= %d\n",ud->Navg);
		printf ("ud->Nort= %d\n",ud->Nort);
		printf ("ud->Nfit= %d\n",ud->Nfit);
		printf ("ud->Nseg= %d\n",ud->Nseg);
		printf ("ud->Pover= %d\n",ud->Pover);
		printf ("ud->dtrnd= %d\n",ud->dtrnd);
		printf ("ud->biasrem= %d\n",ud->biasrem);
		printf ("ud->Dsamp= %d\n",ud->Dsamp);
		printf ("ud->ln= %d\n",ud->ln);
		printf ("ud->nsamp= %d\n",ud->nsamp);
		printf ("ud->ignore= %d\n",ud->ignore);
		printf ("ud->errcode= %d\n",ud->errcode);
		printf ("ud->new_prefix= %s\n",ud->new_prefix);
		printf ("ud->out= %d\n",ud->out);
		printf ("ud->strout= %s\n",ud->strout);
		printf ("ud->outts= %d\n",ud->outts);
		printf ("Hit enter to continue\a\n\n");
		getchar ();
		return;
	}

/* ************************************************************ */
/* function to write user data input to log file        */
/* ************************************************************ */

static void write_ud (hilbert_data_V2* ud)
	{
		fprintf (ud->outlogfile,"\nLogfile output by Hilbert Delay98 plugin\n");
		fprintf (ud->outlogfile,"\n\nUser Data Values \n");
		fprintf (ud->outlogfile,"Input data set = %s\n",ud->dsetname);
		fprintf (ud->outlogfile,"Reference file name = %s\n",ud->refname);
		fprintf (ud->outlogfile,"Number of voxels in X dimension = %d\n",ud->nxx);
		fprintf (ud->outlogfile,"Number of voxels in Y dimension = %d\n",ud->nyy);
		fprintf (ud->outlogfile,"Number of voxels in Z dimension = %d\n",ud->nzz);
		fprintf (ud->outlogfile,"Sampling Frequency = %f\n",ud->fs);
		fprintf (ud->outlogfile,"Stimulus Period = %f\n",ud->T);
		fprintf (ud->outlogfile,"Threshold Cut Off value = %f\n",ud->co);
		fprintf (ud->outlogfile,"Delay units = %d\n",ud->unt);
		fprintf (ud->outlogfile,"Delay wrap = %d\n",ud->wrp);
		fprintf (ud->outlogfile,"Number of segments = %d\n",ud->Nseg);
		fprintf (ud->outlogfile,"Number of samples in time series = %d\n",ud->nsamp);
		fprintf (ud->outlogfile,"Ignore = %d\n",ud->ignore);
		fprintf (ud->outlogfile,"Length of reference time series = %d\n",ud->ln);
		fprintf (ud->outlogfile,"Number of fit parameters = %d\n",ud->Nfit);
		fprintf (ud->outlogfile,"Number of nuisance parameters (orts)= %d\n",ud->Nort);
		fprintf (ud->outlogfile,"Percent overlap = %d\n",ud->Pover);
		fprintf (ud->outlogfile,"Plugin detrending = %d (Always 0, mandatory detrending is performed)\n",ud->dtrnd);
		fprintf (ud->outlogfile,"Bias correction = %d\n",ud->biasrem);
		fprintf (ud->outlogfile,"Acquisition time correction = %d\n",ud->Dsamp);
		fprintf (ud->outlogfile,"Prefix for birck output = %s\n",ud->new_prefix);
		fprintf (ud->outlogfile,"Flag for Ascii output file  = %d\n",ud->out);
		fprintf (ud->outlogfile,"Ascii output file name = %s\n",ud->strout);
		fprintf (ud->outlogfile,"Flag for Ascii time series file output = %d\n",ud->outts);
		fprintf (ud->outlogfile,"\nud->errcode (debugging only)= %d\n\n",ud->errcode);
		fprintf (ud->outlogfile,"\nThe format for the output file is the following:\n");
		fprintf (ud->outlogfile,"VI\tX\tY\tZ\tDuff\tDel\tCov\txCorCoef\tVTS\n");
		fprintf (ud->outlogfile,"\nError Log <message> <index> <x> <y> <z>\n\n");
		
		return;
	}

/* ************************************************************ */ 
/* function to compute x, y, z coordinates from the index       */
/* ************************************************************ */ 

static void indexTOxyz (hilbert_data_V2* ud, int ncall, int *xpos , int *ypos , int *zpos)  	
	{
		*zpos = (int)ncall / (int)(ud->nxx*ud->nyy);
		*ypos = (int)(ncall - *zpos * ud->nxx * ud->nyy) / (int)ud->nxx;
		*xpos = ncall - ( *ypos * ud->nxx ) - ( *zpos * ud->nxx * ud->nyy ) ;
		return;
	}
	
/* ************************************************************ */
/* function to report errors encountered to the logfile         */
/* Only errors that happen during runtime (while delays are 	 */
/* computed, are handeled here, the others are handeled  		 */
/* instantaneously, and need not be logged 							 */
/* ************************************************************ */

static void error_report (hilbert_data_V2* ud, int ncall ) 
	{
		int xpos,ypos,zpos;
		indexTOxyz (ud, ncall, &xpos , &ypos , &zpos); 

		switch (ud->errcode)
			{
				case ERROR_NOTHINGTODO:
					fprintf (ud->outlogfile,"Nothing to do hilbertdelay_V2 call ");
					break;
				case ERROR_LARGENSEG:
					fprintf (ud->outlogfile,"Number of segments Too Large ");
					break;
				case ERROR_LONGDELAY:
					fprintf (ud->outlogfile,"Could not find zero crossing before maxdel limit ");
					break;
				case ERROR_SERIESLENGTH:
					fprintf (ud->outlogfile,"Vectors have different length ");
					break;
				case ERROR_NULLTIMESERIES:
					fprintf (ud->outlogfile,"Null time series vector ");
					break;
				default:
					fprintf (ud->outlogfile,"De Fault, De Fault (%d), the two sweetest words in the english langage ! ",ud->errcode);
					break;
			}	
		fprintf (ud->outlogfile,"%d\t%d\t%d\t%d\t\n", ncall , xpos , ypos , zpos  );
		return;
	}
	
/* *************************************************************** */
/* function to write the time course into a line in the given file */
/* *************************************************************** */

static void writets (hilbert_data_V2 * ud,float * ts)

	{	
		int i;
		
		for (i=0;i<ud->ln;++i)
			{
				fprintf (ud->outwritets, "%f\t",ts[i]);
			}
		fprintf (ud->outwritets,"\n");
	}
