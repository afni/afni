/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 2002, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This program implements the False Discovery Rate (FDR) algorithm for
  thresholding of voxelwise statistics.  

  File:    3dFDR.c
  Author:  B. Douglas Ward
  Date:    31 January 2002

*/
/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dFDR"                         /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_INITIAL "31 January 2002" /* date of initial program release */
#define PROGRAM_LATEST "18 January 2008"    /* date of last program revision */


/*---------------------------------------------------------------------------*/
/*
  Include header files and source code files.
*/

#include "mrilib.h"


/*---------------------------------------------------------------------------*/
/*
  Structure declarations 
*/

struct voxel;

typedef struct voxel
{
  int ixyz;                            /* voxel index */
  float pvalue;                        /* input p-value or output q-value */
  struct voxel * next_voxel;           /* pointer to next voxel in list */
} voxel;



/*-------------------------- global data ------------------------------------*/

#define  FDR_MAX_LL 10000        /* maximum number of linked lists of voxels */


static int    FDR_quiet      = 0;      /* flag for suppress screen output */
static int    FDR_list       = 0;      /* flag for list voxel q-values */
static float  FDR_mask_thr   = 1.0;    /* mask threshold */
static float  FDR_cn         = 1.0;    /* statistical constant c(N) */
static int    FDR_nxyz       = 0;      /* dataset dimensions in voxels */
static int    FDR_nthr       = 0;      /* number of voxels in mask */

static char * FDR_input_filename   = NULL;   /* input 3d functional dataset */
static char * FDR_input1D_filename = NULL;   /* input list of p-values */
static char * FDR_mask_filename    = NULL;   /* input 3d mask dataset */
static char * FDR_output_prefix    = NULL;   /* name for output 3d dataset */

static byte  * FDR_mask = NULL;            /* mask for voxels above thr. */
static float * FDR_input1D_data = NULL;    /* input array of p-values */
static voxel * FDR_head_voxel[FDR_MAX_LL]; /* linked lists of voxels */

static char * commandline = NULL;          /* command line for history notes */
static THD_3dim_dataset * FDR_dset = NULL; /* input dataset */  

/*---------------------------------------------------------------------------*/
/*** 18 Jan 2008 changes [RWCox]:
     * replace FDR calculation with mri_fdrize() unless -old is given
     * add -force option to force conversion, assuming input is p-values
     * add -pmask and -nopmask options
     * add -float option
     * add -qval option
-----------------------------------------------------------------------------*/

static int FDR_old   = 0 ;  /* new mode is on by default */
static int FDR_force = 0 ;  /* only if the user asks */
static int FDR_pmask = 1 ;  /* on by default in new mode */
static int FDR_float = 0 ;  /* must be turned on by user */
static int FDR_qval  = 0 ;  /* must be turned on by user */

static int FDR_curve = 0 ;  /* hidden option: -curve */

/*---------------------------------------------------------------------------*/

/** macro to open a dataset and make it ready for processing **/

#define DOPEN(ds,name)                                                        \
do{ int pv ; (ds) = THD_open_dataset((name)) ;                                \
       CHECK_OPEN_ERROR((ds),(name)) ;                                        \
       DSET_load((ds)) ;                                                      \
       pv = DSET_PRINCIPAL_VALUE((ds)) ;                                      \
       if( DSET_ARRAY((ds),pv) == NULL ){                                     \
          fprintf(stderr,"*** Can't access data: %s\n",(name)) ; exit(1); }   \
       if( DSET_BRICK_TYPE((ds),pv) == MRI_complex ){                         \
          fprintf(stderr,"*** Can't use complex data: %s\n",(name)) ; exit(1);\
       }     \
       break ; } while (0)


/*---------------------------------------------------------------------------*/

/** macro to return pointer to correct location in brick for current processing **/

#define SUB_POINTER(ds,vv,ind,ptr)                                            \
   do{ switch( DSET_BRICK_TYPE((ds),(vv)) ){                                  \
         default: fprintf(stderr,"\n*** Illegal datum! ***\n");exit(1);       \
            case MRI_short:{ short * fim = (short *) DSET_ARRAY((ds),(vv)) ;  \
                            (ptr) = (void *)( fim + (ind) ) ;                 \
            } break ;                                                         \
            case MRI_byte:{ byte * fim = (byte *) DSET_ARRAY((ds),(vv)) ;     \
                            (ptr) = (void *)( fim + (ind) ) ;                 \
            } break ;                                                         \
            case MRI_float:{ float * fim = (float *) DSET_ARRAY((ds),(vv)) ;  \
                             (ptr) = (void *)( fim + (ind) ) ;                \
            } break ; } break ; } while(0)


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

#if 0
void FDR_error (char * message)
{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}
#else
# define FDR_error(s) ERROR_exit("3dFDR -- %s",s)
#endif


/*---------------------------------------------------------------------------*/

/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
if((ptr)==NULL) \
( FDR_error ("Cannot allocate memory") )
     

/*---------------------------------------------------------------------------*/
/*
  Display help file.
*/

void FDR_Syntax(void)
{
printf(
"This program implements the False Discovery Rate (FDR) algorithm for       \n"
"thresholding of voxelwise statistics.                                      \n"
"                                                                           \n"
"Program input consists of a functional dataset containing one (or more)    \n"
"statistical sub-bricks.  Output consists of a bucket dataset with one      \n"
"sub-brick for each input sub-brick.  For non-statistical input sub-bricks, \n"
"the output is a copy of the input.  However, statistical input sub-bricks  \n"
"are replaced by their corresponding FDR values, as follows:                \n"
"                                                                           \n"
"For each voxel, the minimum value of q is determined such that             \n"
"                               E(FDR) <= q                                 \n"
"leads to rejection of the null hypothesis in that voxel. Only voxels inside\n"
"the user specified mask will be considered.  These q-values are then mapped\n"
"to z-scores for compatibility with the AFNI statistical threshold display: \n"
"                                                                           \n"
"               stat ==> p-value ==> FDR q-value ==> FDR z-score            \n"
"                                                                           \n"
);

printf(
"Usage:                                                                     \n"
"  3dFDR                                                                    \n"
"    -input fname       fname = filename of input 3d functional dataset     \n"
"      OR                                                                   \n"
"    -input1D dname     dname = .1D file containing column of p-values      \n"
"                                                                           \n"
"    -mask_file mname   Use mask values from file mname.                    \n"
"     *OR*              Note: If file mname contains more than 1 sub-brick, \n"
"    -mask mname        the mask sub-brick must be specified!               \n"
"                       Default: No mask                                    \n"
"                     ** Generally speaking, you really should use a mask   \n"
"                        to avoid counting non-brain voxels.  However, with \n"
"                        the changes described below, the program will      \n"
"                        automatically ignore voxels where the statistics   \n"
"                        are set to 0, so if the program that created the   \n"
"                        dataset used a mask, then you don't need one here. \n"
"                                                                           \n"
"    -mask_thr m        Only voxels whose corresponding mask value is       \n"
"                       greater than or equal to m in absolute value will   \n"
"                       be considered.  Default: m=1                        \n"
"                                                                           \n"
"                       Constant c(N) depends on assumption about p-values: \n"
"    -cind              c(N) = 1   p-values are independent across N voxels \n"
"    -cdep              c(N) = sum(1/i), i=1,...,N   any joint distribution \n"
"                       Default:  c(N) = 1                                  \n"
"                                                                           \n"
"    -quiet             Flag to suppress screen output                      \n"
"                                                                           \n"
"    -list              Write sorted list of voxel q-values to screen       \n"
"                                                                           \n"
"    -prefix pname      Use 'pname' for the output dataset prefix name.     \n"
"      OR                                                                   \n"
"    -output pname                                                          \n"
"                                                                           \n"
"\n") ;

printf(
"===========================================================================\n"
"\n"
"January 2008: Changes to 3dFDR\n"
"------------------------------\n"
"The default mode of operation of 3dFDR has altered somewhat:\n"
"\n"
" * Voxel p-values of exactly 1 (e.g., from t=0 or F=0 or correlation=0)\n"
"     are ignored by default; in the old mode of operation, they were\n"
"     included in the count which goes into the FDR algorithm.  The old\n"
"     process tends to increase the q-values and so decrease the z-scores.\n"
"\n"
" * The array of voxel p-values are now sorted via Quicksort, rather than\n"
"     by binning, as in the old mode.  This probably has no discernible\n"
"     effect on the results.\n"
"\n"
"New Options:\n"
"------------\n"
"    -old     = Use the old mode of operation\n"
"    -new     = Use the new mode of operation [now the default]\n"
"                N.B.: '-list' does not work in the new mode!\n"
"    -pmask   = Instruct the program to ignore p=1 voxels\n"
"                [the default in the new mode, but not in the old mode]\n"
"               N.B.: voxels that were masked in 3dDeconvolve (etc.)\n"
"                     will have their statistics set to 0, which means p=1,\n"
"                     which means that such voxels are implicitly masked\n"
"                     with '-new', and so don't need to be explicitly\n"
"                     masked with the '-mask' option.\n"
"    -nopmask = Instruct the program to count p=1 voxels\n"
"                [the default in the old mode, but not in the new mode]\n"
"    -force   = Force the conversion of all sub-bricks, even if they\n"
"                are not marked as with a statistical code; such\n"
"                sub-bricks are treated as though they were p-values.\n"
"    -float   = Force the output of z-scores in floating point format.\n"
"    -qval    = Force the output of q-values rather than z-scores.\n"
"                N.B.: A smaller q-value is more significant!\n"
"                [-float is recommended when -qval is used]\n"
"\n"
"* To be clear, you can use '-new -nopmask' to have the new mode of computing\n"
"   carried out, but with p=1 voxels included (which should give results\n"
"   virtually identical to '-old').\n"
"\n"
"* Or you can use '-old -pmask' to use the old mode of computing but where\n"
"   p=1 voxels are not counted (which should give results virtually\n"
"   identical to '-new').\n"
"\n"
"* However, the combination of '-new', '-nopmask' and '-mask_file' does not\n"
"   work -- if you try it, '-pmask' will be turned back on and a warning\n"
"   message printed to aid your path towards elucidation and enlightenment.\n"
"\n"
"Other Notes:\n"
"------------\n"
"* '3drefit -addFDR' can be used to add FDR curves of z(q) as a function\n"
"    of threshold for all statistic sub-bricks in a dataset; in turn, these\n"
"    curves let you see the (estimated) q-value as you move the threshold\n"
"    slider in AFNI.\n"
"   - Since 3drefit doesn't have a '-mask' option, you will have to mask\n"
"     statistical sub-bricks yourself via 3dcalc (if desired):\n"
"       3dcalc -a stat+orig -b mask+orig -expr 'a*step(b)' -prefix statmm\n"
"   - '-addFDR' runs as if '-new -pmask' were given to 3dFDR, so that\n"
"     stat values == 0 are ignored in the FDR calculations.\n"
"\n"
"* q-values are estimates of the False Discovery Rate at a given threshold;\n"
"   that is, about 5%% of all voxels with q <= 0.05 (z >= 1.96) are\n"
"   (presumably) 'false positive' detections, and the other 95%% are\n"
"   (presumably) 'true positives'.  Of course, there is no way to tell\n"
"   which above-threshold voxels are 'true' detections and which are 'false'.\n"
"\n"
"* Note the use of the words 'estimate' and 'about' in the above statement!\n"
"   In particular, the accuracy of the q-value calculation depends on the\n"
"   assumption that the p-values calculated from the input statistics are\n"
"   correctly distributed (e.g., that the DOF parameters are correct).\n"
"\n"
"* The z-score is the conversion of the q-value to a double-sided tail\n"
"   probability of the unit Gaussian N(0,1) distribution; that is, z(q)\n"
"   is the value such that if x is a N(0,1) random variable, then\n"
"   Prob[|x|>z] = q: for example, z(0.05) = 1.95996.\n"
"\n"
"* changes above by RWCox -- 18 Jan 2008 == Cary Grant's Birthday!\n"
"\n"
"26 Mar 2009 -- Another Change [RWCox]\n"
"-------------------------------------\n"
"* FDR calculations in AFNI now 'adjust' the q-values downwards by\n"
"   estimating the number of true negatives [m0 in the statistics\n"
"   literature], and then reporting\n"
"     q_new = q_old * m0 / m, where m = number of voxels being tested.\n"
"   If you do NOT want this adjustment, then set environment variable\n"
"   AFNI_DONT_ADJUST_FDR to YES.  You can do this on the 3dFDR command\n"
"   line with the option '-DAFNI_DONT_ADJUST_FDR=YES'\n"
"\n"
"For Further Reading\n"
"-------------------\n"
"* cf. http://en.wikipedia.org/wiki/False_discovery_rate\n"
"* cf. http://afni.nimh.nih.gov/pub/dist/doc/misc/FDR/FDR_Jan2008.pdf\n"
"* cf. http://dx.doi.org/10.1093/bioinformatics/bti448\n"
"* cf. http://dx.doi.org/10.1093/biomet/93.3.491\n"
"* cf. C implementation in mri_fdrize.c [trust in the source]\n"
) ;

   PRINT_COMPILE_DATE ;

   exit(0) ;

}


/*---------------------------------------------------------------------------*/
/*
   Read the arguments, load the global variables

*/

void read_options ( int argc , char * argv[] )
{
  int nopt = 1 ;           /* count of input arguments */
  char message[80];        /* error message */


  if( AFNI_yesenv("AFNI_FLOATIZE") ) FDR_float = 1 ;
  
  /*----- main loop over input options -----*/
  while( nopt < argc )
    {

      /*-----   -input fname   -----*/
      if (strcmp(argv[nopt], "-input") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FDR_error ("need argument after -input ");
	  FDR_input_filename = (char *) malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (FDR_input_filename);
	  strcpy (FDR_input_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      
      
      /*-----   -input1D dname   -----*/
      if (strcmp(argv[nopt], "-input1D") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FDR_error ("need argument after -input1D ");
	  FDR_input1D_filename = (char *) malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (FDR_input1D_filename);
	  strcpy (FDR_input1D_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      
      
      /*-----   -mask_file mname   -----*/
      if (strcmp(argv[nopt], "-mask_file") == 0 || strcmp(argv[nopt],"-mask") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FDR_error ("need argument after -mask_file ");
	  FDR_mask_filename = (char *) malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (FDR_mask_filename);
	  strcpy (FDR_mask_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*----- -mask_thr m -----*/
      if( strcmp(argv[nopt],"-mask_thr") == 0 ){
	 float fval;
         nopt++ ;
         if( nopt >= argc ){
            FDR_error (" need 1 argument after -mask_thr"); 
         }
	 sscanf (argv[nopt], "%f", &fval); 
	 if (fval < 0.0){
            FDR_error (" Require mask_thr >= 0.0 ");
         }
	 FDR_mask_thr = fval;
	 nopt++;  continue;
      }

      /*---- -force & -old & -pmask & -float etc. [18 Jan 2008] -----*/

      if( strcmp(argv[nopt],"-force") == 0 ){
        FDR_force = 1 ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-qval") == 0 ){
        FDR_qval = 1 ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-old") == 0 ){
        FDR_old = 1 ; FDR_pmask = 0 ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-new") == 0 ){
        FDR_old = -1 ; FDR_pmask = 1 ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-pmask") == 0 ){
        FDR_pmask = 1 ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-nopmask") == 0 ){
        FDR_pmask = 0 ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-float") == 0 ){
        FDR_float = 1 ; nopt++ ; continue ;
      }
#if 0
      if( strcmp(argv[nopt],"-curve") == 0 ){  /* hidden option */
        FDR_curve = 1 ; nopt++ ; continue ;
      }
#endif

      /*----- -cind -----*/
      if( strcmp(argv[nopt],"-cind") == 0 ){
         FDR_cn = 1.0;
         nopt++ ; continue ;
      }

      
      /*----- -cdep -----*/
      if( strcmp(argv[nopt],"-cdep") == 0 ){
         FDR_cn = -1.0;
         nopt++ ; continue ;
      }

      
      /*----- -quiet -----*/
      if( strcmp(argv[nopt],"-quiet") == 0 ){
         FDR_quiet = 1;
         nopt++ ; continue ;
      }

      
      /*----- -list -----*/
      if( strcmp(argv[nopt],"-list") == 0 ){
         FDR_list = 1;
         nopt++ ; continue ;
      }

      
      /*----- -prefix prefix -----*/
      if( strcmp(argv[nopt],"-prefix") == 0 ||
          strcmp(argv[nopt],"-output") == 0   ){
         nopt++ ;
         if( nopt >= argc ){
            FDR_error (" need argument after -prefix!");
         }
	 FDR_output_prefix = (char *) malloc (sizeof(char) * THD_MAX_PREFIX); 
         MCW_strncpy( FDR_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }


      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      FDR_error (message);
      

   }  /*----- end of loop over command line arguments -----*/


   if( FDR_old == 0 && FDR_pmask ){  /* the new way is on by default */
     fprintf(stderr,"\n"
       "++ The 'new' method of FDR calculation is on by default; in particular:\n"
       " + * Voxel p-values of exactly 1 (e.g., from t=0 or F=0 or correlation=0)\n"
       " +   are ignored by default; in the old mode of operation, they were\n"
       " +   included in the count which goes into the FDR algorithm.  The old\n"
       " +   process tends to increase the q-values and so decrease the z-scores.\n"
       " + * If you wish to do the FDR conversion using the old mode, use '-old'\n"
       " +   on the command line.  For more information, use '3dFDR -help'.\n"
       " + * If you don't want to see this message again, use the '-new' option.\n"
       "++ RWCox - 18 Jan 2008\n\n"
     ) ;
   }

   if( FDR_old < 1 && FDR_pmask == 0 && FDR_mask != NULL ){  /* 29 Jan 2008 */
     fprintf(stderr,"\n"
       "++ In the 'new' method of FDR calculation, options '-nopmask' and\n"
       " +  -mask_file are incompatible.  Am now turning '-pmask' back on\n"
       " +  so that the mask can be used.\n"
       "++ RWCox - 29 Jan 2008\n\n"
     ) ;
     FDR_pmask = 1 ;
   }

   return ;
}


/*---------------------------------------------------------------------------*/
/*
  Read time series from specified file name.  This file name may have
  a column selector attached.
*/

float * read_time_series 
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
  float * ts_data = NULL;  /* input time series data */


  /*----- First, check for empty filename -----*/
  if (ts_filename == NULL)
    FDR_error ("Missing input time series file name");


  /*----- Read the time series file -----*/
  flim = mri_read_1D(ts_filename) ;
  if (flim == NULL)
    {
      sprintf (message,  "Unable to read time series file: %s",  ts_filename);
      FDR_error (message);
    }

  far = MRI_FLOAT_PTR(flim);
  nx = flim->nx;
  ny = flim->ny; iy = 0 ;
  if( ny > 1 ){
    fprintf(stderr,"WARNING: time series %s has more than 1 column\n",ts_filename);
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
      FDR_error (message);
    }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) )
    {
      sprintf (message,
	       "Output dataset file %s already exists "
	       " -- cannot continue! ",
	       new_dset->dblk->diskptr->header_name);
      FDR_error (message);
    }
  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the program: get all operator inputs; create mask
  for voxels above mask threshold.
*/

void * initialize_program (int argc, char * argv[])
{
  int iv;                  /* index number of sub-brick */
  void * vfim = NULL;      /* sub-brick data pointer */
  float * ffim = NULL;     /* sub-brick data in floating point format */
  int ixyz;                /* voxel index */
  int nx, ny, nz, nxyz;    /* numbers of voxels in input dataset */
  int mx, my, mz, mxyz;    /* numbers of voxels in mask dataset */
  int nthr;                /* number of voxels above mask threshold */
  char message[80];        /* error message */
  int ibin;                /* p-value bin index */


  /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/
  machdep() ; 
  { int new_argc ; char ** new_argv ;
  addto_args( argc , argv , &new_argc , &new_argv ) ;
  if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
  }
  

  /*----- Save command line for history notes -----*/
  commandline = tross_commandline( PROGRAM_NAME , argc,argv ) ;


  /*----- Does user request help menu? -----*/
  if( argc < 2 || strcmp(argv[1],"-help") == 0 ) FDR_Syntax() ;

  
  /*----- Add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv); 


  /*----- Read input options -----*/
  read_options( argc , argv ) ;


  /*----- Open the mask dataset -----*/
  if (FDR_mask_filename != NULL)
    {
      if (!FDR_quiet) 
        printf ("Reading mask dataset: %s \n", FDR_mask_filename);
      DOPEN (FDR_dset, FDR_mask_filename);

      if (FDR_dset == NULL)
      {
        sprintf (message, "Cannot open mask dataset %s", FDR_mask_filename); 
        FDR_error (message);
      }

      if (DSET_NVALS(FDR_dset) != 1)
        WARNING_message("Mask dataset: using sub-brick #0") ;


      /*----- Get dimensions of mask dataset -----*/
      mx   = DSET_NX(FDR_dset);   
      my   = DSET_NY(FDR_dset);   
      mz   = DSET_NZ(FDR_dset);
      mxyz = mx*my*mz;


      /*----- Allocate memory for float data -----*/
      ffim = (float *) malloc (sizeof(float) * mxyz);   MTEST (ffim);


      /*----- Convert mask dataset sub-brick to floats (in ffim) -----*/
      iv = 0 ;
      SUB_POINTER (FDR_dset, iv, 0, vfim);
      EDIT_coerce_scale_type (mxyz, DSET_BRICK_FACTOR(FDR_dset,iv),
			      DSET_BRICK_TYPE(FDR_dset,iv), vfim,  /* input  */
			      MRI_float                   , ffim); /* output */
  
      
      /*----- Allocate memory for mask volume -----*/
      FDR_mask = (byte *) malloc (sizeof(byte) * mxyz);
      MTEST (FDR_mask);
      
      
      /*----- Create mask of voxels above mask threshold -----*/
      nthr = 0;
      for (ixyz = 0;  ixyz < mxyz;  ixyz++){
        if (fabs(ffim[ixyz]) >= FDR_mask_thr){ FDR_mask[ixyz] = 1; nthr++; }
        else                                   FDR_mask[ixyz] = 0;
      }

      if (!FDR_quiet)  
        printf ("Number of voxels above mask threshold = %d \n", nthr);
      if (nthr < 1)  
        FDR_error ("No voxels above mask threshold.  Cannot continue.");


      /*----- Delete floating point sub-brick -----*/
      if (ffim != NULL) { free (ffim);   ffim = NULL; }

      /*----- Delete mask dataset -----*/
      THD_delete_3dim_dataset (FDR_dset, False);  FDR_dset = NULL ;

    }


  /*----- Get the input data -----*/

  if (FDR_input1D_filename != NULL)
    {
      /*----- Read the input .1D file -----*/
      if (!FDR_quiet)  printf ("Reading input data: %s \n", 
			       FDR_input1D_filename);
      FDR_input1D_data = read_time_series (FDR_input1D_filename, &nxyz);

      if (FDR_input1D_data == NULL)  
	{ 
	  sprintf (message,  "Unable to read input .1D data file: %s", 
		   FDR_input1D_filename);
	  FDR_error (message);
	}
      
      if (nxyz < 1)  
	{ 
	  sprintf (message,  "No p-values in input .1D data file: %s", 
		   FDR_input1D_filename);
	  FDR_error (message);
	}

      FDR_nxyz = nxyz;
      FDR_nthr = nxyz;
    }
  
  else
    {
      /*----- Open the input 3D dataset -----*/
      if (!FDR_quiet)  printf ("Reading input dataset: %s \n", 
			       FDR_input_filename);
      FDR_dset = THD_open_dataset(FDR_input_filename);
      CHECK_OPEN_ERROR(FDR_dset,FDR_input_filename);
      
      /*----- Get dimensions of input dataset -----*/
      nx   = DSET_NX(FDR_dset);   
      ny   = DSET_NY(FDR_dset);   
      nz   = DSET_NZ(FDR_dset);
      nxyz = nx*ny*nz;
      
      
      /*----- Check for compatible dimensions -----*/
      if (FDR_mask != NULL)
	{
	  if ((nx != mx) || (ny != my) || (nz != mz))
	    FDR_error ("Mask and input dataset have incompatible dimensions");
	  FDR_nxyz = nxyz;
	  FDR_nthr = nthr;
	}
      else
	{
	  FDR_nxyz = nxyz;
	  FDR_nthr = nxyz;
	}


      /*----- Check whether output dataset already exists -----*/
      if( THD_deathcon() ) check_one_output_file (FDR_dset, FDR_output_prefix);
    }


  /*----- Initialize constant c(N) -----*/
  if (FDR_cn < 0.0)
    {
      double cn;
      cn = 0.0;
      for (ixyz = 1;  ixyz <= FDR_nthr;  ixyz++)
	cn += 1.0 / ixyz;
      FDR_cn = cn;
      if (!FDR_quiet)
	printf ("c(N) = %f \n", FDR_cn);
    }
  
  /*----- Initialize voxel pointers -----*/
  for (ibin = 0;  ibin < FDR_MAX_LL;  ibin++)
    FDR_head_voxel[ibin] = NULL;


}


/*---------------------------------------------------------------------------*/
/*
  Create an empty voxel.
*/
  
voxel * create_voxel ()
{
  voxel * voxel_ptr = NULL;

  voxel_ptr = (voxel *) malloc (sizeof(voxel));
  MTEST (voxel_ptr);
  
  voxel_ptr->ixyz = 0;
  voxel_ptr->pvalue = 0.0;
  voxel_ptr->next_voxel = NULL;

  return (voxel_ptr);
  
}


/*---------------------------------------------------------------------------*/
/*
  Add a new voxel to the linked list of voxels.
*/

voxel * add_voxel (voxel * new_voxel, voxel * head_voxel)
{
  voxel * voxel_ptr = NULL;

  if ((head_voxel == NULL) || (new_voxel->pvalue >= head_voxel->pvalue))
    {
      new_voxel->next_voxel = head_voxel;
      head_voxel = new_voxel;
    }

  else
    {
      voxel_ptr = head_voxel;

      while ((voxel_ptr->next_voxel != NULL) && 
	     (new_voxel->pvalue < voxel_ptr->next_voxel->pvalue))
	voxel_ptr = voxel_ptr->next_voxel;
      
      new_voxel->next_voxel = voxel_ptr->next_voxel;
      voxel_ptr->next_voxel = new_voxel;
    }

  return (head_voxel);
}


/*---------------------------------------------------------------------------*/
/*
  Create and initialize a new voxel, and add to list of voxels.
*/

voxel * new_voxel (int ixyz, float pvalue, voxel * head_voxel)

{
  voxel * voxel_ptr = NULL;

  voxel_ptr = create_voxel ();

  voxel_ptr->ixyz      = ixyz;
  voxel_ptr->pvalue    = pvalue;

  head_voxel = add_voxel (voxel_ptr, head_voxel);

  return (head_voxel);
  
}


/*---------------------------------------------------------------------------*/
/*
  Deallocate memory for all voxel lists.
*/

void delete_all_voxels ()
{
  int ibin;
  voxel * voxel_ptr  = NULL;     /* pointer to current voxel */
  voxel * next_voxel = NULL;     /* pointer to next voxel */


  for (ibin = 0;  ibin < FDR_MAX_LL;  ibin++) 
    {
      voxel_ptr = FDR_head_voxel[ibin];
      while (voxel_ptr != NULL)
	{
	  next_voxel = voxel_ptr->next_voxel;
	  free (voxel_ptr);
	  voxel_ptr = next_voxel;
	}
      FDR_head_voxel[ibin] = NULL;
    }
  
}


/*---------------------------------------------------------------------------*/
/*
  Save voxel contents of all voxels into float array (sub-brick).
*/

void save_all_voxels (float * far)
{
  int ixyz, ibin;
  voxel * voxel_ptr  = NULL;     /* pointer to voxel */
  

  /*----- Initialize all voxels to zero -----*/
  for (ixyz = 0;  ixyz < FDR_nxyz;  ixyz++)
    far[ixyz] = 0.0;


  for (ibin = 0;  ibin < FDR_MAX_LL;  ibin++) 
    {
      voxel_ptr = FDR_head_voxel[ibin];
  
      while (voxel_ptr != NULL)
	{
	  far[voxel_ptr->ixyz] = voxel_ptr->pvalue;
	  voxel_ptr = voxel_ptr->next_voxel;
	}

    }

}


/*---------------------------------------------------------------------------*/
/*
  Calculate FDR z-scores for all voxels within one volume.
*/

void process_volume (float * ffim, int statcode, float * stataux)

{
  int ixyz;                      /* voxel index */
  int icount;                    /* count of sorted p-values */
  float fval;                    /* voxel input statistical value */
  float pval;                    /* voxel input stat. p-value */
  float qval;                    /* voxel FDR q-value */
  float zval;                    /* voxel FDR z-score */
  float qval_min;                /* smallest previous q-value */
  voxel * head_voxel = NULL;     /* linked list of voxels */
  voxel * voxel_ptr  = NULL;     /* pointer to current voxel */
  int ibin;                      /* p-value bin */
  int   * iarray = NULL;         /* output array of voxel indices */
  float * parray = NULL;         /* output array of voxel p-values */
  float * qarray = NULL;         /* output array of voxel FDR q-values */
  float * zarray = NULL;         /* output array of voxel FDR z-scores */

  float numer ;


  /*------------ 18 Jan 2008: use the 'new' method? ------------*/

  if( FDR_old < 1 ){
    MRI_IMAGE *qim ; int flags=0 ;
    qim = mri_new_vol_empty( FDR_nxyz,1,1 , MRI_float ) ;
    mri_fix_data_pointer( ffim , qim ) ;
    if( FDR_mask != NULL ){
      float zz = (FUNC_IS_STAT(statcode)) ? 0.0f : 1.0f ;
      for( ixyz=0 ; ixyz < FDR_nxyz ; ixyz++ )
        if( !FDR_mask[ixyz] ) ffim[ixyz] = zz ;
    }
    if( FDR_curve ){ /* hidden option: produce t-z curve */
      floatvec *fv = mri_fdr_curve( qim , statcode , stataux ) ;
      if( fv == NULL ) ERROR_message("mri_fdr_curve fails!") ;
      else {
        printf("# FDR thresh-z curve\n") ;
        for( ixyz=0 ; ixyz < fv->nar ; ixyz++ )
          printf("%g %g\n", fv->x0+ixyz*fv->dx , fv->ar[ixyz] ) ;
      }
      exit(0) ;
    } else {         /* normal operation: convert to z(q) or q */
      if( FDR_pmask == 0    ) flags |= 1 ;  /* compatibility mode */
      if( FDR_cn    >  1.0f ) flags |= 2 ;  /* dependency flag */
      if( FDR_qval          ) flags |= 4 ;  /* qval flag */
      (void)mri_fdrize( qim , statcode,stataux , flags ) ;
    }
    mri_clear_data_pointer(qim); mri_free(qim);
    return ;
  }

  /*---------------- back to the 'old' method ------------------*/
  
  /*----- Allocate memory for screen output arrays -----*/
  if (FDR_list)
    {
      iarray = (int   *) malloc (sizeof(int)   * FDR_nthr);   MTEST(iarray);
      parray = (float *) malloc (sizeof(float) * FDR_nthr);   MTEST(parray);
      qarray = (float *) malloc (sizeof(float) * FDR_nthr);   MTEST(qarray);
      zarray = (float *) malloc (sizeof(float) * FDR_nthr);   MTEST(zarray);
    }
  
  
  /*----- Loop over all voxels; sort p-values -----*/
  icount = FDR_nthr;

  for (ixyz = 0;  ixyz < FDR_nxyz;  ixyz++)
    {

      /*----- First, check if voxel is inside the mask -----*/
      if( FDR_mask != NULL && !FDR_mask[ixyz] ) continue;


      /*----- Convert stats to p-values -----*/
      fval = fabs(ffim[ixyz]);
      if (statcode <= 0)
	pval = fval;
      else
	pval = THD_stat_to_pval (fval, statcode, stataux);

      if (pval >= 1.0)  
	{
	  /*----- Count but don't sort voxels with p-value = 1 -----*/
	  icount--;
	  if (FDR_list)
	    {
	      iarray[icount] = ixyz;
	      parray[icount] = 1.0;
	      qarray[icount] = 1.0;
	      zarray[icount] = 0.0;
	    }
	}
      else
	{ 
	  /*----- Place voxel in p-value bin -----*/
	  ibin = (int)  (pval * (FDR_MAX_LL));
	  if (ibin < 0)  ibin = 0;
	  if (ibin > FDR_MAX_LL-1)  ibin = FDR_MAX_LL-1;
	  head_voxel = new_voxel (ixyz, pval, FDR_head_voxel[ibin]);
	  FDR_head_voxel[ibin] = head_voxel;
	}
    }

  /*----- Calculate FDR q-values -----*/
  qval_min = 1.0;
  ibin = FDR_MAX_LL-1;
  numer = (FDR_pmask) ? icount : FDR_nthr ;  /* 18 Jan 2008 */
  while (ibin >= 0) 
    {
      voxel_ptr = FDR_head_voxel[ibin];
  
      while (voxel_ptr != NULL)
	{
          /*----- Convert sorted p-values to FDR q-values -----*/
	  pval = voxel_ptr->pvalue;
	  qval = FDR_cn * (pval*numer) / icount;
	  if (qval > qval_min)
	    qval = qval_min;
	  else
	    qval_min = qval;

	  /*----- Convert FDR q-value to FDR z-score -----*/
          if( !FDR_qval ){
            if (qval < 1.0e-20) zval = 10.0;
            else                zval = normal_p2t(qval);
          } else {              zval = qval ; }

	  icount--;

	  /*----- Save calculated values -----*/
	  if (FDR_list)
	    {
	      iarray[icount] = voxel_ptr->ixyz;
	      parray[icount] = pval;
	      qarray[icount] = qval;
	      zarray[icount] = zval;
	    }

	  voxel_ptr->pvalue = zval;
	  voxel_ptr = voxel_ptr->next_voxel;
	}

      ibin--;
    }


  /*----- Write out the calculated values -----*/
  if (FDR_list)
    {
      printf ("%12s %12s %12s %12s \n", 
	      "Index", "p-value", "q-value", "z-score");
      for (icount = 0;  icount < FDR_nthr;  icount++)
	{
	  if (FDR_input1D_filename != NULL)
	    ixyz = iarray[icount] + 1;
	  else
	    ixyz = iarray[icount];
	  printf ("%12d %12.6f %12.6f %12.6f \n",  
		  ixyz, parray[icount], qarray[icount], zarray[icount]);
	}

      /*----- Deallocate memory for output arrays -----*/
      free (iarray);   free (parray);   free (qarray);   free (zarray);
    }


  /*----- Place FDR z-scores into float array -----*/
  save_all_voxels (ffim);


  /*----- Deallocate linked-list memory -----*/
  delete_all_voxels();

}


/*---------------------------------------------------------------------------*/
/*
  Perform all processing for this array of p-values.
*/

void process_1ddata ()

{
  float * ffim = NULL;     /* input list of p-values */

  
  /*----- Set pointer to input array of p-values -----*/
  ffim = FDR_input1D_data;


  /*----- Calculate FDR z-scores for all input p-values  -----*/
  process_volume (ffim, -1, NULL);

  if( FDR_output_prefix != NULL && ffim != NULL ){
    MRI_IMAGE *im = mri_new_vol_empty( FDR_nxyz,1,1 , MRI_float ) ;
    mri_fix_data_pointer( ffim , im ) ;
    mri_write_1D( FDR_output_prefix , im ) ;
    mri_fix_data_pointer( NULL , im ) ;
    mri_free( im ) ;
  }

  /*----- Deallocate memory -----*/
  if (ffim != NULL) { free (ffim);   ffim = NULL; }

}

/*---------------------------------------------------------------------------*/
/*
  Perform all processing for this sub-brick.
*/

void process_subbrick (THD_3dim_dataset * dset, int ibrick)

{
  const float EPSILON = 1.0e-10;
  float factor;            /* factor is new scale factor for this sub-brick */
  void * vfim = NULL;      /* sub-brick data pointer */
  float * ffim = NULL;     /* sub-brick data in floating point format */
  char brick_label[THD_MAX_NAME];       /* sub-brick label */


  if (!FDR_quiet)  printf ("Processing sub-brick #%d \n", ibrick);

  
  /*----- Allocate memory for float data -----*/
  ffim = (float *) malloc (sizeof(float) * FDR_nxyz);   MTEST (ffim);


  /*----- Convert sub-brick to float stats -----*/
  SUB_POINTER (dset, ibrick, 0, vfim);
  EDIT_coerce_scale_type (FDR_nxyz, DSET_BRICK_FACTOR(dset,ibrick),
			  DSET_BRICK_TYPE(dset,ibrick), vfim,   /* input  */
			  MRI_float                   , ffim);  /* output */


  /*----- Calculate FDR z-scores for all voxels within this volume -----*/
  process_volume (ffim, DSET_BRICK_STATCODE(dset,ibrick),
		        DSET_BRICK_STATAUX (dset,ibrick));


  /*----- Replace old sub-brick with new z-scores -----*/
  if( !FDR_float || DSET_BRICK_TYPE(dset,ibrick)==MRI_float ){
    SUB_POINTER (dset, ibrick, 0, vfim);
    factor = EDIT_coerce_autoscale_new (FDR_nxyz, MRI_float, ffim,
				      DSET_BRICK_TYPE(dset,ibrick), vfim);  
    if (factor < EPSILON)  factor = 0.0;
    else factor = 1.0 / factor;
    if( DSET_BRICK_TYPE(dset,ibrick) == MRI_short )
      EDIT_misfit_report( DSET_FILECODE(dset) , ibrick ,
                          FDR_nxyz , factor , vfim , ffim ) ;
  } else {                          /*** if -float was given ***/
    EDIT_substitute_brick( dset , ibrick , MRI_float , ffim ) ;
    ffim = NULL ; factor = 0.0f ;
  }
  

  /*----- edit the sub-brick -----*/
  if( FDR_qval ) strcpy (brick_label, "FDRq:");
  else           strcpy (brick_label, "FDRz:");
  strcat (brick_label, DSET_BRICK_LABEL(dset, ibrick));
  EDIT_BRICK_LABEL (dset, ibrick, brick_label);
  EDIT_BRICK_FACTOR (dset, ibrick, factor);
  if( !FDR_qval ) EDIT_BRICK_TO_FIZT  (dset,ibrick);
  else            EDIT_BRICK_TO_NOSTAT(dset,ibrick);

  /*----- Deallocate memory -----*/
  if (ffim != NULL) { free (ffim);   ffim = NULL; }

}


/*---------------------------------------------------------------------------*/
/*
  Process the input dataset.
*/

THD_3dim_dataset * process_dataset ()

{
  THD_3dim_dataset * new_dset = NULL;     /* output bucket dataset */
  int ibrick, nbricks;                    /* sub-brick indices */
  int statcode;                           /* type of stat. sub-brick */


  /*----- Make full copy of input dataset -----*/
  new_dset = EDIT_full_copy(FDR_dset, FDR_output_prefix);


  /*----- Record history of dataset -----*/
  tross_Copy_History( FDR_dset , new_dset ) ;

  if( commandline != NULL )
    {
      tross_Append_History ( new_dset, commandline);
      free(commandline) ;
    }


  /*----- Deallocate memory for input dataset -----*/   
  THD_delete_3dim_dataset (FDR_dset , False );  FDR_dset = NULL ;


  /*----- Loop over sub-bricks in the dataset -----*/
  nbricks = DSET_NVALS(new_dset);
  for (ibrick = 0;  ibrick < nbricks;  ibrick++)
    {
      statcode = DSET_BRICK_STATCODE(new_dset, ibrick);
      if (FUNC_IS_STAT(statcode) || FDR_force )
	{
	  /*----- Process the statistical sub-bricks -----*/
	  if (!FDR_quiet)  
	    printf ("ibrick = %3d   statcode = %5s \n", 
		    ibrick, FUNC_prefixstr[statcode]);
	  process_subbrick (new_dset, ibrick);
	}
    }


  return (new_dset);
}


/*---------------------------------------------------------------------------*/
/*
  Output the results as a bucket dataset.
*/

void output_results (THD_3dim_dataset * new_dset)
{
  int ierror;     /* flag for errors in editing dataset */


  /*----- Make sure that output is a bucket dataset -----*/
  ierror = EDIT_dset_items( new_dset ,
			    ADN_func_type , FUNC_BUCK_TYPE,
			    ADN_none ) ;
  if (ierror > 0)  
    FDR_error ("Errors in attempting to create output dataset.");


  /*----- Output the FDR dataset -----*/
  if( !FDR_quiet ) fprintf(stderr,"Computing sub-brick statistics\n") ;
  THD_load_statistics( new_dset ) ;

  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  if( !FDR_quiet ) fprintf(stderr,"Wrote output to %s\n", DSET_BRIKNAME(new_dset) );
  

  /*----- Deallocate memory for output dataset -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}


/*---------------------------------------------------------------------------*/

int main( int argc , char * argv[] )

{
  THD_3dim_dataset * new_dset = NULL;      /* output bucket dataset */
  

  /*----- Identify software -----*/
#if 0
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR); 
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");
#endif

  PRINT_VERSION("3dFDR") ; AUTHOR(PROGRAM_AUTHOR); mainENTRY("3dFDR main") ; machdep() ;

  /*----- Initialize program:  get all operator inputs; 
    create mask for voxels above mask threshold -----*/
  initialize_program (argc, argv);


  if (FDR_input1D_filename != NULL)
    {
      /*----- Process list of p-values -----*/
      process_1ddata ();
    }
  else
    {
      /*----- Process 3d dataset -----*/
      new_dset = process_dataset ();

      /*----- Output the results as a bucket dataset -----*/
      output_results (new_dset);
    }
  
  exit(0) ;
}


/*---------------------------------------------------------------------------*/


