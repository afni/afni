/*---------------------------------------------------------------------------*/
/*
  Program to estimate the PDF for a dataset.

  File:    3destpdf.c
  Author:  B. Douglas Ward
  Date:    20 January 2000


  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.

*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3destpdf"                      /* name of this program */
#define PROGRAM_AUTHOR "B. D. Ward"                        /* program author */
#define PROGRAM_DATE "20 January 2000"           /* date of last program mod */

/*---------------------------------------------------------------------------*/
/*
  Include header files.
*/


#include "mrilib.h"


/*---------------------------------------------------------------------------*/
/*
  Global variables and constants.
*/

static char * anat_filename = NULL;      /* file name for input anat dataset */

static THD_3dim_dataset * anat;                 /* input anatomical dataset  */

static float min_val_float;                 /* minimum voxel intensity limit */
static float max_val_float;                 /* maximum voxel intensity limit */

static Boolean quiet = FALSE;            /* flag for suppress screen output */

#define MAX_STRING_LENGTH 80

/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void estPDF_error (char * message)
{
  fprintf (stderr, "\n");
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/

/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
if((ptr)==NULL) \
( estPDF_error ("Cannot allocate memory") )
     
/*---------------------------------------------------------------------------*/
/*
  Include source code.
*/

#include "estpdf3.c"                    /* code for PDF estimation */


/*---------------------------------------------------------------------------*/
/*
   Routine to display 3destpdf help menu.
*/

void display_help_menu()
{
  printf 
    (
     "This program estimates the PDF for a dataset.\n\n"
     "Usage: \n"
     "3destpdf \n"
     "-anat filename    Filename of anat dataset to be segmented            \n"
      );
  
  exit(0);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to get user specified input options.
*/

void get_options
(
  int argc,                        /* number of input arguments */
  char ** argv                     /* array of input arguments */ 
)

{
  int nopt = 1;                     /* input option argument counter */
  int ival, index;                  /* integer input */
  float fval;                       /* float input */
  char message[MAX_STRING_LENGTH];  /* error message */


  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();  
   

  /*----- main loop over input options -----*/
  while (nopt < argc )
    {

      /*-----   -anat filename   -----*/
      if (strncmp(argv[nopt], "-anat", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  estPDF_error ("need argument after -anat ");
	  anat_filename = malloc (sizeof(char) * MAX_STRING_LENGTH);
	  MTEST (anat_filename);
	  strcpy (anat_filename, argv[nopt]);

	  anat = THD_open_one_dataset (anat_filename);
	  if (!ISVALID_3DIM_DATASET (anat))
	    {
	      sprintf (message, "Can't open dataset: %s\n", anat_filename); 
	      estPDF_error (message); 
	    } 
	  THD_load_datablock (anat->dblk, NULL); 
	  if (DSET_ARRAY(anat,0) == NULL)
	    {
	      sprintf (message, "Can't access data: %s\n", anat_filename); 
	      estPDF_error (message); 
	    }

	  nopt++;
	  continue;
	}
      

      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      estPDF_error (message);
      
    }

  
}


/*---------------------------------------------------------------------------*/
/*
  Program initialization.
*/

void initialize_program 
(
  int argc,                        /* number of input arguments */
  char ** argv                     /* array of input arguments */ 
)

{
  float parameters [DIMENSION];    /* parameters for PDF estimation */
  Boolean ok = TRUE;               /* flag for successful PDF estimation */

  int nx, ny, nz, nxy, nxyz, ixyz;       /* voxel counters */
  int n;                                 /* histogram bin index */
  short * sfim = NULL;                   /* pointer to anat data */
  short * rfim = NULL;                   /* truncated data */ 
  int icount;
  int lower_cutoff = 25;

  /*----- Get operator inputs -----*/
  get_options (argc, argv);


  /*----- Initialize local variables -----*/
  if (anat == NULL)  estPDF_error ("Unable to read anat dataset");
  nx = DSET_NX(anat);   ny = DSET_NY(anat);   nz = DSET_NZ(anat);
  nxy = nx*ny;   nxyz = nxy*nz;
  sfim  = (short *) DSET_BRICK_ARRAY(anat,0) ;
  if (sfim == NULL)  estPDF_error ("Unable to read anat dataset");
  rfim = (short *) malloc (sizeof(short) * nxyz);   MTEST (rfim);


  /*----- Just use voxels whose intensity is above the lower cutoff -----*/
  icount = 0;
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (sfim[ixyz] > lower_cutoff)
      {
	rfim[icount] = sfim[ixyz];
	icount++;
      }
  printf ("%d voxels above lower cutoff = %d \n", icount, lower_cutoff);


  /*----- Get PDF estimate and set voxel intensity limits -----*/
  estpdf_short (icount, rfim, parameters);
  min_val_float = parameters[4] - 2.0*parameters[5];
  max_val_float = parameters[7] + 2.0*parameters[8];
  
   
  if (! quiet)
    {
      printf ("\n");
      printf ("Control inputs: \n");
      printf ("anat filename = %s \n", anat_filename);
      printf ("min value = %f \n", min_val_float);
      printf ("max value = %f \n", max_val_float);
    }


}


/*---------------------------------------------------------------------------*/
/*
  This is the main routine for program 3destpdf.
*/

int main
(
  int argc,                /* number of input arguments */
  char ** argv             /* array of input arguments */ 
)

{

  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR);
  printf ("Date:    %s \n", PROGRAM_DATE);
  printf ("\n");

  
  /*----- Program initialization -----*/
  initialize_program (argc, argv);

  

}

/*---------------------------------------------------------------------------*/






