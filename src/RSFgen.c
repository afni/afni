/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1999-2003, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This sample program generates random stimulus functions.

  File:    RSFgen.c
  Author:  B. Douglas Ward
  Date:    06 July 1999


  Mod:     Increased max. allowed number of input stimulus functions.
  Date:    24 August 1999

  Mod:     Added option to create multiple column stimulus function files.
  Date:    24 November 1999

  Mod:     Changed option label "-nstim" to "-num_stimts".
  Date:    29 November 1999

  Mod:     Added option "-nblock" to specify block length for each stim fn.
  Date:    15 December 1999

  Mod:     Added flag to expand array for block type design.
  Date:    14 January 2000

  Mod:     Added -markov and -pzero options.
  Date:    03 October 2000

  Mod:     Added -pseed option for permutation of stimulus labels.
  Date:    27 April 2001

  Mod:     Changes to eliminate constraints on number of stimulus time series.
  Date:    11 May 2001

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

  Mod:     Made -nblock option compatible with the Markov Chain options.
  Date:    06 March 2002

  Mod:     Added -one_col option to write stimulus functions as a single
           column of decimal integers.
  Date:    18 April 2002

  Mod:     Added -quiet option.
  Date:    30 December 2002

  Mod:     Added -table option, to generate random permutations of the rows
           of an input column or table of numbers.
  Date:    13 March 2003

*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "RSFgen"                        /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_INITIAL "06 July 1999"    /* date of initial program release */
#define PROGRAM_LATEST "13 March 2003"    /* date of latest program revision */

/*---------------------------------------------------------------------------*/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include "mrilib.h"
#include "matrix.h"

#include "randgen.c"
#include "matrix.c"


/*---------------------------------------------------------------------------*/
/*
  Global variables.
*/


int NT = 0;             /* length of stimulus time series */
int nt = 0;             /* length of simple time series (block length = 1) */
int num_stimts = 0;     /* number of input stimuli (experimental conditions) */
int * num_reps = NULL;  /* number of repetitions for each stimulus */
int * nblock = NULL;    /* block length for each stimulus */
int expand = 0;         /* flag to expand the array for block type design */
long seed = 1234567;    /* random number seed */
long pseed = 0;         /* stimulus permutation random number seed */
char * prefix = NULL;   /* prefix for output .1D stimulus functions */
int one_file = 0;       /* flag for place stim functions into a single file */
int one_col = 0;        /* flag for write stim functions as a single column */
int markov = 0;         /* flag for Markov process */
char * tpm_file = NULL; /* file name for input of transition prob. matrix */
float pzero = 0.0;      /* zero (null) state probability */
int quiet = 0;          /* flag for suppress screen output */
char * table_file = NULL;  /* input time series file */


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void RSF_error (char * message)
{
  fprintf (stderr, "\n%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/

/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
if((ptr)==NULL) \
( RSF_error ("Cannot allocate memory") )
     

/*---------------------------------------------------------------------------*/
/*
  Identify software.
*/

void identify_software ()
{
  
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR); 
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");
}


/*---------------------------------------------------------------------------*/
/*
  Routine to display help menu for program RSFgen.
*/

void display_help_menu()
{
  identify_software();

  printf (
    "Sample program to generate random stimulus functions.                  \n"
    "                                                                       \n"
    "Usage:                                                                 \n"
    PROGRAM_NAME "                                                          \n"
    "-nt n            n = length of time series                             \n"
    "-num_stimts p    p = number of input stimuli (experimental conditions) \n"
    "[-nblock i k]    k = block length for stimulus i  (1<=i<=p)            \n"
    "                     (default: k = 1)                                  \n"
    "[-seed s]        s = random number seed                                \n"
    "[-quiet]         flag to suppress screen output                        \n"
    "[-one_file]      place stimulus functions into a single .1D file       \n"
    "[-one_col]       write stimulus functions as a single column of decimal\n"
    "                   integers (default: multiple columns of binary nos.) \n"
    "[-prefix pname]  pname = prefix for p output .1D stimulus functions    \n"
    "                   e.g., pname1.1D, pname2.1D, ..., pnamep.1D          \n"
    "                                                                       \n"
    "The following Random Permutation, Markov Chain, and Input Table options\n"
    "are mutually exclusive.                                                \n"
    "                                                                       \n"
    "Random Permutation options:                                            \n"
    "-nreps i r       r = number of repetitions for stimulus i  (1<=i<=p)   \n"
    "[-pseed s]       s = stim label permutation random number seed         \n"
    "                                     p                                 \n"
    "                 Note: Require n >= Sum (r[i] * k[i])                  \n"
    "                                    i=1                                \n"
    "                                                                       \n"
    "Markov Chain options:                                                  \n"
    "-markov mfile    mfile = file containing the transition prob. matrix   \n"
    "[-pzero z]       probability of a zero (i.e., null) state              \n"
    "                     (default: z = 0)                                  \n"
    "                                                                       \n"
    "Input Table row permutation options:                                   \n"
    "[-table dfile]   dfile = filename of column or table of numbers        \n"
    "                 Note: dfile may have a column selector attached       \n"
    "                 Note: With this option, all other input options,      \n"
    "                       except -seed and -prefix, are ignored           \n"
    "                                                                       \n"
    "                                                                       \n"
    "Warning: This program will overwrite pre-existing .1D files            \n"
    "                                                                       \n"
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
  int ival;                         /* integer input */
  float fval;                       /* float input */
  long lval;                        /* long integer input */
  char message[THD_MAX_NAME];       /* error message */
  int i;


  /*----- Does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();  

  
  /*----- add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv); 


  /*----- Main loop over input options -----*/
  while (nopt < argc )
    {
     
      /*-----  -nt n  -----*/
      if (strncmp(argv[nopt], "-nt", 3) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  RSF_error ("need argument after -nt ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    RSF_error ("illegal argument after -nt ");
	  NT = ival;
	  nopt++;
	  continue;
	}


      /*-----  -num_stimts p  -----*/
      if (strncmp(argv[nopt], "-num_stimts", 11) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  RSF_error ("need argument after -num_stimts ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    RSF_error ("illegal argument after -num_stimts ");
	  num_stimts = ival;

	  /*----- Initialize repetition number array -----*/
	  num_reps = (int *) malloc (sizeof(int) * num_stimts);
	  MTEST (num_reps);
	  for (i = 0;  i < num_stimts;  i++)
	    num_reps[i] = 0;

	  /*----- Initialize block length array -----*/
	  nblock = (int *) malloc (sizeof(int) * num_stimts);
	  MTEST (nblock);
	  for (i = 0;  i < num_stimts;  i++)
	    nblock[i] = 1;

	  nopt++;
	  continue;
	}


      /*-----  -nreps i r  -----*/
      if (strncmp(argv[nopt], "-nreps", 6) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  RSF_error ("need 2 arguments after -nreps ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > num_stimts))
	    RSF_error ("illegal i argument for -nreps i r ");
	  i = ival - 1;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    RSF_error ("illegal r argument for -nreps i r ");
	  num_reps[i] = ival;
	  nopt++;
	  continue;
	}


      /*-----  -nblock i k  -----*/
      if (strncmp(argv[nopt], "-nblock", 7) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  RSF_error ("need 2 arguments after -nblock ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > num_stimts))
	    RSF_error ("illegal i argument for -nblock i k ");
	  i = ival - 1;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    RSF_error ("illegal k argument for -nblock i k ");
	  nblock[i] = ival;
	  if (ival > 1)  expand = 1;
	  nopt++;
	  continue;
	}


      /*-----  -seed s  -----*/
      if (strncmp(argv[nopt], "-seed", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  RSF_error ("need argument after -seed ");
	  sscanf (argv[nopt], "%ld", &lval);
	  if (lval <= 0)
	    RSF_error ("illegal argument after -seed ");
	  seed = lval;
	  nopt++;
	  continue;
	}


      /*-----  -pseed s  -----*/
      if (strcmp(argv[nopt], "-pseed") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  RSF_error ("need argument after -pseed ");
	  sscanf (argv[nopt], "%ld", &lval);
	  if (lval <= 0)
	    RSF_error ("illegal argument after -pseed ");
	  pseed = lval;
	  nopt++;
	  continue;
	}


      /*-----  -quiet  -----*/
      if (strcmp(argv[nopt], "-quiet") == 0)
	{
	  quiet = 1;
	  nopt++;
	  continue;
	}


      /*-----  -one_file  -----*/
      if (strncmp(argv[nopt], "-one_file", 9) == 0)
	{
	  one_file = 1;
	  nopt++;
	  continue;
	}


      /*-----  -one_col  -----*/
      if (strcmp(argv[nopt], "-one_col") == 0)
	{
	  one_col = 1;
	  nopt++;
	  continue;
	}


      /*-----   -prefix pname   -----*/
      if (strncmp(argv[nopt], "-prefix", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  RSF_error ("need argument after -prefix ");
	  prefix = AFMALL(char, sizeof(char) * THD_MAX_NAME);
	  MTEST (prefix);
	  strcpy (prefix, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -markov mfile   -----*/
      if (strcmp(argv[nopt], "-markov") == 0)
	{
	  markov = 1;
	  nopt++;
	  if (nopt >= argc)  RSF_error ("need argument after -markov ");
	  tpm_file = AFMALL(char, sizeof(char) * THD_MAX_NAME);
	  MTEST (tpm_file);
	  strcpy (tpm_file, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*-----   -pzero z   -----*/
      if (strcmp(argv[nopt], "-pzero") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  RSF_error ("need argument after -pzero ");
	  sscanf (argv[nopt], "%f", &fval);
	  if ((fval < 0.0) || (fval > 1.0))  
	    RSF_error ("Require  0.0 <= pzero <= 1.0");
	  pzero = fval;
	  nopt++;
	  continue;
	}
      

      /*-----   -table dfile   -----*/
      if (strcmp(argv[nopt], "-table") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  RSF_error ("need argument after -table ");
	  table_file = AFMALL(char, sizeof(char) * THD_MAX_NAME);
	  MTEST (table_file);
	  strcpy (table_file, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*----- Unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      RSF_error (message);
      
    }
}


/*---------------------------------------------------------------------------*/
/*
  Read input table.
*/

void read_table 
(
  char * table_file,       /* file containing input table */
  MRI_IMAGE ** flim        /* data structure containing input table */
)

{
  int i;
  char message[THD_MAX_NAME];  /* error message */

  /*----- Read table -----*/
  *flim = mri_read_1D(table_file);
  if ((*flim) == NULL)  
    {
      sprintf (message,  "Unable to read table file: %s", table_file);
      RSF_error (message);
    }

  /*----- Initialize control variables -----*/
  NT = (*flim)->nx;
  num_stimts = NT;
  markov = 0;
  pseed = 0;

  /*----- Initialize repetition number array -----*/
  num_reps = (int *) malloc (sizeof(int) * num_stimts);
  MTEST (num_reps);
  for (i = 0;  i < num_stimts;  i++)
    num_reps[i] = 1;
  
  /*----- Initialize block length array -----*/
  nblock = (int *) malloc (sizeof(int) * num_stimts);
  MTEST (nblock);
  for (i = 0;  i < num_stimts;  i++)
    nblock[i] = 1;
  
}


/*---------------------------------------------------------------------------*/
/*
  Print input options. 
*/

void print_options ()

{
  int i;

  identify_software();
  
  if (table_file != NULL)
    {
      printf ("table file    = %s \n", table_file);
      printf ("nt            = %d \n", NT);
      printf ("seed          = %ld \n", seed);
      printf ("output prefix = %s \n", prefix);
    }
  else
    {
      printf ("nt            = %d \n", NT);
      printf ("num_stimts    = %d \n", num_stimts);
      printf ("seed          = %ld \n", seed);
      if (pseed)  printf ("pseed         = %ld \n", pseed);
      printf ("output prefix = %s \n", prefix);
      if (markov)
	{
	  printf ("TPM file      = %s \n", tpm_file);
	  printf ("pzero         = %f \n", pzero);
	  for (i = 0;  i < num_stimts;  i++)
	    printf ("nblock[%d] = %d \n", i+1, nblock[i]);
	}
      else
	for (i = 0;  i < num_stimts;  i++)
	  printf ("nreps[%d]  = %d    nblock[%d] = %d \n", 
		  i+1, num_reps[i], i+1, nblock[i]);
    }
}


/*---------------------------------------------------------------------------*/
/*
  Perform all program initialization. 
*/

void initialize 
(  
  int argc,                /* number of input arguments */
  char ** argv,            /* array of input arguments */ 
  int ** darray,           /* original design array (block length = 1) */
  int ** earray,           /* expanded design array (arbitrary block length) */
  MRI_IMAGE ** flim        /* data structure containing input table */
)

{ 
  int i, total;


  /*----- Get command line inputs -----*/
  get_options (argc, argv);


  /*----- Read input table -----*/
  if (table_file != NULL)  read_table (table_file, flim);


  /*----- Print input options -----*/
  if (! quiet)  print_options ();


  /*----- Check for valid inputs -----*/
  if (NT == 0)          RSF_error ("Must specify nt");
  if (num_stimts == 0)  RSF_error ("Must specify num_stimts");
  total = 0;
  nt = NT;

  if (! markov)
    {
      for (i = 0;  i < num_stimts;  i++)
	{
	  if (num_reps[i] == 0)  
	    RSF_error ("Must specify nreps > 0 for each stimulus");
	  total += num_reps[i] * nblock[i];
	  nt -= num_reps[i] * (nblock[i] - 1);
	}
      if (total > NT)  RSF_error ("Require  nt >= Sum (r[i] * k[i]) ");
    }


  /*----- Allocate memory for experimental design -----*/
  *darray = (int *) malloc (sizeof(int) * nt);   MTEST (*darray);
  *earray = (int *) malloc (sizeof(int) * NT);   MTEST (*earray);


}


/*---------------------------------------------------------------------------*/
/*
  Use Markov chain to create random stimulus functions.
*/

void markov_array (int * design)

{
  int it, is, id, isprev;
  float prob, cumprob;
  matrix tpm;
  char message[THD_MAX_NAME];  /* error message */


  matrix_initialize (&tpm);


  /*----- Read the transition probability matrix -----*/
  matrix_file_read (tpm_file, num_stimts, num_stimts, &tpm, 1);
  if (tpm.elts == NULL)
    { 
      sprintf (message,  "Unable to read Markov chain matrix from file: %s", 
	       tpm_file);
      RSF_error (message);
    }  
  if (!quiet)  matrix_sprint ("\nTPM matrix:", tpm);


  /*----- Verify that the TPM has the correct form -----*/
  for (is = 0;  is < num_stimts;  is++)
    {
      cumprob = 0.0;
      for (it = 0;  it < num_stimts;  it++)
	cumprob += tpm.elts[is][it];
      if (cumprob < 0.9999)  
	{
	  sprintf (message, "Row %d of TPM sums to %f, which is < 1.0",
		   is, cumprob);
	  RSF_error (message);
	}
      if (cumprob > 1.0001)  
	{
	  sprintf (message, "Row %d of TPM sums to %f, which is > 1.0",
		   is, cumprob);
	  RSF_error (message);
	}
    }
  

  /*----- Initialize the experimental design array -----*/
  for (it = 0;  it < NT;  it++)
    design[it] = 0;


  /*----- Initialize random number seed -----*/
  srand48 (seed);


  /*----- Generate Markov process -----*/
  isprev = (int) (rand_uniform(0.0,1.0)*num_stimts);
  it = 0;  id = 0;
  while (it < NT)
    {
      if ((pzero > 0.0) && (rand_uniform(0.0,1.0) < pzero))
	{
	  design[id] = 0;
	  id++;  it++;
	} 
      else
	{
	  prob = rand_uniform(0.0,1.0);
	  cumprob = 0.0;
	  for (is = 0;  is < num_stimts;  is++)
	    {
	      cumprob += tpm.elts[isprev][is];
	      if (prob <= cumprob)
		{
		  design[id] = is+1;
		  isprev = is;
		  id++;  it += nblock[is];
		  break;
		}
	    }
	}
    }

  nt = id;


  matrix_destroy (&tpm);

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Fill the experimental design array.
*/

void fill_array (int * design)

{
  int i, is, m;


  for (i = 0;  i < nt;  i++)
    design[i] = 0;

  i = 0;
  for (is = 0;  is < num_stimts;  is++)
    {
      for (m = 0;  m < num_reps[is];  m++)
	{
	  design[i] = is+1;
	  i++;
	}
    }


  return;
}


/*---------------------------------------------------------------------------*/
/*
  Permute the stimulus functions within the experimental design array.
*/

void permute_array (int * design)

{
  int i, j, temp;
  int is, nb;


  /*----- Initialize random number seed -----*/
  srand48 (pseed);

  
  /*----- Determine total number of blocks -----*/
  nb = 0;
  for (is = 0;  is < num_stimts;  is++)
    nb += num_reps[is];


  /*----- Permute the blocks -----*/
  for (i = 0;  i < nb;  i++)
    {
      j = rand_uniform(0.0,1.0) * nb;

      /*----- Just in case -----*/
      if (j < 0)  j = 0;
      if (j > nb-1)  j = nb-1;

      temp = design[i];
      design[i] = design[j];
      design[j] = temp;
    }
      
  return;
}


/*---------------------------------------------------------------------------*/
/*
  Shuffle the experimental design array.
*/

void shuffle_array (int * design)

{
  int i, j, temp;


  /*----- Initialize random number seed -----*/
  srand48 (seed);


  for (i = 0;  i < nt;  i++)
    {
      j = rand_uniform(0.0,1.0) * nt;

      /*----- Just in case -----*/
      if (j < 0)  j = 0;
      if (j > nt-1)  j = nt-1;

      temp = design[i];
      design[i] = design[j];
      design[j] = temp;
    }
      
  return;
}


/*---------------------------------------------------------------------------*/
/*
  Expand the experimental design array, to allow for block-type designs.
*/

void expand_array (int * darray, int * earray)

{
  int i, j, k, m;

  j = 0;
  for (i = 0;  i < nt, j < NT;  i++)
    {
      m = darray[i];

      if (m == 0)
	{
	  earray[j] = 0;
	  j++;
	}
      else
	{
	  for (k = 0;  k < nblock[m-1];  k++)
	    {
	      earray[j] = m;
	      j++;
	      if (j >= NT)  break;
	    }  
	}
    }
      
  return;
}


/*---------------------------------------------------------------------------*/
/*
  Print array.
*/

void print_array (int * array, int n)

{
  int i;

  for (i = 0;  i < n;  i++)
    {
      printf (" %2d ", array[i]);
      if ((i+1) % 20 == 0)  printf ("\n");
    }

  printf ("\n");

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Print labeled array.
*/

void sprint_array (char * str, int * array, int n)

{
  int i;

  if (!quiet)
    {
      printf ("%s \n", str);
      print_array (array, n);
    }

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Write one time series array to specified file.
*/

void write_one_ts (char * filename, int * array)
{
  int i;
  FILE * outfile = NULL;


  outfile = fopen (filename, "w");


  for (i = 0;  i < NT;  i++)
    {
      fprintf (outfile, "%d", array[i]);
      fprintf (outfile, " \n");
    }


  fclose (outfile);
}


/*---------------------------------------------------------------------------*/
/*
  Write multiple time series arrays to specified file.
*/

void write_many_ts (char * filename, int * design)
{
  int it, is;
  FILE * outfile = NULL;


  outfile = fopen (filename, "w");


  for (it = 0;  it < NT;  it++)
    {
      if (one_col)
	fprintf (outfile, "  %d", design[it]);
      else
	for (is = 0;  is < num_stimts;  is++)
	  if (design[it] == is+1)
	    fprintf (outfile, "  %d", 1);
	  else
	    fprintf (outfile, "  %d", 0);	  
      fprintf (outfile, " \n");
    }


  fclose (outfile);
}


/*---------------------------------------------------------------------------*/
/*
  Write experimental design to stimulus function time series files.
*/

void write_results (char * prefix, int * design, int NT)
{
  char filename[THD_MAX_NAME];    /* output file name */
  int * array;                    /* output binary sequence representing one
                                     stimulus function. */
  int is, i;


  if (one_file || one_col)
    {
      sprintf (filename, "%s.1D", prefix);
      if (!quiet)  printf ("\nWriting file: %s\n", filename);
      write_many_ts (filename, design);
    }

  else
    {
      /*----- Allocate memory for output array -----*/
      array = (int *) malloc (sizeof(int) * NT);
      MTEST (array);

      for (is = 1;  is <= num_stimts; is++)
	{
	  sprintf (filename, "%s%d.1D", prefix, is);
	  if (!quiet)  printf ("\nWriting file: %s\n", filename);
	  for (i = 0;  i < NT;  i++)
	    {
	      if (design[i] == is)  array[i] = 1;
	      else                  array[i] = 0;
	    }
	  write_one_ts (filename, array);
	}  
      
      /*----- Deallocate memory -----*/
      free (array);   array = NULL;
    }

}


/*---------------------------------------------------------------------------*/
/*
  Write permutation of input table.
*/

void write_table 
(
  char * prefix,                  /* prefix for output file */
  int * design,                   /* permutation array */
  MRI_IMAGE * flim                /* data structure containing input table */
)

{
  FILE * outfile = NULL;          /* pointer to output file */
  char filename[THD_MAX_NAME];    /* output file name */
  int nx, ny;                     /* dimensions of input table */
  int it, is, icol;               /* row and column indices */
  float * far;                    /* pointer to input table float array */ 


  /*----- Assemble output file name -----*/
  sprintf (filename, "%s.1D", prefix);
  outfile = fopen (filename, "w");

  /*----- Get dimensions of input table -----*/
  nx = flim->nx ;
  ny = flim->ny ;

  /*----- Set pointer to input table float array -----*/
  far = MRI_FLOAT_PTR (flim);

  /*----- Loop over rows of table -----*/
  for (it = 0;  it <nx;  it++)
    {
      /*----- Get permuted row index -----*/
      is = design[it]-1;

      /*----- Copy row from input table to output file -----*/
      for (icol = 0;  icol < ny;  icol++)
	{
	  fprintf (outfile, "  %f", far[icol*nx+is]);
	}
      fprintf (outfile, " \n");
    }

  fclose (outfile);
}


/*---------------------------------------------------------------------------*/

int main 
(
  int argc,                /* number of input arguments */
  char ** argv             /* array of input arguments */ 
)

{
  int * darray = NULL;     /* design array (block length = 1) */
  int * earray = NULL;     /* expanded array (arbitrary block length) */
  MRI_IMAGE * flim = NULL; /* data structure containing input table */

  
  /*----- Perform program initialization -----*/
  initialize (argc, argv, &darray, &earray, &flim);


  /*----- Use Markov chain to generate random stimulus functions -----*/
  if (markov)
    {
      markov_array (darray);
      sprint_array ("\nMarkov chain time series: ", darray, nt);
    }

  /*----- Use random permutations to generate random stimulus functions -----*/
  else
    {
      /*----- Generate required number of repetitions of stim. fns. -----*/
      fill_array (darray);
      if (!quiet)  sprint_array ("\nOriginal array: ", darray, nt);
      
      /*----- Permute the stimulus functions -----*/
      if (pseed)  
	{
	  permute_array (darray);
	  if (!quiet)  sprint_array ("\nPermuted array: ", darray, nt);
	}

      /*----- Randomize the order of the stimulus functions -----*/
      shuffle_array (darray);
      if (!quiet)  sprint_array ("\nShuffled array: ", darray, nt);

    }

  
  /*----- Expand the darray for block type designs -----*/
  expand_array (darray, earray);
  if (expand && (!quiet))  sprint_array ("\nExpanded array: ", earray, NT);
  

  /*----- Output results -----*/
  if (prefix != NULL)  
    {
      if (flim == NULL)
	write_results (prefix, earray, NT);
      else
	write_table (prefix, earray, flim);
    }


  /*----- Deallocate memory -----*/
  if (darray != NULL)  { free (darray);   darray = NULL; }
  if (earray != NULL)  { free (earray);   earray = NULL; }
  if (flim   != NULL)  { mri_free(flim);  flim = NULL; }
  
  exit(0);
}

/*---------------------------------------------------------------------------*/




