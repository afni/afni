/*---------------------------------------------------------------------------*/
/*
  This sample program generates random stimulus functions.

  File:    RSFgen.c
  Author:  B. Douglas Ward
  Date:    06 July 1999


  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.

*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "RSFgen"                        /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_DATE "06 July 1999"              /* date of last program mod */

/*---------------------------------------------------------------------------*/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include "mrilib.h"
#include "randgen.c"


/*---------------------------------------------------------------------------*/
/*
  Global variables and constants.
*/

#define MAX_STIM 10            /* max. number of stimulus time series */
#define MAX_STRING_LENGTH 80   /* max. length of input string */


int nt = 0;             /* length of time series */
int num_stim = 0;       /* number of input stimuli (experimental conditions) */
int num_reps[MAX_STIM]; /* number of repetitions for each stimulus */
long seed = 1234567;    /* random number seed */
char * prefix = NULL;   /* prefix for output .1D stimulus functions */


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
  Routine to display help menu for program RSFgen.
*/

void display_help_menu()
{
  printf (
    "Sample program to generate random stimulus functions.                  \n"
    "                                                                       \n"
    "Usage:                                                                 \n"
    "RSFgen                                                                 \n"
    "-nt n            n = length of time series                             \n"
    "-nstim p         p = number of input stimuli (experimental conditions) \n"
    "-nreps i r       r = number of repetitions for stimulus i  (1<=i<=p)   \n"
    "[-seed s]        s = random number seed                                \n"
    "[-prefix pname]  pname = prefix for p output .1D stimulus functions    \n"
    "                   e.g., pname1.1D, pname2.1D, ..., pnamep.1D          \n"
    "                 Warning:  This will overwrite pre-existing .1D files  \n"
    "                           that have the same name.                    \n"
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
  long lval;                        /* long integer input */
  char message[MAX_STRING_LENGTH];  /* error message */
  int i;


  /*----- Does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();  


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
	  nt = ival;
	  nopt++;
	  continue;
	}


      /*-----  -nstim p  -----*/
      if (strncmp(argv[nopt], "-nstim", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  RSF_error ("need argument after -nstim ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    RSF_error ("illegal argument after -nstim ");
	  num_stim = ival;
	  nopt++;
	  continue;
	}


      /*-----  -nreps i r  -----*/
      if (strncmp(argv[nopt], "-nreps", 6) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  RSF_error ("need 2 arguments after -nreps ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > num_stim))
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


      /*-----   -prefix pname   -----*/
      if (strncmp(argv[nopt], "-prefix", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  RSF_error ("need argument after -prefix ");
	  prefix = malloc (sizeof(char) * MAX_STRING_LENGTH);
	  MTEST (prefix);
	  strcpy (prefix, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*----- Unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      RSF_error (message);
      
    }

  /*----- Print options -----*/
  printf ("nt    = %d \n", nt);
  printf ("nstim = %d \n", num_stim);
  for (i = 0;  i < num_stim;  i++)
    printf ("nreps[%d] = %d \n", i+1, num_reps[i]);
  printf ("seed  = %ld \n", seed);
}


/*---------------------------------------------------------------------------*/
/*
  Perform all program initialization. 
*/

void initialize 
(  
  int argc,                /* number of input arguments */
  char ** argv,            /* array of input arguments */ 
  int ** design            /* experimental design array */  
)

{ 
  int i, total;


  /*----- Initialize repetition number array -----*/
  for (i = 0;  i < MAX_STIM;  i++)
    num_reps[i] = 0;


  /*----- Get command line inputs -----*/
  get_options (argc, argv);


  /*----- Check for valid inputs -----*/
  if (nt == 0)        RSF_error ("Must specify nt");
  if (num_stim == 0)  RSF_error ("Must specify nstim");
  total = 0;
  for (i = 0;  i < num_stim;  i++)
    {
      if (num_reps[i] == 0)  
	RSF_error ("Must specify nreps >0 for each stimulus");
      total += num_reps[i];
    }
  if (total > nt)  RSF_error ("Can't have sum of nreps > nt ");
 

  /*----- Allocate memory for experimental design -----*/
  *design = (int *) malloc (sizeof(int) * nt);
  MTEST (*design);


  /*----- Initialize random number seed -----*/
  srand48 (seed);

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
  for (is = 0;  is < num_stim;  is++)
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
  Shuffle the experimental design array.
*/

void shuffle_array (int * design)

{
  int i, j, temp;


  for (i = 0;  i < nt;  i++)
    {
      j = rand_uniform(0.0,1.0) * nt;

      temp = design[i];
      design[i] = design[j];
      design[j] = temp;
    }
      
  return;
}


/*---------------------------------------------------------------------------*/
/*
  Print the randomized experimental design array.
*/

void print_array (int * design)

{
  int i;

  for (i = 0;  i < nt;  i++)
    {
      printf (" %d ", design[i]);
      if ((i+1) % 20 == 0)  printf ("\n");
    }

  printf ("\n");

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Write time series array to specified file.
*/

void write_ts (char * filename, int * array)
{
  int i;
  FILE * outfile = NULL;


  outfile = fopen (filename, "w");


  for (i = 0;  i < nt;  i++)
    {
      fprintf (outfile, "%d", array[i]);
      fprintf (outfile, " \n");
    }


  fclose (outfile);
}


/*---------------------------------------------------------------------------*/
/*
  Write experimental design to separate stimulus function time series files.
*/

void write_results (int * design)
{
  char filename[80];
  int * array;
  int is, i;


  /*----- Allocate memory for output array -----*/
  array = (int *) malloc (sizeof(int) * nt);
  MTEST (array);


  for (is = 1;  is <= num_stim; is++)
    {
      sprintf (filename, "%s%d.1D", prefix, is);
      printf ("\nWriting file: %s\n", filename);
      for (i = 0;  i < nt;  i++)
	{
	  if (design[i] == is)  array[i] = 1;
	  else                  array[i] = 0;
	}
      write_ts (filename, array);
    } 


  /*----- Deallocate memory -----*/
  free (array);   array = NULL;


}


/*---------------------------------------------------------------------------*/

int main 
(
  int argc,                /* number of input arguments */
  char ** argv             /* array of input arguments */ 
)

{
  int * design = NULL;

  
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR); 
  printf ("Date:    %s \n", PROGRAM_DATE);
  printf ("\n");

  
  /*----- Perform program initialization -----*/
  initialize (argc, argv, &design);


  /*----- Generate random stimulus functions -----*/
  fill_array (design);

  printf ("\nOriginal array: \n");
  print_array (design);

  shuffle_array (design);


  /*----- Output results -----*/
  printf ("\nShuffled array: \n");
  print_array (design);

  if (prefix != NULL)  write_results (design);


  /*----- Deallocate memory -----*/
  if (design != NULL)  { free (design); design = NULL; }

  return;
}

/*---------------------------------------------------------------------------*/











