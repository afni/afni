/*
  This program performs the nonparametric Kruskal-Wallis test for comparison
  of more than two treatments.

  File:    3dKruskalWallis.c
  Author:  B. Douglas Ward
  Date:    23 July 1997

  Mod:     Added changes for incorporating History notes.
  Date:    08 September 1999

  Mod:     Replaced dataset input code with calls to THD_open_dataset,
           to allow operator selection of individual sub-bricks for input.
  Date:    03 December 1999


*/


/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/


/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dKruskalWallis"               /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_DATE "03 December 1999"          /* date of last program mod */

#define MAX_TREATMENTS 100     /* max. number of treatments */
#define MAX_OBSERVATIONS 100   /* max. number of observations per treatment */
#define MAX_NAME_LENGTH 80     /* max. strength length for file names */ 
#define MEGA  1048576          /* one megabyte */


#include <stdio.h>
#include <math.h>
#include "mrilib.h"


typedef struct NP_options
{ 
  int   datum;                  /* data type for "intensity" data subbrick */
  char  session[MAX_NAME_LENGTH];     /* name of output directory */

  
  int   nvoxel;                 /* number of voxel for special output */

  int   s;                      /* number of treatments */
  int   n[MAX_TREATMENTS];      /* number of observations per treatment */

  char  *** xname;              /* names of the input data files */
  char  * first_dataset;        /* name of the first data set */
   
  int   nx, ny, nz;             /* data set dimensions */
  int   nxyz;                   /* number of voxels per image */

  int workmem;                  /* working memory */

  char  * outfile;              /* name of output file  */


} NP_options;


#include "NPstats.c"


/*---------------------------------------------------------------------------*/
/*
   Routine to display 3dKruskalWallis help menu.
*/

void display_help_menu()
{
  printf 
    (
     "This program performs nonparametric Kruskal-Wallis test for         \n"
     "comparison of multiple treatments.                                \n\n"
     "Usage:                                                              \n"
     "3dKruskalWallis                                                     \n"
     "-levels s                      s = number of treatments             \n"
     "-dset 1 filename               data set for treatment #1            \n"
     " . . .                           . . .                              \n"
     "-dset 1 filename               data set for treatment #1            \n"
     " . . .                           . . .                              \n"
     "-dset s filename               data set for treatment #s            \n"
     " . . .                           . . .                              \n"
     "-dset s filename               data set for treatment #s            \n"
     "                                                                    \n"
     "[-workmem mega]                number of megabytes of RAM to use    \n"
     "                                 for statistical workspace          \n"
     "[-voxel num]                   screen output for voxel # num        \n"
     "-out prefixnamename            Kruskal-Wallis statistics are written\n"
     "                                 to file prefixname                 \n"
     "\n");
  
  
  printf
    (
     "\n"
     "N.B.: For this program, the user must specify 1 and only 1 sub-brick  \n"
     "      with each -dset command. That is, if an input dataset contains  \n"
     "      more than 1 sub-brick, a sub-brick selector must be used, e.g.: \n"
     "      -dset 2 'fred+orig[3]'                                          \n"
     );
  
  exit(0);
}


/*---------------------------------------------------------------------------*/
/*
   Routine to initialize input options.
*/

void initialize_options (NP_options * option_data)
{
  int i;          /* index */
  
  option_data->datum = ILLEGAL_TYPE;
  strcpy (option_data->session, "./");
 

  option_data->nvoxel = -1;
  
  option_data->s = 0;
  
  for (i = 0;  i < MAX_TREATMENTS;  i++)
    option_data->n[i] = 0;

  option_data->workmem = 12;
 
  /*----- allocate memory for storing data file names -----*/
  option_data->xname = (char ***) malloc (sizeof(char **) * MAX_TREATMENTS);
  for (i = 0;  i < MAX_TREATMENTS;  i++)
    option_data->xname[i]
      = (char **) malloc (sizeof(char *) * MAX_OBSERVATIONS);

  option_data->first_dataset = NULL;
  
  option_data->nx = 0;
  option_data->ny = 0;
  option_data->nz = 0;
  option_data->nxyz = 0;

  option_data->outfile = NULL;

}

   
/*---------------------------------------------------------------------------*/
/*
   Routine to get user specified Kruskal-Wallis options.
*/

void get_options (int argc, char ** argv, NP_options * option_data)
{
  int nopt = 1;                  /* input option argument counter */
  int ival;                      /* integer input */
  int nijk;                      /* count of data files */     
  float fval;                    /* float input */
  THD_3dim_dataset * dset=NULL;             /* test whether data set exists */
  char message[MAX_NAME_LENGTH];            /* error message */


  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();

  /*----- initialize the input options -----*/
  initialize_options (option_data);

  
  /*----- main loop over input options -----*/
  while (nopt < argc)
    {
      
      
      /*-----   -datum type   -----*/
      if( strncmp(argv[nopt],"-datum",6) == 0 ){
	if( ++nopt >= argc ) NP_error("need an argument after -datum!") ;
	
	if( strcmp(argv[nopt],"short") == 0 ){
	  option_data->datum = MRI_short ;
	} else if( strcmp(argv[nopt],"float") == 0 ){
	  option_data->datum = MRI_float ;
	} else {
	  char buf[256] ;
	  sprintf(buf,
		  "-datum of type '%s' is not supported in 3dKruskalWallis.",
		  argv[nopt] ) ;
	  NP_error(buf) ;
	}
	nopt++ ; continue ;  /* go to next arg */
      }
      
      
      /*-----   -session dirname    -----*/
      if( strncmp(argv[nopt],"-session",6) == 0 ){
	nopt++ ;
	if( nopt >= argc ) NP_error("need argument after -session!") ;
	strcpy(option_data->session , argv[nopt++]) ;
	continue ;
      }
      
      
      /*-----   -voxel num  -----*/
      if (strncmp(argv[nopt], "-voxel", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NP_error ("need argument after -voxel ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    NP_error ("illegal argument after -voxel ");
	  option_data->nvoxel = ival;
	  nopt++;
	  continue;
	}
      
      
      /*-----   -workmem megabytes  -----*/

      if( strncmp(argv[nopt],"-workmem",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) NP_error ("need argument after -workmem!") ;
         sscanf (argv[nopt], "%d", &ival);
         if( ival <= 0 ) NP_error ("illegal argument after -workmem!") ;
         option_data->workmem = ival ;
         nopt++ ; continue ;
      }


      /*-----   -levels s  -----*/
      if (strncmp(argv[nopt], "-levels", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NP_error ("need argument after -levels ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > MAX_TREATMENTS))
	    NP_error ("illegal argument after -levels ");
	  option_data->s = ival;
	  nopt++;
	  continue;
	}
      
      
      /*-----   -dset level filename   -----*/
      if (strncmp(argv[nopt], "-dset", 5) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  NP_error ("need 2 arguments after -dset ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->s))
	    NP_error ("illegal argument after -dset ");
	  
	  option_data->n[ival-1] += 1;

	  if (option_data->n[ival-1] > MAX_OBSERVATIONS)
	    NP_error ("too many data files");
	  nijk = option_data->n[ival-1];
	  
	  /*--- check whether input files exist ---*/
	  nopt++;
	  dset = THD_open_dataset( argv[nopt] ) ;
	  if( ! ISVALID_3DIM_DATASET(dset) )
	    {
	     sprintf(message,"Unable to open dataset file %s\n", argv[nopt]);
	     NP_error (message);
	    }

	  /*--- check number of selected sub-bricks ---*/
	  if (DSET_NVALS(dset) != 1)
	    {
	      sprintf(message,"Must specify exactly 1 sub-brick for file %s\n",
		      argv[nopt]);
	      NP_error (message);
	    }

	  THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
	  
	  option_data->xname[ival-1][nijk-1] 
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->xname[ival-1][nijk-1], argv[nopt]);
	  nopt++;
	  continue;
	}
      
      
      /*-----   -out filename   -----*/
      if (strncmp(argv[nopt], "-out", 4) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  NP_error ("need argument after -out ");
	  option_data->outfile = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->outfile, argv[nopt]);
	  nopt++;
	  continue;
	}
            
      
      /*----- unknown command -----*/
      NP_error ("unrecognized command line option ");
    }

}


/*---------------------------------------------------------------------------*/
/*
  Routine to check for valid inputs.
*/

void check_for_valid_inputs (NP_options * option_data)
{
  int i;
  char message[MAX_NAME_LENGTH];            /* error message */


  for (i = 0;  i < option_data->s;  i++)
    if (option_data->n[i] < 1)
      {
	sprintf(message,"Too few data sets for treatment level %d \n", 
		i+1);
	NP_error (message);
      } 

  if (option_data->nvoxel > option_data->nxyz)
    NP_error ("argument of -voxel is too large");

}


/*---------------------------------------------------------------------------*/
/*
  Routine to perform all Kruskal-Wallis initialization.
*/

void initialize 
(
  int argc,                    /* number of input arguments */
  char ** argv,                /* array of input arguments */ 
  NP_options ** option_data,   /* user input options */
  float ** best,               /* index of best treatment */
  float ** kstat               /* Kruskal-Wallis statistic */
)

{
  
  
  /*----- allocate memory space for input data -----*/   
  *option_data = (NP_options *) malloc(sizeof(NP_options));
  if (*option_data == NULL)
    NP_error ("memory allocation error");
  
  /*----- get command line inputs -----*/
  get_options(argc, argv, *option_data);
  
  /*----- use first data set to get data set dimensions -----*/
  (*option_data)->first_dataset = (*option_data)->xname[0][0];
  get_dimensions (*option_data);
  printf ("Data set dimensions:  nx = %d  ny = %d  nz = %d  nxyz = %d \n",
	  (*option_data)->nx, (*option_data)->ny,
	  (*option_data)->nz, (*option_data)->nxyz);
  

  /*----- check for valid inputs -----*/
  check_for_valid_inputs (*option_data);
    
  /*----- check whether output files already exist -----*/
  check_one_output_file (*option_data, (*option_data)->outfile);

  /*----- allocate memory -----*/
  *best = (float *) malloc(sizeof(float) * (*option_data)->nxyz);
  if (*best == NULL)
    NP_error ("memory allocation error");
  *kstat = (float *) malloc(sizeof(float) * (*option_data)->nxyz);
  if (*kstat == NULL)
    NP_error ("memory allocation error");
 
  
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the Kruskal-Wallis statistic.
*/

void calc_stat 
(
  int nvox,                          /* flag for voxel output */
  int s,                             /* number of treatments */
  int  * n,                          /* number of observations per treatment */
  float ** xarray,                   /* array of data arrays */
  float * best,                      /* index of best treatment */
  float * kstat                      /* Kruskal-Wallis statistic */
)

{
  const float EPSILON = 1.0e-10;      /* protection from roundoff error */
  int i, j;                   /* array indices */
  node * head = NULL;         /* points to head of list */        
  node * ptr = NULL;          /* points to current position in list */
  int NN;                     /* total number of sample points */
  float rsum;                 /* sum of squares of ranks */
  int d;                      /* count of number of ties */ 
  float corr;                 /* correction to account for ties */
  float rank;                 /* rank of data point */
  float ranksum;              /* sum of ranks for ith treatment */
  float knum;                 /* numerator of Kruskal-Wallis statistic */
  float kden;                 /* denominator of Kruskal-Wallis statistic */
  float best_rank;            /* best average rank for a treatment */


  /*----- count total number of datasets -----*/
  NN = 0;
  for (i = 0;  i < s;  i++)
    NN += n[i];


  /*----- enter data arrays -----*/
  for (i = 0;  i < s;  i++)
    for (j = 0;  j < n[i];  j++)
      node_addvalue (&head, xarray[i][j]);


  /*----- if display voxel, write the ranks of the input data -----*/
  if (nvox > 0)
    {
      printf ("\n");
      for (i = 0;  i < s;  i++)
	{
	  printf ("Y%d ranks: ", i+1);
	  for (j = 0;  j < n[i];  j++)
	    {
	      rank = node_get_rank (head, xarray[i][j]);
	      printf (" %6.1f", rank);
	      if (((j+1) % 8 == 0) && (j < n[i]-1))
		printf ("\n          ");		  
	    }
	  printf ("\n");
	}
      printf ("\n");
      for (i = 0;  i < s;  i++)
	{
	  printf ("Y%d: ", i+1);
	  ranksum = 0.0;
	  for (j = 0;  j < n[i];  j++)
	    {
	      rank = node_get_rank (head, xarray[i][j]);
	      ranksum += rank;
	    }
	  printf ("   Rank sum = %6.1f    Rank average = %6.1f \n", 
		  ranksum, ranksum/n[i]);
	}
      printf ("\n");
    }


  /*----- calculate the sum of the rank averages -----*/
  rsum = 0.0;
  *best = 0.0;
  best_rank = (NN + 1.0) / 2.0 + EPSILON;
  for (i = 0;  i < s;  i++)
    {
      ranksum = 0.0;
      for (j = 0;  j < n[i];  j++)
	ranksum += node_get_rank (head, xarray[i][j]);
      rsum += ranksum * ranksum / n[i];

      if (ranksum / n[i] > best_rank)
	{
	  *best = (float) (i+1);
	  best_rank = ranksum / n[i];
	}
    }


  /*----- numerator of Kruskal-Wallis statistic -----*/
  knum = (12.0/(NN*(NN+1)))*rsum - 3.0*(NN+1);


  /*----- denominator of Kruskal-Wallis statistic -----*/
  corr = 0.0;
  ptr = head;
  while (ptr != NULL)
    {
      d = ptr->d;
      corr += d*d*d - d;
      ptr = ptr->next;
    }
  kden = 1.0 - (corr / (NN*NN*NN-NN));


  /*----- calculate Kruskal-Wallis statistic -----*/
  if (kden < EPSILON)
    *kstat = 0.0;
  else
    *kstat = knum / kden;
  if (nvox > 0)  printf ("K = %f \n", *kstat);


  /*----- deallocate memory -----*/
  list_delete (&head);
}


/*---------------------------------------------------------------------------*/
/*
  Calculate results for a single voxel.
*/

void process_voxel
(
  int nvox,                          /* flag for voxel output */
  int s,                             /* number of treatments */
  int * n,                           /* number of observations per treatment */
  float ** xarray,                   /* array of data arrays */
  float * best,                      /* index of best treatment */
  float * kstat                      /* Kruskal-Wallis statistic */
)

{
  int i;                             /* treatment index */
  int j;                             /* array index */


  /*----- check for voxel output  -----*/
  if (nvox > 0)
    {
      printf ("\nResults for voxel #%d : \n\n", nvox);

      for (i = 0;  i < s;  i++)
	{
	  printf ("Y%d data:  ", i+1);
	  for (j = 0;  j < n[i];  j++)
	    {
	      printf (" %6.1f", xarray[i][j]);
	      if (((j+1) % 8 == 0) && (j < n[i]-1))
		printf ("\n          ");
	    }
	  printf ("\n");
	  if (n[i] > 8)  printf ("\n");
	}
      if (n[s] <= 8)  printf ("\n");
    }


  /*----- calculate Kruskal-Wallis statistic -----*/
  calc_stat (nvox, s, n, xarray, best, kstat);

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the Kruskal-Wallis statistics for all voxels  (by breaking the 
  datasets into sub-volumes, if necessary).
*/

void calculate_results 
(
  NP_options * option_data,    /* user input options */
  float * best,                /* index of best treatment */
  float * kstat                /* Kruskal-Wallis statistics */
)

{
  int i, j, m;                 /* array indices */
  int s;                       /* number of treatments */  
  int * n;                     /* number of observations per treatment */
  int nxyz;                    /* number of voxels per dataset */
  int num_datasets;            /* total number of datasets */
  int piece_size;              /* number of voxels in dataset sub-volume */
  int num_pieces;              /* dataset is divided into this many pieces */
  int piece;                   /* piece index */
  int piece_len;               /* number of voxels in current sub-volume */
  int fim_offset;              /* array offset to current sub-volume */
  int ivox;                    /* index to voxels in current sub-volume */
  int nvox;                    /* index of voxel within entire volume */
  float b;                     /* index of best treatment */
  float k;                     /* Kruskal-Wallis statistic */
  float ** xfimar;             /* array of sub-volumes of datasets */
  float ** xarray;             /* array of data arrays */


  /*----- initialize local variables -----*/
  s = option_data->s;
  nxyz = option_data->nxyz;
  num_datasets = 0;
  n = (int *) malloc (sizeof(int) * s);  MTEST(n);
  for (i = 0;  i < s;  i++)
    {
      n[i] = option_data->n[i];
      num_datasets += n[i];
    }


  /*----- break problem into smaller pieces -----*/
  piece_size = option_data->workmem * MEGA / (num_datasets * sizeof(float));
  if (piece_size > nxyz)  piece_size = nxyz;
  num_pieces = (nxyz + piece_size - 1) / piece_size;
  printf ("num_pieces = %d    piece_size = %d \n", num_pieces, piece_size);    

  
  /*----- allocate memory space -----*/
  xarray = (float **) malloc (sizeof(float *) * s);  MTEST(xarray);
  for (i = 0;  i < s;  i++)
    {
      xarray[i] = (float *) malloc (sizeof(float) * option_data->n[i]);
      MTEST(xarray[i]);
    }

  xfimar = (float **) malloc (sizeof(float *) * num_datasets);  MTEST(xfimar);
  for (i = 0;  i < num_datasets;  i++)
    {
      xfimar[i] = (float *) malloc (sizeof(float) * piece_size);  
      MTEST(xfimar[i]);
    }


  /*----- loop over the pieces of the input datasets -----*/
  nvox = 0;
  for (piece = 0;  piece < num_pieces;  piece++)
    {
      printf ("piece = %d \n", piece);
      fim_offset = piece * piece_size;
      if (piece < num_pieces-1)
	piece_len = piece_size;
      else
	piece_len = nxyz - fim_offset;


      /*----- read in sub-volume of data from each dataset -----*/
      m = 0;
      for (i = 0;  i < s;  i++)
	for (j = 0;  j < option_data->n[i];  j++)
	  {
	    read_afni_data (option_data, option_data->xname[i][j],
			    piece_len, fim_offset, xfimar[m]);
	    m++;
	  }


      /*----- loop over voxels in this piece -----*/
      for (ivox = 0;  ivox < piece_len;  ivox++)
	{
	  nvox += 1;

	  m = 0;
	  for (i = 0;  i < s;  i++)
	    for (j = 0;  j < option_data->n[i];  j++)
	      {
		xarray[i][j] = xfimar[m][ivox];
		m++;
	      }


	  /*----- calculate results for this voxel -----*/
	  if (nvox == option_data->nvoxel)
	    process_voxel (nvox, s, n, xarray, &b, &k);
	  else
	    process_voxel (-1, s, n, xarray, &b, &k);
    

	  /*----- save results for this voxel -----*/
	  best[ivox+fim_offset] = b;
	  kstat[ivox+fim_offset] = k;
  
	} 
	  
    }  /* loop over pieces */


  /*----- deallocate memory -----*/
  free (n);   n = NULL;
  
  for (i = 0;  i < s;  i++)
    {
      free (xarray[i]);   xarray[i] = NULL;
    }
  free (xarray);   xarray = NULL;

  for (i = 0;  i < num_datasets;  i++)
    {
      free (xfimar[i]);   xfimar[i] = NULL;
    }
  free (xfimar);   xfimar = NULL;
}


/*---------------------------------------------------------------------------*/
/*
  Generate the requested output.
*/

void output_results 
(
  int argc,                         /* number of input arguments */
  char ** argv,                     /* array of input arguments */ 
  NP_options * option_data,         /* user input options */
  float * best,                     /* index of best treatment */
  float * kstat                     /* Kruskal-Wallis statistic */
)

{

  /*----- write out afni fict data file -----*/
  write_afni_fict (argc, argv, option_data, option_data->outfile, 
		   best, kstat, option_data->s - 1);

}



/*---------------------------------------------------------------------------*/
/*
   Routine to release memory and remove any remaining temporary data files.
*/

void terminate 
(
  NP_options ** option_data,   /* user input options */
  float ** best,               /* index of best treatment */
  float ** kstat               /* Kruskal-Wallis statistics */
)

{
   int i, j;                       /* dataset indices */


   /*----- deallocate memory -----*/
   for (i = 0;  i < (*option_data)->s;  i++)
     for (j = 0;  j < (*option_data)->n[i];  j++)
       {
	 free ((*option_data)->xname[i][j]);
	 (*option_data)->xname[i][j] = NULL;
       }
   for (i = 0;  i < (*option_data)->s;  i++)
     {
       free ((*option_data)->xname[i]);
       (*option_data)->xname[i] = NULL;
     }
   free ((*option_data)->xname);
   (*option_data)->xname = NULL;

   if ((*option_data)->outfile != NULL)
   {
      free ((*option_data)-> outfile);
      (*option_data)->outfile = NULL;
   }

   free (*option_data);   *option_data = NULL;

   free (*best);          *best = NULL;

   free (*kstat);         *kstat = NULL;
}


/*---------------------------------------------------------------------------*/
/*
   Perform nonparametric Kruskal-Wallis test for comparison of multiple
   treatments.
*/
 
int main (int argc, char ** argv)
{
  NP_options * option_data = NULL;    /* user input options */
  float * best;                       /* index of best treatment */
  float * kstat;                      /* Kruskal-Wallis statistic */

  
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR); 
  printf ("Date:    %s \n", PROGRAM_DATE);
  printf ("\n");


  /*----- program initialization -----*/
  initialize (argc, argv, &option_data, &best, &kstat);
  
  /*----- calculate nonparameteric Kruskal-Wallis statistics -----*/
  calculate_results (option_data, best, kstat);
  
  /*----- generate requested output -----*/
  output_results (argc, argv, option_data, best, kstat);

  /*----- terminate program -----*/
  terminate (&option_data, &best, &kstat);

}

















