/*
  This program performs the nonparametric Wilcoxon-Mann-Whitney two-sample 
  test for determining whether the observations come from the same population.
  The output consists of an AFNI 'fizt' dataset; the first sub-brick contains
  an estimate of the treatment effect, the second sub-brick contains the
  normalized Wilcoxon rank-sum statistic.

  File:    3dMannWhitney.c
  Author:  B. Douglas Ward
  Date:    23 July 1997

*/

/*---------------------------------------------------------------------------*/
/*
  This software is Copyright 1997 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application. The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.

*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dMannWhitney"   /* name of this program */
#define LAST_MOD_DATE "23 July 1997"   /* date of last program modification */

#define MAX_OBSERVATIONS 100     /* max. number of observations per cell */
#define MAX_NAME_LENGTH 80       /* max. strength length for file names */ 
#define MEGA  1048576            /* one megabyte */


#include <stdio.h>
#include <math.h>
#include "editvol.h"


typedef struct NP_options
{ 
  int   datum;                  /* data type for "intensity" data subbrick */
  char  session[MAX_NAME_LENGTH];     /* name of output directory */

  
  int   nvoxel;                 /* number of voxel for special output */

  int   m;                      /* number of X observations */
  int   n;                      /* number of Y observations */

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
   Routine to display 3dMannWhitney help menu.
*/

void display_help_menu()
{
   printf 
      (
       "This program performs nonparametric Mann-Whitney two-sample test. \n\n"
       "Usage: \n"
       "3dMannWhitney \n"
       "-dset 1 filename               data set for X observations          \n"
       " . . .                           . . .                              \n"
       "-dset 1 filename               data set for X observations          \n"
       "-dset 2 filename               data set for Y observations          \n"
       " . . .                           . . .                              \n"
       "-dset 2 filename               data set for Y observations          \n"
       "                                                                    \n"
       "[-workmem mega]                number of megabytes of RAM to use    \n"
       "                                 for statistical workspace          \n"
       "[-voxel num]                   screen output for voxel # num        \n"
       "-out prefixname                estimated population delta and       \n"
       "                                 Wilcoxon-Mann-Whitney statistics   \n"
       "                                 written to file prefixname         \n"

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
  
  option_data->m = 0;
  option_data->n = 0;

  option_data->workmem = 12;
 
  /*----- allocate memory for storing data file names -----*/
  option_data->xname = (char ***) malloc (sizeof(char **) * 2);
  for (i = 0;  i < 2;  i++)
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
   Routine to get user specified Mann-Whitney options.
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
	  sprintf(buf,"-datum of type '%s' is not supported in 3dMannWhitney.",
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


      /*-----   -dset level filename   -----*/
      if (strncmp(argv[nopt], "-dset", 5) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  NP_error ("need 2 arguments after -dset ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > 2))
	    NP_error ("illegal argument after -dset ");
	  
	  if (ival == 1)
	    {
	      option_data->m += 1;
	      nijk = option_data->m;
	    }
	  else
	    {
	      option_data->n += 1;
	      nijk = option_data->n;
	    }
	  if (nijk > MAX_OBSERVATIONS)
	    NP_error ("too many data files");
	  
	  /*--- check whether input files exist ---*/
	  nopt++;
	  dset = THD_open_one_dataset( argv[nopt] ) ;
	  if( ! ISVALID_3DIM_DATASET(dset) )
	    {
	     sprintf(message,"Unable to open dataset file %s\n", argv[nopt]);
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
  
  if (option_data->m < 1)
    NP_error ("too few data sets for X-observations ");
  
  if (option_data->n < 1) 
    NP_error ("too few data sets for Y-observations ");

  if (option_data->nvoxel > option_data->nxyz)
    NP_error ("argument of -voxel is too large");
 
}


/*---------------------------------------------------------------------------*/
/*
  Routine to perform all Mann-Whitney initialization.
*/

void initialize 
(
  int argc,                    /* number of input arguments */
  char ** argv,                /* array of input arguments */ 
  NP_options ** option_data,   /* user input options */
  float ** delta,              /* estimated shift parameter */
  float **zvar                 /* normalized Mann-Whitney statistic */
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
  *delta = (float *) malloc(sizeof(float) * (*option_data)->nxyz);
  if (*delta == NULL)
    NP_error ("memory allocation error");
  *zvar = (float *) malloc(sizeof(float) * (*option_data)->nxyz);
  if (*zvar == NULL)
    NP_error ("memory allocation error");
 
  
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the normalized Wilcoxon-Mann-Whitney statistic.
*/

void calc_stat 
(
  int nvox,                          /* flag for voxel output */
  int m,                             /* number of control subjects */
  int n,                             /* number of treatment subjects */
  float * xarray,                    /* array of control data (x data) */
  float * yarray,                    /* array of treatment data (y data) */
  float * zvar                       /* normalized Mann-Whitney statistic */
)

{
  const float EPSILON = 1.0e-10;      /* minimum variance limit */
  int i, j;                   /* array indices */
  node * head = NULL;         /* points to head of list */        
  node * ptr = NULL;          /* points to current position in list */
  int NN;                     /* total number of sample points */
  float wy;                   /* rank sum statistic */
  float ewy;                  /* expected value of wy */
  float varwy;                /* variance of wy */
  int d;                      /* count of number of ties */ 
  float corr;                 /* correction to variance to account for ties */
  float rank;                 /* rank of data point */


  /*----- enter and sort x-array -----*/
  for (i = 0;  i < m;  i++)
    node_addvalue (&head, xarray[i]);


  /*----- enter and sort y-array -----*/
  for (j = 0;  j < n;  j++)
    node_addvalue (&head, yarray[j]); 


  /*----- if display voxel, write the ranks of the input data -----*/
  if (nvox > 0)
    {
      printf ("\n");
      printf ("X ranks: \n");
      for (i = 0;  i < m;  i++)
	{
	  rank = node_get_rank (head, xarray[i]);
	  printf (" %6.1f", rank);
	  if (((i+1) % 10 == 0) && (i < m-1))
	    printf ("\n");
	}

      printf ("\n\n");
      printf ("Y ranks: \n");
      for (i = 0;  i < n;  i++)
	{
	  rank = node_get_rank (head, yarray[i]);
	  printf (" %6.1f", rank);
	  if (((i+1) % 10 == 0) && (i < n-1))
	    printf ("\n");
	}

      printf ("\n\n");
    }


  /*----- calculate rank sum of y-values -----*/
  wy = 0.0;
  for (j = 0;  j < n;  j++)
    wy += node_get_rank (head, yarray[j]);
  if (nvox > 0)  printf ("Wy = %f \n", wy);


  /*----- calculate expected value of rank sum statistic -----*/
  NN = m + n;
  ewy = 0.5 * n * (NN+1);
  if (nvox > 0)  printf ("E(Wy) = %f \n", ewy);

  /*----- calculate variance of rank sum statisitc -----*/
  varwy = m * n * (NN+1) / 12.0;

  /*----- correction to variance due to ties in data -----*/
  corr = 0.0;
  ptr = head;
  while (ptr != NULL)
    {
      d = ptr->d;
      corr += d*d*d - d;
      ptr = ptr->next;
    }
  corr *= m * n / (12.0 * NN * (NN - 1));
  varwy -= corr;
  if (nvox > 0)  printf ("Var(Wy) = %f \n", varwy);


  /*----- calculate normalized Wilcoxon-Mann-Whitney rank-sum statistic -----*/
  if (varwy < EPSILON)
    *zvar = 0.0;
  else
    *zvar = (wy - ewy) / sqrt(varwy);
  if (nvox > 0)  printf ("Z = %f \n", *zvar);


  /*----- deallocate memory -----*/
  list_delete (&head);
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the estimated shift parameter, using the median of the Walsh
  differences.
*/

void calc_shift 
(
  int nvox,                          /* flag for voxel output */
  int m,                             /* number of control subjects */
  int n,                             /* number of treatment subjects */
  float * xarray,                    /* array of control data (x data) */
  float * yarray,                    /* array of treatment data (y data) */
  float * delta_hat                  /* median of differences */
)

{
  int i, j;                          /* array indices */
  int mn;                            /* number of Walsh differences */
  node * head = NULL;                /* points to head of list */
  int count;                         /* list print counter */


  /*----- enter and sort array of differences -----*/
  for (i = 0;  i < m;  i++)
    for (j = 0;  j < n;  j++)
      node_addvalue (&head, yarray[j] - xarray[i]); 


  /*----- if output requested, write the array of ordered differences -----*/
  if (nvox > 0)
    {
      printf ("\n");
      printf ("Ordered differences: \n");
      count = 0;
      list_print (head, &count);
      printf ("\n");
    }
  

  /*----- find median of differences -----*/
  mn = m * n;
  *delta_hat = node_get_median (head, mn);

  if (nvox > 0)
    {
      printf ("\n");
      printf ("Delta hat = %f \n\n", *delta_hat);
    }


  /*----- deallocate memory -----*/
  list_delete (&head);
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the Mann-Whitney statistic and shift parameter for a single voxel.
*/

void process_voxel
(
  int nvox,                          /* flag for voxel output */
  int m,                             /* number of control subjects */
  int n,                             /* number of treatment subjects */
  float * xarray,                    /* array of control data (x data) */
  float * yarray,                    /* array of treatment data (y data) */
  float * delta_hat,                 /* estimated shift parameter */
  float * zvar                       /* normalized Mann-Whitney statistic */
)

{
  int i;                             /* array index */


  /*----- check for voxel output  -----*/
  if (nvox > 0)
    {
      printf ("\nResults for voxel #%d : \n", nvox);

      printf ("\n");
      printf ("X data:  \n");
      for (i = 0;  i < m;  i++)
	{
	  printf (" %6.1f", xarray[i]);
	  if (((i+1) % 10 == 0) && (i < m-1))
	    printf ("\n");
	}

      printf ("\n\n");
      printf ("Y data:  \n");
      for (i = 0;  i < n;  i++)
	{
	  printf (" %6.1f", yarray[i]);
	  if (((i+1) % 10 == 0) && (i < n-1))
	    printf ("\n");
	}
      printf ("\n");
    }


  /*----- calculate normalized Mann-Whitney statistic -----*/
  calc_stat (nvox, m, n, xarray, yarray, zvar);


  /*----- estimate shift parameter -----*/
  calc_shift (nvox, m, n, xarray, yarray, delta_hat);

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the Wilcoxon-Mann-Whitney rank-sum statistics for all voxels  
  (by breaking the datasets into sub-volumes, if necessary).
*/

void calculate_results 
(
  NP_options * option_data,    /* user input options */
  float * delta,               /* estimated shift parameter */
  float * zvar                 /* normalized Mann-Whitney statistic */
)

{
  int i;                       /* dataset index */
  int m;                       /* control sample size */  
  int n;                       /* treatment sample size */
  int nxyz;                    /* number of voxels per dataset */
  int num_datasets;            /* total number of datasets */
  int piece_size;              /* number of voxels in dataset sub-volume */
  int num_pieces;              /* dataset is divided into this many pieces */
  int piece;                   /* piece index */
  int piece_len;               /* number of voxels in current sub-volume */
  int fim_offset;              /* array offset to current sub-volume */
  int ivox;                    /* index to voxels in current sub-volume */
  int nvox;                    /* index of voxel within entire volume */
  float delta_hat;             /* estimate of shift parameter */
  float z;                     /* normalized Mann-Whitney statistic */
  float ** xfimar;             /* array of pieces of X-datasets */
  float ** yfimar;             /* array of pieces of Y-datasets */
  float * xarray;              /* array of control data (X-data) */
  float * yarray;              /* array of treatment data (Y-data) */


  /*----- initialize local variables -----*/
  m = option_data->m;
  n = option_data->n;
  nxyz = option_data->nxyz;
  num_datasets = m + n;


  /*----- break problem into smaller pieces -----*/
  piece_size = option_data->workmem * MEGA / (num_datasets * sizeof(float));
  if (piece_size > nxyz)  piece_size = nxyz;
  num_pieces = (nxyz + piece_size - 1) / piece_size;
  printf ("num_pieces = %d    piece_size = %d \n", num_pieces, piece_size);    
  
  /*----- allocate memory space -----*/
  xarray = (float *) malloc (sizeof(float) * m);     MTEST(xarray);
  yarray = (float *) malloc (sizeof(float) * n);     MTEST(yarray);
  xfimar = (float **) malloc (sizeof(float *) * m);  MTEST(xfimar);
  yfimar = (float **) malloc (sizeof(float *) * n);  MTEST(yfimar);
  for (i = 0;  i < m;  i++)
    {
      xfimar[i] = (float *) malloc(sizeof(float) * piece_size);  
      MTEST(xfimar[i]);
    }
  for (i = 0;  i < n;  i++)
    {
      yfimar[i] = (float *) malloc(sizeof(float) * piece_size);  
      MTEST(yfimar[i]);
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

      /*----- read in the X-data -----*/
      for (i = 0;  i < m;  i++)
	read_afni_data (option_data, option_data->xname[0][i],
			piece_len, fim_offset, xfimar[i]);

      /*----- read in the Y-data -----*/
      for (i = 0;  i < n;  i++)
	read_afni_data (option_data, option_data->xname[1][i],
			piece_len, fim_offset, yfimar[i]);


      /*----- loop over voxels in this piece -----*/
      for (ivox = 0;  ivox < piece_len;  ivox++)
	{
	  nvox += 1;

	  for (i = 0;  i < m;  i++)
	    xarray[i] = xfimar[i][ivox];
	  for (i = 0;  i < n;  i++)
	    yarray[i] = yfimar[i][ivox];


	  /*----- calculate results for this voxel -----*/
	  if (nvox == option_data->nvoxel)
	    process_voxel (nvox, m, n, xarray, yarray, &delta_hat, &z);
	  else
	    process_voxel (-1, m, n, xarray, yarray, &delta_hat, &z);
    

	  /*----- save results for this voxel -----*/
	  delta[ivox+fim_offset] = delta_hat;
	  zvar[ivox+fim_offset] = z;
  
	} 
	  
    } /* loop over pieces */


  /*----- deallocate memory -----*/
  free (xarray);   xarray = NULL;
  free (yarray);   yarray = NULL;

  for (i = 0;  i < m;  i++)
    {
      free (xfimar[i]);   xfimar[i] = NULL;
    }
  free (xfimar);   xfimar = NULL;

  for (i = 0;  i < n;  i++)
    {
      free (yfimar[i]);   yfimar[i] = NULL;
    }
  free (yfimar);   yfimar = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Generate the requested output.
*/

void output_results 
(
  NP_options * option_data,    /* user input options */
  float * delta,               /* estimated shift parameter */
  float * zvar                 /* normalized Mann-Whitney rank-sum statistic */
)

{

  /*----- write out afni fizt data file -----*/
  write_afni_fizt (option_data, option_data->outfile, 
		   delta, zvar);

}



/*---------------------------------------------------------------------------*/
/*
   Routine to release memory and remove any remaining temporary data files.
*/

void terminate 
(
  NP_options ** option_data,   /* user input options */
  float ** delta,              /* estimated shift parameter */
  float ** zvar                /* normalized Mann-Whitney rank-sum statistic */
)

{
   int i, j;                       /* dataset indices */


   /*----- deallocate memory -----*/
   for (j = 0; j < (*option_data)->m; j++)
     {
       free ((*option_data)->xname[0][j]);
       (*option_data)->xname[0][j] = NULL;
     }
   for (j = 0; j < (*option_data)->n; j++)
     {
       free ((*option_data)->xname[1][j]);
       (*option_data)->xname[1][j] = NULL;
     }
   for (i = 0;  i < 2;  i++)
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

   free (*delta);         *delta = NULL;

   free (*zvar);          *zvar = NULL;
}


/*---------------------------------------------------------------------------*/
/*
   Perform nonparametric Wilcoxon-Mann-Whitney rank-sum two sample test.
*/
 
void main (int argc, char ** argv)
{
  NP_options * option_data = NULL;   /* user input options */
  float * delta;                     /* estimated shift parameter */
  float * zvar;                      /* normalized Mann-Whitney statistic */
 
  printf ("\n\nProgram %s \n\n", PROGRAM_NAME);
  printf ("Last revision: %s \n", LAST_MOD_DATE);


  /*----- program initialization -----*/
  initialize (argc, argv, &option_data, &delta, &zvar);
  
  /*----- calculate nonparameteric Mann-Whitney statistics -----*/
  calculate_results (option_data, delta, zvar);
  
  /*----- generate requested output -----*/
  output_results (option_data, delta, zvar);

  /*----- terminate program -----*/
  terminate (&option_data, &delta, &zvar);

}

















