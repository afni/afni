/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
   This program calculates the single-factor analysis of variance (ANOVA)
   for 3 dimensional AFNI data sets. 

   File:    3dANOVA.c
   Author:  B. D. Ward
   Date:    09 December 1996

   Mod:     Incorporated include file 3dANOVA.h.
   Date:    15 January 1997

   Mod:     Added option to check for required disk space.
   Date:    23 January 1997

   Mod:     Added protection against divide by zero.
   Date:    13 November 1997

   Mod:     Extensive changes required to implement the 'bucket' dataset.
   Date:    30 December 1997

   Mod:     Separated 3dANOVA.h and 3dANOVA.lib files.
   Date:    5 January 1998

   Mod:     Continuation of 'bucket' dataset changes.
   Date:    9 January 1998

   Mod:     Added software for statistical tests of individual cell means.
   Date:    27 October 1998

   Mod:     Added changes for incorporating History notes.
   Date:    09 September 1999

   Mod:     Replaced dataset input code with calls to THD_open_dataset,
            to allow operator selection of individual sub-bricks for input.
   Date:    03 December 1999

   Mod:     Added call to AFNI_logger.
   Date:    15 August 2001

   Mod:     Modified routine write_afni_data of 3dANOVA.lib so that all output
            subbricks will now have the scaled short integer format.
   Date:    14 March 2002

   Mod:     Set MAX_NAME_LENGTH equal to THD_MAX_NAME.
   Date:    02 December 2002

   Mod:     Setup to use .1D dataset filenames on output if these are input.
   Date:    14 March 2003 - RWCox
*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME    "3dANOVA"                    /* name of this program */
#define PROGRAM_AUTHOR  "B. Douglas Ward"                  /* program author */
#define PROGRAM_INITIAL "09 Dec 1996"     /* date of initial program release */
#define PROGRAM_LATEST  "14 Mar 2003"     /* date of latest program revision */

/*---------------------------------------------------------------------------*/

#define SUFFIX ".3danova"                 /* suffix for temporary data files */

#include "3dANOVA.h"
#include "3dANOVA.lib"


/*---------------------------------------------------------------------------*/
/*
   Routine to display 3dANOVA help menu.
*/

void display_help_menu()
{
  printf 
    (
     "This program performs single factor ANOVA on 3D data sets \n\n"
     "Usage: \n"
     "3dANOVA \n"
     "-levels r                      r = number of factor levels            \n"
     "-dset 1 filename               data set for factor level 1            \n"
     " . . .                           . . .                                \n"
     "-dset 1 filename               data set for factor level 1            \n"
     " . . .                           . . .                                \n"
     "-dset r filename               data set for factor level r            \n"
     " . . .                           . . .                                \n"
     "-dset r filename               data set for factor level r            \n"
     "                                                                      \n"
     "[-voxel num]                   screen output for voxel # num          \n"
     "[-diskspace]                   print out disk space required for      \n"
     "                                  program execution                   \n"
     "                                                                      \n"
     "                                                                      \n"
     "The following commands generate individual AFNI 2 sub-brick datasets: \n"
     "  (In each case, output is written to the file with the specified     \n"
     "   prefix file name.)                                                 \n"
     "                                                                      \n"
     "[-ftr prefix]                  F-statistic for treatment effect       \n"
     "[-mean i prefix]               estimate of factor level i mean        \n"
     "[-diff i j prefix]             difference between factor levels       \n"
     "[-contr c1...cr prefix]        contrast in factor levels              \n"
     "                                                                      \n"
     "                                                                      \n"
     "The following command generates one AFNI 'bucket' type dataset:       \n"
     "                                                                      \n"
     "[-bucket prefix]         create one AFNI 'bucket' dataset whose       \n"
     "                           sub-bricks are obtained by concatenating   \n"
     "                           the above output files; the output 'bucket'\n"
     "                           is written to file with prefix file name   \n"
     "\n");

  printf
    (
     "\n"
     "N.B.: For this program, the user must specify 1 and only 1 sub-brick  \n"
     "      with each -dset command. That is, if an input dataset contains  \n"
     "      more than 1 sub-brick, a sub-brick selector must be used, e.g.: \n"
     "      -dset 2 'fred+orig[3]'                                          \n"
     );
	  
  
  printf("\n" MASTER_SHORTHELP_STRING ) ;

  exit(0);
}


/*---------------------------------------------------------------------------*/
/*
   Routine to get user specified anova options.
*/

void get_options (int argc, char ** argv, anova_options * option_data)
{
  int nopt = 1;                  /* input option argument counter */
  int ival;                      /* integer input */
  int i, j, k;                   /* factor level counter */                   
  int nijk;                      /* number of data files in cell i */     
  float fval;                    /* float input */
  THD_3dim_dataset * dset=NULL;             /* test whether data set exists */
  char message[MAX_NAME_LENGTH];            /* error message */
  int nbricks;                   /* number of sub-bricks selected */ 


  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();

  /*----- add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv); 

  /*----- initialize the input options -----*/
  initialize_options (option_data);

  
  /*----- main loop over input options -----*/
  while (nopt < argc)
    {
      
      /*----- allocate memory for storing data file names -----*/
      if ((option_data->xname == NULL) && (option_data->a > 0))
	{
	  option_data->xname = 
	    (char *****) malloc (sizeof(char ****) * option_data->a);
	  for (i = 0;  i < option_data->a;  i++)
	    {
	      option_data->xname[i] =
		(char ****) malloc (sizeof(char ***) * 1);
	      for (j = 0;  j < 1;  j++)
		{
		  option_data->xname[i][j] =
		    (char ***) malloc (sizeof(char **) * 1);
		  for (k = 0;  k < 1;  k++)
		    {
		      option_data->xname[i][j][k] =
			(char **) malloc (sizeof(char *) * MAX_OBSERVATIONS);
		    }
		}
	    }
	}


      /*-----   -diskspace   -----*/
      if( strncmp(argv[nopt],"-diskspace",5) == 0 )
	{
	  option_data->diskspace = 1;
	  nopt++ ; continue ;  /* go to next arg */
	}

      
      /*-----   -datum type   -----*/
      if( strncmp(argv[nopt],"-datum",6) == 0 ){
	if( ++nopt >= argc ) ANOVA_error("need an argument after -datum!") ;
	
	if( strcmp(argv[nopt],"short") == 0 ){
	  option_data->datum = MRI_short ;
	} else if( strcmp(argv[nopt],"float") == 0 ){
	  option_data->datum = MRI_float ;
	} else {
	  char buf[256] ;
	  sprintf(buf,"-datum of type '%s' is not supported in 3dANOVA!",
		  argv[nopt] ) ;
	  ANOVA_error(buf) ;
	}
	nopt++ ; continue ;  /* go to next arg */
      }
      
      
      /*-----   -session dirname    -----*/
      if( strncmp(argv[nopt],"-session",6) == 0 ){
	nopt++ ;
	if( nopt >= argc ) ANOVA_error("need argument after -session!") ;
	strcpy(option_data->session , argv[nopt++]) ;
	continue ;
      }
      
      
      /*-----   -voxel num  -----*/
      if (strncmp(argv[nopt], "-voxel", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -voxel ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    ANOVA_error ("illegal argument after -voxel ");
	  option_data->nvoxel = ival;
	  nopt++;
	  continue;
	}
      
      
      /*-----   -levels r  -----*/
      if (strncmp(argv[nopt], "-levels", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -levels ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > MAX_LEVELS))
	    ANOVA_error ("illegal argument after -levels ");
	  option_data->a = ival;
	  nopt++;
	  continue;
	}
      
      
      /*-----   -dset level filename   -----*/
      if (strncmp(argv[nopt], "-dset", 5) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  ANOVA_error ("need 2 arguments after -dset ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->a))
	    ANOVA_error ("illegal argument after -dset ");
	  
	  option_data->na[ival-1] += 1;
	  nijk = option_data->na[ival-1];
	  if (nijk > MAX_OBSERVATIONS)
	    ANOVA_error ("too many data files");
	  
	  /*--- check whether input files exist ---*/
	  nopt++;
	  dset = THD_open_dataset( argv[nopt] ) ;
	  if( ! ISVALID_3DIM_DATASET(dset) )
	    {
	     sprintf(message,"Unable to open dataset file %s\n", 
		     argv[nopt]);
	     ANOVA_error (message);
	    }

	  /*--- check number of selected sub-bricks ---*/
	  if (DSET_NVALS(dset) != 1)
	    {
	     sprintf(message,"Must specify exactly 1 sub-brick for file %s\n",
 		     argv[nopt]);
	     ANOVA_error (message);
	    }

	  THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
	  
	  option_data->xname[ival-1][0][0][nijk-1] 
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->xname[ival-1][0][0][nijk-1], 
		  argv[nopt]);

	  nopt++;
	  continue;
	}
      
      
      /*-----   -ftr filename   -----*/
      if (strncmp(argv[nopt], "-ftr", 4) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -ftr ");
	  option_data->nftr = 1;
	  option_data->ftrname = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->ftrname, argv[nopt]);
	  nopt++;
	  continue;
	}
      
      
      /*-----   -mean level filename   -----*/
      if (strncmp(argv[nopt], "-mean", 5) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  ANOVA_error ("need 2 arguments after -mean ");
	  
	  option_data->num_ameans++;
	  if (option_data->num_ameans > MAX_MEANS)
	    ANOVA_error ("too many factor level mean estimates");
	  
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->a))
	    ANOVA_error ("illegal argument after -mean ");
	  option_data->ameans[option_data->num_ameans-1] = ival - 1;
	  nopt++;
	  
	  option_data->amname[option_data->num_ameans-1] 
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->amname[option_data->num_ameans-1], argv[nopt]);
	  nopt++;
	  continue;
	}
      
      
      /*-----   -diff level1 level2 filename   -----*/
      if (strncmp(argv[nopt], "-diff", 5) == 0)
	{
	  nopt++;
	  if (nopt+2 >= argc)  ANOVA_error ("need 3 arguments after -diff ");
	  
	  option_data->num_adiffs++;
	  if (option_data->num_adiffs > MAX_DIFFS)
	    ANOVA_error ("too many factor level differences");
	  
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->a))
	    ANOVA_error ("illegal argument after -diff ");
	  option_data->adiffs[option_data->num_adiffs-1][0] = ival - 1;
	  nopt++;
	  
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->a))
	    ANOVA_error ("illegal argument after -diff ");
	  option_data->adiffs[option_data->num_adiffs-1][1] = ival - 1;
	  nopt++;
	  
	  option_data->adname[option_data->num_adiffs-1] 
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->adname[option_data->num_adiffs-1], argv[nopt]);
	  nopt++;
	  continue;
	}
      
      
      /*-----   -contr c1 ... cr filename   -----*/
      if (strncmp(argv[nopt], "-contr", 6) == 0)
	{
	  nopt++;
	  if (nopt + option_data->a >= argc)  
            ANOVA_error ("need r+1 arguments after -contr ");
	  
	  option_data->num_acontr++;
	  if (option_data->num_acontr > MAX_CONTR)
	    ANOVA_error ("too many factor level contrasts");
	  
	  for (i = 0;  i < option_data->a;  i++)
	    {
	      sscanf (argv[nopt], "%f", &fval); 
	      option_data->acontr[option_data->num_acontr - 1][i] = fval ;
	      nopt++;
	    }
	  
	  option_data->acname[option_data->num_acontr-1] 
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->acname[option_data->num_acontr-1], argv[nopt]);
	  nopt++;
	  continue;
	}
      
      
      /*-----   -bucket filename   -----*/
      if (strncmp(argv[nopt], "-bucket", 4) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -bucket ");
	  option_data->bucket_filename = malloc (sizeof(char)*MAX_NAME_LENGTH);
	  strcpy (option_data->bucket_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      
      
      /*----- unknown command -----*/
      sprintf (message,"Unrecognized command line option: %s\n", argv[nopt]);
      ANOVA_error (message);
    }

}

/*---------------------------------------------------------------------------*/
/*
   Routine to check whether temporary files already exist.
*/

void check_temporary_files (anova_options * option_data)
{
   char filename[MAX_NAME_LENGTH];           /* temporary file name */ 

   int i;                                    /* file counter */

   for (i = 0;  i < option_data->a;  i++)
     {
       /*----- temporary file name -----*/
       sprintf (filename, "y.%d", i);

       /*-----  see if file already exists -----*/
       check_one_temporary_file (filename);
     }


   check_one_temporary_file ("ysum");
   check_one_temporary_file ("ss");
   check_one_temporary_file ("ssto");
   check_one_temporary_file ("sstr");
   check_one_temporary_file ("sse");
 
}


/*---------------------------------------------------------------------------*/
/*
   Routine to check whether output files already exist.
*/

void check_output_files (anova_options * option_data)
{
  int i;       /* index */

  if (option_data->nftr > 0)   
    check_one_output_file (option_data, option_data->ftrname);

  if (option_data->num_ameans > 0)
    for (i = 0;  i < option_data->num_ameans;  i++)
      check_one_output_file (option_data, option_data->amname[i]);

  if (option_data->num_adiffs > 0)
    for (i = 0;  i < option_data->num_adiffs;  i++)
      check_one_output_file (option_data, option_data->adname[i]);

  if (option_data->num_acontr > 0)
    for (i = 0;  i < option_data->num_acontr;  i++)
      check_one_output_file (option_data, option_data->acname[i]);

  if (option_data->bucket_filename != NULL)
    check_one_output_file (option_data, option_data->bucket_filename);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check for valid inputs.
*/

void check_for_valid_inputs (anova_options * option_data)
{
  int i;                               /* factor level index */
  char message[MAX_NAME_LENGTH];       /* error message */
  
  if (option_data->a < 2)
    ANOVA_error ("must specify number of factor levels (r>1) ");
  
  if (option_data->nt < option_data->a + 1) 
    ANOVA_error ("too few data sets for ANOVA");

  for (i = 0;  i < option_data->a;  i++)
    if (option_data->na[i] == 0)
      {
	sprintf (message, "level %d has too few data sets", i+1);
	ANOVA_error (message);
      }
}
/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the number of data files that have to be stored.
  Modified to account for 'bucket' dataset output.
*/

int required_data_files (anova_options * option_data)
{
  int r;                           /* number of factor levels */
  int nftr;                        /* number of F-treatment output files 
				      (0 or 1) */
  int nmeans;                      /* number of estimated mean output files */
  int ndiffs;                      /* number of difference output files */
  int ncontr;                      /* number of contrast output files */
  int nout;                        /* number of output files */
  int nmax;                        /* maximum number of disk files */


  /*----- initialize local variables -----*/
  r = option_data->a;
  nftr = option_data->nftr;
  nmeans = option_data->num_ameans;
  ndiffs = option_data->num_adiffs;
  ncontr = option_data->num_acontr;
  nout = nftr + nmeans + ndiffs + ncontr;

  nmax = r + 2 + nout;
  if (option_data->bucket_filename != NULL)
    nmax = max (nmax, 2*nout);
  
  return (nmax);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to perform all ANOVA initialization.
*/

void initialize (int argc,  char ** argv,  anova_options ** option_data)
{
  int i;                               /* factor level index */
  
  

  /*----- save command line for history notes -----*/
  commandline = tross_commandline( PROGRAM_NAME , argc,argv ) ;


  /*----- allocate memory space for input data -----*/   
  *option_data = (anova_options *) malloc(sizeof(anova_options));
  if (*option_data == NULL)
    ANOVA_error ("memory allocation error");
  
  /*----- get command line inputs -----*/
  get_options(argc, argv, *option_data);
  
  /*----- use first data set to get data set dimensions -----*/
  (*option_data)->first_dataset = (*option_data)->xname[0][0][0][0];
  get_dimensions (*option_data);
  printf ("Data set dimensions:  nx = %d  ny = %d  nz = %d  nxyz = %d \n",
	  (*option_data)->nx, (*option_data)->ny,
	  (*option_data)->nz, (*option_data)->nxyz);
  if ((*option_data)->nvoxel > (*option_data)->nxyz)
    ANOVA_error ("argument of -voxel is too large");
  
  /*----- count number of observations per treatment level -----*/
  (*option_data)->nt = 0;
  for (i = 0;  i < (*option_data)->a;  i++)
    (*option_data)->nt += (*option_data)->na[i];

  /*----- check for valid inputs -----*/
  check_for_valid_inputs (*option_data);
    
  /*----- check whether temporary files already exist -----*/
  check_temporary_files (*option_data);
  
  /*----- check whether output files already exist -----*/
  check_output_files (*option_data);
  
  /*----- check whether there is sufficient disk space -----*/
  if ((*option_data)->diskspace)  check_disk_space (*option_data);
}


/*---------------------------------------------------------------------------*/
/*
   Routine to sum over the observations within one treatment level.
   Output is stored (temporarily) in data file y"i".3danova, where
   "i" = treatment level. 
*/

void calculate_y (anova_options * option_data)
{
   float * x = NULL;                /* pointer to original input data */
   float * y = NULL;                /* pointer to sum within treatment data */
   int i;                           /* factor level index */
   int j;                           /* observation number index */
   int r;                           /* number of factor levels (treatments) */
   int ixyz, nxyz;                  /* voxel counters */
   int nvoxel;                      /* output voxel # */
   char filename[MAX_NAME_LENGTH];  /* name of file for sum within treatment */


   /*----- initialize local variables -----*/
   r = option_data->a;
   nxyz = option_data->nxyz;
   nvoxel = option_data->nvoxel;
 
   /*----- allocate memory space for calculations -----*/
   x = (float *) malloc(sizeof(float)*nxyz);
   y = (float *) malloc(sizeof(float)*nxyz);
   if ((x == NULL) || (y == NULL))
      ANOVA_error ("unable to allocate sufficient memory");
  

   /*----- loop over treatment levels -----*/
   for (i = 0;  i < r;  i++)
   {
     /*----- sum observations within a treatment level -----*/
      volume_zero (y, nxyz);
      for (j = 0;  j < option_data->na[i];  j++)
      {
	 read_afni_data (option_data, 
			 option_data->xname[i][0][0][j], x);
	 if (nvoxel > 0)
	    printf ("y[%d][%d] = %f \n", i+1, j+1, x[nvoxel-1]);
         for (ixyz = 0;  ixyz < nxyz;  ixyz++)
            y[ixyz] += x[ixyz];
      }
 
      /*----- save the sum for this treatment level -----*/
      if (nvoxel > 0)
  	 printf ("y[%d]. = %f \n", i+1, y[nvoxel-1]); 
      sprintf (filename, "y.%d", i);
      volume_write (filename, y, nxyz);
   }

   /*----- release memory -----*/
   free (y);   y = NULL;
   free (x);   x = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to  sum over all observations at all treatment levels.
   The sum is stored (temporarily) in disk file ysum.3danova.
*/

void calculate_ysum (anova_options * option_data)
{
   float * y = NULL;                 /* pointer to sum within treatment data */
   float * ysum = NULL;              /* pointer to overall sum data */
   int i;                            /* factor level index */
   int ixyz, nxyz;                   /* voxel counters */
   int nvoxel;                       /* output voxel # */
   char filename[MAX_NAME_LENGTH];   /* sum within treatment input file name */


   /*----- initialize local variables -----*/
   nxyz = option_data->nxyz;
   nvoxel = option_data->nvoxel;
  
   /*----- allocate memory space for calculations -----*/
   y = (float *) malloc(sizeof(float)*nxyz);
   ysum = (float *) malloc(sizeof(float)*nxyz);
   if ((y == NULL) || (ysum == NULL))
      ANOVA_error ("unable to allocate sufficient memory");
  

   /*----- sum over all observations -----*/
   volume_zero (ysum, nxyz);
   for (i = 0;  i < option_data->a;  i++)
   { 
      sprintf (filename, "y.%d", i);
      volume_read (filename, y, nxyz);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
         ysum[ixyz] += y[ixyz];
   }

   if (nvoxel > 0)
      printf ("y.. = %f \n", ysum[nvoxel-1]);
   volume_write ("ysum",ysum, nxyz);

   /*----- release memory -----*/
   free (ysum);  ysum = NULL;
   free (y);     y = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to calculate the sum of squares of all observations (SS).
   The output is stored (temporarily) in disk file ss.3danova.
*/

void calculate_ss (anova_options * option_data)
{
   float * x = NULL;                   /* pointer to original input data */
   float * ss = NULL;                  /* pointer to sum of squares data */
   int i;                              /* factor level index */
   int j;                              /* observation data index */
   int r;                              /* number of factor levels */
   int ixyz, nxyz;                     /* voxel counters */
   int nvoxel;                         /* output voxel # */
   float xval;                         /* float input data */

 
   /*----- initialize local variables -----*/
   r = option_data->a;
   nxyz = option_data->nxyz;
   nvoxel = option_data->nvoxel;
  
   /*----- allocate memory space for calculations -----*/
   x = (float *) malloc(sizeof(float)*nxyz);
   ss = (float *) malloc(sizeof(float)*nxyz);
   if ((x == NULL) || (ss == NULL))
      ANOVA_error ("unable to allocate sufficient memory");

   /*----- sum the squares of all observations -----*/
   volume_zero (ss, nxyz);
   for (i = 0;  i < r;  i++)
      for (j = 0;  j < option_data->na[i];  j++)
      {  
	 read_afni_data (option_data, 
			 option_data->xname[i][0][0][j], x);
         for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	 {
	    xval = x[ixyz];
	    ss[ixyz] += xval * xval;
	 }
      }

   if (nvoxel > 0)
      printf ("SS = %f \n", ss[nvoxel-1]);
   volume_write ("ss", ss, nxyz);

   /*----- release memory -----*/
   free (ss);  ss = NULL;
   free (x);   x = NULL;

}

/*---------------------------------------------------------------------------*/
/*
   Routine to calculate total (corrected for the mean) sum of squares (SSTO).
   The output is stored (temporarily) in disk file ssto.3danova.
*/

void calculate_ssto (anova_options * option_data)
{
   float * ysum = NULL;                /* ptr to sum over all observations */
   float * ssto = NULL;                /* pointer to ssto data */
   int ixyz, nxyz;                     /* voxel counters */
   int nvoxel;                         /* output voxel # */
   int nt;                             /* total number of observations */
   float yval;                         /* sum over all observations */


   /*----- initialize local variables -----*/
   nt = option_data->nt;
   nxyz = option_data->nxyz;
   nvoxel = option_data->nvoxel;
 
   /*----- allocate memory space for calculations -----*/
   ysum = (float *) malloc(sizeof(float)*nxyz);
   ssto = (float *) malloc(sizeof(float)*nxyz);
   if ((ysum == NULL) || (ssto == NULL))
      ANOVA_error ("unable to allocate sufficient memory");
 
   /*-----  SSTO = SS - ((YSUM)^2)/nt  -----*/
   volume_read ("ss", ssto, nxyz);
   volume_read ("ysum", ysum, nxyz); 
   for (ixyz = 0;  ixyz < nxyz;  ixyz++)
   {
      yval = ysum[ixyz];
      ssto[ixyz] -= yval * yval / nt;
   }

   /*----- ss data file is no longer needed -----*/
   volume_delete ("ss");

   /*----- save total (corrected) sum of squares -----*/ 
   if (nvoxel > 0)
      printf ("SSTO = %f \n", ssto[nvoxel-1]);
   volume_write ("ssto", ssto, nxyz);

   /*----- release memory -----*/
   free (ssto);   ssto = NULL;
   free (ysum);   ysum = NULL;

}

/*---------------------------------------------------------------------------*/
/*
   Routine to calculate the sum of squares due to the treatment (SSTR).
   The output is stored (temporarily) in file sstr.3danova.
*/

void calculate_sstr (anova_options * option_data)
{
   float * y = NULL;                   /* input data pointer */
   float * sstr = NULL;                /* sstr data pointer */
   int i;                              /* factor level index */
   int ixyz, nxyz;                     /* voxel counters */
   int nvoxel;                         /* output voxel # */
   int ni;                             /* number of observations at level i */
   int nt;                             /* total number of observations */
   float yval;                         /* temporary float value */
   char filename[MAX_NAME_LENGTH];     /* input data file name */


   /*----- assign local variables -----*/
   nt = option_data->nt;
   nxyz = option_data->nxyz;
   nvoxel = option_data->nvoxel;
 
   /*----- allocate memory space for calculations -----*/
   y = (float *) malloc(sizeof(float)*nxyz);
   sstr = (float *) malloc(sizeof(float)*nxyz);
   if ((y == NULL) || (sstr == NULL))
      ANOVA_error ("unable to allocate sufficient memory");
 
   volume_zero (sstr, nxyz);
   for (i = 0;  i < option_data->a;  i++)
   {
      ni = option_data->na[i];
      sprintf (filename, "y.%d", i);
      volume_read (filename, y, nxyz);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      {
	 yval = y[ixyz];
         sstr[ixyz] += yval * yval / ni; 
      }
   }

   volume_read ("ysum", y, nxyz);
   for (ixyz = 0;  ixyz < nxyz;  ixyz++)
   {
      yval = y[ixyz];
      sstr[ixyz] -= yval * yval / nt;

      /*----- protection against round-off error -----*/
      if (sstr[ixyz] < 0.0)  sstr[ixyz] = 0.0;
   }

   /*----- ysum data file is no longer needed -----*/
   volume_delete ("ysum");

   /*----- save treatment sum of squares -----*/
   if (nvoxel > 0)
      printf ("SSTR = %f \n", sstr[nvoxel-1]);
   volume_write ("sstr", sstr, nxyz);

   /*----- release memory -----*/
   free (sstr);   sstr = NULL;
   free (y);      y = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to calculate the error sum of squares,  SSE = SSTO - SSTR .
   The result is stored (temporarily) in disk file sse.3danova.
*/

void calculate_sse (anova_options * option_data)
{
   float * sstr = NULL;                /* pointer to sstr data */
   float * sse = NULL;                 /* pointer to sse data */
   int ixyz, nxyz;                     /* voxel counters */
   int nvoxel;                         /* output voxel # */


   /*----- assign local variables -----*/
   nxyz = option_data->nxyz;
   nvoxel = option_data->nvoxel;
   
   /*----- allocate memory space for calculations -----*/
   sstr = (float *) malloc(sizeof(float)*nxyz);
   sse = (float *) malloc(sizeof(float)*nxyz);
   if ((sstr == NULL) || (sse == NULL))
      ANOVA_error ("unable to allocate sufficient memory");
   
   /*----- read SSTO (total sum of squares) -----*/
   volume_read ("ssto", sse, nxyz);

   /*----- read SSTR (treatment sum of squares) -----*/
   volume_read ("sstr", sstr, nxyz);
   for (ixyz = 0;  ixyz < nxyz;  ixyz++)
   {
      /*-----  SSE = SSTO - SSTR  -----*/
      sse[ixyz] -= sstr[ixyz];

      /*----- protection against round-off error -----*/
      if (sse[ixyz] < 0.0)  sse[ixyz] = 0.0;
   }

   /*----- ssto data file is no longer needed -----*/
   volume_delete ("ssto");

   /*----- save error sum of squares -----*/
   if (nvoxel > 0)
      printf ("SSE = %f \n", sse[nvoxel-1]);
   volume_write ("sse", sse, nxyz);

   /*----- release memory -----*/
   free (sse);    sse  = NULL;
   free (sstr);   sstr = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to calculate the F-statistic for treatment, F = MSTR / MSE,
      where MSTR = SSTR / (r-1),
      and   MSE  = SSE  / (n-r).
 
   The output is stored as a 2 sub-brick AFNI data set.  The first sub-brick 
   contains the square root of MSTR (mean sum of squares due to treatment), 
   and the second sub-brick contains the corresponding F-statistic. 
*/

void calculate_f_statistic (anova_options * option_data)
{
   const float  EPSILON = 1.0e-10;      /* protect against divide by zero */
   float * fin = NULL;                  /* pointer to input float data */
   float * mstr = NULL;                 /* pointer to MSTR data */
   float * ftr = NULL;                  /* pointer to F due-to-treatment */
   int r;                               /* number of factor levels */
   int nt;                              /* total number of observations */
   int ixyz, nxyz;                      /* voxel counters */
   int nvoxel;                          /* output voxel # */
   float fval;                          /* float value used in calculations */
   float mse;                           /* mean square error */
 

   /*----- initialize local variables -----*/
   r = option_data->a;
   nt = option_data->nt;
   nxyz = option_data->nxyz;
   nvoxel = option_data->nvoxel;
   
   /*----- allocate memory space for calculations -----*/
   /*----- Note:  the order in which memory allocations take place  
                  is important! -----*/
   ftr = (float *) malloc(sizeof(float)*nxyz);
   mstr = (float *) malloc(sizeof(float)*nxyz);
   fin = (float *) malloc(sizeof(float)*nxyz);
   if ((fin == NULL) || (ftr == NULL) || (mstr == NULL))
      ANOVA_error ("unable to allocate sufficient memory");


   /*----- calculate mean SS due to treatments -----*/
   volume_read ("sstr", fin, nxyz);   
   fval = 1.0 / (r - 1.0);
   for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      mstr[ixyz] = fin[ixyz] * fval;     /*---  MSTR = SSTR / (r-1)  ---*/
   if (nvoxel > 0)
      printf ("MSTR = %f \n", mstr[nvoxel-1]);

   /*----- calculate F-statistic  FTR = MSTR / MSE  -----*/
   volume_read ("sse", fin, nxyz);     
   fval = 1.0 / (nt - r);
   for (ixyz = 0;  ixyz < nxyz;  ixyz++)
     {
       mse = fin[ixyz] * fval;            /*---  MSE = SSE / (nt-r)  ---*/
       if (mse < EPSILON)
	 ftr[ixyz] = 0.0;
       else
	 ftr[ixyz] = mstr[ixyz] / mse;      /*---  FTR = MSTR / MSE  ---*/
     }
   if (nvoxel > 0)
      printf ("FTR = %f \n", ftr[nvoxel-1]);

   /*----- release memory -----*/
   free (fin);   fin = NULL;

   /*----- write out afni data file -----*/
   for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      mstr[ixyz] = sqrt(mstr[ixyz]);      /*-- mstr now holds square root --*/
   write_afni_data (option_data, option_data->ftrname, mstr, ftr, r-1, nt-r);

   /*----- this data file is no longer needed -----*/
   volume_delete ("sstr");

   /*----- release memory -----*/
   free (mstr);   mstr = NULL;
   free (ftr);    ftr = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to calculate the mean treatment effect at the user specified
   treatment level.  The output is stored as a 2 sub-brick AFNI data set.
   The first sub-brick contains the estimated mean at this treatment level, 
   and the second sub-brick contains the corresponding t-statistic.
*/

void calculate_means (anova_options * option_data)
{
   const float  EPSILON = 1.0e-10;    /* protect against divide by zero */
   float * fin = NULL;                /* pointer to float input data */
   float * mean = NULL;               /* pointer to treatment mean data */
   float * tmean = NULL;              /* pointer to t-statistic data */
   int imean;                         /* output mean option index */
   int level;                         /* factor level index */
   int ni;                            /* number obs. at factor level i */
   int ixyz, nxyz;                    /* voxel counters */
   int nvoxel;                        /* output voxel # */
   int r;                             /* number of factor levels */
   int nt;                            /* total number of observations */
   int num_means;                     /* number of user requested means */
   float fval;                        /* for calculating std. dev. */
   float stddev;                      /* est. std. dev. of factor mean */
   char filename[MAX_NAME_LENGTH];    /* input data file name */
 

   /*----- initialize local variables -----*/
   r = option_data->a;
   nt = option_data->nt;
   num_means = option_data->num_ameans;
   nxyz = option_data->nxyz;
   nvoxel = option_data->nvoxel;

   /*----- allocate memory space for calculations -----*/
   mean = (float *) malloc(sizeof(float)*nxyz);
   tmean = (float *) malloc(sizeof(float)*nxyz);
   if ((mean == NULL) || (tmean == NULL))  
      ANOVA_error ("unable to allocate sufficient memory");
   
   /*----- loop over user specified treatment means -----*/ 
   for (imean = 0;  imean < num_means;  imean++)
   {
      level = option_data->ameans[imean];
      ni = option_data->na[level];

      /*----- allocate temporary memory space -----*/
      fin = (float *) malloc(sizeof(float)*nxyz);
      if (fin == NULL)  ANOVA_error ("unable to allocate sufficient memory");
 
      /*----- estimate factor mean for this treatment level -----*/
      sprintf (filename, "y.%d", level); 
      volume_read (filename, fin, nxyz);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	 mean[ixyz] =  fin[ixyz] / ni;
      if (nvoxel > 0)
         printf ("Mean [%d] = %f \n", level+1, mean[nvoxel-1]);

      /*----- divide by estimated standard deviation of factor mean -----*/
      volume_read ("sse", fin, nxyz);      
      fval = (1.0 / (nt-r)) * (1.0 / ni);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      {
	 stddev =  sqrt(fin[ixyz] * fval);
	 if (stddev < EPSILON)
	   tmean[ixyz] = 0.0;
	 else
	   tmean[ixyz] = mean[ixyz] / stddev;
      }
      if (nvoxel > 0)
         printf ("t-Mean [%d] = %f \n", level+1, tmean[nvoxel-1]);
 
      /*----- release memory -----*/
      free (fin);   fin = NULL;

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->amname[imean], 
                       mean, tmean, nt-r, 0);

   }

   /*----- release memory -----*/
   free (tmean);  tmean = NULL;
   free (mean);   mean = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to estimate the difference in the means between two user specified
   treatment levels.  The output is a 2 sub-brick AFNI data set.  The first
   sub-brick contains the estimated difference in the means.  The second
   sub-brick contains the corresponding t-statistic.
*/

void calculate_differences (anova_options * option_data)
{
   const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
   float * fin = NULL;                 /* pointer to float input data */
   float * diff = NULL;                /* pointer to est. diff. in means */
   float * tdiff = NULL;               /* pointer to t-statistic data */
   int r;                              /* number of factor levels */
   int nt;                             /* total number of observations */
   int ixyz, nxyz;                     /* voxel counters */
   int nvoxel;                         /* output voxel # */
   int num_diffs;                      /* number of user requested diffs. */
   int idiff;                          /* index for requested differences */
   int i, j;                           /* factor level indices */
   int ni, nj;                         /* number of obs. at levels i and j */
   float fval;                         /* for calculating std. dev. */
   float stddev;                       /* est. std. dev. of difference */
   char filename[MAX_NAME_LENGTH];     /* input file name */


   /*----- initialize local variables -----*/
   r = option_data->a;
   nt = option_data->nt;
   num_diffs = option_data->num_adiffs;
   nxyz = option_data->nxyz;
   nvoxel = option_data->nvoxel;
   
   /*----- allocate memory space for calculations -----*/
   diff = (float *) malloc(sizeof(float)*nxyz);
   tdiff = (float *) malloc(sizeof(float)*nxyz);
   if ((diff == NULL) || (tdiff == NULL))
      ANOVA_error ("unable to allocate sufficient memory");

   /*----- loop over user specified treatment differences -----*/
   for (idiff = 0;  idiff < num_diffs;  idiff++)
   {
 
      /*----- allocate temporary memory space -----*/
      fin = (float *) malloc(sizeof(float)*nxyz);
      if (fin == NULL)  ANOVA_error ("unable to allocate sufficient memory");
 
     /*----- read first treatment level mean -----*/
      i = option_data->adiffs[idiff][0];
      ni = option_data->na[i];
      sprintf (filename, "y.%d", i);
      volume_read (filename, fin, nxyz); 
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
         diff[ixyz] = fin[ixyz] / ni;

      /*----- subtract second treatment level mean -----*/
      j = option_data->adiffs[idiff][1];
      nj = option_data->na[j];
      sprintf (filename, "y.%d", j);
      volume_read (filename, fin, nxyz);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
         diff[ixyz] -= fin[ixyz] / nj;
      if (nvoxel > 0)
         printf ("Diff [%d] - [%d] = %f \n", i+1, j+1, diff[nvoxel-1]);

      /*----- divide by estimated standard deviation of difference -----*/
      volume_read ("sse", fin, nxyz);
      fval = (1.0 / (nt-r)) * ((1.0/ni) + (1.0/nj));
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      {
	 stddev = sqrt (fin[ixyz] * fval);
	 if (stddev < EPSILON)
	   tdiff[ixyz] = 0.0;
	 else
	   tdiff[ixyz] = diff[ixyz] / stddev;
      }          
      if (nvoxel > 0)
         printf ("t-Diff [%d] - [%d] = %f \n", i+1, j+1, tdiff[nvoxel-1]);

      /*----- release memory -----*/
      free (fin);   fin = NULL;

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->adname[idiff], 
                       diff, tdiff, nt-r, 0);

   }

   /*----- release memory -----*/
   free (tdiff);  tdiff = NULL;
   free (diff);   diff = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to estimate a user specified contrast in treatment levels.
   The output is stored as a 2 sub-brick AFNI data set.  The first
   sub-brick contains the estimated contrast.  The second sub-brick contains 
   the corresponding t-statistic.
*/

void calculate_contrasts (anova_options * option_data)
{
   const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
   float * fin = NULL;                 /* pointer to float input data */
   float * contr = NULL;               /* pointer to contrast estimate */
   float * tcontr = NULL;              /* pointer to t-statistic data */
   int r;                              /* number of factor levels */
   int nt;                             /* total number of observations */
   int ixyz, nxyz;                     /* voxel counters */
   int nvoxel;                         /* output voxel # */
   int num_contr;                      /* number of user requested contrasts */
   int icontr;                         /* index of user requested contrast */
   int level;                          /* factor level index */
   int ni;                             /* number of obs. at factor level i */
   float fval;                         /* for calculating std. dev. */
   float c;                            /* contrast coefficient */
   float stddev;                       /* est. std. dev. of contrast */
   char filename[MAX_NAME_LENGTH];     /* input data file name */


   /*----- initialize local variables -----*/
   r = option_data->a;
   nt = option_data->nt;
   num_contr = option_data->num_acontr;
   nxyz = option_data->nxyz;
   nvoxel = option_data->nvoxel;
   
   /*----- allocate memory space for calculations -----*/
   contr  = (float *) malloc(sizeof(float)*nxyz);
   tcontr = (float *) malloc(sizeof(float)*nxyz);
   if ((contr == NULL) || (tcontr == NULL))
      ANOVA_error ("unable to allocate sufficient memory");


   /*----- loop over user specified constrasts -----*/
   for (icontr = 0;  icontr < num_contr;  icontr++)
   {
      volume_zero (contr, nxyz);
      fval = 0.0;

      /*----- allocate temporary memory space -----*/
      fin = (float *) malloc(sizeof(float)*nxyz);
      if (fin == NULL)  ANOVA_error ("unable to allocate sufficient memory");
  
      for (level = 0;  level < r;  level++)
      {
	 c = option_data->acontr[icontr][level]; 
	 if (c == 0.0) continue; 
    
	 /*----- add c * treatment level mean to contrast -----*/
         sprintf (filename, "y.%d", level);
	 volume_read (filename, fin, nxyz);
         ni = option_data->na[level];
         fval += c * c / ni;
	 for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	    contr[ixyz] += c * fin[ixyz] / ni;
      }
      if (nvoxel > 0)
         printf ("Contr no.%d = %f \n", icontr+1, contr[nvoxel-1]);

      /*----- divide by estimated standard deviation of the contrast -----*/
      volume_read ("sse", fin, nxyz);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      {
	 stddev = sqrt ((fin[ixyz] / (nt-r)) * fval);
	 if (stddev < EPSILON)
	   tcontr[ixyz] = 0.0;
	 else
	   tcontr[ixyz] = contr[ixyz] / stddev;
      }   
      if (nvoxel > 0)
         printf ("t-Contr no.%d = %f \n", icontr+1, tcontr[nvoxel-1]);

      /*----- release memory -----*/
      free (fin);   fin = NULL;

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->acname[icontr], 
                       contr, tcontr, nt-r, 0);
   }

   /*----- release memory -----*/
   free (tcontr);   tcontr = NULL;
   free (contr);    contr = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to perform single factor ANOVA.
*/

void calculate_anova (anova_options * option_data)
{

   /*-----  sum observations within treatment -----*/
   calculate_y (option_data);
 
   /*-----  sum all observations  -----*/
   calculate_ysum (option_data);

   /*-----  calculate sum of squares  -----*/
   calculate_ss (option_data);

   /*-----  calculate total (corrected for the mean) sum of squares -----*/
   calculate_ssto (option_data);

   /*-----  calculate treatment sum of squares  -----*/
   calculate_sstr (option_data);

   /*-----  calculate error sum of squares  -----*/
   calculate_sse (option_data);

}


/*---------------------------------------------------------------------------*/
/*
   Routine to analyze the results from a single factor ANOVA.
*/

void analyze_results (anova_options * option_data)
{
 
   /*-----  calculate F-statistic for treatment effect  -----*/
   if (option_data->nftr > 0)  calculate_f_statistic (option_data);

   /*-----  estimate factor level means  -----*/
   if (option_data->num_ameans > 0)  calculate_means (option_data);

   /*-----  estimate differences in factor level means -----*/
   if (option_data->num_adiffs > 0)  calculate_differences (option_data);

   /*-----  calculate contrasts in factor levels -----*/
   if (option_data->num_acontr > 0)  calculate_contrasts (option_data);

}


/*---------------------------------------------------------------------------*/
/*
   Routine to create an AFNI "bucket" output dataset.
*/

void create_bucket (anova_options * option_data)

{
  char bucket_str[10000];             /* command line for program 3dbucket */
  char refit_str[10000];              /* command line for program 3drefit */
  THD_3dim_dataset * dset=NULL;       /* input afni data set pointer */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int i;                              /* file index */
  int ibrick;                         /* sub-brick index number */
  char str[100];                      /* temporary character string */


  /*----- read first dataset -----*/
  dset = THD_open_dataset (option_data->first_dataset) ;
  if( ! ISVALID_3DIM_DATASET(dset) ){
    fprintf(stderr,"*** Unable to open dataset file %s\n",
	    option_data->first_dataset);
    exit(1) ;
  }

  if( DSET_IS_1D(dset) ) USE_1D_filenames(1) ; /* 14 Mar 2003 */
  

  /*----- make an empty copy of this dataset -----*/
  new_dset = EDIT_empty_copy( dset ) ;
  THD_delete_3dim_dataset (dset , False);   dset = NULL;
  EDIT_dset_items (new_dset, 
		   ADN_directory_name, option_data->session,
		   ADN_none);


  /*----- begin command line for program 3dbucket -----*/
  strcpy (bucket_str, "3dbucket ");
  strcat (bucket_str, " -prefix ");
  strcat (bucket_str, option_data->bucket_filename);


  /*----- begin command line for program 3drefit -----*/
  strcpy (refit_str, "3drefit ");
  ibrick = -1;

 
  /*----- make F-statistic sub-bricks -----*/
  if (option_data->nftr != 0)
    {
      add_file_name (new_dset, option_data->ftrname, bucket_str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:Inten ",
	       ibrick, option_data->ftrname);
      strcat (refit_str, str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:F-stat ", 
	       ibrick, option_data->ftrname);
      strcat (refit_str, str);
    }
      
  
  /*----- make factor level mean sub-bricks -----*/
  if (option_data->num_ameans > 0)
    for (i = 0; i < option_data->num_ameans; i++)
      {
	add_file_name (new_dset, option_data->amname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Mean ", 
		 ibrick, option_data->amname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ", 
		 ibrick, option_data->amname[i]);
	strcat (refit_str, str);
      }
  

  /*----- make difference in factor level means sub-bricks -----*/
  if (option_data->num_adiffs > 0)
    for (i = 0; i < option_data->num_adiffs; i++)
      {
	add_file_name (new_dset, option_data->adname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Diff ", 
		 ibrick, option_data->adname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ", 
		 ibrick, option_data->adname[i]);
	strcat (refit_str, str);
      }
  

  /*----- make contrast in factor level means sub-bricks -----*/
  if (option_data->num_acontr > 0)
    for (i = 0; i < option_data->num_acontr; i++)
      {
	add_file_name (new_dset, option_data->acname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Contr ", 
		 ibrick, option_data->acname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ", 
		 ibrick, option_data->acname[i]);
	strcat (refit_str, str);
      }


  /*----- invoke program 3dbucket to generate bucket type output dataset
          by concatenating previous output files -----*/
  printf("Writing `bucket' dataset ");
  printf("into %s\n", option_data->bucket_filename);
  system (bucket_str);


  /*----- invoke program 3drefit to label individual sub-bricks -----*/
  add_file_name (new_dset, option_data->bucket_filename, refit_str);
  system (refit_str);


  /*----- release memory -----*/
  THD_delete_3dim_dataset (new_dset , False);   new_dset = NULL;
  
}


/*---------------------------------------------------------------------------*/
/*
   Routine to release memory and remove any remaining temporary data files.
   If the '-bucket' option has been used, create the bucket dataset and
   dispose of the 2 sub-brick datasets.
*/

void terminate (anova_options * option_data)
{
  int i, j;
  THD_3dim_dataset * dset=NULL;       /* input afni data set pointer */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  char filename[MAX_NAME_LENGTH];


  /*----- remove temporary data files -----*/
  volume_delete ("sstr");
  volume_delete ("sse");
  for (i = 0; i < option_data->a; i++)
    {
      sprintf (filename, "y.%d", i);
      volume_delete (filename);
    }
  
  
  /*----- create bucket dataset -----*/
  if (option_data->bucket_filename != NULL)
    create_bucket (option_data);

  
  /*----- if 'bucket' datset was created, remove the individual 2-subbrick
          data files -----*/
  if (option_data->bucket_filename != NULL)
    {

      /*----- read first dataset -----*/
      dset = THD_open_dataset (option_data->first_dataset) ;
      if( ! ISVALID_3DIM_DATASET(dset) ){
	fprintf(stderr,"*** Unable to open dataset file %s\n",
		option_data->first_dataset);
	exit(1) ;
      }
      
      /*----- make an empty copy of this dataset -----*/
      new_dset = EDIT_empty_copy (dset);
      THD_delete_3dim_dataset (dset , False);   dset = NULL;
      EDIT_dset_items (new_dset, 
		       ADN_directory_name, option_data->session,
		       ADN_none);
      
      /*----- remove F-statistic data file -----*/
      if (option_data->nftr != 0)
	remove_dataset (new_dset, option_data->ftrname);
      
      /*----- remove mean factor level data files -----*/
      if (option_data->num_ameans > 0)
	for (i = 0; i < option_data->num_ameans; i++)
	  remove_dataset (new_dset, option_data->amname[i]);
      
      /*----- remove difference in factor levels data files -----*/
      if (option_data->num_adiffs > 0)
	for (i = 0; i < option_data->num_adiffs; i++)
	  remove_dataset (new_dset, option_data->adname[i]);
      
      /*----- remove contrast in factor levels data files -----*/
      if (option_data->num_acontr > 0)
	for (i = 0; i < option_data->num_acontr; i++)
	  remove_dataset (new_dset, option_data->acname[i]);
      
      THD_delete_3dim_dataset (new_dset , False);   new_dset = NULL;
    }  


  /*----- deallocate memory -----*/
  destroy_anova_options (option_data);

}


/*---------------------------------------------------------------------------*/
/*
   Single factor analysis of variance (ANOVA).
*/
 
int main (int argc, char ** argv)
{
   anova_options * option_data = NULL;

  
   /*----- Identify software -----*/
   printf ("\n\n");
   printf ("Program:          %s \n", PROGRAM_NAME);
   printf ("Author:           %s \n", PROGRAM_AUTHOR); 
   printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
   printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
   printf ("\n");


   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   machdep() ;
   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   /*----- program initialization -----*/
   initialize (argc, argv, &option_data);

   /*----- calculate sums of squares -----*/
   calculate_anova (option_data);

   /*----- generate requested output -----*/
   analyze_results (option_data);

   /*----- terminate program -----*/
   terminate (option_data);
   free (option_data);   option_data = NULL;

   exit(0);
}

















