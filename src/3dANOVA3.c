/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
   This program calculates three-factor analysis of variance
   for 3 dimensional AFNI data sets.

   File:    3dANOVA3.c
   Author:  B. D. Ward
   Date:    29 January 1997

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

   Mod:     In get_options(), calloc and free n (stack space is limited).
   Date:    19 July 2004 [rickr]

   Mod:     Update computation of sums of squares for a/bmean, a/bdiff
            and a/bcontr, for type 4 ANOVAs.  See calc_mean_sum2_acontr().
            Similar update for model type 5.  See calc_type5_acontr() and
            calc_type5_bcontr().  Cleaned up compiler warnings.
   Date:    23 Sep 2005 [rickr,gangc]

   Mod:     Added options -aBcontr and -Abcontr for types 4 and 5.
            The contrast and t-stat files will go at the end of any bucket.
            Changed some EPSILON values from e-10 to e-15.
   Date:    17 Oct 2005 [rickr,gangc]

   Mod:     Fixed -help typo, num_Abcontr assignment and df in calc_Abc().
   Date:    27 Oct 2005 [rickr,gangc]

   Mod:     Allow old computations to be done via -old_method.  This affects
            calc_ameans, bmeans, adiff, bdiff, acontr, bcontr, and for the
            types 4 and 5 models, only (not type 1).
   Date:    22 Nov 2005 [rickr]

   Mod:     The -old_method option requires -OK.
            Added -assume_sph and a check that the contrasts sum to zero.
            (if so, use old_method)
   Date:    02 Dec 2005 [rickr]

   Mod:     Added -aBdiff, -Abdiff and -abmean options and routines.
            Replaced calc_mean_sum2_acontr() with calc_type4_acontr(), to
              avoid intermediate storage of data as floats (by absorbing the
              calculate_t_from_sums() operation).
            Replaced calc_mean_sum2_bcontr() with calc_type4_bcontr().
            Removed calculate_t_from_sums().
            Do checks against EPSILON before sqrt(), in case < 0.
   Date:    28 Dec 2005 [rickr]

   Mod:     Bothered to actually assign df_prod before using it in
            calc_type4_bcontr().  Fixed -aBdiff label.  Thanks to Debbie
            at U-Toronto.
   Date:    31 Jan 2005 [rickr]

   Mod:     Small help update, including example.
   Date:    09 Feb 2006 [rickr]
*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME    "3dANOVA3"                   /* name of this program */
#define PROGRAM_AUTHOR  "B. Douglas Ward"                  /* program author */
#define PROGRAM_INITIAL "29 Jan 1997"     /* date of initial program release */
#define PROGRAM_LATEST  "23 Sep 2005"     /* date of latest program revision */

/*---------------------------------------------------------------------------*/

#define SUFFIX ".3danova3"                /* suffix for temporary data files */


#include "3dANOVA.h"
#include "3dANOVA.lib"


/*---------------------------------------------------------------------------*/
/*
  Routine to display 3dANOVA3 help menu.
*/

void display_help_menu()
{
  printf
    (
     "This program performs three-factor ANOVA on 3D data sets.           \n\n"
     "Usage: \n"
     "3dANOVA3 \n"
     "-type  k          type of ANOVA model to be used:\n"
     "                         k = 1   A,B,C fixed;          AxBxC\n"
     "                         k = 2   A,B,C random;         AxBxC\n"
     "                         k = 3   A fixed; B,C random;  AxBxC\n"
     "                         k = 4   A,B fixed; C random;  AxBxC\n"
     "                         k = 5   A,B fixed; C random;  AxB,BxC,C(A)\n"
     "\n"
     "-alevels a                     a = number of levels of factor A\n"
     "-blevels b                     b = number of levels of factor B\n"
     "-clevels c                     c = number of levels of factor C\n"
     "-dset 1 1 1 filename           data set for level 1 of factor A\n"
     "                                        and level 1 of factor B\n"
     "                                        and level 1 of factor C\n"
     " . . .                           . . .\n"
     "\n"
     "-dset i j k filename           data set for level i of factor A\n"
     "                                        and level j of factor B\n"
     "                                        and level k of factor C\n"
     " . . .                           . . .\n"
     "\n"
     "-dset a b c filename           data set for level a of factor A\n"
     "                                        and level b of factor B\n"
     "                                        and level c of factor C\n"
     "\n"
     "[-voxel num]                   screen output for voxel # num\n"
     "[-diskspace]                   print out disk space required for\n"
     "                                  program execution\n"
     "\n"
     "[-mask mset]                   use sub-brick #0 of dataset 'mset'\n"
     "                               to define which voxels to process\n"
     "\n"
     "\n"
     "The following commands generate individual AFNI 2 sub-brick datasets:\n"
     "  (In each case, output is written to the file with the specified\n"
     "   prefix file name.)\n"
     "\n"
     "[-fa prefix]                F-statistic for factor A effect\n"
     "[-fb prefix]                F-statistic for factor B effect\n"
     "[-fc prefix]                F-statistic for factor C effect\n"
     "[-fab prefix]               F-statistic for A*B interaction\n"
     "[-fac prefix]               F-statistic for A*C interaction\n"
     "[-fbc prefix]               F-statistic for B*C interaction\n"
     "[-fabc prefix]              F-statistic for A*B*C interaction\n"
     "\n"
     "[-amean i prefix]           estimate of factor A level i mean\n"
     "[-bmean i prefix]           estimate of factor B level i mean\n"
     "[-cmean i prefix]           estimate of factor C level i mean\n"
     "[-xmean i j k prefix]       estimate mean of cell at factor A level i,\n"
     "                               factor B level j, factor C level k\n"
     "\n"
     "[-adiff i j prefix]         difference between factor A levels i and j\n"
     "                               (with factors B and C collapsed)\n"
     "[-bdiff i j prefix]         difference between factor B levels i and j\n"
     "                               (with factors A and C collapsed)\n"
     "[-cdiff i j prefix]         difference between factor C levels i and j\n"
     "                               (with factors A and B collapsed)\n"
     "[-xdiff i j k l m n prefix] difference between cell mean at A=i,B=j,\n"
     "                               C=k, and cell mean at A=l,B=m,C=n\n"
     "\n"
     "[-acontr c1...ca prefix]    contrast in factor A levels\n"
     "                               (with factors B and C collapsed)\n"
     "[-bcontr c1...cb prefix]    contrast in factor B levels\n"
     "                               (with factors A and C collapsed)\n"
     "[-ccontr c1...cc prefix]    contrast in factor C levels\n"
     "                               (with factors A and B collapsed)\n"
     "\n"
     "[-aBcontr c1 ... ca : j prefix]   2nd order contrast in A, at fixed\n"
     "                                     B level j (collapsed across C)\n"
     "[-Abcontr i : c1 ... cb prefix]   2nd order contrast in B, at fixed\n"
     "                                     A level i (collapsed across C)\n"
     "\n"
     "[-aBdiff i_1 i_2 : j prefix] difference between levels i_1 and i_2 of\n"
     "                               factor A, with factor B fixed at level j\n"
     "\n"
     "[-Abdiff i : j_1 j_2 prefix] difference between levels j_1 and j_2 of\n"
     "                               factor B, with factor A fixed at level i\n"
     "\n"
     "[-abmean i j prefix]         mean effect at factor A level i and\n"
     "                               factor B level j\n"
     "\n"
     "The following command generates one AFNI 'bucket' type dataset:\n"
     "\n"
     "[-bucket prefix]         create one AFNI 'bucket' dataset whose\n"
     "                           sub-bricks are obtained by concatenating\n"
     "                           the above output files; the output 'bucket'\n"
     "                           is written to file with prefix file name\n"
     "\n"
     "Modified ANOVA computation options:    (December, 2005)\n"
     "\n"
     "     ** These options apply to model types 4 and 5, only.\n"
     "        For details, see %s\n"
     "\n"
     "[-old_method]       request to perform ANOVA using the previous\n"
     "                    functionality (requires -OK, also)\n"
     "\n"
     "[-OK]               confirm you understand that contrasts that\n"
     "                    do not sum to zero have inflated t-stats, and\n"
     "                    contrasts that do sum to zero assume sphericity\n"
     "                    (to be used with -old_method)\n"
     "\n"
     "[-assume_sph]       assume sphericity (zero-sum contrasts, only)\n"
     "\n"
     "                    This allows use of the old_method for\n"
     "                    computing contrasts which sum to zero (this\n"
     "                    includes diffs, for instance).  Any contrast\n"
     "                    that does not sum to zero is invalid, and\n"
     "                    cannot be used with this option (such as\n"
     "                    ameans).\n"

     "\n"
     "-----------------------------------------------------------------\n"
     "example: \"classic\" houses/faces/donuts for 4 subjects (2 genders)\n"
     "         (level sets are gender (M/W), image (H/F/D), and subject)\n"
     "\n"
     "    Note: factor C is really subject within gender (since it is\n"
     "          nested).  There are 4 subjects in this example, and 2\n"
     "          subjects per gender.  So clevels is 2.\n"
     "\n"
     "    3dANOVA3 -type 5                            \\\n"
     "        -alevels 2                              \\\n"
     "        -blevels 3                              \\\n"
     "        -clevels 2                              \\\n"
     "        -dset 1 1 1 man1_houses+tlrc            \\\n"
     "        -dset 1 2 1 man1_faces+tlrc             \\\n"
     "        -dset 1 3 1 man1_donuts+tlrc            \\\n"
     "        -dset 1 1 2 man2_houses+tlrc            \\\n"
     "        -dset 1 2 2 man2_faces+tlrc             \\\n"
     "        -dset 1 3 2 man2_donuts+tlrc            \\\n"
     "        -dset 2 1 1 woman1_houses+tlrc          \\\n"
     "        -dset 2 2 1 woman1_faces+tlrc           \\\n"
     "        -dset 2 3 1 woman1_donuts+tlrc          \\\n"
     "        -dset 2 1 2 woman2_houses+tlrc          \\\n"
     "        -dset 2 2 2 woman2_faces+tlrc           \\\n"
     "        -dset 2 3 2 woman2_donuts+tlrc          \\\n"
     "        -adiff   1 2           MvsW             \\\n"
     "        -bdiff   2 3           FvsD             \\\n"
     "        -bcontr -0.5 1 -0.5    FvsHD            \\\n"
     "        -aBcontr 1 -1 : 1      MHvsWH           \\\n"
     "        -aBdiff  1  2 : 1      same_as_MHvsWH   \\\n"
     "        -Abcontr 2 : 0 1 -1    WFvsWD           \\\n"
     "        -Abdiff  2 : 2 3       same_as_WFvsWD   \\\n"
     "        -Abcontr 2 : 1 7 -4.2  goofy_example    \\\n"
     "        -bucket donut_anova\n"
     "\n", ANOVA_MODS_LINK);

  printf
    (
     "\n"
     "N.B.: For this program, the user must specify 1 and only 1 sub-brick\n"
     "      with each -dset command. That is, if an input dataset contains\n"
     "      more than 1 sub-brick, a sub-brick selector must be used, e.g.:\n"
     "      -dset 2 4 5 'fred+orig[3]'\n"
     );

  printf("\n" MASTER_SHORTHELP_STRING ) ;

  printf(ANOVA_FLOAT_HELP) ;

  PRINT_COMPILE_DATE; exit(0);
}

/* define index into n[MAX_LEVELS][MAX_LEVELS][MAX_LEVELS] 19 Jul 2004 [rickr]*/
#define N_INDEX(i,j,k) n[(k) + MAX_LEVELS * ((j) + MAX_LEVELS * (i))]

/*---------------------------------------------------------------------------*/
/*
   Routine to get user specified anova options.
*/

void get_options (int argc, char ** argv, anova_options * option_data)
{
  int nopt = 1;                  /* input option argument counter */
  int ival, jval, kval;          /* integer input */
  int i, j, k;                   /* factor level counters */
  int nijk;                      /* number of data files in cell i,j,k */
  float fval;                    /* float input */
  THD_3dim_dataset * dset=NULL;             /* test whether data set exists */
  char message[MAX_NAME_LENGTH];            /* error message */
  /* int n[MAX_LEVELS][MAX_LEVELS][MAX_LEVELS];    data file counters       */
  int * n;                       /* save stack space    19 Jul 2004 [rickr] */


  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();

  /*----- add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv);

  /*----- initialize the input options -----*/
  initialize_options (option_data);

  /*----- initialize data file counters -----*/
  n = (int *)calloc(MAX_LEVELS*MAX_LEVELS*MAX_LEVELS, sizeof(int));
  if ( !n )
  {
    sprintf(message, "failed to allocate %u bytes for file counters\n",
            (unsigned int)(MAX_LEVELS*MAX_LEVELS*MAX_LEVELS * sizeof(int)) );
    ANOVA_error(message);
  }

#if 0  /* replaced array and init with pointer and calloc() */
  for (i = 0;  i < MAX_LEVELS;  i++)
    for (j = 0;  j < MAX_LEVELS;  j++)
      for (k = 0;  k < MAX_LEVELS;  k++)
	n[i][j][k] = 0;
#endif


  /*----- main loop over input options -----*/
  while (nopt < argc )
    {

      /*----- allocate memory for storing data file names -----*/
      if ((option_data->xname == NULL) && (option_data->a > 0) &&
	  (option_data->b > 0) && (option_data->c > 0))
	{
	  option_data->xname =
	    (char *****) malloc (sizeof(char ****) * option_data->a);
	  for (i = 0;  i < option_data->a;  i++)
	    {
	      option_data->xname[i] =
		(char ****) malloc (sizeof(char ***) * option_data->b);
	      for (j = 0;  j < option_data->b;  j++)
		{
		  option_data->xname[i][j] =
		    (char ***) malloc (sizeof(char **) * option_data->c);
		  for (k = 0;  k < option_data->c;  k++)
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


      /*-----    -datum type   -----*/
      if( strncmp(argv[nopt],"-datum",5) == 0 ){
	if( ++nopt >= argc ) ANOVA_error("need an argument after -datum!") ;
	
	if( strcmp(argv[nopt],"short") == 0 ){
	  option_data->datum = MRI_short ;
	} else if( strcmp(argv[nopt],"float") == 0 ){
	  option_data->datum = MRI_float ;
	} else {
	  char buf[256] ;
	  sprintf(buf,"-datum of type '%s' is not supported in 3dANOVA3!",
		  argv[nopt] ) ;
	  ANOVA_error(buf) ;
	}
	nopt++ ; continue ;  /* go to next arg */
      }


      /*-----   -session dirname    -----*/
      if( strncmp(argv[nopt],"-session",5) == 0 ){
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


      /*-----  -type k  -----*/
      if (strncmp(argv[nopt], "-type", 5) == 0)
      {
         nopt++;
	 if (nopt >= argc)  ANOVA_error ("need argument after -type ");
         sscanf (argv[nopt], "%d", &ival);
	 if ((ival < 1) || (ival > 5))
	    ANOVA_error ("illegal argument after -type ");
	 option_data->model = ival;
         nopt++;
         continue;
      }


      /*------------------------------------------------------------*/
      /*-----  Using the old_method:      02 Dec 2005 [rickr]  -----*/
      /*   if -old_method
               if -OK, all contrasts are okay
               else if -assume_sph, contrasts adding to 0 are okay
               else complain and fail

           bits: -old_method = 001, -OK = 010, -assume_sph = 100

           valid bit patterns:
               000 - use the new method
               011 - use the old method
               101 - use the old method (only allows zero-sum contrasts)
        ------------------------------------------------------------*/

      /*-----  -old_method      23 Nov 2005 [rickr]  -----*/
      if (strncmp(argv[nopt], "-old_method", 6) == 0)
      {
         option_data->old_method |= 1;
         nopt++;
         continue;
      }

      /*----- -OK: denote both OK and old_method by old_method = 3 -----*/
      if (strncmp(argv[nopt], "-OK", 3) == 0)
      {
         option_data->old_method |= 2;
         nopt++;
         continue;
      }

      /*----- -assume_sph: denote assume_sphericity by old_method = 4 ----*/
      if (strncmp(argv[nopt], "-assume_sph", 11) == 0)
      {
         option_data->old_method |= 5;  /* also set -old_method bit */
         nopt++;
         continue;
      }

      /*------- end old_method checks ------------------------------*/
      /*------------------------------------------------------------*/


      /*-----  -old_method      22 Nov 2005 [rickr]  -----*/
      if (strncmp(argv[nopt], "-old_method", 6) == 0)
      {
	 option_data->old_method = 1;
         nopt++;
         continue;
      }


      /*-----   -alevels a  -----*/
      if (strncmp(argv[nopt], "-alevels", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -alevels ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > MAX_LEVELS))
	    ANOVA_error ("illegal argument after -alevels ");
	  option_data->a = ival;
	  nopt++;
	  continue;
	}


      /*-----   -blevels b  -----*/
      if (strncmp(argv[nopt], "-blevels", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -blevels ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > MAX_LEVELS))
	    ANOVA_error ("illegal argument after -blevels ");
	  option_data->b = ival;
	  nopt++;
	  continue;
	}


      /*-----   -clevels c  -----*/
      if (strncmp(argv[nopt], "-clevels", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -clevels ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > MAX_LEVELS))
	    ANOVA_error ("illegal argument after -clevels ");
	  option_data->c = ival;
	  nopt++;
	  continue;
	}


      /*-----   -dset alevel blevel clevel filename   -----*/
      if (strncmp(argv[nopt], "-dset", 5) == 0)
	{
	  nopt++;
	  if (nopt+3 >= argc)  ANOVA_error ("need 4 arguments after -dset ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->a))
	    ANOVA_error ("illegal argument after -dset ");
	
	  nopt++;
	  sscanf (argv[nopt], "%d", &jval);
	  if ((jval <= 0) || (jval > option_data->b))
	    ANOVA_error ("illegal argument after -dset ");
	
	  nopt++;
	  sscanf (argv[nopt], "%d", &kval);
	  if ((kval <= 0) || (kval > option_data->c))
	    ANOVA_error ("illegal argument after -dset ");

	  N_INDEX(ival-1, jval-1, kval-1) += 1;
	  nijk = N_INDEX(ival-1, jval-1, kval-1);
	  if (nijk > MAX_OBSERVATIONS)
	    ANOVA_error ("too many data files");
	
	  /*--- check whether input files exist ---*/
	  nopt++;
	  dset = THD_open_dataset( argv[nopt] ) ;
     CHECK_OPEN_ERROR(dset,argv[nopt]) ;

	  /*--- check number of selected sub-bricks ---*/
	  if (DSET_NVALS(dset) != 1)
	    {
	     sprintf(message,"Must specify exactly 1 sub-brick for file %s\n",
 		     argv[nopt]);
	     ANOVA_error (message);
	    }

	  THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
	
	  option_data->xname[ival-1][jval-1][kval-1][nijk-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->xname[ival-1][jval-1][kval-1][nijk-1],
		  argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -fa filename   -----*/
      if (strncmp(argv[nopt], "-fa", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -fa ");
	  option_data->nfa = 1;
	  option_data->faname = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->faname, argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -fb filename   -----*/
      if (strncmp(argv[nopt], "-fb", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -fb ");
	  option_data->nfb = 1;
	  option_data->fbname = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->fbname, argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -fc filename   -----*/
      if (strncmp(argv[nopt], "-fc", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -fc ");
	  option_data->nfc = 1;
	  option_data->fcname = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->fcname, argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -fab filename   -----*/
      if (strncmp(argv[nopt], "-fab", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -fab ");
	  option_data->nfab = 1;
	  option_data->fabname = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->fabname, argv[nopt]);
	  nopt++;
	 continue;
	}


      /*-----   -fac filename   -----*/
      if (strncmp(argv[nopt], "-fac", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -fac ");
	  option_data->nfac = 1;
	  option_data->facname = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->facname, argv[nopt]);
	  nopt++;
	 continue;
	}


      /*-----   -fbc filename   -----*/
      if (strncmp(argv[nopt], "-fbc", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -fbc ");
	  option_data->nfbc = 1;
	  option_data->fbcname = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->fbcname, argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -fabc filename   -----*/
      if (strncmp(argv[nopt], "-fabc", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -fabc ");
	  option_data->nfabc = 1;
	  option_data->fabcname = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->fabcname, argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -amean level filename   -----*/
      if (strncmp(argv[nopt], "-amean", 5) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  ANOVA_error ("need 2 arguments after -amean ");
	
	  option_data->num_ameans++;
	  if (option_data->num_ameans > MAX_MEANS)
	    ANOVA_error ("too many factor A level mean estimates");
	
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->a))
	    ANOVA_error ("illegal argument after -amean ");
	  option_data->ameans[option_data->num_ameans-1] = ival - 1;
	  nopt++;
	
	  option_data->amname[option_data->num_ameans-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->amname[option_data->num_ameans-1], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -bmean level filename   -----*/
      if (strncmp(argv[nopt], "-bmean", 5) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  ANOVA_error ("need 2 arguments after -bmean ");
	
	  option_data->num_bmeans++;
	  if (option_data->num_bmeans > MAX_MEANS)
	    ANOVA_error ("too many factor B level mean estimates");
	
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->b))
	    ANOVA_error ("illegal argument after -bmean ");
	  option_data->bmeans[option_data->num_bmeans-1] = ival - 1;
	  nopt++;
	
	  option_data->bmname[option_data->num_bmeans-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->bmname[option_data->num_bmeans-1], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -cmean level filename   -----*/
      if (strncmp(argv[nopt], "-cmean", 5) == 0)
	{
	  nopt++;
	  if (nopt+1 >= argc)  ANOVA_error ("need 2 arguments after -cmean ");
	
	  option_data->num_cmeans++;
	  if (option_data->num_cmeans > MAX_MEANS)
	    ANOVA_error ("too many factor C level mean estimates");
	
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->c))
	    ANOVA_error ("illegal argument after -cmean ");
	  option_data->cmeans[option_data->num_cmeans-1] = ival - 1;
	  nopt++;
	
	  option_data->cmname[option_data->num_cmeans-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->cmname[option_data->num_cmeans-1], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -xmean i j k filename   -----*/
      if (strncmp(argv[nopt], "-xmean", 5) == 0)
	{
	  nopt++;
	  if (nopt+3 >= argc)  ANOVA_error ("need 4 arguments after -xmean ");
	
	  option_data->num_xmeans++;
	  if (option_data->num_xmeans > MAX_MEANS)
	    ANOVA_error ("too many cell mean estimates");
	
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->a))
	    ANOVA_error ("illegal argument after -xmean ");
	  option_data->xmeans[option_data->num_xmeans-1][0] = ival - 1;
	  nopt++;
	
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->b))
	    ANOVA_error ("illegal argument after -xmean ");
	  option_data->xmeans[option_data->num_xmeans-1][1] = ival - 1;
	  nopt++;
	
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->c))
	    ANOVA_error ("illegal argument after -xmean ");
	  option_data->xmeans[option_data->num_xmeans-1][2] = ival - 1;
	  nopt++;
	
	  option_data->xmname[option_data->num_xmeans-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->xmname[option_data->num_xmeans-1], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -adiff level1 level2 filename   -----*/
      if (strncmp(argv[nopt], "-adiff", 5) == 0)
	{
	  nopt++;
	  if (nopt+2 >= argc)  ANOVA_error ("need 3 arguments after -adiff ");
	
	  option_data->num_adiffs++;
	  if (option_data->num_adiffs > MAX_DIFFS)
	    ANOVA_error ("too many factor A level differences");
	
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->a))
	    ANOVA_error ("illegal argument after -adiff ");
	  option_data->adiffs[option_data->num_adiffs-1][0] = ival - 1;
	  nopt++;
	
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->a))
	    ANOVA_error ("illegal argument after -adiff ");
	  option_data->adiffs[option_data->num_adiffs-1][1] = ival - 1;
	  nopt++;
	
	  option_data->adname[option_data->num_adiffs-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->adname[option_data->num_adiffs-1], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -bdiff level1 level2 filename   -----*/
      if (strncmp(argv[nopt], "-bdiff", 5) == 0)
	{
	  nopt++;
	  if (nopt+2 >= argc)  ANOVA_error ("need 3 arguments after -bdiff ");
	
	  option_data->num_bdiffs++;
	  if (option_data->num_bdiffs > MAX_DIFFS)
	    ANOVA_error ("too many factor B level differences");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->b))
	    ANOVA_error ("illegal argument after -bdiff ");
	  option_data->bdiffs[option_data->num_bdiffs-1][0] = ival - 1;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->b))
	    ANOVA_error ("illegal argument after -bdiff ");
	  option_data->bdiffs[option_data->num_bdiffs-1][1] = ival - 1;
	  nopt++;

	  option_data->bdname[option_data->num_bdiffs-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->bdname[option_data->num_bdiffs-1], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -cdiff level1 level2 filename   -----*/
      if (strncmp(argv[nopt], "-cdiff", 5) == 0)
	{
	  nopt++;
	  if (nopt+2 >= argc)  ANOVA_error ("need 3 arguments after -cdiff ");
	
	  option_data->num_cdiffs++;
	  if (option_data->num_cdiffs > MAX_DIFFS)
	    ANOVA_error ("too many factor C level differences");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->c))
	    ANOVA_error ("illegal argument after -cdiff ");
	  option_data->cdiffs[option_data->num_cdiffs-1][0] = ival - 1;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->c))
	    ANOVA_error ("illegal argument after -cdiff ");
	  option_data->cdiffs[option_data->num_cdiffs-1][1] = ival - 1;
	  nopt++;

	  option_data->cdname[option_data->num_cdiffs-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->cdname[option_data->num_cdiffs-1], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -xdiff i j k l m n filename   -----*/
      if (strncmp(argv[nopt], "-xdiff", 5) == 0)
	{
	  nopt++;
	  if (nopt+6 >= argc)  ANOVA_error ("need 7 arguments after -xdiff ");
	
	  option_data->num_xdiffs++;
	  if (option_data->num_xdiffs > MAX_DIFFS)
	    ANOVA_error ("too many cell means differences");

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->a))
	    ANOVA_error ("illegal argument after -xdiff ");
	  option_data->xdiffs[option_data->num_xdiffs-1][0][0] = ival - 1;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->b))
	    ANOVA_error ("illegal argument after -xdiff ");
	  option_data->xdiffs[option_data->num_xdiffs-1][0][1] = ival - 1;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->c))
	    ANOVA_error ("illegal argument after -xdiff ");
	  option_data->xdiffs[option_data->num_xdiffs-1][0][2] = ival - 1;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->a))
	    ANOVA_error ("illegal argument after -xdiff ");
	  option_data->xdiffs[option_data->num_xdiffs-1][1][0] = ival - 1;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->b))
	    ANOVA_error ("illegal argument after -xdiff ");
	  option_data->xdiffs[option_data->num_xdiffs-1][1][1] = ival - 1;
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->c))
	    ANOVA_error ("illegal argument after -xdiff ");
	  option_data->xdiffs[option_data->num_xdiffs-1][1][2] = ival - 1;
	  nopt++;

	  option_data->xdname[option_data->num_xdiffs-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->xdname[option_data->num_xdiffs-1], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -acontr c1 ... cr filename   -----*/
      if (strncmp(argv[nopt], "-acontr", 5) == 0)
	{
	  nopt++;
	  if (nopt + option_data->a >= argc)
            ANOVA_error ("need a+1 arguments after -acontr ");
	
	  option_data->num_acontr++;
	  if (option_data->num_acontr > MAX_CONTR)
	    ANOVA_error ("too many factor A level contrasts");
	
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


      /*-----   -bcontr c1 ... cr filename   -----*/
      if (strncmp(argv[nopt], "-bcontr", 5) == 0)
	{
	  nopt++;
	  if (nopt + option_data->b >= argc)
            ANOVA_error ("need b+1 arguments after -bcontr ");
	
	  option_data->num_bcontr++;
	  if (option_data->num_bcontr > MAX_CONTR)
	    ANOVA_error ("too many factor B level contrasts");
	  	
	  for (i = 0;  i < option_data->b;  i++)
	    {
	      sscanf (argv[nopt], "%f", &fval);
	      option_data->bcontr[option_data->num_bcontr - 1][i] = fval ;
	      nopt++;
	    }
	
	  option_data->bcname[option_data->num_bcontr-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->bcname[option_data->num_bcontr-1], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -ccontr c1 ... cr filename   -----*/
      if (strncmp(argv[nopt], "-ccontr", 5) == 0)
	{
	  nopt++;
	  if (nopt + option_data->c >= argc)
            ANOVA_error ("need c+1 arguments after -ccontr ");
	
	  option_data->num_ccontr++;
	  if (option_data->num_ccontr > MAX_CONTR)
	    ANOVA_error ("too many factor C level contrasts");
	
	
	  for (i = 0;  i < option_data->c;  i++)
	    {
	      sscanf (argv[nopt], "%f", &fval);
	      option_data->ccontr[option_data->num_ccontr - 1][i] = fval ;
	      nopt++;
	    }
	
	  option_data->ccname[option_data->num_ccontr-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->ccname[option_data->num_ccontr-1], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -aBcontr c1 ... ca : j filename   -----*/
      if (strncmp(argv[nopt], "-aBcontr", 6) == 0)
	{
	  nopt++;
	  if (nopt + option_data->a + 2 >= argc)
            ANOVA_error ("need a+3 arguments after -aBcontr");
	
	  option_data->num_aBcontr++;
	  if (option_data->num_aBcontr > MAX_CONTR)
	    ANOVA_error ("too many aB contrasts ");
	
	  for (i = 0;  i < option_data->a;  i++)
	    {
	      sscanf (argv[nopt], "%f", &fval);
	      option_data->aBcontr[option_data->num_aBcontr - 1][i] = fval ;
	      nopt++;
	    }

          /* skip ':' */
          if (strcmp(argv[nopt],":") != 0)
            {
              fprintf(stderr,"** missing ':' parameter after -aBcontr\n");
              fprintf(stderr,"** should look like '-aBcontr c1 ... ca : j'");
              ANOVA_error ("failing...");
            }
          nopt++;

          /* get B level */
	  sscanf (argv[nopt], "%d", &ival);
	  option_data->aBclevel[option_data->num_aBcontr-1] = ival - 1;
          if (ival <= 0 || ival > option_data->b)
            ANOVA_error("invalid B level in -aBcontr");
          nopt++;

          /* and finally the label */
	  option_data->aBcname[option_data->num_aBcontr-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->aBcname[option_data->num_aBcontr-1], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -Abcontr i : c1 ... ca filename   -----*/
      if (strncmp(argv[nopt], "-Abcontr", 6) == 0)
	{
	  nopt++;
	  if (nopt + option_data->b + 2 >= argc)
            ANOVA_error ("need b+3 arguments after -Abcontr");

	  option_data->num_Abcontr++;
	  if (option_data->num_Abcontr > MAX_CONTR)
	    ANOVA_error ("too many Ab contrasts ");

          /* get A level */
	  sscanf (argv[nopt], "%d", &ival);
	  option_data->Abclevel[option_data->num_Abcontr-1] = ival - 1;
          if (ival <= 0 || ival > option_data->a)
            ANOVA_error("invalid A level in -Abcontr");
          nopt++;

          /* skip ':' */
          if (strcmp(argv[nopt],":") != 0)
            {
              fprintf(stderr,"** missing ':' parameter after -Abcontr\n");
              fprintf(stderr,"** should look like '-Abcontr i : c1 ... cb'");
              ANOVA_error ("failing...");
            }
          nopt++;

          /* read in B contrast */
	  for (i = 0;  i < option_data->b;  i++)
	    {
	      sscanf (argv[nopt], "%f", &fval);
	      option_data->Abcontr[option_data->num_Abcontr - 1][i] = fval ;
	      nopt++;
	    }

          /* and finally the label */
	  option_data->Abcname[option_data->num_Abcontr-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->Abcname[option_data->num_Abcontr-1], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -aBdiff a1 a2 : j filename   -----*/
      if (strncmp(argv[nopt], "-aBdiff", 6) == 0)
	{
	  nopt++;
	  if (nopt + 4 >= argc)
            ANOVA_error ("need 5 arguments after -aBdiff");

	  option_data->num_aBdiffs++;
	  if (option_data->num_aBdiffs > MAX_DIFFS)
	    ANOVA_error ("too many aB diffs");

          /* get A levels */
	  for (i = 0;  i < 2;  i++)
	    {
	      sscanf (argv[nopt], "%d", &ival);
	      option_data->aBdiffs[option_data->num_aBdiffs-1][i] = ival - 1 ;
              if (ival <= 0 || ival > option_data->a)
                ANOVA_error("invalid A level in -aBdiff");
	      nopt++;
	    }

          /* skip ':' */
          if (strcmp(argv[nopt],":") != 0)
            {
              fprintf(stderr,"** missing ':' parameter after -aBdiff\n");
              fprintf(stderr,"** should look like '-aBdiff a1 a2 : j'");
              ANOVA_error ("failing...");
            }
          nopt++;

          /* get B level */
	  sscanf (argv[nopt], "%d", &ival);
	  option_data->aBdlevel[option_data->num_aBdiffs-1] = ival - 1;
          if (ival <= 0 || ival > option_data->b)
            ANOVA_error("invalid B level in -aBdiff");
          nopt++;

          /* and finally the label */
	  option_data->aBdname[option_data->num_aBdiffs-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->aBdname[option_data->num_aBdiffs-1], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -Abdiff i : b1 b2 filename   -----*/
      if (strncmp(argv[nopt], "-Abdiff", 6) == 0)
	{
	  nopt++;
	  if (nopt + 4 >= argc)
            ANOVA_error ("need 5 arguments after -Abdiff");

	  option_data->num_Abdiffs++;
	  if (option_data->num_Abdiffs > MAX_DIFFS)
	    ANOVA_error ("too many Ab diffs");

          /* get A level */
	  sscanf (argv[nopt], "%d", &ival);
	  option_data->Abdlevel[option_data->num_Abdiffs-1] = ival - 1;
          if (ival <= 0 || ival > option_data->a)
            ANOVA_error("invalid A level in -Abdiff");
          nopt++;

          /* skip ':' */
          if (strcmp(argv[nopt],":") != 0)
            {
              fprintf(stderr,"** missing ':' parameter after -Abdiff\n");
              fprintf(stderr,"** should look like '-Abdiff i : b1 b2'");
              ANOVA_error ("failing...");
            }
          nopt++;

          /* get B levels */
	  for (i = 0;  i < 2;  i++)
	    {
	      sscanf (argv[nopt], "%d", &ival);
	      option_data->Abdiffs[option_data->num_Abdiffs-1][i] = ival - 1 ;
              if (ival <= 0 || ival > option_data->b)
                ANOVA_error("invalid B level in -Abdiff");
	      nopt++;
	    }

          /* and finally the label */
	  option_data->Abdname[option_data->num_Abdiffs-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->Abdname[option_data->num_Abdiffs-1], argv[nopt]);
	  nopt++;
	  continue;
	}

      /*-----   -abmean i j filename   -----*/
      if (strncmp(argv[nopt], "-abmean", 7) == 0)
	{
	  nopt++;
	  if (nopt + 2 >= argc)
            ANOVA_error ("need 3 arguments after -abmean");

	  option_data->num_abmeans++;
	  if (option_data->num_abmeans > MAX_MEANS)
	    ANOVA_error ("too many ab means");

          /* get A,B levels */
	  sscanf (argv[nopt], "%d", &ival);
	  option_data->abmeans[option_data->num_abmeans-1][0] = ival - 1 ;
          if (ival <= 0 || ival > option_data->a)
             ANOVA_error("invalid A level in -abmean");
	  nopt++;

	  sscanf (argv[nopt], "%d", &ival);
	  option_data->abmeans[option_data->num_abmeans-1][1] = ival - 1 ;
          if (ival <= 0 || ival > option_data->b)
             ANOVA_error("invalid B level in -abmean");
	  nopt++;

          /* and finally the label */
	  option_data->abmname[option_data->num_abmeans-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy(option_data->abmname[option_data->num_abmeans-1], argv[nopt]);
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

      /*----- -mask filename [11 Mar 2009: RWCox] -----*/
      if( strncmp(argv[nopt],"-mask",5) == 0 )
   {  
     THD_3dim_dataset *mset ; byte *amask ;
     nopt++ ;
     if( option_data->mask != NULL ) ANOVA_error("Can't have 2 -mask options");
     if( nopt >= argc )              ANOVA_error("need argument after -mask" );
     mset = THD_open_dataset( argv[nopt] ) ;
     CHECK_OPEN_ERROR(mset,argv[nopt]) ;
     DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
     amask = THD_makemask( mset , 0 , 1.0f , -1.0f ) ;
     if( amask == NULL ){
       WARNING_message("Can't create mask from dataset '%s'",argv[nopt]) ;
     } else { 
       int nmvox = THD_countmask(DSET_NVOX(mset),amask) ;
       if( nmvox < 1 ){
         WARNING_message("Mask from dataset '%s' is empty",argv[nopt]) ;
         free(amask) ;
       } else {
         INFO_message("Mask from dataset '%s' has %d voxels",argv[nopt],nmvox);
         option_data->mask  = amask ;        
         option_data->nmask = DSET_NVOX(mset) ;
       }
     }
     DSET_delete(mset) ; nopt++ ; continue ;
   }   



      /*----- unknown command -----*/
      sprintf (message,"Unrecognized command line option: %s\n", argv[nopt]);
      ANOVA_error (message);
    }


  /*----- check that all treatment sample sizes are equal -----*/
  option_data->n = N_INDEX(0, 0, 0);
  for (i = 0;  i < option_data->a;  i++)
    for (j = 0;  j < option_data->b;  j++)
      for (k = 0;  k < option_data->c;  k++)
	if (N_INDEX(i, j, k) != option_data->n)
	  ANOVA_error ("must have equal sample sizes for 3dANOVA3");

  /*----- checks on -old_method -----*/
  if (option_data->old_method)
  {
    if (option_data->model != 4 && option_data->model != 5 )
      ANOVA_error("currently, -old_method applies to model types 4 or 5, only");
    if (option_data->old_method == 1 )
      ANOVA_error("-old_method is insufficient by itself");
  }

  free(n);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check whether temporary files already exist.
*/

void check_temporary_files (anova_options * option_data)
{

  check_one_temporary_file ("ss0");
  check_one_temporary_file ("ssi");
  check_one_temporary_file ("ssj");
  check_one_temporary_file ("ssk");
  check_one_temporary_file ("ssij");
  check_one_temporary_file ("ssik");
  check_one_temporary_file ("ssjk");
  check_one_temporary_file ("ssijk");
  check_one_temporary_file ("ssijkm");

  check_one_temporary_file ("ssto");
  check_one_temporary_file ("sse");
  check_one_temporary_file ("ssa");
  check_one_temporary_file ("ssb");
  check_one_temporary_file ("ssc");
  check_one_temporary_file ("ssab");
  check_one_temporary_file ("ssac");
  check_one_temporary_file ("ssbc");
  check_one_temporary_file ("ssabc");
  check_one_temporary_file ("ssca");
  check_one_temporary_file ("ssbca");

}


/*---------------------------------------------------------------------------*/
/*
  Routine to check whether output files already exist.
*/

void check_output_files (anova_options * option_data)
{
  int i;       /* index */

  if (option_data->nfa > 0)
    check_one_output_file (option_data, option_data->faname);

  if (option_data->nfb > 0)
    check_one_output_file (option_data, option_data->fbname);

  if (option_data->nfc > 0)
    check_one_output_file (option_data, option_data->fcname);

  if (option_data->nfab > 0)
    check_one_output_file (option_data, option_data->fabname);

  if (option_data->nfac > 0)
    check_one_output_file (option_data, option_data->facname);

 if (option_data->nfbc > 0)
    check_one_output_file (option_data, option_data->fbcname);

 if (option_data->nfabc > 0)
    check_one_output_file (option_data, option_data->fabcname);

  if (option_data->num_ameans > 0)
    for (i = 0;  i < option_data->num_ameans;  i++)
      check_one_output_file (option_data, option_data->amname[i]);

  if (option_data->num_bmeans > 0)
    for (i = 0;  i < option_data->num_bmeans;  i++)
      check_one_output_file (option_data, option_data->bmname[i]);

  if (option_data->num_cmeans > 0)
    for (i = 0;  i < option_data->num_cmeans;  i++)
      check_one_output_file (option_data, option_data->cmname[i]);

  if (option_data->num_xmeans > 0)
    for (i = 0;  i < option_data->num_xmeans;  i++)
      check_one_output_file (option_data, option_data->xmname[i]);

  if (option_data->num_adiffs > 0)
    for (i = 0;  i < option_data->num_adiffs;  i++)
      check_one_output_file (option_data, option_data->adname[i]);

  if (option_data->num_bdiffs > 0)
    for (i = 0;  i < option_data->num_bdiffs;  i++)
      check_one_output_file (option_data, option_data->bdname[i]);

  if (option_data->num_cdiffs > 0)
    for (i = 0;  i < option_data->num_cdiffs;  i++)
      check_one_output_file (option_data, option_data->cdname[i]);

  if (option_data->num_xdiffs > 0)
    for (i = 0;  i < option_data->num_xdiffs;  i++)
      check_one_output_file (option_data, option_data->xdname[i]);

  if (option_data->num_acontr > 0)
    for (i = 0;  i < option_data->num_acontr;  i++)
      check_one_output_file (option_data, option_data->acname[i]);

  if (option_data->num_bcontr > 0)
    for (i = 0;  i < option_data->num_bcontr;  i++)
      check_one_output_file (option_data, option_data->bcname[i]);

  if (option_data->num_ccontr > 0)
    for (i = 0;  i < option_data->num_ccontr;  i++)
      check_one_output_file (option_data, option_data->ccname[i]);

  if (option_data->num_aBcontr > 0)
    for (i = 0;  i < option_data->num_aBcontr;  i++)
      check_one_output_file (option_data, option_data->aBcname[i]);

  if (option_data->num_Abcontr > 0)
    for (i = 0;  i < option_data->num_Abcontr;  i++)
      check_one_output_file (option_data, option_data->Abcname[i]);

  if (option_data->num_aBdiffs > 0)
    for (i = 0;  i < option_data->num_aBdiffs;  i++)
      check_one_output_file (option_data, option_data->aBdname[i]);

  if (option_data->num_Abdiffs > 0)
    for (i = 0;  i < option_data->num_Abdiffs;  i++)
      check_one_output_file (option_data, option_data->Abdname[i]);

  if (option_data->num_abmeans > 0)
    for (i = 0;  i < option_data->num_abmeans;  i++)
      check_one_output_file (option_data, option_data->abmname[i]);

  if (option_data->bucket_filename != NULL)
    check_one_output_file (option_data, option_data->bucket_filename);

}


/*---------------------------------------------------------------------------*/
/*
  Routine to check for valid inputs.
*/

void check_for_valid_inputs (anova_options * option_data)
{
  int n;

  /*----- check for valid inputs -----*/
  if (option_data->a < 2)
    ANOVA_error ("must specify number of factor A levels (a>1) ");
  if (option_data->b < 2)
    ANOVA_error ("must specify number of factor B levels (b>1) ");
  if (option_data->c < 2)
    ANOVA_error ("must specify number of factor C levels (c>1) ");
  if (option_data->n < 1)  ANOVA_error ("sample size is too small");

  n = option_data->n;

  switch (option_data->model)
    {
    case 1:
      if (n == 1)  ANOVA_error ("sample size is too small for Model 1");
      if (option_data->num_aBcontr > 0)
	ANOVA_error ("-aBcontr not allowed for Model 1");
      if (option_data->num_Abcontr > 0)
	ANOVA_error ("-Abcontr not allowed for Model 1");
      if (option_data->num_aBdiffs > 0)
	ANOVA_error ("-aBdiffs not allowed for Model 1");
      if (option_data->num_Abdiffs > 0)
	ANOVA_error ("-Abdiffs not allowed for Model 1");
      if (option_data->num_abmeans > 0)
	ANOVA_error ("-abmeans not allowed for Model 1");
      break;
    case 2:
      if (option_data->nfa > 0)
	ANOVA_error ("cannot calculate F(A) for Model 2");
      if (option_data->nfb > 0)
	ANOVA_error ("cannot calculate F(B) for Model 2");
      if (option_data->nfc > 0)
	ANOVA_error ("cannot calculate F(C) for Model 2");
      if ((option_data->nfabc > 0) && (n == 1))
	ANOVA_error ("sample size is too small for calculating F(ABC)");
      if (option_data->num_ameans > 0)
	ANOVA_error ("-amean not allowed for Model 2");
      if (option_data->num_bmeans > 0)
	ANOVA_error ("-bmean not allowed for Model 2");
      if (option_data->num_cmeans > 0)
	ANOVA_error ("-cmean not allowed for Model 2");
      if (option_data->num_xmeans > 0)
	ANOVA_error ("-xmean not allowed for Model 2");
      if (option_data->num_adiffs > 0)
	ANOVA_error ("-adiff not allowed for Model 2");
      if (option_data->num_bdiffs > 0)
	ANOVA_error ("-bdiff not allowed for Model 2");
      if (option_data->num_cdiffs > 0)
	ANOVA_error ("-cdiff not allowed for Model 2");
      if (option_data->num_xdiffs > 0)
	ANOVA_error ("-xdiff not allowed for Model 2");
      if (option_data->num_acontr > 0)
	ANOVA_error ("-acontr not allowed for Model 2");
      if (option_data->num_bcontr > 0)
	ANOVA_error ("-bcontr not allowed for Model 2");
      if (option_data->num_ccontr > 0)
	ANOVA_error ("-ccontr not allowed for Model 2");
      if (option_data->num_aBcontr > 0)
	ANOVA_error ("-aBcontr not allowed for Model 2");
      if (option_data->num_Abcontr > 0)
	ANOVA_error ("-Abcontr not allowed for Model 2");
      if (option_data->num_aBdiffs > 0)
	ANOVA_error ("-aBdiffs not allowed for Model 2");
      if (option_data->num_Abdiffs > 0)
	ANOVA_error ("-Abdiffs not allowed for Model 2");
      if (option_data->num_abmeans > 0)
	ANOVA_error ("-abmeans not allowed for Model 2");
      break;
    case 3:
      if (option_data->nfa > 0)
	ANOVA_error ("cannot calculate F(A) for Model 3");
      if ((option_data->nfbc > 0) && (n == 1))
	ANOVA_error ("sample size is too small for calculating F(BC)");
      if ((option_data->nfabc > 0) && (n == 1))
	ANOVA_error ("sample size is too small for calculating F(ABC)");
      if (option_data->num_ameans > 0)
	ANOVA_error ("-amean not allowed for Model 3");
      if (option_data->num_bmeans > 0)
	ANOVA_error ("-bmean not allowed for Model 3");
      if (option_data->num_cmeans > 0)
	ANOVA_error ("-cmean not allowed for Model 3");
      if (option_data->num_xmeans > 0)
	ANOVA_error ("-xmean not allowed for Model 3");
      if (option_data->num_adiffs > 0)
	ANOVA_error ("-adiff not allowed for Model 3");
      if (option_data->num_bdiffs > 0)
	ANOVA_error ("-bdiff not allowed for Model 3");
      if (option_data->num_cdiffs > 0)
	ANOVA_error ("-cdiff not allowed for Model 3");
      if (option_data->num_xdiffs > 0)
	ANOVA_error ("-xdiff not allowed for Model 3");
      if (option_data->num_acontr > 0)
	ANOVA_error ("-acontr not allowed for Model 3");
      if (option_data->num_bcontr > 0)
	ANOVA_error ("-bcontr not allowed for Model 3");
      if (option_data->num_ccontr > 0)
	ANOVA_error ("-ccontr not allowed for Model 3");
      if (option_data->num_aBcontr > 0)
	ANOVA_error ("-aBcontr not allowed for Model 3");
      if (option_data->num_Abcontr > 0)
	ANOVA_error ("-Abcontr not allowed for Model 3");
      if (option_data->num_aBdiffs > 0)
	ANOVA_error ("-aBdiffs not allowed for Model 3");
      if (option_data->num_Abdiffs > 0)
	ANOVA_error ("-Abdiffs not allowed for Model 3");
      if (option_data->num_abmeans > 0)
	ANOVA_error ("-abmeans not allowed for Model 3");
      break;
    case 4:
      if ((option_data->nfc > 0) && (n == 1))
	ANOVA_error ("sample size is too small for calculating F(C)");
      if ((option_data->nfac > 0) && (n == 1))
	ANOVA_error ("sample size is too small for calculating F(AC)");
      if ((option_data->nfbc > 0) && (n == 1))
	ANOVA_error ("sample size is too small for calculating F(BC)");
      if ((option_data->nfabc > 0) && (n == 1))
	ANOVA_error ("sample size is too small for calculating F(ABC)");
      if (option_data->num_cmeans > 0)
	ANOVA_error ("-cmean not allowed for Model 4");
      if (option_data->num_xmeans > 0)
	ANOVA_error ("-xmean not allowed for Model 4");
      if (option_data->num_cdiffs > 0)
	ANOVA_error ("-cdiff not allowed for Model 4");
      if (option_data->num_xdiffs > 0)
	ANOVA_error ("-xdiff not allowed for Model 4");
      if (option_data->num_ccontr > 0)
	ANOVA_error ("-ccontr not allowed for Model 4");
      break;
    case 5:
      if ((option_data->nfc > 0) && (n == 1))
	ANOVA_error ("sample size is too small for calculating F(C(A))");
      if ((option_data->nfbc > 0) && (n == 1))
	ANOVA_error ("sample size is too small for calculating F(BC(A))");
      if (option_data->nfac > 0)
	ANOVA_error ("F(AC) is meaningless for Model 5");
      if (option_data->nfabc > 0)
	ANOVA_error ("F(ABC) is meaningless for Model 5");
      if (option_data->num_cmeans > 0)
	ANOVA_error ("-cmean not allowed for Model 5");
      if (option_data->num_xmeans > 0)
	ANOVA_error ("-xmean not allowed for Model 5");
      if (option_data->num_cdiffs > 0)
	ANOVA_error ("-cdiff not allowed for Model 5");
      if (option_data->num_xdiffs > 0)
	ANOVA_error ("-xdiff not allowed for Model 5");
      if (option_data->num_ccontr > 0)
	ANOVA_error ("-ccontr not allowed for Model 5");
      break;
    }

  if (option_data->model == 4 || option_data->model == 5 )
  {
     /* check contrasts (show errors, and specify ANOVA-3) */
     if ( !contrasts_are_valid(option_data, 1, 3) )
        ANOVA_error("invalid contrast(s)");
  }
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the number of data files that have to be stored.
*/

int required_data_files (anova_options * option_data)
{
  int now;                         /* current number of disk files */
  int nout;                        /* number of output files */
  int nmax;                        /* maximum number of disk files */


  /*----- maximum and current number of temporary data files -----*/
  if (option_data->model != 5)
    {
      nmax = 10;
      now = 8;
    }
  else
    {
      nmax = 8;
      now = 6;
    }

  if (option_data->n == 1)
    {
      nmax -= 1;
      now -= 1;
    }


  /*----- space for output files -----*/
  nout = option_data->nfa + option_data->nfb + option_data->nfc
    + option_data->nfab + option_data->nfac + option_data->nfbc
    + option_data->nfabc
    + option_data->num_ameans  + option_data->num_bmeans
    + option_data->num_cmeans  + option_data->num_xmeans
    + option_data->num_adiffs  + option_data->num_bdiffs
    + option_data->num_cdiffs  + option_data->num_xdiffs
    + option_data->num_acontr  + option_data->num_bcontr
    + option_data->num_ccontr  +
    + option_data->num_aBcontr + option_data->num_Abcontr
    + option_data->num_aBdiffs + option_data->num_Abdiffs
    + option_data->num_abmeans;

  now = now + nout;

  nmax = max (now, nmax);

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
  int a, b, c;                         /* number of factor levels */
  int n;                               /* number of observations per cell */


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

  /*----- initialize local variables  -----*/
  a = (*option_data)->a;
  b = (*option_data)->b;
  c = (*option_data)->c;
  n = (*option_data)->n;

  /*----- total number of observations -----*/
  (*option_data)->nt = n * a * b * c;
  printf ("Number of input datasets = %d \n", (*option_data)->nt);

  /*----- check for valid inputs -----*/
  check_for_valid_inputs (*option_data);

  /*----- check whether temporary files already exist -----*/
  check_temporary_files (*option_data);

  /*----- check whether output files already exist -----*/
  if( THD_deathcon() ) check_output_files (*option_data);

  /*----- check whether there is sufficient disk space -----*/
  if ((*option_data)->diskspace)  check_disk_space (*option_data);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to sum over the specified set of observations.
  The output is returned in ysum.
*/

void calculate_sum (anova_options * option_data,
		    int ii, int jj, int kk, float * ysum)
{
  float * y = NULL;                /* pointer to input data */
  int i, itop, ibot;               /* factor A level index */
  int j, jtop, jbot;               /* factor B level index */
  int k, ktop, kbot;               /* factor C level index */
  int m;                           /* observation number index */
  int a;                           /* number of levels for factor A */
  int b;                           /* number of levels for factor B */
  int c;                           /* number of levels for factor C */
  int n;                           /* number of observations per cell */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */
  char sum_label[MAX_NAME_LENGTH]; /* name of sum for print to screen */
  char str[MAX_NAME_LENGTH];       /* temporary string */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  y = (float *) malloc(sizeof(float)*nxyz);
  if (y == NULL)  ANOVA_error ("unable to allocate sufficient memory");


  /*-----  set up summation limits -----*/
  if (ii < 0)
    { ibot = 0;   itop = a; }
  else
    { ibot = ii;  itop = ii+1; }

  if (jj < 0)
    { jbot = 0;   jtop = b; }
  else
    { jbot = jj;  jtop = jj+1; }

  if (kk < 0)
    { kbot = 0;   ktop = c; }
  else
    { kbot = kk;  ktop = kk+1; }


  volume_zero (ysum, nxyz);

  /*-----  loop over levels of factor A  -----*/
  for (i = ibot;  i < itop;  i++)
    {
      /*-----  loop over levels of factor B  -----*/
      for (j = jbot;  j < jtop;  j++)
	{
	  /*-----  loop over levels of factor C  -----*/
	  for (k = kbot;  k < ktop;  k++)
	    {
	      /*----- sum observations within this cell -----*/	
	      for (m = 0;  m < n;  m++)
		{
		  read_afni_data (option_data,
				  option_data->xname[i][j][k][m], y);
		  if (nvoxel > 0)
		    printf ("y[%d][%d][%d][%d] = %f \n",
			    i+1, j+1, k+1, m+1, y[nvoxel-1]);
		  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
		    ysum[ixyz] += y[ixyz];
		} /* m */
	    }  /* k */
	}  /* j */
    }  /* i */


  /*----- print the sum for this cell -----*/
  if (nvoxel > 0)
    {
      strcpy (sum_label, "y");
      if (ii < 0)
	strcat (sum_label, "[.]");
      else
	{
	  sprintf (str, "[%d]", ii+1);
	  strcat (sum_label, str);
	}
      if (jj < 0)
	strcat (sum_label, "[.]");
      else
	{
	  sprintf (str, "[%d]", jj+1);
	  strcat (sum_label, str);
	}
      if (kk < 0)
	strcat (sum_label, "[.]");
      else
	{
	  sprintf (str, "[%d]", kk+1);
	  strcat (sum_label, str);
	}
      printf ("%s[.] = %f \n", sum_label, ysum[nvoxel-1]);
    }

  /*----- release memory -----*/
  free (y);     y = NULL;

}


/*---------------------------------------------------------------------------*/
/*                                                    8 Sep 2005 [rickr,gangc]
  routine to compute the mean and t-stat for a type 4 A contrast

  In the case of an A contrast, the mean is computed across B and any
  multiple observations, first.  The contrast is then computed over the
  result.  So this 4-D data is reduced to 2-D, with 1 for the contrast,
  and one for the sum and sum of squares.

      mean = sum_over_k[ (sum_over_i_in_contrast[c_i * y_i.k.])   ] / K
      sum2 = sum_over_k[ (sum_over_i_in_contrast[c_i * y_i.k.])^2 ]

      t = mean * sqrt[     df ( df + 1 )      ]
                     [ ---------------------- ]
                     [ sum_sq - (df+1)*mean^2 ]

  Note that y_i.j. is the mean over B and any multiple observations.
  Note also that b may be 1 if bindex is in [0,B-1], else B is equal
  to #blevels.

  For accuracy, sums are computed using doubles, then copied to float.
  The contrast is passed in to allow for more uses of this function.
*/
void calc_type4_acontr(anova_options *option_data, float *acontr,
                       int bindex, int df, float *mean, float * tmean)
{
  const float EPSILON = 10e-15;    /* to prevent division by zero */
  double * dsum, * dsum2;          /* cumulative sums, for accuracy */
  double * dcontr;                 /* current contrast sum */
  double   dval, dmean;            /* for typing ease */
  int i, k;                        /* indices for levels of factors A and C */
  int a, b, c;                     /* number of levels factors A, B and C */
  int n, df_prod;                  /* number of observations per cell */
  int nvals;                       /* B levels used times n (n or n*b) */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */

  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  if (bindex < 0) nvals = n*b;  /* collapse across B and n */
  else            nvals = n;    /* collapse across n, at the given B level */

  /*----- allocate memory space for calculations -----*/
  dsum = (double *) malloc(sizeof(double)*nxyz);
  dsum2 = (double *) malloc(sizeof(double)*nxyz);
  dcontr = (double *) malloc(sizeof(double)*nxyz);
  if (dsum == NULL || dsum2 == NULL || dcontr == NULL)
      ANOVA_error ("calc_type4_acontr: unable to allocate sufficient memory");

  for (ixyz = 0; ixyz < nxyz; ixyz++)  /* init sums to zero */
      dsum[ixyz] = dsum2[ixyz] = 0.0;

  /*-----  loop over factor C levels -----*/
  for ( k = 0; k < c; k++ )
  {
      for (ixyz = 0; ixyz < nxyz; ixyz++)
          dcontr[ixyz] = 0.0;

      /*-----  compute contrast for the current j -----*/
      for (i = 0;  i < a;  i++)
      {
          if (acontr[i] == 0.0 ) continue;  /* then skip this index */

          /* get sum over n (and B?) for mean (cheat, using mean for memory) */
          calculate_sum(option_data, i, bindex, k, mean);
          for (ixyz = 0; ixyz < nxyz; ixyz++)
              dcontr[ixyz] += acontr[i] * (double)mean[ixyz] / nvals;
      }

      /*-----  tally sum of contrast and squares for the current k -----*/
      for (ixyz = 0; ixyz < nxyz; ixyz++)
      {
          dsum[ixyz] += dcontr[ixyz];
          dsum2[ixyz] += dcontr[ixyz] * dcontr[ixyz];
      }
  }

  /*----- compute results -----*/
  df_prod = df * (df + 1);
  for (ixyz = 0; ixyz < nxyz; ixyz++)
  {
      dmean = dsum[ixyz] / c;    /* divide by k for the mean */
      mean[ixyz] = dmean;        /* copy the result to float output */

      dval = dsum2[ixyz] - (df+1.0) * dmean * dmean;
      if (dval < EPSILON) tmean[ixyz] = 0.0;
      else                tmean[ixyz] = dmean * sqrt( df_prod / dval );
  }

  /*----- release memory -----*/
  free (dsum);  free (dsum2);  free (dcontr);
}


/*---------------------------------------------------------------------------*/
/*                                                   20 Sep 2005 [rickr,gangc]
  routine to compute the mean and tstat of a type-5 A contrast

  t      = L / sqrt(1/c * SL2 * sum_c2)
  L      = sum_i(contr[i] * Y_i...)
  SL     = 1/w(c-1) * sum_i[ step(|contr[i]|) * sum_k(Y_i.k. - Y_i...)^2 ]
  sum_c2 = sum_i(contr[i]^2)

  note: Y_i.k. or Y_i... is considered the mean Y here

  For accuracy, sums are computed using doubles, then copied to float.
  The contrast is passed in to allow for more uses of this function.

  If blevel is not -1, then results are computed for only that level.
*/
void calc_type5_acontr(anova_options * option_data, float * contr, int blevel,
                       float * mean, float * tmean )
{
  const float EPSILON = 1.0e-15;   /* protect against divide by zero */
  double * L, * SL;                /* final mean, and S_L^2 values */
  double * S1, * S2;               /* cumulative sums, for accuracy */
  double   dval;                   /* just for slight, useless speed gain */
  int i, k;                        /* indices for levels of factors A and C */
  int a, b, c;                     /* number of levels factors A, B and C */
  int n;                           /* number of observations per cell */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */

  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  if (blevel >= 0) b = 1;

  /*----- allocate memory space for calculations -----*/
  L  = (double *) malloc(sizeof(double)*nxyz);
  SL = (double *) malloc(sizeof(double)*nxyz);
  S1 = (double *) malloc(sizeof(double)*nxyz);
  S2 = (double *) malloc(sizeof(double)*nxyz);
  if (!L || !SL || !S1 || !S2)
      ANOVA_error ("calc_type5_acontr: unable to allocate sufficient memory");

  for (ixyz = 0; ixyz < nxyz; ixyz++)   /* init cumulative L and SL sums */
      L[ixyz] = SL[ixyz] = 0.0;

  /*-----  loop over factor C levels -----*/
  for ( i = 0; i < a; i++ )
  {
      if (contr[i] == 0.0 ) continue;  /* then skip this index */

      for (ixyz = 0; ixyz < nxyz; ixyz++)
          S1[ixyz] = S2[ixyz] = 0.0;

      /*-----  compute contrast for the current j -----*/
      for (k = 0;  k < c;  k++)
      {
          /* get mean over n and B (cheat, using input mean for memory) */
          calculate_sum(option_data, i, blevel, k, mean);
          for (ixyz = 0; ixyz < nxyz; ixyz++)
          {
              dval = mean[ixyz] / (b*n);  /* dislike repeated indexing */
              mean[ixyz] = dval;
              S1[ixyz] += dval;
              S2[ixyz] += dval * dval;
          }

      }

      /*-----  tally sum of contrast and squares for the current k -----*/
      for (ixyz = 0; ixyz < nxyz; ixyz++)
      {
          S1[ixyz] /= c;                         /* this is mean Y_i... */
          L [ixyz] += contr[i]*S1[ixyz];
          SL[ixyz] += S2[ixyz] - c * S1[ixyz] * S1[ixyz];
      }
  }

  /*----- compute count and sum of squared contrast elements -----*/
  for (i = 0, n = 0, dval = 0.0; i < a; i++)
      if (contr[i])
      {
          n++;
          dval += contr[i] * contr[i];
      }
  dval = sqrt(c/dval);  /* for computing t = L*dval/sqrt(SL) */

  /*----- copy final results to float output -----*/
  for (ixyz = 0; ixyz < nxyz; ixyz++)
  {
      SL[ixyz] = (n == 0) ? 0.0 : SL[ixyz] / (n * (c-1));
      mean [ixyz] = L[ixyz];
      tmean[ixyz] = (SL[ixyz] < EPSILON) ? 0.0 : L[ixyz]*dval/sqrt(SL[ixyz]);
  }

  /*----- fly, be free! -----*/
  free(L);  free(SL);  free (S1);  free (S2);
}


/*---------------------------------------------------------------------------*/
/*                                                   23 Sep 2005 [rickr,gangc]
  routine to compute the mean and tstat of a type-5 B contrast

  t  = L / sqrt(1/ac * SL )
  L  = sum_j(contr[j] * Y_.j..)
  SL = 1/a(c-1) * sum_i[ sum_k[ (L_ik - L_i)^2 ] ]
     = 1/a(c-1) * sum_i[ sum_k[ L_ik^2 ] - c * L_i^2 ]

  note: Y_ijk., etc. are mean values here

  For accuracy, sums are computed using doubles, then copied to float.
  The contrast is passed in to allow for more uses of this function.

  If alevel is not -1, then results are computed for only that level.
*/
void calc_type5_bcontr(anova_options * option_data, float * contr, int alevel,
                       float * mean, float * tmean )
{
  const float EPSILON = 1.0e-15;   /* protect against divide by zero        */
  double * L, * SL;                /* final L, S_L^2 values                 */
  double * S1, * S2, * csum;       /* L_i, sum(L_ik^2), and contrast sum    */
  double   dval, sl_val;           /* just for slight, useless speed gain   */
  int i, j, k;                     /* indices for levels of factors A and C */
  int a, b, c;                     /* number of levels factors A, B and C   */
  int ibot, itop;                  /* loop bounds, in case A is fixed       */
  int n;                           /* number of observations per cell       */
  int ixyz, nxyz;                  /* voxel counters                        */
  int nvoxel;                      /* output voxel #                        */

  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  if (alevel >= 0)
  {
      a = 1;
      ibot = itop = alevel;
  }
  else
  {
      ibot = 0;
      itop = a-1;
  }

  /*----- allocate memory space for calculations -----*/
  L    = (double *) malloc(sizeof(double)*nxyz);  /* L                     */
  SL   = (double *) malloc(sizeof(double)*nxyz);  /* sum_i(S2 - c * S1^2)  */
  S1   = (double *) malloc(sizeof(double)*nxyz);  /* L_i                   */
  S2   = (double *) malloc(sizeof(double)*nxyz);  /* sum_k( L_ik ^2 )      */
  csum = (double *) malloc(sizeof(double)*nxyz);  /* sum_j( c_j * Y_ijk. ) */
  if (!L || !SL || !S1 || !S2 || !csum)
      ANOVA_error ("calc_type5_bcontr: unable to allocate sufficient memory");

  for (ixyz = 0; ixyz < nxyz; ixyz++)   /* init cumulative L and SL sums */
      L[ixyz] = SL[ixyz] = 0.0;

  /*-----  loop over factor A levels -----*/
  for ( i = ibot; i <= itop; i++ )
  {
      for (ixyz = 0; ixyz < nxyz; ixyz++)
          S1[ixyz] = S2[ixyz] = 0.0;

      /*-----  loop over factor C levels -----*/
      for (k = 0; k < c; k++)
      {
          /*-----  compute contrast sum (csum) for the current i, k -----*/
          for (ixyz = 0; ixyz < nxyz; ixyz++) csum[ixyz] = 0.0;
          for (j = 0; j < b; j++)
          {
              if( contr[j] == 0.0 ) continue;
              calculate_sum(option_data, i, j, k, mean); /* memory from mean */
              for (ixyz = 0; ixyz < nxyz; ixyz++)
                  csum[ixyz] += (double)contr[j] * mean[ixyz] / n;
          }

          /*-----  tally sum and sum of squared contrasts -----*/
          for (ixyz = 0; ixyz < nxyz; ixyz++)
          {
              S1[ixyz] += csum[ixyz];
              S2[ixyz] += csum[ixyz] * csum[ixyz];
          }

      }

      /*-----  tally sum of contrast and squares for the current k -----*/
      for (ixyz = 0; ixyz < nxyz; ixyz++)
      {
          dval = S1[ixyz] / c;   /* L_i */
          L [ixyz] += dval;
          SL[ixyz] += S2[ixyz] - c * dval * dval;
      }
  }

  /*----- copy final results to float output -----*/
  dval = sqrt(1.0/(a*c));
  for (ixyz = 0; ixyz < nxyz; ixyz++)
  {
      L[ixyz]     /= a;            /* mean L_i */
      sl_val       = SL[ixyz] / (a * (c-1));
      mean [ixyz]  = L[ixyz];
      tmean[ixyz]  = (sl_val < EPSILON) ? 0.0 : L[ixyz] / (sqrt(sl_val)*dval);
  }

  /*----- fly, be free! -----*/
  free(L);  free(SL);  free(S1);  free(S2);  free(csum);
}


/*---------------------------------------------------------------------------*/
/*                                                    8 Sep 2005 [rickr,gangc]
  routine to compute the mean and t-stat for a type 4 B contrast

  In the case of a B contrast, the mean is computed across A and any
  multiple observations, first.  The contrast is then computed over the
  result.  So this 4-D data is reduced to 2-D, with 1 for the contrast,
  and one for the sum and sum of squares.

      mean = sum_over_k[ (sum_over_i_in_contrast[c_j * y_.jk.])   ] / K
      sum2 = sum_over_k[ (sum_over_i_in_contrast[c_j * y_.jk.])^2 ]

      t = mean * sqrt[     df ( df + 1 )      ]
                     [ ---------------------- ]
                     [ sum_sq - (df+1)*mean^2 ]

  Note that y_.jk. is the mean over A and any multiple observations.
  Note also that a may be 1 if aindex is in [0,A-1], else a is equal
  to #alevels.

  For accuracy, sums are computed using doubles, then copied to float.
  The contrast is passed in to allow for more uses of this function.
*/
void calc_type4_bcontr(anova_options *option_data, float *acontr,
                       int aindex, int df, float *mean, float * tmean)
{
  const float  EPSILON = 1.0e-15;  /* protect against divide by zero */
  double dval, dmean;              /* for typing ease */
  double * dsum, * dsum2;          /* cumulative sums, for accuracy */
  double * dcontr;                 /* current contrast sum */
  int j, k;                        /* indices for levels of factors B and C */
  int a, b, c;                     /* number of levels factors A, B and C */
  int n, df_prod;                  /* number of observations per cell */
  int nvals;                       /* A levels used times n (n or n*a) */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */

  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  if (aindex < 0) nvals = n*a;  /* collapse across A and n */
  else            nvals = n;    /* collapse across n, at the given A level */

  /*----- allocate memory space for calculations -----*/
  dsum = (double *) malloc(sizeof(double)*nxyz);
  dsum2 = (double *) malloc(sizeof(double)*nxyz);
  dcontr = (double *) malloc(sizeof(double)*nxyz);
  if (dsum == NULL || dsum2 == NULL || dcontr == NULL)
      ANOVA_error ("calc_type4_bcontr: unable to allocate sufficient memory");

  for (ixyz = 0; ixyz < nxyz; ixyz++)  /* init sums to zero */
      dsum[ixyz] = dsum2[ixyz] = 0.0;

  /*-----  loop over factor C levels -----*/
  for ( k = 0; k < c; k++ )
  {
      for (ixyz = 0; ixyz < nxyz; ixyz++)
          dcontr[ixyz] = 0.0;

      /*-----  compute contrast for the current j -----*/
      for (j = 0;  j < b;  j++)
      {
          if (acontr[j] == 0.0 ) continue;  /* then skip this index */

          /* get sum over n (and A?) for mean (cheat, using mean for memory) */
          calculate_sum(option_data, aindex, j, k, mean);
          for (ixyz = 0; ixyz < nxyz; ixyz++)
              dcontr[ixyz] += acontr[j] * (double)mean[ixyz] / nvals;
      }

      /*-----  tally sum of contrast and squares for the current k -----*/
      for (ixyz = 0; ixyz < nxyz; ixyz++)
      {
          dsum[ixyz] += dcontr[ixyz];
          dsum2[ixyz] += dcontr[ixyz] * dcontr[ixyz];
      }
  }

  /*----- compute results -----*/
  df_prod = df * (df + 1);
  for (ixyz = 0; ixyz < nxyz; ixyz++)
  {
      dmean = dsum[ixyz] / c;    /* divide by k for the mean */
      mean[ixyz] = dmean;        /* copy result to float output */

      dval = dsum2[ixyz] - (df+1.0) * dmean * dmean;
      if (dval < EPSILON) tmean[ixyz] = 0.0;
      else                tmean[ixyz] = dmean * sqrt( df_prod / dval );
  }

  /*----- release memory -----*/
  free (dsum);  free (dsum2);  free (dcontr);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate SS0.  Result is stored in temporary output file
  ss0.3danova3.
*/

void calculate_ss0 (anova_options * option_data)
{
  float * ss0 = NULL;              /* pointer to output data */
  float * ysum = NULL;             /* pointer to sum over all observations */
  int a;                           /* number of levels for factor A */
  int b;                           /* number of levels for factor B */
  int c;                           /* number of levels for factor C */
  int n;                           /* number of observations per cell */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */
  int nval;                        /* divisor of sum */
  char filename[MAX_NAME_LENGTH];  /* name of output file */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;
  nval = a * b * c * n;

  /*----- allocate memory space for calculations -----*/
  ss0 = (float *) malloc(sizeof(float)*nxyz);
  ysum = (float *) malloc(sizeof(float)*nxyz);
  if ((ss0 == NULL) || (ysum == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  /*----- sum over all observations -----*/
  calculate_sum (option_data, -1, -1, -1, ysum);

  /*----- calculate ss0 -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ss0[ixyz] = ysum[ixyz] * ysum[ixyz] / nval;


  /*----- save the sum -----*/
  if (nvoxel > 0)
    printf ("SS0 = %f \n", ss0[nvoxel-1]);
  strcpy (filename, "ss0");
  volume_write (filename, ss0, nxyz);


  /*----- release memory -----*/
  free (ysum);    ysum = NULL;
  free (ss0);     ss0 = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate SSI.  Result is stored in temporary output file
  ssi.3danova3.
*/

void calculate_ssi (anova_options * option_data)
{
  float * ssi = NULL;              /* pointer to output data */
  float * ysum = NULL;             /* pointer to sum over observations */
  int a;                           /* number of levels for factor A */
  int b;                           /* number of levels for factor B */
  int c;                           /* number of levels for factor C */
  int n;                           /* number of observations per cell */
  int i;                           /* index for factor A levels */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */
  int nval;                        /* divisor of sum */
  char filename[MAX_NAME_LENGTH];  /* name of output file */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;
  nval = b * c * n;

  /*----- allocate memory space for calculations -----*/
  ssi = (float *) malloc(sizeof(float)*nxyz);
  ysum = (float *) malloc(sizeof(float)*nxyz);
  if ((ssi == NULL) || (ysum == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  volume_zero (ssi, nxyz);

  /*----- loop over levels of factor A -----*/
  for (i = 0;  i < a;  i++)
    {
      /*----- sum over observations -----*/
      calculate_sum (option_data, i, -1, -1, ysum);

      /*----- add to ssi -----*/
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	ssi[ixyz] += ysum[ixyz] * ysum[ixyz] / nval;
    }

  /*----- save the sum -----*/
  if (nvoxel > 0)
    printf ("SSI = %f \n", ssi[nvoxel-1]);
  strcpy (filename, "ssi");
  volume_write (filename, ssi, nxyz);


  /*----- release memory -----*/
  free (ysum);    ysum = NULL;
  free (ssi);     ssi = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate SSJ.  Result is stored in temporary output file
  ssj.3danova3.
*/

void calculate_ssj (anova_options * option_data)
{
  float * ssj = NULL;              /* pointer to output data */
  float * ysum = NULL;             /* pointer to sum over observations */
  int a;                           /* number of levels for factor A */
  int b;                           /* number of levels for factor B */
  int c;                           /* number of levels for factor C */
  int n;                           /* number of observations per cell */
  int j;                           /* index for factor B levels */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */
  int nval;                        /* divisor of sum */
  char filename[MAX_NAME_LENGTH];  /* name of output file */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;
  nval = a * c * n;

  /*----- allocate memory space for calculations -----*/
  ssj = (float *) malloc(sizeof(float)*nxyz);
  ysum = (float *) malloc(sizeof(float)*nxyz);
  if ((ssj == NULL) || (ysum == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  volume_zero (ssj, nxyz);

  /*----- loop over levels of factor B -----*/
  for (j = 0;  j < b;  j++)
    {
      /*----- sum over observations -----*/
      calculate_sum (option_data, -1, j, -1, ysum);

      /*----- add to ssj -----*/
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	ssj[ixyz] += ysum[ixyz] * ysum[ixyz] / nval;
    }

  /*----- save the sum -----*/
  if (nvoxel > 0)
    printf ("SSJ = %f \n", ssj[nvoxel-1]);
  strcpy (filename, "ssj");
  volume_write (filename, ssj, nxyz);


  /*----- release memory -----*/
  free (ysum);    ysum = NULL;
  free (ssj);     ssj = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate SSK.  Result is stored in temporary output file
  ssk.3danova3.
*/

void calculate_ssk (anova_options * option_data)
{
  float * ssk = NULL;              /* pointer to output data */
  float * ysum = NULL;             /* pointer to sum over observations */
  int a;                           /* number of levels for factor A */
  int b;                           /* number of levels for factor B */
  int c;                           /* number of levels for factor C */
  int n;                           /* number of observations per cell */
  int k;                           /* index for factor C levels */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */
  int nval;                        /* divisor of sum */
  char filename[MAX_NAME_LENGTH];  /* name of output file */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;
  nval = a * b * n;

  /*----- allocate memory space for calculations -----*/
  ssk = (float *) malloc(sizeof(float)*nxyz);
  ysum = (float *) malloc(sizeof(float)*nxyz);
  if ((ssk == NULL) || (ysum == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  volume_zero (ssk, nxyz);

  /*----- loop over levels of factor C -----*/
  for (k = 0;  k < c;  k++)
    {
      /*----- sum over observations -----*/
      calculate_sum (option_data, -1, -1, k, ysum);

      /*----- add to ssk -----*/
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	ssk[ixyz] += ysum[ixyz] * ysum[ixyz] / nval;
    }

  /*----- save the sum -----*/
  if (nvoxel > 0)
    printf ("SSK = %f \n", ssk[nvoxel-1]);
  strcpy (filename, "ssk");
  volume_write (filename, ssk, nxyz);


  /*----- release memory -----*/
  free (ysum);    ysum = NULL;
  free (ssk);     ssk = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate SSIJ.  Result is stored in temporary output file
  ssij.3danova3.
*/

void calculate_ssij (anova_options * option_data)
{
  float * ssij = NULL;             /* pointer to output data */
  float * ysum = NULL;             /* pointer to sum over observations */
  int a;                           /* number of levels for factor A */
  int b;                           /* number of levels for factor B */
  int c;                           /* number of levels for factor C */
  int n;                           /* number of observations per cell */
  int i, j;                        /* indices for factor A and B levels */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */
  int nval;                        /* divisor of sum */
  char filename[MAX_NAME_LENGTH];  /* name of output file */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;
  nval = c * n;

  /*----- allocate memory space for calculations -----*/
  ssij = (float *) malloc(sizeof(float)*nxyz);
  ysum = (float *) malloc(sizeof(float)*nxyz);
  if ((ssij == NULL) || (ysum == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  volume_zero (ssij, nxyz);

  /*----- loop over levels of factor A -----*/
  for (i = 0;  i < a;  i++)
    {
      /*----- loop over levels of factor B -----*/
      for (j = 0;  j < b;  j++)
	{
	  /*----- sum over observations -----*/
	  calculate_sum (option_data, i, j, -1, ysum);
	
	  /*----- add to ssij -----*/
	  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	    ssij[ixyz] += ysum[ixyz] * ysum[ixyz] / nval;
	}
    }

  /*----- save the sum -----*/
  if (nvoxel > 0)
    printf ("SSIJ = %f \n", ssij[nvoxel-1]);
  strcpy (filename, "ssij");
  volume_write (filename, ssij, nxyz);


  /*----- release memory -----*/
  free (ysum);    ysum = NULL;
  free (ssij);    ssij = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate SSIK.  Result is stored in temporary output file
  ssik.3danova3.
*/

void calculate_ssik (anova_options * option_data)
{
  float * ssik = NULL;             /* pointer to output data */
  float * ysum = NULL;             /* pointer to sum over observations */
  int a;                           /* number of levels for factor A */
  int b;                           /* number of levels for factor B */
  int c;                           /* number of levels for factor C */
  int n;                           /* number of observations per cell */
  int i, k;                        /* indices for factor A and C levels */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */
  int nval;                        /* divisor of sum */
  char filename[MAX_NAME_LENGTH];  /* name of output file */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;
  nval = b * n;

  /*----- allocate memory space for calculations -----*/
  ssik = (float *) malloc(sizeof(float)*nxyz);
  ysum = (float *) malloc(sizeof(float)*nxyz);
  if ((ssik == NULL) || (ysum == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  volume_zero (ssik, nxyz);

  /*----- loop over levels of factor A -----*/
  for (i = 0;  i < a;  i++)
    {
      /*----- loop over levels of factor C -----*/
      for (k = 0;  k < c;  k++)
	{
	  /*----- sum over observations -----*/
	  calculate_sum (option_data, i, -1, k, ysum);
	
	  /*----- add to ssij -----*/
	  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	    ssik[ixyz] += ysum[ixyz] * ysum[ixyz] / nval;
	}
    }

  /*----- save the sum -----*/
  if (nvoxel > 0)
    printf ("SSIK = %f \n", ssik[nvoxel-1]);
  strcpy (filename, "ssik");
  volume_write (filename, ssik, nxyz);


  /*----- release memory -----*/
  free (ysum);    ysum = NULL;
  free (ssik);    ssik = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate SSJK.  Result is stored in temporary output file
  ssjk.3danova3.
*/

void calculate_ssjk (anova_options * option_data)
{
  float * ssjk = NULL;             /* pointer to output data */
  float * ysum = NULL;             /* pointer to sum over observations */
  int a;                           /* number of levels for factor A */
  int b;                           /* number of levels for factor B */
  int c;                           /* number of levels for factor C */
  int n;                           /* number of observations per cell */
  int j, k;                        /* indices for factor B and C levels */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */
  int nval;                        /* divisor of sum */
  char filename[MAX_NAME_LENGTH];  /* name of output file */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;
  nval = a * n;

  /*----- allocate memory space for calculations -----*/
  ssjk = (float *) malloc(sizeof(float)*nxyz);
  ysum = (float *) malloc(sizeof(float)*nxyz);
  if ((ssjk == NULL) || (ysum == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  volume_zero (ssjk, nxyz);

  /*----- loop over levels of factor B -----*/
  for (j = 0;  j < b;  j++)
    {
      /*----- loop over levels of factor C -----*/
      for (k = 0;  k < c;  k++)
	{
	  /*----- sum over observations -----*/
	  calculate_sum (option_data, -1, j, k, ysum);
	
	  /*----- add to ssjk -----*/
	  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	    ssjk[ixyz] += ysum[ixyz] * ysum[ixyz] / nval;
	}
    }

  /*----- save the sum -----*/
  if (nvoxel > 0)
    printf ("SSJK = %f \n", ssjk[nvoxel-1]);
  strcpy (filename, "ssjk");
  volume_write (filename, ssjk, nxyz);


  /*----- release memory -----*/
  free (ysum);    ysum = NULL;
  free (ssjk);    ssjk = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate SSIJK.  Result is stored in temporary output file
  ssijk.3danova3.
*/

void calculate_ssijk (anova_options * option_data)
{
  float * ssijk = NULL;            /* pointer to output data */
  float * ysum = NULL;             /* pointer to sum over observations */
  int a;                           /* number of levels for factor A */
  int b;                           /* number of levels for factor B */
  int c;                           /* number of levels for factor C */
  int n;                           /* number of observations per cell */
  int i, j, k;                     /* indices for factor levels */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */
  int nval;                        /* divisor of sum */
  char filename[MAX_NAME_LENGTH];  /* name of output file */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;
  nval = n;

  /*----- allocate memory space for calculations -----*/
  ssijk = (float *) malloc(sizeof(float)*nxyz);
  ysum = (float *) malloc(sizeof(float)*nxyz);
  if ((ssijk == NULL) || (ysum == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  volume_zero (ssijk, nxyz);

  /*----- loop over levels of factor A -----*/
  for (i = 0;  i < a;  i++)
    {
      /*----- loop over levels of factor B -----*/
      for (j = 0;  j < b;  j++)
	{
	  /*----- loop over levels of factor C -----*/
	  for (k = 0;  k < c;  k++)
	    {
	      /*----- sum over observations -----*/
	      if (n != 1)
		calculate_sum (option_data, i, j, k, ysum);
	      else
		{
		  read_afni_data (option_data,
				  option_data->xname[i][j][k][0], ysum);
		  if (nvoxel > 0)
		    printf ("y[%d][%d][%d][.] = %f \n",
			    i+1, j+1, k+1, ysum[nvoxel-1]);
		}
	
	      /*----- add to ssijk -----*/
	      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
		ssijk[ixyz] += ysum[ixyz] * ysum[ixyz] / nval;
	    }
	}
    }

  /*----- save the sum -----*/
  if (nvoxel > 0)
    printf ("SSIJK = %f \n", ssijk[nvoxel-1]);
  strcpy (filename, "ssijk");
  volume_write (filename, ssijk, nxyz);


  /*----- release memory -----*/
  free (ysum);    ysum = NULL;
  free (ssijk);   ssijk = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to sum the squares of all observations.
  The sum is stored (temporarily) in disk file ssijkm.3danova3.
*/

void calculate_ssijkm (anova_options * option_data)
{
  float * ssijkm = NULL;            /* pointer to output data */
  float * y = NULL;                 /* pointer to input data */
  int i;                            /* factor A level index */
  int j;                            /* factor B level index */
  int k;                            /* factor C level index */
  int m;                            /* observation number index */
  int a;                            /* number of levels for factor A */
  int b;                            /* number of levels for factor B */
  int c;                            /* number of levels for factor C */
  int n;                            /* number of observations per cell */
  int ixyz, nxyz;                   /* voxel counters */
  int nvoxel;                       /* output voxel # */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  ssijkm = (float *) malloc(sizeof(float)*nxyz);
  y = (float *) malloc(sizeof(float)*nxyz);
  if ((ssijkm == NULL) || (y == NULL))
    ANOVA_error ("unable to allocate sufficient memory");


  volume_zero (ssijkm, nxyz);

  for (i = 0;  i < a;  i++)
    {
      for (j = 0;  j < b;  j++)
	{
	  for (k = 0;  k < c;  k++)
	    {
	      for (m = 0;  m < n;  m++)
		{
		  read_afni_data (option_data,
				  option_data->xname[i][j][k][m], y);
		  if (nvoxel > 0)
		    printf ("y[%d][%d][%d][%d] = %f \n",
			    i+1, j+1, k+1, m+1, y[nvoxel-1]);
	
		  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
		      ssijkm[ixyz] += y[ixyz] * y[ixyz];
		}
	    }
	}
    }


  /*----- save the sum -----*/
  if (nvoxel > 0)
    printf ("SSIJKM = %f \n", ssijkm[nvoxel-1]);
  volume_write ("ssijkm", ssijkm, nxyz);

  /*----- release memory -----*/
  free (y);       y = NULL;
  free (ssijkm);  ssijkm = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the total sum of squares (SSTO).
  The output is stored (temporarily) in file ssto.3danova3.
*/

void calculate_ssto (anova_options * option_data)
{
  float * y = NULL;                   /* input data pointer */
  float * ssto = NULL;                /* ssto data pointer */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */


  /*----- assign local variables -----*/
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  ssto = (float *) malloc (sizeof(float)*nxyz);
  y = (float *) malloc (sizeof(float)*nxyz);
  if ((y == NULL) || (ssto == NULL))
    ANOVA_error ("unable to allocate sufficient memory");


  /*----- calculate SSTO -----*/
  if (option_data->n != 1)
    volume_read ("ssijkm", ssto, nxyz);
  else
    volume_read ("ssijk", ssto, nxyz);

  volume_read ("ss0", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssto[ixyz] -= y[ixyz];


  /*----- protection against round-off error -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (ssto[ixyz] < 0.0)  ssto[ixyz] = 0.0;

  /*----- save total sum of squares -----*/
  if (nvoxel > 0)
    printf ("SSTO = %f \n", ssto[nvoxel-1]);
  volume_write ("ssto", ssto, nxyz);

  /*----- release memory -----*/
  free (y);      y = NULL;
  free (ssto);    ssto = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the error sum of squares (SSE).
  The output is stored (temporarily) in file sse.3danova3.
*/

void calculate_sse (anova_options * option_data)
{
  float * y = NULL;                   /* input data pointer */
  float * sse = NULL;                 /* sse data pointer */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */


  /*----- assign local variables -----*/
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  sse = (float *) malloc (sizeof(float)*nxyz);
  y = (float *) malloc (sizeof(float)*nxyz);
  if ((y == NULL) || (sse == NULL))
    ANOVA_error ("unable to allocate sufficient memory");


  /*----- calculate SSE -----*/
  volume_read ("ssto", sse, nxyz);

  volume_read ("ssijk", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    sse[ixyz] -= y[ixyz];

  volume_read ("ss0", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    sse[ixyz] += y[ixyz];


  /*----- protection against round-off error -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (sse[ixyz] < 0.0)  sse[ixyz] = 0.0;

  /*----- save error sum of squares -----*/
  if (nvoxel > 0)
    printf ("SSE = %f \n", sse[nvoxel-1]);
  volume_write ("sse", sse, nxyz);

  /*----- release memory -----*/
  free (y);      y = NULL;
  free (sse);    sse = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the sum of squares due to factor A (SSA).
  The output is stored (temporarily) in file ssa.3danova3.
*/

void calculate_ssa (anova_options * option_data)
{
  float * y = NULL;                   /* input data pointer */
  float * ssa = NULL;                 /* output data pointer */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */


  /*----- assign local variables -----*/
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  ssa = (float *) malloc (sizeof(float)*nxyz);
  y = (float *) malloc (sizeof(float)*nxyz);
  if ((y == NULL) || (ssa == NULL))
    ANOVA_error ("unable to allocate sufficient memory");


  /*----- calculate SSA -----*/
  volume_read ("ssi", ssa, nxyz);

  volume_read ("ss0", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssa[ixyz] -= y[ixyz];


  /*----- protection against round-off error -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (ssa[ixyz] < 0.0)  ssa[ixyz] = 0.0;

  /*----- save factor A sum of squares -----*/
  if (nvoxel > 0)
    printf ("SSA = %f \n", ssa[nvoxel-1]);
  volume_write ("ssa", ssa, nxyz);

  /*----- release memory -----*/
  free (y);     y = NULL;
  free (ssa);   ssa = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the sum of squares due to factor B (SSB).
  The output is stored (temporarily) in file ssb.3danova3.
*/

void calculate_ssb (anova_options * option_data)
{
  float * y = NULL;                   /* input data pointer */
  float * ssb = NULL;                 /* output data pointer */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */


  /*----- assign local variables -----*/
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  ssb = (float *) malloc (sizeof(float)*nxyz);
  y = (float *) malloc (sizeof(float)*nxyz);
  if ((y == NULL) || (ssb == NULL))
    ANOVA_error ("unable to allocate sufficient memory");


  /*----- calculate SSB -----*/
  volume_read ("ssj", ssb, nxyz);

  volume_read ("ss0", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssb[ixyz] -= y[ixyz];


  /*----- protection against round-off error -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (ssb[ixyz] < 0.0)  ssb[ixyz] = 0.0;

  /*----- save factor B sum of squares -----*/
  if (nvoxel > 0)
    printf ("SSB = %f \n", ssb[nvoxel-1]);
  volume_write ("ssb", ssb, nxyz);

  /*----- release memory -----*/
  free (y);     y = NULL;
  free (ssb);   ssb = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the sum of squares due to factor C (SSC).
  The output is stored (temporarily) in file ssc.3danova3.
*/

void calculate_ssc (anova_options * option_data)
{
  float * y = NULL;                   /* input data pointer */
  float * ssc = NULL;                 /* output data pointer */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */


  /*----- assign local variables -----*/
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  ssc = (float *) malloc (sizeof(float)*nxyz);
  y = (float *) malloc (sizeof(float)*nxyz);
  if ((y == NULL) || (ssc == NULL))
    ANOVA_error ("unable to allocate sufficient memory");


  /*----- calculate SSC -----*/
  volume_read ("ssk", ssc, nxyz);

  volume_read ("ss0", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssc[ixyz] -= y[ixyz];


  /*----- protection against round-off error -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (ssc[ixyz] < 0.0)  ssc[ixyz] = 0.0;

  /*----- save factor C sum of squares -----*/
  if (nvoxel > 0)
    printf ("SSC = %f \n", ssc[nvoxel-1]);
  volume_write ("ssc", ssc, nxyz);

  /*----- release memory -----*/
  free (y);     y = NULL;
  free (ssc);   ssc = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the A*B interaction sum of squares (SSAB).
  The output is stored (temporarily) in file ssab.3danova3.
*/

void calculate_ssab (anova_options * option_data)
{
  float * y = NULL;                   /* input data pointer */
  float * ssab = NULL;                /* output data pointer */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */


  /*----- assign local variables -----*/
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  ssab = (float *) malloc (sizeof(float)*nxyz);
  y = (float *) malloc (sizeof(float)*nxyz);
  if ((y == NULL) || (ssab == NULL))
    ANOVA_error ("unable to allocate sufficient memory");


  /*----- calculate SSAB -----*/
  volume_read ("ssij", ssab, nxyz);

  volume_read ("ssa", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssab[ixyz] -= y[ixyz];

  volume_read ("ssb", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssab[ixyz] -= y[ixyz];

  volume_read ("ss0", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssab[ixyz] -= y[ixyz];


  /*----- protection against round-off error -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (ssab[ixyz] < 0.0)  ssab[ixyz] = 0.0;

  /*----- save factor A*B sum of squares -----*/
  if (nvoxel > 0)
    printf ("SSAB = %f \n", ssab[nvoxel-1]);
  volume_write ("ssab", ssab, nxyz);

  /*----- release memory -----*/
  free (y);      y = NULL;
  free (ssab);   ssab = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the A*C interaction sum of squares (SSAC).
  The output is stored (temporarily) in file ssac.3danova3.
*/

void calculate_ssac (anova_options * option_data)
{
  float * y = NULL;                   /* input data pointer */
  float * ssac = NULL;                /* output data pointer */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */


  /*----- assign local variables -----*/
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  ssac = (float *) malloc (sizeof(float)*nxyz);
  y = (float *) malloc (sizeof(float)*nxyz);
  if ((y == NULL) || (ssac == NULL))
    ANOVA_error ("unable to allocate sufficient memory");


  /*----- calculate SSAC -----*/
  volume_read ("ssik", ssac, nxyz);

  volume_read ("ssa", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssac[ixyz] -= y[ixyz];

  volume_read ("ssc", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssac[ixyz] -= y[ixyz];

  volume_read ("ss0", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssac[ixyz] -= y[ixyz];


  /*----- protection against round-off error -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (ssac[ixyz] < 0.0)  ssac[ixyz] = 0.0;

  /*----- save factor A*C sum of squares -----*/
  if (nvoxel > 0)
    printf ("SSAC = %f \n", ssac[nvoxel-1]);
  volume_write ("ssac", ssac, nxyz);

  /*----- release memory -----*/
  free (y);      y = NULL;
  free (ssac);   ssac = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the factor C(A) sum of squares (SSC(A)).
  The output is stored (temporarily) in file ssca.3danova3.
*/

void calculate_ssca (anova_options * option_data)
{
  float * y = NULL;                   /* input data pointer */
  float * ssca = NULL;                /* output data pointer */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */


  /*----- assign local variables -----*/
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  ssca = (float *) malloc (sizeof(float)*nxyz);
  y = (float *) malloc (sizeof(float)*nxyz);
  if ((y == NULL) || (ssca == NULL))
    ANOVA_error ("unable to allocate sufficient memory");


  /*----- calculate SSCA -----*/
  volume_read ("ssik", ssca, nxyz);

  volume_read ("ssa", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssca[ixyz] -= y[ixyz];

  volume_read ("ss0", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssca[ixyz] -= y[ixyz];


  /*----- protection against round-off error -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (ssca[ixyz] < 0.0)  ssca[ixyz] = 0.0;

  /*----- save factor C(A) sum of squares -----*/
  if (nvoxel > 0)
    printf ("SSC(A) = %f \n", ssca[nvoxel-1]);
  volume_write ("ssca", ssca, nxyz);

  /*----- release memory -----*/
  free (y);      y = NULL;
  free (ssca);   ssca = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the B*C interaction sum of squares (SSBC).
  The output is stored (temporarily) in file ssbc.3danova3.
*/

void calculate_ssbc (anova_options * option_data)
{
  float * y = NULL;                   /* input data pointer */
  float * ssbc = NULL;                /* output data pointer */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */


  /*----- assign local variables -----*/
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  ssbc = (float *) malloc (sizeof(float)*nxyz);
  y = (float *) malloc (sizeof(float)*nxyz);
  if ((y == NULL) || (ssbc == NULL))
    ANOVA_error ("unable to allocate sufficient memory");


  /*----- calculate SSBC -----*/
  volume_read ("ssjk", ssbc, nxyz);

  volume_read ("ssb", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssbc[ixyz] -= y[ixyz];

  volume_read ("ssc", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssbc[ixyz] -= y[ixyz];

  volume_read ("ss0", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssbc[ixyz] -= y[ixyz];


  /*----- protection against round-off error -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (ssbc[ixyz] < 0.0)  ssbc[ixyz] = 0.0;

  /*----- save factor B*C sum of squares -----*/
  if (nvoxel > 0)
    printf ("SSBC = %f \n", ssbc[nvoxel-1]);
  volume_write ("ssbc", ssbc, nxyz);

  /*----- release memory -----*/
  free (y);      y = NULL;
  free (ssbc);   ssbc = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the A*B*C interaction sum of squares (SSABC).
  The output is stored (temporarily) in file ssabc.3danova3.
*/

void calculate_ssabc (anova_options * option_data)
{
  float * y = NULL;                   /* input data pointer */
  float * ssabc = NULL;               /* output data pointer */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */


  /*----- assign local variables -----*/
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  ssabc = (float *) malloc (sizeof(float)*nxyz);
  y = (float *) malloc (sizeof(float)*nxyz);
  if ((y == NULL) || (ssabc == NULL))
    ANOVA_error ("unable to allocate sufficient memory");


  /*----- calculate SSABC -----*/
  volume_read ("ssto", ssabc, nxyz);

  if (option_data->n > 1)
    {
      volume_read ("sse", y, nxyz);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	ssabc[ixyz] -= y[ixyz];
    }

  volume_read ("ssa", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssabc[ixyz] -= y[ixyz];

  volume_read ("ssb", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssabc[ixyz] -= y[ixyz];

  volume_read ("ssc", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssabc[ixyz] -= y[ixyz];

  volume_read ("ssab", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssabc[ixyz] -= y[ixyz];

  volume_read ("ssac", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssabc[ixyz] -= y[ixyz];

  volume_read ("ssbc", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssabc[ixyz] -= y[ixyz];


  /*----- protection against round-off error -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (ssabc[ixyz] < 0.0)  ssabc[ixyz] = 0.0;

  /*----- save A*B*C interaction sum of squares -----*/
  if (nvoxel > 0)
    printf ("SSABC = %f \n", ssabc[nvoxel-1]);
  volume_write ("ssabc", ssabc, nxyz);

  /*----- release memory -----*/
  free (y);     y = NULL;
  free (ssabc);   ssabc = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the B*C(A) interaction sum of squares (SSBC(A)).
  The output is stored (temporarily) in file ssbca.3danova3.
*/

void calculate_ssbca (anova_options * option_data)
{
  float * y = NULL;                   /* input data pointer */
  float * ssbca = NULL;               /* output data pointer */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */


  /*----- assign local variables -----*/
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  ssbca = (float *) malloc (sizeof(float)*nxyz);
  y = (float *) malloc (sizeof(float)*nxyz);
  if ((y == NULL) || (ssbca == NULL))
    ANOVA_error ("unable to allocate sufficient memory");


  /*----- calculate SSBC(A) -----*/
  volume_read ("ssto", ssbca, nxyz);

  if (option_data->n > 1)
    {
      volume_read ("sse", y, nxyz);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	ssbca[ixyz] -= y[ixyz];
    }

  volume_read ("ssa", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssbca[ixyz] -= y[ixyz];

  volume_read ("ssb", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssbca[ixyz] -= y[ixyz];

  volume_read ("ssca", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssbca[ixyz] -= y[ixyz];

  volume_read ("ssab", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssbca[ixyz] -= y[ixyz];


  /*----- protection against round-off error -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (ssbca[ixyz] < 0.0)  ssbca[ixyz] = 0.0;

  /*----- save B*C(A) interaction sum of squares -----*/
  if (nvoxel > 0)
    printf ("SSBC(A) = %f \n", ssbca[nvoxel-1]);
  volume_write ("ssbca", ssbca, nxyz);

  /*----- release memory -----*/
  free (y);     y = NULL;
  free (ssbca);   ssbca = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the F-statistic for factor A.


  Model 1:   F(A) = MSA / MSE
  Model 4:   F(A) = MSA / MSAC
  Model 5:   F(A) = MSA / MSC(A)

       where MSA    = SSA / (a-1)
             MSE    = SSE / abc(n-1)
             MSAC   = SSAC / (a-1)(c-1)
             MSC(A) = SSC(A) / a(c-1)

  The output is stored as a 2 sub-brick AFNI data set.  The first sub-brick
  contains the square root of MSA (mean sum of squares due to factor A),
  and the second sub-brick contains the corresponding F-statistic.
*/

void calculate_fa (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;      /* protect against divide by zero */
  float * msa = NULL;                  /* pointer to MSA data */
  float * fa = NULL;                   /* pointer to F due to factor A */
  int a;                               /* number of levels for factor A */
  int b;                               /* number of levels for factor B */
  int c;                               /* number of levels for factor C */
  int n;                               /* number of observations per cell */
  int ixyz, nxyz;                      /* voxel counters */
  int nvoxel;                          /* output voxel # */
  int numdf;                           /* numerator degrees of freedom */
  int dendf;                           /* denominator degrees of freedom */
  float fval;                          /* denominator of F-statistic */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  fa = (float *) malloc(sizeof(float)*nxyz);
  msa = (float *) malloc(sizeof(float)*nxyz);
  if ((fa == NULL) || (msa == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  /*----- calculate mean SS due to factor A -----*/
  volume_read ("ssa", msa, nxyz);
  numdf = a - 1;
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    msa[ixyz] = msa[ixyz] / numdf;
  if (nvoxel > 0)
    printf ("MSA = %f \n", msa[nvoxel-1]);

  /*----- set up denominator of F-statistic    -----*/
  switch (option_data->model)
    {
    case 1:
      volume_read ("sse", fa, nxyz);
      dendf = a * b * c * (n-1);
      break;
    case 4:
      volume_read ("ssac", fa, nxyz);
      dendf = (a-1) * (c-1);
      break;
    case 5:
      volume_read ("ssca", fa, nxyz);
      dendf = a * (c-1);
      break;
    }

  /*----- calculate F-statistic -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      fval = fa[ixyz] / dendf;
      if (fval < EPSILON)
	fa[ixyz] = 0.0;
      else
	fa[ixyz] = msa[ixyz] / fval;
    }

  if (nvoxel > 0)
    printf ("F(A) = %f \n", fa[nvoxel-1]);

  /*----- write out afni data file -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    msa[ixyz] = sqrt(msa[ixyz]);        /*-- msa now holds square root --*/
  write_afni_data (option_data, option_data->faname,
		   msa, fa, numdf, dendf);

  /*----- release memory -----*/
  free (msa);   msa = NULL;
  free (fa);    fa = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the F-statistic for factor B.


  Model 1:   F(B) = MSB / MSE
  Model 3:   F(B) = MSB / MSBC
  Model 4:   F(B) = MSB / MSBC
  Model 5:   F(B) = MSB / MSBC(A)

       where MSB     = SSB / (b-1)
             MSE     = SSE / abc(n-1)
             MSBC    = SSBC / (b-1)(c-1)
             MSBC(A) = SSBC(A) / a(b-1)(c-1)

  The output is stored as a 2 sub-brick AFNI data set.  The first sub-brick
  contains the square root of MSB (mean sum of squares due to factor B),
  and the second sub-brick contains the corresponding F-statistic.
*/

void calculate_fb (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;      /* protect against divide by zero */
  float * msb = NULL;                  /* pointer to MSB data */
  float * fb = NULL;                   /* pointer to F due to factor B */
  int a;                               /* number of levels for factor A */
  int b;                               /* number of levels for factor B */
  int c;                               /* number of levels for factor C */
  int n;                               /* number of observations per cell */
  int ixyz, nxyz;                      /* voxel counters */
  int nvoxel;                          /* output voxel # */
  int numdf;                           /* numerator degrees of freedom */
  int dendf;                           /* denominator degrees of freedom */
  float fval;                          /* denominator of F-statistic */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  fb = (float *) malloc(sizeof(float)*nxyz);
  msb = (float *) malloc(sizeof(float)*nxyz);
  if ((fb == NULL) || (msb == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  /*----- calculate mean SS due to factor B -----*/
  volume_read ("ssb", msb, nxyz);
  numdf = b - 1;
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    msb[ixyz] = msb[ixyz] / numdf;
  if (nvoxel > 0)
    printf ("MSB = %f \n", msb[nvoxel-1]);

  /*----- set up denominator of F-statistic    -----*/
  switch (option_data->model)
    {
    case 1:
      volume_read ("sse", fb, nxyz);
      dendf = a * b * c * (n-1);
      break;
    case 3:
    case 4:
      volume_read ("ssbc", fb, nxyz);
      dendf = (b-1) * (c-1);
      break;
    case 5:
      volume_read ("ssbca", fb, nxyz);
      dendf = a * (b-1) * (c-1);
      break;
    }

  /*----- calculate F-statistic -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      fval = fb[ixyz] / dendf;
      if (fval < EPSILON)
	fb[ixyz] = 0.0;
      else
	fb[ixyz] = msb[ixyz] / fval;
    }

  if (nvoxel > 0)
    printf ("F(B) = %f \n", fb[nvoxel-1]);

  /*----- write out afni data file -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    msb[ixyz] = sqrt(msb[ixyz]);        /*-- msb now holds square root --*/
  write_afni_data (option_data, option_data->fbname,
		   msb, fb, numdf, dendf);

  /*----- release memory -----*/
  free (msb);    msb = NULL;
  free (fb);     fb  = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the F-statistic for factor C.


  Model 1:   F(C)    = MSC / MSE
  Model 3:   F(C)    = MSC / MSBC
  Model 4:   F(C)    = MSC / MSE
  Model 5:   F(C(A)) = MSC(A) / MSE

       where MSC     = SSC / (c-1)
             MSE     = SSE / abc(n-1)
             MSBC    = SSBC / (b-1)(c-1)
             MSC(A)  = SSC(A) / a(c-1)

  The output is stored as a 2 sub-brick AFNI data set.  The first sub-brick
  contains the square root of MSC (mean sum of squares due to factor C),
  and the second sub-brick contains the corresponding F-statistic.
*/

void calculate_fc (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;      /* protect against divide by zero */
  float * msc = NULL;                  /* pointer to MSC(A) data */
  float * fc = NULL;                   /* pointer to F due to factor C */
  int a;                               /* number of levels for factor A */
  int b;                               /* number of levels for factor B */
  int c;                               /* number of levels for factor C */
  int n;                               /* number of observations per cell */
  int ixyz, nxyz;                      /* voxel counters */
  int nvoxel;                          /* output voxel # */
  int numdf;                           /* numerator degrees of freedom */
  int dendf;                           /* denominator degrees of freedom */
  float fval;                          /* denominator of F-statistic */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  fc = (float *) malloc(sizeof(float)*nxyz);
  msc = (float *) malloc(sizeof(float)*nxyz);
  if ((fc == NULL) || (msc == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  /*----- set up numerator of F-statistic    -----*/
  switch (option_data->model)
    {
    case 1:
    case 3:
    case 4:
      volume_read ("ssc", msc, nxyz);
      numdf = c - 1;
      break;
    case 5:
      volume_read ("ssca", msc, nxyz);
      numdf = a * (c-1);
      break;
    }

  /*----- calculate mean SS due to factor C -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    msc[ixyz] = msc[ixyz] / numdf;
  if (nvoxel > 0){
    if (option_data->model != 5)
      printf ("MSC = %f \n", msc[nvoxel-1]);
    else
      printf ("MSC(A) = %f \n", msc[nvoxel-1]);
  }


  /*----- set up denominator of F-statistic    -----*/
  switch (option_data->model)
    {
    case 1:
    case 4:
    case 5:
      volume_read ("sse", fc, nxyz);
      dendf = a * b * c * (n-1);
      break;
    case 3:
      volume_read ("ssbc", fc, nxyz);
      dendf = (b-1) * (c-1);
      break;
    }

  /*----- calculate F-statistic    -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      fval = fc[ixyz] / dendf;
      if (fval < EPSILON)
	fc[ixyz] = 0.0;
      else
	fc[ixyz] = msc[ixyz] / fval;
    }

  if (nvoxel > 0){
    if (option_data->model != 5)
      printf ("F(C) = %f \n", fc[nvoxel-1]);
    else
      printf ("F(C(A)) = %f \n", fc[nvoxel-1]);
  }

  /*----- write out afni data file -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    msc[ixyz] = sqrt(msc[ixyz]);        /*-- msc now holds square root --*/
  write_afni_data (option_data, option_data->fcname,
		   msc, fc, numdf, dendf);

  /*----- release memory -----*/
  free (msc);    msc = NULL;
  free (fc);     fc  = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the F-statistic for A*B interaction.


  Model 1:   F(AB) = MSAB / MSE
  Model 2:   F(AB) = MSAB / MSABC
  Model 3:   F(AB) = MSAB / MSABC
  Model 4:   F(AB) = MSAB / MSABC
  Model 5:   F(AB) = MSAB / MSBC(A)

       where MSAB    = SSAB / (a-1)(b-1)
             MSE     = SSE / abc(n-1)
             MSABC   = SSABC / (a-1)(b-1)(c-1)
             MSBC(A) = SSBC(A) / a(b-1)(c-1)

  The output is stored as a 2 sub-brick AFNI data set.  The first sub-brick
  contains the square root of MSAB (mean sum of squares due to interaction),
  and the second sub-brick contains the corresponding F-statistic.
*/

void calculate_fab (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;      /* protect against divide by zero */
  float * msab = NULL;                 /* pointer to MSAB data */
  float * fab = NULL;                  /* pointer to F due to interaction */
  int a;                               /* number of levels for factor A */
  int b;                               /* number of levels for factor B */
  int c;                               /* number of levels for factor C */
  int n;                               /* number of observations per cell */
  int ixyz, nxyz;                      /* voxel counters */
  int nvoxel;                          /* output voxel # */
  int numdf;                           /* numerator degrees of freedom */
  int dendf;                           /* denominator degrees of freedom */
  float fval;                          /* denominator of F-statistic */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  fab = (float *) malloc(sizeof(float)*nxyz);
  msab = (float *) malloc(sizeof(float)*nxyz);
  if ((fab == NULL) || (msab == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  /*----- calculate mean SS due to A*B interaction -----*/
  volume_read ("ssab", msab, nxyz);
  numdf = (a-1) * (b-1);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    msab[ixyz] = msab[ixyz] / numdf;
  if (nvoxel > 0)
    printf ("MSAB = %f \n", msab[nvoxel-1]);

  /*----- set up denominator of F-statistic    -----*/
  switch (option_data->model)
    {
    case 1:
      volume_read ("sse", fab, nxyz);
      dendf = a * b * c * (n-1);
      break;
    case 2:
    case 3:
    case 4:
      volume_read ("ssabc", fab, nxyz);
      dendf = (a-1) * (b-1) * (c-1);
      break;
    case 5:
      volume_read ("ssbca", fab, nxyz);
      dendf = a * (b-1) * (c-1);
      break;
    }

  /*----- calculate F-statistic    -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      fval = fab[ixyz] / dendf;
      if (fval < EPSILON)
	fab[ixyz] = 0.0;
      else
	fab[ixyz] = msab[ixyz] / fval;
    }
  if (nvoxel > 0)
    printf ("F(AB) = %f \n", fab[nvoxel-1]);


  /*----- write out afni data file -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    msab[ixyz] = sqrt(msab[ixyz]);      /*-- msab now holds square root --*/
  write_afni_data (option_data, option_data->fabname,
		   msab, fab, numdf, dendf);


  /*----- release memory -----*/
  free (msab);   msab = NULL;
  free (fab);    fab  = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the F-statistic for B*C interaction.

  Model 1:   F(BC)    = MSBC / MSE
  Model 2:   F(BC)    = MSBC / MSABC
  Model 3:   F(BC)    = MSBC / MSE
  Model 4:   F(BC)    = MSBC / MSE
  Model 5:   F(BC(A)) = MSBC(A) / MSE

       where MSBC    = SSBC / (b-1)(c-1)
             MSE     = SSE / abc(n-1)
             MSABC   = SSABC / (a-1)(b-1)(c-1)
             MSBC(A) = SSBC(A) / a(b-1)(c-1)


  The output is stored as a 2 sub-brick AFNI data set.  The first sub-brick
  contains the square root of MSBC (mean sum of squares due to interaction),
  and the second sub-brick contains the corresponding F-statistic.
*/

void calculate_fbc (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;      /* protect against divide by zero */
  float * msbc = NULL;                 /* pointer to MSBC data */
  float * fbc = NULL;                  /* pointer to F for B*C interaction */
  int a;                               /* number of levels for factor A */
  int b;                               /* number of levels for factor B */
  int c;                               /* number of levels for factor C */
  int n;                               /* number of observations per cell */
  int ixyz, nxyz;                      /* voxel counters */
  int nvoxel;                          /* output voxel # */
  int numdf;                           /* numerator degrees of freedom */
  int dendf;                           /* denominator degrees of freedom */
  float fval;                          /* denominator of F-statistic */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  fbc = (float *) malloc(sizeof(float)*nxyz);
  msbc = (float *) malloc(sizeof(float)*nxyz);
  if ((fbc == NULL) || (msbc == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  /*----- set up numerator of F-statistic    -----*/
  switch (option_data->model)
    {
    case 1:
    case 2:
    case 3:
    case 4:
      volume_read ("ssbc", msbc, nxyz);
      numdf = (b-1) * (c-1);
      break;
    case 5:
      volume_read ("ssbca", msbc, nxyz);
      numdf = a * (b-1) * (c-1);
      break;
    }

  /*----- calculate mean SS due to B*C interaction -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    msbc[ixyz] = msbc[ixyz] / numdf;
  if (nvoxel > 0){
    if (option_data->model != 5)
      printf ("MSBC = %f \n", msbc[nvoxel-1]);
    else
      printf ("MSBC(A) = %f \n", msbc[nvoxel-1]);
  }

 /*----- set up denominator of F-statistic    -----*/
  switch (option_data->model)
    {
    case 1:
    case 3:
    case 4:
    case 5:
      volume_read ("sse", fbc, nxyz);
      dendf = a * b * c * (n-1);
      break;
    case 2:
      volume_read ("ssabc", fbc, nxyz);
      dendf = (a-1) * (b-1) * (c-1);
      break;
    }

  /*----- calculate F-statistic    -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      fval = fbc[ixyz] / dendf;
      if (fval < EPSILON)
	fbc[ixyz] = 0.0;
      else
	fbc[ixyz] = msbc[ixyz] / fval;
    }
  if (nvoxel > 0){
    if (option_data->model != 5)
      printf ("F(BC) = %f \n", fbc[nvoxel-1]);
    else
      printf ("F(BC(A)) = %f \n", fbc[nvoxel-1]);
  }

  /*----- write out afni data file -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    msbc[ixyz] = sqrt(msbc[ixyz]);      /*-- msbc now holds square root --*/
  write_afni_data (option_data, option_data->fbcname,
		   msbc, fbc, numdf, dendf);

  /*----- release memory -----*/
  free (msbc);   msbc = NULL;
  free (fbc);    fbc  = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the F-statistic for A*C interaction.

  Model 1:   F(AC) = MSAC / MSE
  Model 2:   F(AC) = MSAC / MSABC
  Model 3:   F(AC) = MSAC / MSABC
  Model 4:   F(AC) = MSAC / MSE

       where MSAC  = SSAC / (a-1)(c-1)
             MSE     = SSE / abc(n-1)
             MSABC   = SSABC / (a-1)(b-1)(c-1)

  The output is stored as a 2 sub-brick AFNI data set.  The first sub-brick
  contains the square root of MSAC (mean sum of squares due to interaction),
  and the second sub-brick contains the corresponding F-statistic.
*/

void calculate_fac (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;      /* protect against divide by zero */
  float * msac = NULL;                 /* pointer to MSAC data */
  float * fac = NULL;                  /* pointer to F for A*C interaction */
  int a;                               /* number of levels for factor A */
  int b;                               /* number of levels for factor B */
  int c;                               /* number of levels for factor C */
  int n;                               /* number of observations per cell */
  int ixyz, nxyz;                      /* voxel counters */
  int nvoxel;                          /* output voxel # */
  int numdf;                           /* numerator degrees of freedom */
  int dendf;                           /* denominator degrees of freedom */
  float fval;                          /* denominator of F-statistic */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  fac = (float *) malloc(sizeof(float)*nxyz);
  msac = (float *) malloc(sizeof(float)*nxyz);
  if ((fac == NULL) || (msac == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  /*----- calculate mean SS due to A*C interaction -----*/
  volume_read ("ssac", msac, nxyz);
  numdf = (a-1) * (c-1);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    msac[ixyz] = msac[ixyz] / numdf;
  if (nvoxel > 0)
    printf ("MSAC = %f \n", msac[nvoxel-1]);

 /*----- set up denominator of F-statistic    -----*/
  switch (option_data->model)
    {
    case 1:
    case 4:
      volume_read ("sse", fac, nxyz);
      dendf = a * b * c * (n-1);
      break;
    case 2:
    case 3:
      volume_read ("ssabc", fac, nxyz);
      dendf = (a-1) * (b-1) * (c-1);
      break;
    }

  /*----- calculate F-statistic    -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      fval = fac[ixyz] / dendf;
      if (fval < EPSILON)
	fac[ixyz] = 0.0;
      else
	fac[ixyz] = msac[ixyz] / fval;
    }
  if (nvoxel > 0)
    printf ("F(AC) = %f \n", fac[nvoxel-1]);

  /*----- write out afni data file -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    msac[ixyz] = sqrt(msac[ixyz]);      /*-- msac now holds square root --*/
  write_afni_data (option_data, option_data->facname,
		   msac, fac, numdf, dendf);

  /*----- release memory -----*/
  free (msac);   msac = NULL;
  free (fac);    fac  = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the F-statistic for A*B*C interaction.

  Model 1:   F(ABC) = MSABC / MSE
  Model 2:   F(ABC) = MSABC / MSE
  Model 3:   F(ABC) = MSABC / MSE
  Model 4:   F(ABC) = MSABC / MSE

       where MSABC   = SSABC / (a-1)(b-1)(c-1)
             MSE     = SSE / abc(n-1)

  The output is stored as a 2 sub-brick AFNI data set.  The first sub-brick
  contains the square root of MSAC (mean sum of squares due to interaction),
  and the second sub-brick contains the corresponding F-statistic.
*/

void calculate_fabc (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;      /* protect against divide by zero */
  float * msabc = NULL;                /* pointer to MSABC data */
  float * fabc = NULL;                 /* pointer to F for A*B*C interaction */
  int a;                               /* number of levels for factor A */
  int b;                               /* number of levels for factor B */
  int c;                               /* number of levels for factor C */
  int n;                               /* number of observations per cell */
  int ixyz, nxyz;                      /* voxel counters */
  int nvoxel;                          /* output voxel # */
  int numdf;                           /* numerator degrees of freedom */
  int dendf;                           /* denominator degrees of freedom */
  float fval;                          /* denominator of F-statistic */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  fabc = (float *) malloc(sizeof(float)*nxyz);
  msabc = (float *) malloc(sizeof(float)*nxyz);
  if ((fabc == NULL) || (msabc == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  /*----- calculate mean SS due to A*B*C interaction -----*/
  volume_read ("ssabc", msabc, nxyz);
  numdf = (a-1) * (b-1) * (c-1);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    msabc[ixyz] = msabc[ixyz] / numdf;
  if (nvoxel > 0)
    printf ("MSABC = %f \n", msabc[nvoxel-1]);

 /*----- set up denominator of F-statistic    -----*/
  switch (option_data->model)
    {
    case 1:
    case 2:
    case 3:
    case 4:
      volume_read ("sse", fabc, nxyz);
      dendf = a * b * c * (n-1);
      break;
    }

  /*----- calculate F-statistic    -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      fval = fabc[ixyz] / dendf;
      if (fval < EPSILON)
	fabc[ixyz] = 0.0;
      else
	fabc[ixyz] = msabc[ixyz] / fval;
    }
  if (nvoxel > 0)
    printf ("F(ABC) = %f \n", fabc[nvoxel-1]);

  /*----- write out afni data file -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    msabc[ixyz] = sqrt(msabc[ixyz]);      /*-- msabc now holds square root --*/
  write_afni_data (option_data, option_data->fabcname,
		   msabc, fabc, numdf, dendf);

  /*----- release memory -----*/
  free (msabc);   msabc = NULL;
  free (fabc);    fabc  = NULL;

}

/*---------------------------------------------------------------------------*/
/*
  The following are the old calculate_* functions, before the changes
  by rickr/gangc.  The function names have been prepended by old_,
  allowing the addition of an option to call those functions, instead
  of the new varieties.  This seems to be the cleanest way to allow
  users to compare results between the old vs. new functions.  Putting
  multiple tests in each function would be quite messy.

  Note that the different functionality exists for types 4 and 5 of
  a/b means, a/b diff, and a/b contr.
                                                   22 Nov 2005 [rickr]
*/


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the mean treatment effect for factor A at the user
  specified treatment level.  The output is stored as a 2 sub-brick AFNI
  data set. The first sub-brick contains the estimated mean at this treatment
  level, and the second sub-brick contains the corresponding t-statistic.
*/
void old_calculate_ameans (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;    /* protect against divide by zero */
  float * mean = NULL;               /* pointer to treatment mean data */
  float * tmean = NULL;              /* pointer to t-statistic data */
  int a;                             /* number of levels for factor A */
  int b;                             /* number of levels for factor B */
  int c;                             /* number of levels for factor C */
  int n;                             /* number of observations per cell */
  int ixyz, nxyz;                    /* voxel counters */
  int nvoxel;                        /* output voxel # */
  int num_means;                     /* number of user requested means */
  int imean;                         /* output mean option index */
  int level;                         /* factor A level index */
  int df;                            /* degrees of freedom for t-test */
  float fval;                        /* for calculating std. dev. */
  float stddev;                      /* est. std. dev. of factor mean */

  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
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

      /*----- estimate factor mean for this treatment level -----*/
      calculate_sum (option_data, level, -1, -1, mean);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	mean[ixyz] = mean[ixyz] / (b*c*n);
      if (nvoxel > 0)
	printf ("Mean of factor A level %d = %f \n", level+1, mean[nvoxel-1]);

      /*----- standard deviation depends on model type -----*/
      switch (option_data->model)
	{
	case 1:
	  volume_read ("sse", tmean, nxyz);
	  df = a * b * c * (n-1);
	  break;
	case 4:
	  volume_read ("ssac", tmean, nxyz);
	  df = (a-1) * (c-1);
	  break;
	case 5:
	  volume_read ("ssca", tmean, nxyz);
	  df = a * (c-1);
	  break;
	}

      /*----- divide by estimated standard deviation of factor mean -----*/
      fval = (1.0 / df) * (1.0 / (b*c*n));
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  stddev =  sqrt(tmean[ixyz] * fval);
	  if (stddev < EPSILON)
	    tmean[ixyz] = 0.0;
	  else
	    tmean[ixyz] = mean[ixyz] / stddev;
	}

      if (nvoxel > 0)
	printf ("t for mean of factor A level %d = %f \n",
		level+1, tmean[nvoxel-1]);

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->amname[imean],
                       mean, tmean, df, 0);

    }

  /*----- release memory -----*/
  free (tmean);   tmean = NULL;
  free (mean);    mean = NULL;
}

/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the mean treatment effect for factor B at the user
  specified treatment level.  The output is stored as a 2 sub-brick AFNI
  data set. The first sub-brick contains the estimated mean at this treatment
  level, and the second sub-brick contains the corresponding t-statistic.
*/

void old_calculate_bmeans (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;    /* protect against divide by zero */
  float * mean = NULL;               /* pointer to treatment mean data */
  float * tmean = NULL;              /* pointer to t-statistic data */
  int a;                             /* number of levels for factor A */
  int b;                             /* number of levels for factor B */
  int c;                             /* number of levels for factor C */
  int n;                             /* number of observations per cell */
  int ixyz, nxyz;                    /* voxel counters */
  int nvoxel;                        /* output voxel # */
  int nt;                            /* total number of observations */
  int num_means;                     /* number of user requested means */
  int imean;                         /* output mean option index */
  int level;                         /* factor B level index */
  int df;                            /* degrees of freedom for t-test */
  float fval;                        /* for calculating std. dev. */
  float stddev;                      /* est. std. dev. of factor mean */

  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nt = option_data->nt;
  num_means = option_data->num_bmeans;
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
      level = option_data->bmeans[imean];

      /*----- estimate factor mean for this treatment level -----*/
      calculate_sum (option_data, -1, level, -1, mean);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	mean[ixyz] = mean[ixyz] / (a*c*n);
      if (nvoxel > 0)
	printf ("Mean of factor B level %d = %f \n", level+1, mean[nvoxel-1]);

      /*----- standard deviation depends on model type -----*/
      switch (option_data->model)
	{
	case 1:
	  volume_read ("sse", tmean, nxyz);
	  df = a * b * c * (n-1);
	  break;
	case 4:
	  volume_read ("ssbc", tmean, nxyz);
	  df = (b-1) * (c-1);
	  break;
	case 5:
	  volume_read ("ssbca", tmean, nxyz);
	  df = a * (b-1) * (c-1);
	  break;
	}

      /*----- divide by estimated standard deviation of factor mean -----*/
      fval = (1.0 / df) * (1.0 / (a*c*n));
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  stddev =  sqrt(tmean[ixyz] * fval);
	  if (stddev < EPSILON)
	    tmean[ixyz] = 0.0;
	  else
	    tmean[ixyz] = mean[ixyz] / stddev;
	}
      if (nvoxel > 0)
	printf ("t for mean of factor B level %d = %f \n",
		level+1, tmean[nvoxel-1]);

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->bmname[imean],
                       mean, tmean, df, 0);

    }

  /*----- release memory -----*/
  free (tmean);   tmean = NULL;
  free (mean);    mean = NULL;
}


/*---------------------------------------------------------------------------*/
/*
   Routine to estimate the difference in the means between two user specified
   treatment levels for factor A.  The output is a 2 sub-brick AFNI data set.
   The first sub-brick contains the estimated difference in the means.
   The second sub-brick contains the corresponding t-statistic.
*/

void old_calculate_adifferences (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
  float * diff = NULL;                /* pointer to est. diff. in means */
  float * tdiff = NULL;               /* pointer to t-statistic data */
  int a;                              /* number of levels for factor A */
  int b;                              /* number of levels for factor B */
  int c;                              /* number of levels for factor C */
  int n;                              /* number of observations per cell */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */
  int num_diffs;                      /* number of user requested diffs. */
  int idiff;                          /* index for requested differences */
  int i, j;                           /* factor level indices */
  int df;                             /* degrees of freedom for t-test */
  float fval;                         /* for calculating std. dev. */
  float stddev;                       /* est. std. dev. of difference */

  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
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

      /*----- read first treatment level mean -----*/
      i = option_data->adiffs[idiff][0];
      calculate_sum (option_data, i, -1, -1, diff);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	diff[ixyz] = diff[ixyz] / (b*c*n);

      /*----- subtract second treatment level mean -----*/
      j = option_data->adiffs[idiff][1];
      calculate_sum (option_data, j, -1, -1, tdiff);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	diff[ixyz] -= tdiff[ixyz] / (b*c*n);
      if (nvoxel > 0)
	printf ("Difference of factor A level %d - level %d = %f \n",
		i+1, j+1, diff[nvoxel-1]);

      /*----- standard deviation depends on model type -----*/
      switch (option_data->model)
	{
	case 1:
	  volume_read ("sse", tdiff, nxyz);
	  df = a * b * c * (n-1);
	  break;
	case 4:
	  volume_read ("ssac", tdiff, nxyz);
	  df = (a-1) * (c-1);
	  break;
	case 5:
	  volume_read ("ssca", tdiff, nxyz);
	  df = a * (c-1);
	  break;
	}

      /*----- divide by estimated standard deviation of difference -----*/
      fval = (1.0 / df) * (2.0 / (b*c*n));
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  stddev = sqrt (tdiff[ixyz] * fval);
	  if (stddev < EPSILON)
	    tdiff[ixyz] = 0.0;
	  else
	    tdiff[ixyz] = diff[ixyz] / stddev;
	}

      if (nvoxel > 0)
	printf ("t for difference of factor A level %d - level %d = %f \n",
		i+1, j+1, tdiff[nvoxel-1]);

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->adname[idiff],
                       diff, tdiff, df, 0);

    }

  /*----- release memory -----*/
  free (tdiff);   tdiff = NULL;
  free (diff);    diff = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to estimate the difference in the means between two user specified
   treatment levels for factor B.  The output is a 2 sub-brick AFNI data set.
   The first sub-brick contains the estimated difference in the means.
   The second sub-brick contains the corresponding t-statistic.
*/

void old_calculate_bdifferences (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
  float * diff = NULL;                /* pointer to est. diff. in means */
  float * tdiff = NULL;               /* pointer to t-statistic data */
  int a;                              /* number of levels for factor A */
  int b;                              /* number of levels for factor B */
  int c;                              /* number of levels for factor C */
  int n;                              /* number of observations per cell */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */
  int num_diffs;                      /* number of user requested diffs. */
  int idiff;                          /* index for requested differences */
  int i, j;                           /* factor level indices */
  int df;                             /* degrees of freedom for t-test */
  float fval;                         /* for calculating std. dev. */
  float stddev;                       /* est. std. dev. of difference */

  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  num_diffs = option_data->num_bdiffs;
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

      /*----- read first treatment level mean -----*/
      i = option_data->bdiffs[idiff][0];
      calculate_sum (option_data, -1, i, -1, diff);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	diff[ixyz] = diff[ixyz] / (a*c*n);

      /*----- subtract second treatment level mean -----*/
      j = option_data->bdiffs[idiff][1];
      calculate_sum (option_data, -1, j, -1, tdiff);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	diff[ixyz] -= tdiff[ixyz] / (a*c*n);
      if (nvoxel > 0)
	printf ("Difference of factor B level %d - level %d = %f \n",
		i+1, j+1, diff[nvoxel-1]);

      /*----- standard deviation depends on model type -----*/
      switch (option_data->model)
	{
	case 1:
	  volume_read ("sse", tdiff, nxyz);
	  df = a * b * c * (n-1);
	  break;
	case 4:
	  volume_read ("ssbc", tdiff, nxyz);
	  df = (b-1) * (c-1);
	  break;
	case 5:
	  volume_read ("ssbca", tdiff, nxyz);
	  df = a * (b-1) * (c-1);
	  break;
	}

      /*----- divide by estimated standard deviation of difference -----*/
      fval = (1.0 / df) * (2.0 / (a*c*n));
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  stddev = sqrt (tdiff[ixyz] * fval);
	  if (stddev < EPSILON)
	    tdiff[ixyz] = 0.0;
	  else
	    tdiff[ixyz] = diff[ixyz] / stddev;
	}

      if (nvoxel > 0)
	printf ("t for difference of factor B level %d - level %d = %f \n",
		i+1, j+1, tdiff[nvoxel-1]);

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->bdname[idiff],
                       diff, tdiff, df, 0);

    }

  /*----- release memory -----*/
  free (tdiff);   tdiff = NULL;
  free (diff);    diff = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to estimate a user specified contrast in treatment levels for
  factor A.  The output is stored as a 2 sub-brick AFNI data set.  The first
  sub-brick contains the estimated contrast.  The second sub-brick contains
  the corresponding t-statistic.
*/

void old_calculate_acontrasts (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
  float * contr = NULL;               /* pointer to contrast estimate */
  float * tcontr = NULL;              /* pointer to t-statistic data */
  int a;                              /* number of levels for factor A */
  int b;                              /* number of levels for factor B */
  int c;                              /* number of levels for factor C */
  int n;                              /* number of observations per cell */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */
  int num_contr;                      /* number of user requested contrasts */
  int icontr;                         /* index of user requested contrast */
  int level;                          /* factor level index */
  int df;                             /* degrees of freedom for t-test */
  float fval;                         /* for calculating std. dev. */
  float coef;                         /* contrast coefficient */
  float stddev;                       /* est. std. dev. of contrast */

  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
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

      for (level = 0;  level < a;  level++)
	{
	  coef = option_data->acontr[icontr][level];
	  if (coef == 0.0) continue;
	
	  /*----- add coef * treatment level mean to contrast -----*/
	  calculate_sum (option_data, level, -1, -1, tcontr);
	  fval += coef * coef / (b*c*n);
	  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	    contr[ixyz] += coef * tcontr[ixyz] / (b*c*n);
	}

      if (nvoxel > 0)
	printf ("No.%d contrast for factor A = %f \n",
		icontr+1, contr[nvoxel-1]);

      /*----- standard deviation depends on model type -----*/
      switch (option_data->model)
	{
	case 1:
	  volume_read ("sse", tcontr, nxyz);
	  df = a * b * c * (n-1);
	  break;
	case 4:
	  volume_read ("ssac", tcontr, nxyz);
	  df = (a-1) * (c-1);
	  break;
	case 5:
	  volume_read ("ssca", tcontr, nxyz);
	  df = a * (c-1);
	  break;
	}

      /*----- divide by estimated standard deviation of the contrast -----*/
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  stddev = sqrt ((tcontr[ixyz] / df) * fval);
	  if (stddev < EPSILON)
	    tcontr[ixyz] = 0.0;
	  else
	    tcontr[ixyz] = contr[ixyz] / stddev;
	}

      if (nvoxel > 0)
	printf ("t of No.%d contrast for factor A = %f \n",
		icontr+1, tcontr[nvoxel-1]);

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->acname[icontr],
                       contr, tcontr, df, 0);

    }

  /*----- release memory -----*/
  free (tcontr);   tcontr = NULL;
  free (contr);    contr = NULL;

}

/*---------------------------------------------------------------------------*/
/*
  Routine to estimate a user specified contrast in treatment levels for
  factor B.  The output is stored as a 2 sub-brick AFNI data set.  The first
  sub-brick contains the estimated contrast.  The second sub-brick contains
  the corresponding t-statistic.
*/

void old_calculate_bcontrasts (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
  float * contr = NULL;               /* pointer to contrast estimate */
  float * tcontr = NULL;              /* pointer to t-statistic data */
  int a;                              /* number of levels for factor A */
  int b;                              /* number of levels for factor B */
  int c;                              /* number of levels for factor C */
  int n;                              /* number of observations per cell */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */
  int num_contr;                      /* number of user requested contrasts */
  int icontr;                         /* index of user requested contrast */
  int level;                          /* factor level index */
  int df;                             /* degrees of freedom for t-test */
  float fval;                         /* for calculating std. dev. */
  float coef;                         /* contrast coefficient */
  float stddev;                       /* est. std. dev. of contrast */

  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  num_contr = option_data->num_bcontr;
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

      for (level = 0;  level < b;  level++)
	{
	  coef = option_data->bcontr[icontr][level];
	  if (coef == 0.0) continue;
	
	  /*----- add coef * treatment level mean to contrast -----*/
	  calculate_sum (option_data, -1, level, -1, tcontr);
	  fval += coef * coef / (a*c*n);
	  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	    contr[ixyz] += coef * tcontr[ixyz] / (a*c*n);
	}

      if (nvoxel > 0)
	printf ("No.%d contrast for factor B = %f \n",
		icontr+1, contr[nvoxel-1]);

      /*----- standard deviation depends on model type -----*/
      switch (option_data->model)
	{
	case 1:
	  volume_read ("sse", tcontr, nxyz);
	  df = a * b * c * (n-1);
	  break;
	case 4:
	  volume_read ("ssbc", tcontr, nxyz);
	  df = (b-1) * (c-1);
	  break;
	case 5:
	  volume_read ("ssbca", tcontr, nxyz);
	  df = a * (b-1) * (c-1);
	  break;
	}

      /*----- divide by estimated standard deviation of the contrast -----*/
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  stddev = sqrt ((tcontr[ixyz] / df) * fval);
	  if (stddev < EPSILON)
	    tcontr[ixyz] = 0.0;
	  else
	    tcontr[ixyz] = contr[ixyz] / stddev;
	}

      if (nvoxel > 0)
	printf ("t of No.%d contrast for factor B = %f \n",
		icontr+1, tcontr[nvoxel-1]);

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->bcname[icontr],
                       contr, tcontr, df, 0);

    }

  /*----- release memory -----*/
  free (tcontr);   tcontr = NULL;
  free (contr);    contr = NULL;

}

/*---------------------------------------------------------------------------*/
/*                  --- end old_calclulte_* functions ---                    */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the mean treatment effect for factor A at the user
  specified treatment level.  The output is stored as a 2 sub-brick AFNI
  data set. The first sub-brick contains the estimated mean at this treatment
  level, and the second sub-brick contains the corresponding t-statistic.
*/

void calculate_ameans (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;    /* protect against divide by zero */
  float * mean = NULL;               /* pointer to treatment mean data */
  float * tmean = NULL;              /* pointer to t-statistic data */
  float * contr;                     /* array for trivial A contrast */
  int a;                             /* number of levels for factor A */
  int b;                             /* number of levels for factor B */
  int c;                             /* number of levels for factor C */
  int n;                             /* number of observations per cell */
  int ixyz, nxyz;                    /* voxel counters */
  int cindex;                        /* contrast index */
  int nvoxel;                        /* output voxel # */
  int num_means;                     /* number of user requested means */
  int imean;                         /* output mean option index */
  int level;                         /* factor A level index */
  int df;                            /* degrees of freedom for t-test */
  float fval;                        /* for calculating std. dev. */
  float stddev;                      /* est. std. dev. of factor mean */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  num_means = option_data->num_ameans;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  mean = (float *) malloc(sizeof(float)*nxyz);
  tmean = (float *) malloc(sizeof(float)*nxyz);
  contr = (float *) malloc(sizeof(float)*a);
  if ((mean == NULL) || (tmean == NULL) || (contr == NULL))
    ANOVA_error ("ameans: unable to allocate sufficient memory");

  /*----- loop over user specified treatment means -----*/
  for (imean = 0;  imean < num_means;  imean++)
  {
      level = option_data->ameans[imean];

      if (option_data->model == 1)
      {
          /*----- estimate factor mean for this treatment level -----*/
          calculate_sum (option_data, level, -1, -1, mean);
          for (ixyz = 0; ixyz < nxyz; ixyz++) mean[ixyz] = mean[ixyz] / (b*c*n);

          volume_read ("sse", tmean, nxyz);
          df = a * b * c * (n-1);
          fval = (1.0 / df) * (1.0 / (b*c*n));

          /*----- divide by estimated standard deviation of factor mean -----*/
          for (ixyz = 0;  ixyz < nxyz;  ixyz++)
          {
              stddev =  sqrt(tmean[ixyz] * fval);
              if (stddev < EPSILON) tmean[ixyz] = 0.0;
              else                  tmean[ixyz] = mean[ixyz] / stddev;
          }
      } else if (option_data->model == 4 || option_data->model == 5) {
                                                 /* 8 Sep 2005 [rickr,gangc] */
        /* set mean as simple contrast */
        for ( cindex = 0; cindex < a; cindex++ ) contr[cindex] = 0.0;
        contr[level] = 1.0;

        df = c - 1;  /* degrees of freedom from random variable only */
        calc_type4_acontr( option_data, contr, -1, df, mean, tmean );
      }

      if (nvoxel > 0) {
	printf ("Mean of factor A level %d = %f \n", level+1, mean[nvoxel-1]);
	printf ("t for mean of factor A level %d = %f \n",
		level+1, tmean[nvoxel-1]);
      }

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->amname[imean],
                       mean, tmean, df, 0);
  }

  /*----- release memory -----*/
  free (tmean);  free (mean);  free(contr);
}

/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the mean treatment effect for factor B at the user
  specified treatment level.  The output is stored as a 2 sub-brick AFNI
  data set. The first sub-brick contains the estimated mean at this treatment
  level, and the second sub-brick contains the corresponding t-statistic.
*/

void calculate_bmeans (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;    /* protect against divide by zero */
  float * mean = NULL;               /* pointer to treatment mean data */
  float * tmean = NULL;              /* pointer to t-statistic data */
  float * contr;                     /* array for trivial B contrasts */
  int a;                             /* number of levels for factor A */
  int b;                             /* number of levels for factor B */
  int c;                             /* number of levels for factor C */
  int n;                             /* number of observations per cell */
  int ixyz, nxyz;                    /* voxel counters */
  int cindex;                        /* contrast index */
  int nvoxel;                        /* output voxel # */
  int nt;                            /* total number of observations */
  int num_means;                     /* number of user requested means */
  int imean;                         /* output mean option index */
  int level;                         /* factor B level index */
  int df;                            /* degrees of freedom for t-test */
  float fval;                        /* for calculating std. dev. */
  float stddev;                      /* est. std. dev. of factor mean */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  nt = option_data->nt;
  num_means = option_data->num_bmeans;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  mean = (float *) malloc(sizeof(float)*nxyz);
  tmean = (float *) malloc(sizeof(float)*nxyz);
  contr = (float *) malloc(sizeof(float)*b);
  if ((mean == NULL) || (tmean == NULL) || (contr == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  /*----- loop over user specified treatment means -----*/
  for (imean = 0;  imean < num_means;  imean++)
    {
      level = option_data->bmeans[imean];

      if (option_data->model == 1)
      {
          /*----- estimate factor mean for this treatment level -----*/
          calculate_sum (option_data, -1, level, -1, mean);
          for (ixyz = 0; ixyz < nxyz; ixyz++) mean[ixyz] = mean[ixyz] / (a*c*n);

          volume_read ("sse", tmean, nxyz);
          df = a * b * c * (n-1);

          /*----- divide by estimated standard deviation of factor mean -----*/
          fval = (1.0 / df) * (1.0 / (a*c*n));
          for (ixyz = 0;  ixyz < nxyz;  ixyz++)
          {
              stddev =  sqrt(tmean[ixyz] * fval);
              if (stddev < EPSILON) tmean[ixyz] = 0.0;
              else                  tmean[ixyz] = mean[ixyz] / stddev;
          }
      } else if (option_data->model == 4) {      /* 8 Sep 2005 [rickr,gangc] */

        /* set mean as simple contrast */
        for ( cindex = 0; cindex < b; cindex++ ) contr[cindex] = 0.0;
        contr[level] = 1.0;

        df = c - 1;  /* degrees of freedom from random variable only */
        calc_type4_bcontr( option_data, contr, -1, df, mean, tmean );

      } else { /* option_data->model == 5 (set mean as simple contrast) */

        for ( cindex = 0; cindex < b; cindex++ ) contr[cindex] = 0.0;
        contr[level] = 1.0;

        calc_type5_bcontr(option_data, contr, -1, mean, tmean);
        df = a * (c - 1);
      }

      if (nvoxel > 0){
	printf ("Mean of factor B level %d = %f \n", level+1, mean[nvoxel-1]);
	printf ("t for mean of factor B level %d = %f \n",
		level+1, tmean[nvoxel-1]);
      }

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->bmname[imean],
                       mean, tmean, df, 0);
    }

  /*----- release memory -----*/
  free (tmean);  free (mean);  free(contr);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the mean treatment effect for factor C at the user
  specified treatment level.  The output is stored as a 2 sub-brick AFNI
  data set. The first sub-brick contains the estimated mean at this treatment
  level, and the second sub-brick contains the corresponding t-statistic.
*/

void calculate_cmeans (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;    /* protect against divide by zero */
  float * mean = NULL;               /* pointer to treatment mean data */
  float * tmean = NULL;              /* pointer to t-statistic data */
  int a;                             /* number of levels for factor A */
  int b;                             /* number of levels for factor B */
  int c;                             /* number of levels for factor C */
  int n;                             /* number of observations per cell */
  int ixyz, nxyz;                    /* voxel counters */
  int nvoxel;                        /* output voxel # */
  int nt;                            /* total number of observations */
  int num_means;                     /* number of user requested means */
  int imean;                         /* output mean option index */
  int level;                         /* factor C level index */
  int df;                            /* degrees of freedom for t-test */
  float fval;                        /* for calculating std. dev. */
  float stddev;                      /* est. std. dev. of factor mean */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  df = a * b * c * (n-1);
  nt = option_data->nt;
  num_means = option_data->num_cmeans;
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
      level = option_data->cmeans[imean];

      /*----- estimate factor mean for this treatment level -----*/
      calculate_sum (option_data, -1, -1, level, mean);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	mean[ixyz] = mean[ixyz] / (a*b*n);
      if (nvoxel > 0)
	printf ("Mean of factor C level %d = %f \n", level+1, mean[nvoxel-1]);

      /*----- divide by estimated standard deviation of factor mean -----*/
      volume_read ("sse", tmean, nxyz);
      fval = (1.0 / df) * (1.0 / (a*b*n));
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  stddev =  sqrt(tmean[ixyz] * fval);
	  if (stddev < EPSILON)
	    tmean[ixyz] = 0.0;
	  else
	    tmean[ixyz] = mean[ixyz] / stddev;
	}
      if (nvoxel > 0)
	printf ("t for mean of factor C level %d = %f \n",
		level+1, tmean[nvoxel-1]);

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->cmname[imean],
                       mean, tmean, df, 0);

    }

  /*----- release memory -----*/
  free (tmean);   tmean = NULL;
  free (mean);    mean = NULL;
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the mean for an individual cell (treatment).
  The output is stored as a 2 sub-brick AFNI data set. The first sub-brick
  contains the estimated mean for this cell, and the second sub-brick contains
  the corresponding t-statistic.
*/

void calculate_xmeans (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;    /* protect against divide by zero */
  float * mean = NULL;               /* pointer to treatment mean data */
  float * tmean = NULL;              /* pointer to t-statistic data */
  int a;                             /* number of levels for factor A */
  int b;                             /* number of levels for factor B */
  int c;                             /* number of levels for factor C */
  int n;                             /* number of observations per cell */
  int ixyz, nxyz;                    /* voxel counters */
  int nvoxel;                        /* output voxel # */
  int nt;                            /* total number of observations */
  int num_means;                     /* number of user requested means */
  int imean;                         /* output mean option index */
  int alevel, blevel, clevel;        /* factor level indices */
  int df;                            /* degrees of freedom for t-test */
  float fval;                        /* for calculating std. dev. */
  float stddev;                      /* est. std. dev. of cell mean */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  df = a * b * c * (n-1);
  nt = option_data->nt;
  num_means = option_data->num_xmeans;
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
      alevel = option_data->xmeans[imean][0];
      blevel = option_data->xmeans[imean][1];
      clevel = option_data->xmeans[imean][2];

      /*----- estimate factor mean for this treatment level -----*/
      calculate_sum (option_data, alevel, blevel, clevel, mean);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	mean[ixyz] = mean[ixyz] / n;
      if (nvoxel > 0)
	printf ("Mean of Cell[%d][%d][%d] = %f \n",
		alevel+1, blevel+1, clevel+1, mean[nvoxel-1]);

      /*----- divide by estimated standard deviation of factor mean -----*/
      volume_read ("sse", tmean, nxyz);
      fval = (1.0 / df) * (1.0 / n);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  stddev =  sqrt(tmean[ixyz] * fval);
	  if (stddev < EPSILON)
	    tmean[ixyz] = 0.0;
	  else
	    tmean[ixyz] = mean[ixyz] / stddev;
	}
      if (nvoxel > 0)
	printf ("t-stat for Mean of Cell[%d][%d][%d] = %f \n",
		alevel+1, blevel+1, clevel+1, tmean[nvoxel-1]);

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->xmname[imean],
                       mean, tmean, df, 0);

    }

  /*----- release memory -----*/
  free (tmean);   tmean = NULL;
  free (mean);    mean = NULL;
}


/*---------------------------------------------------------------------------*/
/*
   Routine to estimate the difference in the means between two user specified
   treatment levels for factor A.  The output is a 2 sub-brick AFNI data set.
   The first sub-brick contains the estimated difference in the means.
   The second sub-brick contains the corresponding t-statistic.
*/

void calculate_adifferences (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
  float * diff = NULL;                /* pointer to est. diff. in means */
  float * tdiff = NULL;               /* pointer to t-statistic data */
  float * contr;                      /* pointer to t-statistic data */
  int a;                              /* number of levels for factor A */
  int b;                              /* number of levels for factor B */
  int c;                              /* number of levels for factor C */
  int n;                              /* number of observations per cell */
  int ixyz, nxyz;                     /* voxel counters */
  int cindex;                         /* contrast index */
  int nvoxel;                         /* output voxel # */
  int num_diffs;                      /* number of user requested diffs. */
  int idiff;                          /* index for requested differences */
  int i, j;                           /* factor level indices */
  int df;                             /* degrees of freedom for t-test */
  float fval;                         /* for calculating std. dev. */
  float stddev;                       /* est. std. dev. of difference */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  num_diffs = option_data->num_adiffs;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  diff = (float *) malloc(sizeof(float)*nxyz);
  tdiff = (float *) malloc(sizeof(float)*nxyz);
  contr = (float *) malloc(sizeof(float)*a);
  if ((diff == NULL) || (tdiff == NULL) || (contr == NULL))
    ANOVA_error ("adiff: unable to allocate sufficient memory");

  /*----- loop over user specified treatment differences -----*/
  for (idiff = 0;  idiff < num_diffs;  idiff++)
  {
      i = option_data->adiffs[idiff][0];
      j = option_data->adiffs[idiff][1];

      if (option_data->model == 1)
      {
          /*----- read first treatment level mean -----*/
          calculate_sum (option_data, i, -1, -1, diff);
          for (ixyz = 0;  ixyz < nxyz;  ixyz++)
            diff[ixyz] = diff[ixyz] / (b*c*n);

          /*----- subtract second treatment level mean -----*/
          calculate_sum (option_data, j, -1, -1, tdiff);
          for (ixyz = 0;  ixyz < nxyz;  ixyz++)
            diff[ixyz] -= tdiff[ixyz] / (b*c*n);

          volume_read ("sse", tdiff, nxyz);
          df = a * b * c * (n-1);

          /*----- divide by estimated standard deviation of difference -----*/
          fval = (1.0 / df) * (2.0 / (b*c*n));
          for (ixyz = 0;  ixyz < nxyz;  ixyz++)
          {
              stddev = sqrt (tdiff[ixyz] * fval);
              if (stddev < EPSILON) tdiff[ixyz] = 0.0;
              else                  tdiff[ixyz] = diff[ixyz] / stddev;
          }
      }
      else if (option_data->model == 4)        /* 8 Sep 2005 [rickr,gangc] */
      {
          /* implement diff as a contrast */
          for ( cindex = 0; cindex < a; cindex++ ) contr[cindex] = 0.0;
          contr[i] = 1.0;  contr[j] = -1.0;

          df = c - 1;  /* degrees of freedom from random variable only */
          calc_type4_acontr( option_data, contr, -1, df, diff, tdiff );
      }
      else  /* model == 5 */
      {
          for ( cindex = 0; cindex < a; cindex++ ) contr[cindex] = 0.0;
          contr[i] = 1.0;  contr[j] = -1.0;
          calc_type5_acontr(option_data, contr, -1, diff, tdiff);
          df = 2*(c-1);  /* weight of diff as contrast is 2 */
      }

      if (nvoxel > 0){
	printf ("Difference of factor A level %d - level %d = %f \n",
		i+1, j+1, diff[nvoxel-1]);
	printf ("t for difference of factor A level %d - level %d = %f \n",
		i+1, j+1, tdiff[nvoxel-1]);
      }

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->adname[idiff],
                       diff, tdiff, df, 0);
    }

  /*----- release memory -----*/
  free(tdiff);  free(diff);  free(contr);
}


/*---------------------------------------------------------------------------*/
/*
   Routine to estimate the difference in the means between two user specified
   treatment levels for factor B.  The output is a 2 sub-brick AFNI data set.
   The first sub-brick contains the estimated difference in the means.
   The second sub-brick contains the corresponding t-statistic.
*/

void calculate_bdifferences (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
  float * diff = NULL;                /* pointer to est. diff. in means */
  float * tdiff = NULL;               /* pointer to t-statistic data */
  float * contr;                      /* pointer to t-statistic data */
  int a;                              /* number of levels for factor A */
  int b;                              /* number of levels for factor B */
  int c;                              /* number of levels for factor C */
  int n;                              /* number of observations per cell */
  int ixyz, nxyz;                     /* voxel counters */
  int cindex;                         /* contrast index */
  int nvoxel;                         /* output voxel # */
  int num_diffs;                      /* number of user requested diffs. */
  int idiff;                          /* index for requested differences */
  int i, j;                           /* factor level indices */
  int df;                             /* degrees of freedom for t-test */
  float fval;                         /* for calculating std. dev. */
  float stddev;                       /* est. std. dev. of difference */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  num_diffs = option_data->num_bdiffs;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  diff = (float *) malloc(sizeof(float)*nxyz);
  tdiff = (float *) malloc(sizeof(float)*nxyz);
  contr = (float *) malloc(sizeof(float)*b);
  if ((diff == NULL) || (tdiff == NULL) || (contr == NULL))
    ANOVA_error ("bdiff: unable to allocate sufficient memory");

  /*----- loop over user specified treatment differences -----*/
  for (idiff = 0;  idiff < num_diffs;  idiff++)
  {
      i = option_data->bdiffs[idiff][0];
      j = option_data->bdiffs[idiff][1];

      if (option_data->model == 1)
      {
          /*----- read first treatment level mean -----*/
          calculate_sum (option_data, -1, i, -1, diff);
          for (ixyz = 0;  ixyz < nxyz;  ixyz++)
            diff[ixyz] = diff[ixyz] / (a*c*n);

          /*----- subtract second treatment level mean -----*/
          calculate_sum (option_data, -1, j, -1, tdiff);
          for (ixyz = 0;  ixyz < nxyz;  ixyz++)
            diff[ixyz] -= tdiff[ixyz] / (a*c*n);

          volume_read ("sse", tdiff, nxyz);
          df = a * b * c * (n-1);

          /*----- divide by estimated standard deviation of difference -----*/
          fval = (1.0 / df) * (2.0 / (a*c*n));
          for (ixyz = 0;  ixyz < nxyz;  ixyz++)
          {
              stddev = sqrt (tdiff[ixyz] * fval);
              if (stddev < EPSILON) tdiff[ixyz] = 0.0;
              else                  tdiff[ixyz] = diff[ixyz] / stddev;
          }
      }
      else if (option_data->model == 4)         /* 8 Sep 2005 [rickr,gangc] */
      {
          /* implement diff as a contrast */
          for ( cindex = 0; cindex < b; cindex++ ) contr[cindex] = 0.0;
          contr[i] = 1.0;  contr[j] = -1.0;

          df = c - 1;  /* degrees of freedom from random variable only */
          calc_type4_bcontr( option_data, contr, -1, df, diff, tdiff );
      }
      else  /* model == 5 (implement diff as a contrast) */
      {
          for ( cindex = 0; cindex < b; cindex++ ) contr[cindex] = 0.0;
          contr[i] = 1.0;  contr[j] = -1.0;

          calc_type5_bcontr(option_data, contr, -1, diff, tdiff);
          df = a * (c - 1);
      }

      if (nvoxel > 0){
	printf ("Difference of factor B level %d - level %d = %f \n",
		i+1, j+1, diff[nvoxel-1]);
	printf ("t for difference of factor B level %d - level %d = %f \n",
		i+1, j+1, tdiff[nvoxel-1]);
      }

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->bdname[idiff],
                       diff, tdiff, df, 0);
  }

  /*----- release memory -----*/
  free(tdiff);  free(diff);  free(contr);
}


/*---------------------------------------------------------------------------*/
/*
   Routine to estimate the difference in the means between two user specified
   treatment levels for factor C.  The output is a 2 sub-brick AFNI data set.
   The first sub-brick contains the estimated difference in the means.
   The second sub-brick contains the corresponding t-statistic.
*/

void calculate_cdifferences (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
  float * diff = NULL;                /* pointer to est. diff. in means */
  float * tdiff = NULL;               /* pointer to t-statistic data */
  int a;                              /* number of levels for factor A */
  int b;                              /* number of levels for factor B */
  int c;                              /* number of levels for factor C */
  int n;                              /* number of observations per cell */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */
  int num_diffs;                      /* number of user requested diffs. */
  int idiff;                          /* index for requested differences */
  int i, j;                           /* factor level indices */
  int df;                             /* degrees of freedom for t-test */
  float fval;                         /* for calculating std. dev. */
  float stddev;                       /* est. std. dev. of difference */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  num_diffs = option_data->num_cdiffs;
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

      /*----- read first treatment level mean -----*/
      i = option_data->cdiffs[idiff][0];
      calculate_sum (option_data, -1, -1, i, diff);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	diff[ixyz] = diff[ixyz] / (a*b*n);

      /*----- subtract second treatment level mean -----*/
      j = option_data->cdiffs[idiff][1];
      calculate_sum (option_data, -1, -1, j, tdiff);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	diff[ixyz] -= tdiff[ixyz] / (a*b*n);
      if (nvoxel > 0)
	printf ("Difference of factor C level %d - level %d = %f \n",
		i+1, j+1, diff[nvoxel-1]);

      /*----- standard deviation -----*/
      volume_read ("sse", tdiff, nxyz);
      df = a * b * c * (n-1);

      /*----- divide by estimated standard deviation of difference -----*/
      fval = (1.0 / df) * (2.0 / (a*b*n));
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  stddev = sqrt (tdiff[ixyz] * fval);
	  if (stddev < EPSILON)
	    tdiff[ixyz] = 0.0;
	  else
	    tdiff[ixyz] = diff[ixyz] / stddev;
	}

      if (nvoxel > 0)
	printf ("t for difference of factor C level %d - level %d = %f \n",
		i+1, j+1, tdiff[nvoxel-1]);

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->cdname[idiff],
                       diff, tdiff, df, 0);

    }

  /*----- release memory -----*/
  free (tdiff);   tdiff = NULL;
  free (diff);    diff = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to estimate the difference in the cell means between two user
   specified cells (treatments).  The output is a 2 sub-brick AFNI data set.
   The first sub-brick contains the estimated difference in the cell means.
   The second sub-brick contains the corresponding t-statistic.
*/

void calculate_xdifferences (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
  float * diff = NULL;                /* pointer to est. diff. in means */
  float * tdiff = NULL;               /* pointer to t-statistic data */
  int a;                              /* number of levels for factor A */
  int b;                              /* number of levels for factor B */
  int c;                              /* number of levels for factor C */
  int n;                              /* number of observations per cell */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */
  int num_diffs;                      /* number of user requested diffs. */
  int idiff;                          /* index for requested differences */
  int ia, ib, ic, ja, jb, jc;         /* cell indices */
  int df;                             /* degrees of freedom for t-test */
  float fval;                         /* for calculating std. dev. */
  float stddev;                       /* est. std. dev. of difference */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  df = a * b * c * (n-1);
  num_diffs = option_data->num_xdiffs;
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

      /*----- read first treatment level mean -----*/
      ia = option_data->xdiffs[idiff][0][0];
      ib = option_data->xdiffs[idiff][0][1];
      ic = option_data->xdiffs[idiff][0][2];
      calculate_sum (option_data, ia, ib, ic, diff);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	diff[ixyz] = diff[ixyz] / n;

      /*----- subtract second treatment level mean -----*/
      ja = option_data->xdiffs[idiff][1][0];
      jb = option_data->xdiffs[idiff][1][1];
      jc = option_data->xdiffs[idiff][1][2];
      calculate_sum (option_data, ja, jb, jc, tdiff);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	diff[ixyz] -= tdiff[ixyz] / n;
      if (nvoxel > 0)
	printf ("Difference Cell[%d][%d][%d] - Cell[%d][%d][%d] = %f \n",
		ia+1, ib+1, ic+1, ja+1, jb+1, jc+1, diff[nvoxel-1]);

      /*----- divide by estimated standard deviation of difference -----*/
      volume_read ("sse", tdiff, nxyz);
      fval = (1.0 / df) * (2.0 / n);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  stddev = sqrt (tdiff[ixyz] * fval);
	  if (stddev < EPSILON)
	    tdiff[ixyz] = 0.0;
	  else
	    tdiff[ixyz] = diff[ixyz] / stddev;
	}

      if (nvoxel > 0)
	printf ("t-stat for Cell[%d][%d][%d] - Cell[%d][%d][%d] = %f \n",
		ia+1, ib+1, ic+1, ja+1, jb+1, jc+1, tdiff[nvoxel-1]);

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->xdname[idiff],
                       diff, tdiff, df, 0);

    }

  /*----- release memory -----*/
  free (tdiff);   tdiff = NULL;
  free (diff);    diff = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to estimate a user specified contrast in treatment levels for
  factor A.  The output is stored as a 2 sub-brick AFNI data set.  The first
  sub-brick contains the estimated contrast.  The second sub-brick contains
  the corresponding t-statistic.
*/

void calculate_acontrasts (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
  float * contr = NULL;               /* pointer to contrast estimate */
  float * tcontr = NULL;              /* pointer to t-statistic data */
  int i, a;                           /* number of levels for factor A */
  int b;                              /* number of levels for factor B */
  int c;                              /* number of levels for factor C */
  int n;                              /* number of observations per cell */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */
  int num_contr;                      /* number of user requested contrasts */
  int icontr;                         /* index of user requested contrast */
  int level;                          /* factor level index */
  int df;                             /* degrees of freedom for t-test */
  float fval;                         /* for calculating std. dev. */
  float coef;                         /* contrast coefficient */
  float stddev;                       /* est. std. dev. of contrast */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
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
      if (option_data->model == 1)
      {
          volume_zero (contr, nxyz);
          fval = 0.0;

          for (level = 0;  level < a;  level++)
	  {
              coef = option_data->acontr[icontr][level];
              if (coef == 0.0) continue;

              /*----- add coef * treatment level mean to contrast -----*/
              calculate_sum (option_data, level, -1, -1, tcontr);
              fval += coef * coef / (b*c*n);
              for (ixyz = 0;  ixyz < nxyz;  ixyz++)
                  contr[ixyz] += coef * tcontr[ixyz] / (b*c*n);
	  }

          volume_read ("sse", tcontr, nxyz);
          df = a * b * c * (n-1);

          /*----- divide by estimated standard deviation of the contrast -----*/
          for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	  {
              stddev = sqrt ((tcontr[ixyz] / df) * fval);
              if (stddev < EPSILON) tcontr[ixyz] = 0.0;
              else                  tcontr[ixyz] = contr[ixyz] / stddev;
          }
      }
      else if (option_data->model == 4)         /* 8 Sep 2005 [rickr,gangc] */
      {
          df = c - 1;
          calc_type4_acontr(option_data, option_data->acontr[icontr], -1, df,
                            contr, tcontr );
      }
      else   /* model == 5 */
      {
          calc_type5_acontr(option_data, option_data->acontr[icontr], -1,
                            contr, tcontr);
          /* compute df = weight*(c-1) */
          for(i=0,df=0; i<a; i++) if(option_data->acontr[icontr][i]) df++;
          df *= c - 1;
      }

      if (nvoxel > 0) {
	printf ("No.%d contrast for factor A = %f \n",
		icontr+1, contr[nvoxel-1]);
	printf ("t of No.%d contrast for factor A = %f \n",
		icontr+1, tcontr[nvoxel-1]);
      }

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->acname[icontr],
                       contr, tcontr, df, 0);
  }

  /*----- release memory -----*/
  free(tcontr);  free(contr);
}

/*---------------------------------------------------------------------------*/
/*
  Routine to estimate a user specified contrast in treatment levels for
  factor B.  The output is stored as a 2 sub-brick AFNI data set.  The first
  sub-brick contains the estimated contrast.  The second sub-brick contains
  the corresponding t-statistic.
*/

void calculate_bcontrasts (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
  float * contr = NULL;               /* pointer to contrast estimate */
  float * tcontr = NULL;              /* pointer to t-statistic data */
  float * bcontr = NULL;              /* pointer to B contrast data */
  int a;                              /* number of levels for factor A */
  int b;                              /* number of levels for factor B */
  int c;                              /* number of levels for factor C */
  int n;                              /* number of observations per cell */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */
  int num_contr;                      /* number of user requested contrasts */
  int icontr;                         /* index of user requested contrast */
  int level;                          /* factor level index */
  int df;                             /* degrees of freedom for t-test */
  float fval;                         /* for calculating std. dev. */
  float coef;                         /* contrast coefficient */
  float stddev;                       /* est. std. dev. of contrast */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  num_contr = option_data->num_bcontr;
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
      bcontr = option_data->bcontr[icontr];  /* set a pointer to the contrast */

      if (option_data->model == 1)
      {
          volume_zero (contr, nxyz);
          fval = 0.0;

          for (level = 0;  level < b;  level++)
          {
              coef = bcontr[level];
              if (coef == 0.0) continue;

              /*----- add coef * treatment level mean to contrast -----*/
              calculate_sum (option_data, -1, level, -1, tcontr);
              fval += coef * coef / (a*c*n);
              for (ixyz = 0;  ixyz < nxyz;  ixyz++)
                  contr[ixyz] += coef * tcontr[ixyz] / (a*c*n);
	  }

          volume_read ("sse", tcontr, nxyz);
          df = a * b * c * (n-1);

          /*----- divide by estimated standard deviation of the contrast -----*/
          for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	  {
              stddev = sqrt ((tcontr[ixyz] / df) * fval);
              if (stddev < EPSILON) tcontr[ixyz] = 0.0;
              else                  tcontr[ixyz] = contr[ixyz] / stddev;
  	  }
      }
      else if (option_data->model == 4)         /* 8 Sep 2005 [rickr,gangc] */
      {
          df = c - 1;
          calc_type4_bcontr(option_data, bcontr, -1, df, contr, tcontr);
      }
      else   /* model == 5                        23 Sep 2005 [rickr,gangc] */
      {
        calc_type5_bcontr(option_data, bcontr, -1, contr, tcontr);
        df = a * (c - 1);
      }

      if (nvoxel > 0){
	printf ("No.%d contrast for factor B = %f \n",
		icontr+1, contr[nvoxel-1]);
	printf ("t of No.%d contrast for factor B = %f \n",
		icontr+1, tcontr[nvoxel-1]);
      }

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->bcname[icontr],
                       contr, tcontr, df, 0);
  }

  /*----- release memory -----*/
  free (tcontr);  free (contr);
}

/*---------------------------------------------------------------------------*/
/*
  Routine to estimate a user specified contrast in treatment levels for
  factor C.  The output is stored as a 2 sub-brick AFNI data set.  The first
  sub-brick contains the estimated contrast.  The second sub-brick contains
  the corresponding t-statistic.
*/

void calculate_ccontrasts (anova_options * option_data)
{
  const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
  float * contr = NULL;               /* pointer to contrast estimate */
  float * tcontr = NULL;              /* pointer to t-statistic data */
  int a;                              /* number of levels for factor A */
  int b;                              /* number of levels for factor B */
  int c;                              /* number of levels for factor C */
  int n;                              /* number of observations per cell */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */
  int num_contr;                      /* number of user requested contrasts */
  int icontr;                         /* index of user requested contrast */
  int level;                          /* factor level index */
  int df;                             /* degrees of freedom for t-test */
  float fval;                         /* for calculating std. dev. */
  float coef;                         /* contrast coefficient */
  float stddev;                       /* est. std. dev. of contrast */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  n = option_data->n;
  num_contr = option_data->num_ccontr;
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

      for (level = 0;  level < c;  level++)
	{
	  coef = option_data->ccontr[icontr][level];
	  if (coef == 0.0) continue;
	
	  /*----- add coef * treatment level mean to contrast -----*/
	  calculate_sum (option_data, -1, -1, level, tcontr);
	  fval += coef * coef / (a*b*n);
	  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	    contr[ixyz] += coef * tcontr[ixyz] / (a*b*n);
	}

      if (nvoxel > 0)
	printf ("No.%d contrast for factor C = %f \n",
		icontr+1, contr[nvoxel-1]);

      /*----- standard deviation -----*/
      volume_read ("sse", tcontr, nxyz);
      df = a * b * c * (n-1);

      /*----- divide by estimated standard deviation of the contrast -----*/
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  stddev = sqrt ((tcontr[ixyz] / df) * fval);
	  if (stddev < EPSILON)
	    tcontr[ixyz] = 0.0;
	  else
	    tcontr[ixyz] = contr[ixyz] / stddev;
	}

      if (nvoxel > 0)
	printf ("t of No.%d contrast for factor C = %f \n",
		icontr+1, tcontr[nvoxel-1]);

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->ccname[icontr],
                       contr, tcontr, df, 0);

    }

  /*----- release memory -----*/
  free (tcontr);   tcontr = NULL;
  free (contr);    contr = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to estimate a user specified 2nd order contrast in treatment levels
  for factor A, at a fixed B level.  The output is stored as a 2 sub-brick
  AFNI data set.  The first sub-brick contains the estimated contrast.  The
  second sub-brick contains the corresponding t-statistic.
*/

void calculate_aBcontrasts (anova_options * option_data)
{
  float * contr = NULL;               /* pointer to contrast estimate */
  float * tcontr = NULL;              /* pointer to t-statistic data */
  float * acontr = NULL;              /* local pointer to A contrast */
  int i, a;                           /* number of levels for factor A */
  int c;                              /* number of levels for factor C */
  int nxyz;                           /* number of voxels */
  int nvoxel;                         /* output voxel # */
  int num_contr;                      /* number of user requested contrasts */
  int icontr;                         /* index of user requested contrast */
  int Blevel;                         /* fixed B level for A contrast */
  int df;                             /* degrees of freedom for t-test */


  /*----- initialize local variables -----*/
  a = option_data->a;
  c = option_data->c;
  num_contr = option_data->num_aBcontr;
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
      acontr = option_data->aBcontr[icontr];
      Blevel = option_data->aBclevel[icontr];
      if (option_data->model == 4)
      {
          df = c - 1;
          calc_type4_acontr(option_data, acontr, Blevel, df, contr, tcontr );
      }
      else if (option_data->model == 5)
      {
          calc_type5_acontr(option_data, acontr, Blevel, contr, tcontr);
          /* compute df = weight*(c-1) */
          for(i=0,df=0; i<a; i++) if(acontr[i]) df++;
          df *= c - 1;
      }
      else /* (option_data->model == 1) */
      {
          fprintf(stderr,"** error: calc_aBcontr() should not allow model %d\n",
                         option_data->model);
          exit(1);
      }

      if (nvoxel > 0) {
	printf ("No.%d 2nd order contrast for factor A at B level %d = %f\n",
		icontr+1, Blevel+1, contr[nvoxel-1]);
	printf ("t of No.%d 2nd order contrast for factor A = %f\n",
		icontr+1, tcontr[nvoxel-1]);
      }

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->aBcname[icontr],
                       contr, tcontr, df, 0);
  }

  /*----- release memory -----*/
  free(tcontr);  free(contr);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to estimate a user specified contrast in treatment levels for
  factor B, at a fixed A level.  The output is stored as a 2 sub-brick
  AFNI data set.  The first sub-brick contains the estimated contrast.
  The second sub-brick contains the corresponding t-statistic.
*/

void calculate_Abcontrasts (anova_options * option_data)
{
  float * contr = NULL;               /* pointer to contrast estimate */
  float * tcontr = NULL;              /* pointer to t-statistic data */
  float * bcontr = NULL;              /* pointer to B contrast data */
  int a;                              /* number of levels for factor A */
  int b;                              /* number of levels for factor B */
  int c;                              /* number of levels for factor C */
  int nxyz;                           /* number of voxels */
  int nvoxel;                         /* output voxel # */
  int num_contr;                      /* number of user requested contrasts */
  int icontr;                         /* index of user requested contrast */
  int Alevel;                         /* fixed A level for B contrast */
  int df;                             /* degrees of freedom for t-test */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  c = option_data->c;
  num_contr = option_data->num_Abcontr;
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
      Alevel = option_data->Abclevel[icontr];
      bcontr = option_data->Abcontr[icontr]; /* set a pointer to the contrast */

      if (option_data->model == 4 || option_data->model == 5)
      {
        calc_type5_bcontr(option_data, bcontr, Alevel, contr, tcontr);
        df = c - 1;
      }
      else /* invalid */
      {
          fprintf(stderr,"** error: calc_Abcontr() should not allow model %d\n",
                         option_data->model);
          exit(1);
      }

      if (nvoxel > 0) {
	printf ("No.%d 2nd order contrast for factor B at A level %d = %f\n",
		icontr+1, Alevel+1, contr[nvoxel-1]);
	printf ("t of No.%d 2nd order contrast for factor B = %f\n",
		icontr+1, tcontr[nvoxel-1]);
      }

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->Abcname[icontr],
                       contr, tcontr, df, 0);
  }

  /*----- release memory -----*/
  free (tcontr);  free (contr);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to estimate the difference between 2 levels of factor A, at a fixed
  level of factor B.                                      19 Dec 2005 [rickr]

*/
void calculate_aBdiffs (anova_options * option_data)
{
  float * contr = NULL;               /* pointer to contrast estimate */
  float * tcontr = NULL;              /* pointer to t-statistic data */
  float * acontr = NULL;              /* treat A-diff as a contrast */
  int i, a;                           /* number of levels for factor A */
  int a1, a2;                         /* factor A levels in diff */
  int c;                              /* number of levels for factor C */
  int nxyz;                           /* number of voxels */
  int nvoxel;                         /* output voxel # */
  int num_diffs;                      /* number of user requested diffs */
  int idiff;                          /* index of user requested diff */
  int Blevel;                         /* fixed B level for A diff */
  int df;                             /* degrees of freedom for t-test */


  /*----- initialize local variables -----*/
  a = option_data->a;
  c = option_data->c;
  num_diffs = option_data->num_aBdiffs;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  contr  = (float *) malloc(sizeof(float)*nxyz);
  tcontr = (float *) malloc(sizeof(float)*nxyz);
  acontr = (float *) malloc(sizeof(float)*a);
  if ((contr == NULL) || (tcontr == NULL) || (acontr == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  /*----- loop over user specified constrasts -----*/
  for (idiff = 0;  idiff < num_diffs;  idiff++)
  {
      for( i=0; i<a; i++ ) acontr[i] = 0;    /* clear old contr */
      a1 = option_data->aBdiffs[idiff][0];
      a2 = option_data->aBdiffs[idiff][1];

      acontr[a1] =  1;  /* a1 minus a2, at given factor B level */
      acontr[a2] = -1;
      Blevel = option_data->aBdlevel[idiff];

      if (option_data->model == 4)
      {
          df = c - 1;
          calc_type4_acontr(option_data, acontr, Blevel, df, contr, tcontr );
      }
      else if (option_data->model == 5)
      {
          calc_type5_acontr(option_data, acontr, Blevel, contr, tcontr);
          df = 2 * (c - 1);  /* as with acontr, for 2 fixed factor A levels */
      }
      else /* (option_data->model == 1) */
      {
          fprintf(stderr,"** error: calc_aBdiffs() should not allow model %d\n",
                         option_data->model);
          exit(1);
      }

      if (nvoxel > 0) {
	printf ("No.%d aBdiff for A levels (%d,%d), B level (%d) = %f\n",
		idiff+1, a1, a2, Blevel+1, contr[nvoxel-1]);
	printf ("t of No.%d aBdiff = %f\n", idiff+1, tcontr[nvoxel-1]);
      }

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->aBdname[idiff],
                       contr, tcontr, df, 0);
  }

  /*----- release memory -----*/
  free(tcontr);  free(contr);  free(acontr);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to estimate the difference between 2 levels of factor B, at a fixed
  level of factor A.                                      19 Dec 2005 [rickr]

*/
void calculate_Abdiffs (anova_options * option_data)
{
  float * contr = NULL;               /* pointer to contrast estimate */
  float * tcontr = NULL;              /* pointer to t-statistic data */
  float * bcontr = NULL;              /* treat B-diff as a contrast */
  int i, b;                           /* number of levels for factor B */
  int b1, b2;                         /* factor B levels in diff */
  int c;                              /* number of levels for factor C */
  int nxyz;                           /* number of voxels */
  int nvoxel;                         /* output voxel # */
  int num_diffs;                      /* number of user requested diffs */
  int idiff;                          /* index of user requested diff */
  int Alevel;                         /* fixed A level for B diff */
  int df;                             /* degrees of freedom for t-test */


  /*----- initialize local variables -----*/
  b = option_data->b;
  c = option_data->c;
  num_diffs = option_data->num_Abdiffs;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  contr  = (float *) malloc(sizeof(float)*nxyz);
  tcontr = (float *) malloc(sizeof(float)*nxyz);
  bcontr = (float *) malloc(sizeof(float)*b);
  if ((contr == NULL) || (tcontr == NULL) || (bcontr == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  /*----- loop over user specified constrasts -----*/
  for (idiff = 0;  idiff < num_diffs;  idiff++)
  {
      for( i=0; i<b; i++ ) bcontr[i] = 0;    /* clear old contr */
      b1 = option_data->Abdiffs[idiff][0];
      b2 = option_data->Abdiffs[idiff][1];

      bcontr[b1] =  1;  /* b1 minus b2, at given factor B level */
      bcontr[b2] = -1;
      Alevel = option_data->Abdlevel[idiff];

      if (option_data->model == 4 || option_data->model == 5)
      {
          calc_type5_bcontr(option_data, bcontr, Alevel, contr, tcontr);
          df = c - 1;
      }
      else /* invalid */
      {
          fprintf(stderr,"** error: calc_Abdiffs() should not allow model %d\n",
                         option_data->model);
          exit(1);
      }

      if (nvoxel > 0) {
	printf ("No.%d Abdiff for A level (%d), B levels (%d,%d) = %f\n",
		idiff+1, Alevel+1, b1, b2, contr[nvoxel-1]);
	printf ("t of No.%d Abdiff = %f\n", idiff+1, tcontr[nvoxel-1]);
      }

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->Abdname[idiff],
                       contr, tcontr, df, 0);
  }

  /*----- release memory -----*/
  free(tcontr);  free(contr);  free(bcontr);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to estimate each mean given a fixed level a factor A and a fixed
  level of factor B.                                   19 Dec 2005 [rickr]

*/
void calculate_abmeans (anova_options * option_data)
{
  float * mean = NULL;                /* pointer to mean estimate */
  float * tmean = NULL;               /* pointer to t-statistic data */
  float * contr = NULL;               /* treat as trivial B contrast */
  int b, c;                           /* number of levels for factors B, C */
  int nxyz;                           /* number of voxels */
  int nvoxel;                         /* output voxel # */
  int num_means;                      /* number of user requested contrasts */
  int imean;                          /* index of user requested mean */
  int ib;                             /* index for b levels */
  int Alevel, Blevel;                 /* fixed levels for mean */
  int df;                             /* degrees of freedom for t-test */


  /*----- initialize local variables -----*/
  b = option_data->b;
  c = option_data->c;
  num_means = option_data->num_abmeans;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  mean  = (float *) malloc(sizeof(float)*nxyz);
  tmean = (float *) malloc(sizeof(float)*nxyz);
  contr = (float *) malloc(sizeof(float)*b);
  if ((mean == NULL) || (tmean == NULL) || (contr == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  /*----- check model type -----*/
  if (option_data->model != 4 && option_data->model != 5)
  {
      fprintf(stderr,"** error: calc_abmeans() should not allow model %d\n",
                     option_data->model);
      exit(1);
  }

  /*----- loop over user specified constrasts -----*/
  for (imean = 0;  imean < num_means;  imean++)
  {
      Alevel = option_data->abmeans[imean][0];
      Blevel = option_data->abmeans[imean][1];

      for( ib=0; ib<b; ib++ ) contr[ib] = 0;             /* clear old contr */
      contr[Blevel] = 1;            /* set the Blevel as a trivial contrast */

      calc_type5_bcontr(option_data, contr, Alevel, mean, tmean);
      df = c - 1;

      if (nvoxel > 0) {
	printf ("No.%d abmean for A level %d, B level %d = %f\n",
		imean+1, Alevel+1, Blevel+1, mean[nvoxel-1]);
	printf ("t of No.%d abmean = %f\n", imean+1, tmean[nvoxel-1]);
      }

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->abmname[imean],
                       mean, tmean, df, 0);
  }

  /*----- release memory -----*/
  free(mean);  free(tmean);  free(contr);
}


/*---------------------------------------------------------------------------*/
/*
   Routine to calculate sums and sums of squares for a three-factor ANOVA.
*/

void calculate_anova (anova_options * option_data)
{

  /*----- calculate various sum and sums of squares -----*/
  calculate_ss0 (option_data);
  calculate_ssi (option_data);
  calculate_ssj (option_data);
  if (option_data->model != 5)  calculate_ssk (option_data);
  calculate_ssij (option_data);
  calculate_ssik (option_data);
  if (option_data->model != 5)  calculate_ssjk (option_data);
  calculate_ssijk (option_data);
  if (option_data->n != 1)  calculate_ssijkm (option_data);


  /*-----  calculate total (corrected for the mean) sum of squares  -----*/
  calculate_ssto (option_data);
  if (option_data->n != 1)  volume_delete ("ssijkm");

  /*-----  calculate error sum of squares  -----*/
  if (option_data->n != 1)  calculate_sse (option_data);
  volume_delete ("ssijk");

  /*-----  calculate sum of squares due to A effect  -----*/
  calculate_ssa (option_data);
  volume_delete ("ssi");

  /*-----  calculate sum of squares due to B effect  -----*/
  calculate_ssb (option_data);
  volume_delete ("ssj");

  if (option_data->model != 5)
    {
      /*-----  calculate sum of squares due to C effect  -----*/
      calculate_ssc (option_data);
      volume_delete ("ssk");
    }

  /*-----  calculate sum of squares due to A*B interaction  -----*/
  calculate_ssab (option_data);
  volume_delete ("ssij");

  if (option_data->model != 5)
    /*-----  calculate sum of squares due to A*C interaction  -----*/
    calculate_ssac (option_data);
  else
    /*-----  calculate sum of squares due to C(A) effect  -----*/
    calculate_ssca (option_data);
  volume_delete ("ssik");

  if (option_data->model != 5)
    {
      /*-----  calculate sum of squares due to B*C interaction  -----*/
      calculate_ssbc (option_data);
      volume_delete ("ssjk");
    }

  volume_delete ("ss0");

  if (option_data->model != 5)
    /*-----  calculate sum of squares due to A*B*C interaction  -----*/
    calculate_ssabc (option_data);
  else
    /*-----  calculate sum of squares due to B*C(A) interaction  -----*/
    calculate_ssbca (option_data);
  volume_delete ("ssto");
}


/*---------------------------------------------------------------------------*/
/*
   Routine to analyze the results from a three-factor ANOVA.
*/

void analyze_results (anova_options * option_data)
{

   /*-----  calculate F-statistic for factor A effect  -----*/
   if (option_data->nfa)  calculate_fa (option_data);

   /*-----  calculate F-statistic for factor B effect  -----*/
   if (option_data->nfb)  calculate_fb (option_data);

   /*-----  calculate F-statistic for factor C effect  -----*/
   if (option_data->nfc)  calculate_fc (option_data);

   /*-----  calculate F-statistic for A*B interaction effect  -----*/
   if (option_data->nfab)  calculate_fab (option_data);

   /*-----  calculate F-statistic for A*C interaction effect  -----*/
   if (option_data->nfac)  calculate_fac (option_data);

   /*-----  calculate F-statistic for B*C interaction effect  -----*/
   if (option_data->nfbc)  calculate_fbc (option_data);

   /*-----  calculate F-statistic for A*B*C interaction effect  -----*/
   if (option_data->nfabc)  calculate_fabc (option_data);

   /*-----  estimate level means for factor A  -----*/
   /* the user may have requested the "old" calculation  22 Nov 2005 [rickr] */
   if (option_data->num_ameans){
      if (option_data->old_method) old_calculate_ameans (option_data);
      else                         calculate_ameans (option_data);
   }

   /*-----  estimate level means for factor B  -----*/
   if (option_data->num_bmeans){
      if (option_data->old_method) old_calculate_bmeans (option_data);
      else                         calculate_bmeans (option_data);
   }

   /*-----  estimate level means for factor C  -----*/
   if (option_data->num_cmeans)  calculate_cmeans (option_data);

   /*-----  estimate cell means  -----*/
   if (option_data->num_xmeans)  calculate_xmeans (option_data);

   /*-----  estimate level differences for factor A  -----*/
   if (option_data->num_adiffs){
      if (option_data->old_method) old_calculate_adifferences (option_data);
      else                         calculate_adifferences (option_data);
   }

   /*-----  estimate level differences for factor B  -----*/
   if (option_data->num_bdiffs){
      if (option_data->old_method) old_calculate_bdifferences (option_data);
      else                         calculate_bdifferences (option_data);
   }

   /*-----  estimate level differences for factor C  -----*/
   if (option_data->num_cdiffs)  calculate_cdifferences (option_data);

   /*-----  estimate differences in cell means  -----*/
   if (option_data->num_xdiffs)  calculate_xdifferences (option_data);

   /*-----  estimate level contrasts for factor A  -----*/
   if (option_data->num_acontr){
      if (option_data->old_method) old_calculate_acontrasts (option_data);
      else                         calculate_acontrasts (option_data);
   }

   /*-----  estimate level contrasts for factor B  -----*/
   if (option_data->num_bcontr){
      if (option_data->old_method) old_calculate_bcontrasts (option_data);
      else                         calculate_bcontrasts (option_data);
   }

   /*-----  estimate level contrasts for factor C  -----*/
   if (option_data->num_ccontr)  calculate_ccontrasts (option_data);

   /*-----  estimate level contrasts for factor A at fixed B level -----*/
   if (option_data->num_aBcontr) calculate_aBcontrasts (option_data);

   /*-----  estimate level contrasts for factor B at fixed A level -----*/
   if (option_data->num_Abcontr) calculate_Abcontrasts (option_data);

   /*-----  estimate level diffs for factor A at fixed B level -----*/
   if (option_data->num_aBdiffs) calculate_aBdiffs (option_data);

   /*-----  estimate level diffs for factor B at fixed A level -----*/
   if (option_data->num_Abdiffs) calculate_Abdiffs (option_data);

   /*-----  estimate means for fixed factor B level and fixed A level -----*/
   if (option_data->num_abmeans) calculate_abmeans (option_data);

}


/*---------------------------------------------------------------------------*/
/*
   Routine to create an AFNI "bucket" output dataset.
*/

void create_bucket (anova_options * option_data)

{
  char bucket_str[20000];             /* command line for program 3dbucket */
  char refit_str[20000];              /* command line for program 3drefit */
                                      /* (changed from 10K to 20K for Shruti) */
  THD_3dim_dataset * dset=NULL;       /* input afni data set pointer */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int i;                              /* file index */
  int ibrick;                         /* sub-brick index number */
  char str[100];                      /* temporary character string */


  /*----- read first dataset -----*/
  dset = THD_open_dataset (option_data->first_dataset) ;
  CHECK_OPEN_ERROR(dset,option_data->first_dataset) ;

       if( DSET_IS_1D(dset) ) USE_1D_filenames(1) ; /* 14 Mar 2003 */
  else if( DSET_IS_3D(dset) ) USE_1D_filenames(3) ; /* 21 Mar 2003 */


  /*----- make an empty copy of this dataset -----*/
  new_dset = EDIT_empty_copy( dset ) ;
  THD_delete_3dim_dataset (dset , False);   dset = NULL;
  EDIT_dset_items (new_dset,
		   ADN_directory_name, option_data->session,
		   ADN_none);


  /*----- begin command line for program 3dbucket -----*/
  strcpy (bucket_str, "3dbucket");
  strcat (bucket_str, " -prefix ");
  strcat (bucket_str, option_data->bucket_filename);


  /*----- begin command line for program 3drefit -----*/
  strcpy (refit_str, "3drefit ");
  ibrick = -1;


  /*----- make F-stat for factor A data sub-bricks -----*/
  if (option_data->nfa != 0)
    {
      add_file_name (new_dset, option_data->faname, bucket_str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:Inten ",
	       ibrick, option_data->faname);
      strcat (refit_str, str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:F-stat ",
	       ibrick, option_data->faname);
      strcat (refit_str, str);
    }


  /*----- make F-stat for factor B sub-bricks -----*/
  if (option_data->nfb != 0)
    {
      add_file_name (new_dset, option_data->fbname, bucket_str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:Inten ",
	       ibrick, option_data->fbname);
      strcat (refit_str, str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:F-stat ",
	       ibrick, option_data->fbname);
      strcat (refit_str, str);
    }


  /*----- make F-stat for factor C sub-bricks -----*/
  if (option_data->nfc != 0)
    {
      add_file_name (new_dset, option_data->fcname, bucket_str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:Inten ",
	       ibrick, option_data->fcname);
      strcat (refit_str, str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:F-stat ",
	       ibrick, option_data->fcname);
      strcat (refit_str, str);
    }


  /*----- make F-stat for A*B interaction sub-bricks -----*/
  if (option_data->nfab != 0)
    {
      add_file_name (new_dset, option_data->fabname, bucket_str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:Inten ",
	       ibrick, option_data->fabname);
      strcat (refit_str, str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:F-stat ",
	       ibrick, option_data->fabname);
      strcat (refit_str, str);
    }


  /*----- make F-stat for A*C interaction sub-bricks -----*/
  if (option_data->nfac != 0)
    {
      add_file_name (new_dset, option_data->facname, bucket_str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:Inten ",
	       ibrick, option_data->facname);
      strcat (refit_str, str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:F-stat ",
	       ibrick, option_data->facname);
      strcat (refit_str, str);
    }


  /*----- make F-stat for B*C interaction sub-bricks -----*/
  if (option_data->nfbc != 0)
    {
      add_file_name (new_dset, option_data->fbcname, bucket_str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:Inten ",
	       ibrick, option_data->fbcname);
      strcat (refit_str, str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:F-stat ",
	       ibrick, option_data->fbcname);
      strcat (refit_str, str);
    }


  /*----- make F-stat for A*B*C interaction sub-bricks -----*/
  if (option_data->nfabc != 0)
    {
      add_file_name (new_dset, option_data->fabcname, bucket_str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:Inten ",
	       ibrick, option_data->fabcname);
      strcat (refit_str, str);

      ibrick++;
      sprintf (str, " -sublabel %d %s:F-stat ",
	       ibrick, option_data->fabcname);
      strcat (refit_str, str);
    }


  /*----- make factor A level mean sub-bricks -----*/
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


  /*----- make factor B level mean sub-bricks -----*/
  if (option_data->num_bmeans > 0)
    for (i = 0; i < option_data->num_bmeans; i++)
      {
	add_file_name (new_dset, option_data->bmname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Mean ",
		 ibrick, option_data->bmname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ",
		 ibrick, option_data->bmname[i]);
	strcat (refit_str, str);
      }


  /*----- make factor C level mean sub-bricks -----*/
  if (option_data->num_cmeans > 0)
    for (i = 0; i < option_data->num_cmeans; i++)
      {
	add_file_name (new_dset, option_data->cmname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Mean ",
		 ibrick, option_data->cmname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ",
		 ibrick, option_data->cmname[i]);
	strcat (refit_str, str);
      }


  /*----- make individual cell mean sub-bricks -----*/
  if (option_data->num_xmeans > 0)
    for (i = 0; i < option_data->num_xmeans; i++)
      {
	add_file_name (new_dset, option_data->xmname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Mean ",
		 ibrick, option_data->xmname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ",
		 ibrick, option_data->xmname[i]);
	strcat (refit_str, str);
      }


  /*----- make difference in factor A level means sub-bricks -----*/
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


  /*----- make difference in factor B level means sub-bricks -----*/
  if (option_data->num_bdiffs > 0)
    for (i = 0; i < option_data->num_bdiffs; i++)
      {
	add_file_name (new_dset, option_data->bdname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Diff ",
		 ibrick, option_data->bdname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ",
		 ibrick, option_data->bdname[i]);
	strcat (refit_str, str);
      }


  /*----- make difference in factor C level means sub-bricks -----*/
  if (option_data->num_cdiffs > 0)
    for (i = 0; i < option_data->num_cdiffs; i++)
      {
	add_file_name (new_dset, option_data->cdname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Diff ",
		 ibrick, option_data->cdname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ",
		 ibrick, option_data->cdname[i]);
	strcat (refit_str, str);
      }


  /*----- make difference in cell means sub-bricks -----*/
  if (option_data->num_xdiffs > 0)
    for (i = 0; i < option_data->num_xdiffs; i++)
      {
	add_file_name (new_dset, option_data->xdname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Diff ",
		 ibrick, option_data->xdname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ",
		 ibrick, option_data->xdname[i]);
	strcat (refit_str, str);
      }


  /*----- make contrast in factor A level means sub-brickss -----*/
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


  /*----- make contrast in factor B level means sub-bricks -----*/
  if (option_data->num_bcontr > 0)
    for (i = 0; i < option_data->num_bcontr; i++)
      {
	add_file_name (new_dset, option_data->bcname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Contr ",
		 ibrick, option_data->bcname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ",
		 ibrick, option_data->bcname[i]);
	strcat (refit_str, str);
      }


  /*----- make contrast in factor C level means sub-bricks -----*/
  if (option_data->num_ccontr > 0)
    for (i = 0; i < option_data->num_ccontr; i++)
      {
	add_file_name (new_dset, option_data->ccname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Contr ",
		 ibrick, option_data->ccname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ",
		 ibrick, option_data->ccname[i]);
	strcat (refit_str, str);
      }


  /*----- make contrast in factor A for fixed B level -----*/
  if (option_data->num_aBcontr > 0)
    for (i = 0; i < option_data->num_aBcontr; i++)
      {
	add_file_name (new_dset, option_data->aBcname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Contr ",
		 ibrick, option_data->aBcname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ",
		 ibrick, option_data->aBcname[i]);
	strcat (refit_str, str);
      }


  /*----- make contrast in factor B for fixed A level -----*/
  if (option_data->num_Abcontr > 0)
    for (i = 0; i < option_data->num_Abcontr; i++)
      {
	add_file_name (new_dset, option_data->Abcname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Contr ",
		 ibrick, option_data->Abcname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ",
		 ibrick, option_data->Abcname[i]);
	strcat (refit_str, str);
      }


  /*----- add diffs in factor A for fixed B level -----*/
  if (option_data->num_aBdiffs > 0)
    for (i = 0; i < option_data->num_aBdiffs; i++)
      {
	add_file_name (new_dset, option_data->aBdname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Diff ",
		 ibrick, option_data->aBdname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ",
		 ibrick, option_data->aBdname[i]);
	strcat (refit_str, str);
      }


  /*----- add diffs in factor B for fixed A level -----*/
  if (option_data->num_Abdiffs > 0)
    for (i = 0; i < option_data->num_Abdiffs; i++)
      {
	add_file_name (new_dset, option_data->Abdname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Diff ",
		 ibrick, option_data->Abdname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ",
		 ibrick, option_data->Abdname[i]);
	strcat (refit_str, str);
      }


  /*----- add mean for fixed A and B levels -----*/
  if (option_data->num_abmeans > 0)
    for (i = 0; i < option_data->num_abmeans; i++)
      {
	add_file_name (new_dset, option_data->abmname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:mean ",
		 ibrick, option_data->abmname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ",
		 ibrick, option_data->abmname[i]);
	strcat (refit_str, str);
      }


  /*----- invoke program 3dbucket to generate bucket type output dataset
          by concatenating previous output files -----*/
  printf("Writing `bucket' dataset ");
  printf("into %s\n", option_data->bucket_filename);
  fprintf(stderr,"RUNNING COMMAND: %s\n",bucket_str) ;
  system (bucket_str);


  /*----- invoke program 3drefit to label individual sub-bricks -----*/
  add_file_name (new_dset, option_data->bucket_filename, refit_str);
  fprintf(stderr,"RUNNING COMMAND: %s\n",refit_str) ;
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
  int i;
  THD_3dim_dataset * dset=NULL;       /* input afni data set pointer */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */


  /*----- remove temporary data files -----*/
  if (option_data->n != 1)  volume_delete ("sse");
  volume_delete ("ssa");
  volume_delete ("ssb");
  volume_delete ("ssab");
  if (option_data->model != 5)
    {
      volume_delete ("ssc");
      volume_delete ("ssac");
      volume_delete ("ssbc");
      volume_delete ("ssabc");
    }
  else
    {
      volume_delete ("ssca");
      volume_delete ("ssbca");
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
      CHECK_OPEN_ERROR(dset,option_data->first_dataset) ;

      /*----- make an empty copy of this dataset -----*/
      new_dset = EDIT_empty_copy (dset);
      THD_delete_3dim_dataset (dset , False);   dset = NULL;
      EDIT_dset_items (new_dset,
		       ADN_directory_name, option_data->session,
		       ADN_none);

      /*----- remove F-stat for factor A main effect data file -----*/
      if (option_data->nfa != 0)
	remove_dataset (new_dset, option_data->faname);

      /*----- remove F-stat for factor B main effect data file -----*/
      if (option_data->nfb != 0)
	remove_dataset (new_dset, option_data->fbname);

      /*----- remove F-stat for factor C main effect data file -----*/
      if (option_data->nfc != 0)
	remove_dataset (new_dset, option_data->fcname);

      /*----- remove F-stat for A*B interaction data file -----*/
      if (option_data->nfab != 0)
	remove_dataset (new_dset, option_data->fabname);

      /*----- remove F-stat for A*C interaction data file -----*/
      if (option_data->nfac != 0)
	remove_dataset (new_dset, option_data->facname);

      /*----- remove F-stat for B*C interaction data file -----*/
      if (option_data->nfbc != 0)
	remove_dataset (new_dset, option_data->fbcname);

      /*----- remove F-stat for A*B*C interaction data file -----*/
      if (option_data->nfabc != 0)
	remove_dataset (new_dset, option_data->fabcname);

     /*----- remove factor A level mean data files -----*/
      if (option_data->num_ameans > 0)
	for (i = 0; i < option_data->num_ameans; i++)
	  remove_dataset (new_dset, option_data->amname[i]);

      /*----- remove factor B level mean data files -----*/
      if (option_data->num_bmeans > 0)
	for (i = 0; i < option_data->num_bmeans; i++)
	  remove_dataset (new_dset, option_data->bmname[i]);

      /*----- remove factor C level mean data files -----*/
      if (option_data->num_cmeans > 0)
	for (i = 0; i < option_data->num_cmeans; i++)
	  remove_dataset (new_dset, option_data->cmname[i]);

      /*----- remove individual cell mean data files -----*/
      if (option_data->num_xmeans > 0)
	for (i = 0; i < option_data->num_xmeans; i++)
	  remove_dataset (new_dset, option_data->xmname[i]);

      /*----- remove difference in factor A levels data files -----*/
      if (option_data->num_adiffs > 0)
	for (i = 0; i < option_data->num_adiffs; i++)
	  remove_dataset (new_dset, option_data->adname[i]);

      /*----- remove difference in factor B levels data files -----*/
      if (option_data->num_bdiffs > 0)
	for (i = 0; i < option_data->num_bdiffs; i++)
	  remove_dataset (new_dset, option_data->bdname[i]);

      /*----- remove difference in factor C levels data files -----*/
      if (option_data->num_cdiffs > 0)
	for (i = 0; i < option_data->num_cdiffs; i++)
	  remove_dataset (new_dset, option_data->cdname[i]);

      /*----- remove difference in cell means data files -----*/
      if (option_data->num_xdiffs > 0)
	for (i = 0; i < option_data->num_xdiffs; i++)
	  remove_dataset (new_dset, option_data->xdname[i]);

      /*----- remove contrast in factor A levels data files -----*/
      if (option_data->num_acontr > 0)
	for (i = 0; i < option_data->num_acontr; i++)
	  remove_dataset (new_dset, option_data->acname[i]);

      /*----- remove contrast in factor B levels data files -----*/
      if (option_data->num_bcontr > 0)
	for (i = 0; i < option_data->num_bcontr; i++)
	  remove_dataset (new_dset, option_data->bcname[i]);

      /*----- remove contrast in factor C levels data files -----*/
      if (option_data->num_ccontr > 0)
	for (i = 0; i < option_data->num_ccontr; i++)
	  remove_dataset (new_dset, option_data->ccname[i]);

      /*----- remove contrast in factor A for fixed B level -----*/
      if (option_data->num_aBcontr > 0)
	for (i = 0; i < option_data->num_aBcontr; i++)
	  remove_dataset (new_dset, option_data->aBcname[i]);

      /*----- remove contrast in factor B for fixed A level -----*/
      if (option_data->num_Abcontr > 0)
	for (i = 0; i < option_data->num_Abcontr; i++)
	  remove_dataset (new_dset, option_data->Abcname[i]);

      /*----- remove diff in factor A for fixed B level -----*/
      if (option_data->num_aBdiffs > 0)
	for (i = 0; i < option_data->num_aBdiffs; i++)
	  remove_dataset (new_dset, option_data->aBdname[i]);

      /*----- remove diff in factor B for fixed A level -----*/
      if (option_data->num_Abdiffs > 0)
	for (i = 0; i < option_data->num_Abdiffs; i++)
	  remove_dataset (new_dset, option_data->Abdname[i]);

      /*----- remove mean in fixed factor levels A and B -----*/
      if (option_data->num_abmeans > 0)
	for (i = 0; i < option_data->num_abmeans; i++)
	  remove_dataset (new_dset, option_data->abmname[i]);

      THD_delete_3dim_dataset (new_dset , False);   new_dset = NULL;
    }


  /*----- deallocate memory -----*/
  destroy_anova_options (option_data);

}


/*---------------------------------------------------------------------------*/
/*
   Three factor analysis of variance (ANOVA).
*/

int main (int argc, char ** argv)
{
  anova_options * option_data = NULL;


  /*----- Identify software -----*/
#if 0
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR);
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");
#endif

  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();

  /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

  mainENTRY("3dANOVA3 main") ; machdep() ; PRINT_VERSION("3dANOVA3") ; AUTHOR(PROGRAM_AUTHOR);
   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   /*----- program initialization -----*/
   initialize (argc, argv, &option_data);

   /*----- warn user (after any help) -----*/
   if( (option_data->model == 4 || option_data->model == 5)
       && !option_data->old_method )
       fprintf(stderr,"\n"
       "** Changes have been made for 3dANOVA3 computations of types 4 and 5.\n"
       "   For details, please see:\n"
       "   %s\n\n", ANOVA_MODS_LINK);

   /*----- calculate sums and sums of squares -----*/
   calculate_anova (option_data);

   /*----- generate requested output -----*/
   analyze_results (option_data);

   /*----- terminate program -----*/
   terminate (option_data);
   free (option_data);   option_data = NULL;

   exit(0);
}
