/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
   This program calculates two-factor analysis of variance (ANOVA)
   for 3 dimensional AFNI data sets.

   File:    3dANOVA2.c
   Author:  B. D. Ward
   Date:    09 December 1996

   Mod:     Incorporated header file 3dANOVA.h.
   Date:    15 January 1997

   Mod:     Major changes to sequence of calculations in order to reduce
            the disk space required to store temporary data files.
            Added option to check for required disk space.
   Date:    23 January 1997

   Mod:     Corrected problem with premature deletion of temporary file
            ssab.3danova2
   Date:    12 November 1997

   Mod:     Added protection against divide by zero.
   Date:    13 November 1997

   Mod:     Extensive changes required to implement the 'bucket' dataset.
   Date:    30 December 1997

   Mod:     Separated 3dANOVA.h and 3dANOVA.lib files.
   Date:    5 January 1998

   Mod:     Continuation of 'bucket' dataset changes.
   Date:    9 January 1998

   Mod:     Added software for statistical tests of individual cell means,
            cell differences, and cell contrasts.
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

   Mod:     -help menu modified.
   Date:    21 July 2005 - P Christidis

   Mod:     Update computation of sums of squares for all a contrasts,
            including trivial amean, and adiff.
   Date:    02 August 2005 [rickr, gangc]
   Date:    01 September 2005 [rickr, gangc]

   Mod:     Allow old (sphericity assuming) computations to be done via
            the -old_method option.  This applies to ameans, adiff and
            acontr for type 3 ANOVA, only.
   Date:    23 Nov 2005 [rickr]

   Mod:     The -old_method option requires -OK.
            Added the -assume_sph option and a check for validity of the
            contrasts (that they all sum to zero).  If valid, use old_method.
   Date:    01 Dec 2005 [rickr]

   Mod:     Wrote calc_type3_acontr() to replace calc_sum_sum2_acontr() and
            calc_t_from_sums(), to avoid an intermediate conversion to floats.
   Date:    04 Jan 2006 [rickr]
*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME    "3dANOVA2"                   /* name of this program */
#define PROGRAM_AUTHOR  "B. Douglas Ward"                  /* program author */
#define PROGRAM_INITIAL "09 Dec 1996"     /* date of initial program release */
#define PROGRAM_LATEST  "01 Sep 2005"     /* date of latest program revision */

/*---------------------------------------------------------------------------*/

#define SUFFIX ".3danova2"                /* suffix for temporary data files */

#include "3dANOVA.h"
#include "3dANOVA.lib"


/*---------------------------------------------------------------------------*/
/*
   Routine to display 3dANOVA2 help menu.
*/
void display_help_menu()
{
  printf
    (
 "This program performs a two-factor Analysis of Variance (ANOVA)\n"
 "on 3D datasets\n"
 "\n"
 "-----------------------------------------------------------\n"
 "\n"
 "Usage:\n"
 "\n"
 "   3dANOVA2\n"
 "      -type k              : type of ANOVA model to be used:\n"
 "                              k=1  fixed effects model  (A and B fixed)    \n"
 "                              k=2  random effects model (A and B random)   \n"
 "                              k=3  mixed effects model  (A fixed, B random)\n"
 "\n"
 "      -alevels a           : a = number of levels of factor A\n"
 "\n"
 "      -blevels b           : b = number of levels of factor B\n"
 "\n"
 "      -dset 1 1 filename   : data set for level 1 of factor A\n"
 "                                      and level 1 of factor B\n"
 "            . . .                           . . .\n"
 "      -dset i j filename   : data set for level i of factor A\n"
 "                                      and level j of factor B\n"
 "            . . .                           . . .\n"
 "      -dset a b filename   : data set for level a of factor A\n"
 "                                      and level b of factor B\n"
 "\n"
 "     [-voxel num]          : screen output for voxel # num\n"
 "\n"
 "     [-diskspace]          : print out disk space required for\n"
 "                             program execution\n"
 "\n"
 "\n"
 "   The following commands generate individual AFNI 2-sub-brick datasets:\n"
 "  (In each case, output is written to the file with the specified\n"
 "   prefix file name.)\n"
 "\n"
 "     [-ftr prefix]         : F-statistic for treatment effect\n"
 "\n"
 "     [-fa prefix]          : F-statistic for factor A effect\n"
 "\n"
 "     [-fb prefix]          : F-statistic for factor B effect\n"
 "\n"
 "     [-fab prefix]         : F-statistic for interaction\n"
 "\n"
 "     [-amean i prefix]     : estimate mean of factor A level i\n"
 "\n"
 "     [-bmean j prefix]     : estimate mean of factor B level j\n"
 "\n"
 "     [-xmean i j prefix]   : estimate mean of cell at level i of factor A,\n"
 "                                                      level j of factor B\n"
 "\n"
 "     [-adiff i j prefix]   : difference between levels i and j of factor A\n"
 "\n"
 "     [-bdiff i j prefix]   : difference between levels i and j of factor B\n"
 "\n"
 "     [-xdiff i j k l prefix]     : difference between cell mean at A=i,B=j\n"
 "                                                  and cell mean at A=k,B=l\n"
 "\n"
 "     [-acontr c1 ... ca prefix]  : contrast in factor A levels\n"
 "\n"
 "     [-bcontr c1 ... cb prefix]  : contrast in factor B levels\n"
 "\n"
 "     [-xcontr c11 ... c1b c21 ... c2b  ...  ca1 ... cab  prefix]\n"
 "                                 : contrast in cell means\n"
 "\n"
 "\n"
 "The following command generates one AFNI 'bucket' type dataset:\n"
 "\n"
 "     [-bucket prefix]      : create one AFNI 'bucket' dataset whose\n"
 "                             sub-bricks are obtained by concatenating\n"
 "                             the above output files; the output 'bucket'\n"
 "                             is written to file with prefix file name\n"
 "\n"
 "Modified ANOVA computation options:    (December, 2005)\n"
 "\n"
 "     ** These options apply to model type 3, only.\n"
 "        For details, see %s\n"
 "\n"
 "     [-old_method]        : request to perform ANOVA using the previous\n"
 "                            functionality (requires -OK, also)\n"
 "\n"
 "     [-OK]                : confirm you understand that contrasts that\n"
 "                            do not sum to zero have inflated t-stats, and\n"
 "                            contrasts that do sum to zero assume sphericity\n"
 "                            (to be used with -old_method)\n"
 "\n"
 "     [-assume_sph]        : assume sphericity (zero-sum contrasts, only)\n"
 "\n"
 "                            This allows use of the old_method for\n"
 "                            computing contrasts which sum to zero (this\n"
 "                            includes diffs, for instance).  Any contrast\n"
 "                            that does not sum to zero is invalid, and\n"
 "                            cannot be used with this option (such as\n"
 "                            ameans).\n"
 "\n"
 "----------------------------------------------------------\n"
 "\n"
 " Example of 3dANOVA2:\n"
 "\n"
 "      Example is based on a study with a 3 x 4 mixed factorial design:\n"
 "\n"
 "              Factor 1 - DONUTS has 3 levels:\n"
 "                         (1) chocolate, (2) glazed, (3) sugar\n"
 "\n"
 "              Factor 2 - SUBJECTS, of which there are 4 in this analysis:\n"
 "                         (1) fred, (2) ethel, (3) lucy, (4) ricky\n"
 "\n"
 " 3dANOVA2 -type 3 -alevels 3 -blevels 4   \\\n"
 "          -dset 1 1 fred_choc+tlrc        \\\n"
 "          -dset 2 1 fred_glaz+tlrc        \\\n"
 "          -dset 3 1 fred_sugr+tlrc        \\\n"
 "          -dset 1 2 ethel_choc+tlrc       \\\n"
 "          -dset 2 2 ethel_glaz+tlrc       \\\n"
 "          -dset 3 2 ethel_sugr+tlrc       \\\n"
 "          -dset 1 3 lucy_choc+tlrc        \\\n"
 "          -dset 2 3 lucy_glaz+tlrc        \\\n"
 "          -dset 3 3 lucy_sugr+tlrc        \\\n"
 "          -dset 1 3 ricky_choc+tlrc       \\\n"
 "          -dset 2 3 ricky_glaz+tlrc       \\\n"
 "          -dset 3 3 ricky_sugr+tlrc       \\\n"
 "          -amean 1 Chocolate              \\\n"
 "          -amean 2 Glazed                 \\\n"
 "          -amean 3 Sugar                  \\\n"
 "          -adiff 1 2 CvsG                 \\\n"
 "          -adiff 2 3 GvsS                 \\\n"
 "          -adiff 1 3 CvsS                 \\\n"
 "          -acontr 1 1 -2 CGvsS            \\\n"
 "          -acontr -2 1 1 CvsGS            \\\n"
 "          -acontr 1 -2 1 CSvsG            \\\n"
 "          -fa Donuts                      \\\n"
 "          -bucket ANOVA_results\n"
 "\n"
 " The -bucket option will place all of the 3dANOVA2 results (i.e., main\n"
 " effect of DONUTS, means for each of the 3 levels of DONUTS, and\n"
 " contrasts between the 3 levels of DONUTS) into one big dataset with\n"
 " multiple sub-bricks called ANOVA_results+tlrc.\n"
 "\n"
"-----------------------------------------------------------\n"
 "\n", ANOVA_MODS_LINK);

  printf
    (
 "\n"
 "N.B.: For this program, the user must specify 1 and only 1 sub-brick\n"
 "      with each -dset command. That is, if an input dataset contains\n"
 "      more than 1 sub-brick, a sub-brick selector must be used, e.g.:\n"
 "      -dset 2 4 'fred+orig[3]'\n"
     );

  printf("\n" MASTER_SHORTHELP_STRING ) ;

  printf
    (
 "\n"
 "Also see HowTo #5: Group Analysis on the AFNI website:\n"
 " http://afni.nimh.gov/pub/dist/HOWTO/howto/ht05_group/html/index.shtml\n"
 "\n"
    );

  printf(ANOVA_FLOAT_HELP) ;

  PRINT_COMPILE_DATE; exit(0);
}

/*---------------------------------------------------------------------------*/
/*
   Routine to get user specified anova options.
*/

void get_options (int argc, char ** argv, anova_options * option_data)
{
  int nopt = 1;                  /* input option argument counter */
  int ival, jval;                /* integer input */
  int i, j, k;                   /* factor level counters */
  int nij;                       /* number of data files in cell i,j */
  float fval;                    /* float input */
  THD_3dim_dataset * dset=NULL;             /* test whether data set exists */
  char message[MAX_NAME_LENGTH];            /* error message */
  int n[MAX_LEVELS][MAX_LEVELS];            /* data file counters */


  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();

  /*----- add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv);

  /*----- initialize the input options -----*/
  initialize_options (option_data);

  /*----- initialize data file counters -----*/
  for (i = 0;  i < MAX_LEVELS;  i++)
    for (j = 0;  j < MAX_LEVELS;  j++)
      n[i][j] = 0;


  /*----- main loop over input options -----*/
  while (nopt < argc)
    {

      /*----- allocate memory for storing data file names -----*/
      if ((option_data->xname == NULL) && (option_data->a > 0) &&
	  (option_data->b > 0))
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


      /*-----    -datum type   -----*/
      if( strncmp(argv[nopt],"-datum",5) == 0 ){
	if( ++nopt >= argc ) ANOVA_error("need an argument after -datum!") ;
	
	if( strcmp(argv[nopt],"short") == 0 ){
	  option_data->datum = MRI_short ;
	} else if( strcmp(argv[nopt],"float") == 0 ){
	  option_data->datum = MRI_float ;
	} else {
	  char buf[256] ;
	  sprintf(buf,"-datum of type '%s' is not supported in 3dANOVA2!",
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
	  if ((ival < 1) || (ival > 3))
	    ANOVA_error ("illegal argument after -type ");
	  option_data->model = ival;
	  nopt++;
	  continue;
	}


      /*------------------------------------------------------------*/
      /*-----  Using the old_method:      23 Nov 2005 [rickr]  -----*/
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

      /*-----  -OK: denote both OK and old_method by old_method = 3 -----*/
      if (strncmp(argv[nopt], "-OK", 3) == 0)
      {
         option_data->old_method |= 2;
         nopt++;
         continue;
      }

      /*-----  -assume_sph: denote assume_sphericity by old_method = 4 -----*/
      if (strncmp(argv[nopt], "-assume_sph", 11) == 0)
      {
         option_data->old_method |= 5;  /* also set -old_method bit */
         nopt++;
         continue;
      }

      /*------- end old_method checks ------------------------------*/
      /*------------------------------------------------------------*/


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


      /*-----   -dset alevel blevel filename   -----*/
      if (strncmp(argv[nopt], "-dset", 5) == 0)
	{
	  nopt++;
	  if (nopt+2 >= argc)  ANOVA_error ("need 3 arguments after -dset ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->a))
	    ANOVA_error ("illegal argument after -dset ");
	
	  nopt++;
	  sscanf (argv[nopt], "%d", &jval);
	  if ((jval <= 0) || (jval > option_data->b))
	    ANOVA_error ("illegal argument after -dset ");
	
	  n[ival-1][jval-1] += 1;
	  nij = n[ival-1][jval-1];
	  if (nij > MAX_OBSERVATIONS)
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
	
	  option_data->xname[ival-1][jval-1][0][nij-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->xname[ival-1][jval-1][0][nij-1], argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -ftr filename   -----*/
      if (strncmp(argv[nopt], "-ftr", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  ANOVA_error ("need argument after -ftr ");
	  option_data->nftr = 1;
	  option_data->ftrname = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->ftrname, argv[nopt]);
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


      /*-----   -xmean i j filename   -----*/
      if (strncmp(argv[nopt], "-xmean", 5) == 0)
	{
	  nopt++;
	  if (nopt+2 >= argc)  ANOVA_error ("need 3 arguments after -xmean ");
	
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


      /*-----   -xdiff i j k l filename   -----*/
      if (strncmp(argv[nopt], "-xdiff", 5) == 0)
	{
	  nopt++;
	  if (nopt+4 >= argc)  ANOVA_error ("need 5 arguments after -xdiff ");
	
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
	  if ((ival <= 0) || (ival > option_data->a))
	    ANOVA_error ("illegal argument after -xdiff ");
	  option_data->xdiffs[option_data->num_xdiffs-1][1][0] = ival - 1;
	  nopt++;
	
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival <= 0) || (ival > option_data->b))
	    ANOVA_error ("illegal argument after -xdiff ");
	  option_data->xdiffs[option_data->num_xdiffs-1][1][1] = ival - 1;
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


      /*--- -xcontr  c11 ... c1b  c21 ... c2b  ...  ca1 ... cab  filename ---*/
      if (strncmp(argv[nopt], "-xcontr", 5) == 0)
	{
	  nopt++;
	  if (nopt + (option_data->a * option_data->b) >= argc)
            ANOVA_error ("need ab+1 arguments after -xcontr ");
	
	  option_data->num_xcontr++;
	  if (option_data->num_xcontr > MAX_CONTR)
	    ANOVA_error ("too many cell means contrasts");
	  	
	  for (i = 0;  i < option_data->a;  i++)
	    for (j = 0;  j < option_data->b;  j++)
	      {
		sscanf (argv[nopt], "%f", &fval);
		option_data->xcontr[option_data->num_xcontr - 1][i][j] = fval ;
		nopt++;
	      }
	
	  option_data->xcname[option_data->num_xcontr-1]
	    =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->xcname[option_data->num_xcontr-1], argv[nopt]);
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


  /*----- check that all treatment sample sizes are equal -----*/
  option_data->n = n[0][0];
  for (i = 0;  i < option_data->a;  i++)
    for (j = 0;  j < option_data->b;  j++)
      if (n[i][j] != option_data->n)
	ANOVA_error ("must have equal sample sizes for 3dANOVA2");

  /*----- checks on -old_method -----*/
  if (option_data->old_method)
  {
    if (option_data->model != 3 )
      ANOVA_error("currently, -old_method applies to model type 3, only");
    if (option_data->old_method == 1 )
      ANOVA_error("-old_method is insufficient by itself");
  }
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
   check_one_temporary_file ("ssij");
   check_one_temporary_file ("ssijk");

   check_one_temporary_file ("sse");
   check_one_temporary_file ("sstr");
   check_one_temporary_file ("ssa");
   check_one_temporary_file ("ssb");
   check_one_temporary_file ("ssab");
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

  if (option_data->nfa > 0)
    check_one_output_file (option_data, option_data->faname);

  if (option_data->nfb > 0)
    check_one_output_file (option_data, option_data->fbname);

  if (option_data->nfab > 0)
    check_one_output_file (option_data, option_data->fabname);

  if (option_data->num_ameans > 0)
    for (i = 0;  i < option_data->num_ameans;  i++)
      check_one_output_file (option_data, option_data->amname[i]);

  if (option_data->num_bmeans > 0)
    for (i = 0;  i < option_data->num_bmeans;  i++)
      check_one_output_file (option_data, option_data->bmname[i]);

  if (option_data->num_xmeans > 0)
    for (i = 0;  i < option_data->num_xmeans;  i++)
      check_one_output_file (option_data, option_data->xmname[i]);

  if (option_data->num_adiffs > 0)
    for (i = 0;  i < option_data->num_adiffs;  i++)
      check_one_output_file (option_data, option_data->adname[i]);

  if (option_data->num_bdiffs > 0)
    for (i = 0;  i < option_data->num_bdiffs;  i++)
      check_one_output_file (option_data, option_data->bdname[i]);

  if (option_data->num_xdiffs > 0)
    for (i = 0;  i < option_data->num_xdiffs;  i++)
      check_one_output_file (option_data, option_data->xdname[i]);

  if (option_data->num_acontr > 0)
    for (i = 0;  i < option_data->num_acontr;  i++)
      check_one_output_file (option_data, option_data->acname[i]);

  if (option_data->num_bcontr > 0)
    for (i = 0;  i < option_data->num_bcontr;  i++)
      check_one_output_file (option_data, option_data->bcname[i]);

  if (option_data->num_xcontr > 0)
    for (i = 0;  i < option_data->num_xcontr;  i++)
      check_one_output_file (option_data, option_data->xcname[i]);

  if (option_data->bucket_filename != NULL)
    check_one_output_file (option_data, option_data->bucket_filename);

}


/*---------------------------------------------------------------------------*/
/*
  Routine to check for valid inputs.
*/

void check_for_valid_inputs (anova_options * option_data)
{
   int a, b;                            /* number of factor levels */
   int n;                               /* number of observations per cell */


   /*----- initialize local variables  -----*/
   a = option_data->a;
   b = option_data->b;
   n = option_data->n;


  /*----- check for valid inputs -----*/
   if (a < 2)  ANOVA_error ("must specify number of factor A levels (a>1) ");
   if (b < 2)  ANOVA_error ("must specify number of factor B levels (b>1) ");
   if (n < 1)  ANOVA_error ("sample size is too small");

   switch (option_data->model)
   {
      case 1:   /* fixed effects */
	 if (n == 1)
	    ANOVA_error ("sample size is too small for fixed effects model");
	 break;
      case 2:   /* random effects */
	 if (option_data->nftr > 0)
	    ANOVA_error ("-ftr is inappropriate for random effects model");
	 if (option_data->num_ameans > 0)
	    ANOVA_error ("-amean is inappropriate for random effects model");
	 if (option_data->num_bmeans > 0)
	    ANOVA_error ("-bmean is inappropriate for random effects model");
	 if (option_data->num_xmeans > 0)
	    ANOVA_error ("-xmean is inappropriate for random effects model");
	 if (option_data->num_adiffs > 0)
	    ANOVA_error ("-adiff is inappropriate for random effects model");
	 if (option_data->num_bdiffs > 0)
	    ANOVA_error ("-bdiff is inappropriate for random effects model");
	 if (option_data->num_xdiffs > 0)
	    ANOVA_error ("-xdiff is inappropriate for random effects model");
	 if (option_data->num_acontr > 0)
	    ANOVA_error ("-acontr is inappropriate for random effects model");
	 if (option_data->num_bcontr > 0)
	    ANOVA_error ("-bcontr is inappropriate for random effects model");
	 if (option_data->num_xcontr > 0)
	    ANOVA_error ("-xcontr is inappropriate for random effects model");
	 if ((n == 1) && (option_data->nfab > 0))
	    ANOVA_error ("sample size too small to calculate F-interaction");
	 break;
      case 3:   /* mixed effects */
	 if (option_data->nftr > 0)
	    ANOVA_error ("-ftr is inappropriate for mixed effects model");
	 if (option_data->num_bmeans > 0)
	    ANOVA_error ("-bmean is inappropriate for mixed effects model");
	 if (option_data->num_xmeans > 0)
	    ANOVA_error ("-xmean is inappropriate for mixed effects model");
	 if (option_data->num_bdiffs > 0)
	    ANOVA_error ("-bdiff is inappropriate for mixed effects model");
	 if (option_data->num_xdiffs > 0)
	    ANOVA_error ("-xdiff is inappropriate for mixed effects model");
	 if (option_data->num_bcontr > 0)
	    ANOVA_error ("-bcontr is inappropriate for mixed effects model");
	 if (option_data->num_xcontr > 0)
	    ANOVA_error ("-xcontr is inappropriate for mixed effects model");
	 if ((n == 1) && (option_data->nfab > 0))
	    ANOVA_error ("sample size too small to calculate F-interaction");
	 if ((n == 1) && (option_data->nfb > 0))
	    ANOVA_error ("sample size too small to calculate F for B effect");

         /* this check only applies to type 3 at the moment... */
         /* check contrasts (show errors, and specify ANOVA-2) */
         if ( !contrasts_are_valid(option_data, 1, 2) )
            ANOVA_error("invalid contrast(s)");

	 break;
   }
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the number of data files that have to be stored.
  Modified to account for 'bucket' dataset output.
*/

int required_data_files (anova_options * option_data)
{
  int now;                         /* number of disk files just prior
				      to program termination */
  int nout;                        /* number of output files */
  int nmax;                        /* maximum number of disk files */


  if (option_data->n != 1)
    {
      nmax = 6;  now = 5;
    }
  else
    {
      nmax = 5;  now = 5;
    }

  nout = option_data->nftr + option_data->nfab
  + option_data->nfa + option_data->nfb
  + option_data->num_ameans + option_data->num_bmeans + option_data->num_xmeans
  + option_data->num_adiffs + option_data->num_bdiffs + option_data->num_xdiffs
  + option_data->num_acontr + option_data->num_bcontr
  + option_data->num_xcontr;

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
  int a, b;                            /* number of factor levels */
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
  n = (*option_data)->n;

  /*----- total number of observations -----*/
  (*option_data)->nt = n * a * b;

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
		    int ii, int jj, float * ysum)
{
  float * y = NULL;                /* pointer to input data */
  int i, itop, ibot;               /* factor A level index */
  int j, jtop, jbot;               /* factor B level index */
  int m;                           /* observation number index */
  int a;                           /* number of levels for factor A */
  int b;                           /* number of levels for factor B */
  int n;                           /* number of observations per cell */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */
  char sum_label[MAX_NAME_LENGTH]; /* name of sum for print to screen */
  char str[MAX_NAME_LENGTH];       /* temporary string */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
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


  volume_zero (ysum, nxyz);

  /*-----  loop over levels of factor A  -----*/
  for (i = ibot;  i < itop;  i++)
    {
      /*-----  loop over levels of factor B  -----*/
      for (j = jbot;  j < jtop;  j++)
	{
	  /*----- sum observations within this cell -----*/	
	  for (m = 0;  m < n;  m++)
	    {
	      read_afni_data (option_data,
			      option_data->xname[i][j][0][m], y);
	      if (nvoxel > 0)
		printf ("y[%d][%d][%d] = %f \n",
			i+1, j+1, m+1, y[nvoxel-1]);
	      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
		ysum[ixyz] += y[ixyz];
	    } /* m */
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
      printf ("%s[.] = %f \n", sum_label, ysum[nvoxel-1]);
    }

  /*----- release memory -----*/
  free (y);     y = NULL;

}


/*---------------------------------------------------------------------------*/
/*                                                        03 Jan 2006 [rickr]

  Routine to compute the mean and t-stat for a type 3 A contrast.

      mean = sum_over_j[ (sum_over_i_in_contrast[c_i * y_i_j])   ] / b
      sum2 = sum_over_j[ (sum_over_i_in_contrast[c_i * y_i_j])^2 ]

      t = mean * sqrt[     df ( df + 1 )      ]
                     [ ---------------------- ]
                     [ sum_sq - (df+1)*mean^2 ]


  Note that y_i_j is the mean over k, i.e. 1/K*sum_over_k[y_i_j_k].

  For accuracy, results are computed using doubles, then copied to float.
  The contrast is passed in to allow for more uses of this function.
*/

void calc_type3_acontr(anova_options *option_data, float *acontr, int df,
                       float *mean, float * tmean)
{
  const float  EPSILON = 1.0e-15;  /* protect against divide by zero */
  double * dsum, * dsum2;          /* cumulative sums, for accuracy */
  double * dcontr;                 /* current contrast sum */
  double   dval, dmean;            /* for ease in computations */
  int i, j;                        /* indices for levels of factors A and B */
  int a, b;                        /* number of levels for factors A and B */
  int n;                           /* number of observations per cell */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel, df_prod;             /* output voxel #, df*(df+1) */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  dsum = (double *) malloc(sizeof(double)*nxyz);
  dsum2 = (double *) malloc(sizeof(double)*nxyz);
  dcontr = (double *) malloc(sizeof(double)*nxyz);
  if (dsum == NULL || dsum2 == NULL || dcontr == NULL)
      ANOVA_error ("calc_sum_sq_acontr: unable to allocate sufficient memory");

  for (ixyz = 0; ixyz < nxyz; ixyz++)  /* init sums to zero */
      dsum[ixyz] = dsum2[ixyz] = 0.0;

  /*-----  loop over factor B levels and repeated measures  -----*/
  for ( j = 0; j < b; j++ )
  {
      for (ixyz = 0; ixyz < nxyz; ixyz++)
          dcontr[ixyz] = 0.0;

      /*-----  compute contrast for the current j -----*/
      for (i = 0;  i < a;  i++)
      {
          if (acontr[i] == 0.0 ) continue;  /* then skip this index */

          /* get sum over k for mean (cheat, using sum for memory) */
          calculate_sum(option_data, i, j, mean);
          for (ixyz = 0; ixyz < nxyz; ixyz++)
              dcontr[ixyz] += acontr[i] * (double)mean[ixyz] / n;
      }

      /*-----  tally sum of contrast and squares for the current j -----*/
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
      dmean = dsum[ixyz] / b;     /* store single mean for efficiency */
      mean[ixyz] = dmean;         /* copy mean to float output */

      dval = dsum2[ixyz] - (df+1) * dmean * dmean;
      if (dval < EPSILON) tmean[ixyz] = 0.0;
      else                tmean[ixyz] = dmean * sqrt( df_prod / dval );
  }

  /*----- release memory -----*/
  free (dsum);  free (dsum2);  free (dcontr);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate SS0.  Result is stored in temporary output file
  ss0.3danova2.
*/

void calculate_ss0 (anova_options * option_data)
{
  float * ss0 = NULL;              /* pointer to output data */
  float * ysum = NULL;             /* pointer to sum over all observations */
  int a;                           /* number of levels for factor A */
  int b;                           /* number of levels for factor B */
  int n;                           /* number of observations per cell */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */
  int nval;                        /* divisor of sum */
  char filename[MAX_NAME_LENGTH];  /* name of output file */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;
  nval = a * b * n;

  /*----- allocate memory space for calculations -----*/
  ss0 = (float *) malloc(sizeof(float)*nxyz);
  ysum = (float *) malloc(sizeof(float)*nxyz);
  if ((ss0 == NULL) || (ysum == NULL))
    ANOVA_error ("unable to allocate sufficient memory");

  /*----- sum over all observations -----*/
  calculate_sum (option_data, -1, -1, ysum);

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
  ssi.3danova2.
*/

void calculate_ssi (anova_options * option_data)
{
  float * ssi = NULL;              /* pointer to output data */
  float * ysum = NULL;             /* pointer to sum over observations */
  int a;                           /* number of levels for factor A */
  int b;                           /* number of levels for factor B */
  int n;                           /* number of observations per cell */
  int i;                           /* index for factor A levels */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */
  int nval;                        /* divisor of sum */
  char filename[MAX_NAME_LENGTH];  /* name of output file */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;
  nval = b * n;

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
      calculate_sum (option_data, i, -1, ysum);

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
  ssj.3danova2.
*/

void calculate_ssj (anova_options * option_data)
{
  float * ssj = NULL;              /* pointer to output data */
  float * ysum = NULL;             /* pointer to sum over observations */
  int a;                           /* number of levels for factor A */
  int b;                           /* number of levels for factor B */
  int n;                           /* number of observations per cell */
  int j;                           /* index for factor B levels */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */
  int nval;                        /* divisor of sum */
  char filename[MAX_NAME_LENGTH];  /* name of output file */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;
  nval = a * n;

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
      calculate_sum (option_data, -1, j, ysum);

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
  Routine to calculate SSIJ.  Result is stored in temporary output file
  ssij.3danova2.
*/

void calculate_ssij (anova_options * option_data)
{
  float * ssij = NULL;             /* pointer to output data */
  float * ysum = NULL;             /* pointer to sum over observations */
  int a;                           /* number of levels for factor A */
  int b;                           /* number of levels for factor B */
  int n;                           /* number of observations per cell */
  int i, j;                        /* indices for factor A and B levels */
  int ixyz, nxyz;                  /* voxel counters */
  int nvoxel;                      /* output voxel # */
  int nval;                        /* divisor of sum */
  char filename[MAX_NAME_LENGTH];  /* name of output file */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;
  nval = n;

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
	  calculate_sum (option_data, i, j, ysum);
	
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
  Routine to sum the squares of all observations.
  The sum is stored (temporarily) in disk file ssijk.3danova2.
*/

void calculate_ssijk (anova_options * option_data)
{
  float * ssijk = NULL;            /* pointer to output data */
  float * y = NULL;                 /* pointer to input data */
  int i;                            /* factor A level index */
  int j;                            /* factor B level index */
  int m;                            /* observation number index */
  int a;                            /* number of levels for factor A */
  int b;                            /* number of levels for factor B */
  int n;                            /* number of observations per cell */
  int ixyz, nxyz;                   /* voxel counters */
  int nvoxel;                       /* output voxel # */


  /*----- initialize local variables -----*/
  a = option_data->a;
  b = option_data->b;
  n = option_data->n;
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  ssijk = (float *) malloc(sizeof(float)*nxyz);
  y = (float *) malloc(sizeof(float)*nxyz);
  if ((ssijk == NULL) || (y == NULL))
    ANOVA_error ("unable to allocate sufficient memory");


  volume_zero (ssijk, nxyz);

  for (i = 0;  i < a;  i++)
    {
      for (j = 0;  j < b;  j++)
	{
	  for (m = 0;  m < n;  m++)
	    {
	      read_afni_data (option_data,
			      option_data->xname[i][j][0][m], y);
	
	      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
		ssijk[ixyz] += y[ixyz] * y[ixyz];
	    }
	}
    }


  /*----- save the sum -----*/
  if (nvoxel > 0)
    printf ("SSIJK = %f \n", ssijk[nvoxel-1]);
  volume_write ("ssijk", ssijk, nxyz);

  /*----- release memory -----*/
  free (y);       y = NULL;
  free (ssijk);   ssijk = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the error sum of squares (SSE).
  The output is stored (temporarily) in file sse.3danova2.
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
  volume_read ("ssijk", sse, nxyz);

  volume_read ("ssij", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    sse[ixyz] -= y[ixyz];


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
  Routine to calculate the treatment sum of squares (SSTR).
  The output is stored (temporarily) in file sstr.3danova2.
*/

void calculate_sstr (anova_options * option_data)
{
  float * y = NULL;                   /* input data pointer */
  float * sstr = NULL;                /* sstr data pointer */
  int ixyz, nxyz;                     /* voxel counters */
  int nvoxel;                         /* output voxel # */


  /*----- assign local variables -----*/
  nxyz = option_data->nxyz;
  nvoxel = option_data->nvoxel;

  /*----- allocate memory space for calculations -----*/
  sstr = (float *) malloc (sizeof(float)*nxyz);
  y = (float *) malloc (sizeof(float)*nxyz);
  if ((y == NULL) || (sstr == NULL))
    ANOVA_error ("unable to allocate sufficient memory");


  /*----- calculate SSTR -----*/
  volume_read ("ssij", sstr, nxyz);

  volume_read ("ss0", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    sstr[ixyz] -= y[ixyz];


  /*----- protection against round-off error -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (sstr[ixyz] < 0.0)  sstr[ixyz] = 0.0;

  /*----- save error sum of squares -----*/
  if (nvoxel > 0)
    printf ("SSTR = %f \n", sstr[nvoxel-1]);
  volume_write ("sstr", sstr, nxyz);

  /*----- release memory -----*/
  free (y);      y = NULL;
  free (sstr);   sstr = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the sum of squares due to factor A (SSA).
  The output is stored (temporarily) in file ssa.3danova2.
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
  The output is stored (temporarily) in file ssb.3danova2.
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
  Routine to calculate the A*B interaction sum of squares (SSAB).
  The output is stored (temporarily) in file ssab.3danova2.
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
  volume_read ("sstr", ssab, nxyz);

  volume_read ("ssa", y, nxyz);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    ssab[ixyz] -= y[ixyz];

  volume_read ("ssb", y, nxyz);
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
   Routine to calculate the F-statistic for treatment, F = MSTR / MSE,
      where MSTR = SSTR / (ab-1),
      and   MSE  = SSE  / (ab(n-1)).

   The output is stored as a 2 sub-brick AFNI data set.  The first sub-brick
   contains the square root of MSTR (mean sum of squares due to treatment),
   and the second sub-brick contains the corresponding F-statistic.
*/

void calculate_ftr (anova_options * option_data)
{
   const float  EPSILON = 1.0e-10;      /* protect against divide by zero */
   float * mstr = NULL;                 /* pointer to MSTR data */
   float * ftr = NULL;                  /* pointer to F due-to-treatment */
   int a;                               /* number of levels for factor A */
   int b;                               /* number of levels for factor B */
   int n;                               /* number of observations per cell */
   int ixyz, nxyz;                      /* voxel counters */
   int nvoxel;                          /* output voxel # */
   float fval;                          /* float value used in calculations */
   float mse;                           /* mean square error */



   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
   n = option_data->n;
   nxyz = option_data->nxyz;
   nvoxel = option_data->nvoxel;

   /*----- allocate memory space for calculations -----*/
   ftr = (float *) malloc(sizeof(float)*nxyz);
   mstr = (float *) malloc(sizeof(float)*nxyz);
   if ((ftr == NULL) || (mstr == NULL))
      ANOVA_error ("unable to allocate sufficient memory");

   /*----- calculate mean SS due to treatments -----*/
   volume_read ("sstr", mstr, nxyz);
   fval = 1.0 / (a*b - 1.0);
   for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      mstr[ixyz] = mstr[ixyz] * fval;     /*---   MSTR = SSTR / (ab-1)  ---*/
   if (nvoxel > 0)
      printf ("MSTR = %f \n", mstr[nvoxel-1]);

   /*----- calculate F-statistic    -----*/
   volume_read ("sse", ftr, nxyz);
   fval = 1.0 / (a * b * (n-1));
   for (ixyz = 0;  ixyz < nxyz;  ixyz++)
     {
       mse = ftr[ixyz] * fval;            /*---  MSE = SSE / (ab(n-1))  ---*/
       if (mse < EPSILON)
	 ftr[ixyz] = 0.0;
       else
	 ftr[ixyz] = mstr[ixyz] / mse;      /*---  F = MSTR / MSE  ---*/
     }
   if (nvoxel > 0)
      printf ("FTR = %f \n", ftr[nvoxel-1]);

   /*----- write out afni data file -----*/
   for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      mstr[ixyz] = sqrt(mstr[ixyz]);      /*-- mstr now holds square root --*/
   write_afni_data (option_data, option_data->ftrname,
		    mstr, ftr, a*b-1, a*b*(n-1));

   /*----- this data file is no longer needed -----*/
   volume_delete ("sstr");

   /*----- release memory -----*/
   free (mstr);   mstr = NULL;
   free (ftr);    ftr = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to calculate the F-statistic for factor A.

   For fixed effects model:
      F = MSA / MSE,
         where MSA  = SSA / (a-1),
         and   MSE  = SSE / (ab(n-1)).

   For random effects model or mixed effects model:
      F = MSA / MSAB,
         where MSA  = SSA  / (a-1),
         and   MSAB = SSAB / ((a-1)(b-1)).

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
   int n;                               /* number of observations per cell */
   int ixyz, nxyz;                      /* voxel counters */
   int nvoxel;                          /* output voxel # */
   int numdf;                           /* numerator degrees of freedom */
   int dendf;                           /* denominator degrees of freedom */
   float mse;                           /* mean square error */
   float msab;                          /* mean square interaction */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
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
      msa[ixyz] = msa[ixyz] / numdf;      /*---   MSA = SSA / (a-1)  ---*/
   if (nvoxel > 0)
      printf ("MSA = %f \n", msa[nvoxel-1]);

   /*----- calculate F-statistic    -----*/
   if (option_data->model == 1)
   {
      /*----- fixed effects model -----*/
      volume_read ("sse", fa, nxyz);
      dendf = a * b * (n-1);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      {
          mse = fa[ixyz] / dendf;        /*---  MSE = SSE / (ab(n-1))  ---*/
	  if (mse < EPSILON)
	    fa[ixyz] = 0.0;
	  else
	    fa[ixyz] = msa[ixyz] / mse;    /*---  F = MSA / MSE  ---*/
      }
   }
   else
   {
      /*----- random or mixed effects model -----*/
      volume_read ("ssab", fa, nxyz);
      dendf = (a-1) * (b-1);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      {
          msab = fa[ixyz] / dendf;       /*---  MSAB = SSAB / (a-1)(b-1) ---*/
	  if (msab < EPSILON)
	    fa[ixyz] = 0.0;
	  else
	    fa[ixyz] = msa[ixyz] / msab;   /*---  F = MSA / MSAB  ---*/
      }
   }

   if (nvoxel > 0)
      printf ("FA = %f \n", fa[nvoxel-1]);

   /*----- write out afni data file -----*/
   for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      msa[ixyz] = sqrt(msa[ixyz]);        /*-- msa now holds square root --*/
   write_afni_data (option_data, option_data->faname,
		    msa, fa, numdf, dendf);

   /*----- this data file is no longer needed -----*/
   volume_delete ("ssa");

   /*----- release memory -----*/
   free (msa);   msa = NULL;
   free (fa);    fa = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to calculate the F-statistic for factor B.

   For fixed effects or mixed effects model:
      F = MSB / MSE,
         where MSB  = SSB / (b-1),
         and   MSE  = SSE / (ab(n-1)).

   For random effects model:
      F = MSB / MSAB,
         where MSB  = SSB  / (b-1),
         and   MSAB = SSAB / ((a-1)(b-1)).

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
   int n;                               /* number of observations per cell */
   int ixyz, nxyz;                      /* voxel counters */
   int nvoxel;                          /* output voxel # */
   int numdf;                           /* numerator degrees of freedom */
   int dendf;                           /* denominator degrees of freedom */
   float mse;                           /* mean square error */
   float msab;                          /* mean square interaction */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
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
      msb[ixyz] = msb[ixyz] / numdf;     /*---   MSB = SSB / (b-1)  ---*/
   if (nvoxel > 0)
      printf ("MSB = %f \n", msb[nvoxel-1]);

   /*----- calculate F-statistic    -----*/
   if ((option_data->model == 1) || (option_data->model == 3))
   {
      /*----- fixed effects model or mixed effects model -----*/
      volume_read ("sse", fb, nxyz);
      dendf = a * b * (n-1);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      {
          mse = fb[ixyz] / dendf;        /*---  MSE = SSE / (ab(n-1))  ---*/
	  if (mse < EPSILON)
	    fb[ixyz] = 0.0;
	  else
	    fb[ixyz] = msb[ixyz] / mse;    /*---  F = MSB / MSE  ---*/
      }
   }
   else
   {
      /*----- random effects model -----*/
      volume_read ("ssab", fb, nxyz);
      dendf = (a-1) * (b-1);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      {
          msab = fb[ixyz] / dendf;       /*---  MSAB = SSAB / (a-1)(b-1) ---*/
	  if (msab < EPSILON)
	    fb[ixyz] = 0.0;
	  else
	    fb[ixyz] = msb[ixyz] / msab;   /*---  F = MSB / MSAB  ---*/
      }
   }

   if (nvoxel > 0)
      printf ("FB = %f \n", fb[nvoxel-1]);

   /*----- write out afni data file -----*/
   for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      msb[ixyz] = sqrt(msb[ixyz]);        /*-- msb now holds square root --*/
   write_afni_data (option_data, option_data->fbname,
		    msb, fb, numdf, dendf);

   /*----- this data file is no longer needed -----*/
   volume_delete ("ssb");

   /*----- release memory -----*/
   free (msb);   msb = NULL;
   free (fb);    fb  = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to calculate the F-statistic for interaction, F = MSAB / MSE,
      where MSAB = SSAB / ((a-1)(b-1)),
      and   MSE  = SSE  / (ab(n-1)).

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
   int n;                               /* number of observations per cell */
   int ixyz, nxyz;                      /* voxel counters */
   int nvoxel;                          /* output voxel # */
   float fval;                          /* float value used in calculations */
   float mse;                           /* mean square error */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
   n = option_data->n;
   nxyz = option_data->nxyz;
   nvoxel = option_data->nvoxel;

   /*----- allocate memory space for calculations -----*/
   fab = (float *) malloc(sizeof(float)*nxyz);
   msab = (float *) malloc(sizeof(float)*nxyz);
   if ((fab == NULL) || (msab == NULL))
      ANOVA_error ("unable to allocate sufficient memory");

   /*----- calculate mean SS due to interaction -----*/
   volume_read ("ssab", msab, nxyz);
   fval = 1.0 / ((a - 1.0)*(b - 1.0));
   for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      msab[ixyz] = msab[ixyz] * fval;    /*---  MSAB = SSAB/((a-1)(b-1))  ---*/
   if (nvoxel > 0)
      printf ("MSAB = %f \n", msab[nvoxel-1]);

   /*----- calculate F-statistic    -----*/
   volume_read ("sse", fab, nxyz);
   fval = 1.0 / (a * b * (n-1));
   for (ixyz = 0;  ixyz < nxyz;  ixyz++)
     {
       mse = fab[ixyz] * fval;          /*---  MSE = SSE / (ab(n-1))  ---*/
       if (mse < EPSILON)
	 fab[ixyz] = 0.0;
       else
	 fab[ixyz] = msab[ixyz] / mse;    /*---  F = MSAB / MSE  ---*/
     }
   if (nvoxel > 0)
      printf ("FAB = %f \n", fab[nvoxel-1]);

   /*----- write out afni data file -----*/
   for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      msab[ixyz] = sqrt(msab[ixyz]);      /*-- msab now holds square root --*/
   write_afni_data (option_data, option_data->fabname,
		    msab, fab, (a-1)*(b-1), a*b*(n-1));

   /*----- release memory -----*/
   free (msab);   msab = NULL;
   free (fab);    fab  = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  The following are the old calculate_* functions, before the changes
  by rickr/gangc.  The function names have been prepended by old_,
  allowing the addition of an option to call those functions, instead
  of the new varieties.  This seems to be the cleanest way to allow
  users to compare results between the old vs. new functions.  Putting
  multiple tests in each function would be quite messy.

  Note, the new functionality is for type 3 ameans, adiff and acontr.

                                                  23 Nov 2005 [rickr]
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
   int imean;                         /* output mean option index */
   int level;                         /* factor A level index */
   int n;                             /* number of observations per cell */
   int ixyz, nxyz;                    /* voxel counters */
   int nvoxel;                        /* output voxel # */
   int a;                             /* number of levels for factor A */
   int b;                             /* number of levels for factor B */
   int nt;                            /* total number of observations */
   int num_means;                     /* number of user requested means */
   int df;                            /* degrees of freedom for t-test */
   float fval;                        /* for calculating std. dev. */
   float stddev;                      /* est. std. dev. of factor mean */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
   n = option_data->n;
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

      /*----- estimate factor mean for this treatment level -----*/
      calculate_sum (option_data, level, -1, mean);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	 mean[ixyz] = mean[ixyz] / (n*b);
      if (nvoxel > 0)
         printf ("Mean factor A level %d = %f \n", level+1, mean[nvoxel-1]);

      /*----- divide by estimated standard deviation of factor mean -----*/
      if (option_data->model == 1)
      {
   	 /*----- fixed effects model -----*/
         volume_read ("sse", tmean, nxyz);
	 df = a*b*(n-1);
      }
      else
      {
	 /*----- mixed effects model -----*/
         volume_read ("ssab", tmean, nxyz);
	 df = (a-1)*(b-1);
      }
      fval = (1.0 / df) * (1.0 / (b*n));
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
   int ixyz, nxyz;                     /* voxel counters */
   int nvoxel;                         /* output voxel # */
   int num_diffs;                      /* number of user requested diffs. */
   int idiff;                          /* index for requested differences */
   int i, j;                           /* factor level indices */
   int n;                              /* number of observations per cell */
   int df;                             /* degrees of freedom for t-test */
   float fval;                         /* for calculating std. dev. */
   float stddev;                       /* est. std. dev. of difference */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
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
      calculate_sum (option_data, i, -1, diff);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
         diff[ixyz] = diff[ixyz] / (b*n);

      /*----- subtract second treatment level mean -----*/
      j = option_data->adiffs[idiff][1];
      calculate_sum (option_data, j, -1, tdiff);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
         diff[ixyz] -= tdiff[ixyz] / (b*n);
      if (nvoxel > 0)
         printf ("Difference of factor A level %d - level %d = %f \n",
		 i+1, j+1, diff[nvoxel-1]);

      /*----- divide by estimated standard deviation of difference -----*/
      if (option_data->model == 1)
      {
   	 /*----- fixed effects model -----*/
         volume_read ("sse", tdiff, nxyz);
	 df = a*b*(n-1);
      }
      else
      {
	 /*----- mixed effects model -----*/
         volume_read ("ssab", tdiff, nxyz);
	 df = (a-1)*(b-1);
      }
      fval = (1.0 / df) * (2.0 / (b*n));
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
   int ixyz, nxyz;                     /* voxel counters */
   int nvoxel;                         /* output voxel # */
   int num_contr;                      /* number of user requested contrasts */
   int icontr;                         /* index of user requested contrast */
   int level;                          /* factor level index */
   int n;                              /* number of observations per cell */
   int df;                             /* degrees of freedom for t-test */
   float fval;                         /* for calculating std. dev. */
   float c;                            /* contrast coefficient */
   float stddev;                       /* est. std. dev. of contrast */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
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
	 c = option_data->acontr[icontr][level];
	 if (c == 0.0) continue;

	 /*----- add c * treatment level mean to contrast -----*/
	 calculate_sum (option_data, level, -1, tcontr);
         fval += c * c / (b*n);
	 for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	    contr[ixyz] += c * tcontr[ixyz] / (b*n);
      }
      if (nvoxel > 0)
	 printf ("No.%d contrast for factor A = %f \n",
		 icontr+1, contr[nvoxel-1]);

      /*----- standard deviation depends on model type -----*/
      if (option_data->model == 1)
      {
   	 /*----- fixed effects model -----*/
         volume_read ("sse", tcontr, nxyz);
	 df = a*b*(n-1);
      }
      else
      {
	 /*----- mixed effects model -----*/
         volume_read ("ssab", tcontr, nxyz);
	 df = (a-1)*(b-1);
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
/*                  --- end old_calclulte_* functions ---                    */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/*
   Routine to calculate the mean treatment effect for factor A at the user
   specified treatment level.  The output is stored as a 2 sub-brick AFNI
   data set. The first sub-brick contains the estimated mean at this treatment
   level, and the second sub-brick contains the corresponding t-statistic.

                                   updated by gangc and rickr, 11 Jul 2005 :

   Try to be efficient with the following formula, coming from the mean (sum),
   the sum of squares, and df = b - 1 :

       t = mean * sqrt[     df ( df + 1 )      ]
                      [ ---------------------- ]
                      [ sum_sq - (df+1)*mean^2 ]
*/
void calculate_ameans (anova_options * option_data)
{
   const float  EPSILON = 1.0e-10;    /* protect against divide by zero */
   float * mean = NULL;               /* treatment mean volume */
   float * tmean = NULL;              /* t-statistic */
   float * contr;                     /* compute mean as basic contrast */
   float   fval, stddev;              /* type-1 stddev computation */
   int imean;                         /* output mean option index */
   int level;                         /* factor A level index */
   int n;                             /* number of observations per cell */
   int ixyz, nxyz, index;             /* voxel counters */
   int nvoxel;                        /* output voxel # */
   int a, b;                          /* numbers of factor levels */
   int num_means;                     /* number of user requested means */
   int df;                            /* degrees of freedom for t-test */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
   n = option_data->n;
   num_means = option_data->num_ameans;
   nxyz = option_data->nxyz;
   nvoxel = option_data->nvoxel;

   /*----- allocate memory space for calculations -----*/
   mean = (float *)malloc(sizeof(float)*nxyz);
   tmean = (float *)malloc(sizeof(float)*nxyz);
   contr = (float *)malloc(sizeof(float)*a); /* length is number of A levels */
   if ((mean == NULL) || (tmean == NULL))
      ANOVA_error ("unable to allocate sufficient memory");

   /*----- loop over user specified treatment means -----*/
   for (imean = 0;  imean < num_means;  imean++)
   {
      level = option_data->ameans[imean];

      if (option_data->model == 1)      /*----- fixed effects model -----*/
      {
          /*----- estimate factor mean for this treatment level -----*/
          calculate_sum (option_data, level, -1, mean);
          for (ixyz = 0; ixyz < nxyz; ixyz++) mean[ixyz] /= (n*b);

          volume_read ("sse", tmean, nxyz);
          df = a*b*(n-1);

          /*----- divide by estimated standard deviation of factor mean -----*/
          fval = (1.0 / df) * (1.0 / (b*n));
          for (ixyz = 0;  ixyz < nxyz;  ixyz++)
          {
             stddev =  sqrt(tmean[ixyz] * fval);
             if (stddev < EPSILON) tmean[ixyz] = 0.0;
             else                  tmean[ixyz] = mean[ixyz] / stddev;
          }

      } else {                            /*----- mixed effects model -----*/
                                          /*-  01 Sep 2005 [rickr, gangc] -*/

          /*----- set level as a trivial contrast -----*/
          for (index = 0; index < a; index++ ) contr[index] = 0.0;
          contr[level] = 1.0;

          df = b - 1;  /*-- note degrees of freedom, and compute contrast --*/
          calc_type3_acontr(option_data, contr, df, mean, tmean);
      }

      if (nvoxel > 0)
         printf ("factor A level %d: mean = %f, t = %f, df = %d\n",
                 level+1, mean[nvoxel-1], tmean[nvoxel-1], df);

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->amname[imean],
                       mean, tmean, df, 0);
   }

   /*----- release memory -----*/
   free(tmean);  free(mean);  free(contr);
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
   int imean;                         /* output mean option index */
   int level;                         /* factor B level index */
   int n;                             /* number of observations per cell */
   int ixyz, nxyz;                    /* voxel counters */
   int nvoxel;                        /* output voxel # */
   int a;                             /* number of levels for factor A */
   int b;                             /* number of levels for factor B */
   int nt;                            /* total number of observations */
   int num_means;                     /* number of user requested means */
   int df;                            /* degrees of freedom for t-test */
   float fval;                        /* for calculating std. dev. */
   float stddev;                      /* est. std. dev. of factor mean */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
   n = option_data->n;
   df = a * b * (n-1);
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
      calculate_sum (option_data, -1, level, mean);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
         mean[ixyz] = mean[ixyz] / (n*a);
      if (nvoxel > 0)
         printf ("Mean of factor B level %d = %f \n", level+1, mean[nvoxel-1]);

      /*----- divide by estimated standard deviation of factor mean -----*/
      volume_read ("sse", tmean, nxyz);
      fval = (1.0 / df) * (1.0 / (a*n));
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
  Routine to calculate the mean for an individual cell (treatment).
  The output is stored as a 2 sub-brick AFNI  data set. The first sub-brick
  contains the estimated mean for this cell, and the second sub-brick contains
  the corresponding t-statistic.
*/

void calculate_xmeans (anova_options * option_data)
{
   const float  EPSILON = 1.0e-10;    /* protect against divide by zero */
   float * mean = NULL;               /* pointer to treatment mean data */
   float * tmean = NULL;              /* pointer to t-statistic data */
   int imean;                         /* output mean option index */
   int alevel;                        /* factor A level index */
   int blevel;                        /* factor B level index */
   int n;                             /* number of observations per cell */
   int ixyz, nxyz;                    /* voxel counters */
   int nvoxel;                        /* output voxel # */
   int a;                             /* number of levels for factor A */
   int b;                             /* number of levels for factor B */
   int nt;                            /* total number of observations */
   int num_means;                     /* number of user requested means */
   int df;                            /* degrees of freedom for t-test */
   float fval;                        /* for calculating std. dev. */
   float stddev;                      /* est. std. dev. of cell mean */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
   n = option_data->n;
   df = a * b * (n-1);
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

      /*----- estimate mean for this cell -----*/
      calculate_sum (option_data, alevel, blevel, mean);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	 mean[ixyz] = mean[ixyz] / n;
      if (nvoxel > 0)
         printf ("Mean of Cell[%d][%d] = %f \n",
		 alevel+1, blevel+1, mean[nvoxel-1]);

      /*----- divide by estimated standard deviation of cell mean -----*/
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
         printf ("t-stat for Mean of Cell[%d][%d] = %f \n",
		 alevel+1, blevel+1, tmean[nvoxel-1]);

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

   Modified to use calc_type3_acontr functions, for ease of calculation of
   sum of squared differences.                      31 Aug 2005 [gangc,rickr]
*/

void calculate_adifferences (anova_options * option_data)
{
   const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
   float * diff = NULL;                /* pointer to est. diff. in means */
   float * tdiff = NULL;               /* pointer to t-statistic data */
   float * contrast;                   /* for using diff as contrast */
   float   fval, stddev;               /* for stddev computation */
   int a;                              /* number of levels for factor A */
   int b;                              /* number of levels for factor B */
   int ixyz, nxyz;                     /* number of voxels */
   int nvoxel;                         /* output voxel # */
   int num_diffs;                      /* number of user requested diffs. */
   int idiff;                          /* index for requested differences */
   int i, j, c;                        /* factor level indices */
   int n;                              /* number of observations per cell */
   int df;                             /* degrees of freedom for t-test */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
   n = option_data->n;
   num_diffs = option_data->num_adiffs;
   nxyz = option_data->nxyz;
   nvoxel = option_data->nvoxel;

   /*----- allocate memory space for calculations -----*/
   diff = (float *) malloc(sizeof(float)*nxyz);
   tdiff = (float *) malloc(sizeof(float)*nxyz);
   contrast = (float *) malloc(sizeof(float)*a);
   if ((diff == NULL) || (tdiff == NULL) || (contrast == NULL))
      ANOVA_error ("calc_adiffs: unable to allocate sufficient memory");

   /*----- loop over user specified treatment differences -----*/
   for (idiff = 0;  idiff < num_diffs;  idiff++)
   {
      i = option_data->adiffs[idiff][0];
      j = option_data->adiffs[idiff][1];

      if (option_data->model == 1)     /*----- fixed effects model -----*/
      {
          /*----- read first treatment level mean -----*/
          calculate_sum (option_data, i, -1, diff);
          for (ixyz = 0; ixyz < nxyz; ixyz++) diff[ixyz] = diff[ixyz] / (b*n);

          /*----- subtract second treatment level mean -----*/
          calculate_sum (option_data, j, -1, tdiff);
          for (ixyz = 0; ixyz < nxyz; ixyz++) diff[ixyz] -= tdiff[ixyz] / (b*n);

          /*----- divide by estimated standard deviation of difference -----*/
          volume_read ("sse", tdiff, nxyz);
          df = a*b*(n-1);

          fval = (1.0 / df) * (2.0 / (b*n));
          for (ixyz = 0;  ixyz < nxyz;  ixyz++)
            {
              stddev = sqrt (tdiff[ixyz] * fval);
              if (stddev < EPSILON) tdiff[ixyz] = 0.0;
              else                  tdiff[ixyz] = diff[ixyz] / stddev;
            }
      } else {                         /*----- mixed effects model -----*/
                                       /* updated 31 Aug 2005 [rickr]   */

          /* set the diff as a contrast */
          for (c = 0 ; c < a; c++ ) contrast[c] = 0.0;
          contrast[i] = 1.0;  contrast[j] = -1.0;

          /*----- compute sums as contrast, compute mean, tstat -----*/
          df = b - 1;
          calc_type3_acontr(option_data, contrast, df, diff, tdiff);
      }

      if (nvoxel > 0)
      {
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
   free(tdiff);  free(diff);  free(contrast);
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
   int a;                              /* number of levels for factor A */
   int b;                              /* number of levels for factor B */
   int ixyz, nxyz;                     /* voxel counters */
   int nvoxel;                         /* output voxel # */
   int num_diffs;                      /* number of user requested diffs. */
   int idiff;                          /* index for requested differences */
   int i, j;                           /* factor level indices */
   int n;                              /* number of observations per cell */
   int df;                             /* degrees of freedom for t-test */
   float fval;                         /* for calculating std. dev. */
   float stddev;                       /* est. std. dev. of difference */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
   n = option_data->n;
   df = a*b*(n-1);
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
      calculate_sum (option_data, -1, i, diff);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
         diff[ixyz] = diff[ixyz] / (a*n);

      /*----- subtract second treatment level mean -----*/
      j = option_data->bdiffs[idiff][1];
      calculate_sum (option_data, -1, j, tdiff);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
         diff[ixyz] -= tdiff[ixyz] / (a*n);
      if (nvoxel > 0)
         printf ("Difference of factor B level %d - level %d = %f \n",
                 i+1, j+1, diff[nvoxel-1]);

      /*----- divide by estimated standard deviation of difference -----*/
      volume_read ("sse", tdiff, nxyz);
      fval = (1.0 / df) * (2.0 / (a*n));
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
   int ixyz, nxyz;                     /* voxel counters */
   int nvoxel;                         /* output voxel # */
   int num_diffs;                      /* number of user requested diffs. */
   int idiff;                          /* index for requested differences */
   int ia, ib, ja, jb;                 /* cell indices */
   int n;                              /* number of observations per cell */
   int df;                             /* degrees of freedom for t-test */
   float fval;                         /* for calculating std. dev. */
   float stddev;                       /* est. std. dev. of difference */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
   n = option_data->n;
   df = a*b*(n-1);
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

      /*----- calculate first cell mean -----*/
      ia = option_data->xdiffs[idiff][0][0];
      ib = option_data->xdiffs[idiff][0][1];
      calculate_sum (option_data, ia, ib, diff);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
         diff[ixyz] = diff[ixyz] / n;

      /*----- subtract second cell mean -----*/
      ja = option_data->xdiffs[idiff][1][0];
      jb = option_data->xdiffs[idiff][1][1];
      calculate_sum (option_data, ja, jb, tdiff);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
         diff[ixyz] -= tdiff[ixyz] / n;
      if (nvoxel > 0)
         printf ("Difference of Cell[%d][%d] - Cell[%d][%d] = %f \n",
		 ia+1, ib+1, ja+1, jb+1, diff[nvoxel-1]);

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
	printf ("t-stat for Cell[%d][%d] - Cell[%d][%d] = %f \n",
		ia+1, ib+1, ja+1, jb+1, tdiff[nvoxel-1]);

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
   float * contr;                      /* pointer to contrast estimate */
   float * tcontr;                     /* pointer to t-statistic data */
   float   fval, stddev;               /* stddev computation */
   float   c;                          /* contrast value */
   int a, b, level;                    /* levels variables */
   int ixyz, nxyz;                     /* number of voxels */
   int nvoxel;                         /* output voxel # */
   int num_contr;                      /* number of user requested contrasts */
   int icontr;                         /* index of user requested contrast */
   int n;                              /* number of observations per cell */
   int df;                             /* degrees of freedom for t-test */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
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
            c = option_data->acontr[icontr][level];
            if (c == 0.0) continue;

            /*----- add c * treatment level mean to contrast -----*/
            calculate_sum (option_data, level, -1, tcontr);
            fval += c * c / (b*n);
            for (ixyz = 0;  ixyz < nxyz;  ixyz++)
               contr[ixyz] += c * tcontr[ixyz] / (b*n);
         }

         volume_read ("sse", tcontr, nxyz);
         df = a*b*(n-1);

         /*----- divide by estimated standard deviation of the contrast -----*/
         for (ixyz = 0;  ixyz < nxyz;  ixyz++)
         {
            stddev = sqrt ((tcontr[ixyz] / df) * fval);
            if (stddev < EPSILON)
              tcontr[ixyz] = 0.0;
            else
              tcontr[ixyz] = contr[ixyz] / stddev;
         }
      }
      else      /* type-3: now, new and improved!   1 Sep 2005 [rickr,gangc] */
      {
         /*----- compute the acontr_mean and tstat -----*/
         df = b - 1;
         calc_type3_acontr(option_data, option_data->acontr[icontr], df,
                              contr, tcontr);
      }

      if (nvoxel > 0)
	 printf ("No.%d contrast for factor A = %f, t = %f, df = %d\n",
		 icontr+1, contr[nvoxel-1], tcontr[nvoxel-1], df);

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
   int a;                              /* number of levels for factor A */
   int b;                              /* number of levels for factor B */
   int ixyz, nxyz;                     /* voxel counters */
   int nvoxel;                         /* output voxel # */
   int num_contr;                      /* number of user requested contrasts */
   int icontr;                         /* index of user requested contrast */
   int level;                          /* factor level index */
   int df;                             /* degrees of freedom for t-test */
   int n;                              /* number of observations per cell */
   float fval;                         /* for calculating std. dev. */
   float c;                            /* contrast coefficient */
   float stddev;                       /* est. std. dev. of contrast */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
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
         c = option_data->bcontr[icontr][level];
         if (c == 0.0) continue;

         /*----- add c * treatment level mean to contrast -----*/
         calculate_sum (option_data, -1, level, tcontr);
         fval += c * c / (a*n);
         for (ixyz = 0;  ixyz < nxyz;  ixyz++)
            contr[ixyz] += c * tcontr[ixyz] / (a*n);
      }
      if (nvoxel > 0)
         printf ("No.%d contrast for factor B = %f \n",
                 icontr+1, contr[nvoxel-1]);

      /*----- divide by estimated standard deviation of the contrast -----*/
      volume_read ("sse", tcontr, nxyz);
      df = a * b * (n-1);
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
                       contr, tcontr, a*b*(n-1), 0);

   }

   /*----- release memory -----*/
   free (tcontr);   tcontr = NULL;
   free (contr);    contr = NULL;
}


/*---------------------------------------------------------------------------*/
/*
   Routine to estimate a user specified contrast in individual cell means.
   The output is stored as a 2 sub-brick AFNI data set.  The first
   sub-brick contains the estimated contrast.  The second sub-brick contains
   the corresponding t-statistic.
*/

void calculate_xcontrasts (anova_options * option_data)
{
   const float  EPSILON = 1.0e-10;     /* protect against divide by zero */
   float * contr = NULL;               /* pointer to contrast estimate */
   float * tcontr = NULL;              /* pointer to t-statistic data */
   int a;                              /* number of levels for factor A */
   int b;                              /* number of levels for factor B */
   int ixyz, nxyz;                     /* voxel counters */
   int nvoxel;                         /* output voxel # */
   int num_contr;                      /* number of user requested contrasts */
   int icontr;                         /* index of user requested contrast */
   int ilevel, jlevel;                 /* factor level indices */
   int df;                             /* degrees of freedom for t-test */
   int n;                              /* number of observations per cell */
   float fval;                         /* for calculating std. dev. */
   float c;                            /* contrast coefficient */
   float stddev;                       /* est. std. dev. of contrast */


   /*----- initialize local variables -----*/
   a = option_data->a;
   b = option_data->b;
   n = option_data->n;
   num_contr = option_data->num_xcontr;
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

      for (ilevel = 0;  ilevel < a;  ilevel++)
	for (jlevel = 0;  jlevel < b;  jlevel++)
	  {
	    c = option_data->xcontr[icontr][ilevel][jlevel];
	    if (c == 0.0) continue;

	    /*----- add c * cell mean to contrast -----*/
	    calculate_sum (option_data, ilevel, jlevel, tcontr);
	    fval += c * c / n;
	    for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	      contr[ixyz] += c * tcontr[ixyz] / n;
	  }
      if (nvoxel > 0)
	printf ("No.%d contrast for cell means = %f \n",
		icontr+1, contr[nvoxel-1]);

      /*----- divide by estimated standard deviation of the contrast -----*/
      volume_read ("sse", tcontr, nxyz);
      df = a * b * (n-1);
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
      {
	  stddev = sqrt ((tcontr[ixyz] / df) * fval);
	  if (stddev < EPSILON)
	    tcontr[ixyz] = 0.0;
	  else
	    tcontr[ixyz] = contr[ixyz] / stddev;
      }

      if (nvoxel > 0)
	printf ("t-stat for No.%d contrast for cell means = %f \n",
		icontr+1, tcontr[nvoxel-1]);

      /*----- write out afni data file -----*/
      write_afni_data (option_data, option_data->xcname[icontr],
                       contr, tcontr, a*b*(n-1), 0);

   }

   /*----- release memory -----*/
   free (tcontr);   tcontr = NULL;
   free (contr);    contr = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to calculate sums and sums of squares for two-factor ANOVA.
*/

void calculate_anova (anova_options * option_data)
{

  /*----- calculate various sum and sums of squares -----*/
  calculate_ss0 (option_data);
  calculate_ssi (option_data);
  calculate_ssj (option_data);
  calculate_ssij (option_data);
  if (option_data->n != 1)  calculate_ssijk (option_data);


  /*-----  calculate error sum of squares  -----*/
  if (option_data->n != 1)
    {
      calculate_sse (option_data);
      volume_delete ("ssijk");
    }

  /*-----  calculate treatment sum of squares -----*/
  calculate_sstr (option_data);
  volume_delete ("ssij");

  /*-----  calculate sum of squares due to A effect  -----*/
  calculate_ssa (option_data);
  volume_delete ("ssi");

  /*-----  calculate sum of squares due to B effect  -----*/
  calculate_ssb (option_data);
  volume_delete ("ssj");

  volume_delete ("ss0");

  /*-----  calculate sum of squares due to A*B interaction  -----*/
  calculate_ssab (option_data);

}


/*---------------------------------------------------------------------------*/
/*
   Routine to analyze the results from a two-factor ANOVA.
*/

void analyze_results (anova_options * option_data)
{

   /*-----  calculate F-statistic for treatment effect  -----*/
   if (option_data->nftr)  calculate_ftr (option_data);

   /*-----  calculate F-statistic for factor A effect  -----*/
   if (option_data->nfa)  calculate_fa (option_data);

   /*-----  calculate F-statistic for factor B effect  -----*/
   if (option_data->nfb)  calculate_fb (option_data);

   /*-----  calculate F-statistic for interaction effect  -----*/
   if (option_data->nfab)  calculate_fab (option_data);

   /*-----  estimate level means for factor A  -----*/
   if (option_data->num_ameans){
      if (option_data->old_method) old_calculate_ameans (option_data);
      else                         calculate_ameans (option_data);
   }

   /*-----  estimate level means for factor B  -----*/
   if (option_data->num_bmeans)  calculate_bmeans (option_data);

   /*-----  estimate cell means  -----*/
   if (option_data->num_xmeans)  calculate_xmeans (option_data);

   /*-----  estimate level differences for factor A  -----*/
   if (option_data->num_adiffs){
      if (option_data->old_method) old_calculate_adifferences (option_data);
      else                         calculate_adifferences (option_data);
   }

   /*-----  estimate level differences for factor B  -----*/
   if (option_data->num_bdiffs)  calculate_bdifferences (option_data);

   /*-----  estimate differences in cell means  -----*/
   if (option_data->num_xdiffs)  calculate_xdifferences (option_data);

   /*-----  estimate level contrasts for factor A  -----*/
   if (option_data->num_acontr){
      if (option_data->old_method) old_calculate_acontrasts (option_data);
      else                         calculate_acontrasts (option_data);
   }

   /*-----  estimate level contrasts for factor B  -----*/
   if (option_data->num_bcontr)  calculate_bcontrasts (option_data);

   /*-----  estimate contrasts in cell means  -----*/
   if (option_data->num_xcontr)  calculate_xcontrasts (option_data);

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


 /*----- make F-stat for treatment sub-bricks -----*/
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


  /*----- make F-stat for factor A sub-bricks -----*/
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


  /*----- make contrast in cell means sub-bricks -----*/
  if (option_data->num_xcontr > 0)
    for (i = 0; i < option_data->num_xcontr; i++)
      {
	add_file_name (new_dset, option_data->xcname[i], bucket_str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:Contr ",
		 ibrick, option_data->xcname[i]);
	strcat (refit_str, str);

	ibrick++;
	sprintf (str, " -sublabel %d %s:t-stat ",
		 ibrick, option_data->xcname[i]);
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
  int i, j;
  THD_3dim_dataset * dset=NULL;       /* input afni data set pointer */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  char filename[MAX_NAME_LENGTH];


   /*----- remove temporary data files -----*/
   volume_delete ("sstr");
   volume_delete ("sse");
   volume_delete ("ssa");
   volume_delete ("ssb");
   volume_delete ("ssab");
   for (i = 0;  i < option_data->a;  i++)
   {
      sprintf (filename, "ya.%d", i);
      volume_delete (filename);
   }
   for (j = 0;  j < option_data->b;  j++)
   {
      sprintf (filename, "yb.%d", j);
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
      CHECK_OPEN_ERROR(dset,option_data->first_dataset) ;

      /*----- make an empty copy of this dataset -----*/
      new_dset = EDIT_empty_copy (dset);
      THD_delete_3dim_dataset (dset , False);   dset = NULL;
      EDIT_dset_items (new_dset,
		       ADN_directory_name, option_data->session,
		       ADN_none);

      /*----- remove F-stat for treatment data file -----*/
      if (option_data->nftr != 0)
	remove_dataset (new_dset, option_data->ftrname);

      /*----- remove F-stat for factor A main effect data file -----*/
      if (option_data->nfa != 0)
	remove_dataset (new_dset, option_data->faname);

      /*----- remove F-stat for factor B main effect data file -----*/
      if (option_data->nfb != 0)
	remove_dataset (new_dset, option_data->fbname);

      /*----- remove F-stat for A*B interaction data file -----*/
      if (option_data->nfab != 0)
	remove_dataset (new_dset, option_data->fabname);

      /*----- remove factor A level mean data files -----*/
      if (option_data->num_ameans > 0)
	for (i = 0; i < option_data->num_ameans; i++)
	  remove_dataset (new_dset, option_data->amname[i]);

      /*----- remove factor B level mean data files -----*/
      if (option_data->num_bmeans > 0)
	for (i = 0; i < option_data->num_bmeans; i++)
	  remove_dataset (new_dset, option_data->bmname[i]);

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

      /*----- remove contrast in cell means data files -----*/
      if (option_data->num_xcontr > 0)
	for (i = 0; i < option_data->num_xcontr; i++)
	  remove_dataset (new_dset, option_data->xcname[i]);

      THD_delete_3dim_dataset (new_dset , False);   new_dset = NULL;
    }


  /*----- deallocate memory -----*/
  destroy_anova_options (option_data);

}


/*---------------------------------------------------------------------------*/
/*
  Two- factor analysis of variance (ANOVA).
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

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dANOVA main"); machdep(); PRINT_VERSION("3dANOVA"); AUTHOR(PROGRAM_AUTHOR);

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   /*----- program initialization -----*/
   initialize (argc, argv, &option_data);

   /*----- warn user (after any help) -----*/
   if( option_data->model == 3 && !option_data->old_method )
       fprintf(stderr,"\n"
           "** Changes have been made for 3dANOVA2 computations of type 3.\n"
           "   For details, please see:\n"
           "   %s\n\n", ANOVA_MODS_LINK);

   /*----- calculate sums of squares -----*/
   calculate_anova (option_data);

   /*----- generate requested output -----*/
   analyze_results (option_data);

   /*----- terminate program -----*/
   terminate (option_data);
   free (option_data);   option_data = NULL;

   exit(0);
}
