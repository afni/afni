/* 
   This is a 1-for-1 replacement for an original shell script that had
   the same name.
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include "mrilib.h"
#include "statpval_opts.h"


/* help text */
int usage_p2dsetstat() 
{
   char *author  = "PA Taylor and RC Reynolds (SSCC, NIMH, NIH)";
   char *version = "2.0";
   char *rev_dat = "Nov 04, 2023";
   PARAMS_statpval opts;

   opts = set_p2dsetstat_defaults();  // to use in help text, as nec

   printf(
"\n"
"Overview ~1~ \n"
"\n"
"This program converts a p-value to a statistic of choice, with\n"
"reference to a particular dataset.\n"
"\n"
"Often to convert a p-value to a statistic, supplementary\n"
"information is needed, such as number of degrees of freedom.  AFNI\n"
"programs that write statistics *do* store that info in headers, and\n"
"this program is meant to be a useful to do conversions based on\n"
"that info.  Here, the user provides the p-value and the specific [i]th\n"
"brick of the dataset in question, and a statistic (either as single number,\n"
"or with supplementary info) is output to screen.\n"
"\n"
"This program should give equivalent results to other AFNI programs\n"
"like ccalc and cdf, but with less work by the user.\n"
"\n"
"See also the complementary program for doing the inverse, converting\n"
"a statistic to an equivalent p-value:  dsetstat2p.\n"
"\n"
"**Note that the user will have to choose explicitly whether they\n"
"  are doing one-sided or bi-sided/two-sided testing!** This is\n"
"  equivalent to choosing \"Pos&Neg\" or just \"Pos\" (or just \"Neg\",\n"
"  if the user multiplies the output by a negative) in the AFNI\n"
"  GUI's clickable p-to-statistic calculator.\n"
"\n"
"written by : %s\n"
"version    : %s\n"
"rev date   : %s\n"
"\n"
"--------------------------------------------------------------------------\n"
"\n"
"Options ~1~ \n"
"\n"
"  p2dsetstat                                  \\\n"
"        -inset   DDD\"[i]\"                     \\\n"
"        -pval    P                            \\\n"
"        -bisided|-2sided|-1sided              \\\n"
"        {-quiet}\n"
"\n"
"  where:\n"
"    -inset  DDD\"[i]\"\n"
"               :specify a dataset DDD and, if it has multiple sub-bricks,\n"
"                the [i]th subbrick with the statistic of interest MUST\n"
"                be selected explicitly; note the use of quotation marks \n"
"                around the brick selector (because of the square-brackets).\n"
"                Note that 'i' can be either a number of a string label\n"
"                selector.\n"
"                NB: we refer to \"sub-bricks\" here, but the inset\\\n"
"                could also be a surface dataset, too.\\\n"
"\n"
"    -pval  P   :input p-value P, which MUST be in the interval (0,1).\n"
"\n"
"    -bisided\n"
"       or\n"
"    -2sided\n"
"       or\n"
"    -1sided    :one of these sidedness options MUST be chosen, and it is\n"
"                up to the researcher to choose which is appropriate.\n"
"\n"
"    -quiet     :an optional flag so that output ONLY the final statistic\n"
"                value is output to standard output; this can be then be\n"
"                viewed, redirected to a text file or saved as a shell\n"
"                variable.  (Default: display supplementary text.)\n"
"\n"
"--------------------------------------------------------------------------\n"
"\n"
"Outputs ~1~ \n"
"\n"
"The types of statistic values that can be calculated are:\n"
"    corr coef, t-stat, F-stat or z-score.\n"
"\n"
"If \"-quiet\" is used, then basically just a single number (the converted\n"
"statistic value) is output.  See examples for saving this in a file or\n"
"variable.\n"
"\n"
"Without the \"-quiet\" option, some descriptive text is also output with\n"
"the calculation, stating what kind of statistic is being output, etc.\n"
"\n"
"Sidenote: another way to get stat+parameter information is via 3dAttribute,\n"
"and in particular asking for the \"BRICK_STATAUX\" information. That output\n"
"is probably a bit more cryptic, but it is described on the attributes page,\n"
"which users may look upon here:\n"
"https://afni.nimh.nih.gov/pub/dist/doc/program_help/README.attributes.html\n"
"and tremble.\n"
"\n"
"--------------------------------------------------------------------------\n"
"\n"
"Examples ~1~ \n"
"\n"
"In all cases note the use of the single quotes around the subbrick\n"
"selector-- these are necessary in some shell types!\n"
"\n"
"1) Do a calculation and display various information to screen:\n"
"     p2dsetstat                                       \\\n"
"         -inset stats.sub01+tlrc\"[2]\"                 \\\n"
"         -pval 0.001                                  \\\n"
"         -bisided\n"
"\n"
"2) Do a calculation and just display a single number (and also\n"
"   use a string label to conveniently select the subbrick):\n"
"     p2dsetstat                                       \\\n"
"         -inset stats.sub01+tlrc\"[Full_Fstat]\"      \\\n"
"         -pval 0.0005                                 \\\n"
"         -1sided                                      \\\n"
"         -quiet\n"
"\n"
"3) Do a calculation and store the output number as a variable,\n"
"   here using tcsh syntax:\n"
"     set my_stat = `p2dsetstat                        \\\n"
"                     -inset stats.sub02+tlrc\"[8]\"     \\\n"
"                     -pval 0.001                      \\\n"
"                     -bisided                         \\\n"
"                     -quiet`\n"
"\n"
"4) Do a calculation and store the output number into a text\n"
"   file:\n"
"     p2dsetstat                                       \\\n"
"         -inset stats.sub02+tlrc\"[8]\"                 \\\n"
"         -pval 0.001                                  \\\n"
"         -bisided                                     \\\n"
"         -quiet > MY_STAT_FILE.txt\n"
"\n"
"==========================================================================\n"
"\n",
author, version, rev_dat );

	return 0;
}

int main(int argc, char *argv[]) {

   int iarg;
   PARAMS_statpval InOpts;
   
   THD_3dim_dataset *dset_inp = NULL;

   int num_side_arg = 0;  // count how many sidedness args were inp
   
   int nt;
   int scode;

   int HAVE_INP_VAL = 0; // to ensure user input a value

   mainENTRY("p2dsetstat"); machdep(); 

   // fill option struct with defaults
   InOpts = set_p2dsetstat_defaults();
  
   // ****************************************************************
   //                  parse command line arguments
   // ****************************************************************
	
   /* no command line args -> see the help */
   if (argc == 1) { usage_p2dsetstat(); exit(0); }

   /* scan through args */
   iarg = 1; 
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_p2dsetstat();
         exit(0);
      }

      if( strcmp(argv[iarg],"-inset") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);
         InOpts.input_name = strdup(argv[iarg]);
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-pval") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);
         InOpts.pval = atof(argv[iarg]);
         HAVE_INP_VAL = 1;
         iarg++ ; continue ;
      }

      // read in any/all here, and below will check that exactly one of
      // these was input
      if( strcmp(argv[iarg],"-1sided") == 0 ){
         InOpts.sidedness  = strdup("1sided");
         InOpts.as_1_sided = 1;
         num_side_arg++;
         iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-2sided") == 0 ){
         InOpts.sidedness  = strdup("2sided");
         InOpts.as_1_sided = 0;
         num_side_arg++;
         iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-bisided") == 0 ){
         InOpts.sidedness  = strdup("bisided");
         InOpts.as_1_sided = 0;
         num_side_arg++;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-quiet") == 0 ){
         InOpts.verb = 0;
         iarg++ ; continue ;
      }

      // whine about any bad option, but also try to be helpful in that case
      ERROR_message("Bad option '%s'\n",argv[iarg]);
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);
   }
	
   // ****************************************************************
   //               verify presence+behavior of inputs
   // ****************************************************************

   /* -------------------------- check inputs ------------------------------ */

   // check sidedness args
   if ( num_side_arg < 1 )
      ERROR_exit("Need to input exactly one of these sidedness options:\n"
                 "   -1sided, -2sided, -bisided");
   else if ( num_side_arg > 1 )
      ERROR_exit("Can only use exactly one of these sidedness options:\n"
                 "   -1sided, -2sided, -bisided");

   // pvalue used and in OK range --- [PT] why not include 0 and 1 ???
   if ( !HAVE_INP_VAL )
      ERROR_exit("User must enter '-pval ..' value.");
   if ( InOpts.pval < 0.0 || InOpts.pval > 1.0 )
      ERROR_exit("The pvalue can only be in range (0, 1).");
      //ERROR_exit("The pvalue can only be in range [0, 1].");

   // check input dset
   if ( !InOpts.input_name )
      ERROR_exit("You need to provide an input dset with '-inset ..'");

   dset_inp = THD_open_dataset(InOpts.input_name);
   if ( dset_inp == NULL )
      ERROR_exit("Can't open dataset '%s'", argv[iarg]);
   DSET_load(dset_inp); CHECK_LOAD_ERROR(dset_inp);

   nt = DSET_NVALS(dset_inp); 
   if ( nt != 1 )
      ERROR_exit("Input dset must have exactly one volume. Perhaps use\n"
                 "   subbrick selectors, e.g., to choose DSET's [3]rd volume:\n"
                 "   -inset DSET\"[3]\"");

   // check volume has stat info
   scode = DSET_BRICK_STATCODE(dset_inp, 0);
   if ( scode <= 0 )
      ERROR_exit("Checked statcode for input dset, but it does not look like a "
                 "recognized statistical volume.");

   // ****************************************************************
   //                         Actual work
   // ****************************************************************

   // stat value
   InOpts.statval = THD_volume_pval_to_thresh(dset_inp, 0,
                                              InOpts.pval, 
                                              InOpts.as_1_sided);

   // stat name and params as convenient string
   InOpts.stat_and_pars = THD_make_statsym_string(dset_inp, 0);

   // subbrick label
   InOpts.brick_lab = strdup(DSET_BRICK_LABEL(dset_inp, 0));

   // **************************************************************
   //                 Store and output
   // **************************************************************
   
   if ( InOpts.verb ){
      fprintf(stdout, "++ Found input file  : %s\n", InOpts.input_name);
      fprintf(stdout, "++ Subbrick label    : %s\n", InOpts.brick_lab);
      if ( InOpts.pval <= 1.e-6)
         fprintf(stdout, "++ p-value           : %e\n", InOpts.pval);
      else
         fprintf(stdout, "++ p-value           : %.8f\n", InOpts.pval);
      fprintf(stdout, "++ stat type and par : %s\n", InOpts.stat_and_pars);
      fprintf(stdout, "++ sidedness         : %s\n", InOpts.sidedness);
      fprintf(stdout, "++ Final stat val    : %.6f\n", InOpts.statval);
   }
   else
      fprintf(stdout, "%.6f", InOpts.statval);
   
   
   // ****************************************************************
   //                           Freeing
   // ****************************************************************
   
   if( dset_inp ){
      DSET_delete(dset_inp);
      free(dset_inp);
   }
   
   return 0;
}
