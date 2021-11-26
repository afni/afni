/* 
   This is an educational/starter program for writing your own AFNI
   program.  

   To ensure this program is compiled, see how it is added in
   Makefile.INCLUDE (details of which might change depending on
   dependencies used here) by searching for instances of '3dEduProg'.
*/


/* 
   Files to include (dependencies).  The files in "double quotes" are
   first searched for in this source file's local directory, and those
   in <angle brackets> are searched for in "a sequence of
   implementation-defined places".  In practice, AFNI-created header
   files and those distributed with the AFNI source code are included
   in "double quotes", while standard C libraries and external
   dependencies are included in <angle brackets>.
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include "mrilib.h"
#include "3ddata.h"


/*
  HELP TEXT NOTES

  The following function displays a help file for the program.  We try
  to keep the content width <=80 chars for readability.

  Each option should be listed with a single hyphen.  The 'apsearch'
  program will parse all help files and enable tab-autocompletion of
  program options from the command line. It will recognize an option
  as a string starting with '-' that has only white space to its left.
  Hence, we list options like:
      -opt0   DSET    :something that requires a dset input
      -opt_1  A B C   :something that requires three inputs
      -opt2_name      :something that is a flag
  Note: it is useful to clearly distinguish between option flags and
  options that take one or more arguments, as above. 

  Section headings should have '~1~' to the right of the title, and
  subsections should have a '~2~'.  During documentation building,
  this simple syntax will be parsed to create a table of contents and
  prettier title for the RST/Sphinx-based documentation.  As an output
  example, see:
  https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/programs/afni_proc.py_sphx.html#ahelp-afni-proc-py
*/
int usage_3dEduProg(int detail) 
{
   char *author = "L. Cranston";

   printf(
"\n"
"Overview ~1~ \n"
"\n"
"This is an example starting program for those who want to create a new\n"
"AFNI program to see some examples of possible I/O and internal calcs.\n"
"Please see the source code file in the main afni/src/3dEduProg.c\n"
"for more information.\n"
"\n"
"This program is intended purely for educational and code-development\n"
"purposes.\n"
"\n"
"written by: %s\n"
"\n"
"==========================================================================\n"
"\n"
"Command usage and option list ~1~ \n"
"\n"
"  3dEduProg [something]\n"
"\n"
"where: \n"
"\n"
"  -input DSET      :(req) input dataset\n"
"\n"
"  -mask  DSET_MASK :(opt) mask dataset on same grid/data structure\n"
"                    as the input dset\n"
"\n"
"  -some_opt        :(opt) option flag to do something\n"
"\n"
"  -mult_facs  A B  :(opt) numerical factors for multiplying each voxel value;\n"
"                    that is, each voxel is multiplied by both A and B.\n"
"\n"
"==========================================================================\n"
"\n"
"Examples ~1~\n"
"\n"
"1) Description ... *** \n"
"\n"
"\n"
"2) Another case ... *** \n"
"\n"
"==========================================================================\n"
"\n",
author );

	return 0;
}

int main(int argc, char *argv[]) {
   
   int iarg;

   int DO_SOME_OPT = 0;
   float *mult_facs = NULL;

   THD_3dim_dataset *dset_inp = NULL;
   THD_3dim_dataset *dset_mask = NULL;

   char *prefix = NULL;

   int Nvox=-1;  
   int *Dim=NULL;

   mainENTRY("3dEduProg"); machdep(); 
  
   // ****************************************************************
   //                  parse command line arguments
   // ****************************************************************
	
   /** scan through args **/
   if (argc == 1) { usage_3dEduProg(1); exit(0); }
   iarg = 1; 
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_3dEduProg(strlen(argv[iarg])>3 ? 2:1);
         exit(0);
      }
			 
      // Option requires input filename
      if( strcmp(argv[iarg],"-input") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         dset_inp = THD_open_dataset(argv[iarg]);

         // read in and check dset
         if( (dset_inp == NULL ))
            ERROR_exit("Can't open dataset '%s'", argv[iarg]);
         DSET_load(dset_inp); CHECK_LOAD_ERROR(dset_inp);

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-mask") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         dset_mask = THD_open_dataset(argv[iarg]);
         if( dset_mask == NULL )
            ERROR_exit("Can't open dataset '%s'", argv[iarg]);
         DSET_load(dset_mask); CHECK_LOAD_ERROR(dset_mask);
			
         iarg++ ; continue ;
      }

      // Option requires one arg: whine if none given.
      if( strcmp(argv[iarg],"-prefix") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '%s'", argv[iarg-1]);
         prefix = strdup(argv[iarg]) ;

         // check if the name for output is allowed
         if( !THD_filename_ok(prefix) ) 
            ERROR_exit("Illegal name after '%s'", argv[iarg-1]);
         iarg++ ; continue ;
      }

      // Option flag example: takes no args
      if( strcmp(argv[iarg],"-some_opt") == 0) {
         DO_SOME_OPT = 1;
         iarg++ ; continue ;
      }

      //  Option requires two inputs, both numerical
      if( strcmp(argv[iarg],"-mult_facs") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         mult_facs = (float *)calloc(2, sizeof(float));

         mult_facs[0] = atof(argv[iarg]);

         // ... and again for second argumnet
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need 2nd argument after '%s'", argv[iarg-2]);
         mult_facs[1] = atof(argv[iarg]);

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

   // NB: the *_message() functions are useful for communicating with the user

   INFO_message("Starting to check inputs...");

   if ( !DO_SOME_OPT ) 
      WARNING_message("Are you sure you didn't want to use the flag "
                      "-some_opt'?");

   if ( !dset_inp ) { 
      ERROR_message("You need to provide an input dset with '-input ..'");
      exit(1);
   }

   if ( dset_mask ) {
      INFO_message("User input a mask");

      // check that mask and input can be analyzed together
      if( THD_dataset_mismatch( dset_inp , dset_mask ) )
         ERROR_exit("Mismatch between input and mask dsets!\n");
   }
   
   Dim  = (int *)calloc(4, sizeof(int));
   Dim[0] = DSET_NX(dset_inp); Dim[1] = DSET_NY(dset_inp); 
   Dim[2] = DSET_NZ(dset_inp); Dim[3] = DSET_NVALS(dset_inp); 

   Nvox = DSET_NVOX(dset_inp);

   INFO_message("The matrix dims of the input dset are:  %d %d %d\n"
                "   ... for a total of %d voxels per volume.\n"
                "   The input has %d total time point(s).", 
                Dim[0], Dim[1], Dim[2], Nvox, Dim[3]);

   // ****************************************************************
   //                         Actual work
   // ****************************************************************




	

   // **************************************************************
   //                 Store and output
   // **************************************************************





   // ****************************************************************
   //                           Freeing
   // ****************************************************************

   if( dset_inp ){
      DSET_delete(dset_inp);
      free(dset_inp);
   }

   if( dset_mask ) {
      DSET_delete(dset_mask);
      free(dset_mask);
   }

   if( mult_facs ) 
      free(mult_facs);
 
   if( prefix )
      free(prefix);

   if( Dim )
      free(Dim);
	
   return 0;
}
