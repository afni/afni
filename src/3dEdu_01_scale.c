/* 
   This is an educational/starter program for writing your own AFNI
   program.  

   To ensure this program is compiled, see how it is added in
   Makefile.INCLUDE (details of which might change depending on
   dependencies used here) by searching for instances of
   '3dEdu_01_scale'.
*/


/* 
   NOTE: INCLUDING HEADER FILES

   The files in "double quotes" are first searched for in this source
   file's local directory, and those in <angle brackets> are searched
   for in "a sequence of implementation-defined places" (according to
   C standard).  

   In practice, AFNI-created header files and those distributed with
   the AFNI source code are included in "double quotes", while
   standard C libraries and external dependencies are included in
   <angle brackets>.
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include "mrilib.h"
#include "3ddata.h"


/*
  NOTE: HELP TEXT

  The following function displays help for the program.  We try to
  keep the content width <=80 chars for readability.

  Each option should be listed with a single hyphen.  AFNI's apsearch
  program will parse all program help files and enable
  tab-autocompletion of program options from the command line. It will
  recognize an option as a string starting with '-' that has only
  white space to its left.  Hence, we list options like:

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
int usage_3dEdu_01_scale() 
{
   char *author = "PA Taylor";

   printf(
"\n"
"Overview ~1~ \n"
"\n"
"This is an example starting program for those who want to create a new\n"
"AFNI program to see some examples of possible I/O and internal calcs.\n"
"Please see the source code file in the main afni/src/3dEdu_01_scale.c\n"
"for more information.\n"
"\n"
"This program is intended purely for educational and code-development\n"
"purposes.\n"
"\n"
"written by: %s\n"
"\n"
"Description ~2~ \n"
"\n"
"This program will take one dataset as input, and output a copy of its [0]th\n"
"volume.  A mask can be provided, as well as two multiplicative factors to\n"
"mask and scale the output, respectively.\n"
"\n"
"==========================================================================\n"
"\n"
"Command usage and option list ~1~ \n"
"\n"
"  3dEdu_01_scale [something]\n"
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
"  -mult_facs  A B  :(opt) numerical factors for multiplying each voxel;\n"
"                    that is, each voxel is multiplied by both A and B.\n"
"\n"
"==========================================================================\n"
"\n"
"Examples ~1~\n"
"\n"
"1) Output a copy of the [0]th volume of the input:\n"
"\n"
"   3dEdu_01_scale                                   \\\n"
"      -input  epi_r1+orig.HEAD                      \\\n"
"      -prefix OUT_edu_01                            \n"
"\n"
"2) Output a masked copy of the [0]th volume of the input:\n"
"\n"
"   3dEdu_01_scale                                   \\\n"
"      -input  epi_r1+orig.HEAD                      \\\n"
"      -mask   mask.auto.nii.gz                      \\\n"
"      -prefix OUT_edu_02                            \n"
"\n"
"3) Output a masked+scaled copy of the [0]th volume of the input:\n"
"\n"
"   3dEdu_01_scale                                   \\\n"
"      -mult_facs 3 5.5                              \\\n"
"      -input     epi_r1+orig.HEAD                   \\\n"
"      -mask      mask.auto.nii.gz                   \\\n"
"      -prefix    OUT_edu_03                         \n"
"\n"
"==========================================================================\n"
"\n",
author );

	return 0;
}

/*
  NOTE: FIND AND USE FUNCTIONS

  Below we should some basic I/O and other functionality for dealing
  with datasets.  We list the files where most of those functions and
  macros are found.

  - Note 1 -
  If I want to find where a function is defined in the large AFNI
  codebase, I will often use something like the following grep:
   
      grep -H FUNC_NAME *.c  | grep TYPE_RETURNED

  If I can't tell the type returned from usage, then I might grep
  through header files first to find it:

     grep -H FUNC_NAME *.h 

  ... and then use that new-found TYPE_RETURNED information to refine
  the grep through *.c files.  

  One might have to resort to recursive searching, if the function
  might be defined in the SUMA/ or other subdirectory, such as with:

     grep -r FUNC_NAME .  | grep TYPE_RETURNED
   
  - Note 2 -
  Most DSET_*(...) functionalities are macros that are defined in
  3ddata.h.
*/
int main(int argc, char *argv[]) {
   /*
     Initializing+declaring variables.  Pointers should be set to
     NULL, to allow for easy checking+usage+freeing later.
   */

   int iarg;
   int idx;

   int DO_SOME_OPT = 0;
   float *mult_facs = NULL;

   /*
     THD_3dim_dataset: One of the main AFNI input datastructures.  It
     can actually can be used quite generally beyond 'just' 3D vols.
     This is one way that so many 3d* programs run usefully on 3D or
     4D volumes, or surfaces, or (transposed) *.1D files, ...

     This datastructure is defined in 3ddata.h, search for:
        'typedef struct THD_3dim_dataset'.
   */
   THD_3dim_dataset *dset_inp = NULL, *dset_mask = NULL;
   THD_3dim_dataset *dset_out1 = NULL;

   char *prefix = NULL;
   int nx, ny, nz, nvox, nvals;

   // an array which will be used to populate the output dset
   float *arr_mskd = NULL;

   mainENTRY("3dEdu_01_scale"); machdep(); 
  
   // ****************************************************************
   //                  parse command line arguments
   // ****************************************************************
	
   /* no command line args -> see the help */
   if (argc == 1) { usage_3dEdu_01_scale(); exit(0); }

   /* scan through args */
   iarg = 1; 
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_3dEdu_01_scale();
         exit(0);
      }
			 
      /* Option taking 1 arg (here, a dset filename)
         + the first 'if' condition applies generally to any opt with 1 arg
         + since the arg is a dset name, we check+load it here, too

         For:
         + THD_open_dataset(...), see thd_mastery.c
         + DSET_load(...), see 3ddata.h
         + CHECK_LOAD_ERROR(...), see mrilib.h
      */
      if( strcmp(argv[iarg],"-input") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         dset_inp = THD_open_dataset(argv[iarg]);
         if( (dset_inp == NULL ))
            ERROR_exit("Can't open dataset '%s'", argv[iarg]);
         DSET_load(dset_inp); CHECK_LOAD_ERROR(dset_inp);

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-mask") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         dset_mask = THD_open_dataset(argv[iarg]);
         if( dset_mask == NULL )
            ERROR_exit("Can't open dataset '%s'", argv[iarg]);
         DSET_load(dset_mask); CHECK_LOAD_ERROR(dset_mask);
			
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         prefix = strdup(argv[iarg]) ;

         // check if the name for output is allowed
         if( !THD_filename_ok(prefix) ) 
            ERROR_exit("Illegal name after '%s'", argv[iarg-1]);
         iarg++ ; continue ;
      }

      // Option flag: takes no args
      if( strcmp(argv[iarg],"-some_opt") == 0) {
         DO_SOME_OPT = 1;
         iarg++ ; continue ;
      }

      //  Option requires two inputs, both numerical
      if( strcmp(argv[iarg],"-mult_facs") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         // allocate and verify
         mult_facs = (float *)calloc(2, sizeof(float));
         if( mult_facs == NULL )
            ERROR_exit("MemAlloc failure.\n");

         mult_facs[0] = atof(argv[iarg]);

         // check again for second argument; note argv[] index
         if( ++iarg >= argc ) 
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

   /*
     Examples of ways to check for required inputs, as well as
     checking consistency of some (e.g., if dsets have the same grid).

     NB: the *_message() functions are useful for communicating with
     the user.  These are each defined in debugtrace.c.
   */

   INFO_message("Starting to check inputs...");

   if ( !DO_SOME_OPT ) 
      WARNING_message("Are you sure you didn't want to use the flag "
                      "-some_opt'? (Juuust an example warning.)");

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

   if ( !prefix )
      ERROR_exit("Need an output name via '-prefix ..'\n");

   /* 
      These DSET_*(...) macros are defined in 3ddata.h---along with
      many other macros to access THD_3dim_dataset elements.
   */
   nx = DSET_NX(dset_inp);
   ny = DSET_NY(dset_inp); 
   nz = DSET_NZ(dset_inp); 
   nvox = DSET_NVOX(dset_inp);
   nvals = DSET_NVALS(dset_inp); 

   INFO_message("The matrix dims of the input dset are:  %d %d %d\n"
                "   ... for a total of %d voxels per volume.\n"
                "   The input has %d total time point(s).", 
                nx, ny, nz, nvox, nvals);

   // make a 1D array of appropriate length for the dset...
	arr_mskd = (float *)calloc(nvox, sizeof(float)); 

   // ... and check that allocation went OK
   if( arr_mskd == NULL ) { 
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(2);
   }

   // ****************************************************************
   //                         Actual work
   // ****************************************************************

   /*
     Go through the array making some calc: here, get the [idx]th
     value from [0]th subvolume of the 'dset_mask' and 'dset_inp'
     datasets.

     For THD_get_voxel(...), see thd_loaddblk.c.  
   */
   for( idx=0 ; idx<nvox ; idx++ ) {
      if ( dset_mask ) {
         if ( THD_get_voxel(dset_mask, idx, 0) != 0.0 ) 
            arr_mskd[idx] = THD_get_voxel(dset_inp, idx, 0);
      }
      else
         arr_mskd[idx] = THD_get_voxel(dset_inp, idx, 0);

      if ( arr_mskd[idx] != 0.0 && mult_facs ) 
         arr_mskd[idx]*= mult_facs[0] * mult_facs[1];
   }
   
   // **************************************************************
   //                 Store and output
   // **************************************************************
   
   /* 
      Prepare header for output by copying that of input, and then
      changing items as necessary.

      For:
      + EDIT_empty_copy(...), see edt_emptycopy.c
      + EDIT_dset_items(...), see edt_dsetitems.c
      + EDIT_substitute_brick(...), see edt_substbrick.c

      NB: EDIT_dset_items() is the *only* way you should change
      elements in a THD_3dmin_dataset structure.
   */
   dset_out1 = EDIT_empty_copy( dset_inp ); 
   EDIT_dset_items(dset_out1,
                   ADN_nvals, 1,                 // just one brick here
						 ADN_datum_all, MRI_float ,    
                   ADN_prefix, prefix,
						 ADN_none );                   /* Always last param */  
   
   // Provide values for the dset_out1 from the appropriately-sized
   // array (and then nullify the arr_mskd, to not have 2 pointers to
   // the same place)
	EDIT_substitute_brick(dset_out1, 0, MRI_float, arr_mskd); 
	arr_mskd=NULL;

   /*
     Prepare to write out dataset.

     For: 
     + THD_load_statistics(...), see thd_bstats.c
     + THD_ok_overwrite(), see afni_environ.c
     + THD_is_ondisk(...), see thd_filestuff.c
     + DSET_HEADNAME(...), see 3ddata.h
     + tross_Make_History(...), see thd_notes.c
     + THD_write_3dim_dataset(...), see thd_writedset.c
     + DSET_delete(...), see 3ddata.h
   */
	THD_load_statistics( dset_out1 );
	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(dset_out1)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(dset_out1));
	tross_Make_History("3dEdu_01_scale", argc, argv, dset_out1);

   // write and free dset 
	THD_write_3dim_dataset(NULL, NULL, dset_out1, True);
	DSET_delete(dset_out1); 
  	free(dset_out1); 


   // ****************************************************************
   //                           Freeing
   // ****************************************************************

   /* 
      Note the use of both DSET_delete() and free() with
      THD_3dim_dataset type (and also above).  The first cleans up
      the internal dataset contents, and the latter frees the pointer
      itself.
   */
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

   return 0;
}
