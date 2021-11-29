/* 
   Working from P. Lauren's distanceField.c, which was made in
   connection with PA Taylor's Python library for this.

# ==========================================================================
#
# This code calculates the Euclidean Distance Transform (EDT) for 3D
# volumes following this nice, efficient algorithm, by Felzenszwalb
# and Huttenlocher (2012;  FH2012):
#
#   Felzenszwalb PF, Huttenlocher DP (2012). Distance Transforms of
#   Sampled Functions. Theory of Computing 8:415-428.
#   https://cs.brown.edu/people/pfelzens/papers/dt-final.pdf
#
# Another useful/illustrative resource abotu this is by Philip Rideout:
#
#   https://prideout.net/blog/distance_fields/
#
# The current code here extends/tweaks the FH2012 algorithm to a more
# general case of having several different ROIs present, for running
# in 3D (trivial extension), and for having voxels of non-unity and
# non-isotropic lengths.  It does this by utilizing the fact that at
# its very heart, the FH2012 algorithm works line by line and can even
# be thought of as working boundary-by-boundary.
#
# Here, the zero-valued "background" is also just treated like an ROI,
# with one difference.  At a FOV boundary, the zero-valued
# ROI/backgroud is treated as open, so that the EDT value at each
# "zero" voxel is always to one of the shapes within the FOV.  For
# nonzero ROIs, one can treat the FOV boundary *either* as an ROI edge
# (EDT value there will be 1 edge length) *or* as being open.
#
# written by:  PA Taylor (NIH)
#

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <float.h>
#include "mrilib.h"
#include "3ddata.h"
#include "thd_euler_dist.c"

#define BIG FLT_MAX     // from float.h

int calc_EDT_3D( int comline, PARAMS_euler_dist opts,
                 int argc, char *argv[] );

// --------------------------------------------------------------------------

int usage_3dEulerDist() 
{
   char *author = "P Lauren and PA Taylor";

   printf(
"\n"
"Overview ~1~ \n"
"\n"
"This program calculates the Eulerian Distance Transform (EDT).\n"
"\n"
"written by: %s\n"
"\n"
"Description ~2~ \n"
"\n"
"\n"
"==========================================================================\n"
"\n"
"Command usage and option list ~1~ \n"
"\n"
"  3dEulerDist [something]\n"
"\n"
"where: \n"
"\n"
"  -input DSET      :(req) input dataset\n"
"\n"
"==========================================================================\n"
"\n"
"Examples ~1~\n"
"\n"
"\n"
"==========================================================================\n"
"\n",
author );

	return 0;
}

int main(int argc, char *argv[]) {

   int ii = 0;
   int iarg;
   PARAMS_euler_dist InOpts;

   mainENTRY("3dEulerDist"); machdep(); 
  
   // fill option struct with defaults
   InOpts = set_euler_dist_defaults();

   // ****************************************************************
   //                  parse command line arguments
   // ****************************************************************
	
   if (argc == 1) { usage_3dEulerDist(); exit(0); }

   /* scan through args */
   iarg = 1; 
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_3dEulerDist();
         exit(0);
      }
			 
      if( strcmp(argv[iarg],"-input") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         InOpts.input_name = strdup(argv[iarg]) ;

         iarg++ ; continue ;
      }

      /*
      if( strcmp(argv[iarg],"-mask") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         dset_mask = THD_open_dataset(argv[iarg]);
         if( dset_mask == NULL )
            ERROR_exit("Can't open dataset '%s'", argv[iarg]);
         DSET_load(dset_mask); CHECK_LOAD_ERROR(dset_mask);
			
         iarg++ ; continue ;
      }
      */

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         InOpts.prefix = strdup(argv[iarg]) ;

         if( !THD_filename_ok(InOpts.prefix) ) 
            ERROR_exit("Illegal name after '%s'", argv[iarg-1]);
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-zeros_are_zero") == 0) {
         InOpts.zeros_are_zeroed = 1;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-bounds_are_not_zero") == 0) {
         InOpts.bounds_are_zero = 0;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-dist_square") == 0) {
         InOpts.do_sqrt = 1;
         iarg++ ; continue ;
      }

      ERROR_message("Bad option '%s'\n",argv[iarg]);
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);
   }
	
   // ****************************************************************
   //               verify presence+behavior of inputs
   // ****************************************************************

   INFO_message("Starting to check inputs...");


   if ( !InOpts.input_name ) { 
      ERROR_message("You need to provide an input dset with '-input ..'");
      exit(1);
   }

   if ( !InOpts.prefix )
      ERROR_exit("Need an output name via '-prefix ..'\n");

   // DONE FILLING, now call
   ii = calc_EDT_3D(1, InOpts, argc, argv);


   return 0;
}


int calc_EDT_3D( int comline, PARAMS_euler_dist opts,
                 int argc, char *argv[] )
{

	THD_3dim_dataset *insetUC = NULL;



   return 0;
}
