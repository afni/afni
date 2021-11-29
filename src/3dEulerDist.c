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
#include <float.h>
#include "debugtrace.h"
#include "mrilib.h"
#include "3ddata.h"
#include "thd_euler_dist.c"

#define BIG FLT_MAX     // from float.h

int run_EDT_3D( int comline, PARAMS_euler_dist opts,
                int argc, char *argv[] );
int calc_EDT_3D( float ***arr_dist, PARAMS_euler_dist opts,
                 THD_3dim_dataset *dset_roi, int ival);

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
   ii = run_EDT_3D(1, InOpts, argc, argv);

   return 0;
}


int run_EDT_3D( int comline, PARAMS_euler_dist opts,
                int argc, char *argv[] )
{
   int i, j, k;
   int nn;
	THD_3dim_dataset *dset_roi = NULL;          // input
	THD_3dim_dataset *dset_edt = NULL;          // output

   int nx, ny, nz, nvals;
   
   nx = DSET_NX(dset_roi);
   ny = DSET_NY(dset_roi);
   nz = DSET_NZ(dset_roi);

   nvals = DSET_NVALS(dset_roi);

   for( nn=0 ; nn<nvals ; nn++ ){
      float ***arr_dist = NULL;           // array that will hold dist values

      arr_dist = (float ***) calloc( nx, sizeof(float **) );
      for ( i=0 ; i<nx ; i++ ) 
         arr_dist[i] = (float **) calloc( ny, sizeof(float *) );
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            arr_dist[i][j] = (float *) calloc( nz, sizeof(float) );
      if( arr_dist == NULL ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(12);
      }

      i = calc_EDT_3D(arr_dist, opts, dset_roi, nn);

      // SOMETHING HERE TO CONVERT OUTPUT ARR TO OUTPUT DATASET

      if(arr_dist){
         for ( i=0 ; i<nx ; i++ ) 
            for ( j=0 ; j<ny ; j++ ) 
               free(arr_dist[i][j]);
         for ( i=0 ; i<nx ; i++ ) 
            free(arr_dist[i]);
         free(arr_dist);
      }
   } // end of loop over nvals



   // free

   return 0;
}

/*
  arr_dist :  initialized 3D distance array of floats ('odt' in lib_EDT.py)
              -> what is filled in here
  opts     :  struct containing options 
  dset_roi :  the ROI map (dataset, basically 'im' in lib_EDT.py)
  ival     :  index value of the subbrick/subvolume to analyze
  */
int calc_EDT_3D( float ***arr_dist, PARAMS_euler_dist opts,
                 THD_3dim_dataset *dset_roi, int ival)
{
   int i, j, k, idx;
   float minmax[2] = {0, 0};
   int nx, ny, nz;

   float Ledge[3];            // voxel edge lengths ('edims' in lib_EDT.py)
   int vox_ord_rev[3];

   // check if a subbrick is const; there are a couple cases where we
   // would be done at this stage.
   i = THD_subbrick_minmax( dset_roi, ival, 1, &minmax[0], &minmax[1] );
   if ( minmax[0] == minmax[1] ){
      if ( minmax[1] == 0.0 ){
         WARNING_message("Constant (zero) subbrick: %d", ival);
         return 0;
      }
      else if( !opts.bounds_are_zero ){
         WARNING_message("Constant (nonzero) subbrick, "
                         "and no bounds_are_zero: %d", ival);
         return 0;
      }
   }

   // dset properties we need to know
   nx = DSET_NX(dset_roi);
   ny = DSET_NY(dset_roi);
   nz = DSET_NZ(dset_roi);
   Ledge[0] = fabs(DSET_DX(dset_roi)); 
   Ledge[1] = fabs(DSET_DY(dset_roi)); 
   Ledge[2] = fabs(DSET_DZ(dset_roi)); 

   // initialize distance array to BIG
   for ( i=0 ; i<nx ; i++ ) 
      for ( j=0 ; j<ny ; j++ ) 
         for ( k=0 ; k<nz ; k++ ) 
            arr_dist[i][j][k] = BIG;

   // find axis order of decreasing voxel sizes, to avoid pathology in
   // the EDT alg
   i = sort_vox_ord_desc(3, Ledge, vox_ord_rev);

   for( i=0 ; i<3 ; i++ ){
      switch( vox_ord_rev[i] ){

      case 0 :
         j = calc_EDT_3D_dim0( arr_dist, opts, dset_roi, ival );
         break;
      case 1 :
         j = calc_EDT_3D_dim1( arr_dist, opts, dset_roi, ival );
         break;
      case 2 :
         j = calc_EDT_3D_dim2( arr_dist, opts, dset_roi, ival );
         break;
      default:
         WARNING_message("Should never be here in EDT prog");
      }
   } // end of looping over axes

   // Zero out EDT values in "zero" ROI?
    if( opts.zeros_are_zeroed ) {
       idx = 0;
       for ( i=0 ; i<nx ; i++ ) 
          for ( j=0 ; j<ny ; j++ ) 
             for ( k=0 ; k<nz ; k++ ){
                if( !THD_get_voxel(dset_roi, idx, ival) )
                   arr_dist[i][j][k] = 0.0;
                idx++;
             }
    }

    // Output distance-squared, or just distance (sqrt of what we have
    // so-far calc'ed)?
    if( opts.do_sqrt ) {
       idx = 0;
       for ( i=0 ; i<nx ; i++ ) 
          for ( j=0 ; j<ny ; j++ ) 
             for ( k=0 ; k<nz ; k++ ){
                arr_dist[i][j][k] = (float) sqrt(arr_dist[i][j][k]);
                idx++;
             }
    }

   return 0;
}



