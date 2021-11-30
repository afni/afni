/* 
   Working from P. Lauren's distanceField.c, which was made in
   connection with PA Taylor's Python library for this.

ver = 2.0;  date = Nov 29, 2021
+ [PT] this program has been a longtime coming.  This version merges
  P Taylor's Python version in lib_EDT.py with P Lauren's concurrent
  work on a C version (which had been compared/developed in part with 
  the aformentioned lib_EDT.py).


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

int run_EDT_3D( int comline, PARAMS_euler_dist opts,
                int argc, char *argv[] );
int calc_EDT_3D( float ***arr_dist, PARAMS_euler_dist opts,
                 THD_3dim_dataset *dset_roi, int ival);

// --------------------------------------------------------------------------

int usage_3dEulerDist() 
{
   char *author = "P Lauren and PA Taylor (SSCC, NIMH, NIH)";

   printf(
"\n"
"Overview ~1~ \n"
"\n"
"This program calculates the Eulerian Distance Transform (EDT).\n"
"\n"
"Basically, this means calculating the Euclidean distance of each\n"
"voxel's centroid to the nearest boundary  with a separate ROI (well, to be\n"
"brutally technical, to centroid of the nearest voxel in a neighboring ROI.\n"
"The input dataset should be a map of ROIs (so, integer-valued). The\n"
"EDT values are calculated throughout the entire FOV by default,\n"
"even in the zero/background regions (there is an option to control this).\n"
"\n"
"written by: %s\n"
"\n"
"Description ~2~ \n"
"\n"
"This code calculates the Euclidean Distance Transform (EDT) for 3D\n"
"volumes following this nice, efficient algorithm, by Felzenszwalb\n"
"and Huttenlocher (2012;  FH2012):\n"
"\n"
"   Felzenszwalb PF, Huttenlocher DP (2012). Distance Transforms of\n"
"   Sampled Functions. Theory of Computing 8:415-428.\n"
"   https://cs.brown.edu/people/pfelzens/papers/dt-final.pdf\n"
"\n"
"Thanks to C. Rorden for pointing this paper out and discussing it.\n"
"\n"
"The current code here extends/tweaks the FH2012 algorithm to a more\n"
"general case of having several different ROIs present, for running\n"
"in 3D (trivial extension), and for having voxels of non-unity and\n"
"non-isotropic lengths.  It does this by utilizing the fact that at\n"
"its very heart, the FH2012 algorithm works line by line and can even\n"
"be thought of as working boundary-by-boundary.\n"
"\n"
"Here, the zero-valued 'background' is also just treated like an ROI,\n"
"with one difference.  At a FOV boundary, the zero-valued\n"
"ROI/backgroud is treated as open, so that the EDT value at each\n"
"'zero' voxel is always to one of the shapes within the FOV.  For\n"
"nonzero ROIs, one can treat the FOV boundary *either* as an ROI edge\n"
"(EDT value there will be 1 edge length) *or* as being open.\n"
"\n"
"==========================================================================\n"
"\n"
"Command usage and option list ~1~ \n"
"\n"
"    3dEulerDist [options] -prefix PREF -input DSET\n"
"\n"
"where: \n"
"\n"
"  -input DSET      :(req) input dataset\n"
"\n"
"  -prefix PREF     :(req) output prefix name\n"
"\n"
"  -dist_squared    :by default, the output EDT volume contains distance\n"
"                    values.  By using this option, the output values are\n"
"                    distance**2.\n"
"\n"
"  -zeros_are_zero  :by default, EDT values are output for the full FOV,\n"
"                    even zero-valued regions.  If this option is used, EDT\n"
"                    values are only reported within the non-zero locations\n"
"                    of the input dataset.\n"
"\n"
"  -bounds_are_zero :this flag affects how FOV boundaries are treated for\n"
"                    nonzero ROIs: by default, they are viewed as ROI\n"
"                    boundaries (so the FOV is a closed boundary for an ROI);\n"
"                    but when this option is used, the ROI behaves as if it\n"
"                    continued 'infinitely' at the FOV boundary (so it is\n"
"                    an open boundary).  Zero-valued ROIs (= background)\n"
"                    are not affected by this option.\n"
"\n"
"==========================================================================\n"
"\n"
"Examples ~1~\n"
"\n"
"1) Basic case:\n"
"   3dEulerDist                                                     \\\n"
"       -input  roi_map.nii.gz                                      \\\n"
"       -prefix roi_map_EDT.nii.gz                                  \n"
"\n"
"2) Same as above, but only output distances within non-zero regions/ROIs:\n"
"   3dEulerDist                                                     \\\n"
"       -zeros_are_zero                                             \\\n"
"       -input  roi_map.nii.gz                                      \\\n"
"       -prefix roi_map_EDT_NZ.nii.gz                               \n"
"\n"
"3) Output distance-squared at each voxel:\n"
"   3dEulerDist                                                     \\\n"
"       -dist_squared                                               \\\n"
"       -input  mask.nii.gz                                         \\\n"
"       -prefix mask_EDT_SQ.nii.gz                                  \n"
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

      if( strcmp(argv[iarg],"-dist_squared") == 0) {
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
   int i, j, k, idx;
   int nn;
   int nx, ny, nz, nxy, nvox, nvals;
	THD_3dim_dataset *dset_roi = NULL;          // input
	THD_3dim_dataset *dset_edt = NULL;          // output

   float ***arr_dist = NULL;           // array that will hold dist values
   
   ENTRY("run_EDT_3D");

   dset_roi = THD_open_dataset(opts.input_name);
   if( (dset_roi == NULL ))
      ERROR_exit("Can't open dataset '%s'", opts.input_name);
   DSET_load(dset_roi); CHECK_LOAD_ERROR(dset_roi);

   nx = DSET_NX(dset_roi);
   ny = DSET_NY(dset_roi);
   nz = DSET_NZ(dset_roi);
   nxy = nx*ny;
   nvox = DSET_NVOX(dset_roi);
   nvals = DSET_NVALS(dset_roi);

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

   for( nn=0 ; nn<nvals ; nn++ ){
      float *tmp_arr = NULL;
      tmp_arr = (float *) calloc( nvox, sizeof(float) );

      i = calc_EDT_3D(arr_dist, opts, dset_roi, nn);

      for( i=0 ; i<nx ; i++ )
         for( j=0 ; j<ny ; j++ ) 
            for( k=0; k<nz ; k++ ) {
               idx = THREE_TO_IJK(i, j, k, nx, nxy);
               tmp_arr[idx] = arr_dist[i][j][k];
            }

      /* Prepare header for output by copying that of input, and then
         changing items as necessary */
      dset_edt = EDIT_empty_copy( dset_roi ); 
      EDIT_dset_items(dset_edt,
                      ADN_datum_all, MRI_float ,    
                      ADN_prefix, opts.prefix,
                      ADN_none );
      
      // provide volume values from the appropriately-sized array
      EDIT_substitute_brick(dset_edt, nn, MRI_float, tmp_arr); 
      tmp_arr=NULL;
    
   } // end of loop over nvals

   // free input dset
	DSET_delete(dset_roi); 
  	free(dset_roi); 

   // prepare to write output
	THD_load_statistics( dset_edt );
	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(dset_edt)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(dset_edt));
	tross_Make_History("3dEulerDist", argc, argv, dset_edt);

   // write and free dset 
	THD_write_3dim_dataset(NULL, NULL, dset_edt, True);
	DSET_delete(dset_edt); 
  	free(dset_edt); 

   // free more
   if(arr_dist){
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            free(arr_dist[i][j]);
      for ( i=0 ; i<nx ; i++ ) 
         free(arr_dist[i]);
      free(arr_dist);
   }

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
   int nx, ny, nz, nxy;

   float Ledge[3];            // voxel edge lengths ('edims' in lib_EDT.py)
   int vox_ord_rev[3];

   ENTRY("calc_EDT_3D");

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
   nxy = nx*ny;
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
      float *flarr=NULL;   // store distances along one dim
      int *maparr=NULL;    // store ROI map along one dim

      switch( vox_ord_rev[i] ){
         // note pairings per case: 0 and nx; 1 and ny; 2 and nz
      case 0 :
         INFO_message("Move along axis %d (delta = %.6f)", 
                      vox_ord_rev[i], Ledge[vox_ord_rev[i]]);
         flarr = (float *) calloc( nx, sizeof(float) );
         maparr = (int *) calloc( nx, sizeof(int) );
         if( flarr == NULL || maparr == NULL ) 
            ERROR_exit("MemAlloc failure: flarr/maparr\n");
         
         j = calc_EDT_3D_dim0( arr_dist, opts, dset_roi, ival, 
                               flarr, maparr );
         break;

      case 1 :
         INFO_message("Move along axis %d (delta = %.6f)", 
                      vox_ord_rev[i], Ledge[vox_ord_rev[i]]);
         flarr = (float *) calloc( ny, sizeof(float) );
         maparr = (int *) calloc( ny, sizeof(int) );
         if( flarr == NULL || maparr == NULL ) 
            ERROR_exit("MemAlloc failure: flarr/maparr\n");

         j = calc_EDT_3D_dim1( arr_dist, opts, dset_roi, ival, 
                               flarr, maparr );
         break;

      case 2 :
         INFO_message("Move along axis %d (delta = %.6f)", 
                      vox_ord_rev[i], Ledge[vox_ord_rev[i]]);
         flarr = (float *) calloc( nz, sizeof(float) );
         maparr = (int *) calloc( nz, sizeof(int) );
         if( flarr == NULL || maparr == NULL ) 
            ERROR_exit("MemAlloc failure: flarr/maparr\n");

         j = calc_EDT_3D_dim2( arr_dist, opts, dset_roi, ival, 
                               flarr, maparr );
         
         break;

      default:
         WARNING_message("Should never be here in EDT prog");
      }

      if( flarr) free(flarr);
      if( maparr) free(maparr);
   } // end of looping over axes

   // Zero out EDT values in "zero" ROI?
   if( opts.zeros_are_zeroed ) {
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            for ( k=0 ; k<nz ; k++ ){
               idx = THREE_TO_IJK(i, j, k, nx, nxy);
               if( !THD_get_voxel(dset_roi, idx, ival) )
                  arr_dist[i][j][k] = 0.0;
            }
   }

   // Output distance-squared, or just distance (sqrt of what we have
   // so-far calc'ed)
   if( opts.do_sqrt ) {
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            for ( k=0 ; k<nz ; k++ ){
               arr_dist[i][j][k] = (float) sqrt(arr_dist[i][j][k]);
            }
   }
    
   // at this point, arr_dist should have the correct distance values
   // for this 3D volume

   return 0;
}



