/* 


ver = 1.0;  date = Dec 1, 2021
+ [PT] start of this program


*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <float.h>
#include "debugtrace.h"
#include "mrilib.h"
#include "3ddata.h"
#include "thd_edge_dog.c"

int run_edge_dog( int comline, PARAMS_edge_dog opts,
                  int argc, char *argv[] );

// --------------------------------------------------------------------------

int usage_3dedgedog() 
{
   char *author = "PA Taylor and DR Glen (SSCC, NIMH, NIH)";
   PARAMS_edge_dog opts;

   opts = set_edge_dog_defaults();

   printf(
"\n"
"Overview ~1~ \n"
"\n"
"This program calculates edges in an image using the Difference of Gaussians\n"
"(DOG) method by Wilson and Giese (1977) and later combined with work by\n"
"Marr and Hildreth (1980) to provide a computationally efficient\n"
"approximation to their Lagrangian of Gaussian (LOG) method for calculating\n"
"edges in an image.  This is a fascinating set of papers to read.  But you\n"
"don't have to take *my* word for it!...\n"
"\n"
"Generating edges in this way has some interesting properties, such as\n"
"numerical efficiency and edges that are closed loops/surfaces.  The edges\n"
"can be tuned to focus on structures of a particular size, too, which can be\n"
"particularly useful in some applications.\n"
"\n"
"written by: %s\n"
"\n"
"Description ~2~ \n"
"\n"
"The primary papers for learning more about the DOG and LOG methods are:\n"
"\n"
"   Wilson HR, Giese SC  (1977). Threshold visibility of frequency\n"
"   gradient patterns. Vision Res. 17(10):1177-90.\n"
"   doi: 10.1016/0042-6989(77)90152-3. PMID: 595381.\n"
"\n"
"   Marr D, Hildreth E (1980). Theory of edge detection. Proc R Soc\n"
"   Lond B Biol Sci. 207(1167):187-217.\n"
"   doi: 10.1098/rspb.1980.0020. PMID: 6102765.\n"
"\n"
"Thanks to C. Rorden for pointing these papers out and discussing them.\n"
"\n"
"The current code here extends/tweaks the MH1980 algorithm a bit.  It runs\n"
"in 3D by default (a straightforward extension), it also employs the\n"
"Eulerian Distance Transform (EDT) to pick out the actual edges from the\n"
"DOG step---see 3dEulerDist for more information about the EDT.\n"
"\n"
"The DOG-based edges require specifying a couple parameters, the main\n"
"one being interpretable as a minimal 'scale size' for structures.  In this\n"
"code, this is the 'sigma_rad' (or 'sigma_nvox', if you want to specify it\n"
"in terms of the number of voxels along a given axis), which is the 'inner\n"
"Gaussian' sigma value, if you are following MH1980.  The default for this\n"
"sigma_rad parameter is set based on the expected average thickness of adult\n"
"human GM, but it is easily alterable at the command line for any other\n"
"values.\n"
"\n"
"==========================================================================\n"
"\n"
"Command usage and option list ~1~ \n"
"\n"
"    3dedgedog [options] -prefix PREF -input DSET\n"
"\n"
"where: \n"
"\n"
"  -input DSET      :(req) input dataset\n"
"\n"
"  -prefix PREF     :(req) output prefix name\n"
"\n"
"  -mask  MASK      :mask dataset.  NB: this mask is only applied *after*\n"
"                    the EDT has been calculated.  Therefore, the boundaries\n"
"                    of this mask have no affect on the calculated distance\n"
"                    values, except for potentially zeroing some out at the\n"
"                    end.\n"
"\n"
"  -sigma_rad RRR   :radius for 'inner' Gaussian, in units of mm; RRR must\n"
"                    by greater than zero (def: %.2f). Default is chosen to\n"
"                    approximate typical GM thickness in human adults.\n"
"\n"
"  -sigma_nvox NNN  :define radius for 'inner' Gaussian by providing a\n"
"                    multiplicative factor for voxel edge length, which will\n"
"                    be applied in each direction; NNN can be any float\n"
"                    greater than zero.  This is an alternative to the\n"
"                    '-sigma_rad ..' opt (def: use '-sigma_rad' and its\n"
"                    default value).\n"
"\n"
"  -ratio_sigma RS  :the ratio of inner and outer Gaussian sigma values.\n"
"                    That is, RS defines the size of the outer Gaussian,\n"
"                    by scaling up the inner value.  RS can be any float\n"
"                    greater than 1 (def: %.2f). Default chosen because\n"
"                    MH1980 liked this value.\n"
"\n"
"  -output_intermed :use this option flag if you would like to output some\n"
"                    intermediate dataset(s):\n"
"                         + DOG (difference of Gaussian)\n"
"                         + EDT2 (Euler Distance Transform, dist**2 values),\n"
"                           [0]th vol only\n"
"                    (def: not output).  Output names  will be user-entered\n"
"                    prefix with a representative suffix appended.\n"
"\n"
"  -edge_bnd_NN EBN :specify the 'nearest neighbor' (NN) value for the\n"
"                    connectedness of the drawn boundaries.  EBN must be\n"
"                    one of the following integer values:\n"
"                        1 -> for face only\n"
"                        2 -> for face+edge\n"
"                        3 -> for face+edge+node\n"
"                    (def: %d).\n"
"\n"
"\n"
"  -edge_bnd_sign EBS :specify which boundary layer around the zero-layer\n"
"                    to use in the algorithm.  EBS must be one of the\n"
"                    following integer values:\n"
"                        1 -> for positive (outer) boundary\n"
"                       -1 -> for negative (inner) boundary\n"
"                        0 -> for both (inner+outer) boundary\n"
"                    (def: %d).\n"
"\n"
"==========================================================================\n"
"\n"
"Examples ~1~\n"
"\n"
"1) Basic case:\n"
"   3dedgedog                                                       \\\n"
"       -input  roi_map.nii.gz                                      \\\n"
"       -prefix roi_map_EDT.nii.gz                                  \n"
"\n"
"2) ***:\n"
"   3dedgedog                                                       \\\n"
"       *******                                                     \\\n"
"       -input  roi_map.nii.gz                                      \\\n"
"       -prefix roi_map_EDT_ZZ.nii.gz                               \n"
"\n"
"==========================================================================\n"
"\n",
author, opts.sigma_rad[0], opts.ratio_sigma, opts.edge_bnd_NN, 
opts.edge_bnd_sign );

	return 0;
}

int main(int argc, char *argv[]) {

   int ii = 0;
   int iarg;
   PARAMS_edge_dog InOpts;
   float tmp;
   int itmp;

   mainENTRY("3dedgedog"); machdep(); 
  
   // fill option struct with defaults
   InOpts = set_edge_dog_defaults();

   // ****************************************************************
   //                  parse command line arguments
   // ****************************************************************
	
   if (argc == 1) { usage_3dedgedog(); exit(0); }

   /* scan through args */
   iarg = 1; 
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_3dedgedog();
         exit(0);
      }
			 
      if( strcmp(argv[iarg],"-input") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         InOpts.input_name = strdup(argv[iarg]) ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-mask") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         InOpts.mask_name = strdup(argv[iarg]) ;			
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         InOpts.prefix = strdup(argv[iarg]) ;

         if( !THD_filename_ok(InOpts.prefix) ) 
            ERROR_exit("Illegal name after '%s'", argv[iarg-1]);
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-sigma_rad") == 0) {
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         tmp = atof(argv[iarg]);
         if( tmp <= 0 )
            ERROR_exit("Need positive value after '%s'", argv[iarg-1]);

         InOpts.sigma_rad[0] = tmp;
         InOpts.sigma_rad[1] = tmp;
         InOpts.sigma_rad[2] = tmp;

         iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-sigma_nvox") == 0) {
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         tmp = atof(argv[iarg]);
         if( tmp <= 0 )
            ERROR_exit("Need positive value after '%s'", argv[iarg-1]);
         
         InOpts.sigma_nvox[0] = tmp;
         InOpts.sigma_nvox[1] = tmp;
         InOpts.sigma_nvox[2] = tmp;

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-edge_bnd_NN") == 0) {
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         itmp = atoi(argv[iarg]);
         if( 1 <= itmp && itmp <=3 )
            InOpts.edge_bnd_NN = itmp;
         else
            ERROR_exit("Need either 1, 2 or 3 after '%s'", argv[iarg-1]);

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-edge_bnd_sign") == 0) {
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         itmp = atoi(argv[iarg]);
         if( -1 <= itmp && itmp <= 1 )
            InOpts.edge_bnd_sign = itmp;
         else
            ERROR_exit("Need either -1, 0 or 1 after '%s'", argv[iarg-1]);

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-ratio_sigma") == 0) {
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         tmp = atof(argv[iarg]);
         if( tmp <= 1.0 )
            ERROR_exit("Need value >1 after '%s'", argv[iarg-1]);

         InOpts.ratio_sigma = tmp;

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-output_intermed") == 0 ){
         InOpts.do_output_intermed = 1;
         iarg++ ; continue ;
      }

      ERROR_message("Bad option '%s'\n",argv[iarg]);
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);
   }
	
   // ****************************************************************
   //               verify presence+behavior of inputs
   // ****************************************************************

   INFO_message("3dedgedog: verify inputs");

   if ( !InOpts.input_name ) { 
      ERROR_message("You need to provide an input dset with '-input ..'");
      exit(1);
   }

   if ( !InOpts.prefix )
      ERROR_exit("Need an output name via '-prefix ..'\n");

   // DONE FILLING, now do the work
   ii = run_edge_dog(1, InOpts, argc, argv);

   return 0;
}


int run_edge_dog( int comline, PARAMS_edge_dog opts,
                int argc, char *argv[] )
{
   int i, j, k, idx;
   int nn;
   int nx, ny, nz, nxy, nvox, nvals;
	THD_3dim_dataset *dset_input = NULL;        // input
   THD_3dim_dataset *dset_mask = NULL;         // mask
	THD_3dim_dataset *dset_dog = NULL;         // intermed/out
   THD_3dim_dataset *dset_bnd = NULL;         // output
   char prefix_dog[THD_MAX_PREFIX];

   ENTRY("run_edge_dog");

   dset_input = THD_open_dataset(opts.input_name);
   if( (dset_input == NULL ))
      ERROR_exit("Can't open dataset '%s'", opts.input_name);
   DSET_load(dset_input); CHECK_LOAD_ERROR(dset_input);

   if( opts.mask_name ) {
      dset_mask = THD_open_dataset(opts.mask_name);
      if( dset_mask == NULL )
         ERROR_exit("Can't open dataset '%s'", opts.mask_name);
      DSET_load(dset_mask); CHECK_LOAD_ERROR(dset_mask);

      if( THD_dataset_mismatch( dset_input , dset_mask ) )
         ERROR_exit("Mismatch between input and mask dsets!\n");
   }

   // NTS: do we need all these quantities?
   nx = DSET_NX(dset_input);
   ny = DSET_NY(dset_input);
   nz = DSET_NZ(dset_input);
   nxy = nx*ny;
   nvox = DSET_NVOX(dset_input);
   nvals = DSET_NVALS(dset_input);

   /* Prepare header for output by copying that of input, and then
      changing items as necessary */
   dset_dog = EDIT_empty_copy( dset_input ); 
   i = build_edge_dog_suppl_prefix( &opts, prefix_dog, "_DOG" );
   EDIT_dset_items(dset_dog,
                   ADN_nvals, nvals,
                   ADN_datum_all, MRI_float,    
                   ADN_prefix, prefix_dog,
                   ADN_none );

   // calculate DOG
   INFO_message("Calculate DOG");
   for( nn=0 ; nn<nvals ; nn++ )
      i = calc_edge_dog_DOG(dset_dog, opts, dset_input, nn);
   
   // Output DOG data, if asked
   if ( opts.do_output_intermed ){
      INFO_message("Output intermediate dset: %s", prefix_dog);

      THD_load_statistics( dset_dog );
      if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(dset_dog)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(dset_dog));
      tross_Make_History("3dedgedog", argc, argv, dset_dog);

      // write and free dset 
      THD_write_3dim_dataset(NULL, NULL, dset_dog, True);
   }


   // ------------------------ calc edge/bnd ---------------------------

   dset_bnd = EDIT_empty_copy( dset_input ); 
   EDIT_dset_items(dset_bnd,
                   ADN_nvals, nvals,
                   ADN_datum_all, MRI_short,    
                   ADN_prefix, opts.prefix,
                   ADN_none );

   INFO_message("Calculate boundaries");

   // might be several ways to calc edges/bnds from DOG data
   for( nn=0 ; nn<nvals ; nn++ )
      i = calc_edge_dog_BND(dset_bnd, opts, dset_dog, nn);

   INFO_message("Output main dset: %s", opts.prefix);

   THD_load_statistics( dset_bnd );
   if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(dset_bnd)) )
      ERROR_exit("Can't overwrite existing dataset '%s'",
                 DSET_HEADNAME(dset_bnd));
   tross_Make_History("3dedgedog", 0, NULL, dset_bnd);
   
   // write edge/bnd
   THD_write_3dim_dataset(NULL, NULL, dset_bnd, True);


   // free dsets
   if( dset_input ){
      DSET_delete(dset_input); 
      free(dset_input); 
   }

   if( dset_dog ){
      DSET_delete(dset_dog); 
      free(dset_dog); 
   }

   if( dset_bnd ){
      DSET_delete(dset_bnd); 
      free(dset_bnd); 
   }

   if( dset_mask ){
      DSET_delete(dset_mask);
      free(dset_mask);
   }

   // free more


   return 0;
}

