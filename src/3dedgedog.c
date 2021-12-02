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

   printf(
"\n"
"Overview ~1~ \n"
"\n"
"This program calculates edges in an image using the Difference of Gaussians\n"
"(DOG) method by Marr and Hildreth (1980) and Wilson and Giese (1977). Edges\n"
"\n"
"\n"
"\n"
"\n"
"\n"
"written by: %s\n"
"\n"
"Description ~2~ \n"
"\n"
"This code calculates\n"
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
"                    by greater than zero (def: 2). Default is chosen to\n"
"                    approximate typical GM thickness in human adults.\n"
"\n"
"  -sigma_nvox NNN  :define radius for 'inner' Gaussian by providing a\n"
"                    multiplicative factor for voxel edge length, which will\n"
"                    be applied in each direction; NNN can be any float\n"
"                    greater than zero.  This is an alternative to the\n"
"                    '-sigma_rad ..' opt (def: use '-sigma_rad' and its\n"
"                    default value).\n"
"\n"
"  -ratio_sig   RS  :the ratio of inner and outer Gaussian sigma values.\n"
"                    That is, RS defines the size of the outer Gaussian,\n"
"                    by scaling up the inner value.  RS can be any float\n"
"                    greater than 1 (def: 1.6). Default chosen because\n"
"                    MH1980 liked this value.\n"
"\n"
"  -output_dog      :use this option flag if you would like to output the\n"
"                    intermediate difference of Gaussian (DOG) dset\n"
"                    (def: not output).  Output will be prefix name with\n"
"                    '_DOG' appended to it.\n"
"\n"
"\n"
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
author );

	return 0;
}

int main(int argc, char *argv[]) {

   int ii = 0;
   int iarg;
   PARAMS_edge_dog InOpts;
   float tmp;

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

      if( strcmp(argv[iarg],"-ratio_sig") == 0) {
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         tmp = atof(argv[iarg]);
         if( tmp <= 1.0 )
            ERROR_exit("Need value >1 after '%s'", argv[iarg-1]);

         InOpts.ratio_sig = tmp;

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-output_dog") == 0 ){
         InOpts.do_output_dog = 1;
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

   // build prefix for DOG dset, if outputting it
   if ( InOpts.do_output_dog )
      ii = build_dog_prefix( &InOpts );
   
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
	THD_3dim_dataset *dset_dog = NULL;         // output
   
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

   nx = DSET_NX(dset_input);
   ny = DSET_NY(dset_input);
   nz = DSET_NZ(dset_input);
   nxy = nx*ny;
   nvox = DSET_NVOX(dset_input);
   nvals = DSET_NVALS(dset_input);

   /* Prepare header for output by copying that of input, and then
      changing items as necessary */
   dset_dog = EDIT_empty_copy( dset_input ); 
   EDIT_dset_items(dset_dog,
                   ADN_nvals, nvals,
                   ADN_datum_all, MRI_float,    
                   ADN_prefix, opts.prefix_dog,
                   ADN_none );

   // calculate DOG
   for( nn=0 ; nn<nvals ; nn++ ){
      i = calc_edge_dog_DOG(dset_dog, opts, dset_input, nn);
   } // end of loop over nvals

   INFO_message("***JUST OUTPUTTING THE DOG MAP AT THE MOMENT***");

   // free input dset
	DSET_delete(dset_input); 
  	free(dset_input); 

   // output the DOG dset?
   if( opts.do_output_dog){
      THD_load_statistics( dset_dog );
      if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(dset_dog)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(dset_dog));
      tross_Make_History("3dedgedog", argc, argv, dset_dog);

      // write and free dset 
      THD_write_3dim_dataset(NULL, NULL, dset_dog, True);
      DSET_delete(dset_dog); 
      free(dset_dog); 
   }

   // free more
   if( dset_mask ){
      DSET_delete(dset_mask);
      free(dset_mask);
   }

   return 0;
}

