/* 

ver = 1.0;  date = Dec 1, 2021
+ [PT] start of this program

ver = 1.2;  date = Dec 3, 2021
+ [PT] getting there, for basic functionality.  Defaults work pretty well.
     
ver = 1.3;  date = Dec 6, 2021
+ [PT] start to add scaling of edge values...

ver = 1.31;  date = Dec 6, 2021
+ [PT] ... and scaling now activated: using DOG value at each edge vox

ver = 1.4;  date = Dec 9, 2021
+ [PT] add -only2D opt, for DRG

ver = 1.5;  date = Dec 23, 2021
+ [PT] run with binarized/optimized Euclidean Dist Transform. Shd be faster

ver = 1.51;  date = Dec 26, 2021
+ [PT] all output dsets get their history full made

ver = 1.52;  date = Dec 26, 2021
+ [PT] start adding in pieces for masking---have NOT applied yet

ver = 1.6;  date = Dec 26, 2021
+ [PT] add in -automask, automask+X, and '-mask ..' option behavior

ver = 1.7;  date = Dec 29, 2021
+ [PT] change default rad : 2.0 seems much to big to catch details
  even in a human anatomical dset.  Going for 1.4 now

ver = 1.8;  date = Feb 1, 2022
+ [PT] change the way scaling is done: use the input dset, for better
  color variation (uses 3dLocalstat -> cvar data
+ Also apply masking before scaling, so scales aren't set by non-output info

ver = 1.9;  date = Feb 6, 2022
+ [PT] debugged some issues if multivolume datasets are entered

ver = 2.0;  date = Feb 6, 2022
+ [PT] account for -only2D opt in blur radius
  - Also use 'non-opt' double blur with Gaussians---better for some
    boundary conditions

ver = 2.1;  date = Feb 9, 2022
+ [RCR] output intermediate blur dsets (of [0]th vol)
  - underlying blurring improved at boundary


*** still might add:
  - multi-spatial scale requests

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include "debugtrace.h"
#include "mrilib.h"

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
"Euclidean Distance Transform (EDT) to pick out the actual edges from the\n"
"DOG step---see 3dDepthMap for more information about the EDT.\n"
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
"                    end. Mask only gets made from [0]th vol.\n"
"\n"
"  -automask        :alternative to '-mask ..', for automatic internal\n"
"                    calculation of a mask in the usual AFNI way. Again, this\n"
"                    mask is only applied after all calcs (so using this does\n"
"                    not speed up the calc or affect distance values).\n"
"                    ** Special note: you can also write '-automask+X', where\n"
"                    X is some integer; this will dilate the initial automask\n"
"                    X number of times (as in 3dAllineate); must have X>0.\n"
"\n"
"  -sigma_rad RRR   :radius for 'inner' Gaussian, in units of mm; RRR must\n"
"                    by greater than zero (def: %.2f). Default is chosen to\n"
"                    capture useful features in typical adult, human GM,\n"
"                    which has typical thickness of 2-2.5mm.  So, if you are\n"
"                    analyzing some other kind of data, you might want to\n"
"                    adapt this value appropriately.\n"
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
"                    greater than 1 (def: %.2f). See 'Notes' for more about\n"
"                    this parameter.\n"
"\n"
"  -output_intermed :use this option flag if you would like to output some\n"
"                    intermediate dataset(s):\n"
"                        + DOG (difference of Gaussian)\n"
"                        + EDT2 (Euclidean Distance Transform, dist**2 vals),\n"
"                          [0]th vol only\n"
"                        + BLURS (inner- and outer-Gaussian blurred dsets),\n"
"                          [0]th vol only\n"
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
"  -edge_bnd_side EBS :specify which boundary layer around the zero-layer\n"
"                    to use in the algorithm.  EBS must be one of the\n"
"                    following keywords:\n"
"                       \"NEG\"  -> for negative (inner) boundary\n"
"                       \"POS\"  -> for positive (outer) boundary\n"
"                       \"BOTH\" -> for both (inner+outer) boundary\n"
"                       \"BOTH_SIGN\" -> for both (inner+outer) boundary,\n"
"                                        with pos/neg sides keeping sign\n"
"                    (def: \"%s\").\n"
"\n"
"  -edge_bnd_scale  :by default, this program outputs a mask of edges, so\n"
"                    edge locations have value=1, and everything else is 0.\n"
"                    Using this option means the edges will have values\n"
"                    scaled to have a relative magnitude between 0 and 100\n"
"                    (NB: the output dset will still be datum=short)\n"
"                    depending on the gradient value at the edge.\n"
"                    When using this opt, likely setting the colorbar scale\n"
"                    to 25 will provide nice images (in N=1 cases tested,\n"
"                    at least!).\n"
"\n"
"  -only2D   SLI    :instead of estimating edges in full 3D volume, calculate\n"
"                    edges just in 2D, per plane.  Provide the slice plane\n"
"                    you want to run along as the single argument SLI:\n"
"                       \"axi\"  -> for axial slice\n"
"                       \"cor\"  -> for coronal slice\n"
"                       \"sag\"  -> for sagittal slice\n"
"\n"
"==========================================================================\n"
"\n"
"Notes ~1~\n"
"\n"
"The value of sigma_rad ~2~\n"
"\n"
"(... which sounds like the title of a great story, no? Anyways...)\n"
"This parameter represents the ratio of the width of the two Gaussians that\n"
"are blurred in the first stage of the DOG estimation.  In the limit that\n"
"sigma_rad approaches 1, the DOG -> LOG.  So, we want to keep the value of\n"
"this parameter in the general vicinity of 1 (and it can't be less than 1,\n"
"because the ratio is of the outer-to-the-inner Gaussian).  MH1980 suggested\n"
"that sigma_rad=1.6 was optimal 'on engineering grounds' of bandwidth\n"
"sensitivity of filters.  This is *very approximate* reasoning, but provides\n"
"another reference datum for selection.\n"
"\n"
"Because the DOG approximation used here is for visual purposes of MRI\n"
"datasets, often even more specifically for alignment purposes, we have\n"
"chosen a default value that seemed visually appropriate to real data.\n"
"Values of sigma_rad close to one show much noisier, scattered images---that\n"
"is, they pick up *lots* of contrast differences, probably too many for most\n"
"visualization purposes.  Edge images smoothen as sigma_rad increases, but\n"
"as it gets larger, it can also blend together edges of features---such as\n"
"gyri of the brain with dura.  So, long story short, the default value here\n"
"tries to pick a reasonable middle ground.\n"
"\n"
"==========================================================================\n"
"\n"
"Examples ~1~\n"
"\n"
"1) Basic case:\n"
"   3dedgedog                                                       \\\n"
"       -input   anat+orig.HEAD                                     \\\n"
"       -prefix  anat_EDGE.nii.gz                                   \n"
"\n"
"2) Same as above, but output both edges from the DOG+EDT steps, keeping\n"
"   the sign of each side:\n"
"   3dedgedog                                                       \\\n"
"       -edge_bnd_side  BOTH_SIGN                                   \\\n"
"       -input   anat+orig.HEAD                                     \\\n"
"       -prefix  anat_EDGE_BOTHS.nii.gz                             \n"
"\n"
"3) Output both sides of edges, and scale the edge values (by DOG value):\n"
"   3dedgedog                                                       \\\n"
"       -edge_bnd_side  BOTH_SIGN                                   \\\n"
"       -edge_bnd_scale                                             \\\n"
"       -input   anat+orig.HEAD                                     \\\n"
"       -prefix  anat_EDGE_BOTHS_SCALE.nii.gz                       \n"
"\n"
"4) Increase scale size of edged shapes to 2.7mm:\n"
"   3dedgedog                                                       \\\n"
"       -sigma_rad 2.7                                              \\\n"
"       -edge_bnd_scale                                             \\\n"
"       -input   anat+orig.HEAD                                     \\\n"
"       -prefix  anat_EDGE_BOTHS_SCALE.nii.gz                       \n"
"\n"
"5) Apply automasking, with a bit of mask dilation so outer boundary is\n"
"   included:\n"
"   3dedgedog                                                       \\\n"
"       -automask+2                                                 \\\n"
"       -input   anat+orig.HEAD                                     \\\n"
"       -prefix  anat_EDGE_AMASK.nii.gz                             \n"
"\n"
"==========================================================================\n"
"\n",
author, opts.sigma_rad[0], opts.ratio_sigma, opts.edge_bnd_NN, 
opts.edge_bnd_side_user );

	return 0;
}

int main(int argc, char *argv[]) {

   int ii = 0;
   int iarg;
   PARAMS_edge_dog InOpts;
   float tmp;
   int itmp;
   char *auto_tstring = NULL;

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

      if( strncmp(argv[iarg],"-automask", 9) == 0 ) {
         InOpts.do_automask = 1;

         auto_tstring = argv[iarg];
         if ( auto_tstring[9] == '+' && auto_tstring[10] != '\0' ) {
            InOpts.amask_ndil = (int) strtod(auto_tstring+10, NULL);
            if ( InOpts.amask_ndil <= 0 )
               ERROR_exit("Cannot have number of dilations (%d) <=0.",
                          InOpts.amask_ndil);
         }

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

      if( strcmp(argv[iarg],"-edge_bnd_side") == 0) {
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         if( strcmp(argv[iarg],"NEG") == 0)
            InOpts.edge_bnd_side = -1;
         else if( strcmp(argv[iarg],"POS") == 0)
            InOpts.edge_bnd_side = 1;
         else if( strcmp(argv[iarg],"BOTH") == 0)
            InOpts.edge_bnd_side = 2;
         else if( strcmp(argv[iarg],"BOTH_SIGN") == 0)
            InOpts.edge_bnd_side = 3;
         else
            ERROR_exit("Need either \"NEG\", \"POS\" or \"BOTH\" "
                       "after '%s'", argv[iarg-1]);

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-edge_bnd_scale") == 0 ){
         InOpts.edge_bnd_scale = 1;
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

      // same as in 3dDepthMap
      if( strcmp(argv[iarg],"-only2D") == 0) {
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '%s'", argv[iarg-1]);

         if( strcmp(argv[iarg],"cor") == 0 || \
             strcmp(argv[iarg],"axi") == 0 || \
             strcmp(argv[iarg],"sag") == 0 )
            InOpts.only2D = strdup(argv[iarg]);
         else
            ERROR_exit("Need either \"cor\", \"axi\" or \"sag\" "
                       "after '%s'", argv[iarg-1]);

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

   if ( InOpts.mask_name && InOpts.do_automask )
      ERROR_exit("Cannot combine '-mask ..' and '-automask'.  "
                 "You must choose which you *really* want.\n");
   
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
	THD_3dim_dataset *dset_dog = NULL;          // MRI_float; intermed/out
   THD_3dim_dataset *dset_bnd = NULL;          // MRI_short; output

   THD_3dim_dataset *dset_input_ival = NULL;   // input: subset

   char prefix_dog[THD_MAX_PREFIX];

   byte *mask_arr = NULL;                      // what mask becomes
   int nmask = -1;

   ENTRY("run_edge_dog");

   dset_input = THD_open_dataset(opts.input_name);
   if( dset_input == NULL )
      ERROR_exit("Can't open dataset '%s'", opts.input_name);
   DSET_load(dset_input); CHECK_LOAD_ERROR(dset_input);

   nx = DSET_NX(dset_input);
   ny = DSET_NY(dset_input);
   nz = DSET_NZ(dset_input);
   nxy = nx*ny;
   nvox = DSET_NVOX(dset_input);
   nvals = DSET_NVALS(dset_input);

   // masking, if user asks
   if( opts.mask_name ) {
      // read in+check mask from user
      dset_mask = THD_open_dataset(opts.mask_name);
      if( dset_mask == NULL )
         ERROR_exit("Can't open dataset '%s'", opts.mask_name);
      DSET_load(dset_mask); CHECK_LOAD_ERROR(dset_mask);

      if( THD_dataset_mismatch( dset_input, dset_mask ) )
         ERROR_exit("Mismatch between input and mask dsets!\n");

      // mask comes from just [0]th vol
      mask_arr = THD_makemask( dset_mask, 0, 0, -1 );
		nmask    = THD_countmask( nvox, mask_arr );
		if( opts.verb )
         INFO_message("Number of voxels in automask = %d / %d", nmask, nvox);
		if( nmask < 1 )
         ERROR_exit("Automask is too small to process");

      DSET_delete(dset_mask);
      if(dset_mask)
         free(dset_mask);
   }
   else if( opts.do_automask ) {
      // internal mask generation (automasking)
		mask_arr = THD_automask( dset_input );
		if( mask_arr == NULL )
			ERROR_exit("Can't create -automask from input dataset?");

      if( opts.amask_ndil ){
         if( opts.verb )
            INFO_message("Inflate automask %d times", opts.amask_ndil);

         // a very odd recipe for inflating a mask... mirroring
         // default 3dAllineate behavior (slightly different than that
         // of 3dAutomask; commented out)
         for( i=0 ; i < opts.amask_ndil ; i++ ){
            j = THD_mask_dilate( nx, ny, nz, mask_arr, 3, 2 );
            j = THD_mask_fillin_once( nx, ny, nz, mask_arr, 2 );
         }
      }
      
		nmask = THD_countmask( nvox, mask_arr );
		if( opts.verb )
         INFO_message("Number of voxels in automask = %d / %d", nmask, nvox);
		if( nmask < 1 )
         ERROR_exit("Automask is too small to process");
   }

   // if only running in 2D, figure out which slice that is;
   // same as in 3dDepthMap
   if ( opts.only2D )
      i = choose_axes_for_plane( dset_input, opts.only2D,
                                 opts.axes_to_proc, opts.verb );

   /* Prepare header for output by copying that of input, and then
      changing items as necessary */
   dset_dog = EDIT_empty_copy( dset_input ); 
   i = build_edge_dog_suppl_prefix( &opts, prefix_dog, "_DOG" );
   EDIT_dset_items(dset_dog,
                   ADN_nvals, nvals,
                   ADN_datum_all, MRI_float,    
                   ADN_prefix, prefix_dog,
                   ADN_none );

   // ------------------------ calc DOG ---------------------------
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

      // write dset 
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

   // calculate edges/bnds: might be several ways to these from DOG data
   for( nn=0 ; nn<nvals ; nn++ )
      i = calc_edge_dog_BND(dset_bnd, opts, dset_dog, nn, argc, argv);

   // mask edges, if masking
   if ( mask_arr ) {
      if( opts.verb )
         INFO_message("Apply mask");
      THD_3dim_dataset *dset_tmp = NULL;
      
      dset_tmp = thd_apply_mask( dset_bnd, mask_arr, 
                                 opts.prefix );
      DSET_delete( dset_bnd ); 
      dset_bnd = dset_tmp;
      dset_tmp = NULL;
   }

   // scale edges/bnds (opt): may be several ways to these from DOG data
   if( opts.edge_bnd_scale ){
      INFO_message("Scale boundaries");
      for( nn=0 ; nn<nvals ; nn++ )
         i = scale_edge_dog_BND(dset_bnd, opts, dset_input, nn);
   }

   INFO_message("Output main dset: %s", opts.prefix);

   THD_load_statistics( dset_bnd );
   if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(dset_bnd)) )
      ERROR_exit("Can't overwrite existing dataset '%s'",
                 DSET_HEADNAME(dset_bnd));
   tross_Make_History("3dedgedog", argc, argv, dset_bnd);
   
   // write edge/bnd
   THD_write_3dim_dataset(NULL, NULL, dset_bnd, True);

   // ------ write out automask, if used and intermediates are output ------
   if( opts.do_output_intermed && opts.do_automask ){
      char prefix_amask[THD_MAX_PREFIX];
      THD_3dim_dataset *dset_amask = NULL; 
         
      dset_amask = EDIT_empty_copy( dset_input ); 
      i = build_edge_dog_suppl_prefix( &opts, prefix_amask, "_AMASK" );

      if( opts.verb )
         INFO_message("Write out automask dset: %s", prefix_amask);

      EDIT_dset_items(dset_amask,
                      ADN_nvals, 1,
                      ADN_datum_all, MRI_byte,    
                      ADN_prefix, prefix_amask,
                      ADN_none );
         
      EDIT_substitute_brick(dset_amask, 0, MRI_byte, mask_arr); 

      THD_load_statistics( dset_amask );
      if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(dset_amask)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(dset_amask));
      tross_Make_History("3dedgedog", argc, argv, dset_amask);

      // write and free dset 
      THD_write_3dim_dataset(NULL, NULL, dset_amask, True);
      DSET_delete(dset_amask);
      free(dset_amask);
      mask_arr = NULL;
   }


   // ------------------------- free stuff ---------------------------

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

   if( mask_arr )
      free(mask_arr);

   return 0;
}

