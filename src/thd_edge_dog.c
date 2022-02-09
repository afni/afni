#include "mrilib.h" 


PARAMS_edge_dog set_edge_dog_defaults(void)
{

   PARAMS_edge_dog defopt;

   defopt.input_name = NULL;     
   defopt.mask_name  = NULL;     
   defopt.prefix     = NULL;     
   defopt.prefix_dog = NULL;     

   // can do automasking, and have the automask dilated amask_ndil
   // times (with NN=2 dilation)
   defopt.do_automask        = 0;
   defopt.amask_ndil         = 0;

   defopt.do_output_intermed = 0;

   /* 
      units=mm; from typical adult human GM thick.  Will allow this to
      be anisotropic, hence array of 3.

      after some checks, rad=1.4 appears to be a better default than 2
      for human anatomicals; latter misses too many structures,
      wanders off inappropriately.  Things get noisier for smaller
      sigma_rad, but getting more detail seems important
   */
   defopt.sigma_rad[0] = 1.4;
   defopt.sigma_rad[1] = 1.4;  
   defopt.sigma_rad[2] = 1.4;  

   // units=none; alt. to sigma_rad, scale vox dims by this factor to
   // get sigma vals.  Will allow this to be anisotropic, hence array
   // of 3
   defopt.sigma_nvox[0] = 0.0;   
   defopt.sigma_nvox[1] = 0.0;   
   defopt.sigma_nvox[2] = 0.0;   

   // ratio of outer/inner gaussians; from MH1980
   // Note: from testing different values, decreasing this ratio toward
   // 1.1 shows more details---but it gets noisy, too.
   defopt.ratio_sigma = 1.4;

   /*
     EDGE CONTROL PARAMS
     
     --- For the EDT (Euclidean Distance Transform-based) case ---

       edge_bnd_NN: Use NN value to select "corner-ness" of output
       boundary edges. This will be used to threshold the
       voxel-counted EDT maps---this works elegantly because we will
       output distance-squared maps of number of voxels from the EDT:
         + NN=1 -> for face only
         + NN=2 -> for face+edge
         + NN=3 -> for face+edge+node
       NB: We increase them *slightly* for the actual comparisons.

       edge_bnd_side: determine which boundary of the EDT to use for the
       edge.  Encoding is (user enters char string keyword to select):
         + "NEG"  -> -1 -> for negative (inner) boundary
         + "POS"  ->  1 -> for positive (outer) boundary
         + "BOTH" ->  2 -> for both (inner+outer) boundary
         + "BOTH_SIGN"-> 3 -> for both (inner+outer) boundary,
                              with pos/neg sides keeping sign
       Noting that using "NEG"/-1 seems best, by eye (in preliminary tests).
       NB: edge_bnd_side_user is just displayed in the help file---just keep
       it consistent with the internal value.

       edge_bnd_scale: use the values of the gradients to scale the
       edges.
   */
   defopt.edge_bnd_NN = 1;
   defopt.edge_bnd_side = -1;
   defopt.edge_bnd_side_user = "NEG"; 
   defopt.edge_bnd_scale = 0; 

   defopt.verb = 1;

   // same as in PARAMS_euclid_dist
   defopt.only2D = NULL;
   defopt.axes_to_proc[0] = 1;
   defopt.axes_to_proc[1] = 1;
   defopt.axes_to_proc[2] = 1;

   return defopt;
};

// ---------------------------------------------------------------------------

/*
  Build prefixes for supplementary data output, based on user's chosen
  prefix. 

  opts     :struct of default/user input (here, getting inopts->prefix)
  suffix   :that which will be inserted at the end of the prefix_noext 
            from inopts->prefix 

*/
int build_edge_dog_suppl_prefix( PARAMS_edge_dog *opts, char *ostr, 
                                 char *suffix)
{
   char *ext, nullch, tprefix[THD_MAX_PREFIX-strlen(suffix)];

   ENTRY("build_edge_dog_suppl_prefix");

   sprintf(tprefix, "%s", opts->prefix);

   if( has_known_non_afni_extension(opts->prefix) ){
      ext = find_filename_extension(opts->prefix);
      tprefix[strlen(opts->prefix) - strlen(ext)] = '\0';
   }
   else {
      nullch = '\0';
      ext = &nullch;
   }

   sprintf(ostr, "%s%s%s", tprefix, suffix, ext);

   return 0;
}

// ---------------------------------------------------------------------------

/* 
  Use the data in the opts struct to decide how to much to blur in
  each direction.  Blurring sigmas can be anisotropic, but the ratio
  between inner and outer blurring is constant across dimension.

  opts     :struct of default/user opts
  Ledge    :fl arr of len=3 of voxel edge lengths (could be NULL for sigma_nvox)
  rad_in   :fl arr of len=3 of inner Gaussian sigmas (basically an output here)
  rad_out  :fl arr of len=3 of outer Gaussian sigmas (basically an output here)
*/
int calc_edge_dog_sigmas(PARAMS_edge_dog opts, float *Ledge, 
                         float *rad_in, float *rad_out)
{
   int ii;
   float fac;

   ENTRY("calc_edge_dog_sigmas");

   if( opts.sigma_nvox[0] && opts.sigma_nvox[1] && \
       opts.sigma_nvox[2] ){ // user chose to scale voxel edge lengths
      for( ii=0 ; ii<3 ; ii++ )
         rad_in[ii] = opts.sigma_nvox[ii]*Ledge[ii];
   }
   else{ // user chose sigmas with physical mm values
      for( ii=0 ; ii<3 ; ii++ ) 
         rad_in[ii] = opts.sigma_rad[ii];
   }

   // account for '-only2D ..' behavior
   for( ii=0 ; ii<3 ; ii++ )
      if( ! opts.axes_to_proc[ii] )
         rad_in[ii] = 0.0;

   for( ii=0 ; ii<3 ; ii++ )
      rad_out[ii] = rad_in[ii] * opts.ratio_sigma;

   return 0;
}


/* 
   ***** [PT: Feb 6, 2022] now, currently not using this approach ***** 

   this approach is used when we optimized blur calcs with starting
   the outer blur calc from the inner.  however, it does lead to some
   oddities potentially at boundaries of shapes, probably only ever
   noticeable in 'idealized' test cases, rather than out in the wilds
   of real data.

   Outputs here are same as above, except for the rad_out -> diff_rad out:
   diff_rad_out  :fl arr of len=3 of outer Gaussian sigmas, but these are 
            *differential* ones, bc applied to the rad_in-blurred data, for
            computational efficiency (basically an output here)
*/
int calc_edge_dog_sigmas_old(PARAMS_edge_dog opts, float *Ledge, 
                         float *rad_in, float *diff_rad_out)
{
   int ii;
   float fac;

   ENTRY("calc_edge_dog_sigmas_old");

   if( opts.sigma_nvox[0] && opts.sigma_nvox[1] && \
       opts.sigma_nvox[2] ){ // user chose to scale voxel edge lengths
      for( ii=0 ; ii<3 ; ii++ )
         rad_in[ii] = opts.sigma_nvox[ii]*Ledge[ii];
   }
   else{ // user chose sigmas with physical mm values
      for( ii=0 ; ii<3 ; ii++ ) 
         rad_in[ii] = opts.sigma_rad[ii];
   }

   // from formula:   sigma_outer**2 = sigma_inner**2 + fac**2,
   // where sigma_outer = sigma_inner*ratio_sigma.
   fac = sqrt(opts.ratio_sigma * opts.ratio_sigma  - 1.0);

   for( ii=0 ; ii<3 ; ii++ )
      diff_rad_out[ii] = rad_in[ii] * fac;

   return 0;
}

// ---------------------------------------------------------------------------

/*
  Calculate the difference of gaussian (DOG) dataset, which will be
  thresholded to be the edge map.

  dset_dog    :the dset that will be the DOG dataset (essentially, the output)
  opts         :options from the user, with some other quantities calc'ed
  dset_input   :the input dataset of which DOG/edges will be calculated
  ival         :index of subvolume of 'dset_input' to process
  ival         :shared index of subvol of 'dset_input' & 'dset_dog' to process

*/
int calc_edge_dog_DOG( THD_3dim_dataset *dset_dog, PARAMS_edge_dog opts,
                       THD_3dim_dataset *dset_input, int ival)
{
   int ii, idx;
   int nx, ny, nz, nvox;
   float Ledge[3];

   MRI_IMAGE *im_tmp = NULL, *im_inner = NULL, *im_outer = NULL;
   float *fl_im_inner = NULL, *fl_im_outer = NULL;
   float *tmp_arr = NULL;

   float rad_in[3], rad_out[3];

   ENTRY("calc_edge_dog_DOG");

   nx = DSET_NX(dset_input);
   ny = DSET_NY(dset_input);
   nz = DSET_NZ(dset_input);
   nvox = DSET_NVOX(dset_input);
   Ledge[0] = DSET_DX(dset_input);
   Ledge[1] = DSET_DY(dset_input);
   Ledge[2] = DSET_DZ(dset_input);

   // get radii
   ii = calc_edge_dog_sigmas(opts, Ledge, rad_in, rad_out);

   /* copy the subvolume's image (floatizing, if necessary)

      [PT: Feb 6, 2022] No longer doing optimized approach of full
      inner blur, copy that, and then do differential blur on result.
      This way is only minorly slower, and has less boundary quirks
   */
   im_tmp = dset_input->dblk->brick->imarr[ival];
   im_inner = (im_tmp->kind != MRI_float) ? mri_to_float(im_tmp) : \
                                            mri_copy(im_tmp);
   im_outer = mri_copy(im_inner); 

   // apply inner and outer blurring
   fl_im_inner = MRI_FLOAT_PTR(im_inner); 
   EDIT_blur_volume_3d( nx, ny, nz, Ledge[0], Ledge[1], Ledge[2],
                        MRI_float, fl_im_inner,
                        rad_in[0], rad_in[1], rad_in[2] );

   fl_im_outer = MRI_FLOAT_PTR(im_outer);
   EDIT_blur_volume_3d( nx, ny, nz, Ledge[0], Ledge[1], Ledge[2],
                        MRI_float, fl_im_outer,
                        rad_out[0], rad_out[1], rad_out[2] );

   // subtract the outer from the inner at each voxel
   tmp_arr = (float *)calloc(nvox, sizeof(float));
   for ( idx=0 ; idx<nvox ; idx++ )
      tmp_arr[idx] = fl_im_inner[idx] - fl_im_outer[idx];

   // load this array into the dset subvolume
   EDIT_substitute_brick(dset_dog, ival, MRI_float, tmp_arr); 
   tmp_arr = NULL;

   /* possibly write blur images  [9 Feb 2022 rickr] */
   if( opts.do_output_intermed && ival==0 ) {
      THD_3dim_dataset * bset = NULL;
      char               prefix_edt[THD_MAX_PREFIX];

      ii = build_edge_dog_suppl_prefix( &opts, prefix_edt, "_BLURS" );
      INFO_message("Output intermediate dset blur vols");

      bset = EDIT_empty_copy(dset_dog); 
      EDIT_dset_items(bset,
                      ADN_nvals, 2,
                      ADN_datum_all, MRI_float,
                      ADN_prefix, prefix_edt,
                      ADN_none );
      EDIT_substitute_brick(bset, 0, MRI_float, fl_im_inner); 
      EDIT_substitute_brick(bset, 1, MRI_float, fl_im_outer); 
      THD_load_statistics( bset );
      if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(bset)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(bset));
      THD_write_3dim_dataset(NULL, NULL, bset, True);
   }


   // free
   if( im_inner )
      mri_free( im_inner );
   if( im_outer ) 
      mri_free( im_outer );
   // don't free im_tmp: it is just a pointer to dset_input

   return 0;
}

// ---------------------------------------------------------------------------

/*
  Calculate the boundaries from the dog dset.  Might be many ways of
  doing this.

  dset_bnd     :the dset that will be the boundary map (essentially, the 
                output); datum = MRI_short
  opts         :options from the user, with some other quantities calc'ed
  dset_dog     :the input dataset of unthresholded/'raw' DOG values;
                datum = MRI_float
  ival         :shared index of subvolume of 'dset_bnd' & 'dset_dog' to process
  argc         :(int) for history of intermed EDT output, if made 
                (can be 0, to not add to history)
  argv         :(*char) for history of intermed EDT output, if made 
                (can be NULL, to not add to history)

*/
int calc_edge_dog_BND( THD_3dim_dataset *dset_bnd, PARAMS_edge_dog opts,
                       THD_3dim_dataset *dset_dog, int ival,
                       int argc, char *argv[])
{
   int i, idx;
   int nvox;
   short *tmp_arr = NULL;
   THD_3dim_dataset *dset_edt = NULL;        // datum = MRI_float 
   char prefix_edt[THD_MAX_PREFIX];
   PARAMS_euclid_dist EdgeDogOpts;
   THD_3dim_dataset *dset_roi = NULL;   // dset: subset

   ENTRY("calc_edge_dog_BND");

   nvox = DSET_NVOX(dset_dog);
   tmp_arr = (short *) calloc( nvox, sizeof(short) );
   if( tmp_arr == NULL ) 
      ERROR_exit("MemAlloc failure.\n");

   // ------------------ make ROI dset  --------------------

   // Make ROI map entered into EDT (NB: just a single volume, FYI, if
   // outputting)
   dset_roi = EDIT_empty_copy( dset_bnd ); 
   i = build_edge_dog_suppl_prefix( &opts, prefix_edt, "_ROI" );
   EDIT_dset_items(dset_roi,
                   ADN_nvals, 1,
                   ADN_datum_all, MRI_short,
                   ADN_prefix, prefix_edt,
                   ADN_none );

   // make ROI=1 where DOG>=0, and background/ROI=0 elsewhere
   for( idx=0 ; idx<nvox ; idx++ )
      tmp_arr[idx] = (THD_get_voxel(dset_dog, idx, ival) >= 0.0 ) ? 1 : 0;

   EDIT_substitute_brick(dset_roi, 0, MRI_short, tmp_arr); 
   tmp_arr=NULL;

   // ------------------ run EDT calc --------------------

   // Make empty EDT dset (NB: just a single volume, FYI, if outputting)
   dset_edt = EDIT_empty_copy( dset_bnd ); 
   i = build_edge_dog_suppl_prefix( &opts, prefix_edt, "_EDT2" );
   EDIT_dset_items(dset_edt,
                   ADN_nvals, 1,
                   ADN_datum_all, MRI_float,    
                   ADN_prefix, prefix_edt,
                   ADN_none );

   // fill EDT option struct with defaults and a couple desired props
   EdgeDogOpts = set_euclid_dist_defaults();
   EdgeDogOpts.binary_only = 1;      // for faster runtime
   EdgeDogOpts.ignore_voxdims = 1;
   EdgeDogOpts.zero_region_sign = -1;  
   EdgeDogOpts.dist_sq = 1;          // so NN values are directly thresholds
   EdgeDogOpts.verb = 0;
   if( opts.only2D ){
      EdgeDogOpts.only2D = strdup(opts.only2D);
      for( i=0 ; i<3 ; i++ )
         EdgeDogOpts.axes_to_proc[i] = opts.axes_to_proc[i];
   }


   // -------------------------- run EDT -----------------------------------
   INFO_message("Calculate EDT (depth map) for vol %d", ival);

   i = calc_EDT_3D_BIN( dset_edt, EdgeDogOpts, dset_roi, NULL, 0);

   // (opt) output this intermediate dset for the [0]th volume
   if( opts.do_output_intermed && ival==0 ) {
      INFO_message("Output intermediate dset ([0]th vol): %s", prefix_edt);

      THD_load_statistics( dset_edt );
      if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(dset_edt)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(dset_edt));
      tross_Make_History("3dedgedog", argc, argv, dset_edt);

      // write dset 
      THD_write_3dim_dataset(NULL, NULL, dset_edt, True);
   }

   // threshold EDT to make boundaries; dset_edt has only one volume
   // here (to save memory), but dset_bnd can be 4D---hence two indices
   // [PT: Feb 1, 2022] bug fix: had the two indices swapped in this func
   i = calc_edge_dog_thr_EDT( dset_bnd, opts, dset_edt, ival, 0);

   // free dset
	DSET_delete(dset_edt); 
  	free(dset_edt); 
   DSET_delete( dset_roi ); 
  	free(dset_roi); 

   return 0;
}

// ----------------------------------------

/* 
   Same inputs as calc_edge_dog_BND(), except dset_input is the actual
   ival-th volume of the dset_input

   Get 2- and 98-%ile values of DOG values across edge voxels,
   and use DOG values scaled within this range for output
   coloration.
   
   Here, we basically follow what happens in 3dLocalstat.c.
*/ 
int scale_edge_dog_BND( THD_3dim_dataset *dset_bnd, PARAMS_edge_dog opts,
                        THD_3dim_dataset *dset_input_ival, int ival)
{
   void *tmp_vec = NULL;
   byte *mmm = NULL;  // to be byte mask where edges are
   int nvox = 0;
   int ninmask = 0;
   
   int N_mp = 2;                     // number of percentiles to calc
   double mpv[2] = {0.0, 0.0};       // the percentile values to calc
   double perc[2] = {0.0, 0.0};      // will hold the percentile estimates
   int zero_flag = 0, pos_flag = 1, neg_flag = 1; // %ile in nonzero
   
   int i;
   float bot, top, ran, val;
   float *flim = NULL;
   short *shim=NULL;
   short *tmp_arr = NULL;

   // stolen from 3dLocalstat.c
   MCW_cluster *nbhd=NULL;
   int ncode=0, code[MAX_NCODE];
   float codeparams[MAX_NCODE][MAX_CODE_PARAMS+1];
   float redx[3]={0.0, 0.0, 0.0}, mxvx=0.0;           // won't change
   THD_3dim_dataset *outset=NULL;                     // only single vol

   ENTRY("scale_edge_dog_BND");

   nvox = DSET_NVOX(dset_bnd);
   tmp_arr = (short *) calloc( nvox, sizeof(short) );
   if( tmp_arr == NULL ) 
      ERROR_exit("MemAlloc failure.\n");

   mmm = THD_makemask( dset_bnd, ival, 0.0, -1.0 );
   if ( !mmm ) {
      ERROR_message("Failed to general %ile mask.");
      exit(1);         
   }
   ninmask = THD_countmask(nvox, mmm);

   // neighborhood: a 5x5x5 voxel box centered at the coord
   nbhd = MCW_rectmask( 2.0f, 1.0f, 1.0f, 2, 2, 2);
   /* initialize codeparams */
   for (i=0; i<MAX_NCODE; ++i) codeparams[i][0] = -1.0;
   // we just want local stdev
   code[ncode++] = NSTAT_SIGMA; // NSTAT_CVAR; // NSTAT_SIGMA;

   // From 3dLocalstat; has datum = MRI_float; only single vol here
   outset = THD_localstat(dset_input_ival, mmm, nbhd, ncode, code, 
                          codeparams, redx, -1);
   if( outset == NULL ) 
      ERROR_exit("Function THD_localstat() fails via 3dedgedog?!") ;

   flim = MRI_FLOAT_PTR(outset->dblk->brick->imarr[0]);

   // The percentile ranges don't depend on which kind of boundaries we have
   mpv[0] = 0.02;
   mpv[1] = 0.9;

   // outset is always just a single vol
   tmp_vec = Percentate( DSET_ARRAY(outset, 0), mmm, nvox,
                         DSET_BRICK_TYPE(outset, 0), mpv, N_mp,
                         1, perc,
                         zero_flag, pos_flag, neg_flag );
   if ( !tmp_vec ) 
      ERROR_exit("Failed to compute percentiles.");
      
   // Decide on boundary values.  Nothing can have zero EDT here, so
   // don't need to worry about doubling up on that. 
   bot = (float) perc[0];
   top = (float) perc[1];
   ran = top - bot;
   
   if( opts.edge_bnd_side == 3)
      shim = MRI_SHORT_PTR(dset_bnd->dblk->brick->imarr[ival]);
   
   for( i=0 ; i<nvox ; i++ )
      if( mmm[i] ){
         val = (flim[i] - bot)/ran;
         val = ( val > 0.0 ) ? val : 0;
         tmp_arr[i] = (val >= 1.0 ) ? 100 : 99*val+1;

         if( opts.edge_bnd_side == 3 )
            if( shim[i] < 0 )
               tmp_arr[i]*= -1;
      }


   EDIT_substitute_brick(dset_bnd, ival, MRI_short, tmp_arr); 
   tmp_arr=NULL;

   if( tmp_vec ){
      free(tmp_vec); 
      tmp_vec = NULL;
   }
   if( mmm )
      free(mmm);
   
   if(outset){
      DSET_delete(outset);
      free(outset);
   }

   flim = NULL;

   if( shim )
      shim = NULL;

   return 0;
};

/* 
   Same inputs as calc_edge_dog_BND().

   Get 2- and 98-%ile values of DOG values across edge voxels,
   and use DOG values scaled within this range for output
   coloration.
   
   Here, we basically follow what happens in 3dBrickStat.c.
*/ 
int scale_edge_dog_BND_old( THD_3dim_dataset *dset_bnd, PARAMS_edge_dog opts,
                        THD_3dim_dataset *dset_dog, int ival)
{
   void *tmp_vec = NULL;
   byte *mmm = NULL;  // to be byte mask where edges are
   int nvox = 0;
   int ninmask = 0;
   
   int N_mp = 2;                     // number of percentiles to calc
   double mpv[2] = {0.0, 0.0};       // the percentile values to calc
   double perc[2] = {0.0, 0.0};      // will hold the percentile estimates
   int zero_flag = 0, pos_flag = 1, neg_flag = 1; // %ile in nonzero
   
   int i;
   float bot, top, ran, val;
   float *flim = NULL;
   short *tmp_arr = NULL;

   ENTRY("scale_edge_dog_BND");

   nvox = DSET_NVOX(dset_bnd);
   tmp_arr = (short *) calloc( nvox, sizeof(short) );
   if( tmp_arr == NULL ) 
      ERROR_exit("MemAlloc failure.\n");

   mmm = THD_makemask( dset_bnd, ival, 0.0, -1.0 );
   if ( !mmm ) {
      ERROR_message("Failed to general %ile mask.");
      exit(1);         
   }
   ninmask = THD_countmask(nvox, mmm);

   // The percentile ranges depend on which kind of boundaries we have
   if( opts.edge_bnd_side == 1 ){
      mpv[0] = 0.02;
      mpv[1] = 0.50;
   }
   else if( opts.edge_bnd_side == -1 ){
      mpv[0] = 0.98;
      mpv[1] = 0.50;
   }
   else if( opts.edge_bnd_side == 2 || opts.edge_bnd_side == 3 ){
      mpv[0] = 0.25;
      mpv[1] = 0.75;
   }

   tmp_vec = Percentate( DSET_ARRAY(dset_dog, ival), mmm, nvox,
                         DSET_BRICK_TYPE(dset_dog, ival), mpv, N_mp,
                         1, perc,
                         zero_flag, pos_flag, neg_flag );
   if ( !tmp_vec ) {
      ERROR_message("Failed to compute percentiles.");
      exit(1);         
   }
   
   //INFO_message("RANGE: %.6f %.6f", perc[0], perc[1]);

   flim = MRI_FLOAT_PTR(dset_dog->dblk->brick->imarr[ival]);
   
   // Decide on boundary values.  Nothing can have zero EDT here, so
   // don't need to worry about doubling up on that. 
   if( opts.edge_bnd_side == 1 || opts.edge_bnd_side == -1) { 
      bot = (float) perc[0];
      top = (float) perc[1];
      ran = top - bot;

      for( i=0 ; i<nvox ; i++ )
         if( mmm[i] ){
            val = (flim[i] - bot)/ran;
            val = ( val > 0.0 ) ? val : 0.0;
            tmp_arr[i] = (val >= 1.0 ) ? 100 : 99*val+1;
         }
   }
   else if( opts.edge_bnd_side == 2 || opts.edge_bnd_side == 3) {
      bot = (float) perc[0];
      top = (float) perc[1];

      for( i=0 ; i<nvox ; i++ )
         if( mmm[i] ){
            if( flim[i] >= 0 )
               val = flim[i]/top;
            else
               val = flim[i]/bot;
            tmp_arr[i] = (val >= 1.0 ) ? 100 : 99*val+1;

            if( flim[i] < 0 && opts.edge_bnd_side == 3 )
               tmp_arr[i]*= -1;
         }
   }

   EDIT_substitute_brick(dset_bnd, ival, MRI_short, tmp_arr); 
   tmp_arr=NULL;

   if( tmp_vec ){
      free(tmp_vec); 
      tmp_vec = NULL;
   }
   if( mmm )
      free(mmm);

   flim = NULL;

   return 0;
};

// ---------------------------------------------------------------------------

/*
  Threshold the boundaries of the EDT dset.  Might be many ways of
  doing this.

  dset_bnd     :the dset that will be the edge dataset (essentially, the 
                output); datum = MRI_short
  opts         :options from the user, with some other quantities calc'ed
  dset_edt     :the input dataset EDT values, to be thresholded for edges;
                datum = MRI_float
  ival_bnd     :index of subvolume of 'dset_bnd' to process
  ival_edt     :index of subvolume of 'dset_edt' to process

*/
int calc_edge_dog_thr_EDT( THD_3dim_dataset *dset_bnd, PARAMS_edge_dog opts,
                           THD_3dim_dataset *dset_edt, int ival_bnd, 
                           int ival_edt)
{
   int i, idx;
   int nvox;
   float bot=0, top=-1, val;  
   short *tmp_arr = NULL;

   ENTRY("calc_edge_dog_thr_EDT");

   nvox = DSET_NVOX(dset_edt);
   tmp_arr = (short *) calloc( nvox, sizeof(short) );
   if( tmp_arr == NULL ) 
      ERROR_exit("MemAlloc failure.\n");

   // Decide on boundary values.  Nothing can have zero EDT here, so
   // don't need to worry about doubling up on that.
   if( opts.edge_bnd_side == 1 ) {
      bot = 0.0;
      top = opts.edge_bnd_NN * 1.01;
   }
   else if( opts.edge_bnd_side == -1 ) {
      bot = -opts.edge_bnd_NN * 1.01;
      top = 0;
   }
   else if( opts.edge_bnd_side == 2 || opts.edge_bnd_side == 3 ) {
      bot = -opts.edge_bnd_NN * 1.01;
      top = opts.edge_bnd_NN * 1.01;
   }
   
   // Go through EDT values, pick out what becomes a boundary
   for ( idx=0 ; idx<nvox ; idx++ ) {
      val = THD_get_voxel(dset_edt, idx, ival_edt);
      if( bot <= val && val <= top ) {
         tmp_arr[idx] = 1;
         if(  opts.edge_bnd_side == 3 )
            if( val < 0 )
               tmp_arr[idx]*= -1;
      }
   }

   // load this array into the dset subvolume
   EDIT_substitute_brick(dset_bnd, ival_bnd, MRI_short, tmp_arr); 
   tmp_arr = NULL;

   return 0;
}
