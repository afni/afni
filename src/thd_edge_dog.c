#include "mrilib.h" 
#include "thd_edge_dog.h"

PARAMS_edge_dog set_edge_dog_defaults(void)
{

   PARAMS_edge_dog defopt;

   defopt.input_name = NULL;     
   defopt.mask_name  = NULL;     
   defopt.prefix     = NULL;     
   sprintf(defopt.prefix_dog, "tmp_dog");

   defopt.do_output_dog = 0;

   // units=mm; from typical adult human GM thick.  Will allow this to
   // be anisotropic, hence array of 3
   defopt.sigma_rad[0] = 2.0;  
   defopt.sigma_rad[1] = 2.0;  
   defopt.sigma_rad[2] = 2.0;  

   // units=none; alt. to sigma_rad, scale vox dims by this factor to
   // get sigma vals.  Will allow this to be anisotropic, hence array
   // of 3
   defopt.sigma_nvox[0] = 0.0;   
   defopt.sigma_nvox[1] = 0.0;   
   defopt.sigma_nvox[2] = 0.0;   

   // ratio of outer/inner gaussians; from MH1980
   defopt.ratio_sig = 1.6;

   return defopt;
};

// ---------------------------------------------------------------------------

int build_dog_prefix( PARAMS_edge_dog *opts)
{
   char *ext, nullch, tprefix[THD_MAX_PREFIX-4];
   
   sprintf(tprefix, "%s", opts->prefix);
   if( has_known_non_afni_extension(opts->prefix) ){
      ext = find_filename_extension(opts->prefix);
      tprefix[strlen(opts->prefix) - strlen(ext)] = '\0';
   }
   else {
      nullch = '\0';
      ext = &nullch;
   }
   
   sprintf(opts->prefix_dog, "%s_DOG%s", tprefix, ext);

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

   ENTRY("calc_edge_dog");

   nx = DSET_NX(dset_input);
   ny = DSET_NY(dset_input);
   nz = DSET_NZ(dset_input);
   nvox = DSET_NVOX(dset_input);
   Ledge[0] = DSET_DX(dset_input);
   Ledge[1] = DSET_DY(dset_input);
   Ledge[2] = DSET_DZ(dset_input);

   // get radii
   ii = calc_edge_dog_sigmas(opts, Ledge, rad_in, rad_out);

   // copy the subvolume's image (floatizing, if necessary)
   im_tmp = dset_input->dblk->brick->imarr[ival];
   im_inner = (im_tmp->kind != MRI_float) ? mri_to_float(im_tmp) : \
      mri_copy(im_tmp);
   im_outer = mri_copy(im_inner);

   fl_im_inner = MRI_FLOAT_PTR(im_inner); 
   fl_im_outer = MRI_FLOAT_PTR(im_outer);

   // apply inner and outer blurring
   EDIT_blur_volume_3d( nx, ny, nz, Ledge[0], Ledge[1], Ledge[2],
                        MRI_float, fl_im_inner,
                        rad_in[0], rad_in[1], rad_in[2] );
   EDIT_blur_volume_3d( nx, ny, nz, Ledge[0], Ledge[1], Ledge[2],
                        MRI_float, fl_im_outer,
                        rad_out[0], rad_out[1], rad_out[2] );

   // subtract the outer from the inner at each voxel
   tmp_arr = (float *)calloc(nvox, sizeof(float));
   for ( idx=0 ; idx<nvox ; idx++ )
      tmp_arr[idx] = fl_im_inner[idx]- fl_im_outer[idx];

   // load this array into the dset subvolume
   EDIT_substitute_brick(dset_dog, ival, MRI_float, tmp_arr); 
   tmp_arr = NULL;

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

   if( opts.sigma_nvox[0] && opts.sigma_nvox[1] && \
       opts.sigma_nvox[2] ){ // user chose to scale voxel edge lengths
      for( ii=0 ; ii<3 ; ii++ )
         rad_in[ii] = opts.sigma_nvox[ii]*Ledge[ii];
   }
   else{ // user chose sigmas with physical mm values
      for( ii=0 ; ii<3 ; ii++ ) 
         rad_in[ii] = opts.sigma_rad[ii];
   }

   for( ii=0 ; ii<3 ; ii++ )
      rad_out[ii] = rad_in[ii] * opts.ratio_sig;

   return 0;
}
