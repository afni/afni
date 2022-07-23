#ifndef THD_EDT_INCLUDED
#define THD_EDT_INCLUDED

#include <float.h>

#define EUCLID_BIG FLT_MAX            // from float.h

/* struct of quantities for running Euclidean Distance Transform (EDT) 

    only2D       : (str) name of 2D plane to work within; so EDT is calculated
                   across whole FOV, but only planewise with each of the
                   given planes

    dist_sq      : if False (def), the output image of EDT values is distance
                   values; otherwise, the values are distance**2 (because
                   that is what the program works with).

    bounds_are_zero : switch for how to treat FOV boundaries for
                   nonzero-ROIs; True means they are ROI boundaries
                   (so FOV is a "closed" boundary for the ROI), and
                   False means that the ROI continues "infinitely" at
                   FOV boundary (so it is "open").  Zero-valued ROIs
                   (= background) are not affected by this value.

    zeros_are_zeroed : if False, EDT values are output for the
                   zero-valued region; otherwise, zero them out, so EDT
                   values are only reported in the non-zero ROIs. NB: the
                   EDT values are still calculated everywhere; it is just
                   a question of zeroing out later (no time saved for True).

    ignore_voxdims : we have implemented an EDT alg that works in
                   terms of physical distance and can use the voxel dimension
                   info in each direction.  However, users might want 'depth'
                   just in voxel units (so, delta=1 everywhere), which 
                   using this option will give 'em.                   

    rimify       : if 0.0 (def), the normal depthmap is output; if >0,
                   the output is not depth values, but the original
                   input values thresholded (depth<=THR_RIM) at a
                   given value to create a "rim" only based on the
                   user preference (can be in terms of voxel count or
                   mm distance, whatever user was calculating for
                   depth); if <0, then the depth is thresholded as 
                   abs(depth) >= abs(THR_RIM)---but note that this
                   might lead to a region disappearing.

    zero_region_sign :(-1 or +1) user can flip sign of distances in zero-valued
                   region (def: 1)

    nz_region_sign :(-1 or +1) user can flip sign of distances in nonzero-valued
                   region (def: 1)

    edims        : (len=3 fl arr) element dimensions (here, voxel edge lengths)

    shape        : (len=3 int arr) matrix size in each direction

    axes_to_proc : (len=3 int arr) switches for running EDT along selected 
                   axes;  default to run with all of them.

    binary_only  : (int) switch to treat input as a binarized mask only,
                   which leads to faster proc for this special case
                   (def: 0)
                   
*/
typedef struct {

   char *input_name;      
   char *mask_name;      
   char *prefix;          

   int zero_region_sign;
   int nz_region_sign;

   int zeros_are_zeroed;  
   int bounds_are_zero;   
   int ignore_voxdims;
   int dist_sq;           
   float rimify;

   float edims[3];        
   int   shape[3];        

   char *only2D;          
   int axes_to_proc[3];

   int binary_only;

   int verb;

} PARAMS_euclid_dist;

/* function to initialize EDT params */
PARAMS_euclid_dist set_euclid_dist_defaults(void);

// ---------------------------------------------------------------------------

int sort_vox_ord_desc(int N, float *Ledge, int *ord);

int choose_axes_for_plane( THD_3dim_dataset *dset, char *which_slice,
                           int *onoff_arr, int verb );

int calc_EDT_rim(THD_3dim_dataset *dset_rim, THD_3dim_dataset *dset_edt, 
                 THD_3dim_dataset *dset_roi, float rim_thr, int copy_lt);

/*
  Set of funcs for special case of user saying the input is a binary
  mask.
*/
int calc_EDT_3D_BIN( THD_3dim_dataset *dset_edt, PARAMS_euclid_dist opts,
                     THD_3dim_dataset *dset_roi, THD_3dim_dataset *dset_mask,
                     int ival);
int calc_EDT_3D_BIN_dim0( float ***arr_dist, PARAMS_euclid_dist opts,
                          int nx, int ny, int nz, float delta,
                          float *flarr, float *workarr );
int calc_EDT_3D_BIN_dim1( float ***arr_dist, PARAMS_euclid_dist opts,
                          int nx, int ny, int nz, float delta,
                          float *flarr, float *workarr );
int calc_EDT_3D_BIN_dim2( float ***arr_dist, PARAMS_euclid_dist opts,
                          int nx, int ny, int nz, float delta,
                          float *flarr, float *workarr );
int apply_opts_to_edt_arr_BIN( float ***arr_dist, float ***arr_distZ, 
                               PARAMS_euclid_dist opts,
                               int nx, int ny, int nz );

/*
  Set of funcs for more general case of ROI map input.
*/
int calc_EDT_3D_GEN( THD_3dim_dataset *dset_edt, PARAMS_euclid_dist opts,
                     THD_3dim_dataset *dset_roi, THD_3dim_dataset *dset_mask,
                     int ival);
int calc_EDT_3D_GEN_dim0( float ***arr_dist, PARAMS_euclid_dist opts,
                          THD_3dim_dataset *dset_roi, int ival,
                          float *flarr, float *workarr, int *maparr );
int calc_EDT_3D_GEN_dim1( float ***arr_dist, PARAMS_euclid_dist opts,
                          THD_3dim_dataset *dset_roi, int ival,
                          float *flarr, float *workarr, int *maparr );
int calc_EDT_3D_GEN_dim2( float ***arr_dist, PARAMS_euclid_dist opts,
                          THD_3dim_dataset *dset_roi, int ival,
                          float *flarr, float *workarr, int *maparr );
int apply_opts_to_edt_arr_GEN( float ***arr_dist, PARAMS_euclid_dist opts,
                               THD_3dim_dataset *dset_roi, int ival);
int run_EDTD_GEN_per_line( float *dist2_line, float *warr, int *roi_line,
                           int Na,
                           float delta, int bounds_are_zero, int binary_only );

/*
  This always applies (whether input is binary mask or ROI map)---
  it's the heart of the matter, from FH2012.
*/
float * Euclidean_DT_delta(float *f0, int n, float delta);




#endif
