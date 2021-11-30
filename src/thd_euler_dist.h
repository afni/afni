#ifndef THD_EDT_INCLUDED
#define THD_EDT_INCLUDED

#define BIG FLT_MAX     // from float.h


/* struct of quantities for running Euler Distance Transform (EDT) 

    do_sqrt      : if True, the output image of EDT values is distance
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

    edims        : (len=3 fl arr) element dimensions (here, voxel edge lengths)

    shape        : (len=3 int arr) matrix size in each direction
*/
typedef struct {

   char *input_name;      
   char *mask_name;      
   char *prefix;          

   int zeros_are_zeroed;  
   int bounds_are_zero;   
   int do_sqrt;           

   float edims[3];        
   int   shape[3];        

} PARAMS_euler_dist;

/* function to initialize EDT params */
PARAMS_euler_dist set_euler_dist_defaults(void);

// ---------------------------------------------------------------------------

int sort_vox_ord_desc(int N, float *Ledge, int *ord);

int calc_EDT_3D_dim0( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival,
                      float *flarr, int *maparr );
int calc_EDT_3D_dim1( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival,
                      float *flarr, int *maparr );
int calc_EDT_3D_dim2( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival,
                      float *flarr, int *maparr );

int run_EDTD_per_line( float *dist2_line, int *roi_line, int Na,
                       float delta, int bounds_are_zero );

float * Euclidean_DT_delta(float *f, int n, float delta);

#endif
