#ifndef THD_EDGE_DOG_INCLUDED
#define THD_EDGE_DOG_INCLUDED


typedef struct {

   char *input_name;      
   char *mask_name;      
   char *prefix;          
   char *prefix_dog; 

   int do_automask;
   int amask_ndil;

   int do_output_intermed;

   float sigma_rad[3];
   float sigma_nvox[3];
   float ratio_sigma;

   int edge_bnd_NN;
   int edge_bnd_side;
   int edge_bnd_scale;
   char *edge_bnd_side_user;

   char *only2D; 
   int axes_to_proc[3];

   int verb;

} PARAMS_edge_dog;

/* function to initialize params */
PARAMS_edge_dog set_edge_dog_defaults(void);

// ---------------------------------------------------------------------------

int build_edge_dog_suppl_prefix( PARAMS_edge_dog *opts, char *ostr,
                                 char *suffix);

int calc_edge_dog_sigmas(PARAMS_edge_dog opts, float *Ledge, 
                         float *rad_in, float *rad_out);
int calc_edge_dog_sigmas_old(PARAMS_edge_dog opts, float *Ledge, 
                             float *rad_in, float *diff_rad_out);

int calc_edge_dog_DOG( THD_3dim_dataset *dset_dog, PARAMS_edge_dog opts,
                       THD_3dim_dataset *dset_input, int ival);

int calc_edge_dog_BND( THD_3dim_dataset *dset_bnd, PARAMS_edge_dog opts,
                       THD_3dim_dataset *dset_dog, int ival,
                       int argc, char *argv[]);
int scale_edge_dog_BND( THD_3dim_dataset *dset_bnd, PARAMS_edge_dog opts,
                        THD_3dim_dataset *dset_input, int ival);
int scale_edge_dog_BND_old( THD_3dim_dataset *dset_bnd, PARAMS_edge_dog opts,
                        THD_3dim_dataset *dset_dog, int ival);

int calc_edge_dog_thr_EDT( THD_3dim_dataset *dset_bnd, PARAMS_edge_dog opts,
                           THD_3dim_dataset *dset_edt, int ival_bnd, 
                           int ival_edt);

#endif
