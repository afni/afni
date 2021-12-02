#ifndef THD_EDGE_DOG_INCLUDED
#define THD_EDGE_DOG_INCLUDED


typedef struct {

   char *input_name;      
   char *mask_name;      
   char *prefix;          
   char prefix_dog[THD_MAX_PREFIX];          

   int do_output_dog;

   float sigma_rad[3];
   float sigma_nvox[3];
   float ratio_sig;

} PARAMS_edge_dog;

/* function to initialize params */
PARAMS_edge_dog set_edge_dog_defaults(void);

// ---------------------------------------------------------------------------

int build_dog_prefix( PARAMS_edge_dog *opts);

int calc_edge_dog_DOG( THD_3dim_dataset *dset_dog, PARAMS_edge_dog opts,
                       THD_3dim_dataset *dset_input, int ival);

int calc_edge_dog_sigmas(PARAMS_edge_dog opts, float *Ledge, 
                         float *rad_in, float *rad_out);

#endif
