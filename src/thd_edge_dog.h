#ifndef THD_EDGE_DOG_INCLUDED
#define THD_EDGE_DOG_INCLUDED


typedef struct {

   char *input_name;      
   char *mask_name;      
   char *prefix;          

   float sigma_rad[3];
   float sigma_nvox[3];
   float ratio_sig;


} PARAMS_edge_dog;

/* function to initialize params */
PARAMS_edge_dog set_edge_dog_defaults(void);

// ---------------------------------------------------------------------------

int calc_edge_dog( THD_3dim_dataset *dset_edge, PARAMS_edge_dog opts,
                   THD_3dim_dataset *dset_input, int ival);

int calc_edge_dog_sigmas(PARAMS_edge_dog opts, float *Ledge, 
                         float *rad_in, float *rad_out);

#endif
