#include "mrilib.h" 
#include "thd_euler_dist.h"

PARAMS_euler_dist set_euler_dist_defaults(void)
{

   PARAMS_euler_dist defopt;

   defopt.input_name = NULL;     
   defopt.prefix = NULL;         

   defopt.zeros_are_zeroed = 0;  
   defopt.bounds_are_zero = 1;   
   defopt.do_sqrt = 1;           

   defopt.edims[0] = 0.0;       
   defopt.edims[1] = 0.0;
   defopt.edims[2] = 0.0;

   defopt.shape[0] = 0; 
   defopt.shape[1] = 0; 
   defopt.shape[2] = 0; 

   return defopt;
};

// ---------------------------------------------------------------------------

/*
  Ledge : (input) array of 3 vox edge lengths
  ord   : (output) array of 3 indices, describing decreasing vox size
*/
int sort_vox_ord_desc(int N, float *Ledge, int *ord)
{
   int i;
   float far[N];
   int iar[N];

   for( i=0 ; i<N ; i++){
      far[i] = Ledge[i];
      iar[i] = i;     // so that we get indices in decreasing order
   }

   // sorts far in increasing order, carrying along iar values
   qsort_floatint( N , far , iar );
   
   // and we want the 'output' to have the reverse order of iar
   for( i=0 ; i<N ; i++)
      ord[i] = iar[N-1-i]; 

   return 0;
}

// ---------------------------------------------------------------------------

int calc_EDT_3D_dim0( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival)
{

   return 0;
}

int calc_EDT_3D_dim1( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival)
{

   return 0;
}

int calc_EDT_3D_dim2( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival)
{

   return 0;
}
