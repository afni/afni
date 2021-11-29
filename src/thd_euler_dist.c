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
