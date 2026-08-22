#include "mrilib.h" 
#include "statpval_opts.h" 

/* function to initialize defaults for p2dsetstat... */
PARAMS_statpval set_p2dsetstat_defaults(void)
{
   PARAMS_statpval defopt = { 
          .input_name = NULL,
          .brick_lab  = NULL,
          .pval       = 1.0,
          .as_1_sided = 0,    // 1 = 1sided, 0 = 2sided, bisided
          .sidedness  = NULL,
          .stat_and_pars = NULL,
          .statval    = 0.0,
          .params     = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
          .verb       = 1 };

   return defopt;
};

/* ... and for related/complementary dsetstat2p (someday, these could
   differ, in theory. */
PARAMS_statpval set_dsetstat2p_defaults(void)
{
   PARAMS_statpval defopt = { 
          .input_name = NULL,
          .brick_lab  = NULL,
          .pval       = 1.0,
          .as_1_sided = 0,    // 1 = 1sided, 0 = 2sided, bisided
          .sidedness  = NULL,
          .stat_and_pars = NULL,
          .statval    = 0.0,
          .params     = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
          .verb       = 1 };

   return defopt;
};
