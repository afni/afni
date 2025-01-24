#ifndef THD_STATPVAL_INCLUDED
#define THD_STATPVAL_INCLUDED


/*
  Object for things used with either p2dsetstat or dsetstat2p. Even
  though each prog has different inputs, in the end all of hte same
  things have to be known.
*/
typedef struct {

   char *input_name;    // 3D vol (prob from a 4D dset with subbrick selector)
   char *brick_lab;     // brick label

   float pval;          // will be either provided or calc'ed
   int   as_1_sided;    // 1 = 1sided; 0 = 2sided, bisided
   char *sidedness;     // str of sidedness
   char *stat_and_pars; // str of stat name and params

   float statval;       // will be either calc'ed or provided by user
   int params[10];      // far more space than needed, just to be safe!

   int verb;            // verbosity level

} PARAMS_statpval;

// -------------------------------------------------------------------------

/* functions to initialize params, for complementary progs (the use
   same struct type, for convenience) */
PARAMS_statpval set_p2dsetstat_defaults(void);
PARAMS_statpval set_dsetstat2p_defaults(void);




#endif
