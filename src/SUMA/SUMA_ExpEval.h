#ifndef SUMA_EXPEVAL_INCLUDED
#define SUMA_EXPEVAL_INCLUDED

char *SUMA_bool_eval( char *expr, byte *res );
void SUMA_bool_eval_test( char *expr, byte exprval );
void SUMA_bool_eval_truth_table(char *expr, byte use_orig);
SUMA_Boolean SUMA_bool_mask_eval(int N_vals, int N_vars, byte **mask, char *expr,
                                 byte *res, byte *varcol, byte *rescol,
                                 char **resstr);

#endif
