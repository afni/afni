#ifndef SUMA_DOT_INCLUDED

#define SUMA_DOT_INCLUDED

void SUMA_dot_product_CB( void *params);
SUMA_Boolean SUMA_dot_product(SUMA_DSET *in_dset,
                              double *ts, 
                              SUMA_DSET **out_dsetp,
                              NI_element *dotopt); 


#endif
