#ifndef SUMA_DOT_INCLUDED

#define SUMA_DOT_INCLUDED

void SUMA_dot_product_CB( void *params);
SUMA_Boolean SUMA_dot_product(SUMA_DSET *in_dset,
                              double *ts, 
                              SUMA_DSET **out_dsetp,
                              NI_element *dotopt); 
SUMA_DSET *SUMA_GetDotPreprocessedDset(SUMA_DSET *in_dset, NI_element *dotopt);
double *SUMA_DotPreProcessTimeSeries(float *fv, int N_ts, 
                                     float TR, NI_element *dotopts);
NI_element *SUMA_set_dotopts(NI_element *dotopt, int ts_len,
                             float ftop, float fbot,
                             int norm, int prec,
                             int polort, char *ortname);

SUMA_DSET *SUMA_DotDetrendDset(  SUMA_DSET *in_dset, 
                                 float **refvec, int nref,
                                 float fbot, float ftop,
                                 int qdet, int *num_ort); 
void SUMA_DotXform_SetPending (NI_element *dotopts, int pending, char *id);
int SUMA_DotXform_GetRecomputeForDset (NI_element *dotopts, char *id);
SUMA_Boolean SUMA_DotXform_MakeOrts( NI_element *dotopt, int ts_len,
                                     int polort, char *ortname);

#define SUMA_GIQUIT {   \
   if (giset) SUMA_free(giset); SUMAg_CF->giset = NULL; SUMA_RETURN(NOPE); \
}
SUMA_Boolean SUMA_GICOR_setup_func( NI_stream nsg , NI_element *nel );
void SUMA_Show_GISET(GICOR_setup *giset, FILE *out, int verb);
char *SUMA_GISET_Info(GICOR_setup *giset, int verb);
int SUMA_AFNI_gicor_setref( SUMA_SurfaceObject *SO, int node );
SUMA_Boolean SUMA_GICOR_process_dataset( NI_element *nel  );




#endif
