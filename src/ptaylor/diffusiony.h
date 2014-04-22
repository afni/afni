#ifndef _DIFFUSIONY_HEADER_
#define _DIFFUSIONY_HEADER_


int Dyadize( float **DT, 
             int N, 
             THD_3dim_dataset **EVALS,    
             float Lscale,
             THD_3dim_dataset **EVECS, 
             int INV[3], 
             byte *M );

int RicianNoiseDWIs( float **dwi,
                     int N,
                     int Ngrad,
                     THD_3dim_dataset *D,
                     float NOISE_DWI,
                     float NOISE_B0,
                     MRI_IMAGE *g,
                     byte *M,
                     float S0,
                     float bval,
                     gsl_rng *r);

int DT_TORTOISEtoAFNI(float **D, 
                      int N, 
                      THD_3dim_dataset *DTS, 
                      int INV[3], 
                      float Lscale);








#endif /* _DIFFUSIONY_HEADER_ */
