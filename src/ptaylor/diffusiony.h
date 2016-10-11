#ifndef _DIFFUSIONY_HEADER_
#define _DIFFUSIONY_HEADER_

#include <gsl/gsl_rng.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_eigen.h>
#include <gsl/gsl_sort.h>
#include "gsl/gsl_permutation.h"
#include <gsl/gsl_types.h>
#include <gsl/gsl_permute.h>

#define BAD_WEI ( 0 ) // tiny weight to unfit values, if necessary.

int Finalize_Uncert_Array( float **UU, 
                           short *mskd,
                           int Nvox,
                           int Nj );


float Calc_FA_from_gsl_Evals( gsl_vector *E );

int Calc_Eigs_Uncert( int v,
                      float **UU,
                      gsl_vector *dd,
                      gsl_matrix *testD,        // have to recreate
                      gsl_vector *Eval,
                      gsl_matrix *Evec,
                      THD_3dim_dataset **PARS,
                      THD_3dim_dataset **VECS
                      );

int Calc_DTI_lin_tensor( gsl_vector *x,
                         gsl_vector *dd,
                         gsl_matrix *C,
                         gsl_matrix *testD,
                         gsl_vector *Eval,
                         gsl_eigen_symm_workspace *EigenV,
                         int *POSDEF
                         );

int Make_Uncert_Matrs_init( int v,
                            float **bseven,
                            THD_3dim_dataset *DWI,
                            int *SRInd,
                            float *Wei,
                            gsl_vector *x,
                            gsl_matrix *B,
                            gsl_matrix *BTW,
                            int Mj
                            );

int Make_Uncert_Matrs_final( gsl_matrix *B,
                             gsl_matrix *BTW,
                             gsl_matrix *BTWB,
                             gsl_matrix *BTWBinv,
                             gsl_matrix *C
                             );

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


int GradConv_GmatA_from_Gsign( float *matr, float *grad );
// assumes I/O has unit or zero mag!
int GradConv_Gsign_from_GmatA( float *grad, float *matr ); 

int Basic_Grads_to_B7( float **bseven,
                        MRI_IMAGE *flim,
                        int Ng);
int Basic_Bmats_to_B7( float **bseven,
                       MRI_IMAGE *flim,
                       int Nb);

int Make_Jackknife_Inds_keep0th( int **A, 
                                 int M,
                                 int Mj,
                                 int Nj,
                                 int CHOOSE_SEED);



#endif /* _DIFFUSIONY_HEADER_ */
