#ifndef _BASICBORING_HEADER_
#define _BASICBORING_HEADER_

#include <gsl/gsl_matrix.h>

//#define ALLOWED_NROI (150)   // buffer size for array of refset...



int Color_RGB_to_HSL( float *RGB,           // N by 3 array
                      float *HSL            // N by 3 array
                      );
int Color_Vec_RGB_to_HSL( float **RGB,           // N by 3 array
                          float **HSL,           // 3xn array
                          byte *mskd,            // byte mask
                          int Nvox               // num vox  
                          ) ;

int Color_Vec_XYZdset_to_RGB( THD_3dim_dataset *VEC, // 3-vect
                          float **RGB,           // N by 2D array
                          byte *mskd,            // byte mask
                          int Nvox               // num vox  
                          );
float Color_Compand_sRGB(float x);

// ---------------------------------------------------------

int Basic_Info_Dim_and_Nvox( THD_3dim_dataset *X, int *Dim, int Ndim);

int Basic_Compare_DSET_dims( THD_3dim_dataset *X, THD_3dim_dataset *Y,
                             int Ndim);

// ---------------------------------------------------------

// simple viewing/debugging
void Show_2DMatrix_Ints(int **A, int M, int N);
void Show_2DMatrix_Floats(float **A, int M, int N);
void Show_1DArray_Floats(float *A, int N);

void Show_2DMatrix_Floats_gsl(gsl_matrix *A, int M, int N);
void Show_1DVector_Floats_gsl(gsl_vector *A, int N);


#endif /* _BASICBORING_HEADER_ */
