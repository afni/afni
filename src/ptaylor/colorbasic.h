#ifndef _COLORBASIC_HEADER_
#define _COLORBASIC_HEADER_

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

int Basic_Dim_and_Nvox( THD_3dim_dataset *X, int *Dim, int Ndim, 
                        char *prefix);

int Basic_compare_DSET_dims( THD_3dim_dataset *X, THD_3dim_dataset *Y,
                             int Ndim, char *prefixX, char *prefixY);


#endif /* _COLORBASIC_HEADER_ */
