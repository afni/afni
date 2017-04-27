#include <afni.h>
#include "basic_boring.h"
#include <gsl/gsl_matrix.h>


/* 
   Basic thing to get dimensions of a data set and returned Nvox, just
   at one go. Ndim is either 3 or 4, depending on whether you want only
   spatial or space+time, respectively.
*/
int Basic_Info_Dim_and_Nvox( THD_3dim_dataset *X, int *Dim, int Ndim)
{
   int Nvox = -1;
   char *prefix;

   prefix = DSET_PREFIX(X);

   if( Ndim == 4 ) {
      Dim[3] = DSET_NVALS(X); 
      if( Dim[3] <=0 )
         ERROR_exit("\n\n Problem getting %s data set dimension [3].\n\n",
                 prefix);
   }
   else if( Ndim != 3 ){
      ERROR_exit("\n\n 'Ndim' arg must be either 3 or 4.\n\n",
                 prefix);
   }

   Nvox = DSET_NVOX(X);
   Dim[0] = DSET_NX(X); 
   Dim[1] = DSET_NY(X); 
   Dim[2] = DSET_NZ(X); 

   if( (Nvox <=0) || (Dim[0] <=0) || (Dim[1] <=0) || (Dim[2] <=0) ){
      ERROR_exit("\n\n Problem getting %s data set dimensions.\n\n",
                 prefix);
    }

   return Nvox;
}

/*
  Compare basic properties of data set: just dimensions right now
 */
int Basic_Compare_DSET_dims( THD_3dim_dataset *X, THD_3dim_dataset *Y,
                             int Ndim)
{
   int i;
   int NvoxX = -1, NvoxY = -1;
   int DimX[4]={0,0,0,0};     // dim in each dir
	int DimY[4]={0,0,0,0};     // dim in each dir
   char *prefixX, *prefixY;

   prefixX = DSET_PREFIX(X);
   prefixY = DSET_PREFIX(Y);

   NvoxX = Basic_Info_Dim_and_Nvox( X, 
                                    DimX, 
                                    Ndim);

   NvoxY = Basic_Info_Dim_and_Nvox( Y, 
                                    DimY, 
                                    Ndim);

   for( i=0 ; i<Ndim ; i++ )
      if( DimX[i] != DimY[i] ) {
         ERROR_exit("\n\n Dsets %s %s don't match in [%d] dimension.\n\n",
                    prefixX, prefixY, i);
      }

   RETURN(0);
}
 

/*
  for either user-viewing or just debugging: A is an MxN matrix of ints
*/
void Show_2DMatrix_Ints(int **A, int M, int N)
{
   int i,j;

   fprintf(stderr,"\n");
   for( i=0 ; i<M ; i++ ) {
      for( j=0 ; j<N ; j++ ) 
         fprintf(stderr," %5d ", A[i][j]);
      fprintf(stderr,"\n");
   }
   fprintf(stderr,"\n");
}

/*
  for either user-viewing or just debugging: A is an MxN matrix of ints
*/
void Show_2DMatrix_Floats(float **A, int M, int N)
{
   int i,j;

   fprintf(stderr,"\n");
   for( i=0 ; i<M ; i++ ) {
      for( j=0 ; j<N ; j++ ) 
         fprintf(stderr," %8.4f ", A[i][j]);
      fprintf(stderr,"\n");
   }
   fprintf(stderr,"\n");
}

void Show_1DArray_Floats(float *A, int N)
{
   int j;
   
   fprintf(stderr,"\n");
   for( j=0 ; j<N ; j++ ) 
      fprintf(stderr," %8.4f ", A[j]);
   fprintf(stderr,"\n");
}



void Show_2DMatrix_Floats_gsl(gsl_matrix *A, int M, int N)
{
   int i,j;

   fprintf(stderr,"\n");
   for( i=0 ; i<M ; i++ ) {
      for( j=0 ; j<N ; j++ ) 
         fprintf(stderr," %8.4f ", gsl_matrix_get(A,i,j));
      fprintf(stderr,"\n");
   }
   fprintf(stderr,"\n");
}

void Show_1DVector_Floats_gsl(gsl_vector *A, int N)
{
   int i,j;

   fprintf(stderr,"\n");
   for( j=0 ; j<N ; j++ ) 
      fprintf(stderr," %8.4f ", gsl_vector_get(A,j));
   
   fprintf(stderr,"\n");
}

