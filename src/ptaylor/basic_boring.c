#include "mrilib.h"
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

// -----------------------------------------------------
/*
typedef struct {
   int   d[3];      // N along each axis
   int   maxd;      // max dim for len of arrays
   int   **sizeA;   // Nvox per slice in A, 3 x max(d)
   int   **sizeB;   // Nvox per slice in B, 3 x max(d)
   float **dice;    // Dice per slice, 3 x max(d)
   float **coors;   // Coors per slice, 3 x max(d)
} slidice;
*/

slidice *Create_slidice( int *Dim, 
                         THD_3dim_dataset *A,
                         THD_3dim_dataset *B )
{
   slidice *ss=NULL;
   int i;
   int MAXD = -1;
   float cc,dd,ee;
      
   ENTRY("Create_slicedice");
   
   ss = (slidice *)calloc(1, sizeof(slidice));
   if (ss == NULL) {
      ERROR_message("Failed to allocate slice-dice");
      RETURN(NULL);
   }
   ss->d[0] = Dim[0];
   ss->d[1] = Dim[1];
   ss->d[2] = Dim[2];
   
   for( i=0 ; i<3 ; i++ )
      if( Dim[i] > MAXD ) 
         MAXD = Dim[i];
   ss->maxd = MAXD;
   
   INFO_message("Max dimension is: %d", MAXD);

   ss->sizeA = calloc(3, sizeof(ss->sizeA)); 
   for(i=0 ; i<3 ; i++) 
      ss->sizeA[i] = calloc(ss->maxd, sizeof(int)); 
   
   ss->sizeB = calloc(3, sizeof(ss->sizeB)); 
   for(i=0 ; i<3 ; i++) 
      ss->sizeB[i] = calloc(ss->maxd, sizeof(int)); 
   
   ss->dice = calloc(3, sizeof(ss->dice)); 
   for(i=0 ; i<3 ; i++) 
      ss->dice[i] = calloc(ss->maxd, sizeof(float)); 
   
   ss->coors = calloc(3, sizeof(ss->coors)); 
   for(i=0 ; i<3 ; i++) 
      ss->coors[i] = calloc(ss->maxd, sizeof(float)); 

   if ( (ss->sizeA==NULL) || (ss->sizeB==NULL) || (ss->dice==NULL) ||
        (ss->coors==NULL) ) {
      ERROR_message("Failed to allocate slidice arrays");
      Free_slidice(ss,1); RETURN(NULL);
   }
   
   // record coor values, one at a time, unfortunately
   for( i=0 ; i< ss->d[0] ; i++ ) {
   AFNI_ijk_to_xyz( A, i, 0, 0, &cc, &dd, &ee) ;
   ss->coors[0][i] = cc;
   }
   for ( i=0 ; i< ss->d[1] ; i++) {
      AFNI_ijk_to_xyz( A, 0, i, 0, &cc, &dd, &ee) ;
      ss->coors[1][i] = dd;
   }
   for ( i=0 ; i< ss->d[2] ; i++) {
      AFNI_ijk_to_xyz( A,
                       0, 0, i, &cc, &dd, &ee) ;
      ss->coors[2][i] = ee;
   }

   RETURN(ss);
}

slidice *Free_slidice(slidice *ss, int n) 
{
   int i,j;
   
   ENTRY("Free_slice_dice_info");

   if (!ss) RETURN(NULL);

   for (i=0; i<n; ++i) 
      if (ss[i].sizeA) { 
         for (j=0; j<3; j++) {
            free(ss[i].sizeA[j]);
            free(ss[i].sizeB[j]);
            free(ss[i].dice[j]);
            free(ss[i].coors[j]);
         }
         free(ss[i].sizeA);
         free(ss[i].sizeB);
         free(ss[i].dice);
         free(ss[i].coors);
      }
   free(ss); 
   RETURN(NULL);
}

int Dice_em_up_calcs( slidice *ss, 
                      byte ***ma,
                      byte ***mb)
{
   int i,j,k,mm,nn;

   ENTRY("Dice_em_up_calcs");

   for( k=0 ; k<ss->d[2] ; k++ ) 
      for( j=0 ; j<ss->d[1] ; j++ ) 
         for( i=0 ; i<ss->d[0] ; i++ ) {
            if( ma[i][j][k] ) {
               ss->sizeA[0][i]+=1;
               ss->sizeA[1][j]+=1;
               ss->sizeA[2][k]+=1;
            }
            if( mb[i][j][k] ) {
               ss->sizeB[0][i]+=1;
               ss->sizeB[1][j]+=1;
               ss->sizeB[2][k]+=1;

               // INTERSECTION: just temp storage at the moment!
               if( ma[i][j][k] ) {
                  ss->dice[0][i]+=2.;
                  ss->dice[1][j]+=2.;
                  ss->dice[2][k]+=2.;
               }
            }
         }

   // now combine the ratios
   for( nn=0 ; nn<3 ; nn++ )
      for( i=0 ; i<ss->d[nn] ; i++ ) {
         mm = ss->sizeA[nn][i] + ss->sizeB[nn][i];
         if ( mm )
            ss->dice[nn][i] = ss->dice[nn][i] / ( (float) mm );
      }

   return 0;
}

int Find_slidice_orange( slidice *ss, 
                         int FOV_TYPE,
                         byte **orange)
{
   int i,j,k,mm;
   
   ENTRY("Find_slidice_orange");

   switch( FOV_TYPE ) {
      
   case 0:
      // all: on
      for( mm=0 ; mm<3 ; mm++ )
         for( i=0 ; i< ss->d[mm] ; i++ )
            orange[mm][i] = 1;
      break;
      
   case 1:
      // where A or B: on
      for( mm=0 ; mm<3 ; mm++ )
         for( i=0 ; i< ss->d[mm] ; i++ )
            if( ss->sizeA[mm][i] ||  ss->sizeB[mm][i] )
               orange[mm][i] = 1;
      break;
   case 2:
      // where A AND B: on
      for( mm=0 ; mm<3 ; mm++ )
         for( i=0 ; i< ss->d[mm] ; i++ )
            if( ss->sizeA[mm][i] &&  ss->sizeB[mm][i] )
               orange[mm][i] = 1;
      break;
   case 3:
      // where A is not empty: on
      for( mm=0 ; mm<3 ; mm++ )
         for( i=0 ; i< ss->d[mm] ; i++ )
            if( ss->sizeA[mm][i] )
               orange[mm][i] = 1;
      break;
   case 4:
      // where B is not empty: on
      for( mm=0 ; mm<3 ; mm++ )
         for( i=0 ; i< ss->d[mm] ; i++ )
            if( ss->sizeB[mm][i] )
               orange[mm][i] = 1;
      break;
   default:
      ERROR_exit( "Can't find FOV type %d!",
                  FOV_TYPE);
   }
   
   return 0;
}
