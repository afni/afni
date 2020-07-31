#include "mrilib.h"
#include "checks_and_balances.h"
#include "DoTrackit.h"
#include "rsfc.h"



int CompareSetOrients(THD_3dim_dataset *A, THD_3dim_dataset *B)
{
   int i;
   char oriA[4], oriB[4];

   THD_fill_orient_str_3(A->daxes, oriA);
   THD_fill_orient_str_3(B->daxes, oriB);
   
   for( i=0 ; i<3 ; i++ )
      if( oriA[i] != oriB[i] )
         ERROR_exit("Bad orientational matching of inputs: "
                    "'%s' (%s) and '%s' (%s)!",
                    DSET_PREFIX(A), oriA, DSET_PREFIX(B), oriB);

   return 0;
}

// use Ndim to set number of dimensions to check: 3 or 4
int CompareSetDims(THD_3dim_dataset *A, THD_3dim_dataset *B, int Ndim)
{
   int DimA[4] = {0,0,0,0}, DimB[4] = {0,0,0,0};
   int i;

   if ( Ndim > 4)
      ERROR_exit("Bad call to CompareSetDims-- overtime!");

   DimA[0] = DSET_NX(A);   DimA[1] = DSET_NY(A); 
   DimA[2] = DSET_NZ(A);   DimA[3] = DSET_NVALS(A);

   DimB[0] = DSET_NX(B);   DimB[1] = DSET_NY(B); 
   DimB[2] = DSET_NZ(B);   DimB[3] = DSET_NVALS(B);

   for ( i=0 ; i<Ndim ; i++)
      if ( DimA[i] != DimB[i] ) // fixed cond, Nov,2016
         ERROR_exit("Bad dimensional matching of inputs: '%s' and '%s'!",
                    DSET_PREFIX(A), DSET_PREFIX(B));

   return 0;
}

int WB_corr_loop(
                 float *X,float *Y,
                 THD_3dim_dataset *A,
                 int *Dim,
                 byte ***mskd,
                 float *mapA,
                 int *myloc
                 )
{
   int ii,jj,kk,i;
   int idx=0, ctr=0;

   // loop through whole brain
   for( kk=0 ; kk<Dim[2] ; kk++ ) 
      for( jj=0 ; jj<Dim[1] ; jj++ ) 
         for( ii=0 ; ii<Dim[0] ; ii++ ) {
            if( mskd[ii][jj][kk] ) {

               if( !( (myloc[0] == ii) && 
                      (myloc[1] == jj) && 
                      (myloc[2] == kk)) ) {
                  i = THD_extract_float_array(ctr,A,Y) ;  
                  //mapA[idx] = BOBatanhd( (double) CORR_FUN(X,Y,Dim[3]) );
                  // Sept,2016
                  mapA[idx] = BOBatanhf( THD_pearson_corr(Dim[3],X,Y) ); 
               
                  idx++;
               }
            }
            ctr++;
         }
   
   //for ( ii=0 ; ii<Dim[3] ; ii++ )
   //fprintf(stderr," [%f, %f] ", X[ii],Y[ii]);
   
   return 0;
}


