#include <afni.h>
#include "checks_and_balances.h"
#include "DoTrackit.h"





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
      if ( DimA[i] != DimA[i] )
         ERROR_exit("Bad dimensional matching of inputs: '%s' and '%s'!",
                    DSET_PREFIXSTR(A), DSET_PREFIXSTR(B));

   return 0;
}

int WB_corr_loop(
                 double *X,double *Y,
                 THD_3dim_dataset *A,
                 int *Dim,
                 byte ***mskd,
                 double *mapA
                 )
{
   int ii,jj,kk,i;
   int idx=0, ctr=0;

   // loop through whole brain
   for( kk=0 ; kk<Dim[2] ; kk++ ) 
      for( jj=0 ; jj<Dim[1] ; jj++ ) 
         for( ii=0 ; ii<Dim[0] ; ii++ ) {
            if(mskd[ii][jj][kk] ) {

               i = THD_extract_double_array(ctr,A,Y) ;  
               mapA[idx] = (double) CORR_FUN(X,Y,Dim[3]);

               //fprintf(stderr,"MapCorr: %f ", mapA[idx]);

               idx++;
            }
            ctr++;
         }

   //for ( ii=0 ; ii<Dim[3] ; ii++ )
   //fprintf(stderr," [%f, %f] ", X[ii],Y[ii]);

   return 0;
}




int THD_extract_double_array( int ind, THD_3dim_dataset *dset, double *far )
{
   MRI_TYPE typ ;
   int nv , ival , nb , nb1 ;
   char  *iar ;      /* brick in the input */

   if( ind < 0             || far == NULL           ||
       !ISVALID_DSET(dset) || ind >= DSET_NVOX(dset)  ) return(-1) ;

   nv  = dset->dblk->nvals ;
   typ = DSET_BRICK_TYPE(dset,0) ;  /* raw data type */

   switch( typ ){

      default:           /* don't know what to do --> return nada */
         return(-1);
      break ;

      case MRI_byte:{
         byte *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (byte *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) far[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_short:{
         short *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (short *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) far[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_float:{
         float *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (float *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) far[ival] = bar[ind] ;
         }
      }
      break ;

      case MRI_complex:{
         complex *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (complex *) DSET_ARRAY(dset,ival) ;
            if( bar != NULL ) far[ival] = CABS(bar[ind]) ;
         }
      }
      break ;

   }

   if( THD_need_brick_factor(dset) ){
     for( ival=0 ; ival < nv ; ival++ )
       if( DSET_BRICK_FACTOR(dset,ival) > 0.0 )
         far[ival] *= DSET_BRICK_FACTOR(dset,ival) ;
   }

   return(0);
}
