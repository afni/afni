#include "mrilib.h"

#define sinc(x) (((x)==0.0) ? 1.0 : sin(x)/(x))

int main( int argc , char *argv[] )
{
   int iarg , nP , nN , pp,nn ;
   double rr, lam , val ;

   /* help? */

   if( argc < 4 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: sink N P lam\n"
            "Outputs to stdout the .1D file with 2*P+1 columns and 2*N+1 rows:\n"
            " sinc[ pi * (p-P*lam*n/N) ]\n"
            "for p=-P..P (column index) and n=-N..N (row index).\n"
           ) ;
     exit(0) ;
   }

   nN = (int)strtod(argv[1],NULL); if( nN < 1 ){fprintf(stderr,"illegal N\n");exit(1);}
   nP = (int)strtod(argv[2],NULL); if( nP < 1 ){fprintf(stderr,"illegal P\n");exit(1);}
   lam =     strtod(argv[3],NULL);
   rr  = (nP*lam)/nN ;

   for( nn=-nN ; nn <= nN ; nn++ ){
     for( pp=-nP ; pp <= nP ; pp++ ){
       val = sinc( PI * (pp-rr*nn) ) ;
       printf(" %.12g",val) ;
     }
     printf("\n") ;
   }

   exit(0) ;
}
