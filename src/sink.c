#include "mrilib.h"

#define sinc(x) (((x)==0.0) ? 1.0 : sin(x)/(x))

int main( int argc , char *argv[] )
{
   int iarg , nP , nN , pp,nn ;
   double rr,qq, lam , val , beta=0.0 ;

   /* help? */

   if( argc < 4 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: sink N P lam [beta]\n"
            "Outputs to stdout the .1D file with 2*P+1 columns and 2*N+1 rows:\n"
            " sinc[ pi * (p-P*lam*n/N) ]\n"
            "OR, if beta != 0:\n"
            "  sinc[ pi * (p-P*lam*( beta+(1-beta)*n/N)) ]\n"
            " +sinc[ pi * (p-P*lam*(-beta+(1-beta)*n/N)) ]\n"
            "for p=-P..P (column index) and n=-N..N (row index).\n"
           ) ;
     exit(0) ;
   }

   nN = (int)strtod(argv[1],NULL); if( nN < 1 ){fprintf(stderr,"illegal N\n");exit(1);}
   nP = (int)strtod(argv[2],NULL); if( nP < 1 ){fprintf(stderr,"illegal P\n");exit(1);}
   lam = strtod(argv[3],NULL);
   if( argc > 4 ) beta = strtod(argv[4],NULL) ;

   rr = (nP*lam*(1.0-beta))/nN ;
   qq = (nP*lam*beta) ;

   for( nn=-nN ; nn <= nN ; nn++ ){
     for( pp=-nP ; pp <= nP ; pp++ ){
       val = sinc( PI * (pp-rr*nn-qq) ) ;
#if 1
       if( beta != 0.0 ) val += 0.737*sinc( PI * (pp-rr*nn+qq) ) ;
#endif
       printf(" %.12g",val) ;
     }
     printf("\n") ;
   }

   exit(0) ;
}
