#include "mrilib.h"

double efun(double x)
{
  return x*sqrt(1.0+x*x) ;
}

double cfun( int n , double *x )
{
   double xs , f , ee ;
   int ii ;

   xs = x[0] ; ee = efun(xs) ; f = ee*ee ;
   for( ii=1 ; ii < n ; ii++ ){
     xs = 0.777*xs + x[ii] ;
     ee = efun(xs) ; f += ee*ee ;
   }

#if 0
printf("cfun(") ;
for(ii=0 ; ii < n-1 ; ii++)printf("%f,",x[ii]) ;
printf("%f)=%f\n",x[n-1],f) ;
#endif

   return f ;
}

int main( int argc , char *argv[] )
{
   int n , i , aa=1 , quiet=0 ;
   double *x , cpu1,cpu2 ;

   if( argc < 2 ){ printf("test_powell [-q] n1 n2 ...\n"); exit(0); }
   if( strcmp(argv[aa],"-q") == 0 ){ quiet=1; aa++; }
   
   for( ; aa < argc ; aa++ ){
     n = (int)strtod(argv[aa],NULL) ; if( n < 1 ) continue ;
     x = (double *)malloc(sizeof(double)*n) ;
     for( i=0 ; i < n ; i++ ) x[i] = 0.327*cos(i+.372) ;
     cpu1 = COX_cpu_time() ;
     i = powell_newuoa( n , x , 0.3 , 0.001 , 99999 , cfun ) ;
     cpu2 = COX_cpu_time()-cpu1 ;
     if( quiet ){
       printf("%d %f\n",i,cpu2) ; fflush(stdout) ;
     } else {
       printf("powell_newuoa: i=%d x=",i) ;
       for( i=0 ; i < n ; i++ ) printf("%f ",x[i]) ;
       printf("  f=%f  cpu=%f\n",cfun(n,x),cpu2) ;
     }
     free(x) ;
   }
   exit(0) ;
}
