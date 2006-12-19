#include "mrilib.h"

double efun(double x)
{
#if 0
  return x*sqrt(1.0+x*x) ;
#else
  return x + 0.19*sin(8.0*x) ;
#endif
}

double cfun( int n , double *x )
{
   double xs , f , ee ;
   int ii ;

   xs = x[0]+0.777*x[n-1] ; ee = efun(xs) ; f = ee*ee ;
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
   double *x , cpu1,cpu2 , cost ;
   double *xbot, *xtop ;

   if( argc < 2 ){ printf("test_powell [-q] [-f m a] n1 n2 ...\n"); exit(0); }

   while( aa < argc && argv[aa][0] == '-' ){
     if( strcmp(argv[aa],"-q") == 0 ){ quiet=1; aa++; continue; }

     if( strcmp(argv[aa],"-f") == 0 ){
       float m , a ;
       m = (float)strtod(argv[++aa],NULL) ;
       a = (float)strtod(argv[++aa],NULL) ;
       powell_set_mfac(m,a) ; aa++ ; continue ;
     }

     ERROR_exit("Unknown option %s",argv[aa]) ;
   }

   if( !quiet) powell_set_verbose( 1 ) ;

   for( ; aa < argc ; aa++ ){
     n    = (int)strtod(argv[aa],NULL) ; if( n < 1 ) continue ;
     x    = (double *)malloc(sizeof(double)*n) ;
     xbot = (double *)malloc(sizeof(double)*n) ;
     xtop = (double *)malloc(sizeof(double)*n) ;
     for( i=0 ; i < n ; i++ ){
       x[i] = 0.327*cos(i+.372) ; xbot[i] = -1.0 ; xtop[i] = 1.0 ;
     }
     cpu1 = COX_cpu_time() ;
     i = powell_newuoa_constrained(
           n , x , &cost , xbot,xtop , 777*n+1 , 151*n+1 , 5*n+1 , 0.22 , 0.0001 , 66666 , cfun ) ;
     cpu2 = COX_cpu_time()-cpu1 ;
     if( quiet ){
       printf("%d %f\n",i,cpu2) ; fflush(stdout) ;
     } else {
       printf("powell_newuoa: i=%d x=",i) ;
       for( i=0 ; i < n ; i++ ) printf("%f ",x[i]) ;
       printf("  f=%f  cpu=%f\n",cost,cpu2) ;
     }
     free(x); free(xbot); free(xtop);
   }
   exit(0) ;
}
