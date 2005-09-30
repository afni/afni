#include "mrilib.h"

#define TEST_TIMER

int main( int argc , char **argv )
{
   double a[9]  , e[3]  ; int ii ;
   double aa[9] , ee[3] ;
   double am[9] ;
   double ss , smax=0.0 , dd , dmax=0.0 ;
   double c1,c2,c3 ;

   srand48((long)time(NULL)) ; symeig_forbid_23(1) ;

#ifdef TEST_TIMER
   am[0] = 2.0*drand48() ;
   am[4] = 2.0*drand48() ;
   am[8] = 2.0*drand48() ;
   am[1] = am[3] = drand48() ;
   am[2] = am[6] = drand48() ;
   am[5] = am[7] = drand48() ;
   c1 = COX_cpu_time() ;
   for( ii=0 ; ii < 1000000 ; ii++ ){
     memcpy( a , am , sizeof(double)*9 ) ;
     symeig_3( a , e , 1 ) ;
   }
   c2 = COX_cpu_time() ;
   for( ii=0 ; ii < 1000000 ; ii++ ){
     memcpy( aa , am , sizeof(double)*9 ) ;
     symeig_double( 3 , aa , ee ) ;
   }
   c3 = COX_cpu_time() ;

   fprintf(stderr,"CPU: New=%g  Old=%g  Ratio=%g\n",c2-c1 , c3-c2 , (c3-c2)/(c2-c1) ) ;

   fprintf(stderr,"New #1: %10.5f  [ %10.5f %10.5f %10.5f ]\n",
           e[0], a[0],a[1],a[2] ) ;

   fprintf(stderr,"Old #1: %10.5f  [ %10.5f %10.5f %10.5f ]\n",
           ee[0], aa[0],aa[1],aa[2] ) ;

   fprintf(stderr,"New #2: %10.5f  [ %10.5f %10.5f %10.5f ]\n",
           e[1], a[3],a[4],a[5] ) ;

   fprintf(stderr,"Old #2: %10.5f  [ %10.5f %10.5f %10.5f ]\n",
           ee[1], aa[3],aa[4],aa[5] ) ;

   fprintf(stderr,"New #3: %10.5f  [ %10.5f %10.5f %10.5f ]\n",
           e[2], a[6],a[7],a[8] ) ;

   fprintf(stderr,"Old #3: %10.5f  [ %10.5f %10.5f %10.5f ]\n",
           ee[2], aa[6],aa[7],aa[8] ) ;

#else

#if 0
   for( ii=0 ; ii < 300000 ; ii++ ){
     am[0] = drand48() ;
     am[4] = drand48() ;
     am[8] = drand48() ;
     am[1] = am[3] = drand48() ;
     am[2] = am[6] = drand48() ;
     am[5] = am[7] = drand48() ;
     memcpy( a , am , sizeof(double)*9 ) ;
     symeig_3( a , e , 1 ) ;
     memcpy( aa , am , sizeof(double)*9 ) ;
     symeig_double( 3 , aa , ee ) ;
     ss = fabs(ee[0]-e[0]) + fabs(ee[1]-e[1]) + fabs(ee[2]-e[2]) ;
     if( ss > 1.e-6 ) fprintf(stderr,"*") ;
     if( ss > smax  ) smax = ss ;

     dd = fabs( fabs(a[0]*aa[0] + a[1]*aa[1] + a[2]*aa[2])
               +fabs(a[3]*aa[3] + a[4]*aa[4] + a[5]*aa[5])
               +fabs(a[6]*aa[6] + a[7]*aa[7] + a[8]*aa[8]) - 3.0 ) ;

     if( dd > 1.e-6 ) fprintf(stderr,"+") ;
     if( dd > dmax ) dmax = dd ;
   }
#else
   for( ii=0 ; ii < 900000 ; ii++ ){
     am[0] = drand48() ;
     am[3] = drand48() ;
     am[1] = am[2] = ( lrand48()%131 != 0 ) ? drand48() : 0.0 ;
     memcpy( a , am , sizeof(double)*4 ) ;
     symeig_2( a , e , 1 ) ;
     memcpy( aa , am , sizeof(double)*4 ) ;
     symeig_double( 2 , aa , ee ) ;
     ss = fabs(ee[0]-e[0]) + fabs(ee[1]-e[1]) ;
     if( ss > 1.e-6 ) fprintf(stderr,"* OLD=%g,%g NEW=%g,%g ",e[0],e[1],ee[0],ee[1]) ;
     if( ss > smax  ) smax = ss ;

     dd = fabs( fabs(a[0]*aa[0] + a[1]*aa[1])
               +fabs(a[2]*aa[2] + a[3]*aa[3]) - 2.0 ) ;

     if( dd > 1.e-6 ) fprintf(stderr,"+") ;
     if( dd > dmax ) dmax = dd ;
   }
#endif
   fprintf(stderr," smax=%g  dmax=%g\n",smax,dmax) ;
#endif

   exit(0) ;
}
