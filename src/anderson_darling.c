#include "mrilib.h"

/*----------------------------------------------------------------------------*/

double anderson_darling_statistic( int npt, double *val, double (*cf)(double) )
{
   double *yyy , asum , ccc ; int ii ;

   if( npt < 10 || val == NULL || cf == NULL ) return 0.0 ;

   yyy = (double *)malloc(sizeof(double)*npt) ;
   memcpy( yyy , val , sizeof(double)*npt ) ;
   qsort_double( npt , yyy ) ;

   asum = 0.0 ;
   for( ii=0 ; ii < npt ; ii++ ){
     ccc   = cf(yyy[ii]) ;
     asum += (2*ii+1) * log(ccc) + (2*(npt-ii)-1) * log(1.0-ccc) ;
   }

   free(yyy) ; asum = -npt - asum / npt ; return asum ;
}

/*----------------------------------------------------------------------------*/

static double cf_nor( double x ){ return 1.0-0.5*erfc(x/1.414213562373095); }

/*----------------------------------------------------------------------------*/

double anderson_darling_normal( int npt , double *xxx )
{
   double *vvv , xm , xq , ad ; int ii ;

   if( npt < 10 || xxx == NULL ) return 0.0 ;

   vvv = (double *)malloc(sizeof(double)*npt) ;
   memcpy( vvv , xxx , sizeof(double)*npt ) ;

   xm = 0.0 ;
   for( ii=0 ; ii < npt ; ii++ ) xm += vvv[ii] ;

   xm /= npt ; xq = 0.0 ;
   for( ii=0 ; ii < npt ; ii++ ) xq += (vvv[ii]-xm)*(vvv[ii]-xm) ;
   if( xq <= 0.0 ){ free(vvv) ; return 0.0 ; }

   xq = sqrt( (npt-1.0) / xq ) ; 
   for( ii=0 ; ii < npt ; ii++ ) vvv[ii] = (vvv[ii]-xm) * xq ;

   ad = anderson_darling_statistic( npt , vvv , cf_nor ) ;
   ad *= ( 1.0 + 4.0/npt - 25.0/(npt*npt) ) ;
   free(vvv) ; return ad ;
}

/*----------------------------------------------------------------------------*/

#include "zgaussian.c"

int main( int argc , char *argv[] )
{
   int npt , ii ; double *xxx , ad ;

   if( argc < 2 ) exit(0) ;
   npt = (int)strtod(argv[1],NULL) ; if( npt < 10 ) exit(0) ;
   xxx = (double *)malloc(sizeof(double)*npt) ;
   init_rand_seed(0) ;
   for( ii=0 ; ii < npt ; ii++ ) xxx[ii] = zgaussian()+zgaussian()+zgaussian() ;
   ad = anderson_darling_normal( npt , xxx ) ;
   printf( "%.5g\n" , ad ) ;
   exit(0) ;
}
