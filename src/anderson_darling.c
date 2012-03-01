#include "mrilib.h"

/*----------------------------------------------------------------------------*/

static double cf_nor( double x ){ return 1.0-0.5*erfc(x/1.414213562373095); }

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
     if( ccc > 0.0 && ccc < 1.0 )
       asum += (2*ii+1) * log(ccc) + (2*(npt-ii)-1) * log(1.0-ccc) ;
   }

   free(yyy) ; asum = -npt - asum / npt ; return asum ;
}

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

float * anderson_darling_simulate( int npt , int ntrial )
{
   float *ad ; double *xxx ; int ii , jj ;

   if( npt < 10 || ntrial <= 0 ) return NULL ;

   ad  = (float * )malloc(sizeof(float)*ntrial) ;
   xxx = (double *)malloc(sizeof(double)*npt) ;
   for( jj=0 ; jj < ntrial ; jj++ ){
     for( ii=0 ; ii < npt ; ii++ ) xxx[ii] = zgaussian()+zgaussian()+zgaussian() ;
     ad[jj] = - (float)anderson_darling_normal( npt , xxx ) ;
   }
   qsort_float( ntrial , ad ) ;
   for( jj=0 ; jj < ntrial ; jj++ ) ad[jj] = -ad[jj] ;
   return ad ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int npt , ntrial , ii ; float *ad ;

   if( argc < 3 ) exit(0) ;
   npt    = (int)strtod(argv[1],NULL) ;
   ntrial = (int)strtod(argv[2],NULL) ;
   ad = anderson_darling_simulate( npt , ntrial ) ;
   if( ad == NULL ) exit(1) ;
   for( ii=0 ; ii < ntrial ; ii++ ) printf("%g\n",ad[ii]) ;
   exit(0) ;
}
