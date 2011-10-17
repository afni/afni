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

static double cf_nor(double x){ return 1.0-qg(x) ; }

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
   free(vvv) ; return ad ;
}
