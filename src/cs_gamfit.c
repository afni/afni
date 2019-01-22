#include "mrilib.h"

/*--------------------------------------------------------------------------*/
/* Find the gamma variate parameters (p,q) given the desired peak and width */
/*--------------------------------------------------------------------------*/

/* gamma variate formula */

#undef  GV
#define GV(t,p,q) ( pow( (t) / ((p)*(q)) , (p) ) * exp( (p) - (t)/(q) ) )

/* log of gamma variate */

#undef  LGV
#define LGV(t,p,q) ( (p) * log( t / ((p)*(q)) ) + (p) - (t)/(q) )

/* derivative of log( gamma variate ) */

#undef  LGVP
#define LGVP(t,p,q) ( (p) / (t) - 1.0 / (q) )

/*----------*/

static double_pair gam_find_val( double p , double q , double val )
{
   double_pair result = {-1.0,-1.0} ;
   double peak , fwhm ;
   double xx , dx , lval ; int ii ;

   if( p <= 0.0 || q <= 0.0 || val >= 1.0 || val <= 0.0 ) return result ;

   peak = p*q ; fwhm = 2.3 * sqrt(p) * q ; lval = log(val) ;

/** fprintf(stderr,"====== FIND(%g,%g,%g)\n",p,q,val) ; **/

   xx = peak + 0.3*fwhm ;
/** fprintf(stderr,"  x = %g",xx) ; **/
   for( ii=0 ; ii < 6 ; ii++ ){
     dx = (LGV(xx,p,q) - lval) / LGVP(xx,p,q) ;
     xx = xx - dx ;
/** fprintf(stderr,"  %g",xx) ; **/
     if( fabs(dx) < 0.000001*xx ) break ;
   }
   result.b = xx ;

   xx = peak - 0.5*fwhm ; if( xx <= 0.0 ) xx = 0.5*peak ;
/** fprintf(stderr,"\n  x = %g",xx) ; **/
   for( ii=0 ; ii < 6 ; ii++ ){
     dx = (LGV(xx,p,q) - lval) / LGVP(xx,p,q) ;
     xx = xx - dx ;
     if( xx <= 0.0 ) xx = 0.5*(xx+dx) ;
/** fprintf(stderr,"  %g",xx) ; **/
     if( fabs(dx) < 0.000001*xx ) break ;
   }
   result.a = xx ;
/** fprintf(stderr,"\n  RESULT p=%g q=%g --> x=%g x=%g\n",p,q,result.a,result.b) ; **/
   return result ;
}

/*----------*/

static double Gpeak , Gfwhm ;

double gam_find_cost( int npar , double *par )
{
   double fit , p,q ;
   double_pair hmp ;

   p = *par ; q = Gpeak / p ;

   hmp = gam_find_val( p , q , 0.5 ) ;
   return fabsf( hmp.b - hmp.a - Gfwhm ) ;
}

/*---------- This is the externally callable function ----------*/

double_pair gam_find_pq( double peak , double fwhm )
{
   double_pair result = {-1.0,-1.0} ;
   double p , q , pinit ;

   if( peak <= 0.0 || fwhm <= 0.0 ) return result ;

   Gpeak = peak ; Gfwhm = fwhm ;

   pinit = (2.3*peak/fwhm) ; pinit = pinit*pinit ;

   p = minimize_in_1D( 0.5*pinit , 1.5*pinit , gam_find_cost ) ;
   p = minimize_in_1D( 0.8*p     , 1.2*p     , gam_find_cost ) ;

   q = peak / p ;
   result.a = p ; result.b = q ; return result ;
}
