/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#ifndef PI
#  define PI 3.1415926536
#endif

#ifdef NO_GAMMA

/** log of gamma, for argument between 1 and 2 **/

double gamma_12( double y )
{
   double x , g ;
   x = y - 1.0 ;
   g = ((((((( 0.035868343 * x - 0.193527818 ) * x
                               + 0.482199394 ) * x
                               - 0.756704078 ) * x
                               + 0.918206857 ) * x
                               - 0.897056937 ) * x
                               + 0.988205891 ) * x
                               - 0.577191652 ) * x + 1.0 ;
   return log(g) ;
}

/** log of gamma, argument positive (not very efficient!) **/

double gamma( double x )
{
   double w , g ;

   if( x <= 0.0 ){
      fprintf(stderr,"Internal gamma: argument %g <= 0\a\n",x);exit(-1);
   }

   if( x <  1.0 ) return gamma_12( x+1.0 ) - log(x) ;
   if( x <= 2.0 ) return gamma_12( x ) ;

   g = 0 ; w = x ;
   while( w > 2.0 ){
      w -= 1.0 ; g += log(w) ;
   }
   return ( gamma_12(w) + g ) ;
}
#endif  /* NO_GAMMA */

#undef USE_OLD_METHOD

#ifdef USE_OLD_METHOD
   double stas4( double , double ) ;
   double stinv( double , double ) ;
#else
   double lnbeta( double , double ) ;
   double incbeta( double,double,double,double ) ;
   double incbeta_inverse( double,double,double,double ) ;
#endif

double qginv( double ) ;

void main( int argc , char * argv[] )
{
   double pp , dof , tt , bb , binv ;
   double nn,ll,mm ;
   int quiet = 0 ;

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ){
      printf("\n*** NOTE: This program has been superseded by program 'cdf' ***\n\n") ;

      printf("Usage #1: p2t p dof\n"
             "  where p   = double sided tail probability for t-distribution\n"
             "        dof = number of degrees of freedom to use\n"
             "  OUTPUT = t value that matches the input p\n"
             "\n"
             "Usage #2: p2t p N L M\n"
             "  where p   = double sided tail probability of beta distribution\n"
             "        N   = number of measured data points\n"
             "        L   = number of nuisance parameters (orts)\n"
             "        M   = number of fit parameters\n"
             "  OUTPUT = threshold for correlation coefficient\n"
             "\n"
             "Usage #3: p2t p\n"
             "  where p   = one sided tail probability of Gaussian distribution\n"
             "  OUTPUT = z value for which P(x>z) = p\n"
             "\n"
             "Usage #4: p2t p dof N\n"
             "  where p   = double sided tail probability for distribution of\n"
             "                the mean of N  iid zero-mean t-variables\n"
             "        dof = number of degrees of freedom of each t-variable\n"
             "        N   = number of t variables averaged\n"
             "  OUTPUT = threshold for the t average statistic\n"
             "  N.B.: The method used for this calculation is the Cornish-\n"
             "        Fisher expansion in N, and is only an approximation.\n"
             "        This also requires dof > 6, and the results will be\n"
             "        less accurate as dof approaches 6 from above!\n"
            ) ;
      exit(0) ;
   }

   if( strcmp(argv[1],"-q") == 0 ){
      argv++ ;
      argc-- ;
      quiet = 1 ;
      if( argc < 2 ) exit(0) ;
   }

   /** Gaussian statistic **/

   if( argc == 2 ){
      pp = strtod( argv[1] , NULL ) ;
      if( !quiet && (pp < 1.e-20 || pp >= 0.9999999) ){
         fprintf(stderr,"command line p value is illegal!\a\n") ; exit(-1) ;
      }
      tt = qginv(pp) ;
      if( quiet ) printf("%g\n",tt) ;
      else        printf("p = %g  z = %g\n",pp,tt) ;
   }

   /** t statistic **/

   else if( argc == 3 ){
      pp  = strtod( argv[1] , NULL ) ;
      dof = strtod( argv[2] , NULL ) ;
      if( !quiet && (pp <= 1.e-10 || pp >= 0.9999999) ){
         fprintf(stderr,"command line p value is illegal!\a\n") ; exit(-1) ;
      }
      if( dof <= 1.0 ){
         fprintf(stderr,"command line dof value is illegal!\a\n") ; exit(-1) ;
      }

#ifdef USE_OLD_METHOD
      tt = stinv( pp , dof ) ;
#else
      bb   = lnbeta( 0.5*dof , 0.5 ) ;
      binv = incbeta_inverse( pp, 0.5*dof , 0.5 , bb ) ;
      tt   = sqrt( dof*(1.0/binv-1.0) ) ;
#endif

      if( quiet ) printf("%g\n",tt) ;
      else        printf("p = %g   dof = %g   t = %g\n",pp,dof,tt) ;

   /** beta statistic **/

   } else if( argc == 5 ){
      pp  = strtod( argv[1] , NULL ) ;
      nn  = strtod( argv[2] , NULL ) ;
      ll  = strtod( argv[3] , NULL ) ;
      mm  = strtod( argv[4] , NULL ) ;
      if( !quiet && (pp <= 1.e-20 || pp >= 0.9999999) ){
         fprintf(stderr,"command line p value is illegal!\a\n") ; exit(-1) ;
      }
      if( mm < 1.0 || ll < 0.0 || nn-ll-mm < 1.0 ){
         fprintf(stderr,"command line N,L,M values are illegal!\a\n");exit(1) ;
      }
      bb   = lnbeta( 0.5*mm , 0.5*(nn-ll-mm) ) ;
      binv = incbeta_inverse( pp, 0.5*(nn-ll-mm) , 0.5*mm , bb ) ;
      tt   = sqrt(1.0-binv) ;
      if( quiet ) printf("%g\n",tt) ;
      else        printf("p = %g  N = %g  L = %g  M = %g  rho = %g\n",
                         pp,nn,ll,mm,tt) ;

   /** averaged t statistic **/

   } else if( argc == 4 ){
      double ww , xx , gam2,gam4 ;

      pp  = strtod( argv[1] , NULL ) ;
      dof = strtod( argv[2] , NULL ) ;
      nn  = strtod( argv[3] , NULL ) ;

      if( !quiet && (pp <= 1.e-10 || pp >= 0.9999999) ){
         fprintf(stderr,"command line p value is illegal!\a\n") ; exit(-1) ;
      }
      if( dof <= 6.01 || nn < 1.0 ){
         fprintf(stderr,"command line dof or N value is illegal!\a\n");exit(-1);
      }

      /* 4th and 6th order moments */

      gam2 =   6.0 / ( (dof-4.0) * nn ) ;
      gam4 = 240.0 / ( (dof-6.0) * (dof-4.0) * nn * nn ) ;

      /* Cornish-Fisher expansion */

      xx = qginv( 0.5 * pp ) ;  /* Gaussian approx */

      ww = xx + gam2 * xx * (                       xx*xx -  3.0) / 24.0
              + gam4 * xx * (    xx*xx*xx*xx - 10.0*xx*xx + 15.0) / 720.0
       - gam2 * gam2 * xx * (3.0*xx*xx*xx*xx - 24.0*xx*xx + 29.0) / 384.0 ;

      tt = sqrt( dof/(dof-2.0)/nn ) * ww ;

      if( quiet ) printf("%g\n",tt) ;
      else{
         printf("p = %g dof = %g N = %g 4-term t = %g",pp,dof,nn,tt) ;

         ww = xx + gam2 * xx * ( xx*xx - 3.0) / 24.0 ;
         tt = sqrt( dof/(dof-2.0)/nn ) * ww ;
         printf(" [2-term=%g",tt) ;
         ww = xx ;
         tt = sqrt( dof/(dof-2.0)/nn ) * ww ;
         printf(" 1-term=%g]\n",tt) ;
      }
   }

   exit(0) ;
}

#ifdef USE_OLD_METHOD
/*----------------------------------------------------------------------
   code for inverse of central t distribution
   Inputs: p  = double sided tail probability
           nu = degrees of freedom
   Output: T such that P( |t| > T ) = p

   This version is only good for nu >= 5, since it uses the
   approximations in Abramowitz and Stegun, Eq. 26.7.5 (p. 949).
------------------------------------------------------------------------*/

double stinv( double p , double nu )
{
   double xg , t4 ;
   xg = qginv(0.5*p) ;
   t4 = stas4( xg , nu ) ;
   return t4 ;
}

double stas4( double x , double nu)  /* this code generated by Maple */
{
   double t1,t2,t8,t9,t14,t17,t26,t34,t37 ;
   t1  = x*x;
   t2  = t1*x;
   t8  = t1*t1;
   t9  = t8*x;
   t14 = nu*nu;
   t17 = t8*t2;
   t26 = t8*t8;
   t34 = t14*t14;
   t37 = x+(t2/4+x/4)/nu
        +(5.0/96.0*t9+t2/6+x/32)/t14
        +(t17/128+19.0/384.0*t9
        +17.0/384.0*t2-5.0/128.0*x)/t14/nu
        +(79.0/92160.0*t26*x+97.0/11520.0*t17+247.0/15360.0*t9
                                          -t2/48-21.0/2048.0*x)/t34;
   return t37 ;
}
#endif   /* USE_OLD_METHOD */


#ifndef USE_OLD_METHOD

/*---------------------------------------------------------------
  compute log of complete beta function, using the
  Unix math library's log gamma function.  If this is
  not available, tough luck.
-----------------------------------------------------------------*/

double lnbeta( double p , double q )
{
   return (gamma(p) + gamma(q) - gamma(p+q)) ;
}

/*---------------------------------------------------------------
     TRANSLATED FROM THE ORIGINAL FORTRAN:
     algorithm as 63  appl. statist. (1973), vol.22, no.3

     computes incomplete beta function ratio for arguments
     x between zero and one, p and q positive.
     log of complete beta function, beta, is assumed to be known
-----------------------------------------------------------------*/

#define ZERO 0.0
#define ONE  1.0
#define ACU  1.0e-15

double incbeta( double x , double p , double q , double beta )
{
   double betain , psq , cx , xx,pp,qq , term,ai , temp , rx ;
   int indx , ns ;

   if( p <= ZERO || q <= ZERO || x < ZERO || x > ONE ) return -1.0 ;

   if( x == ZERO ) return ZERO ;
   if( x == ONE  ) return ONE ;

   /**  change tail if necessary and determine s **/

   psq = p+q ;
   cx  = ONE-x ;
   if(  p < psq*x ){
      xx   = cx ;
      cx   = x ;
      pp   = q ;
      qq   = p ;
      indx = 1 ;
   } else {
      xx   = x ;
      pp   = p ;
      qq   = q ;
      indx = 0 ;
   }

   term   = ONE ;
   ai     = ONE ;
   betain = ONE ;
   ns     = qq + cx*psq ;

   /** use soper's reduction formulae **/

      rx = xx/cx ;

lab3:
      temp = qq-ai ;
      if(ns == 0) rx = xx ;

lab4:
      term   = term*temp*rx/(pp+ai) ;
      betain = betain+term ;
      temp   = fabs(term) ;
      if(temp <= ACU && temp <= ACU*betain) goto lab5 ;

      ai = ai+ONE ;
      ns = ns-1 ;
      if(ns >= 0) goto lab3 ;
      temp = psq ;
      psq  = psq+ONE ;
      goto lab4 ;

lab5:
      betain = betain*exp(pp*log(xx)+(qq-ONE)*log(cx)-beta)/pp ;
      if(indx) betain=ONE-betain ;

   return betain ;
}

/*----------------------------------------------------------------
    algorithm as 109 appl. statist. (1977), vol.26, no.1
    (replacing algorithm as 64  appl. statist. (1973),
    vol.22, no.3)

    Remark AS R83 and the correction in vol40(1) p.236 have been
    incorporated in this version.

    Computes inverse of the incomplete beta function
    ratio for given positive values of the arguments
    p and q, alpha between zero and one.
    log of complete beta function, beta, is assumed to be known.
------------------------------------------------------------------*/


#define SAE   -15.0
#define TWO     2.0
#define THREE   3.0
#define FOUR    4.0
#define FIVE    5.0
#define SIX     6.0

#ifndef MAX
#  define MAX(a,b) (((a)<(b)) ? (b) : (a))
#  define MIN(a,b) (((a)>(b)) ? (b) : (a))
#endif


double incbeta_inverse( double alpha , double p , double q , double beta )
{
   int indx , iex ;
   double fpu , xinbta , a,pp,qq, r,y,t,s,h,w , acu ,
          yprev,prev,sq , g,adj,tx,xin ;

   fpu = pow(10.0,SAE) ;

   if( p <= ZERO || q <= ZERO || alpha < ZERO || alpha > ONE ) return -1.0 ;

   if( alpha == ZERO ) return ZERO ;
   if( alpha == ONE  ) return ONE ;

   /** change tail if necessary **/

   if( alpha > 0.5 ){
      a    = ONE-alpha ;
      pp   = q ;
      qq   = p ;
      indx = 1 ;
    } else {
      a    = alpha ;
      pp   = p ;
      qq   = q ;
      indx = 0 ;
   }

   /** calculate the initial approximation **/

lab2:
     r = sqrt(-log(a*a)) ;
     y = r - (2.30753 + 0.27061*r) / (ONE+(0.99229+0.04481*r)*r) ;
     if(pp > ONE && qq > ONE) goto lab5 ;

     r = qq+qq ;
     t = ONE/(9.0*qq) ;
     t = r * pow( (ONE-t+y*sqrt(t)) , 3.0 ) ;
     if( t <= ZERO ) goto lab3 ;

     t = (FOUR*pp+r-TWO)/t ;
     if( t <= ONE ) goto lab4 ;

     xinbta = ONE-TWO/(t+ONE) ; goto lab6 ;

lab3:
     xinbta = ONE-exp((log((ONE-a)*qq)+beta)/qq) ; goto lab6 ;

lab4:
     xinbta = exp((log(a*pp)+beta)/pp) ; goto lab6 ;

lab5:
     r = (y*y-THREE)/SIX ;
     s = ONE/(pp+pp-ONE) ;
     t = ONE/(qq+qq-ONE) ;
     h = TWO/(s+t) ;
     w = y*sqrt(h+r)/h-(t-s)*(r+FIVE/SIX-TWO/(THREE*h)) ;
     xinbta = pp/(pp+qq*exp(w+w)) ;

     /** solve for x by a modified newton-raphson method **/

lab6:
    r     = ONE-pp ;
    t     = ONE-qq ;
    yprev = ZERO ;
    sq    = ONE ;
    prev  = ONE ;
    if(xinbta < 0.0001) xinbta = 0.0001 ;
    if(xinbta > 0.9999) xinbta = 0.9999 ;

#if 0
    iex = -5.0 / (pp*pp) - 1.0/(a*a) - 13.0 ; if( iex < SAE ) iex = SAE ;
    acu = pow(10.0,iex) ;
#else
    acu = fpu ;
#endif

lab7:
      y = incbeta( xinbta , pp,qq,beta ) ;
      if( y < ZERO ) return -1.0 ;
      xin = xinbta ;
      y = (y-a)*exp(beta+r*log(xin)+t*log(ONE-xin)) ;
      if(y*yprev <= ZERO) prev = MAX(sq, fpu) ;
      g = ONE ;

lab9:
      adj = g*y ;
      sq = adj*adj ;
      if(sq >= prev) goto lab10 ;
      tx = xinbta-adj ;
      if(tx >= ZERO && tx <= ONE) goto lab11 ;

lab10:
      g = g/THREE ; goto lab9 ;

lab11:
      if(tx == ZERO  || tx == ONE ) goto lab10 ;
      if(prev <= acu || y*y <= acu || fabs(xinbta-tx) < fpu) goto lab12 ;
      xinbta = tx ;
      yprev = y ;
      goto lab7 ;

lab12:
      xinbta = tx ;
      if (indx) xinbta = ONE-xinbta ;
#if 0
printf("alpha = %g  incbeta = %g\n",alpha, incbeta(xinbta,p,q,beta) );
#endif
      return xinbta ;
}
#endif  /* USE_OLD_METHOD */

/*** given p, return x such that Q(x)=p, for 0 < p < 1 ***/

double qginv( double p )
{
   double dp , dx , dt , ddq , dq ;
   int    newt ;

   dp = (p <= 0.5) ? (p) : (1.0-p) ;   /* make between 0 and 0.5 */

   if( dp <= 0.0 ){
      dx = 13.0 ;
      return ( (p <= 0.5) ? (dx) : (-dx) ) ;
   }

/**  Step 1:  use 26.2.23 from Abramowitz and Stegun **/

      dt = sqrt( -2.0 * log(dp) ) ;
      dx = dt
           - ((.010328e+0*dt + .802853e+0)*dt + 2.525517e+0)
           /(((.001308e+0*dt + .189269e+0)*dt + 1.432788e+0)*dt + 1.e+0) ;

/**  Step 2:  do 3 Newton steps to improve this **/

      for( newt=0 ; newt < 3 ; newt++ ){
         dq  = 0.5e+0 * erfc( dx / 1.414213562373095e+0 ) - dp ;
         ddq = exp( -0.5e+0 * dx * dx ) / 2.506628274631000e+0 ;
         dx  = dx + dq / ddq ;
      }

      return ( (p <= 0.5) ? (dx) : (-dx) ) ;  /* return with correct sign */
}
