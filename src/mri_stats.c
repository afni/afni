/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** Not actually MRI processing routines -- just some statistics ***/

/*-----------------------------------------------------------------
  Student t-statistic:
    given threshold, compute 2-sided tail probability  ("t2p"), or
    given statistic, compute equivalent N(0,1) deviate ("t2z"), or
    given 2-sided tail probability, compute threshold  ("p2t").
-------------------------------------------------------------------*/

double student_p2t( double pp , double dof )
{
   double bb , binv , tt ;

   if( pp  <= 0.0      ) return 99.99 ;
   if( pp  >= 0.999999 ) return 0.0 ;
   if( dof < 1.0       ) return 0.0 ;

   bb   = lnbeta( 0.5*dof , 0.5 ) ;
   binv = incbeta_inverse( pp, 0.5*dof , 0.5 , bb ) ;
   tt   = sqrt( dof*(1.0/binv-1.0) ) ;
   return tt ;
}

double student_t2p( double tt , double dof )
{
   double bb , xx , pp ;

   if( tt <= 0.0 || dof < 1.0 ) return 1.0 ;

   bb = lnbeta( 0.5*dof , 0.5 ) ;
   xx = dof/(dof + tt*tt) ;
   pp = incbeta( xx , 0.5*dof , 0.5 , bb ) ;
   return pp ;
}

double student_t2z( double tt , double dof )
{
   static double bb , dof_old = -666.666 ;
   double xx , pp ;

   if( dof != dof_old ){
      bb      = lnbeta( 0.5*dof , 0.5 ) ;
      dof_old = dof ;
   }

   xx = dof/(dof + tt*tt) ;
   pp = incbeta( xx , 0.5*dof , 0.5 , bb ) ;

   if( tt > 0.0 ) pp = 1.0 - 0.5 * pp ;
   else           pp = 0.5 * pp ;

   xx = qginv(pp) ;
   return -xx ;
}

/*---------------------------------------------------------------------
   Correlation coefficient statistic:
     nsam = # of samples used to compute rho ( > nfit+nort )
     nfit = # of fitting parameters  ( >= 1 )
     nort = # of nuisance parameters ( >= 1 )
-----------------------------------------------------------------------*/

double correl_p2t( double pp , double nsam , double nfit , double nort )
{
   double bb , binv , rho ;

   if( pp <= 0.0      ) return 0.999 ;
   if( pp >= 0.999999 ) return 0.0 ;

   if( nsam <= nfit+nort || nfit < 1.0 || nort < 1.0 ) return 0.0 ;

   bb   = lnbeta( 0.5*nfit , 0.5*(nsam-nfit-nort) ) ;
   binv = incbeta_inverse( pp, 0.5*(nsam-nfit-nort) , 0.5*nfit , bb ) ;
   rho  = sqrt(1.0-binv) ;
   return rho ;
}

double correl_t2p( double rho , double nsam , double nfit , double nort )
{
   double bb , xx , pp ;

   if( rho <= 0.0 ||
       nsam <= nfit+nort || nfit < 1.0 || nort < 1.0 ) return 1.0 ;

   if( rho >= 0.9999999 ) return 0.0 ;

   bb   = lnbeta( 0.5*nfit , 0.5*(nsam-nfit-nort) ) ;
   xx   = 1.0 - rho*rho ;
   pp   = incbeta( xx , 0.5*(nsam-nfit-nort) , 0.5*nfit , bb ) ;
   return pp ;
}

/******************************/
/*** added this 17 Sep 1998 ***/

double correl_t2z( double rho , double nsam , double nfit , double nort )
{
   double pp , xx ;
   pp = 0.5 * correl_t2p( fabs(rho) , nsam , nfit , nort ) ;
   xx = qginv(pp) ;
   return ( (rho > 0) ? xx : -xx ) ;
}

/*----------------------------------------------------------
   Averaged t-statistic
------------------------------------------------------------*/

double studave_p2t( double pp , double dof , double nn )
{
   double ww , xx , gam2,gam4 , tt ;

   if( pp <= 0.0      ) return 99.99 ;
   if( pp >= 0.999999 ) return 0.0 ;

   if( dof < 6.01 || nn < 1.0 ) return 0.0 ;

   /* 4th and 6th order moments (or scaled cumulants) */

   gam2 =   6.0 / ( (dof-4.0) * nn ) ;
   gam4 = 240.0 / ( (dof-6.0) * (dof-4.0) * nn * nn ) ;

   /* Cornish-Fisher expansion */

   xx = qginv( 0.5 * pp ) ;  /* Gaussian approx */

   ww = xx + gam2 * xx * (                       xx*xx -  3.0) /  24.0
           + gam4 * xx * (    xx*xx*xx*xx - 10.0*xx*xx + 15.0) / 720.0
    - gam2 * gam2 * xx * (3.0*xx*xx*xx*xx - 24.0*xx*xx + 29.0) / 384.0 ;

   tt = sqrt( dof/(dof-2.0)/nn ) * ww ;
   return tt ;
}

double studave_t2p( double tt , double dof , double nn )
{
   static int nc = 0 ;
   if( nc < 9 ){
      fprintf(stderr,"*** studave_t2p: NOT IMPLEMENTED YET!\n") ; nc++ ;
   }
   return 0.0 ;
}

double studave_t2z( double tt , double dof , double nn )
{
   static int nc = 0 ;
   if( nc < 9 ){
      fprintf(stderr,"*** studave_t2z: NOT IMPLEMENTED YET!\n") ; nc++ ;
   }
   return 0.0 ;
}

/***********************************************************************/
/*** The routines below here are wrappers for the cdflib routines    ***/
/*** (cdf_*.c) from U Texas -- see file cdflib.txt for the details.  ***/
/***********************************************************************/

/*---------------------------------------------------------------
  F statistic: single sided
-----------------------------------------------------------------*/

double fstat_p2t( double pp , double dofnum , double dofden )
{
   int which , status ;
   double p , q , f , dfn , dfd , bound ;

   if( pp <= 0.0      ) return 999.99 ;
   if( pp >= 0.999999 ) return 0.0 ;

   which  = 2 ;
   p      = 1.0 - pp ;  /* 20 Jan 1999: p and q were switched! */
   q      = pp ;
   f      = 0.0 ;
   dfn    = dofnum ;
   dfd    = dofden ;

   cdff( &which , &p , &q , &f , &dfn , &dfd , &status , &bound ) ;

   if( status == 0 ) return f ;
   else              return 0.0 ;
}

double fstat_t2p( double ff , double dofnum , double dofden )
{
   int which , status ;
   double p , q , f , dfn , dfd , bound ;

   which  = 1 ;
   p      = 0.0 ;
   q      = 0.0 ;
   f      = ff ;
   dfn    = dofnum ;
   dfd    = dofden ;

   cdff( &which , &p , &q , &f , &dfn , &dfd , &status , &bound ) ;

   if( status == 0 ) return q ;
   else              return 1.0 ;
}

/******************************/
/*** added this 17 Sep 1998 ***/

double fstat_t2z( double ff , double dofnum , double dofden )
{
   double pp ;
   pp = 0.5 * fstat_t2p( ff , dofnum , dofden ) ;
   return qginv(pp) ;
}

/*---------------------------------------------------------------
  compute log of complete beta function, using the
  Unix math library's log gamma function.  If this is
  not available, see the end of this file.
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

   if( p <= ZERO || q <= ZERO ) return -1.0 ;  /* error! */

   if( x <= ZERO ) return ZERO ;
   if( x >= ONE  ) return ONE ;

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

/*******************************************************************/
/****    Given p, return x such that Q(x)=p, for 0 < p < 1.     ****/
/****    Q(x) = 1-P(x) = reversed cdf of N(0,1) variable.       ****/
/*******************************************************************/

double qg( double x ){ return 0.5*erfc(x/1.414213562373095); }

double log10qg( double x )
{
  double v = qg(x) ;
  if( v > 0.0 ) return log10(v) ;
  return -99.99 ;
}

double qginv( double p )
{
   double dp , dx , dt , ddq , dq ;
   int    newt ;                       /* not Gingrich, but Isaac */

   dp = (p <= 0.5) ? (p) : (1.0-p) ;   /* make between 0 and 0.5 */

   if( dp <= 1.e-37 ){
      dx = 13.0 ;                      /* 13 sigma has p < 10**(-38) */
      return ( (p <= 0.5) ? (dx) : (-dx) ) ;
   }

/**  Step 1:  use 26.2.23 from Abramowitz and Stegun **/

      dt = sqrt( -2.0 * log(dp) ) ;
      dx = dt
           - ((.010328*dt + .802853)*dt + 2.515517)
           /(((.001308*dt + .189269)*dt + 1.432788)*dt + 1.) ;

/**  Step 2:  do 3 Newton steps to improve this
              (uses the math library erfc function) **/

      for( newt=0 ; newt < 3 ; newt++ ){
         dq  = 0.5 * erfc( dx / 1.414213562373095 ) - dp ;
         ddq = exp( -0.5 * dx * dx ) / 2.506628274631000 ;
         dx  = dx + dq / ddq ;
      }

      if( dx > 13.0 ) dx = 13.0 ;
      return ( (p <= 0.5) ? (dx) : (-dx) ) ;  /* return with correct sign */
}

#ifdef NO_GAMMA
/***********************************************************************/
/****   Provide a ln(gamma(x)) function for stupid math libraries.  ****/
/****   This routine is not very efficient!  Don't use elsewhere.   ****/
/****   (cf. Abramowitz and Stegun, Eq. 6.1.36.)                    ****/
/***********************************************************************/

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

/** asymptotic expansion of ln(gamma(x)) for large positive x **/

#define LNSQRT2PI 0.918938533204672  /* ln(sqrt(2*PI)) */

double gamma_asympt(double x)
{
   double sum ;

   sum = (x-0.5)*log(x) - x + LNSQRT2PI + 1.0/(12.0*x) - 1./(360.0*x*x*x) ;
   return sum ;
}


/** log of gamma, argument positive (not very efficient!) **/

double gamma( double x )
{
   double w , g ;

   if( x <= 0.0 ){
      fprintf(stderr,"Internal gamma: argument %g <= 0\a\n",x) ;
      return 0.0 ;
   }

   if( x <  1.0 ) return gamma_12( x+1.0 ) - log(x) ;
   if( x <= 2.0 ) return gamma_12( x ) ;
   if( x >= 6.0 ) return gamma_asympt(x) ;

   g = 0 ; w = x ;
   while( w > 2.0 ){
      w -= 1.0 ; g += log(w) ;
   }
   return ( gamma_12(w) + g ) ;
}
#endif  /* NO_GAMMA */

/*---------------------------------------------------------------
  Compute double-sided tail probability for normal distribution.
-----------------------------------------------------------------*/

double normal_t2p( double zz )
{
   int which , status ;
   double p , q , x , mean,sd,bound ;

   if( zz <= 0.0 ) return 1.0 ;

   which  = 1 ;
   p      = 0.0 ;
   q      = 0.0 ;
   x      = zz ;
   mean   = 0.0 ;
   sd     = 1.0 ;

   cdfnor( &which , &p , &q , &x , &mean , &sd , &status , &bound ) ;

   if( status == 0 ) return 2.0*q ;  /* double sided prob = 2 times single sided */
   else              return 1.0 ;
}

/******************************/
/*** added this 17 Sep 1998 ***/

double normal_p2t( double qq )
{
   int which , status ;
   double p , q , x , mean,sd,bound ;

   if( qq <= 0.0      ) return 9.99 ;
   if( qq >= 0.999999 ) return 0.0 ;

   which  = 2 ;
   p      = 1.0 - 0.5 * qq ;
   q      = 0.5 * qq ;        /* single sided prob = 1/2 of double sided */
   x      = 0.0 ;
   mean   = 0.0 ;
   sd     = 1.0 ;

   cdfnor( &which , &p , &q , &x , &mean , &sd , &status , &bound ) ;
   return x ;
}

/*----------------------------------------------------------------
   Compute single-sided tail probability for Chi-square
------------------------------------------------------------------*/

double chisq_t2p( double xx , double dof )
{
   int which , status ;
   double p,q,x,df,bound ;

   which  = 1 ;
   p      = 0.0 ;
   q      = 0.0 ;
   x      = xx ;
   df     = dof ;

   cdfchi( &which , &p , &q , &x , &df , &status , &bound ) ;

   if( status == 0 ) return q ;
   else              return 1.0 ;
}

/******************************/
/*** added this 17 Sep 1998 ***/

double chisq_p2t( double qq , double dof )
{
   int which , status ;
   double p,q,x,df,bound ;

   if( qq <= 0.0      ) return 999.9 ;
   if( qq >= 0.999999 ) return 0.0 ;

   which  = 2 ;
   p      = 1.0 - qq ;
   q      = qq ;
   x      = 0.0 ;
   df     = dof ;

   cdfchi( &which , &p , &q , &x , &df , &status , &bound ) ;
   return x ;
}

double chisq_t2z( double xx , double dof )
{
   double pp ;
   pp = 0.5 * chisq_t2p( xx , dof ) ;
   return qginv(pp) ;
}

/*----------------------------------------------------------------
   Compute upper tail probability for incomplete beta distribution
------------------------------------------------------------------*/

double beta_t2p( double xx , double aa , double bb )
{
   int which , status ;
   double p,q,x,y,a,b,bound ;

   which  = 1 ;
   p      = 0.0 ;
   q      = 0.0 ;
   x      = xx ;
   y      = 1.0 - xx ;
   a      = aa ;
   b      = bb ;

   cdfbet( &which , &p , &q , &x , &y , &a , &b ,  &status , &bound ) ;

   if( status == 0 ) return q ;
   else              return 1.0 ;
}

/******************************/
/*** added this 17 Sep 1998 ***/

double beta_t2z( double xx , double aa , double bb )
{
   double pp ;
   pp = 0.5 * beta_t2p( xx , aa , bb ) ;
   return qginv(pp) ;
}

double beta_p2t( double qq , double aa , double bb )
{
   int which , status ;
   double p,q,x,y,a,b,bound ;

   if( qq <= 0.0      ) return 0.9999 ;
   if( qq >= 0.999999 ) return 0.0 ;

   which  = 2 ;
   p      = 1.0 - qq ;
   q      = qq ;
   x      = 0.0 ;
   y      = 1.0 ;
   a      = aa ;
   b      = bb ;

   cdfbet( &which , &p , &q , &x , &y , &a , &b ,  &status , &bound ) ;

   return x ;
}

/*----------------------------------------------------------------
   Compute upper tail probability for binomial distribution
   (that is, the probability that more than ss out of ntrial
    trials were successful).
------------------------------------------------------------------*/

double binomial_t2p( double ss , double ntrial , double ptrial )
{
   int which , status ;
   double p,q, s,xn,pr,ompr,bound ;

   which  = 1 ;
   p      = 0.0 ;
   q      = 0.0 ;
   s      = ss ;
   xn     = ntrial ;
   pr     = ptrial ;
   ompr   = 1.0 - ptrial ;

   cdfbin( &which , &p , &q , &s , &xn , &pr , &ompr , &status , &bound ) ;

   if( status == 0 ) return q ;
   else              return 1.0 ;
}

/******************************/
/*** added this 17 Sep 1998 ***/

double binomial_t2z( double ss , double ntrial , double ptrial )
{
   double pp ;
   pp = 0.5 * binomial_t2p( ss , ntrial , ptrial ) ;
   return qginv(pp) ;
}

double binomial_p2t( double qq , double ntrial , double ptrial )
{
   int which , status ;
   double p,q, s,xn,pr,ompr,bound ;

   if( qq <= 0.0      ) return 99.99 ;
   if( qq >= 0.999999 ) return 0.0 ;

   which  = 2 ;
   p      = 1.0 - qq ;
   q      = qq ;
   s      = 0.0 ;
   xn     = ntrial ;
   pr     = ptrial ;
   ompr   = 1.0 - ptrial ;

   cdfbin( &which , &p , &q , &s , &xn , &pr , &ompr , &status , &bound ) ;

   if( status == 0 ) return s ;
   else              return 0.0 ;
}

/*----------------------------------------------------------------
   Compute upper tail probability for the gamma distribution.
------------------------------------------------------------------*/

double gamma_t2p( double xx , double sh , double sc )
{
   int which , status ;
   double p,q, x,shape,scale,bound ;

   which  = 1 ;
   p      = 0.0 ;
   q      = 0.0 ;
   x      = xx ;
   shape  = sh ;
   scale  = sc ;

   cdfgam( &which , &p , &q , &x , &shape , &scale , &status , &bound ) ;

   if( status == 0 ) return q ;
   else              return 1.0 ;
}

/******************************/
/*** added this 17 Sep 1998 ***/

double gamma_t2z( double xx , double sh , double sc )
{
   double pp ;
   pp = 0.5 * gamma_t2p( xx , sh , sc ) ;
   return qginv(pp) ;
}

double gamma_p2t( double qq , double sh , double sc )
{
   int which , status ;
   double p,q, x,shape,scale,bound ;

   if( qq <= 0.0      ) return 999.9 ;
   if( qq >= 0.999999 ) return 0.0 ;

   which  = 2 ;
   p      = 1.0 - qq ;
   q      = qq ;
   x      = 0.0 ;
   shape  = sh ;
   scale  = sc ;

   cdfgam( &which , &p , &q , &x , &shape , &scale , &status , &bound ) ;

   return x ;
}

/*----------------------------------------------------------------
   Compute upper tail probability for the Poisson distribution
   (that is, the probability that the value is greater than xx).
------------------------------------------------------------------*/

double poisson_t2p( double xx , double lambda )
{
   int which , status ;
   double p,q, s,xlam,bound ;

   which  = 1 ;
   p      = 0.0 ;
   q      = 0.0 ;
   s      = xx ;
   xlam   = lambda ;

   cdfpoi( &which , &p , &q , &s , &xlam , &status , &bound ) ;

   if( status == 0 ) return q ;
   else              return 1.0 ;
}

/******************************/
/*** added this 17 Sep 1998 ***/

double poisson_t2z( double xx , double lambda )
{
   double pp ;
   pp = 0.5 * poisson_t2p( xx , lambda ) ;
   return qginv(pp) ;
}

double poisson_p2t( double qq , double lambda )
{
   int which , status ;
   double p,q, s,xlam,bound ;

   if( qq <= 0.0      ) return 999.9 ;
   if( qq >= 0.999999 ) return 0.0 ;

   which  = 2 ;
   p      = 1.0 - qq ;
   q      = qq ;
   s      = 0.0 ;
   xlam   = lambda ;

   cdfpoi( &which , &p , &q , &s , &xlam , &status , &bound ) ;

   return s ;
}
