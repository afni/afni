#include "nifti1.h"   /* for the NIFTI_INTENT constants */
#include "cdflib.h"   /* for the Texas CDF library prototypes */

/*--------------------------------------------------------------------*/
/*!   Given p, return x such that Q(x)=p, for 0 < p < 1.
       - Q(x) = 1-P(x) = reversed cdf of N(0,1) variable.
       - Note erfc() is a Unix math library function.
       - If erfc() is not available on your system, try using the
         erfc1() function in cdf_54.c.
----------------------------------------------------------------------*/

double Qginv( double p )
{
   double dp , dx , dt , ddq , dq ;
   int    newt ;                       /* not Gingrich, but Isaac */

   dp = (p <= 0.5) ? (p) : (1.0-p) ;   /* make between 0 and 0.5 */

   if( dp <= 1.e-99 ){
     dx = 22.0 ;                      /* 22 sigma has p < 10**(-99) */
     return ( (p <= 0.5) ? (dx) : (-dx) ) ;
   }

/**  Step 1:  use 26.2.23 from Abramowitz and Stegun **/

   dt = sqrt( -2.0 * log(dp) ) ;
   dx = dt - ((.010328*dt + .802853)*dt + 2.515517)
            /(((.001308*dt + .189269)*dt + 1.432788)*dt + 1.) ;

/**  Step 2:  do 3 Newton steps to improve this
              (uses the math library erfc function) **/

   for( newt=0 ; newt < 3 ; newt++ ){
     dq  = 0.5 * erfc( dx / 1.414213562373095 ) - dp ;
     ddq = exp( -0.5 * dx * dx ) / 2.506628274631000 ;
     dx  = dx + dq / ddq ;
   }

   if( dx > 22.0 ) dx = 22.0 ;
   return ( (p <= 0.5) ? (dx) : (-dx) ) ;  /* return with correct sign */
}

/*--------------------------------------------------------------------*/
/*! Qg(x) = 1-P(x), where P = cdf for standard normal distribution.
----------------------------------------------------------------------*/

double Qg( double x ){ return 0.5*erfc(x/1.414213562373095); }

/*--------------------------------------------------------------------*/
/* log10Qg(x) = log10(Qg(x)) -- see above!
----------------------------------------------------------------------*/

double log10Qg( double x )
{
  double v = Qg(x) ;
  return (v > 0.0) ? log10(v) : -99.99 ;
}

/***********************************************************************/
/*** The routines below here are wrappers for the cdflib routines    ***/
/*** (cdf_*.c) from U Texas -- see file cdflib.txt for the details.  ***/
/***********************************************************************/

/*---------------------------------------------------------------
  F statistic: single sided (upper tail)
-----------------------------------------------------------------*/

double fstat_p2s( double pp , double dofnum , double dofden )
{
   int which , status ;
   double p , q , f , dfn , dfd , bound ;

   if( pp <= 0.0       ) return 9999.99 ;
   if( pp >= 0.9999999 ) return 0.0 ;

   which  = 2 ;
   p      = 1.0 - pp ;
   q      = pp ;
   f      = 0.0 ;
   dfn    = dofnum ;
   dfd    = dofden ;

   cdff( &which , &p , &q , &f , &dfn , &dfd , &status , &bound ) ;

   if( status == 0 ) return f ;
   else              return 0.0 ;
}

/*------------------------------*/

double fstat_s2p( double ff , double dofnum , double dofden )
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

/*------------------------------*/

double fstat_s2z( double ff , double dofnum , double dofden )
{
   double pp ;
   pp = 0.5 * fstat_s2p( ff , dofnum , dofden ) ;
   return Qginv(pp) ;
}

/*---------------------------------------------------------------
  noncentral F statistic: single sided (upper tail)
-----------------------------------------------------------------*/

double fnonc_p2s( double pp , double dofnum , double dofden , double nonc )
{
   int which , status ;
   double p , q , f , dfn , dfd , bound , pnonc ;

   if( pp <= 0.0       ) return 9999.99 ;
   if( pp >= 0.9999999 ) return 0.0 ;

   which  = 2 ;
   p      = 1.0 - pp ;
   q      = pp ;
   f      = 0.0 ;
   dfn    = dofnum ;
   dfd    = dofden ;
   pnonc  = nonc ;

   cdffnc( &which , &p , &q , &f , &dfn , &dfd , &pnonc , &status , &bound ) ;

   if( status == 0 ) return f ;
   else              return 0.0 ;
}

/*------------------------------*/

double fnonc_s2p( double ff , double dofnum , double dofden , double nonc )
{
   int which , status ;
   double p , q , f , dfn , dfd , bound , pnonc ;

   which  = 1 ;
   p      = 0.0 ;
   q      = 0.0 ;
   f      = ff ;
   dfn    = dofnum ;
   dfd    = dofden ;
   pnonc  = nonc ;

   cdffnc( &which , &p , &q , &f , &dfn , &dfd , &pnonc , &status , &bound ) ;

   if( status == 0 ) return q ;
   else              return 1.0 ;
}

/*------------------------------*/

double fnonc_s2z( double ff , double dofnum , double dofden , double nonc )
{
   double pp ;
   pp = 0.5 * fnonc_s2p( ff , dofnum , dofden , nonc ) ;
   return Qginv(pp) ;
}

/*---------------------------------------------------------------
  Compute double-sided tail probability for normal distribution.
-----------------------------------------------------------------*/

double normal_s2p( double zz )
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

/*------------------------------*/

double normal_p2s( double qq )
{
   int which , status ;
   double p , q , x , mean,sd,bound ;

   if( qq <= 0.0       ) return 99.99 ;
   if( qq >= 0.9999999 ) return 0.0 ;

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

double chisq_s2p( double xx , double dof )
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

/*------------------------------*/

double chisq_p2s( double qq , double dof )
{
   int which , status ;
   double p,q,x,df,bound ;

   if( qq <= 0.0       ) return 9999.9 ;
   if( qq >= 0.9999999 ) return 0.0 ;

   which  = 2 ;
   p      = 1.0 - qq ;
   q      = qq ;
   x      = 0.0 ;
   df     = dof ;

   cdfchi( &which , &p , &q , &x , &df , &status , &bound ) ;
   return x ;
}

/*------------------------------*/

double chisq_s2z( double xx , double dof )
{
   double pp ;
   pp = 0.5 * chisq_s2p( xx , dof ) ;
   return Qginv(pp) ;
}

/*----------------------------------------------------------------
   Compute single-sided tail probability for noncentral Chi-square
------------------------------------------------------------------*/

double chsqnonc_s2p( double xx , double dof , double nonc )
{
   int which , status ;
   double p,q,x,df,bound , pnonc ;

   which  = 1 ;
   p      = 0.0 ;
   q      = 0.0 ;
   x      = xx ;
   df     = dof ;
   pnonc  = nonc ;

   cdfchn( &which , &p , &q , &x , &df , &pnonc , &status , &bound ) ;

   if( status == 0 ) return q ;
   else              return 1.0 ;
}

/*------------------------------*/

double chsqnonc_p2s( double qq , double dof , double nonc )
{
   int which , status ;
   double p,q,x,df,bound , pnonc ;

   if( qq <= 0.0       ) return 9999.9 ;
   if( qq >= 0.9999999 ) return 0.0 ;

   which  = 2 ;
   p      = 1.0 - qq ;
   q      = qq ;
   x      = 0.0 ;
   df     = dof ;
   pnonc  = nonc ;

   cdfchn( &which , &p , &q , &x , &df , &pnonc , &status , &bound ) ;
   return x ;
}

/*------------------------------*/

double chsqnonc_s2z( double xx , double dof , double nonc )
{
   double pp ;
   pp = 0.5 * chsqnonc_s2p( xx , dof , nonc ) ;
   return Qginv(pp) ;
}

/*----------------------------------------------------------------
   Compute upper tail probability for incomplete beta distribution
------------------------------------------------------------------*/

double beta_s2p( double xx , double aa , double bb )
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

/*------------------------------*/

double beta_s2z( double xx , double aa , double bb )
{
   double pp ;
   pp = 0.5 * beta_s2p( xx , aa , bb ) ;
   return Qginv(pp) ;
}

/*------------------------------*/

double beta_p2s( double qq , double aa , double bb )
{
   int which , status ;
   double p,q,x,y,a,b,bound ;

   if( qq <= 0.0       ) return 0.9999 ;
   if( qq >= 0.9999999 ) return 0.0 ;

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

double binomial_s2p( double ss , double ntrial , double ptrial )
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

/*------------------------------*/

double binomial_s2z( double ss , double ntrial , double ptrial )
{
   double pp ;
   pp = 0.5 * binomial_s2p( ss , ntrial , ptrial ) ;
   return Qginv(pp) ;
}

/*------------------------------*/

double binomial_p2s( double qq , double ntrial , double ptrial )
{
   int which , status ;
   double p,q, s,xn,pr,ompr,bound ;

   if( qq <= 0.0       ) return 9999.99 ;
   if( qq >= 0.9999999 ) return 0.0 ;

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

double gamma_s2p( double xx , double sh , double sc )
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

/*------------------------------*/

double gamma_s2z( double xx , double sh , double sc )
{
   double pp ;
   pp = 0.5 * gamma_s2p( xx , sh , sc ) ;
   return Qginv(pp) ;
}

/*------------------------------*/

double gamma_p2s( double qq , double sh , double sc )
{
   int which , status ;
   double p,q, x,shape,scale,bound ;

   if( qq <= 0.0       ) return 9999.9 ;
   if( qq >= 0.9999999 ) return 0.0 ;

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

double poisson_s2p( double xx , double lambda )
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

/*------------------------------*/

double poisson_s2z( double xx , double lambda )
{
   double pp ;
   pp = 0.5 * poisson_s2p( xx , lambda ) ;
   return Qginv(pp) ;
}

/*------------------------------*/

double poisson_p2s( double qq , double lambda )
{
   int which , status ;
   double p,q, s,xlam,bound ;

   if( qq <= 0.0       ) return 999.9 ;
   if( qq >= 0.9999999 ) return 0.0 ;

   which  = 2 ;
   p      = 1.0 - qq ;
   q      = qq ;
   s      = 0.0 ;
   xlam   = lambda ;

   cdfpoi( &which , &p , &q , &s , &xlam , &status , &bound ) ;

   return s ;
}

/*----------------------------------------------------------------
   Compute double-sided tail probability for the t distribution;
   that is, twice the probability that t is above xx, for xx > 0.
------------------------------------------------------------------*/

double student_s2p( double xx , double dof )
{
   int which , status ;
   double p,q, s,xlam,bound ;

   if( xx <= 0.0 ) return 1.0 ;

   which  = 1 ;
   p      = 0.0 ;
   q      = 0.0 ;
   s      = xx ;
   xlam   = dof ;

   cdft( &which , &p , &q , &s , &xlam , &status , &bound ) ;

   if( status == 0 ) return 2.0*q ;
   else              return 1.0 ;
}

/*------------------------------*/

double student_s2z( double xx , double dof )
{
   double pp ;
   pp = 0.5 * student_s2p( xx , dof ) ;
   return Qginv(pp) ;
}

/*------------------------------*/

double student_p2s( double qq , double dof )
{
   int which , status ;
   double p,q, s,xlam,bound ;

   if( qq <= 0.0       ) return 999.9 ;
   if( qq >= 0.9999999 ) return 0.0 ;

   which  = 2 ;
   p      = 1.0 - 0.5 * qq ;
   q      = 0.5 * qq ;        /* single sided prob = 1/2 of double sided */
   s      = 0.0 ;
   xlam   = dof ;

   cdft( &which , &p , &q , &s , &xlam , &status , &bound ) ;

   return s ;
}

/*----------------------------------------------------------------
   Compute double sided probability for null correlation
   distribution; that is, the probability that abs(r) > rr,
   given that the true correlation is 0.
------------------------------------------------------------------*/

double correl_s2p( double rr , double dof )
{
   return beta_s2p( rr*rr , 0.5 , 0.5*dof ) ;
}

/*------------------------------*/

double correl_p2s( double qq , double dof )
{
   double bb = beta_p2s( qq , 0.5 , 0.5*dof ) ;
   return sqrt(bb) ;
}

/*------------------------------*/

double correl_s2z( double xx , double dof )
{
   double pp ;
   pp = 0.5 * correl_s2p( xx , dof ) ;
   return Qginv(pp) ;
}

/*----------------------------------------------------------------
  Compute the upper tail probability for a U(0,1) distribution.
------------------------------------------------------------------*/

double uniform_s2p( double xx )
{
   if( xx <= 0.0 ) return 1.0 ;
   if( xx >= 1.0 ) return 0.0 ;
   return 1.0-xx ;
}

/*------------------------------*/

double uniform_p2s( double qq )
{
   if( qq <= 0.0       ) return 1.0 ;
   if( qq >= 0.9999999 ) return 0.0 ;
   return 1.0-qq ;
}

/*------------------------------*/

double uniform_s2z( double xx )
{
   double pp ;
   pp = 0.5 * uniform_s2p( xx ) ;
   return Qginv(pp) ;
}

/*----------------------------------------------------------------
  Compute the double sided probability for a standard logistic
  distribution
------------------------------------------------------------------*/

double logistic_s2p( double xx )
{
   return (xx <= 0.0) ? 1.0 : 1.0-tanh(xx) ;
}

double logistic_p2s( double qq )
{
   if( qq <= 0.0       ) return 99.99 ;
   if( qq >= 0.9999999 ) return 0.0 ;
   return atanh(1.0-qq) ;
}

double logistic_s2z( double ss )
{
   double pp ;
   pp = 0.5 * logistic_s2p( ss ) ;
   return Qginv(pp) ;
}

/*----------------------------------------------------------------
  Compute the double sided probability for a standard Laplace
  distribution
------------------------------------------------------------------*/

double laplace_s2p( double xx )
{
   return (xx <= 0.0) ? 1.0 : exp(-xx) ;
}

double laplace_p2s( double qq )
{
   if( qq <= 0.0       ) return 99.99 ;
   if( qq >= 0.9999999 ) return 0.0 ;
   return -log(qq) ;
}

double laplace_s2z( double ss )
{
   double pp ;
   pp = 0.5 * laplace_s2p( ss ) ;
   return Qginv(pp) ;
}

/*----------------------------------------------------------------
   Compute double sided tail probability for the
   noncentral t distribution
------------------------------------------------------------------*/

double tnonc_s2p( double xx , double dof , double nonc )
{
   return (xx <= 0.0) ? 1.0 : fnonc_s2p( xx*xx , 1.0 , dof , nonc ) ;
}

/*------------------------------*/

double tnonc_s2z( double xx , double dof , double nonc )
{
   double pp ;
   pp = 0.5 * tnonc_s2p( xx , dof , nonc ) ;
   return Qginv(pp) ;
}

/*------------------------------*/

double tnonc_p2s( double qq , double dof , double nonc )
{
   double xx ;
   if( qq <= 0.0       ) return 9999.9 ;
   if( qq >= 0.9999999 ) return 0.0 ;
   xx = fnonc_p2s( qq , 1.0 , dof , nonc ) ;
   return sqrt(xx) ;
}

/*----------------------------------------------------------------
   Compute double sided tail probability for the chi distribution
------------------------------------------------------------------*/

double chi_s2p( double xx , double dof )
{
   return (xx <= 0.0) ? 1.0 : chisq_s2p( sqrt(xx) , dof ) ;
}

/*------------------------------*/

double chi_s2z( double xx , double dof )
{
   double pp ;
   pp = 0.5 * chi_s2p( xx , dof ) ;
   return Qginv(pp) ;
}

/*------------------------------*/

double chi_p2s( double qq , double dof )
{
   double xx ;
   if( qq <= 0.0       ) return 9999.9 ;
   if( qq >= 0.9999999 ) return 0.0 ;
   xx = chisq_p2s( qq , dof ) ;
   return sqrt(xx) ;
}

/*--------------------------------------------------------------------------*/
/*! Given a statistical value, convert it to a p-value.  This may be
    a one-sided or two-sided value, depending on the distribution.
      - val      = statistic
      - code     = NIFTI_INTENT_* statistical code
      - p1,p2,p3 = parameters of the distribution

    If the inputs are grossly illegal, a value of -1.0 is returned.
----------------------------------------------------------------------------*/

double nifti_stat2pval( double val, int code, double p1,double p2,double p3 )
{
   double pval = -1.0 ;   /* error flag */

   switch( code ){

     case NIFTI_INTENT_CORREL:     pval = correl_s2p  ( val, p1 )      ; break; /**/
     case NIFTI_INTENT_TTEST:      pval = student_s2p ( val, p1 )      ; break; /**/
     case NIFTI_INTENT_FTEST:      pval = fstat_s2p   ( val, p1,p2 )   ; break; /**/
     case NIFTI_INTENT_ZSCORE:     pval = normal_s2p  ( val )          ; break; /**/
     case NIFTI_INTENT_CHISQ:      pval = chisq_s2p   ( val, p1 )      ; break; /**/
     case NIFTI_INTENT_BETA:       pval = beta_s2p    ( val, p1,p2 )   ; break; /**/
     case NIFTI_INTENT_BINOM:      pval = binomial_s2p( val, p1,p2 )   ; break; /**/
     case NIFTI_INTENT_GAMMA:      pval = gamma_s2p   ( val, p1,p2 )   ; break; /**/
     case NIFTI_INTENT_POISSON:    pval = poisson_s2p ( val, p1 )      ; break; /**/
     case NIFTI_INTENT_FTEST_NONC: pval = fnonc_s2p   ( val, p1,p2,p3 ); break; /**/
     case NIFTI_INTENT_CHISQ_NONC: pval = chsqnonc_s2p( val, p1,p2    ); break; /**/
     case NIFTI_INTENT_TTEST_NONC: pval = tnonc_s2p   ( val, p1,p2 )   ; break; /**/

     case NIFTI_INTENT_WEIBULL:    pval = weibull_s2p ( val, p1,p2,p3 ); break;
     case NIFTI_INTENT_CHI:        pval = chi_s2p     ( val, p1 )      ; break; /**/
     case NIFTI_INTENT_INVGAUSS:   pval = invgauss_s2p( val, p1,p2 )   ; break;
     case NIFTI_INTENT_EXTVAL:     pval = extval1_s2p ( val, p1,p2 )   ; break;

     /* these distributions are shifted and scaled copies of a standard case */

     case NIFTI_INTENT_NORMAL:
                    if( p2 > 0.0 ) pval = normal_s2p  ( (val-p1)/p2 )  ; break; /**/

     case NIFTI_INTENT_LOGISTIC:
                    if( p2 > 0.0 ) pval = logistic_s2p( (val-p1)/p2 )  ; break; /**/

     case NIFTI_INTENT_LAPLACE:
                    if( p2 > 0.0 ) pval = laplace_s2p ( (val-p1)/p2 )  ; break; /**/

     case NIFTI_INTENT_UNIFORM:
                    if( p2 > p1  ) pval = uniform_s2p((val-p1)/(p2-p1)); break; /**/

     case NIFTI_INTENT_PVAL:       pval = val                          ; break; /**/
   }

   return pval ;
}
