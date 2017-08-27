/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _RWCOX_PARSER_HEADER_
#define _RWCOX_PARSER_HEADER_

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

#define MAX_PARCODE 9999

typedef struct {
   int num_code ;
   char c_code[8*MAX_PARCODE] ;
} PARSER_code ;

extern void PARSER_set_printout( int ) ;

extern PARSER_code * PARSER_generate_code( char * ) ;

extern double PARSER_evaluate_one( PARSER_code *, double atoz[] ) ;

extern void PARSER_evaluate_vector( PARSER_code * pc, double  * atoz[],
                                    int nv, double vout[] ) ;

extern int PARSER_has_symbol( char * sym , PARSER_code * pc ) ;
extern void PARSER_mark_symbols( PARSER_code * pc , int * sl ) ;

extern int PARSER_1deval( char *, int, float, float, float * ) ; /* 17 Nov 1999 */
extern int PARSER_1dtran( char *, int, float * ) ;               /* 16 Jun 2009 */

extern double PARSER_strtod( char *expr ) ;

extern float * PARSER_fitter( int nval, float *indval, float *depval,   /* 26 Jan 2016 */
                       char *expr, char *indet,
                       float *parbot, float *partop, float *parout, int meth, float *wtar ) ;

#ifdef  __cplusplus
}
#endif

#ifdef NEED_PARSER_INTERNALS
#include "converted_from_fortran.h"

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

extern int parser_(char *c_expr__, logical *l_print__,
                   integer * num_code__, char *c_code__,
                   ftnlen c_expr_len, ftnlen c_code_len) ;

extern doublereal pareval_(integer *num_code__, char *c_code__,
                           doublereal *r8val, ftnlen c_code_len) ;

extern int parevec_(integer *num_code__, char *c_code__,
                    doublereal *va, doublereal *vb, doublereal *vc,
          doublereal *vd, doublereal *ve, doublereal *vf,
          doublereal *vg, doublereal *vh, doublereal *vi,
          doublereal *vj, doublereal *vk, doublereal *vl,
          doublereal *vm, doublereal *vn, doublereal *vo,
          doublereal *vp, doublereal *vq, doublereal *vr,
          doublereal *vs, doublereal *vt, doublereal *vu,
          doublereal *vv, doublereal *vw, doublereal *vx,
          doublereal *vy, doublereal *vz, integer *lvec ,
          doublereal *vout, ftnlen c_code_len) ;

extern doublereal dbesj0_( doublereal * ) ;
extern doublereal dbesj1_( doublereal * ) ;
extern doublereal dbesy0_( doublereal * ) ;
extern doublereal dbesy1_( doublereal * ) ;

extern doublereal derf_ ( doublereal * ) ;
extern doublereal derfc_( doublereal * ) ;

extern doublereal unif_( doublereal * ) ;

#ifdef  __cplusplus
}
#endif

#endif /* NEED_PARSER_INTERNALS */

/*-------------------------------------------------------------------------*/

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

#undef  PARSER_HELP_STRING
#define PARSER_HELP_STRING                                                     \
  " Arithmetic expressions are allowed, using + - * / ** ^ and parentheses.\n" \
  " C relational, boolean, and conditional expressions are NOT implemented!\n" \
  "* Note that the expression evaluator is designed not to fail;  illegal  *\n" \
  "* operations like 'sqrt(-1)' are changed to legal ones to avoid crashes.*\n" \
  " Built in functions include:\n"                                             \
  "\n"                                                                         \
  "    sin  , cos  , tan  , asin  , acos  , atan  , atan2,       \n"           \
  "    sinh , cosh , tanh , asinh , acosh , atanh , exp  ,       \n"           \
  "    log  , log10, abs  , int   , sqrt  , max   , min  ,       \n"           \
  "    J0   , J1   , Y0   , Y1    , erf   , erfc  , qginv, qg ,  \n"           \
  "    rect , step , astep, bool  , and   , or    , mofn ,       \n"           \
  "    sind , cosd , tand , median, lmode , hmode , mad  ,       \n"           \
  "    gran , uran , iran , eran  , lran  , orstat, mod  ,       \n"           \
  "    mean , stdev, sem  , Pleg  , cbrt  , rhddc2, hrfbk4,hrfbk5\n"           \
  "    minabove, maxbelow, extreme, absextreme    , acfwxm\n"                  \
  "    gamp , gampq\n"                                                         \
  "\n"                                                                         \
  " where some of the less obvious funcions are:\n"                            \
  " * qg(x)    = reversed cdf of a standard normal distribution\n"             \
  " * qginv(x) = inverse function to qg\n"                                     \
  " * min, max, atan2 each take 2 arguments ONLY\n"                            \
  " * J0, J1, Y0, Y1 are Bessel functions (see the holy book: Watson)\n"       \
  " * Pleg(m,x) is the m'th Legendre polynomial evaluated at x\n"              \
  " * erf, erfc are the error and complementary error functions\n"             \
  " * sind, cosd, tand take arguments in degrees (vs. radians)\n"              \
  " * median(a,b,c,...) computes the median of its arguments\n"                \
  " * mad(a,b,c,...) computes the MAD of its arguments\n"                      \
  " * mean(a,b,c,...) computes the mean of its arguments\n"                    \
  " * stdev(a,b,c,...) computes the standard deviation of its arguments\n"     \
  " * sem(a,b,c,...) computes standard error of the mean of its arguments,\n"  \
  "                  where sem(n arguments) = stdev(same)/sqrt(n)\n"           \
  " * orstat(n,a,b,c,...) computes the n-th order statistic of\n"              \
  "    {a,b,c,...} - that is, the n-th value in size, starting\n"              \
  "    at the bottom (e.g., orstat(1,a,b,c) is the minimum)\n"                 \
  " * minabove(X,a,b,c,...) computes the smallest value amongst {a,b,c,...}\n" \
  "    that is LARGER than the first argument X; if all values are smaller\n"  \
  "    than X, then X will be returned\n"                                      \
  " * maxbelow(X,a,b,c,...) similarly returns the largest value amongst\n"     \
  "    {a,b,c,...} that is SMALLER than the first argument X.\n"               \
  " * extreme(a,b,c,...) finds the largest absolute value amongst\n"           \
  "    {a,b,c,...} returning one of the original a,b,c,... values.\n"          \
  " * absextreme(a,b,c,...) finds the largest absolute value amongst\n"        \
  "    {a,b,c,...} returning the maximum absolute value of a,b,c,... values.\n"\
  " * lmode(a,b,c,...) and hmode(a,b,c,...) compute the mode\n"                \
  "    of their arguments - lmode breaks ties by choosing the\n"               \
  "    smallest value with the maximal count, hmode breaks ties by\n"          \
  "    choosing the largest value with the maximal count\n"                    \
  "    [\"a,b,c,...\" indicates a variable number of arguments]\n"             \
  " * gran(m,s) returns a Gaussian deviate with mean=m, stdev=s\n"             \
  " * uran(r)   returns a uniform deviate in the range [0,r]\n"                \
  " * iran(t)   returns a random integer in the range [0..t]\n"                \
  " * eran(s)   returns an exponentially distributed deviate\n"                \
  "               with parameter s; mean=s\n"                                  \
  " * lran(t)   returns a logistically distributed deviate\n"                  \
  "               with parameter t; mean=0, stdev=t*1.814\n"                   \
  " * mod(a,b)  returns (a modulo b) = a - b*int(a/b)\n"                       \
  " * hrfbk4(t,L) and hrfbk5(t,L) are the BLOCK4 and BLOCK5 hemodynamic\n"     \
  "    response functions from 3dDeconvolve (L=stimulus duration in sec,\n"    \
  "    and t is the time in sec since start of stimulus); for example:\n"      \
  " 1deval -del 0.1 -num 400 -expr 'hrfbk5(t-2,20)' | 1dplot -stdin -del 0.1\n"\
  "    These HRF functions are scaled to return values in the range [0..1]\n"  \
  "\n"                                                                         \
  " * ACFWXM(a,b,c,x) returns the Full Width at X Maximum for the mixed\n"     \
  "   model ACF function\n"                                                    \
  "     f(r) = a*expr(-r*r/(2*b*b))+(1-a)*exp(-r/c)\n"                         \
  "   for X between 0 and 1 (not inclusive).  This is the model function\n"    \
  "   estimated in program 3dFWHMx.\n"                                         \
  " * gamp(peak,fwhm) returns the parameter p in the formula\n"                \
  "      g(t) = (t/(p*q))^p * exp(p-t/q)\n"                                    \
  "   that gives the peak value of g(t) occuring at t=peak when the\n"         \
  "   FWHM of g(t) is given by fwhm; gamq(peak,fwhm) gives the q parameter.\n" \
  "   These functions are largely used for creating FMRI hemodynamic shapes.\n"\
  "\n"                                                                         \
  " You may use the symbol 'PI' to refer to the constant of that name.\n"      \
  " This is the only 2 letter symbol defined; all variables are\n"             \
  " referred to by 1 letter symbols.  The case of the expression is\n"         \
  " ignored (in fact, it is converted to uppercase as the first step\n"        \
  " in the parsing algorithm).\n"                                              \
  "\n"                                                                         \
  " The following functions are designed to help implement logical\n"          \
  " functions, such as masking of 3D volumes against some criterion:\n"        \
  "       step(x)    = {1 if x>0           , 0 if x<=0},\n"                    \
  "       posval(x)  = {x if x>0           , 0 if x<=0},\n"                    \
  "       astep(x,y) = {1 if abs(x) > y    , 0 otherwise} = step(abs(x)-y)\n"  \
  "  within(x,MI,MX) = {1 if MI <= x <= MX , 0 otherwise},\n"                  \
  "       rect(x)    = {1 if abs(x)<=0.5, 0 if abs(x)>0.5},\n"                 \
  "       bool(x)    = {1 if x != 0.0   , 0 if x == 0.0},\n"                   \
  "    notzero(x)    = bool(x),\n"                                             \
  "     iszero(x)    = 1-bool(x) = { 0 if x != 0.0, 1 if x == 0.0 },\n"        \
  "        not(x)    = same as iszero(x)\n"                                    \
  "     equals(x,y)  = 1-bool(x-y) = { 1 if x == y , 0 if x != y },\n"         \
  "   ispositive(x)  = { 1 if x > 0; 0 if x <= 0 },\n"                         \
  "   isnegative(x)  = { 1 if x < 0; 0 if x >= 0 },\n"                         \
  "   ifelse(x,t,f)  = { t if x != 0; f if x == 0 },\n"                        \
  "        not(x)    = same as iszero(x) = Boolean negation\n"                 \
  "   and(a,b,...,c) = {1 if all arguments are nonzero, 0 if any are zero}\n"  \
  "    or(a,b,...,c) = {1 if any arguments are nonzero, 0 if all are zero}\n"  \
  "  mofn(m,a,...,c) = {1 if at least 'm' arguments are nonzero, else 0 }\n"   \
  "  argmax(a,b,...) = index of largest argument; = 0 if all args are 0\n"     \
  "  argnum(a,b,...) = number of nonzero arguments\n"                          \
  "  pairmax(a,b,...)= finds the 'paired' argument that corresponds to the\n"  \
  "                    maximum of the first half of the input arguments;\n"    \
  "                    for example, pairmax(a,b,c,p,q,r) determines which\n"   \
  "                    of {a,b,c} is the max, then returns corresponding\n"    \
  "                    value from {p,q,r}; requires even number of args.\n"    \
  "  pairmin(a,b,...)= Similar to pairmax, but for minimum; for example,\n"    \
  "                    pairmin(a,b,c,p,q,r} finds the minimum of {a,b,c}\n"    \
  "                    and returns the corresponding value from {p,q,r};\n"    \
  "                      pairmin(3,2,7,5,-1,-2,-3,-4) = -2\n"                  \
  "                    (The 'pair' functions are Lukas Pezawas specials!)\n"   \
  "  amongst(a,b,...)= Return value is 1 if any of the b,c,... values\n"       \
  "                    equals the a value; otherwise, return value is 0.\n"    \
  " choose(n,a,b,...)= chooses the n-th value from the a,b,... values.\n"      \
  "                    (e.g., choose(2,a,b,c) is b)\n"                         \
  "\n"                                                                         \
  "  [These last 9 functions take a variable number of arguments.]\n"          \
  "\n"                                                                         \
  " The following 27 functions are used for statistical conversions,\n"        \
  " as in the program 'cdf':\n"                                                \
  "   fico_t2p(t,a,b,c), fico_p2t(p,a,b,c), fico_t2z(t,a,b,c),\n"              \
  "   fitt_t2p(t,a)    , fitt_p2t(p,a)    , fitt_t2z(t,a)    ,\n"              \
  "   fift_t2p(t,a,b)  , fift_p2t(p,a,b)  , fift_t2z(t,a,b)  ,\n"              \
  "   fizt_t2p(t)      , fizt_p2t(p)      , fizt_t2z(t)      ,\n"              \
  "   fict_t2p(t,a)    , fict_p2t(p,a)    , fict_t2z(t,a)    ,\n"              \
  "   fibt_t2p(t,a,b)  , fibt_p2t(p,a,b)  , fibt_t2z(t,a,b)  ,\n"              \
  "   fibn_t2p(t,a,b)  , fibn_p2t(p,a,b)  , fibn_t2z(t,a,b)  ,\n"              \
  "   figt_t2p(t,a,b)  , figt_p2t(p,a,b)  , figt_t2z(t,a,b)  ,\n"              \
  "   fipt_t2p(t,a)    , fipt_p2t(p,a)    , fipt_t2z(t,a)    .\n"              \
  "\n"                                                                         \
  " See the output of 'cdf -help' for documentation on the meanings of\n"      \
  " and arguments to these functions.  The two functions below use the\n"      \
  " NIfTI-1 statistical codes to map between statistical values and\n"         \
  " cumulative distribution values:\n"                                         \
  "   cdf2stat(val,code,p1,p2,p3) -- val is between 0 and 1\n"                 \
  "   stat2cdf(val,code,p1,p2,p3) -- val is legal for the given distribution\n"\
  " where code is\n"                                                           \
  "   2 = correlation statistic     p1 = DOF\n"                                \
  "   3 = t statistic (central)     p1 = DOF\n"                                \
  "   4 = F statistic (central)     p1 = num DOF, p2 = den DOF\n"              \
  "   5 = N(0,1) statistic          no parameters (p1=p2=p3=0)\n"              \
  "   6 = Chi-squared (central)     p1 = DOF\n"                                \
  "   7 = Beta variable (central)   p1 = a , p2 = b\n"                         \
  "   8 = Binomial variable         p1 = #trials, p2 = prob per trial\n"       \
  "   9 = Gamma distribution        p1 = shape, p2 = scale\n"                  \
  "  10 = Poisson distribution      p1 = mean\n"                               \
  "  11 = N(mu,variance) normal     p1 = mean, p2 = scale\n"                   \
  "  12 = noncentral F statistic    p1 = num DOF, p2 = den DOF, p3 = noncen\n" \
  "  13 = noncentral chi-squared    p1 = DOF, p2 = noncentrality parameter\n"  \
  "  14 = Logistic distribution     p1 = mean, p2 = scale\n"                   \
  "  15 = Laplace distribution      p1 = mean, p2 = scale\n"                   \
  "  16 = Uniform distribution      p1 = min, p2 = max\n"                      \
  "  17 = noncentral t statistic    p1 = DOF, p2 = noncentrality parameter\n"  \
  "  18 = Weibull distribution      p1 = location, p2 = scale, p3 = power\n"   \
  "  19 = Chi statistic (central)   p1 = DOF\n"                                \
  "  20 = inverse Gaussian variable p1 = mu, p2 = lambda\n"                    \
  "  21 = Extreme value type I      p1 = location, p2 = scale\n"               \
  "  22 = 'p-value'                 no parameters\n"                           \
  "  23 = -ln(p)                    no parameters\n"                           \
  "  24 = -log10(p)                 no parameters\n"                           \
  "When fewer than 3 parameters are needed, the values for later parameters\n" \
  "are still required, but will be ignored.  An extreme case is code=5,\n"     \
  "where the correct call is (e.g.) cdf2stat(p,5,0,0,0)\n"                     \
  "\n"                                                                         \
  "Finally, note that the expression evaluator is designed not to crash, or\n" \
  "to return NaN or Infinity.  Illegal operations, such as division by 0,\n"   \
  "logarithm of negative value, etc., are intercepted and something else\n"    \
  "(usually 0) will be returned.  To find out what that 'something else'\n"    \
  "is in any specific case, you should play with the ccalc program.\n"

#ifdef  __cplusplus
}
#endif

#endif /* _RWCOX_PARSER_HEADER_ */
