#include "cdflib.h"
double erfc1(int *ind,double *x)
/*
-----------------------------------------------------------------------
         EVALUATION OF THE COMPLEMENTARY ERROR FUNCTION
 
          ERFC1(IND,X) = ERFC(X)            IF IND = 0
          ERFC1(IND,X) = EXP(X*X)*ERFC(X)   OTHERWISE
-----------------------------------------------------------------------
*/
{
static double c = .564189583547756e0;
static double a[5] = {
    .771058495001320e-04,-.133733772997339e-02,.323076579225834e-01,
    .479137145607681e-01,.128379167095513e+00
};
static double b[3] = {
    .301048631703895e-02,.538971687740286e-01,.375795757275549e+00
};
static double p[8] = {
    -1.36864857382717e-07,5.64195517478974e-01,7.21175825088309e+00,
    4.31622272220567e+01,1.52989285046940e+02,3.39320816734344e+02,
    4.51918953711873e+02,3.00459261020162e+02
};
static double q[8] = {
    1.00000000000000e+00,1.27827273196294e+01,7.70001529352295e+01,
    2.77585444743988e+02,6.38980264465631e+02,9.31354094850610e+02,
    7.90950925327898e+02,3.00459260956983e+02
};
static double r[5] = {
    2.10144126479064e+00,2.62370141675169e+01,2.13688200555087e+01,
    4.65807828718470e+00,2.82094791773523e-01
};
static double s[4] = {
    9.41537750555460e+01,1.87114811799590e+02,9.90191814623914e+01,
    1.80124575948747e+01
};
static int K1 = 1;
static double erfc1,ax,bot,e,t,top,w;
/*
     ..
     .. Executable Statements ..
*/
/*
                     ABS(X) .LE. 0.5
*/
    ax = fabs(*x);
    if(ax > 0.5e0) goto S10;
    t = *x**x;
    top = (((a[0]*t+a[1])*t+a[2])*t+a[3])*t+a[4]+1.0e0;
    bot = ((b[0]*t+b[1])*t+b[2])*t+1.0e0;
    erfc1 = 0.5e0+(0.5e0-*x*(top/bot));
    if(*ind != 0) erfc1 = exp(t)*erfc1;
    return erfc1;
S10:
/*
                  0.5 .LT. ABS(X) .LE. 4
*/
    if(ax > 4.0e0) goto S20;
    top = ((((((p[0]*ax+p[1])*ax+p[2])*ax+p[3])*ax+p[4])*ax+p[5])*ax+p[6])*ax+p[
      7];
    bot = ((((((q[0]*ax+q[1])*ax+q[2])*ax+q[3])*ax+q[4])*ax+q[5])*ax+q[6])*ax+q[
      7];
    erfc1 = top/bot;
    goto S40;
S20:
/*
                      ABS(X) .GT. 4
*/
    if(*x <= -5.6e0) goto S60;
    if(*ind != 0) goto S30;
    if(*x > 100.0e0) goto S70;
    if(*x**x > -exparg(&K1)) goto S70;
S30:
    t = pow(1.0e0/ *x,2.0);
    top = (((r[0]*t+r[1])*t+r[2])*t+r[3])*t+r[4];
    bot = (((s[0]*t+s[1])*t+s[2])*t+s[3])*t+1.0e0;
    erfc1 = (c-t*top/bot)/ax;
S40:
/*
                      FINAL ASSEMBLY
*/
    if(*ind == 0) goto S50;
    if(*x < 0.0e0) erfc1 = 2.0e0*exp(*x**x)-erfc1;
    return erfc1;
S50:
    w = *x**x;
    t = w;
    e = w-t;
    erfc1 = (0.5e0+(0.5e0-e))*exp(-t)*erfc1;
    if(*x < 0.0e0) erfc1 = 2.0e0-erfc1;
    return erfc1;
S60:
/*
             LIMIT VALUE FOR LARGE NEGATIVE X
*/
    erfc1 = 2.0e0;
    if(*ind != 0) erfc1 = 2.0e0*exp(*x**x);
    return erfc1;
S70:
/*
             LIMIT VALUE FOR LARGE POSITIVE X
                       WHEN IND = 0
*/
    erfc1 = 0.0e0;
    return erfc1;
} /* END */
