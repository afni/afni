#include "cdflib.h"
double alngam(double *x)
/*
**********************************************************************
 
     double alngam(double *x)
                 double precision LN of the GAMma function
 
 
                              Function
 
 
     Returns the natural logarithm of GAMMA(X).
 
 
                              Arguments
 
 
     X --> value at which scaled log gamma is to be returned
                    X is DOUBLE PRECISION
 
 
                              Method
 
 
     If X .le. 6.0, then use recursion to get X below 3
     then apply rational approximation number 5236 of
     Hart et al, Computer Approximations, John Wiley and
     Sons, NY, 1968.
 
     If X .gt. 6.0, then use recursion to get X to at least 12 and
     then use formula 5423 of the same source.
 
**********************************************************************
*/
{
#define hln2pi 0.91893853320467274178e0
static double coef[5] = {
    0.83333333333333023564e-1,-0.27777777768818808e-2,0.79365006754279e-3,
    -0.594997310889e-3,0.8065880899e-3
};
static double scoefd[4] = {
    0.62003838007126989331e2,0.9822521104713994894e1,-0.8906016659497461257e1,
    0.1000000000000000000e1
};
static double scoefn[9] = {
    0.62003838007127258804e2,0.36036772530024836321e2,0.20782472531792126786e2,
    0.6338067999387272343e1,0.215994312846059073e1,0.3980671310203570498e0,
    0.1093115956710439502e0,0.92381945590275995e-2,0.29737866448101651e-2
};
static int K1 = 9;
static int K3 = 4;
static int K5 = 5;
static double alngam,offset,prod,xx;
static int i,n;
static double T2,T4,T6;
/*
     ..
     .. Executable Statements ..
*/
    if(!(*x <= 6.0e0)) goto S70;
    prod = 1.0e0;
    xx = *x;
    if(!(*x > 3.0e0)) goto S30;
S10:
    if(!(xx > 3.0e0)) goto S20;
    xx -= 1.0e0;
    prod *= xx;
    goto S10;
S30:
S20:
    if(!(*x < 2.0e0)) goto S60;
S40:
    if(!(xx < 2.0e0)) goto S50;
    prod /= xx;
    xx += 1.0e0;
    goto S40;
S60:
S50:
    T2 = xx-2.0e0;
    T4 = xx-2.0e0;
    alngam = devlpl(scoefn,&K1,&T2)/devlpl(scoefd,&K3,&T4);
/*
     COMPUTE RATIONAL APPROXIMATION TO GAMMA(X)
*/
    alngam *= prod;
    alngam = log(alngam);
    goto S110;
S70:
    offset = hln2pi;
/*
     IF NECESSARY MAKE X AT LEAST 12 AND CARRY CORRECTION IN OFFSET
*/
    n = fifidint(12.0e0-*x);
    if(!(n > 0)) goto S90;
    prod = 1.0e0;
    for(i=1; i<=n; i++) prod *= (*x+(double)(i-1));
    offset -= log(prod);
    xx = *x+(double)n;
    goto S100;
S90:
    xx = *x;
S100:
/*
     COMPUTE POWER SERIES
*/
    T6 = 1.0e0/pow(xx,2.0);
    alngam = devlpl(coef,&K5,&T6)/xx;
    alngam += (offset+(xx-0.5e0)*log(xx)-xx);
S110:
    return alngam;
#undef hln2pi
} /* END */
