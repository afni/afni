#include "cdflib.h"
double dstrem(double *z)
{
/*
**********************************************************************
     double dstrem(double *z)
             Double precision Sterling Remainder
                              Function
     Returns   Log(Gamma(Z))  -  Sterling(Z)  where   Sterling(Z)  is
     Sterling's Approximation to Log(Gamma(Z))
     Sterling(Z) = LOG( SQRT( 2*PI ) ) + ( Z-0.5 ) * LOG( Z ) - Z
                              Arguments
     Z --> Value at which Sterling remainder calculated
           Must be positive.
                  DOUBLE PRECISION Z
                              Method
     If Z >= 6 uses 9 terms of series in Bernoulli numbers
     (Values calculated using Maple)
     Otherwise computes difference explicitly
**********************************************************************
*/
#define hln2pi 0.91893853320467274178e0
#define ncoef 10
static double coef[ncoef] = {
    0.0e0,0.0833333333333333333333333333333e0,
    -0.00277777777777777777777777777778e0,0.000793650793650793650793650793651e0,
    -0.000595238095238095238095238095238e0,
    0.000841750841750841750841750841751e0,-0.00191752691752691752691752691753e0,
    0.00641025641025641025641025641026e0,-0.0295506535947712418300653594771e0,
    0.179644372368830573164938490016e0
};
static int K1 = 10;
static double dstrem,sterl,T2;
/*
     ..
     .. Executable Statements ..
*/
/*
    For information, here are the next 11 coefficients of the
    remainder term in Sterling's formula
            -1.39243221690590111642743221691
            13.4028640441683919944789510007
            -156.848284626002017306365132452
            2193.10333333333333333333333333
            -36108.7712537249893571732652192
            691472.268851313067108395250776
            -0.152382215394074161922833649589D8
            0.382900751391414141414141414141D9
            -0.108822660357843910890151491655D11
            0.347320283765002252252252252252D12
            -0.123696021422692744542517103493D14
*/
    if(*z <= 0.0e0) ftnstop("Zero or negative argument in DSTREM");
    if(!(*z > 6.0e0)) goto S10;
    T2 = 1.0e0/pow(*z,2.0);
    dstrem = devlpl(coef,&K1,&T2)**z;
    goto S20;
S10:
    sterl = hln2pi+(*z-0.5e0)*log(*z)-*z;
    dstrem = dlngam(z)-sterl;
S20:
    return dstrem;
#undef hln2pi
#undef ncoef
} /* END */
