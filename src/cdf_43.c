#include "cdflib.h"
double dlanor(double *x)
/*
**********************************************************************
 
     double dlanor(double *x)
           Double precision Logarith of the Asymptotic Normal
 
 
                              Function
 
 
      Computes the logarithm of the cumulative normal distribution
      from abs( x ) to infinity for abs( x ) >= 5.
 
 
                              Arguments
 
 
      X --> Value at which cumulative normal to be evaluated
                     DOUBLE PRECISION X
 
 
                              Method
 
 
      23 term expansion of formula 26.2.12 of Abramowitz and Stegun.
      The relative error at X = 5 is about 0.5E-5.
 
 
                              Note
 
 
      ABS(X) must be >= 5 else there is an error stop.
 
**********************************************************************
*/
{
#define dlsqpi 0.91893853320467274177e0
static double coef[12] = {
    -1.0e0,3.0e0,-15.0e0,105.0e0,-945.0e0,10395.0e0,-135135.0e0,2027025.0e0,
    -34459425.0e0,654729075.0e0,-13749310575.e0,316234143225.0e0
};
static int K1 = 12;
static double dlanor,approx,correc,xx,xx2,T2;
/*
     ..
     .. Executable Statements ..
*/
    xx = fabs(*x);
    if(xx < 5.0e0) ftnstop(" Argument too small in DLANOR");
    approx = -dlsqpi-0.5e0*xx*xx-log(xx);
    xx2 = xx*xx;
    T2 = 1.0e0/xx2;
    correc = devlpl(coef,&K1,&T2)/xx2;
    correc = dln1px(&correc);
    dlanor = approx+correc;
    return dlanor;
#undef dlsqpi
} /* END */
