#include "cdflib.h"
double rlog1(double *x)
/*
-----------------------------------------------------------------------
             EVALUATION OF THE FUNCTION X - LN(1 + X)
-----------------------------------------------------------------------
*/
{
static double a = .566749439387324e-01;
static double b = .456512608815524e-01;
static double p0 = .333333333333333e+00;
static double p1 = -.224696413112536e+00;
static double p2 = .620886815375787e-02;
static double q1 = -.127408923933623e+01;
static double q2 = .354508718369557e+00;
static double rlog1,h,r,t,w,w1;
/*
     ..
     .. Executable Statements ..
*/
    if(*x < -0.39e0 || *x > 0.57e0) goto S40;
    if(*x < -0.18e0) goto S10;
    if(*x > 0.18e0) goto S20;
/*
              ARGUMENT REDUCTION
*/
    h = *x;
    w1 = 0.0e0;
    goto S30;
S10:
    h = *x+0.3e0;
    h /= 0.7e0;
    w1 = a-h*0.3e0;
    goto S30;
S20:
    h = 0.75e0**x-0.25e0;
    w1 = b+h/3.0e0;
S30:
/*
               SERIES EXPANSION
*/
    r = h/(h+2.0e0);
    t = r*r;
    w = ((p2*t+p1)*t+p0)/((q2*t+q1)*t+1.0e0);
    rlog1 = 2.0e0*t*(1.0e0/(1.0e0-r)-r*w)+w1;
    return rlog1;
S40:
    w = *x+0.5e0+0.5e0;
    rlog1 = *x-log(w);
    return rlog1;
} /* END */
