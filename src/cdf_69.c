#include "cdflib.h"
double rlog(double *x)
/*
     -------------------
     COMPUTATION OF  X - 1 - LN(X)
     -------------------
*/
{
static double a = .566749439387324e-01;
static double b = .456512608815524e-01;
static double p0 = .333333333333333e+00;
static double p1 = -.224696413112536e+00;
static double p2 = .620886815375787e-02;
static double q1 = -.127408923933623e+01;
static double q2 = .354508718369557e+00;
static double rlog,r,t,u,w,w1;
/*
     ..
     .. Executable Statements ..
*/
    if(*x < 0.61e0 || *x > 1.57e0) goto S40;
    if(*x < 0.82e0) goto S10;
    if(*x > 1.18e0) goto S20;
/*
              ARGUMENT REDUCTION
*/
    u = *x-0.5e0-0.5e0;
    w1 = 0.0e0;
    goto S30;
S10:
    u = *x-0.7e0;
    u /= 0.7e0;
    w1 = a-u*0.3e0;
    goto S30;
S20:
    u = 0.75e0**x-1.e0;
    w1 = b+u/3.0e0;
S30:
/*
               SERIES EXPANSION
*/
    r = u/(u+2.0e0);
    t = r*r;
    w = ((p2*t+p1)*t+p0)/((q2*t+q1)*t+1.0e0);
    rlog = 2.0e0*t*(1.0e0/(1.0e0-r)-r*w)+w1;
    return rlog;
S40:
    r = *x-0.5e0-0.5e0;
    rlog = r-log(*x);
    return rlog;
} /* END */
