#include "cdflib.h"
double rcomp(double *a,double *x)
/*
     -------------------
     EVALUATION OF EXP(-X)*X**A/GAMMA(A)
     -------------------
     RT2PIN = 1/SQRT(2*PI)
     -------------------
*/
{
static double rt2pin = .398942280401433e0;
static double rcomp,t,t1,u;
/*
     ..
     .. Executable Statements ..
*/
    rcomp = 0.0e0;
    if(*a >= 20.0e0) goto S20;
    t = *a*log(*x)-*x;
    if(*a >= 1.0e0) goto S10;
    rcomp = *a*exp(t)*(1.0e0+gam1(a));
    return rcomp;
S10:
    rcomp = exp(t)/Xgamm(a);
    return rcomp;
S20:
    u = *x/ *a;
    if(u == 0.0e0) return rcomp;
    t = pow(1.0e0/ *a,2.0);
    t1 = (((0.75e0*t-1.0e0)*t+3.5e0)*t-105.0e0)/(*a*1260.0e0);
    t1 -= (*a*rlog(&u));
    rcomp = rt2pin*sqrt(*a)*exp(t1);
    return rcomp;
} /* END */
