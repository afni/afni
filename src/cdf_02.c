#include "cdflib.h"
double alnrel(double *a)
/*
-----------------------------------------------------------------------
            EVALUATION OF THE FUNCTION LN(1 + A)
-----------------------------------------------------------------------
*/
{
static double p1 = -.129418923021993e+01;
static double p2 = .405303492862024e+00;
static double p3 = -.178874546012214e-01;
static double q1 = -.162752256355323e+01;
static double q2 = .747811014037616e+00;
static double q3 = -.845104217945565e-01;
static double alnrel,t,t2,w,x;
/*
     ..
     .. Executable Statements ..
*/
    if(fabs(*a) > 0.375e0) goto S10;
    t = *a/(*a+2.0e0);
    t2 = t*t;
    w = (((p3*t2+p2)*t2+p1)*t2+1.0e0)/(((q3*t2+q2)*t2+q1)*t2+1.0e0);
    alnrel = 2.0e0*t*w;
    return alnrel;
S10:
    x = 1.e0+*a;
    alnrel = log(x);
    return alnrel;
} /* END */
