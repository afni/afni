#include "cdflib.h"
double dln1px(double *a)
/*
**********************************************************************
 
     double dln1px(double *a)
               Double precision LN(1+X)
 
 
                              Function
 
 
     Returns ln(1+x)
     Note that the obvious code of
               LOG(1.0+X)
     won't work for small X because 1.0+X loses accuracy
 
 
                              Arguments
 
 
     X --> Value for which ln(1-x) is desired.
                                        X is DOUBLE PRECISION
 
 
                              Method
 
 
     Renames ALNREL from:
     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
     Trans. Math.  Softw. 18 (1993), 360-373.
 
**********************************************************************
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
static double dln1px,t,t2,w,x;
/*
     ..
     .. Executable Statements ..
*/
    if(fabs(*a) > 0.375e0) goto S10;
    t = *a/(*a+2.0e0);
    t2 = t*t;
    w = (((p3*t2+p2)*t2+p1)*t2+1.0e0)/(((q3*t2+q2)*t2+q1)*t2+1.0e0);
    dln1px = 2.0e0*t*w;
    return dln1px;
S10:
    x = 1.e0+*a;
    dln1px = log(x);
    return dln1px;
} /* END */
