#include "cdflib.h"
double dexpm1(double *x)
/*
**********************************************************************
 
     double dexpm1(double *x)
            Evaluation of the function EXP(X) - 1
 
 
                              Arguments
 
 
     X --> Argument at which exp(x)-1 desired
                    DOUBLE PRECISION X
 
 
                              Method
 
 
     Renaming of function rexp from code of:
 
     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
     Trans. Math.  Softw. 18 (1993), 360-373.
 
**********************************************************************
*/
{
static double p1 = .914041914819518e-09;
static double p2 = .238082361044469e-01;
static double q1 = -.499999999085958e+00;
static double q2 = .107141568980644e+00;
static double q3 = -.119041179760821e-01;
static double q4 = .595130811860248e-03;
static double dexpm1,w;
/*
     ..
     .. Executable Statements ..
*/
    if(fabs(*x) > 0.15e0) goto S10;
    dexpm1 = *x*(((p2**x+p1)**x+1.0e0)/((((q4**x+q3)**x+q2)**x+q1)**x+1.0e0));
    return dexpm1;
S10:
    w = exp(*x);
    if(*x > 0.0e0) goto S20;
    dexpm1 = w-0.5e0-0.5e0;
    return dexpm1;
S20:
    dexpm1 = w*(0.5e0+(0.5e0-1.0e0/w));
    return dexpm1;
} /* END */
