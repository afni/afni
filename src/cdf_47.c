#include "cdflib.h"
double dlngam(double *a)
/*
**********************************************************************
 
     double dlngam(double *a)
                 Double precision LN of the GAMma function
 
 
                              Function
 
 
     Returns the natural logarithm of GAMMA(X).
 
 
                              Arguments
 
 
     X --> value at which scaled log gamma is to be returned
                    X is DOUBLE PRECISION
 
 
                              Method
 
 
     Renames GAMLN from:
     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
     Trans. Math.  Softw. 18 (1993), 360-373.
 
**********************************************************************
-----------------------------------------------------------------------
            EVALUATION OF LN(GAMMA(A)) FOR POSITIVE A
-----------------------------------------------------------------------
     WRITTEN BY ALFRED H. MORRIS
          NAVAL SURFACE WARFARE CENTER
          DAHLGREN, VIRGINIA
--------------------------
     D = 0.5*(LN(2*PI) - 1)
--------------------------
*/
{
static double c0 = .833333333333333e-01;
static double c1 = -.277777777760991e-02;
static double c2 = .793650666825390e-03;
static double c3 = -.595202931351870e-03;
static double c4 = .837308034031215e-03;
static double c5 = -.165322962780713e-02;
static double d = .418938533204673e0;
static double dlngam,t,w;
static int i,n;
static double T1;
/*
     ..
     .. Executable Statements ..
*/
    if(*a > 0.8e0) goto S10;
    dlngam = gamln1(a)-log(*a);
    return dlngam;
S10:
    if(*a > 2.25e0) goto S20;
    t = *a-0.5e0-0.5e0;
    dlngam = gamln1(&t);
    return dlngam;
S20:
    if(*a >= 10.0e0) goto S40;
    n = *a-1.25e0;
    t = *a;
    w = 1.0e0;
    for(i=1; i<=n; i++) {
        t -= 1.0e0;
        w = t*w;
    }
    T1 = t-1.0e0;
    dlngam = gamln1(&T1)+log(w);
    return dlngam;
S40:
    t = pow(1.0e0/ *a,2.0);
    w = (((((c5*t+c4)*t+c3)*t+c2)*t+c1)*t+c0)/ *a;
    dlngam = d+w+(*a-0.5e0)*(log(*a)-1.0e0);
    return dlngam;
} /* END */
