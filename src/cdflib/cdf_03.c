#include "cdflib.h"
double apser(double *a,double *b,double *x,double *eps)
/*
-----------------------------------------------------------------------
     APSER YIELDS THE INCOMPLETE BETA RATIO I(SUB(1-X))(B,A) FOR
     A .LE. MIN(EPS,EPS*B), B*X .LE. 1, AND X .LE. 0.5. USED WHEN
     A IS VERY SMALL. USE ONLY IF ABOVE INEQUALITIES ARE SATISFIED.
-----------------------------------------------------------------------
*/
{
static double g = .577215664901533e0;
static double apser,aj,bx,c,j,s,t,tol;
/*
     ..
     .. Executable Statements ..
*/
    bx = *b**x;
    t = *x-bx;
    if(*b**eps > 2.e-2) goto S10;
    c = log(*x)+psi(b)+g+t;
    goto S20;
S10:
    c = log(bx)+g+t;
S20:
    tol = 5.0e0**eps*fabs(c);
    j = 1.0e0;
    s = 0.0e0;
S30:
    j += 1.0e0;
    t *= (*x-bx/j);
    aj = t/j;
    s += aj;
    if(fabs(aj) > tol) goto S30;
    apser = -(*a*(c+s));
    return apser;
} /* END */
