#include "cdflib.h"
double fpser(double *a,double *b,double *x,double *eps)
/*
-----------------------------------------------------------------------
 
                 EVALUATION OF I (A,B)
                                X
 
          FOR B .LT. MIN(EPS,EPS*A) AND X .LE. 0.5.
 
-----------------------------------------------------------------------
 
                  SET  FPSER = X**A
*/
{
static int K1 = 1;
static double fpser,an,c,s,t,tol;
/*
     ..
     .. Executable Statements ..
*/
    fpser = 1.0e0;
    if(*a <= 1.e-3**eps) goto S10;
    fpser = 0.0e0;
    t = *a*log(*x);
    if(t < exparg(&K1)) return fpser;
    fpser = exp(t);
S10:
/*
                NOTE THAT 1/B(A,B) = B
*/
    fpser = *b/ *a*fpser;
    tol = *eps/ *a;
    an = *a+1.0e0;
    t = *x;
    s = t/an;
S20:
    an += 1.0e0;
    t = *x*t;
    c = t/an;
    s += c;
    if(fabs(c) > tol) goto S20;
    fpser *= (1.0e0+*a*s);
    return fpser;
} /* END */
