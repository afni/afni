#include "cdflib.h"
double bup(double *a,double *b,double *x,double *y,int *n,double *eps)
/*
-----------------------------------------------------------------------
     EVALUATION OF IX(A,B) - IX(A+N,B) WHERE N IS A POSITIVE INTEGER.
     EPS IS THE TOLERANCE USED.
-----------------------------------------------------------------------
*/
{
static int K1 = 1;
static int K2 = 0;
static double bup,ap1,apb,d,l,r,t,w;
static int i,k,kp1,mu,nm1;
/*
     ..
     .. Executable Statements ..
*/
/*
          OBTAIN THE SCALING FACTOR EXP(-MU) AND
             EXP(MU)*(X**A*Y**B/BETA(A,B))/A
*/
    apb = *a+*b;
    ap1 = *a+1.0e0;
    mu = 0;
    d = 1.0e0;
    if(*n == 1 || *a < 1.0e0) goto S10;
    if(apb < 1.1e0*ap1) goto S10;
    mu = fabs(exparg(&K1));
    k = exparg(&K2);
    if(k < mu) mu = k;
    t = mu;
    d = exp(-t);
S10:
    bup = brcmp1(&mu,a,b,x,y)/ *a;
    if(*n == 1 || bup == 0.0e0) return bup;
    nm1 = *n-1;
    w = d;
/*
          LET K BE THE INDEX OF THE MAXIMUM TERM
*/
    k = 0;
    if(*b <= 1.0e0) goto S50;
    if(*y > 1.e-4) goto S20;
    k = nm1;
    goto S30;
S20:
    r = (*b-1.0e0)**x/ *y-*a;
    if(r < 1.0e0) goto S50;
    k = t = nm1;
    if(r < t) k = r;
S30:
/*
          ADD THE INCREASING TERMS OF THE SERIES
*/
    for(i=1; i<=k; i++) {
        l = i-1;
        d = (apb+l)/(ap1+l)**x*d;
        w += d;
    }
    if(k == nm1) goto S70;
S50:
/*
          ADD THE REMAINING TERMS OF THE SERIES
*/
    kp1 = k+1;
    for(i=kp1; i<=nm1; i++) {
        l = i-1;
        d = (apb+l)/(ap1+l)**x*d;
        w += d;
        if(d <= *eps*w) goto S70;
    }
S70:
/*
               TERMINATE THE PROCEDURE
*/
    bup *= w;
    return bup;
} /* END */
