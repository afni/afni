#include "cdflib.h"
double bfrac(double *a,double *b,double *x,double *y,double *lambda,
	     double *eps)
/*
-----------------------------------------------------------------------
     CONTINUED FRACTION EXPANSION FOR IX(A,B) WHEN A,B .GT. 1.
     IT IS ASSUMED THAT  LAMBDA = (A + B)*Y - B.
-----------------------------------------------------------------------
*/
{
static double bfrac,alpha,an,anp1,beta,bn,bnp1,c,c0,c1,e,n,p,r,r0,s,t,w,yp1;
/*
     ..
     .. Executable Statements ..
*/
    bfrac = brcomp(a,b,x,y);
    if(bfrac == 0.0e0) return bfrac;
    c = 1.0e0+*lambda;
    c0 = *b/ *a;
    c1 = 1.0e0+1.0e0/ *a;
    yp1 = *y+1.0e0;
    n = 0.0e0;
    p = 1.0e0;
    s = *a+1.0e0;
    an = 0.0e0;
    bn = anp1 = 1.0e0;
    bnp1 = c/c1;
    r = c1/c;
S10:
/*
        CONTINUED FRACTION CALCULATION
*/
    n += 1.0e0;
    t = n/ *a;
    w = n*(*b-n)**x;
    e = *a/s;
    alpha = p*(p+c0)*e*e*(w**x);
    e = (1.0e0+t)/(c1+t+t);
    beta = n+w/s+e*(c+n*yp1);
    p = 1.0e0+t;
    s += 2.0e0;
/*
        UPDATE AN, BN, ANP1, AND BNP1
*/
    t = alpha*an+beta*anp1;
    an = anp1;
    anp1 = t;
    t = alpha*bn+beta*bnp1;
    bn = bnp1;
    bnp1 = t;
    r0 = r;
    r = anp1/bnp1;
    if(fabs(r-r0) <= *eps*r) goto S20;
/*
        RESCALE AN, BN, ANP1, AND BNP1
*/
    an /= bnp1;
    bn /= bnp1;
    anp1 = r;
    bnp1 = 1.0e0;
    goto S10;
S20:
/*
                 TERMINATION
*/
    bfrac *= r;
    return bfrac;
} /* END */
