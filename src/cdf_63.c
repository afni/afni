#include "cdflib.h"
void grat1(double *a,double *x,double *r,double *p,double *q,
	   double *eps)
{
static int K2 = 0;
static double a2n,a2nm1,am0,an,an0,b2n,b2nm1,c,cma,g,h,j,l,sum,t,tol,w,z,T1,T3;
/*
     ..
     .. Executable Statements ..
*/
/*
-----------------------------------------------------------------------
        EVALUATION OF THE INCOMPLETE GAMMA RATIO FUNCTIONS
                      P(A,X) AND Q(A,X)
     IT IS ASSUMED THAT A .LE. 1.  EPS IS THE TOLERANCE TO BE USED.
     THE INPUT ARGUMENT R HAS THE VALUE E**(-X)*X**A/GAMMA(A).
-----------------------------------------------------------------------
*/
    if(*a**x == 0.0e0) goto S120;
    if(*a == 0.5e0) goto S100;
    if(*x < 1.1e0) goto S10;
    goto S60;
S10:
/*
             TAYLOR SERIES FOR P(A,X)/X**A
*/
    an = 3.0e0;
    c = *x;
    sum = *x/(*a+3.0e0);
    tol = 0.1e0**eps/(*a+1.0e0);
S20:
    an += 1.0e0;
    c = -(c*(*x/an));
    t = c/(*a+an);
    sum += t;
    if(fabs(t) > tol) goto S20;
    j = *a**x*((sum/6.0e0-0.5e0/(*a+2.0e0))**x+1.0e0/(*a+1.0e0));
    z = *a*log(*x);
    h = gam1(a);
    g = 1.0e0+h;
    if(*x < 0.25e0) goto S30;
    if(*a < *x/2.59e0) goto S50;
    goto S40;
S30:
    if(z > -.13394e0) goto S50;
S40:
    w = exp(z);
    *p = w*g*(0.5e0+(0.5e0-j));
    *q = 0.5e0+(0.5e0-*p);
    return;
S50:
    l = rexp(&z);
    w = 0.5e0+(0.5e0+l);
    *q = (w*j-l)*g-h;
    if(*q < 0.0e0) goto S90;
    *p = 0.5e0+(0.5e0-*q);
    return;
S60:
/*
              CONTINUED FRACTION EXPANSION
*/
    a2nm1 = a2n = 1.0e0;
    b2nm1 = *x;
    b2n = *x+(1.0e0-*a);
    c = 1.0e0;
S70:
    a2nm1 = *x*a2n+c*a2nm1;
    b2nm1 = *x*b2n+c*b2nm1;
    am0 = a2nm1/b2nm1;
    c += 1.0e0;
    cma = c-*a;
    a2n = a2nm1+cma*a2n;
    b2n = b2nm1+cma*b2n;
    an0 = a2n/b2n;
    if(fabs(an0-am0) >= *eps*an0) goto S70;
    *q = *r*an0;
    *p = 0.5e0+(0.5e0-*q);
    return;
S80:
/*
                SPECIAL CASES
*/
    *p = 0.0e0;
    *q = 1.0e0;
    return;
S90:
    *p = 1.0e0;
    *q = 0.0e0;
    return;
S100:
    if(*x >= 0.25e0) goto S110;
    T1 = sqrt(*x);
    *p = erf1(&T1);
    *q = 0.5e0+(0.5e0-*p);
    return;
S110:
    T3 = sqrt(*x);
    *q = erfc1(&K2,&T3);
    *p = 0.5e0+(0.5e0-*q);
    return;
S120:
    if(*x <= *a) goto S80;
    goto S90;
} /* END */
